use 5.12.0;
use warnings;

package Soviet::Minecraft;
use MooseX::POE;

use JSON;
use List::Util qw(max);
use Path::Tiny;
use POE::Wheel::Run;
use POE::Wheel::ReadWrite;

has json => (
  is   => 'ro',
  lazy => 1,
  default => sub { JSON->new->canonical->pretty; }
);

has config => (
  is    => 'ro',
  lazy  => 1,
  default => sub { $_[0]->json->decode( path('soviet.json')->slurp_raw ) },
);

event save_config => sub {
  my ($self) = @_[OBJECT,];
  path('soviet.json')->spew_raw( $self->json->encode( $self->config ) );
};

has server => (
  is   => 'rw',
  init_arg => undef,
  clearer  => 'clear_server',
);

sub _setup_server {
  my ($self) = @_;

  my $server = POE::Wheel::Run->new(
    Program => [
      qw(java -Xms1024M -Xmx1024M -jar minecraft_server.1.8.1.jar nogui),
    ],
    StdoutEvent  => "got_child_stdout",
    StderrEvent  => "got_child_stderr",
    CloseEvent   => "got_child_close",

    StdioFilter  => POE::Filter::Line->new(Literal => "\n"),
  );

  $self->server( $server );
  return;
}

has stdin => (
  is   => 'rw',
  init_arg => undef,
  clearer  => 'clear_stdin',
);

sub _setup_stdin {
  my ($self) = @_;
  my $stdin = POE::Wheel::ReadWrite->new(
    InputHandle  => *STDIN,
    OutputHandle => *STDERR, # Never used...
    InputFilter => POE::Filter::Line->new(Literal => "\n"),
    InputEvent  => 'got_console_stdin',
  );

  $self->stdin($stdin);
}

sub START {
  my ($self) = @_;
  $self->_setup_server;
  $self->_setup_stdin;

  POE::Kernel->sig_child($self->server->PID, "got_child_signal");

  print(
    "Child pid ", $self->server->PID,
    " started as wheel ", $self->server->ID, ".\n"
  );
}

event got_cast_vote => sub {
  my ($self, $who, $which, $vote) = @_[OBJECT, ARG0, ARG1, ARG2];

  # Election in progress?
  #   YES: add/change vote
  #   NO : Last election very recent?
  #     YES: Complain and refuse.
  #     NO : Begin new election.
  my $election = ($_[HEAP]{election}{$which} ||= {});

  if ($election->{completed_at}) {
    if (
      keys %{ $election->{votes} } > 1
      and
      $election->{completed_at} > time - 300
    ) {
      $self->server->put("msg $who You can't change the $which again so soon!");
      return;
    }

    $election = $_[HEAP]{election}{$which} = {};
  }

  if (! $election->{began_at}) {
    $election->{began_at} = time;
    $_[KERNEL]->delay_set(election_complete => 31, $which);
  }

  $election->{votes}{$which} = $vote;
  $self->server->put("list"); # to trigger early termination
};

event got_updated_player_count => sub {
  my ($curr, $max) = @_[ARG0, ARG1];
  for my $which (keys %{ $_[HEAP]{election} }) {
    my $votes = values %{ $_[HEAP]{election}{$which}{votes} };
    if ($votes >= $curr) {
      $_[KERNEL]->yield(election_complete => $which);
    }
  }
};

event election_complete => sub {
  my ($self, $which) = @_[OBJECT,ARG0];

  my $server   = $self->server;
  my $election = $_[HEAP]{election}{$which};

  if ($election->{completed_at}) {
    # This happens when the on-delay event fires after the election completed
    # because of a player count update. -- rjbs, 2014-11-28
    return;
  }

  $election->{completed_at} = time;

  my @votes    = values %{ $election->{votes} };

  my %score;
  $score{$_} ++ for values %{ $election->{votes} };
  my @ranked = sort { $score{$b} <=> $score{$a} } keys %score;
  if (@votes > 1 && $score{ $ranked[0] } == $score{ $ranked[1] }) {
    # n-way tie, no change?
    $server->put("say The $which vote was a tie.  Nothing will change.");
    return;
  }

  my $winner = $ranked[0];

  if ($which eq 'rain') {
    if    ($winner eq 'on')   { $server->put("weather rain");    }
    elsif ($winner eq 'off')  { $server->put("weather clear");   }
    elsif ($winner eq 'hard') { $server->put("weather thunder"); }

  } elsif ($which eq 'time') {
    if    ($winner eq 'sunrise') { $server->put("time set day"); }
    elsif ($winner eq 'sunset')  { $server->put("time set night"); }

  } else {
    warn "don't know how to handle $which election!!";
  };
};

event got_xyz_teleport => sub {
  my ($who, $tp) = @_[ARG0, ARG1];

  return unless my $callbacks = $_[HEAP]{tp}{$who};

  for my $key (keys %$callbacks) {
    my $callback = delete $callbacks->{$key};
    $callback->{code}->(@_) unless time > $callback->{expires_at};
  }
};

sub hub_xyz {
  my ($self) = @_;
  $self->config->{hub}
}

sub home_for {
  my ($self, $player) = @_;

  $self->config->{home}{$player} || $self->hub_xyz;
}

sub porch_for {
  my ($self, $player) = @_;
  $self->config->{porch}{$player} || $self->home_for($player);
}

# Wheel event, including the wheel's ID.
event got_child_stdout => sub {
  my ($self, $stdout_line, $wheel_id) = @_[OBJECT, ARG0, ARG1];
  my $server = $self->server;
  # print "pid ", $child->PID, " STDOUT: $stdout_line\n";
  print "$stdout_line\n";

  # [11:01:21] [Server thread/INFO]: rjbs joined the game
  # [11:01:28] [Server thread/INFO]: <rjbs> go home
  return unless my $parse = naive_parse($stdout_line);

  if (my $tp = tp_parse($parse->{message})) {
    $_[KERNEL]->yield(got_xyz_teleport => lc $tp->{who}, $tp);
    return;
  }

  if (
    my ($curr, $max) =
      $parse->{message} =~ m{\AThere are ([0-9]+)/([0-9]+) players}
  ) {
    $_[KERNEL]->yield(got_updated_player_count => $curr, $max);
    return;
  }

  if (my ($who, $what) = $parse->{message} =~ /\A<([^>]+)>\s+(.+)\z/) {
    $who  = lc $who;
    $what = lc $what;

    if    ($what eq '!hub')     { $server->put("tp $who " . $self->hub_xyz) }
    elsif ($what eq '!home')    { $server->put("tp $who " . $self->home_for($who)); }

    elsif ($what eq '!set home')  {
      $_[HEAP]{tp}{$who}{home} = {
        expires_at => time + 5,
        code       => sub {
          my ($self, $tp) = @_[OBJECT,ARG1];
          $self->config->{home}{$who} = "$tp->{x} $tp->{y} $tp->{z}";
          $_[KERNEL]->yield('save_config');
          $server->put("msg $who Your home has been updated.");
        },
      };
      $server->put("tp $who ~ ~ ~");
    }

    elsif ($what eq '!set porch')  {
      $_[HEAP]{tp}{$who}{porch} = {
        expires_at => time + 5,
        code       => sub {
          my ($self, $tp) = @_[OBJECT,ARG1];
          $self->config->{porch}{$who} = "$tp->{x} $tp->{y} $tp->{z}";
          $_[KERNEL]->yield('save_config');
          $server->put("msg $who Your front porch location has been updated.");
        },
      };
      $server->put("tp $who ~ ~ ~");
    }

    elsif ($what eq '!sunrise') {
      $_[KERNEL]->yield(got_cast_vote => $who, 'time', 'sunrise');
      $server->put("msg $who You cast your vote for sunrise.");
    }
    elsif ($what eq '!sunset') {
      $_[KERNEL]->yield(got_cast_vote => $who, 'time', 'sunset');
      $server->put("msg $who You cast your vote for sunset.");
    }

    elsif ($what eq '!rain on') {
      $_[KERNEL]->yield(got_cast_vote => $who, 'rain', 'on');
      $server->put("msg $who You cast your vote for rain.");
    }
    elsif ($what eq '!rain off') {
      $_[KERNEL]->yield(got_cast_vote => $who, 'rain', 'off');
      $server->put("msg $who You cast your vote for clear skies.");
    }
    elsif ($what eq '!rain hard') {
      $_[KERNEL]->yield(got_cast_vote => $who, 'rain', 'hard');
      $server->put("msg $who You cast your vote for a thunderstorm.");
    }

    elsif ($what =~ /\A!visit (\S+)\z/) {
      $server->put("tp $who " . $self->porch_for($1));
    }

    elsif ($what =~ /\A!mode (creative|survival)\z/i) {
      my $mode = $1 eq 'creative' ? 1 : 0;
      $server->put("gamemode $mode $who");
    }

    elsif ($what =~ /\A!join (\S+)\z/) {
      # If $1 isn't a player, report an error.
      $server->put("tp $who $1");
    }
  }
};

sub tp_parse {
  my ($msg) = @_;

  # [22:15:01] [Server thread/INFO]:
  # Teleported rjbs to 688.330745199867, 75.0, 479.96637932110707

  my ($who, $x, $y, $z) = $msg =~ /\A
    Teleported \s (\S+) \s to \s
    ([0-9]+\.[0-9]+), \s
    ([0-9]+\.[0-9]+), \s
    ([0-9]+\.[0-9]+)
  \z/x;

  return unless defined $who;
  return { who => lc $who, x => $x, y => $y, z => $z };
}

sub naive_parse {
  my ($line) = @_;
  my ($ts, $channel, $rest) = $line =~ /
    \A
    \[([0-9]{2}:[0-9]{2}:[0-9]{2})\]
    \s+
    \[([^\]]+)\]:
    \s+
    (.+)
  /x;

  return unless defined $ts;

  return {
    timestamp => $ts,
    channel   => $channel,
    message   => $rest,
  };
}

# Wheel event, including the wheel's ID.
event got_child_stderr => sub {
  my ($stderr_line, $wheel_id) = @_[ARG0, ARG1];
  my $child = $_[HEAP]{child};
  # print "pid ", $child->PID, " STDERR: $stderr_line\n";
  warn "$stderr_line\n";
};

# Wheel event, including the wheel's ID.
event got_child_close => sub {
  my ($self, $wheel_id) = @_[OBJECT, ARG0];

  my $server = $self->server;

  unless ($wheel_id == $server->ID) {
    die "what's going on!? child closed, but it isn't the server!";
  }

  print "pid ", $server->PID, " closed all pipes.\n";

  $self->clear_server;
  $self->clear_stdin;
  return;
};

event got_child_signal => sub {
  my ($self) = @_[OBJECT,];
  print "pid $_[ARG1] exited with status $_[ARG2].\n";

  # May have been reaped by on_child_close().
  return unless $self->server && $self->server->PID == $_[ARG1];

  $self->clear_server;
};

event got_console_stdin => sub {
  my ($self, $input) = @_[OBJECT, ARG0];

  chomp $input;

  $self->server->put($input);
};

1;
