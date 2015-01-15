use 5.18.0;
use warnings;

package Soviet::Minecraft;
# ABSTRACT: in Soviet Minecraft, server op you!

use MooseX::POE;

use HTTP::Body;
use JSON;
use List::Util qw(max);
use Path::Tiny;
use POE::Component::Server::SimpleHTTP;
use POE::Wheel::Run;
use POE::Wheel::ReadWrite;

has config_filename => (
  is  => 'ro',
  default => 'soviet.json',
);

has json => (
  is   => 'ro',
  lazy => 1,
  default => sub { JSON->new->canonical->pretty; }
);

has config => (
  is    => 'ro',
  lazy  => 1,
  default => sub {
    my ($self) = @_;
    $self->json->decode( path( $self->config_filename )->slurp_raw ),
  },
);

event save_config => sub {
  my ($self) = @_[OBJECT,];
  path($self->config_filename)->spew_raw(
    $self->json->encode( $self->config )
  );
};

has server => (
  is   => 'rw',
  init_arg => undef,
  clearer  => 'clear_server',
);

has minecraft_jar => (
  is => 'ro',
  lazy => 1,
  default => sub {
    my ($self) = @_;
    $self->config->{minecraft_jar} || 'minecraft_server.jar';
  },
);

sub _setup_server {
  my ($self) = @_;

  my $server = POE::Wheel::Run->new(
    Program => [
      qw(java -Xms1024M -Xmx1024M -jar), $self->minecraft_jar, qw(nogui),
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

has httpd => (
  is   => 'ro',
  lazy => 1,
  clearer => 'clear_httpd',
  builder => '_build_httpd',
);

has httpd_port => (is => 'ro', lazy => 1, default => 8181);

sub _build_httpd {
  my ($self) = @_;

  POE::Component::Server::SimpleHTTP->new(
    ALIAS   => 'httpd',
    ADDRESS => 0,
    PORT    => $self->httpd_port,
    HANDLERS => [
      {
        DIR => '^/sms$',
        SESSION => $self->get_session_id,
        EVENT => '_http_sms',
      },
      {
        DIR => '.*',
        SESSION => $self->get_session_id,
        EVENT => '_http_404',
      },
    ],
    HEADERS => { Server => 'Synergy' },
  );
}

sub _params_from_req {
  my ($self, $req) = @_;
  my $body = HTTP::Body->new(
    scalar $req->header('Content-Type'),
    scalar $req->header('Content-Length'),
  );
  $body->add( $req->content );
  return $body->param;
}

event _http_sms => sub {
  my ($kernel, $self, $request, $response, $dirmatch)
    = @_[ KERNEL, OBJECT, ARG0 .. ARG2 ];

  # Check for errors
  if (! defined $request) {
    $kernel->call('httpd', 'DONE', $response );
    return;
  }

  my $param = $self->_params_from_req($request);

  my $from = $param->{From} // '';
  $from =~ s/\A\+1//;

  my $sender_ok = $self->config->{allow_sms_from}->{$from};
  unless ($param->{AccountSid} eq $self->config->{twilio_sid} and $sender_ok) {
    $response->code(400);
    $response->content("Bad request");
    $kernel->call( 'httpd', 'DONE', $response );
    warn sprintf "Bad request for %s from phone %s from IP %s",
      $request->uri->path_query,
      $from,
      $response->connection->remote_ip;
    return;
  }

  my $text = lc $param->{Body};

  my $reply;
  my $command;

  if ($text =~ /\Aallow ([-_0-9a-z]+)\z/i) {
    my $who = $1;
    $command = "whitelist add $who";
    $reply   = "Okay, I've added $who to the whitelist!";
  } elsif ($text =~ /\Aannounce (.+)\z/i) {
    $command = "say $1";
    $reply   = "Okay, I've passed along that message!";
  } elsif ($text eq 'emergency shutdown' or $text eq 'emergency shut down') {
    $command = "stop";
    $reply   = "I'm issuing an emergency shutdown!";
  }

  if (defined $reply) {
    $response->code(200);
    $response->content($reply);
  } else {
    $response->code(200);
    $response->content("Does not compute.");
  }

  $kernel->call( 'httpd', 'DONE', $response );

  warn("Request from " . $response->connection->remote_ip . " " . $request->uri->path_query);

  if (defined $reply and length $command) {
    $self->server->put($command);
  }
};

event _http_404 => sub {
  my ($kernel, $self, $request, $response) = @_[KERNEL, OBJECT, ARG0 .. ARG2];

  if (! defined $request) {
    $kernel->call('httpd', 'DONE', $response );
    return;
  }

  # Do our stuff to HTTP::Response
  $response->code(404);
  $response->content(
    "Hi visitor from "
    . $response->connection->remote_ip
    . ", Page not found -> '"
    . $request->uri->path . "'\n\n"
  );

  # We are done!
  # For speed, you could use $_[KERNEL]->call( ... )
  $kernel->call( 'httpd', 'DONE', $response );
  warn
    "Request from "
    . $response->connection->remote_ip . " " . $request->uri->path_query;
};

sub START {
  my ($self) = @_;
  $self->_setup_server;
  $self->_setup_stdin;

  $self->httpd;

  POE::Kernel->sig_child($self->server->PID, "got_child_signal");

  print(
    "Child pid ", $self->server->PID,
    " started as wheel ", $self->server->ID, ".\n"
  );
}

has _election => (
  is => 'ro',
  init_arg => undef,
  default  => sub {  {}  },
  traits   => [ 'Hash' ],
  handles  => {
    elections    => 'keys',
  },
);

sub election_for {
  my ($self, $which) = @_;
  $self->_election->{$which} ||= {};
}

event got_cast_vote => sub {
  my ($self, $who, $which, $vote) = @_[OBJECT, ARG0, ARG1, ARG2];

  # Election in progress?
  #   YES: add/change vote
  #   NO : Last election very recent?
  #     YES: Complain and refuse.
  #     NO : Begin new election.
  my $election = $self->election_for($which);

  if ($election->{completed_at}) {
    if (
      keys %{ $election->{votes} } > 1
      and
      $election->{completed_at} > time - 300
    ) {
      $self->server->put("msg $who You can't change the $which again so soon!");
      return;
    }

    %$election = ();
  }

  if (! $election->{began_at}) {
    $election->{began_at} = time;
    $_[KERNEL]->delay_set(election_complete => 31, $which);
  }

  $election->{votes}{$who} = $vote;
  $self->server->put("list"); # to trigger early termination
};

event got_updated_player_count => sub {
  my ($self, $curr, $max) = @_[OBJECT, ARG0, ARG1];
  for my $which ($self->elections) {
    my $election = $self->election_for($which);
    my $votes = values %{ $election->{votes} };
    if ($votes >= $curr) {
      $_[KERNEL]->yield(election_complete => $which);
    }
  }
};

event election_complete => sub {
  my ($self, $which) = @_[OBJECT,ARG0];

  my $server   = $self->server;
  my $election = $self->election_for($which);

  if ($election->{completed_at}) {
    # This happens when the on-delay event fires after the election completed
    # because of a player count update. -- rjbs, 2014-11-28
    return;
  }

  $election->{completed_at} = time;

  my @votes    = values %{ $election->{votes} };

  my %score;
  $score{$_} ++ for @votes;
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

has _tp_callbacks => (
  is => 'ro',
  init_arg => undef,
  default  => sub {  {}  },
  traits   => [ 'Hash' ],
  handles  => {
    delete_tp_callbacks_for => 'delete',
  },
);

sub _set_tp_callback {
  my ($self, $who, $name, $hashref) = @_;
  $self->_tp_callbacks->{$who}{$name} = $hashref;
}

event got_xyz_teleport => sub {
  my ($self, $who, $tp) = @_[OBJECT, ARG0, ARG1];

  return unless my $callbacks = $self->delete_tp_callbacks_for($who);

  for my $key (keys %$callbacks) {
    my $callback = $callbacks->{$key};
    $callback->{code}->(@_) unless time > $callback->{expires_at};
  }
};

sub hub_xyz {
  my ($self) = @_;
  return @{ $self->config->{hub} }
}

sub hub_xyz_str {
  my ($self) = @_;
  return "@{ $self->config->{hub} }"
}

sub random_unowned_location {
  my ($self) = @_;
  my @homes = map {; Soviet::Minecraft::Point->from_arrayref($_) }
              values %{ $self->config->{home} };

  TRY: for (1 .. 100) {
    my $point = Soviet::Minecraft::Point->from_xyz(
      int rand 1_000_000,
      100,
      int rand 1_000_000,
    );

    for my $home (@homes) {
      next TRY if $home->planar_distance_to($point) < 10_000;
    }

    return $point;
  }

  return;
}

sub home_for {
  my ($self, $player) = @_;

  return Soviet::Minecraft::Point->from_arrayref(
    $self->config->{home}{$player} || [ $self->hub_xyz ]
  );
}

sub porch_for {
  my ($self, $player) = @_;
  $self->config->{porch}{$player} || $self->home_for($player);

  if (my $porch = $self->config->{porch}{$player}) {
    return Soviet::Minecraft::Point->from_arrayref($porch);
  }

  return $self->home_for($player);
}

sub place_named {
  my ($self, $name) = @_;

  my $place = $self->config->{place}{fc $name};
  return $place
       ? Soviet::Minecraft::Point->from_arrayref($place->{location})
       : undef;
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
    my $orig_what = $what;
    $who  = lc $who;
    $what = lc $what;

    if    ($what eq '!hub')     { $server->put("tp $who " . $self->hub_xyz_str) }
    elsif ($what eq '!home')    { $server->put("tp $who " . $self->home_for($who)->as_string); }

    elsif ($orig_what =~ /\A!goto\s+(.+?)\s*\z/i) {
      my $name = $1;
      my $location = $self->place_named($name);
      if ($location) {
        $server->put("tp $who " . $location->as_string);
      } else {
        $server->put("msg $who I couldn't find a place called that!");
      }
    }

    elsif ($what eq '!set home')  {
      $self->_set_tp_callback($who => home => {
        expires_at => time + 5,
        code       => sub {
          my ($self, $tp) = @_[OBJECT,ARG1];
          $self->config->{home}{$who} = [ $tp->{where}->xyz ];
          $_[KERNEL]->yield('save_config');
          $server->put("msg $who Your home has been updated.");
        },
      });
      $server->put("tp $who ~ ~ ~");
    }

    elsif ($what eq '!random') {
      my $location = $self->random_unowned_location;
      if ($location) {
        $server->put("gamemode 1 $who");
        $server->put("tp $who " . $location->as_string);
      } else {
        $server->put("msg $who I couldn't find a random place to send you!");
      }
    }

    elsif ($orig_what =~ /\A!name\s+(.+?)\s*\z/) {
      my $name = $1;
      $self->_set_tp_callback($who => placename => {
        expires_at => time + 5,
        code       => sub {
          my ($self, $tp) = @_[OBJECT,ARG1];
          $self->config->{place}{fc $name} = {
            name     => $name,
            named_by => $who,
            location => [ $tp->{where}->xyz ],
          };
          $_[KERNEL]->yield('save_config');
          $server->put(qq{msg $who You've named this place "$name"});
        },
      });
      $server->put("tp $who ~ ~ ~");
    }

    elsif ($what eq '!set porch')  {
      $self->_set_tp_callback($who => porch => {
        expires_at => time + 5,
        code       => sub {
          my ($self, $tp) = @_[OBJECT,ARG1];
          $self->config->{porch}{$who} = [ $tp->{where}->xyz ];
          $_[KERNEL]->yield('save_config');
          $server->put("msg $who Your front porch location has been updated.");
        },
      });
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
    (-?[0-9]+\.[0-9]+), \s
    (-?[0-9]+\.[0-9]+), \s
    (-?[0-9]+\.[0-9]+)
  \z/x;

  return unless defined $who;
  return {
    who   => lc $who,
    where => Soviet::Minecraft::Point->from_xyz($x, $y, $z),
  }
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
  my ($self, $stderr_line, $wheel_id) = @_[OBJECT, ARG0, ARG1];
  # my $server = $self->server;
  # print "pid ", $child->server, " STDERR: $stderr_line\n";
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
  $_[KERNEL]->call('httpd', 'SHUTDOWN');
  $self->clear_httpd;
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

package
  Soviet::Minecraft::Point {
  use Moose;
  use namespace::autoclean;
  has [ qw(x y z) ] => (is => 'ro', isa => 'Num', required => 1);

  sub from_xyz {
    my ($class, $x, $y, $z) = @_;
    $class->new({ x => $x, y => $y, z => $z });
  }

  sub from_arrayref {
    my ($class, $arr) = @_;
    $class->new({ x => $arr->[0], y => $arr->[1], z => $arr->[2] });
  }

  sub xyz {
    return map {; $_[0]->$_ } qw(x y z);
  }

  sub as_string {
    return join q{ }, $_[0]->xyz;
  }

  sub planar_distance_to {
    my ($self, $other) = @_;
    return  sqrt( ($other->x - $self->x) ** 2
                + ($other->z - $self->z) ** 2);
  }
}

1;
