#!/usr/bin/perl

=head1 NAME

cronlog - a logging/repeating wrapper for cron jobs

=head1 SYNOPSIS

    cronlog [OPTIONS] [--] COMMAND [ARGS...]
    cronlog --daemon

=head1 DESCRIPTION

Wrapper for cron jobs that provides a number of nice features, such as
automatic logging to systemd logs (i.e. journalctl), a web server to
view recent execution data and logs, and optionally to request an
on-demand re-run of any controlled jobs.

=head2 Usage

Ensure the path to cronlog and any relevant jobs is in the crontab
path and then simply prepend B<cronlog> (plus any options) to the
command.

    PATH=/path/to/bin:/usr/bin:/bin

    */20 * * * * * cronlog /path/to/somejob
    */5  * * * * * cronlog -s 5m -r 1 ondemand arg

This example defines two jobs: B</path/to/somejob> is a normal job
that runs every 20 minutes (at :00, :20, and :40), but will be logged
to the systemd log.  B<ondemand> passes a few arguments to cronlog:
it will be run roughly every 5 minutes at a random time within the
first four minutes of each window.  It can be manually repeated via
the web UI once in between each automatic run.

=head2 Options

=over 16

=item B<-s DURATION>

Sleeps for a random period of time up to DURATION, which should be a
number with an optional unit (i.e. 's', 'm', 'h', or 'd').  If no unit
is given, seconds are assumed.

=item B<-r COUNT>

Allows the command to be re-run manually COUNT times between each
automatic run.  Manual re-runs are requested via the web UI.

=item B<-d DURATION>

Enforces rate-limiting between manual re-runs.

=item B<-n NAME>

Specifies the name to use for the job in systemd logs.

=back

=cut

use v5.32;
use utf8;
use warnings;
use open qw(:std :utf8);
no feature qw(indirect);
use feature qw(signatures);
no warnings qw(experimental::signatures);

use Text::ParseWords qw/shellwords/;
use File::Basename qw/basename/;

sub main(@) {
  if ($ENV{CRONLOG_INDIRECT}) {
    exit do_indirect(@_);
  }

  if (@_ == 1 && $_[0] eq '--daemon') {
    start_server();
    die "impossible";
  }
  my $name = '';
  my $delay = 0;
  my $repeats = 0;
  my $ratelimit = 0;
  while (@_) {
    $_ = shift;
    unshift @_, $_ unless /^-/;
    last if $_ eq '--' or not /^-/;
    if (/^-n(.*)$/) {
      $name = $1 || shift;
    } elsif (/^-s(.*)$/) {
      $delay = $1 || shift;
    } elsif (/^-r(.*)$/) {
      $repeats = $1 || shift;
    } elsif (/^-d(.*)$/) {
      $ratelimit = $1 || shift;
    } else {
      print STDERR "unknown arg: $_\n";
    }
  }
  # Then run it with systemd-run and one extra level of indirection.
  $name = basename $_[0] unless $name;
  exec('systemd-run', '-u', $name, '--user',
       '-E', "PATH=$ENV{PATH}", '-E', 'CRONLOG_INDIRECT=1',
       '-p', "SyslogIdentifier=$name", @_);
}

sub do_indirect(@) {
  print STDERR "Starting @_\n";
  my $result = system @_;
  my $basename = basename $_[0];
  if (not $result or $? == -1) {
    print STDERR "<3>failed to start: $!\n";
    return 1;
  } elsif ($? & 127) {
    printf STDERR "<3>died with signal %d, with%s coredump\n",
      ($? & 127), ($? & 128 ? '' : 'out');
  } elsif ($?) {
    printf STDERR "<4>exited with code %d\n", $? >> 8;
  }
}

sub start_daemon() {
  # TODO - we don't currently use this, since we want systemd logging.
  use POSIX;
  POSIX::setsid or die "setsid: $!";
  my $pid = fork();
  exit 0 if $pid;
  chdir '/';
  umask 0;
  for (0 .. (POSIX::sysconf (&POSIX::_SC_OPEN_MAX) || 1024)) {POSIX::close $_}
  open(STDIN, "</dev/null");
  open(STDOUT, ">/dev/null");
  open(STDERR, ">&STDOUT");
  start_server();
}

sub start_server() {
  # Start a server on localhost:5678
  use Socket;
  use IO::Socket;

  my $port = 5678; # TODO - allow customization?
  my $server = IO::Socket::INET->new(Proto => 'tcp',
                                     LocalPort => $port,
                                     Listen => SOMAXCONN,
                                     Reuse => 1);
  $server or die "Unable to create server socket: $!";

  while (my $client = $server->accept()) {
    my $clientaddr = $client->peerhost();
    my $clientport = $client->peerport();
    $client->autoflush(1);
    my %request = ();
    {
      # local $/ = Socket::CRLF; # somehow breaks PEN?
      while (<$client>) {
        # method path HTTP/#.#
        chomp;
        if (/\s*(\w+)\s*([^\s]+)\s*HTTP\/(\d.\d)/) {
          $request{METHOD} = uc $1;
          $request{PATH} = $2;
          $request{HTTP_VERSION} = $3;
        }
        # header: value
        elsif (/:/) {
          (my $type, my $val) = split /:/, $_, 2;
          $type =~ s/^\s+//;
          foreach ($type, $val) {
            s/^\s+//;
            s/\s+$//;
          }
          $request{lc $type} = $val;
        }
        # post data
        elsif (/^$/) {
          read($client, $request{CONTENT}, $request{'content-length'})
            if defined $request{'content-length'};
          last;
        }
      }
    }

    next unless defined $request{METHOD};
    print STDERR "$request{METHOD} $request{PATH} from $clientaddr:$clientport\n";
    if ($request{METHOD} eq 'GET') {
      do_get($client, %request);
    } else {
      serve_error($client, 400, 'Bad Request');
    }
    close $client;
  print STDERR "closed\n";
  }
}

sub do_get($client, %request) {
  my ($path, $query) = split_query($request{PATH});
  if ($path eq '/') {
    serve_root($client, $query);
  } elsif ($path =~ m|^/log/(.*)$|) {
    serve_log($client, $1, $query);
  } elsif ($path =~ m|^/run/(.*)$|) {
    serve_run($client, $1, $query);
  } else {
    serve_error($client, 404, 'Not Found', "Not found: $request{PATH}");
  }
}

sub serve_run($client, $job, $query) {
  serve_error($client, 403, "Forbidden");
}

sub split_query($url) {
  $url =~ m|([^?]*)((?:\?.*)?)|;
  my ($path, $querystring) = ($1, $2);
  my $query = {};
  if ($querystring =~ s/^\?//) {
    for my $k (split /&/, $querystring) {
      my $v = ($k =~ s/=(.*)//) ? $1 : '';
      my $oldv = $query->{$k};
      if (defined $oldv) {
        if (ref($oldv) eq 'ARRAY') {
          push @$oldv, $v;
        } else {
          $query->{$k} = [$oldv, $v];
        }
      } else {
        $query->{$k} = $v;
      }
    }
  }
  return ($path, $query);
}

sub get_jobs() {
  my %jobs = ();
  open my $crontab, '-|', 'crontab', '-l';
  while (<$crontab>) {
    chomp;
    my $line = $_; # save line before chopping.
    s/\s*#.*//; # TODO: what about escaped octothorpes?
    if (/^\s*(?:\S+\s+){5}(\s*(?<=[\/ ])cronlog\s+.*)$/) {
      my @words = shellwords($1);
      my $name;
      for (my $i = 1; $i < @words; $i++) {
        my $word = $words[$i];
print STDERR "i=$i: $word\n";
        print STDERR "arg $word\n";
        if ($words[$i] =~ /^-n(.*)$/) {
          $name = $1 || $words[$i + 1];
        print STDERR "-n => $name\n";
          last;
        } elsif ($word eq '--') {
          $name = $words[$i + 1];
        print STDERR "-- => $name\n";
          last;
        } elsif ($word !~ /^-/) {
          $name = $word;
        print STDERR "pos => $name\n";
          last;
        } elsif ($word =~ /^-[srd]$/) {
          $i++;
my $w=@words;
        print STDERR "i <- $i ($w)\n";
        }
      }
print STDERR "NAME=$name\n";
      if (defined $jobs{$name}) {
        print STDERR ($jobs{''} = "Name conflict: $name\n");
        next;
      }
      $jobs{$name} = {name => $name, line => $line, full => \@words};
    }
    next unless /\S+/;
  }
  close $crontab;
  return \%jobs;
}

my $STYLE = <<EOF;
<style>
.log { white-space: pre; font-family: monospace; }
.c1  { font-weight: bold; }
.c30 { color: black; }
.c31 { color: red; }
.c32 { color: green; }
.c33 { color: yellow; }
.c34 { color: blue; }
.c35 { color: magenta; }
.c36 { color: cyan; }
.c37 { color: white; }
.c40 { background-color: black; }
.c41 { background-color: red; }
.c42 { background-color: green; }
.c43 { background-color: yellow; }
.c44 { background-color: blue; }
.c45 { background-color: magenta; }
.c46 { background-color: cyan; }
.c47 { background-color: white; }
</style>
EOF

sub serve_log($client, $job, $query) {
  my $lines = $query->{n} || 50;
  $ENV{SYSTEMD_COLORS} = 'true';
  my $journal = `journalctl -t $job -n $lines`;
  $journal = ansi_html($journal);
  my $more = $lines * 4;
  serve_text($client, <<EOF, 'text/html');
<!DOCTYPE html>
$STYLE
<a href="/log/$job?n=$more">more</a>
<div class="log">$journal</div>
EOF
}

sub ansi_html($str) {
  my @parts = ();
  my $end = '';
  my %fmt = ();
  my %c = ();
  for (split /(?=\e\[[0-9;]*m)/, $str) {
    s/^\e\[([0-9;]*)m//;
    my $codes = $1;
    push @parts, $end if $end;
    %fmt = () unless $codes;
    for my $code (split /;/, $codes) {
      if ($code =~ s/^2//) {
        delete $fmt{$code};
        next;
      }
      my $cat = substr $code, 0, 1;
      delete $fmt{$c{$cat}} if defined $c{$cat} and defined $fmt{$c{$cat}};
      $c{$cat} = $code if length($code) > 1;
      $fmt{$code} = 1;
    }
    my $class = join ' ', map { "c$_" } keys %fmt;
    if ($class) {
      push @parts, "<span class=\"$class\">";
      $end = '</span>';
    }
  }
  push @parts, $end if $end;
  return join '', @parts;
}

sub serve_root($client, $query) {
  # Root: run `crontab -l` and show all controlled jobs.
  my @l = ();
  my $jobs = get_jobs();
  for my $name (keys %$jobs) {
    my $line = $jobs->{$name}->{line};
    push @l, "$name => $line\n";
  }
  # for my $k (keys %$query) {
  #   my $v = $query->{$k};
  #   if (ref($v) eq 'ARRAY') {
  #     for my $vv (@$v) {
  #       push @l, "array $k=$vv\n";
  #     }
  #   } else {
  #     push @l, "scalar $k=$v\n";
  #   }
  # }
  serve_text($client, join "\n", @l);
}

sub serve_text($client, $text, $type = 'text-plain') {
  my $length = length($text);
  my $resp = "HTTP/1.0 200 OK\nConnection: close\nContent-type: $type\nContent-length: $length\n\n$text\n\n";
  print $client $resp;
  print STDERR $resp;
}

sub serve_error($client, $code, $err, $text = $err) {
  my $length = length($text);
  print $client "HTTP/1.0 $code $err\nConnection: close\nContent-type: text-plain\nContent-length: $length\n\n$text\n\n";
}

main(@ARGV);
