# Common utilities for tools.

package Sdh::Util;

use strict;
use warnings;

use Carp;
use Exporter;
use IO::Select;
use IO::String;
use IPC::Open3 qw/open3/;
use Symbol;

our @ISA = qw/Exporter/;
our @EXPORT = qw/say shout whisper/;
our @EXPORT_OK = qw//;

our $LOG_LEVEL = defined $ENV{'VERBOSE'} ? $ENV{'VERBOSE'} : 1;

# TODO(sdh): consider reading and modifying @ARGV to remove
#   any -q and -v flag and modify the log level accordingly?
# Optionally make it explicit with a function set_log_level(\@ARGV);

sub say;
sub shout { say \'0', @_; }
sub whisper { say \'2', @_; }

my %COLORS = (
  'k' => 0, # black
  'r' => 1, # red
  'g' => 2, # green
  'y' => 3, # yellow
  'b' => 4, # blue
  'm' => 5, # magenta
  'c' => 6, # cyan
  'w' => 7, # white
 );
# Additional symbols:
#   uppercase=bold, /?=bg
#   i=italic, s=standout
#   u=underline, v=reverse
#   0-9=log level
#   n=no newline at end
#   -=reset colors
#   e=stderr
#   TODO - q=quote words
sub say {
  my $reset = '';
  my $newline = "\n";
  my $printed = 0;
  my $level = 1;
  my $fh = *STDOUT;
  local $_;
  while (@_) {
    my $arg = shift;
    if (ref($arg) eq 'SCALAR') {
      $_ = $$arg;
      $fh = *STDERR if s/e//g;
      my $len = length $_;
      while ($len) {
        $level = int($&) if s/^\d//;
        my $e = '';
        $e = "$e;1" if s/^[KRGYBMCW]/lc($&)/e;
        $e = "$e;3$COLORS{$&}" if s/^[krgybmcw]//;
        $e = "$e;4$COLORS{$1}" if s|^/([krgybmcw])||;
        $e = "$e;3" if s/^i//;
        $e = "$e;4" if s/^u//;
        $e = "$e;5" if s/^s//;
        $e = "$e;7" if s/^v//;
        if ($e) {
          $reset = "\e[m"; print $fh "\e[${e}m";
        }
        if (s/^-//) {
          $reset = ''; print $fh "\e[m";
        }
        $newline = '' if s/^n//;
        s/^.// if length $_ == $len;
        $len = length $_;
      }
    } elsif ($level <= $LOG_LEVEL) {
      $printed = 1;
      print $fh $arg if $arg;
    }
  }
  $newline = '' unless $printed;
  print $fh "$reset$newline";
}

################################################################


# IO:
#   $`1Kq    original command (cannot be redirected)
#   <-       stdin
#   >-`1     stdout
#   2>!`0R   stderr
#   ?!       status (exit code), -1 if signal or failure
# DESTINATIONS:
#   -        /dev/null
#   filename file
#   $        read next argument: string=file, ref=string
#   !        die if nonempty
#   & (internal only, for 2>) merged with >

# run ['foo', 'bar'], '$
sub run {
  my $cmd = shift;
  my %spec = (
    '$' => ['', '1Kq', ''],
    '<' => ['-', '', '', '', ''],
    '>' => ['-', '1', '', '', ''],  # TODO(sdh): support appending?
    '2>' => ['!', '0R', '', '', ''],
    '?' => ['!', '', '']);
  while (@_) {
    local($_) = shift;
    /^([\$<>?]|[&2]>)([^`]*+)(?:`([^`]*))?+(?:`(.*))?+$/ or croak "Bad spec: '$_'";
    my ($fd, $dst, $fmt, $pre) = ($1, $2, $3, $4);
    if ($fd eq '&>') {
      $fd = '>';
      $spec{'2>'} = [];
    }
    $spec{$fd} = [$dst, $fmt, $pre];
    if ($dst eq '$') {
      $spec{$fd}->[3] = shift;
    } elsif ($dst !~ /^[-!^]$/) {
      $spec{$fd}->[3] = $spec{$fd}->[0];
      $spec{$fd}->[0] = '$';
    }
  }

  # Open the things.
  my ($inh, $outh, $errh);
  if ($spec{'2>'}->[0]) {
    $errh = gensym();
  }
  my @args = ();
  if (ref $cmd eq 'ARRAY') {
    @args = @{$cmd};
    $cmd = shift @args;
  }
  say \($spec{'$'}->[1]), $spec{'$'}->[2], $cmd, @args;

  my $pid;
  eval{
    $pid = open3($inh, $outh, $errh, $cmd, @args);
  };
  die $@ if $@;
  my $rd = IO::Select->new; # create a select object
  my $wr = IO::Select->new; # create a select object

  my ($inp, $outp, $errp);
  my $inb = '';

  # Prepare the destinations.
  my @arr = @{$spec{'<'}};
  if ($arr[0] eq '-') {
    close $inh;
  } else {
    $rd->add($inh);
    if (ref $arr[3] eq 'SCALAR') {
      $inp = IO::String->new($arr[3]);
    } else {
      open $inp, $arr[3];
    }
  }

  @arr = @{$spec{'>'}};
  $wr->add($outh);
  if ($arr[0] ne '$' or ref $arr[3] eq 'SCALAR') {
    $outp = IO::String->new($arr[3]);
    print "STRING BUF:  ", $outp->string_ref;
  } else {
    open $outp, ">$arr[3]";  # TODO(sdh): append?
  }

  @arr = @{$spec{'2>'}};
  if (@arr) {
    $wr->add($errh);
    if ($arr[0] ne '$' or ref $arr[3] eq 'SCALAR') {
      $errp = IO::String->new($arr[3]);
      print STDERR "pout = $arr[3]\n";
      print STDERR "errp = $errp = $$errp\n";
      print "STRING BUF:  ", $errp->string_ref;
    } else {
      open $errp, ">$arr[3]";  # TODO(sdh): append?
    }
  }


  print "IN: $inh     OUT: $outh     ERR: $errh\n";
  print "PID was $pid\n";

  # $sel->can_read will block until there is data available
  # on one or more fhs
  while ($rd->handles or $wr->handles) {
    print STDERR "rd: " . @$rd . " wr: " . @$wr;
    my @ready = ($rd->can_write(0), $wr->can_read(0));
    print STDERR "READY: @ready\n";
    # now we have a list of all fhs that we can read from
    foreach my $fh (@ready) { # loop through them

      # First handle STDIN
      if ($fh == $inh) {
        print STDERR "WRITING\n";
        $inb = <$inp> unless $inb;
        if (length $inb) { # buffered input left over
          my $written = syswrite $fh, $inb;
          pr_($spec{'<'}, substr($inb, 0, $written));
          $inb = substr($inb, $written);
        } else {
          $rd->remove($fh);
          close $fh;
        }
        next;
      }
      print STDERR "READING\n";

      # Next do STDOUT/STDERR
      my $line;
      my $len = sysread $fh, $line, 4096;
      print STDERR "LEN=$len\n";
      if (not defined $len) { # read error
        croak "Error from child: $!\n";
      } elsif ($len == 0) { # done reading
        $wr->remove($fh);
        next;
      } else { # read data
        if ($fh == $outh) {
          print $outp $line;
          pr_($spec{'>'}, $line);
        } elsif($fh == $errh) {
          print $errp $line;
          pr_($spec{'2>'}, $line);
        } else {
          croak "Shouldn't be here\n";
        }
      }
    }
  }
  print STDERR "DONE\n";

  # Clean up the job and then figure out what to return.
  waitpid $pid, 0;
  my $status = $?;
  if ($status & 127) {
    print STDERR "STATUS: $status\n";
    $status = -1;
  } elsif ($status != -1) {
    $status >>= 8;
  }

  my $ret = undef;

  croak "exit" if $spec{'>'}->[0] eq '!' and length ${$outp->string_ref};
  croak "exit" if $spec{'2>'}->[0] eq '!' and length ${$errp->string_ref};
  croak "exit" if $spec{'?'}->[0] eq '!' and $status;

  $ret = $$outp->string_ref if $spec{'>'}->[0] eq '^';
  $ret = $$errp->string_ref if $spec{'2>'}->[0] eq '^';
  $ret = $status if $spec{'?'}->[0] eq '^';

  close $inp if $inp;
  close $outp if $outp;
  close $errp if $errp;

  print STDERR "ret: $ret\n";
  return $ret;
}

sub pr_ {
  my $arr = shift;
  $arr->[4] .= shift;
  while ($arr->[4] =~ s/^([^\n]*)\n//) {
    say \($arr->[1]), $arr->[2], $1;
  }
}


# Takes a command (either string or array ref) and optional
# named parameters.  Returns the exit code.
# sub run {
#   my @cmd = (shift);
#   if (ref $cmd[0] eq 'ARRAY') {
#     @cmd = @{$cmd[0]};
#   }
#   # TODO(sdh): options to *not* chomp output for out()
#   my $show = 1;
#   my $echo = 2;
#   my $err = 2;
#   while (@_) {
#     my ($k, $v) = (shift, shift);
#     if ($k eq 'show') { $show = $v; }
#     elsif ($k eq 'echo') { $echo = $v; }
#     elsif ($k eq 'err') { $err = $v; }
#     else { croak "Bad option $k"; }
#   }
#   say \"${show}K", @cmd;
#   # NOTE: can't quiet echo if list.
#   if ($echo > $LOG_LEVEL and @cmd == 1) {
#     @cmd = ("$cmd[0] >/dev/null");
#   }
#   if ($err > $LOG_LEVEL and @cmd == 1) {
#     @cmd = ("$cmd[0] 2>/dev/null");
#   }
#   system @cmd;
#   if ($? == -1) {
#     shout \'eR', "Failed to execute @cmd";
#     return -1;
#   } else if ($? & 127) {
#     my $dump = $? & 128 ? 'with' : 'without';
#     my $signal = $? & 127;
#     shout \'eR', "Child died with signal $signal $dump coredump";
#     return -1;
#   } else {
#     return ($? >> 8);
#   }
# }

# # Takes a command (either string or array ref) and optional
# # named parameters.  Returns the output.
# sub out {
#   my @cmd = (shift);
#   if (ref $cmd[0] eq 'ARRAY') {
#     @cmd = @{$cmd[0]};
#     # TODO(sdh): quotate elements?!? will allow silencing above, too
#   }
#   my $show = 1;
#   my $echo = 2;
#   my $err = 2;
#   my $chomp = 1;
#   while (@_) {
#     my ($k, $v) = (shift, shift);
#     if ($k eq 'show') { $show = $v; }
#     elsif ($k eq 'echo') { $echo = $v; }
#     elsif ($k eq 'err') { $err = $v; }
#     elsif ($k eq 'chomp') { $chomp = $v; }
#     else { croak "Bad option $k"; }
#   }
#   say \"${show}K", @cmd;
#   # NOTE: can't quiet echo if list.
#   if ($echo > $LOG_LEVEL and @cmd == 1) {
#     @cmd = ("$cmd[0] >/dev/null");
#   }
#   if ($err > $LOG_LEVEL and @cmd == 1) {
#     @cmd = ("$cmd[0] 2>/dev/null");
#   }
#   system @cmd;
#   if ($? == -1) {
#     shout \'eR', "Failed to execute @cmd";
#     return -1;
#   } else if ($? & 127) {
#     my $dump = $? & 128 ? 'with' : 'without';
#     my $signal = $? & 127;
#     shout \'eR', "Child died with signal $signal $dump coredump";
#     return -1;
#   } else {
#     return ($? >> 8);
#   }
# }

# # *Sdh::Util::Cmd::new = sub {
# #   my $class = shift;
# #   my @cmd = @_;
# #   return bless \@cmd, $class;
# # };

# # # Runs the command, returns a string.
# # *Sdh::Util::Cmd::out = sub {
# #   my $self = shift;
# #   my $show = 1;
# #   my $echo = 2;
# #   while (@_) {
# #     my $key = shift;
# #     $show = shift if $key eq 'show';
# #     $echo = shift if $key eq 'echo';
# #     # TODO(sdh): error message if anything else
# #   }
# #   my @cmd = @{$self};
# #   say \"${show}K", @cmd;
# #   my $out = `@cmd`;
# #   say \"${echo}Ki", $out;
# #   return $out;
# # };

# # # Runs the command, returns a string.
# # *Sdh::Util::Cmd::out = sub {
# #   my $self = shift;
# #   my @cmd = @{$self->{'cmd'}};
# #   my $show = defined $self->{'show'} ? $self->{'show'} : 1;
# #   my $echo = defined $self->{'echo'} ? $self->{'echo'} : 1;
# #   say \"${show}K", @cmd;
# #   my $out = `@cmd`;
# #   say \"${echo}Ki", $out;
# #   return $out;
# # };

# # say(1, 'K/b')(
# # cmd("cd $whatever")->show(1)->echo(2)->out();
# # run \'<1>2@', "cd $whatever";
# # cmd("git status").say(1).echo(1)


# # # Runs a command.  If the final char is a | then the output is
# # # returned.  If the final char is a
# # sub run {

# # }
