# Utilities for parallelization
# TODO(sdh): perldoc this - it's actually a nice API...!

package Sdh::Parallel;

use strict;
use warnings;

use Attribute::Handlers;
use Carp;
use JSON::PP;

our $DEFAULT_PROCS = 16;

# Basic usage:
# sub foo :PARALLEL(20) {
#   return if $_[0]->is_done;
#   mt $arg = $_[0]->job;
#   # do stuff
#   return $data;
# }

# foo($a, $b, $c) -> (foo($a), foo($b), foo($c))

sub ::PARALLEL :ATTR(CODE) {
  my $globref = $_[1];
  my $procs = $_[4] ? $_[4]->[0] : $DEFAULT_PROCS;
  my $original = *{$globref}{CODE};
  no warnings 'redefine';
  *{$globref} = sub {
    return run_parallel_($original, $procs, @_);
  };
}

sub run_parallel_ {
  my $original = shift;
  my $numprocs = shift;
  my $numjobs = @_;

  my $parallel = bless {
    submitted => 0,
    completed => 0,
    jobs => \@_,
    results => [],
    done => -1,
    status => -1,
   }, 'Sdh::Parallel';

  my %procs = ();
  my %pipes = ();
  while ($parallel->{'submitted'} < $numjobs or %procs) {
    # Fill up the buffer of processes
    while ($parallel->{'submitted'} < $numjobs and keys %procs < $numprocs) {
      my ($reader, $writer); # open up a pipe to communicate result
      pipe($reader, $writer);
      defined (my $pid = fork()) or croak "Could not fork: $!";
      if ($pid) {
        # parent process: add to queue
        close $writer;
        $procs{$pid} = $parallel->{'submitted'}++;
        $pipes{$pid} = $reader;
      } else {
        close $reader;
        $parallel->{'done'} = -1;
        my $result = $original->($parallel); # recover and exit nonzero?
        print $writer encode_json($result);
        close $writer;
        exit(0);
      }
    }
    # Wait for any to complete
    my $pid = waitpid -1, 0;
    $parallel->{'status'} = $?;
    next unless defined ($parallel->{'done'} = $procs{$pid});
    $parallel->{'completed'}++;
    my $reader = $pipes{$pid};
    local($/) = undef;
    my $result = decode_json(<$reader>);
    close $reader;
    delete $procs{$pid};
    delete $pipes{$pid};
    $parallel->{'results'}->[$parallel->{'done'}] = $result;
    $original->($parallel);
  }
  return @{$parallel->{'results'}};
}

# Instance methods of object passed to parallel functions
sub is_done {
  return $_[0]->{'done'} >= 0;
}

sub job {
  my $self = shift;
  my $index = $self->is_done ? $self->{'done'} : $self->{'submitted'};
  return $self->{'jobs'}->[$index];
}

sub total {
  my $count = @{$_[0]->{'jobs'}};
  return $count;
}

sub completed {
  return $_[0]->{'completed'};
}


1;
