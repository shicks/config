# Simple module framework for queueing tasks, similar to
# git-rebase-todo, but independent of git.  Facilitates
# a model where the primary interaction with the user is
# via EDITOR, though prompts are also supported.

package Sdh::Task;

use strict;
use warnings;

use Attribute::Handlers;
use Carp;
use JSON::PP;

sub ::EDITOR :ATTR(CODE) {
  return unless ref $_[1];
  my $name = *{$_[1]}{NAME};
  print "Wrapping $name\n";
}

# Private factory method to make a new instance.
sub new_($$) {
  my $class = shift;
  my $file = shift;
  my $task_file =
      $file eq '.git' ? Sdh::Task::Git->new() : Sdh::Task::Plain->new($file);
  return bless {file => $task_file}, $class;
}

# Inserts a new task to run before all others.
sub run_next {
  my $self = shift;
  my @task = @_;
  if ($task[0]) { # TODO(sdh): if is a globref...

  }
}

# Inserts a new task to run after all existing tasks.
sub run_last {
  my $self = shift;
  my @task = @_;
}


# interface File {
#   boolean exists()
#   ref get_data(boolean lock)
#   void set_data(ref? data, boolean unlock)
# }
# question: should we take tasks as part of data?!?
#  -> special case for e.g. pick, etc?

# Class for operating with a plain queue file.
package Sdh::Task::Plain;

use strict;
use warnings;

sub new($$) {
  my ($class, $file) = @_;
  return bless {file => $file}, $class;
}



# Class for operating with a Git rebase TODO file.
package Sdh::Task::Git;

use strict;
use warnings;
use Carp;

sub new($) {
  my $class = shift;
  my $top = 1; #Sdh::Task::out_ 'git rev-parse --show-toplevel';
  croak 'Must be run from within a git repository' unless $top;
  return bless {top => $top}, $class;
}

1;
