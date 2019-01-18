# Note: The Sdh directory must be copied along with any binary.

use FindBin;
use lib $FindBin::Bin;
use Sdh::Parallel;
use Sdh::Task;
use Sdh::Util;

sub foo :EDITOR {
  print "42\n";
}

sub bar :PARALLEL {
  my $task = shift;
  if ($task->is_done) {
    my $completed = $task->completed;
    my $total = $task->total;
    print "Completed $completed / $total\n";
    return;
  }
  my $arg = $task->job;
  sleep 5;
  return [length($arg), {a=>$arg}];
}

my $t = Sdh::Task->new_('.git');

say \'K', 'merry christmas!';

sub foo {
  while (@_) {
    my $a = shift;
    $a = "@$a" if ref $a eq 'ARRAY';
    print "ARG: $a\n";
  }
}

# @_ = ('a', 'b');
# foo ['foo', @_, 'bar'], z => 42, q => 23;


my @tasks = ('f', 'bar', 'oo', 'quux');

my @result = bar(@tasks);
use Data::Dumper;
print Dumper(\@result);
