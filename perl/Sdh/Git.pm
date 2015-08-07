package Sdh::Git;

use strict;

# my $git = Sdh::Git->new;
sub new {
  my $top = `git rev-parse --show-topdir`; chomp $top;
  die "Could not find topdir" unless $top;
  bless {
    'top' => $top
  , 'verbosity' => 0
  , 'parents' => {}
  }, $_[0];
}

sub debug {
  my $git = shift;
  my $level = shift;
  print STDERR "@_" unless $level > $git->{'verbosity'};
}

sub run {
  # Correctly quotes the string.
  local $_;
  my $git = shift;
  foreach (@_) {
    if (/\s|["'$`]/) {
      if (not /'/) {
        $_ = "'$_'";
      } else {
        s/\\/\\\\/g; s/"/\\"/g; s/`/\\`/g; s/\$/\\\$/g;
        $_ = "\"$_\"";
      }
    }
  }
  $git->debug(1, "\e[1;30m> @_\e[m\n");
  system "@_";
  return $? != 0;
}

sub out {
  # Correctly quotes the string.
  local $_;
  my $git = shift;
  foreach (@_) {
    if (/\s|["'$`]/) {
      if (not /'/) {
        $_ = "'$_'";
      } else {
        s/\\/\\\\/g; s/"/\\"/g; s/`/\\`/g; s/\$/\\\$/g;
        $_ = "\"$_\"";
      }
    }
  }
  $git->debug(1, "\e[1;30m> @_\e[m\n");
  my $ret = `@_`;
  $git->debug(2, "\e[1;30m= $ret\e[m\n");
  return $ret;
}

sub rev_parse {
  my ($git, $commit) = @_;
  return $git->out('git', 'rev-parse', $commit, '2>/dev/null');
}

sub parents {
  my ($git, $commit) = @_;
  my $cache = $git->{'parents'};
  return @{$cache->{$commit}} if $cache->{$commit};
  my @parents = ();
  my $num = 1;
  my $parent;
  do {
    $parent = $git->rev_parse("$commit^");
    push @parents, $parent if $parent;
  } while ($parent);
  $cache->{$commit} = \@parents;
  return @parents;
}

sub current_branch {
  my ($git, $commit) = @_;
  my $top = $git->{'top'};
  open HEAD, "$top/.git/HEAD" or die "Could not open $top/.git/HEAD";
  local ($_) = <HEAD>;
  chomp $_;
  if (s/^ref:\s+//) {
    die "Unknown ref: $_" unless s|^refs/heads/||;
  }
  return $_;
}

# TODO(sdh): support for --continue/--reset/--skip via a command queue


1 # return true value at end.
