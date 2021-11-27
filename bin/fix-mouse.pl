#!/usr/bin/perl

use strict;
use warnings;

sub error {
    print STDERR "Error: @_\n";
    exit 1;
}

################################################################
# Goal: natural scrolling

print "Setting up natural scrolling.\n";
open XINPUT, '-|', 'xinput list';
my $done = 0;
while (<XINPUT>) {
  #print;
  if (/\s*↳?\s*(.*Mouse.*)id=(\d+)/) {
    my ($name, $id) = ($1, $2);
    my $prop = '';
    open LIST, '-|', "xinput list-props $id";
    while (<LIST>) {
      $prop = $1 if /Natural Scrolling Enabled\ \((\d+)\)/;
    }
    close LIST;
    if ($prop) {
      print "  mouse: $name\n";
      system '/usr/bin/xinput', 'set-prop', $id, $prop, '1';
      $done = 1;
    } else {
      print "  no natural scrolling prop: $name\n";
    }
      #error("Could not find natural scrolling property") unless $prop;

    if ($name =~ /VerticalMouse/) {
      # Given that we got natural scrolling enabled above, we don't need to
      # fix that here.  Instead, just change the '2' to '3' so that the middle
      # side button behaves like a right click.  We could change the bottom
      # side button to be something different???  It used to be forward before
      # the top-shoulder button was added.
      print "    swapping buttons\n";
      system qw(/usr/bin/xinput set-button-map), $id, qw/1 3 10 4 5 6 7 9 2 8/;
    }
  }
}
close XINPUT;
error("Could not find mouse with natural scrolling property") unless $done;

# NOTE: this should have been done by
#       /usr/share/X11/xorg.conf.d/80-glinux-evoluent-verticalmouse4.conf
##system qw/xinput set-button-map/, $mouse, qw/1 3 8 4 5 6 7 9 2 10/;
# NOTE: the above seems to break the side buttons, but Natural Scrolling
# is sufficient when it works.
