# Automatically setting up USB devices

We want several programs to run at boot, as well as when
we replug in devices.

One problem with xkeysnail is that it needs to run as root.
By adding a udev rule (and a `uinput` group with myself in
it), we can make `/dev/uinput` writable by user.  One kink
is that the `uinput` module must be loaded.  I tried adding
`modprobe uinput` to `/etc/rc.local` but this does not seem
to work.  Instead, I appended `uinput` to `/etc/modules`,
which seems to work better.

Another issue is getting the rules to run at the right time.
Specifically, running them on "add" is premature.  Waiting
for "bind" seems to work better, though they don't show up
in `udevadm test`.  The keyboard rule also runs twice for
some reason, so we use a dead-simple lockfile to dedupe.

Udev rules run as root, but many things need to be run as
the user.  We add a quick check for `["$(whoami)" = root]`
and switch to user via `sudo -u`, as well as piping `"$0"`
to `at now` to ensure we're in a more normal environment
(no idea if this actually makes a difference).

To install, copy (or symlink) the two udev rules to
`/etc/udev/rules.d`

Potentially it would be better to do this via systemd
to avoid a handful of issues.
