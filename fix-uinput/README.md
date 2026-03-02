# fix-uinput

There are sometimes issues with xkeysnail not being able to write to
/dev/uinput.  This can be particularly problematic in cases where we
have limited root access.

The `fix-uinput` tool is a quick workaround to ensure the block device
has the correct permissions.  It's a golang binary so that we can set
the setuid permissions bit and have it run as root, even if we don't
have general root access.
