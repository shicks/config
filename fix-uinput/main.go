package main

import (
	"log"
	"os"
	"os/user"
	"strconv"
)

// NOTE: the corresponding binary should be copied to bin/fix-uinput
//       and setuid with `chown root:uinput`, `chmod ug+s`
func main() {
	u, err := user.Lookup("root")
	if err != nil {
		log.Fatal("could not find user 'root'", err)
	}
	g, err := user.LookupGroup("uinput")
	if err != nil {
		log.Fatal("could not find group 'uinput'", err)
	}
	gid, err := strconv.Atoi(g.Gid)
	if (err != nil) {
		log.Fatal("non-numeric gid")
	}
	uid, err := strconv.Atoi(u.Uid)
	if (err != nil) {
		log.Fatal("non-numeric uid")
	}
  if err := os.Chown("/dev/uinput", uid, gid); err != nil {
		log.Fatal("Could not chown /dev/uinput", err)
	}
	if err := os.Chmod("/dev/uinput", 0660); err != nil {
		log.Fatal("Could not chmod /dev/uinput")
	}
}

// NO LONGER NEEDED:
