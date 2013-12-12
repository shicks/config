#!/bin/sh

xflock4 &
dbus-send --system --print-reply \
          --dest=org.freedesktop.UPower \
          /org/freedesktop/UPower \
          org.freedesktop.UPower.Suspend
