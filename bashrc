# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Now run all the startup stuff:
for a in $HOME/.bash.d/??_*.sh; do
  . $a
done
