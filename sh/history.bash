# Saves command history into ~/history/bash-*

# TODO(sdh): refactor these into lazy-loaded functions?

# TODO(sdh): teach add_fpath to look at subdirectories and
# add namespaced functions, e.g. sdh::history::file, sdh::history::save

permanent_history_file() {
  echo $HOME/history/$(basename $SHELL)-$(date +%y%m%d)-$$
}

LASTHIST_INDEX=-1

savehistory() {
  local rest="$(sed 's/^ *\([0-9]*\) */\1;/' <<<"$1")"
  local line="${rest%%;*}"
  if [ "$line" -eq "$LASTHIST_INDEX" ]; then return; fi
  LASTHIST_INDEX=$line
  local cmd="${rest#*;}"
  cmd="$(sed ':a;N;$!ba;s/\\/\\\\/g;s/\n/\\n/g' <<<"$cmd")"
  dir="$(sed ':a;N;$!ba;s/\\/\\\\/g;s/\n/\\n/g;s/$/\\$/g' <<<"$PWD")"
  now="$(date +%s)"
  echo "$now \$ $line \$ $$ \$ $LASTSTATUS \$ $dir \$ $USER \$ $cmd" >> $(permanent_history_file)
}

savelasthistory() {
  savehistory "$(history 1)"
}

