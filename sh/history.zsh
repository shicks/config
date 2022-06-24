permanent_history_file() {
  echo $HOME/history/$(basename "$SHELL")-$(date +%y%m%d)-$$
}

function permanent_history_save_command {
  let _permanent_history_line++
  local cmd="$1"
  if [ -z "$cmd" ]; then return; fi
  # NOTE: newer MacOS sed doesn't like semicolons to terminate labels,
  # so we use newlines instead.
  _permanent_history_cmd="$(sed ':a
N
$!ba
s/\\/\\\\/g
s/\n/\\n/g' <<<"$cmd")"
  _permanent_history_dir="$(sed ':a
N
$!ba
s/\\/\\\\/g
s/\n/\\n/g
s/$/\\$/g' <<<"$PWD")"
  _permanent_history_start="$(date +%s)"
  _permanent_history_file="$(permanent_history_file)"
}

function permanent_history_write {
  local laststatus="$(print -P '%?')"
  if [ -z "$_permanent_history_cmd" ]; then return; fi
  local line="$_permanent_history_line"
  local start="$_permanent_history_start"
  local now="$(date +%s)"
  local dur="$((now - start))"
  local dir="$_permanent_history_dir"
  local cmd="$_permanent_history_cmd"
  echo "$start:$dur \$ $line \$ $$ \$ $laststatus \$ $dir \$ $USER \$ $cmd" \
    >>| "$_permanent_history_file"
}

autoload -Uz add-zsh-hook
add-zsh-hook preexec permanent_history_save_command
add-zsh-hook precmd permanent_history_write
