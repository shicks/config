# If not running interactively, don't do anything
[ -z "$PS1" ] && return

function whisper {
  if [ -n "$VERBOSE" ]; then echo "$@"; fi
}

function run_init_scripts {
  local file
  for file in $HOME/.sh.d/*; do
    case "$file" in
      (*.sh)  whisper "sourcing $file"; source "$file" ;;
      (*.zsh) whisper "sourcing $file"; source "$file" ;;
    esac
  done
}
run_init_scripts
