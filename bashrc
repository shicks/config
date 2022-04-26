# If not running interactively, don't do anything (same as [ -z $PS1 ] ?)
case $- in
  *i*) ;;
    *) return;;
esac

function whisper {
  if [ -n "$VERBOSE" ]; then echo "$@"; fi
}

function run_init_scripts {
  local file
  for file in $HOME/.sh.d/*; do
    case "$file" in
      (*.sh)   whisper "sourcing $file"; source "$file" ;;
      (*.bash) whisper "sourcing $file"; source "$file" ;;
    esac
  done
}
run_init_scripts

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
[ -s "$HOME/.cargo/env" ] && \. "$HOME/.cargo/env"
