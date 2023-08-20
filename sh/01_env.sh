export PATH

# Find path components from .sh.d/paths (which is symlinks to dirs)
if [ -d ~/.sh.d/paths ]; then
  for d in $(ls -rd ~/.sh.d/paths/*); do
    if [ -d "$d" ]; then
      PATH=$(readlink -e "$d"):$PATH
    fi
  done
fi

prepend_to_list -e PATH ~/.cargo/bin
prepend_to_list -e PATH ~/local/bin
prepend_to_list -e PATH ./node_modules/.bin

if [ "$(uname)" = Darwin ]; then
  export SHELL="$(ps -ocomm= -p $$)"
  SHELL=${SHELL/-/} # note: not a fully qualified name.
  if [ -n "${SHELL##/*}" ]; then # Ensure it starts with /
    SHELL=$(which "$SHELL")
  fi
else
  export SHELL=$(which "$(ps -ocomm= -q $$)")
fi

if [ "$(hostname)" = briannas-mbp.lan ]; then
  # for whatever reason this doesn't work right now.
  export REPO_SEP=NONE
fi

export CLICOLOR=1
export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd

export EDITOR=quickemacs
export GIT_EDITOR=$EDITOR
export P4_EDITOR=$EDITOR
export VISUAL=$EDITOR

export COLUMNS # needed for mac

# Export the display for WSL
case "$(uname -a)" in
  (*Microsoft*)
    export DISPLAY=:0
    ;;
esac

if which rbenv > /dev/null 2> /dev/null; then
  eval "$(rbenv init -)"
fi

# Init NVM if it's installed
export NVM_DIR="$HOME/.nvm"
source_if_exists "$NVM_DIR/nvm.sh"
source_if_exists "$NVM_DIR/bash_completion"

export PREZTO_DIR="$HOME/.zprezto"
source_if_exists "$PREZTO/init.zsh"

if [ "$(hostname)" = giskard ]; then
  export DISPLAY=DESKTOP-6SBOT1P:0.0
  export LIBGL_ALWAYS_INDIRECT=1
fi
