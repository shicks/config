export PATH

prepend_to_list -e PATH ~/.cargo/bin
prepend_to_list -e PATH ~/local/bin
prepend_to_list -e PATH ./node_modules/.bin

if [ "$(uname)" = Darwin ]; then
  export SHELL="$(ps -ocomm= -p $$)"
  SHELL=${SHELL/-/} # note: not a fully qualified name.
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

# Init NVM if it's installed
export NVM_DIR="$HOME/.nvm"
source_if_exists "$NVM_DIR/nvm.sh"
source_if_exists "$NVM_DIR/bash_completion"

export PREZTO_DIR="$HOME/.zprezto"
source_if_exists "$PREZTO/init.zsh"