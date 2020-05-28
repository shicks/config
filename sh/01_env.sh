export PATH

prepend_to_list -e PATH ~/.cargo/bin
prepend_to_list -e PATH ~/local/bin
prepend_to_list -e PATH ./node_modules/.bin

#PATH=$HOME/local/bin:$PATH

export SHELL=$(which "$(ps -ocomm= -q $$)")
export CLICOLOR=1
export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd

export EDITOR=quickemacs
export GIT_EDITOR=$EDITOR
export P4_EDITOR=$EDITOR
export VISUAL=$EDITOR

export COLUMNS

# Init NVM if it's installed
export NVM_DIR="$HOME/.nvm"
source_if_exists "$NVM_DIR/nvm.sh"
source_if_exists "$NVM_DIR/bash_completion"

export PREZTO_DIR="$HOME/.zprezto"
source_if_exists "$PREZTO/init.zsh"
