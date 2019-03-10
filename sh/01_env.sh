export PATH

prepend_to_list -e PATH ~/.cargo/bin
#prepend_to_list -e PATH ~/local/bin
PATH=$HOME/local/bin:$PATH

export SHELL="$(ps -ocomm= -q $$)"
export CLICOLOR=1
export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd

export EDITOR=quickemacs
export GIT_EDITOR=$EDITOR
export P4_EDITOR=$EDITOR
export VISUAL=$EDITOR

export COLUMNS
