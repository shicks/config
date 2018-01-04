export PATH

prepend_to_list -e PATH ~/.cargo/bin
prepend_to_list -e PATH ~/local/bin

export SHELL="$(ps -ocomm= -q $$)"
export CLICOLOR=1
export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd
