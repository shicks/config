export PATH="$HOME/local/bin:$PATH"

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

export SHELL=$(which bash)

# Now run all the startup stuff:
for a in $HOME/.bash.d/??_*.sh; do
  . $a
done

export CLICOLOR=1
export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
