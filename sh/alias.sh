# Lazily set up dircolors, etc.
alias ls=init_ls

alias javac="javac -Xlint -Xlint:-serial -Werror -g"

alias gut=git
alias got=git

alias grc="git rebase --continue"
alias gra="git rebase --abort"

alias ::='GITEDITOR=: EDITOR=: P4EDITOR=: PAGER=cat '

alias unletterbox="mplayer -vf crop=704:352:8:64"

alias dirs="dirs -v"

if which fzf-tmux &> /dev/null; then
  alias fzf=fzf-tmux
fi
