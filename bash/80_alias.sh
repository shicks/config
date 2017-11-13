# enable color support of ls and also add handy aliases
# WARNING: enabling this can cause multi-second delays due to NFS latency
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

alias javac="javac -Xlint -Xlint:-serial -Werror -g"

alias kf="killall npviewer.bin"

alias myip="ifconfig eth0 | awk '/inet addr/ { print substr(\$2, 6); }'"

alias kill-devapp="ps -ef | grep DevAppServerMain | grep -v KickStart | grep -v grep | awk '{print \$2}' | xargs kill"

#alias psd=pushd
#alias ppd=popd

#alias emacs="emacsclient -c -a ''"

alias gut=git
alias got=git

alias ::='EDITOR=: P4EDITOR=: '
alias :c='PAGER=cat '

alias unletterbox="mplayer -vf crop=704:352:8:64"

alias dirs="dirs -v"

if which fzf-tmux &> /dev/null; then
  alias fzf=fzf-tmux
fi
