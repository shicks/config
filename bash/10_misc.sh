## don't put duplicate lines in the history. See bash(1) for more options
#export HISTCONTROL=ignoredups
## ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

## disable history expansion (i.e. from "!")
set +o histexpand

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# For urxvt/tmux
bind '"\e[33~": backward-kill-word'
bind '"\eOD": backward-word'
bind '"\eOC": forward-word'
