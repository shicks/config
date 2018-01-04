## don't put duplicate lines in the history. See bash(1) for more options
#HISTCONTROL=ignoredups
## ... and ignore same sucessive entries.
HISTCONTROL=ignorespace:erasedups

## disable history expansion (i.e. from "!")
set +o histexpand

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"
