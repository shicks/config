# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).

BASH_COMPLETION_DIR=$HOME/.bash.d/completion
BASH_COMPLETION_COMPAT_DIR=$HOME/.bash.d/completion

[ -f /etc/bash_completion ] &&
   . /etc/bash_completion
