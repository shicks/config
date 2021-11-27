export PATH

#prepend_to_list -e PATH ~/.cargo/bin
#prepend_to_list -e PATH ~/local/bin

# Find path components from .sh.d/paths
if [ -d ~/.sh.d/paths ]; then
  for d in $(ls -rd ~/.sh.d/paths/*); do
    if [ -d "$d" ]; then
      PATH=$(readlink -e "$d"):$PATH
    fi
  done
fi

export SHELL="$(ps -ocomm= -q $$)"
export CLICOLOR=1
export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd

export EDITOR=quickemacs
export GIT_EDITOR=$EDITOR
export P4_EDITOR=$EDITOR
export VISUAL=$EDITOR

export COLUMNS

# Export the display for WSL
case "$(uname -a)" in
  (*Microsoft*)
    export DISPLAY=:0
    ;;
esac

if which rbenv > /dev/null 2> /dev/null; then
  eval "$(rbenv init -)"
fi
