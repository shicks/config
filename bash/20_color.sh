# Color management functions

function set_ansi_colors {
  unalias color &> /dev/null
  function color { ansi_color "$@"; }
}

function unset_ansi_colors {
  alias color=:
}

case "$(basename $SHELL)" in
  (bash)
    PROMPT_ESCAPE_OPEN='\['
    PROMPT_ESCAPE_CLOSE='\]'
    ;;
  (zsh)
    PROMPT_ESCAPE_OPEN='%{'
    PROMPT_ESCAPE_CLOSE='%}'
    ;;
esac

function ansi_color {
  local o=""
  local c=""
  while [ -n "$1" ]; do
    case "$1" in
      -p)      o=$PROMPT_ESCAPE_OPEN; c=$PROMPT_ESCAPE_CLOSE ;;
      off)     echo -ne "$o\033[00m$c" ;;
      black)   echo -ne "$o\033[01;30m$c" ;;
      red)     echo -ne "$o\033[01;31m$c" ;;
      green)   echo -ne "$o\033[01;32m$c" ;;
      yellow)  echo -ne "$o\033[01;33m$c" ;;
      blue)    echo -ne "$o\033[01;34m$c" ;;
      magenta) echo -ne "$o\033[01;35m$c" ;;
      cyan)    echo -ne "$o\033[01;36m$c" ;;
      white)   echo -ne "$o\033[01;37m$c" ;;
    esac
    shift
  done
}
#ANSI_FIX_TERMINAL='\[\017\033(B\]' # already in shift_in...

# function to send ^V ^O [shift in] and ^V ESC ( B [set G0 to a]
# and ESC ) 0 to set G1 to what we expect it to be...
function shift_in {
  if [ "$TERM" != dumb -a "$TERM" != eterm-color ]; then
    echo -ne '\017\033(B\033)0';
  fi
}

# set a fancy prompt (non-color, unless we know we "want" color)
# (sdh - added the  (from a terminal, ^V^O) to force shift-in every line)

case "$TERM:$COLORTERM" in
  xterm:gnome-terminal) TERM=xterm-color ;;
esac

case "$TERM" in
  *-color*)     set_ansi_colors ;;
  *-256color*)  set_ansi_colors ;;
  *)            unset_ansi_colors ;;
esac

export TERM
