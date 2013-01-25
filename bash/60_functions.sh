
urxclip () {
  # Calls xclip via urxvt/mycopy-osc
  local encoded=$(/usr/bin/base64 -w 0 "$@")
  case "$TERM" in
    (tmux*|screen*)
      printf "\033Ptmux;\033\033]777;mycopy;$encoded\007\033\\"
      ;;
    (rxvt*)
      printf "\033]777;mycopy;$encoded\007"
      ;;
    (*)
      xclip -i
      ;;
  esac
}
