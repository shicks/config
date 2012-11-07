# If this is an xterm set the title to user@host:dir
case "$TERM" in
  xterm*)
    function set_title {
      echo -ne "\033]0;$1\007";
    }
    ;;
  rxvt*)
    function set_title {
      echo -ne "\033]0;$1\007\033]777;tabbedex;set_tab_name;$1\007"
    }
    ;;
  screen*)
    function set_title {
      echo -ne "\033]0;$1\007\033]777;tabbedex;set_tab_name;$1\007"
      #echo -ne "\032,$1\015"
      echo -ne "\033k$1\033\\"
    }
    ;;
  *) function set_title { :; }
esac
