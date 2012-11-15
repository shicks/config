# Saves command history into ~/history/bash-*

historyfile() {
  echo $HOME/history/bash-$(date +%y%m%d)-$$
}

case "$(basename $SHELL)" in
  (bash)
    LASTHIST_INDEX=-1

    savehistory() {
      local rest=$(sed 's/^ *\([0-9]*\) */\1;/' <<<"$1")
      local line=${rest%%;*}
      if [ "$line" -eq "$LASTHIST_INDEX" ]; then return; fi
      LASTHIST_INDEX=$line
      local cmd=${rest#*;}
      cmd="$(sed ':a;N;$!ba;s/\\/\\\\/g;s/\n/\\n/g' <<<"$cmd")"
      dir="$(sed ':a;N;$!ba;s/\\/\\\\/g;s/\n/\\n/g;s/$/\\$/g' <<<"$PWD")"
      now="$(date +%s)"
      echo "$now \$ $line \$ $$ \$ $LASTSTATUS \$ $dir \$ $USER \$ $cmd" >> $(historyfile)
    }

    savelasthistory() {
      savehistory "$(history 1)"
    }
    ;;

  (zsh)
    function savelasthistory {
      :
    }
    ;;
esac
