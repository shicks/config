ensure_function google_prompt_command
ensure_function google_cd_hook

ensure_function shift_in
ensure_function set_prompt
ensure_function drawlinebreak
ensure_function savelasthistory
ensure_function save_dir

case "$(basename $SHELL)" in

  (bash)
    PROMPT_COMMAND='LASTSTATUS=$?; shift_in; set_prompt;
                    savelasthistory; google_prompt_command; drawlinebreak'

    ## NOTE: order matters here (for unsymlink?)
    # CD_HOOK='save_dir; unsymlink; google_cd_hook'
    CD_HOOK='save_dir; google_cd_hook'
    eval $CD_HOOK
    ;;

  (zsh)
    function precmd {
      LASTSTATUS=$? # TODO(sdh): print -P '%?'
      shift_in
      set_prompt
      end_timer # TODO(sdh): record time in history?
      savelasthistory # TODO(sdh): move this to preexec?
      google_prompt_command
      drawlinebreak
    }
    function chpwd {
      google_cd_hook
    }
    function preexec {
      # TODO(sdh): possibilities - record command here, set up output capture
      show_exec_time
      case $TERM in
        (tmux*)
          local cmd="$1"
          cmd=$(echo $cmd |
                perl -ne 'if (/--port[= ](\d+)/) { print "port=$1" } else { print; exit 1 }')
          if [ -z "${cmd##port=*}" ]; then
            cmd=${cmd#port=}
            if [ -n "$GIT_BRANCH" ]; then
              cmd="${GIT_BRANCH%%:*}::$cmd"
            else
              cmd=":$cmd:"
            fi
          else
            cmd=${cmd/git /git-}
            cmd=${cmd%% *}
            cmd=${cmd##*/}
            # TODO(sdh): record ports, working dirs, etc?
            if [ -z "${cmd%%emacs*}" ]; then
              cmd=+
            elif [ "${#cmd}" -gt 10 ]; then
              cmd=...
            fi
            cmd="[$cmd]"
          fi
          set_title_term "$cmd"
          ;;
      esac
      start_timer
    }
    chpwd
    ;;
esac



# no more hgit...
#alias hgit='git home'
#complete -o bashdefault -o default -o nospace -F _git hgit 2>/dev/null \
#        || complete -o default -o nospace -F _git hgit
