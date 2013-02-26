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
      LASTSTATUS=$?
      shift_in
      set_prompt
      end_timer # TODO(sdh): record time in history?
      savelasthistory # TODO(sdh): move this to preexec?
      google_prompt_command
      drawlinebreak
    }
    function chpwd {
      save_dir
      google_cd_hook
    }
    function preexec {
      # TODO(sdh): possibilities - record command here, set up output capture
      case $TERM in
        (tmux*) set_title_term "..." ;;
      esac
      start_timer
    }
    chpwd
    ;;
esac


#eval $CD_HOOK

# no more hgit...
#alias hgit='git home'
#complete -o bashdefault -o default -o nospace -F _git hgit 2>/dev/null \
#        || complete -o default -o nospace -F _git hgit
