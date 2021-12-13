# TODO(sdh): just move the function into here? (the one local var wouldn't work)

init_fzf

case "$(basename "$SHELL")" in
  (zsh)
    # Redefine fzf-history-widget
    # CTRL-R - Paste the selected command from history into the command line
    #   - enter runs, tab edits
    fzf-history-widget() {
      local selected num run
      setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
      selected=( $(fc -l 1 |
        FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS --tac -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort --expect=tab $FZF_CTRL_R_OPTS --query=${(q)LBUFFER} +m" $(__fzfcmd)) )
      local ret=$?
      if [ -n "$selected" ]; then
        run=true
        if [ "${selected[1]}" = "tab" ]; then
          run=false
          selected=(${selected[2,-1]})
        fi
        num=${selected[1]}
        if [ -n "$num" ]; then
          zle vi-fetch-history -n $num
          if $run; then zle accept-line; fi
        fi
      fi
      zle redisplay
      typeset -f zle-line-init >/dev/null && zle zle-line-init
      return $ret
    }

    # Unclobber Alt-C back to capitalize-word.  fzf-cd-widget is useful outside
    # google source repos, but even so it needs to be a different key!
    # TODO(sdh): consider making a new widget that switches behavior based on
    # whether there is anything at the cursor?
    bindkey '\eC' fzf-cd-widget
    bindkey '\ec' capitalize-word

    ;;
esac
