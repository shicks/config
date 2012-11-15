
case "$(basename $SHELL)" in
  (bash)
    ## don't put duplicate lines in the history. See bash(1) for more options
    #export HISTCONTROL=ignoredups
    ## ... and ignore same sucessive entries.
    export HISTCONTROL=ignoreboth

    ## disable history expansion (i.e. from "!")
    set +o histexpand

    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    shopt -s checkwinsize

    # make less more friendly for non-text input files, see lesspipe(1)
    [ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

    # For urxvt/tmux
    bind '"\eOD": backward-word'
    bind '"\eOC": forward-word'
    ;;

  (zsh)
    # Configuration copied from zsh-newuser-install
    HISTFILE=~/.histfile
    HISTSIZE=1000
    SAVEHIST=1000
    setopt SH_WORD_SPLIT
    bindkey -e
    # End of lines configured by zsh-newuser-install
    # The following lines were added by compinstall
    zstyle :compinstall filename '/usr/local/google/home/sdh/.zshrc'

    autoload -Uz compinit
    compinit
    # End of lines added by compinstall

    #####
    # TODO(sdh) subword-forward and subword-backward???

    # For urxvt/tmux
    bindkey '\eOd' backward-word  # C-<left> outside tmux
    bindkey '\eOD' backward-word  # C-<left> inside tmux
    bindkey '\e\eD' backward-word # M-<left>
    bindkey '\eOc' forward-word  # C-<right> outside tmux
    bindkey '\eOC' forward-word   # C-<right> inside tmux
    bindkey '\e\eC' forward-word  # M-<right>

    zle_highlight=(region:"bg=87,fg=black" special:standout
        suffix:bold isearch:underline)

    set_or_unset_mark () {
      if [ "$CURSOR" -eq "$MARK" ]; then
        REGION_ACTIVE=$((1 - REGION_ACTIVE))
      else
        zle set-mark-command
      fi
    }
    unset_mark () {
      REGION_ACTIVE=0
    }
    unset_mark_or_break () {
      if [ "$REGION_ACTIVE" -eq 1 ]; then
        REGION_ACTIVE=0
      else
        zle send-break
      fi
    }
    copy_region () {
      zle copy-region-as-kill
      REGION_ACTIVE=0
    }
    zle -N set_or_unset_mark
    zle -N unset_mark
    zle -N unset_mark_or_break
    zle -N copy_region

    bindkey ^@ set_or_unset_mark
    bindkey ^G unset_mark
    bindkey '\ew' copy_region
    bindkey '\eW' copy_region

    autoload -U select-word-style
    select-word-style Bash
    ;;
esac
