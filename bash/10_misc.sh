
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
    bind -r '\e[36~'
    bind -r '\e[37~'
    ;;

  (zsh)
    # NOTE: for some reason, zsh on quantal (5.0.0, rather than 4.2) is sending
    # smkx (set cursor mode: ESC [ ? 1 h ESC =) right before accepting user input,
    # and urxvt seems to be interpreting this as a reason to swap left/right and
    # ctrl-left/right.  The following line is from Etc/FAQ in zsh's sources, and
    # basically tells zsh to leave us in Application mode all the time, rather
    # than switching to keypad mode.  I don't fully understand why this is needed
    # or what it's all about, and it generally seems wrong.
    function zle-line-init() { echoti rmkx; }
    zle -N zle-line-init

    # Configuration copied from zsh-newuser-install

    HISTFILE=~/.histfile
    HISTSIZE=1000
    SAVEHIST=1000
    setopt SH_WORD_SPLIT
    setopt no_nomatch
    setopt interactivecomments
    setopt autopushd
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

    # Unbind ^[[33~ since we use it as a prefix shift.
    function noop { :; }
    zle -N noop
    bindkey '\e[36~' noop # Custom prefix for C-S and super
    bindkey '\e[37~' noop
    bindkey '\e[5~' noop # Page up and Page down
    bindkey '\e[6~' noop

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

    # Enable automatic timing
    function start_timer {
      LAST_START_SECONDS=$SECONDS
    }
    function end_timer {
      if [ -n "$LAST_START_SECONDS" ]; then
        local diff=$((SECONDS - LAST_START_SECONDS))
        if [ $diff -gt ${REPORTTIME:-5} ]; then
          local hours=$((diff / 3600))
          local mins=$((diff / 60 % 60))
          local secs=$((diff % 60))
          hours=${hours#0}
          mins=${mins#0}
          time="${hours:+${hours}h}${mins:+${mins}m}${secs}s"
          echo "$time wallclock"
          case $TERM in # only send BEL if we're in tmux
            (tmux*) echo -e '\007' ;;
          esac
        fi
        LAST_START_SECONDS=
      fi
    }

    # TODO(sdh): consider putting all these functions into a
    # directory on $fpath and version-controlling that dir?
    # (We could potentially emulate $fpath in bash as well by
    # calling eval with a function wrapper?)

    # Make word operations work like bash, but with subword mode
    autoload -U select-word-style
    select-word-style Bash
    # This is a slight change from /usr/share/zsh/functions/Zle,
    # taken from http://stackoverflow.com/questions/10847255
    function forward-word-match {
      emulate -L zsh
      setopt extendedglob
      autoload -Uz match-words-by-style
      local curcontext=":zle:$WIDGET" word
      local -a matched_words
      integer count=${NUMERIC:-1}
      if (( count < 0 )); then
        (( NUMERIC = -count ))
        zle ${WIDGET/forward/backward}
        return
      fi
      while (( count-- )); do
        match-words-by-style
        if [[ -n $matched_words[4] ]]; then
          word=$matched_words[4]$matched_words[5]
        else
          word=$matched_words[5]
        fi
        if [[ -n $word ]]; then
          (( CURSOR += ${#word} ))
        else
          return 1
        fi
      done
      return 0
    }


    ################################################################
    # This should go in 50_dirs but it doesn't work...?

    function lsd {
      popd -l | perl -ne '
        my $i = 0;
        my $prev = "";
        while (s/(\S+) ?//) {
          next if $1 eq $prev;
          printf "%3d   %s\n", $i, $1 if $i;
          $i++; $prev = $1;
          last if $i == 30;
        }'
    }

    function cd {
      if [ "$1" == "-l" ]; then
        lsd
        return
      elif [ "${1#-?}" != "$1" ]; then
        dir=${1#-}
        dir="$(lsd | perl -ne '
          while (<>) {
            if (/^\s*'"$dir"'\s+(.*)/) {
              print $1;
              exit;
            }
          }
          print STDERR "No dir found for '"$dir"'.";')"
        echo "cd $dir"
        builtin cd "$(echo $dir | perl -pe 's+~(/|$)+$ENV{'HOME'}$1+g')"
      else
        builtin cd "$@"
      fi
    }

    ;;
esac
