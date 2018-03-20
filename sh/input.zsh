# Input configuration

# TODO(sdh): subword-forward and subword-backward???

# Note: previously we bound a bunch of different sequences for C-left and
# C-right, to handle various combinations of urxvt and tmux.  I think I
# finally have them straightened out and consistent across all terminals,
# but the old versions can still be found in 10-misc.sh in older branches.

bindkey '\e[1;5C' forward-word  # C-<right>
bindkey '\e[1;3C' forward-word  # M-<right>
bindkey '\e\e[C' forward-word  # ESC-<right>
bindkey '\e[1;5D' backward-word  # C-<left>
bindkey '\e[1;3D' backward-word  # M-<left>
bindkey '\e\e[D' backward-word  # ESC-<left>

# Unbind ^[[33~ since we use it as a prefix shift.
function sdh::input::noop { :; }
zle -N sdh::input::noop
bindkey '\e[36~'  sdh::input::noop # Custom prefix for C-S (36) and super (37)
bindkey '\e[37~'  sdh::input::noop #  -> ignore unless a superstring is bound.
bindkey '\e[5~'   sdh::input::noop # Page up and Page down
bindkey '\e[6~'   sdh::input::noop
bindkey '\e[6;2~' sdh::input::noop # S-<pgdn>

bindkey '\e[1;5A' sdh::input::noop # C-<up>, C-<down>, M-<up>, M-<down>
bindkey '\e[1;3A' sdh::input::noop
bindkey '\e[1;5B' sdh::input::noop
bindkey '\e[1;3B' sdh::input::noop
bindkey '\e[1;2B' sdh::input::noop # S-<down>

zle_highlight=(region:"bg=87,fg=black" special:standout
               suffix:bold isearch:underline)

function sdh::input::set_or_unset_mark {
  if [ "$CURSOR" -eq "$MARK" ]; then
    REGION_ACTIVE=$((1 - REGION_ACTIVE))
  else
    zle set-mark-command
  fi
}
zle -N sdh::input::set_or_unset_mark

function sdh::input::unset_mark {
  REGION_ACTIVE=0
}
zle -N sdh::input::unset_mark

function sdh::input::unset_mark_or_break {
  if [ "$REGION_ACTIVE" -eq 1 ]; then
    REGION_ACTIVE=0
  else
    zle send-break
  fi
}
zle -N sdh::input::unset_mark_or_break

function sdh::input::copy_region {
  zle copy-region-as-kill
  REGION_ACTIVE=0
}
zle -N sdh::input::copy_region

bindkey ^@ sdh::input::set_or_unset_mark
bindkey ^G sdh::input::unset_mark
bindkey '\ew' sdh::input::copy_region
bindkey '\eW' sdh::input::copy_region
bindkey '^W' kill_region

# Make word operations work like bash, but with subword mode
autoload -U select-word-style
select-word-style Bash

# This is a slight change from /usr/share/zsh/functions/Zle,
# taken from http://stackoverflow.com/questions/10847255
# It causes foward-word to behave more like emacs, going to
# the end of the current word, rather than the beginning of
# the next.
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
