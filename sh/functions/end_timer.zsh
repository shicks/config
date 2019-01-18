local minimum format
#zstyle -s ':prezto:module:timing:wallclock' minimum 'minimum'
#zstyle -s ':prezto:module:timing:wallclock' format 'format'

_last_end_seconds=$SECONDS
if [ -n "$_last_start_seconds" ]; then
  local diff=$((SECONDS - $_last_start_seconds))
  if [ $diff -gt ${minimum:-5} ]; then
    local days=$((diff / 86400))
    local hours=$((diff / 3600 % 24))
    local mins=$((diff / 60 % 60))
    local secs=$((diff % 60))
    days=${days#0}
    hours=${hours#0}
    mins=${mins#0}
    time="${days:+${days}d}${hours:+${hours}h}${mins:+${mins}m}${secs}s"
    zformat -f formatted "${format:->>> %(d..%dd)%(h..%hh)%(m..%mm)%(s..%ss) wallclock}" \
        "s:$secs" "m:$mins" "h:$hours" "d:$days"
    echo -n "$formatted"
    # TODO(sdh): consider coloring as www.paradox.io/posts/9-my-new-zsh-prompt
    #if zstyle -t ':prezto:module:timing:tmux' bell; then
    case "$TERM" in # only send BEL if we're in tmux
      (tmux*) echo -e '\007' ;;
    esac
    #fi
  fi
  _last_start_seconds=
fi

if [ -n "$TMUX" ]; then
  tmux set-window-option -t "$TMUX_PANE" monitor-silence 0 &> /dev/null
fi
