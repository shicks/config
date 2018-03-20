_last_start_seconds=$SECONDS
local minimum
local format
#zstyle -s ':prezto:module:timing:start' minimum 'minimum'
#zstyle -s ':prezto:module:timing:start' format 'format'
if [ $((SECONDS - ${_last_end_seconds:-0})) -gt ${minimum:-300} ]; then
  print -P "${format:->>> Execution began at %D{%a, %b %d, %H:%M:%S}}"
fi

if [ -n "$TMUX" ]; then
  local monitor
  #zstyle -s ':prezto:module:timing:silence' monitor 'monitor'
  tmux set-window-option -t "$TMUX_PANE" \
       monitor-silence {$monitor:-60} &> /dev/null
fi
