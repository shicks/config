autoload -Uz add-zsh-hook

# TODO(sdh): consider cleaning these up
#   - define a module as something with optional ::preexec and ::precmd funcs
#   - add functions to install and uninstall modules
#   - consider installing bash-preexec???
#   - would help if we had a structured way to do module/function dirs
#     - privates and namespacing?

add-zsh-hook precmd  save_last_status
add-zsh-hook precmd  shift_in
add-zsh-hook precmd  prompt_repoline
add-zsh-hook precmd  end_timer
add-zsh-hook precmd  set_title_precmd

add-zsh-hook preexec prompt_repoline_preexec
add-zsh-hook preexec set_title_preexec
add-zsh-hook preexec start_timer
