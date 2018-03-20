function add_fpath {
  local path
  while [ $# -gt 0 ]; do
    fpath=("$1" "${fpath[@]}")
    local file
    for file in $1/*; do
      file=${file##*/}
      # Skip bash-only and emacs autosave
      case "$file" in
        (*.bash|*~|.*|\#*) continue ;;
        (*.zsh)
          # Check if it's zsh-only and make a wrapper
          local func=${file%.zsh}
          eval "function $func { autoload -Uz $func.zsh; $func.zsh \"$@\" }"
          ;;
        (*)
          autoload -Uz "$file"
          ;;
      esac
    done
    shift
  done
}

add_fpath ~/.sh.d/functions
