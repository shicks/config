function add_fpath {
  local path
  while [ $# -gt 0 ]; do
    fpath=("$1" "${fpath[@]}")
    local file
    for file in $1/*; do
      file=${file##*/}
      # Skip bash-only and emacs autosave
      if [ -z "${file##*.bash}" -o -z "${file##*~}" ]; then continue; fi
      # Check if it's zsh-only and make a wrapper
      if [ -z "${file##*.zsh}" ]; then
        local func=${file%.zsh}
        eval "function $func { autoload -Uz $func.zsh; $func.zsh \"$@\" }"
      else
        autoload -Uz "$file"
      fi
    done
    shift
  done
}

add_fpath ~/.sh.d/functions
