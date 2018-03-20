function add_fpath {
  local path abs rel func
  while [ $# -gt 0 ]; do
    fpath=("$1" "${fpath[@]}")
    for abs in $1/*; do
      rel=${abs##*/}
      # Skip zsh-only and emacs autosave
      case "$rel" in
        (*.zsh|*~|.*|\#*) continue ;;
        (*)
          local func=${rel%.bash}
          eval "function $func { autoload_function \"$func\" \"$abs\" \"\$@\"; }"
          ;;
      esac
    done
    shift
  done
}

function autoload_function {
  local func=$1
  local path=$2
  shift 2
  local content=$(cat $path)
  eval "function $func { :
    $content
  }"
  $func "$@"
}

add_fpath ~/.sh.d/functions
