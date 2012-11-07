## We have a variety of code to override the "cd" command,
## both to add a few arguments (cd -l, cd -#, etc) and to
## run a hook whenever the directory changes.

if [ -z "${DIRS[0]}" ]; then
  DIRS=()
fi

# function unsymlink {
#   local dir="$(pwd -P)"
#   while [ -n "${dir#?}" ]; do
#     if [ -e "$dir/.nosymlink" ]; then
#       command cd -P .
#       break
#     fi
#     dir="$(dirname "$dir")"
#   done
# }

function save_dir {
  if [ "$OLDPWD" != "${DIRS[0]}" ]; then
    DIRS=("$OLDPWD" "${DIRS[@]:0:30}")
  fi
}

function cd_pop {
  local n=0
  if [ $# -gt 0 ]; then
    n="$1"
  fi
  cd "${DIRS[$n]}"
  DIRS=("${DIRS[@]:$((n+1))}")
}

function match_dir {
  local dir=$1
  local oldpwd=$PWD
  local ret=''
  local root pat match
  if [ -d $dir ]; then
    echo $dir
  else
    if [ "${dir#/}" != "$dir" ]; then
      cd /
      ret=/
      dir="${dir#/}"
    fi
    while [ -n "$dir" ]; do
      if [ "${dir#*/}" != "$dir" ]; then
        root=${dir%%/*}
        dir=${dir#*/}
      else
        root=$dir
        dir=''
      fi
      if [ -d "$root" ]; then
        match=$root
      else # still need to work on this - support ..?
        pat="$(echo "$root" | sed 's/./*\0/g')*"
        match=$(echo $pat)
        if [ ! -d "$match" ]; then
          echo "Could not find match for $root in $PWD: got [$match] instead" >& 2
          cd "$oldpwd"
          return 1
        fi
      fi
      ret=$ret/$match
      cd "$match"
    done
    echo ${ret#/}
    cd "$oldpwd"
  fi
}

function cd_up {
  local dir="$PWD"
  local match=\*"$(echo "$1" | perl -pe'split //;$"="*";$_="@_"')" # (for bash)'
  while [ -n "${dir#?}" ]; do
    case "$(basename "$dir")" in
      ($match) echo $dir; fragment="${PWD#$dir/?}"; cd $dir; break
    esac
    dir="$(dirname "$dir")"
  done
}

function cd_interactive {
  local i=0
  local ans
  while [ -n "${DIRS[$i]}" ]; do
    echo -n "$i: ${DIRS[$i]} [Jkyq] "
    read ans
    case "$ans" in
      ([kK]*) : $((i--)) ;;
      ([yY]*) my_cd "${DIRS[$i]}"
              return ;;
      ([qQ]*) return ;;
      ([jJ]*) : $((i++)) ;;
      ([0123456789])
              my_cd "${DIRS[$ans]}"
              return ;;
      ("")    : $((i++)) ;;
      (*)     echo "Bad response: $ans" >&2 ;;
    esac
  done
}

function my_cd {
  local args=()
  while [ $# -gt 0 ]; do
    case "$1" in
      (-*) args=("${args[@]}" "$1") ;;
      (*)  args=("${args[@]}" "$(match_dir "$1")") ;;
    esac
    shift
  done
  command cd "${args[@]}"
  eval $CD_HOOK
}

function cd {
  case "$1" in
    (-i)  cd_interactive
          return ;;
    (-l)  local i=1
          for dir in "${DIRS[@]}"; do
            echo "$i. $dir"
            let i++
          done
          return ;;
    (-?*) if [[ "${1#-}" =~ ^[0-9]+$ ]]; then
            cd_pop $((${1#-}-1))
            return
          fi ;;
    (^*)  cd_up ${1#^}
          return ;;
  esac
  my_cd "$@"
}
function pushd {
  command pushd "$@"
  eval $CD_HOOK
}
function popd {
  command popd "$@"
  eval $CD_HOOK
}
