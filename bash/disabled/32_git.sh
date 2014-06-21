# Outputs the repo name (i.e. basename of the root dir)
# If /src/ is in the path, the repo name is whatever
# comes after /src/.
function git_repo_name {
  local dir=$1
  if [ "${dir#*/src/}" = "$dir" ]; then
    echo $(basename "$dir")
  else
    echo ${dir#*/src/}
  fi
}

function update_git_branch {
  local GIT=$(git rev-parse --git-dir 2>/dev/null)
  if [ -z "$GIT" ]; then
    GIT_BRANCH=""
    GITROOT=""
    return
  fi
  GIT=$(realpath "$GIT")
  GITROOT=$(dirname "$GIT")
  local HEAD="$(cat "$GIT/HEAD" 2> /dev/null)"
  if [[ "$HEAD" =~ ^ref: ]]; then
    GIT_BRANCH="${HEAD##*/}"
    if [ -e "$GIT/refs/heads/$GIT_BRANCH" ]; then
      local HASH="$(cat "$GIT/refs/heads/$GIT_BRANCH")"
    else
      local HASH="$(git rev-parse HEAD 2>/dev/null)"
    fi
    GIT_BRANCH="${GIT_BRANCH}($(substr "$HASH" 0 6))"
    GIT_BRANCH="$(git_repo_name "$GITROOT"):$GIT_BRANCH"
    
  elif [ -z "$HEAD" ]; then
    GIT_BRANCH=""
  else
    GIT_BRANCH="${HEAD:0:6}"
    GIT_BRANCH="$(git_repo_name "$GITROOT"):$GIT_BRANCH"
  fi
}

# function update_branch_after_checkout {
#   if history 1 | sed 's/ *[0-9][0-9]*  *//' |
#       egrep '^git (co|checkout)' &> /dev/null; then
#     update_git_branch
#   fi
# }