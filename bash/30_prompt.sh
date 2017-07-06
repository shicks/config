
mkdir -p $HOME/.sh

function ps1_dollar {
  case "$UID" in (0) echo '#' ;; (*) echo '$' ;; esac
}

if touch -d "now + 5 minutes" $HOME/.sh/$$.ps1time &> /dev/null; then
  function ps1_updatetime {
    touch -d "now + 5 minutes" $HOME/.sh/$$.ps1time
  }
else
  function ps1_updatetime {
    touch -am $HOME/.sh/$$.ps1time
    touch -A0500 $HOME/.sh/$$.ps1time
  }
fi

function ps1_time {
  ps1_updatetime
  local timecolor
  case "$(hostname -s)" in
    (dors|daneel) timecolor=cyan ;;
    (sdh-glaptop) timecolor=magenta ;;
    (*) timecolor=red ;;
  esac
  echo " $(color -p $timecolor)[$(date +"%a %H:%M")]$(color -p off)"
}

function show_exec_time {
  touch $HOME/.sh/now
  if [ $HOME/.sh/now -nt $HOME/.sh/$$.ps1time ]; then
    echo "Execution began at $(date +"%a %H:%M")"
  fi
}

function unset_time {
  function ps1_time { :; }
}

function ps1_debian {
  echo "${debian_chroot:+($debian_chroot)}"
}

# does a variety of abbreviations to the directory name
function ps1_dir {
  echo "${PWD_ABBREV:-${PWD/#$HOME/~}}"
}

function ps1_host {
  local hostcolor
  case "$(hostname -s)" in
    (sdh-glaptop) hostcolor=$(color -p magenta) ;;
    (*) hostcolor=$(color -p green) ;;
  esac
  if [ -z "$VANITY_HOSTNAME" ]; then
    VANITY_HOSTNAME=$([ -f /etc/vanity-hostname ] &&
        sed s/\\..*// /etc/vanity-hostname || hostname -s)
  fi
  echo $hostcolor$(whoami)@$VANITY_HOSTNAME
}

function ps1_status {
  if [ "$LASTSTATUS" -ne 0 ]; then
    echo " $(color -p red):-($LASTSTATUS$(color -p off)"
    # echo -ne '\a' # beep
  fi
}

ensure_function ps1_google
ensure_function update_branch_google

## This is where the prompt actually gets set.
function set_prompt {
  update_git_branch #update_branch_after_checkout
  update_branch_google
  local FRONT
  local DIR=$(ps1_dir)
  ## TODO(sdh): add a prodaccess check - red "LOAS" when missing
  if [ -n "$GIT_BRANCH" ]; then
    FRONT="$(color -p yellow)$GIT_BRANCH$(color -p off)"
    if [ -e "$GITROOT/.git/refs/stash" ]; then # print stash size...
      local size=$(git stash list | wc -l) # "$GITROOT/.git/refs/stash" | cut -d\  -f1
      FRONT="$FRONT $(color -p magenta){$size}$(color -p off)"
    fi
    DIR=$GIT_BRANCH
  else
    FRONT="$(ps1_host)$(color -p off)"
  fi
  PS1="$(ps1_debian)$FRONT$(ps1_google)$(ps1_time)$(ps1_status)"
  PS1="$PS1 $(color -p blue)$(ps1_dir)$(color -p off)$(ps1_dollar) "
  #set_title "$(whoami)@$(ps1_host) $DIR"
  set_title "$DIR"
}
