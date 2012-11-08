if ! $(type google_prompt_command 2> /dev/null); then
  function google_prompt_command { :; }
fi

PROMPT_COMMAND='LASTSTATUS=$?; shift_in; set_prompt; savelasthistory; google_prompt_command; drawlinebreak'

if ! $(type google_cd_hook 2> /dev/null); then
  function google_cd_hook { :; }
fi

## NOTE: order matters here (for unsymlink?)
# CD_HOOK='save_dir; unsymlink; google_cd_hook'
CD_HOOK='save_dir; google_cd_hook'

#eval $CD_HOOK

# no more hgit...
#alias hgit='git home'
#complete -o bashdefault -o default -o nospace -F _git hgit 2>/dev/null \
#        || complete -o default -o nospace -F _git hgit
