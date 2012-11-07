PROMPT_COMMAND='LASTSTATUS=$?; shift_in; set_prompt; $GOOGLE_PROMPT_COMMAND; drawlinebreak'

## NOTE: order matters here (for unsymlink?)
#CD_HOOK="save_dir; unsymlink; abbrev_dir; google3_shortcuts"

CD_HOOK='save_dir; $GOOGLE_CD_HOOK'

#eval $CD_HOOK

# no more hgit...
#alias hgit='git home'
#complete -o bashdefault -o default -o nospace -F _git hgit 2>/dev/null \
#        || complete -o default -o nospace -F _git hgit
