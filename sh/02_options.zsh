HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt HIST_IGNORE_DUPS
setopt SH_WORD_SPLIT
setopt AUTO_PUSHD             # Automatically push dirs onto stack
setopt INTERACTIVE_COMMENTS   # Comments are allowed interactively
setopt NO_BANG_HIST           # Don't treat '!' special
setopt NO_NOMATCH             # Don't cause errors when globbing fails
setopt PUSHD_MINUS            # Invert the sign of dir stack

unsetopt BRACE_CCL            # Don't expand a{bc}d to (abd acd)
unsetopt EXTENDED_GLOB        # Don't interpret commit^ as a glob

bindkey -e                    # Set to emacs mode
