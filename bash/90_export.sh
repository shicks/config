## Various exports

### TODO - emacsclient?  switch on TERM?
case "$(hostname -s)" in
  dors) export EDITOR=quickemacs ;;
  *) export EDITOR='emacs -q' ;;
esac

export PATH=$HOME/local/bin:$PATH
export PERL5LIB=$HOME/local/lib/perl:$PERL5LIB
PERL5LIB=${PERL5LIB%:} # strip trailing colon if there is one

if [ -d $HOME/local/opt/haskell-platform/bin ]; then
  PATH=$HOME/local/opt/haskell-platform/bin:$PATH
fi

# Use our custom lesspipe, with syntax highlighting
# See http://linux-tips.org/article/78/syntax-highlighting-in-less
export LESSPIPE="$(which lesspipe) %s"
export LESSOPEN="|$LESSPIPE"
export LESS='-R -F -X'  # allow ANSI colors, exit if <1 page, disable ti/te

export GOROOT=$HOME/local/opt/go
