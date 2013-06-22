## Various exports

### TODO - emacsclient?  switch on TERM?
case "$(hostname -s)" in
  baley) export EDITOR=quickemacs ;;
  *) export EDITOR='emacs -q' ;;
esac

export PATH=$HOME/local/bin:$PATH

# Use our custom lesspipe, with syntax highlighting
# See http://linux-tips.org/article/78/syntax-highlighting-in-less
export LESSPIPE="$HOME/local/bin/lesspipe %s"
export LESSOPEN="|$HOME/local/bin/lesspipe %s"
export LESS='-R -F -X'  # allow ANSI colors, exit if <1 page, disable ti/te

