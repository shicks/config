## Various exports

### TODO - emacsclient?  switch on TERM?
export EDITOR=quickemacs

export PATH=$HOME/local/bin:$PATH

# Use our custom lesspipe, with syntax highlighting
# See http://linux-tips.org/article/78/syntax-highlighting-in-less
export LESSPIPE="$HOME/local/bin/lesspipe %s"
export LESSOPEN="|$HOME/local/bin/lesspipe %s"
export LESS=' -R '
