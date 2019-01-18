# Handle C-arrow and M-arrow in both cursor and keypad modes
bind '"\eOD": backward-word'
bind '"\eOC": forward-word'

# Ignore custom terminal prefixes (tmux and emacs interpret them correctly)
bind -r '\e[36~'
bind -r '\e[37~'
