if [ -d "$HOME/.bun" ]; then
  # bun completions
  [ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

  # bun
  export BUN_INSTALL="$HOME/.bun"
  export PATH="$BUN_INSTALL/bin:$PATH"
fi
