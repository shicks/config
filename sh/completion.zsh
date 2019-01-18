zstyle :compinstall filename $HOME/.zshrc
autoload -Uz compinit
compinit

zstyle ':completion:*' completer _complete _ignored _files
