# If not running interactively, don't do anything
[ -z "$PS1" ] && return

export SHELL=$(which zsh)

# Now run all the startup stuff:
for a in $HOME/.bash.d/??_*.sh; do
  . $a
done

PERL_MB_OPT="--install_base \"/home/sdh/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/sdh/perl5"; export PERL_MM_OPT;
