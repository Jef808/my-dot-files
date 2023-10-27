export EDITOR=emacs
export NO_AT_BRIDGE=1

######################################################
# XDG User directories
######################################################
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state

######################################################
# Emacs general settings
######################################################
export EMACSDIR=$HOME/.config/emacs
export DOOMDIR=$HOME/.config/doom

######################################################
# Path
######################################################
export PATH=$HOME/.local/bin:$EMACSDIR/bin:$PATH

######################################################
# This is to make sure that the gpg-agent will
# always communicate using the correct TTY
######################################################
unset SSH_AGENT_PID
 if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
   export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
 fi
export GPG_TTY=`tty`
 #gpg-connect-agent updatestartuptty /bye >/dev/null
 gpgconf --launch gpg-agent
