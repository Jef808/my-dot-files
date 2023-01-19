export NO_AT_BRIDGE=1

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.config/rofi/scripts:$PATH"

######################################################
# XDG User directories
######################################################
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

######################################################
# Emacs general settings
######################################################
export EMACSDIR="$HOME/.config/emacs"
export DOOMDIR="$HOME/.config/doom"
export PATH="$EMACSDIR/bin:$PATH"

######################################################
# Conda setup
######################################################
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/jfa/mambaforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/jfa/mambaforge/etc/profile.d/conda.sh" ]; then
        . "/home/jfa/mambaforge/etc/profile.d/conda.sh"
    else
        export PATH="/home/jfa/mambaforge/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "/home/jfa/mambaforge/etc/profile.d/mamba.sh" ]; then
    . "/home/jfa/mambaforge/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<

######################################################
# This is to make sure that the gpg-agent will
# always communicate using the correct TTY
######################################################
unset SSH_AGENT_PID
 if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
   export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
 fi
 export GPG_TTY=`tty`
 gpg-connect-agent updatestartuptty /bye >/dev/null

######################################################
# GPG
######################################################
# GPG_TTY=$(tty)
# export GPG_TTY

tmpfile_last_pacman=$(mktemp)

