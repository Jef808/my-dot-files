export EDITOR="emacsclient -c"
export NO_AT_BRIDGE=1

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

######################################################
# Path
######################################################
export PATH=$HOME/.local/bin:$EMACSDIR/bin:$PATH

export SBCL_HOME=/usr/local/lib/sbcl
TCLLIBPATH=$HOME/.local/share/tk-themes
export LLAMA_INDEX_CACHE_DIR=${XDG_DATA_DIR}/llama-index
