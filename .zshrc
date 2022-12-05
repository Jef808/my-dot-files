export PATH="$HOME/.local/bin:$PATH:$HOME/.emacs.d/bin"
export PATH="$HOME/.config/rofi/scripts:$PATH"
export MATLAB_INTEL_OVERRIDE='yes'

#export NVM_DIR="$HOME/.nvm"

######################################################
# General aliases
######################################################
# This allows us to prepend commands with arbitrary option
# strings with `sudo'
alias sudo='sudo '
alias sysu='systemctl --user'
alias pacsyu='sudo pacman -Syu'
alias pacqdt='pacman -Qdt'
alias -g ...='../..'
alias cdprojects='cd ~/projects'
alias cdlocal='cd .local'
alias pstree='pstree -hgT --color=age'
alias pass='rofi-pass'
alias dmenu='rofi -dmenu'
alias clion='~/.local/clion-2022.1.3/bin/clion.sh'
alias idea='~/.local/idea-IU-221.5921.22/bin/idea.sh'
alias emacd='emacsclient --create-frame --no-wait'
alias pass='rofi-pass'

######################################################
# Suffix aliases
######################################################
alias -s el=nano
alias -s html=qutebrowser
alias -s {pdf,djvu}=emacs
alias -s {h,hpp,cpp}=nano
alias -s md=emacs

######################################################
# XDG User directories
######################################################
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state

######################################################
# Git clone shorthands
######################################################
function cloneaur() {
  git clone https://aur.archlinux.org/$1.git
}
function clone() {
  git clone https://github.com/$1/$2.git
}

######################################################
# Fish-like auto-suggestion
######################################################
source ~/.zsh/zsh-autosuggestions.zsh

######################################################
# Completion config
######################################################
zstyle ':completion:*' file-sort modification


######################################################
# Xorg config
######################################################
alias sx='sx sh ~/.config/sx/sxrc'
export XORG_CONFIG_PATH='~/.config/xorg/xorg.conf'


######################################################
# Zsh functions
######################################################
fpath=("$HOME/.zsh/functions/site-functions" $fpath)

######################################################
# Completion widget
######################################################
autoload -U compinit && compinit

######################################################
# Managing dotfiles
######################################################
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias configsecure='/usr/bin/git --git-dir=$HOME/.cfg-secure/ --work-tree=$HOME'

######################################################
# This is to make sure that the gpg-agent will
# always communicate using the correct TTY
######################################################
# unset SSH_AGENT_PID
# if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
#   export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
# fi
# export GPG_TTY=`tty`
# gpg-connect-agent updatestartuptty /bye >/dev/null


######################################################
# Read user config when executing sudo nano
######################################################
# Because of the above, sudo nano <file> will then execute
# nano with the local config file
alias nano='nano --rcfile ~/.nanorc'


######################################################
# Ideas from the grml's /etc/zshrc
######################################################
## (I've been meaning to set this up for so long!!)
## stop at '/'s when doing 'forward/backward word'
WORDCHARS='${WORDCHARS:s@/@}'
## warning if file exists ('cat /dev/null > ~/.zshrc')
setopt NO_clobber
## don't warn me about bg processes when exiting
setopt nocheckjobs


######################################################
# Histdb configuration
######################################################
#source "$HOME/.oh-my-zsh/custom/plugins/zsh-histdb/sqlite-history.zsh"
#autoload -Uz add-zsh-hook
#bindkey '^r' _histdb-isearch


######################################################
# fzf config
######################################################
source ~/.zsh/fzf_completion.zsh
source ~/.zsh/fzf_key-bindings.zsh
alias fzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"

######################################################
# bat config
######################################################
# Use 'help cp' or 'help git branch' to get coloured help output
alias bathelp='bat --plain --language=help'
help() {
  "$@" --help 2>$1 | bathelp
}

######################################################
# Clipboard
######################################################
export CM_LAUNCHER="fzf"

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
# GPG
######################################################
GPG_TTY=$(tty)
export GPG_TTY
