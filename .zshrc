export PATH="$HOME/.local/bin:$PATH:$HOME/.emacs.d/bin"

[[ "$INSIDE_EMACS" = 'vterm' ]] && . ~/.zsh/vterm.zsh

######################################################
# General aliases
######################################################
# This allows us to prepend commands with arbitrary option
# strings with `sudo'
alias sudo='sudo '
alias sysu='systemctl --user'
alias pacsyu='sudo pacman -Syu'
alias pacqdt='pacman -Qdt'
alias cdproj='cd ~/projects'
alias cdloc='cd .local'
alias pstree='pstree -hgT --color=age'
alias dmenu='rofi -dmenu'

alias clion='~/.local/clion-2022.1.3/bin/clion.sh'
alias idea='~/.local/idea-IU-221.5921.22/bin/idea.sh'

#alias emacs='emacsclient --reuse-frame --no-wait'
alias emacd='emacsclient --no-wait .'

######################################################
# Suffix aliases
######################################################
alias -s el=emacd
alias -s html=chromium
alias -s {pdf,djvu}=emacd
alias -s {h,hpp,cpp}=emacd
alias -s md=emacd

######################################################
# XDG User directories
######################################################
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state


######################################################
# journalctl
######################################################
# Wrap long lines instead of truncating
export SYSTEMD_LESS=FRXMK journalctl

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
export XORG_CONFIG_PATH='~/.config/xorg/xorg.conf'

######################################################
# Zsh functions
######################################################
fpath=("$HOME/.zsh/Completion/" $fpath)

######################################################
# Managing dotfiles
######################################################
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias configsecure='/usr/bin/git --git-dir=$HOME/.cfg-secure/ --work-tree=$HOME'

######################################################
# Gitignore.io api
######################################################
function gi() { curl -sLw n https://www.toptal.com/developers/gitignore/api/$@ ;}

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
export WORDCHARS=${WORDCHARS/\/}
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
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_DEFAULT_OPTS="--height=40% --preview='bat {}' --preview-window=right:60%:wrap"
# alias fzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"


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
        print(f"Loading profile.d/conda.sh")
    else
        export PATH="/home/jfa/mambaforge/bin:$PATH"
        "./$PATH/"
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
#GPG_TTY=$(tty)
#export GPG_TTY

######################################################
# NVM
######################################################
export NVM_DIR="$HOME/.config/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

######################################################
# Direnv support
######################################################
eval "$(direnv hook zsh)"

######################################################
# Virtualenv/Conda prompt extensions
######################################################
# Conda support
#setopt PROMPT_SUBST

function get_conda_env() {
  echo $CONDA_DEFAULT_ENV
}

function show_conda_env () {
    REPLY=""
    if [ "$CONDA_DEFAULT_ENV" != "base" ]; then
        REPLY="${CONDA_PROMPT_MODIFIER}"
    fi
}
grml_theme_add_token conda-env -f show_conda_env
zstyle ':prompt:grml:left:setup' items rc conda-env virtual-env change-root user at host path vcs percent
