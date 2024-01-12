# profile startup time
#zmodload zsh/zprof

export PATH=$HOME/.local/bin:$PATH

unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye >/dev/null

######################################################
# Global variables
######################################################
export JAVA_HOME=/usr/lib/jvm/default

######################################################
# Local variables
######################################################
local zsh_dir=$HOME/.zsh

source $zsh_dir/index.zsh

######################################################
# Zsh options
######################################################
source $zsh_dir/options.zsh

######################################################
# General aliases
######################################################
# This allows us to prepend commands with arbitrary option
# strings with `sudo'
alias sudo='sudo '
alias sysu='systemctl --user'
alias pacman='pacman --color=always'
alias pacsyu='sudo pacman -Syu'
alias pacqdt='pacman -Qdt'
alias pstree='pstree -hgT --color=age'
alias dmenu='rofi -dmenu'
alias emacd='emacsclient -c'
alias chrome='systemd-run --user --unit=chrome google-chrome-stable'
alias xclip='kitten clipboard'
alias l='ls -latr'

######################################################
# Suffix aliases
######################################################
alias -s el=emacd
alias -s html=google-chrome-stable
alias -s {pdf,djvu}=emacd
alias -s {h,hpp,cpp}=emacd
alias -s md=emacd

######################################################
# journalctl
######################################################
# Wrap long lines instead of truncating
export SYSTEMD_LESS=FRXMK journalctl

#####################################################
# Fish-like auto-suggestion
######################################################
source $zsh_dir/zsh-autosuggestions.zsh

######################################################
# Managing dotfiles
######################################################
alias config="/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME"
alias configsecure="/usr/bin/git --git-dir=$HOME/.cfg-secure/ --work-tree=$HOME"

######################################################
# Gitignore.io api
######################################################
function gi() { curl -sLw n https://www.toptal.com/developers/gitignore/api/$@ ;}

######################################################
# Read user config when executing sudo nano
######################################################
# Because of the above, sudo nano <file> will then execute
# nano with the local config file
alias nano="nano --rcfile ~/.nanorc"

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
source $zsh_dir/zsh-histdb/sqlite-history.zsh
unsetopt HIST_IGNORE_SPACE
autoload -Uz add-zsh-hook
# bindkey '^r' _histdb-isearch

######################################################
# fzf configuration
######################################################
source $zsh_dir/fzf_key-bindings.zsh
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_DEFAULT_OPTS="--height=40% --preview='bat {}' --preview-window=right:60%:wrap"
# alias fzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"

######################################################
# Clipboard
######################################################
export CM_LAUNCHER="fzf"

#####################################################
# Direnv support
######################################################
eval "$(direnv hook zsh)"

########################################################
# Emacs vterm integration
#######################################################
[[ "$INSIDE_EMACS" = 'vterm' ]] && source '~/.zsh/vterm.zsh'

######################################################
# NVM # See github.com/lukechilds/zsh-nvm
######################################################
export NVM_DIR="${XDG_CONFIG_HOME}/nvm"
export NVM_LAZY_LOAD=true
export NVM_LAZY_LOAD_EXTRA_COMMANDS=('emacs', 'emacsclient')
export NVM_NO_USE=true
source "${zsh_dir}/zsh-nvm/zsh-nvm.plugin.zsh"

########################################################
# Conda
#######################################################
#source "$zsh_dir/condarc.zsh"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/miniconda3/etc/profile.d/conda.sh"
        print(f"Loading profile.d/conda.sh")
    else
        export PATH="/opt/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
#unfunction load_conda
# >>> conda initialize >>>

if [ -f "/opt/miniconda3/etc/profile.d/mamba.sh" ]; then
    . "/opt/miniconda3/etc/profile.d/mamba.sh"
fi
