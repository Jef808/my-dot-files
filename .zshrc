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
alias emacd='emacsclient --create-frame --display=:0.0'
alias google='google-chrome-stable --remote-debugging-port=9222'

######################################################
# Alacritty color themes
######################################################
export ALACRITTY_COLOR_SCHEME_LIGHT='solarized_light.yaml'
export ALACRITTY_COLOR_SCHEME_DARK='one_dark.yaml'

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
# Zsh completions
######################################################

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
#source $zsh_dir/zsh-histdb/sqlite-history.zsh
#autoload -Uz add-zsh-hook
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

########################################################
# Tab completion for the dotnet CLI
#######################################################
# TODO Only do this in dotnet directories using direnv
#source ~/.zsh/dotnetrc.zsh

########################################################
# Emacs vterm integration
#######################################################
[[ "$INSIDE_EMACS" = 'vterm' ]] && source '~/.zsh/vterm.zsh'

########################################################
# Conda
#######################################################
source $zsh_dir/condarc.zsh

#
# When profiling startup time
#zprof
#
source /usr/share/nvm/init-nvm.sh
export NODE_VERSIONS=$HOME/.config/nvm/versions/node
