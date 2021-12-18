export PATH="$PATH:$HOME/.emacs.d/bin"
export PATH="$HOME/.local/bin:$PATH"
export FCEDIT=nano

######################################################
# Zsh functions
######################################################
fpath=("$HOME/.zfunctions" $fpath)

######################################################
# Run mozilla developper edition as firefox
######################################################
alias firefox=firefox-developper-edition

######################################################
# Managing dotfiles
######################################################
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias config-secure='/usr/bin/git --git-dir=$HOME/.cfg-secure --work-tree=$HOME'

######################################################
# Pacman configuration
######################################################

######################################################
# Systemctl alias
######################################################
alias sysu='systemctl --user'

######################################################
# This is to make sure that the gpg-agent will
# always communicate using the correct TTY
######################################################
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpg-connect-agent updatestartuptty /bye >/dev/null

######################################################
# Read user config when executing sudo nano
######################################################
alias sudo='sudo '
alias nano='nano --rcfile ~/.nanorc'

######################################################
# Ideas from the grml's /etc/zshrc
######################################################
## (I've been meaning to set this up for so long!!)
## stop at '/'s when doing 'forward/backward word'
WORDCHARS='${WORDCHARS:s@/@}'
## try to avoid the 'zsh: no matches found...'
setopt nonomatch
## warning if file exists ('cat /dev/null > ~/.zshrc')
setopt NO_clobber
## don't warn me about bg processes when exiting
setopt nocheckjobs
## changed completer settings
#zstyle ':completion:*' completer _complete _correct _approximate
#zstyle ':completion:*' expand prefix suffix
## another different completer setting: expand shell aliases
#zstyle ':completion:*' completer _complete _expand_alias _approximate

######################################################
# Histdb configuration
######################################################
source "$HOME/.oh-my-zsh/custom/plugins/zsh-histdb/sqlite-history.zsh"
autoload -Uz add-zsh-hook
#source "$HOME/.oh-my-zsh/custom/plugins/zsh-histdb/histdb-interactive.zsh"
#bindkey '^r' _histdb-isearch

######################################################
# fzf config
######################################################
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh
export LD_RUN_PATH=/usr/local/lib
export LD_LIBRARY_PATH=/usr/local/lib
alias fzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"
