export PATH="$PATH:$HOME/.emacs.d/bin"
export PATH="$HOME/.local/bin:$PATH"

setopt incappendhistorytime
export FCEDIT=nano
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
#alias fzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"
######################################################
# Run mozilla developper edition as firefox
######################################################
alias firefox=firefox-developper-edition

######################################################
# Managing dotfiles
######################################################
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

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
gpg-connect-agent updatestartuptty /bye >/dev/null
