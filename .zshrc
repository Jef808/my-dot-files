export PATH="$PATH:$HOME/.emacs.d/bin"
export PATH="$HOME/.local/bin:$PATH"

######################################################
# History options
######################################################
setopt incappendhistorytime
source "$HOME/.oh-my-zsh/custom/plugins/zsh-histdb/sqlite-history.zsh"
autoload -Uz add-zsh-hook
export FCEDIT=nano

######################################################
# fzf config
######################################################
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh
export LD_RUN_PATH=/usr/local/lib
export LD_LIBRARY_PATH=/usr/local/lib

######################################################
# Run mozilla developper edition as firefox
######################################################
alias firefox=firefox-developper-edition

######################################################
# Managing dotfiles
######################################################
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
