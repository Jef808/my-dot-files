# profile startup time
zmodload zsh/zprof

######################################################
# Local variables
######################################################
zsh_dir=$HOME/.zsh

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
alias pacsyu='sudo pacman -Syu'
alias pacqdt='pacman -Qdt'
alias pstree='pstree -hgT --color=age'
alias dmenu='rofi -dmenu'
alias emacd='emacsclient --create-frame --display=:0.0'

######################################################
# Suffix aliases
######################################################
alias -s el=emacd
alias -s html=chromium
alias -s {pdf,djvu}=emacd
alias -s {h,hpp,cpp}=emacd
alias -s md=emacd

######################################################
# journalctl
######################################################
# Wrap long lines instead of truncating
export SYSTEMD_LESS=FRXMK journalctl

######################################################
# Fish-like auto-suggestion
######################################################
source $zsh_dir/zsh-autosuggestions.zsh

######################################################
# Zsh functions
######################################################
fpath=($zsh_dir/Completion/ $fpath)

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
autoload -Uz add-zsh-hook
# bindkey '^r' _histdb-isearch

######################################################
# fzf configuration
######################################################
source $zsh_dir/fzf_completion.zsh
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
# Syntax highlighting in the prompt
#######################################################
# source '/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh'

########################################################
# Emacs vterm integration
#######################################################
[[ "$INSIDE_EMACS" = 'vterm' ]] && source '~/.zsh/vterm.zsh'

########################################################
# Conda
#######################################################
. $zsh_dir/condarc.zsh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/home/jfa/mambaforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/home/jfa/mambaforge/etc/profile.d/conda.sh" ]; then
#         . "/home/jfa/mambaforge/etc/profile.d/conda.sh"
#         print(f"Loading profile.d/conda.sh")
#     else
#         export PATH="/home/jfa/mambaforge/bin:$PATH"
#         "./$PATH/"
#     fi
# fi
# unset __conda_setup

# if [ -f "/home/jfa/mambaforge/etc/profile.d/mamba.sh" ]; then
#     . "/home/jfa/mambaforge/etc/profile.d/mamba.sh"
# fi

# When profiling startup time
zprof
