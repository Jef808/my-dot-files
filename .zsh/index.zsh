#source $HOME/.zsh/named-directories.zsh
#source $HOME/.zsh/functions/get_command_description.zsh
#source $HOME/.zsh/functions/cmd_description.zsh
#source $HOME/.zsh/zsh-better-npm-completion/zsh-better-npm-completion.plugin.zsh
#source $HOME/.zsh/zsh-nvm/zsh-nvm.plugin.zsh

fpath+=$zsh_dir/function

source $zsh_dir/zsh-autosuggestions.zsh
source $zsh_dir/zsh-histdb/sqlite-history.zsh

source $zsh_dir/fastapi-completion.zsh

source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh
