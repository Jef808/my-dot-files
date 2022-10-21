cat ~/.zshrc.local | awk '/alias/ print{ $1, $2 } >
.local/tmp/zsh-aliases
cat ~/.bashrc | awk '/alias/ print{ $1, $2 }>
.local/tmp/bash-aliases
