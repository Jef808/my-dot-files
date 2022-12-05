#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias rnano='sudo nano'

export CDPATH='.:~/'

PS1='[\u \W]\$ '

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/jfa/mambaforge/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
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
# <<< conda initialize <<<

