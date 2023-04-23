#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias rnano='sudo nano'

export CDPATH='.:~/'

PS1='[\u \W]\$ '

