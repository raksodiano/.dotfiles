#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

if [[ $- == *i* ]] && [ -f "$HOME/.brushrc" ]; then
    source "$HOME/.brushrc"
fi
export PATH=~/.npm-global/bin:$PATH
