if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Start ssh-agent
set -q SSH_AGENT_PID; or begin
    # Iniciar el ssh-agent
    eval (ssh-agent -c) > /dev/null 2>&1
end

# add all ssh key ~/.ssh/
for key in (ls ~/.ssh/id_* 2>/dev/null)
    ssh-add $key > /dev/null 2>&1
end

set -U fish_user_paths ~/.volta/bin $fish_user_paths
set -g fish_greeting ""

alias ls 'lsd'
alias cat 'bat'
alias doom '~/.config/emacs/bin/doom'
