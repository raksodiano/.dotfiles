if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Start ssh-agent
set -q SSH_AGENT_PID; or begin
    # Iniciar el ssh-agent
    eval (ssh-agent -c)
end

# add all ssh key ~/.ssh/
for key in (ls ~/.ssh/id_* 2>/dev/null)
    ssh-add $key
end

alias ls 'lsd'
alias cat 'bat'
