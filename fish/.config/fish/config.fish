if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Start ssh-agent
if not set -q SSH_AGENT_PID
    eval (ssh-agent -c)
end

alias ls 'lsd'
alias cat 'bat'
