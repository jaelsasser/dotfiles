# fire up a server if one doesn't exist
tmux start-server

# Create a session if no session has been defined in tmux.conf.
tmux_session="persist"
if ! tmux has-session -t "$tmux_session" 2> /dev/null; then
    # create our persistent session
    tmux \
        new-session -d -s "$tmux_session" \; \
        set-option -t "$tmux_session" destroy-unattached off &> /dev/null
    # attach to the first window in the persistent session
    tmux \
        new-session -t "$tmux_session" \; \
        set-option destroy-unattached on &> /dev/null
else 
    # open a new window when attaching to the pre-existing session
    tmux \
        new-session -t "$tmux_session" \; \
        set-option destroy-unattached on &> /dev/null \; \
        new-window
fi
