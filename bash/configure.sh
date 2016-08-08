# make sure bash can find our config in XDG_CONFIG_HOME
BASHINIT='source ${XDG_CONFIG_HOME:-$HOME/.config}/bash/bashrc'
if ! grep -q "$BASHINIT" /etc/bashrc; then
    if ! echo $BASHINIT | sudo tee -a /etc/bashrc; then
        # fallback: ~/.bashrc
        ! [[ -f ~/.bashrc ]] && \
            echo $BASHINIT >> ~/.bashrc
    fi
fi
