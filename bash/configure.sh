ENVFILE=${HOME}/.bashrc
for FILE in /etc/bashrc /etc/bash.bashrc; do
    if [[ -f "$FILE" ]]; then
        ENVFILE=$FILE
        break
    fi
done

# make sure bash can find our config in XDG_CONFIG_HOME
BASHINIT='source ${XDG_CONFIG_HOME:-$HOME/.config}/bash/bashrc'
if ! grep -q "$BASHINIT" ${ENVFILE}; then
    if ! echo $BASHINIT | sudo tee -a ${ENVFILE}; then
        echo "Failed to set custom bashrc location"
    fi
fi
