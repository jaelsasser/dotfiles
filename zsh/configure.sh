#!/usr/bin/env sh

if [ -f /etc/zsh/zshenv ]; then
    readonly zshenv_file="/etc/zsh/zshenv"
else
    readonly zshenv_file="/etc/zshenv"
fi

# make sure zsh looks for .zshrc in the right spot on launch
snippet='test -d "${XDG_CONFIG_BASE:-$HOME/.config/}zsh" && export ZDOTDIR=${XDG_CONFIG_BASE:-$HOME/.config/}zsh'
if ! grep -q "$snippet" "$zshenv_file" >/dev/null 2>&1; then
    if ! echo "$snippet" | sudo tee -a "$zshenv_file"; then
        printf 'Failed to set ZDOTDIR vis %s, symlinking into $HOME\n' "$zshenv_file" >&2
        ln -sf "$(cwd)/zsh/.zshenv" "$HOME/.zshenv"
    fi
fi

ZPLUG_HOME=${XDG_DATA_BASE:-"$HOME/.local/share"}/zsh/zplug
if [ ! -d $ZPLUG_HOME ]; then
    git clone https://github.com/zplug/zplug --depth 1 "$ZPLUG_HOME"
fi
