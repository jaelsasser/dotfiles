#!/bin/bash

# default target
DOTFILES="${HOME}/.dotfiles"

DOTFILES_REMOTE="git@github.com:jaelsasser/dotfiles.git"
DOTFILES_HTTPS="https://github.com/jaelsasser/dotfiles.git"

# determine the operating system
if [[ "$OSTYPE" == darwin* ]]; then
  HAS_OSX=1
  INSTALL='brew install'
else
  HAS_LINUX=1
  INSTALL='sudo apt-get install'
fi

# os x isn't nice enough to ship with a package manager
if [[ -n "${HAS_OSX}" ]] && ! [[ -f /usr/local/bin/brew ]]; then
    /usr/bin/ruby -e \
        "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# setup git
if ! which -s git; then
    eval ${INSTALL} git  
fi

# generate a unique key for this computer
id_git=${HOME}/.ssh/id_git.pub
if ! [[ -f "${id_git}" ]]; then
    ssh-keygen -f ${id_git} -t rsa -N ''
    cat "${id_git}"

    read -p "Add the new key to Githb now: https://github.com/settings/ssh"
fi

if ! [[ -d "${DOTFILES}" ]]; then
    if ! git clone ${DOTFILES_REMOTE} ${DOTFILES}; then 
        echo "Failed to clone the dotfiles repository. Attempting"
        if ! git clone ${DOTFILES_HTTPS} ${DOTFILES}; then
            echo "Yeah, even the https fallback didn't work. SOL, buddy."
        fi
    fi
else
    echo "Dotfiles repository already exists."
fi

