#!/bin/sh
DOTFILES="${HOME}/.dotfiles"
DOTFILES_HTTPS="https://github.com/jaelsasser/dotfiles.git"

if [ "$(id -u)" != "0" ]; then
    ROOT_CMD="sudo"
    SUDOER="test -e /etc/sudoers.d/$USER_sudoer"
    SUDOER="$SUDOER || echo '$USER ALL=NOPASSWD: ALL' > /etc/sudoers.d/$USER_sudoer"
else
    ROOT_CMD=
fi

# determine the operating system
if [ "$(uname -s)" = "Darwin" ]; then
    PLATFORM="mac"
    INSTALL="brew install"
elif [ -f /etc/debian_version]; then
    PLATFORM="debian"
    INSTALL="${ROOT_CMD} apt-get install"
elif [ -f /etc/arch-release ]; then
    PLATFORM="arch"
    INSTALL="${ROOT_CMD} pacman -S"
fi

############
# Functions

function bootstrap_mac() {
    # macOS isn't nice enough to ship with a package manager
    if ! which -s brew; then
        /usr/bin/ruby -e \
            "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
    brew update
}

function bootstrap_debian() {
    if [ -n "$ROOT_CMD"] && ! which -s sudo; then
        su -c "apt-get update; apt-get install sudo; $SUDOER" root
    else
        $ROOT_CMD apt-get update
    fi
}

funciton bootstrap_arch() {
    if [ -n "$ROOT_CMD"] && ! which -s sudo; then
        su -c "pacman -S sudo; $SUDOER" root
    fi
}

function bootstrap() {
    [ "$PLATFORM" = "mac" ] && bootstrap_mac
    [ "$PLATFORM" = "debian" ] && bootstrap_debian
    [ "$PLATFORM" = "arch" ] && bootstrap_arch

    $INSTALL git
    if ! [ -d ${DOTFILES} ]; then
        git clone ${DOTFILES_HTTPS} ${DOTFILES}
        git submodule update --init --recursive
    fi
}

############
# Execution

while [ "$1" != ""]; do
    PARAM=$(echo $1 | awk -F= '{print $1}')
    VALUE=$(echo $1 | awk -F= '{print $2}')
    case $PARAM in
        --platform)
            echo "$PLATFORM"
            exit
            ;;
        --installer)
            echo "$INSTALL"
            exit
            ;;
        *)
            bootstrap
            exit 0
            ;;
    esac
    shift
done
