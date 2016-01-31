#!/usr/bin/env bash

# determine the operating system
if [[ "$OSTYPE" == darwin* ]]; then
  export HAS_OSX=1
  confd_extra='*osx'
else
  export HAS_LINUX=1
  confd_extra='*deb*'
fi

# make sure all our submodules are initialized before we link
git submodule update --init --recursive

# ask for sudo command, keep the sudo alive
sudo -v
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# execute all files in the conf.d directory
for script in $(find conf.d -name "*all*" -o -name "$confd_extra"); do
    bash -x ${script}
done
