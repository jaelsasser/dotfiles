#! /usr/bin/env bash
# Modified from jimeh/docker-znc

# Options.
DATADIR="/var/lib/znc/"

# Build modules from source.
if [ -d "${DATADIR}/modules" ]; then
  # Find and build modules
  for module in $(find ${DATADIR}/modules -name "*.cpp"); do
    echo "Building module $module..."
    pushd "$(dirname $module)"
    znc-buildmod "$module"
    popd
  done
fi

# Start ZNC.
exec znc --foreground --datadir="$DATADIR" $@
