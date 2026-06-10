#!/bin/sh
# Regenerate the emacs symlink farm: one stub per emacs/**/*.el, so a renamed or
# added conf file needs no hand-written symlink_*.tmpl. claude's farm is
# hand-curated (USER_CLAUDE.md->CLAUDE.md rename, selective inclusion), stays manual.
set -eu
cd "$(dirname "$0")"

farm=home/dot_config/emacs
find "$farm" -name 'symlink_*.el.tmpl' -delete

find emacs -name '*.el' -type f | sort | while IFS= read -r f; do
  rel=${f#emacs/}
  out=${farm}/$(dirname "$rel"); out=${out%/.}
  mkdir -p "$out"
  printf '{{ .chezmoi.sourceDir }}/../emacs/%s' "$rel" > "$out/symlink_$(basename "$rel").tmpl"
done
