#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
VERSION="${VERSION:-0.1.0}"
PAYLOAD="${1:?usage: build.sh <payload-dir> ; payload-dir contains the files for INSTDIR (borgi.exe, app/, runtime/)}"
PAYLOAD="$(cd "$PAYLOAD" && pwd)"
test -f "$PAYLOAD/borgi.exe" || { echo "kein borgi.exe in $PAYLOAD" >&2; exit 1; }
OUT="borg-imager-setup-${VERSION}.exe"
makensis -V2 -NOCD -DVERSION="$VERSION" -DPAYLOAD="$PAYLOAD" -DOUTFILE="$OUT" borg-imager.nsi
ls -l "$OUT"
