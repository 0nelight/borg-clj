#!/bin/bash
set -euo pipefail

usage() {
  cat <<'USAGE'
recovery/build.sh [--borgi PATH] [--out DIR] [--suite NAME] [--clean]

Builds the borg-imager recovery USB image with Debian live-build. The result is a
hybrid ISO: write it to a USB stick with

    dd if=live-image-amd64.hybrid.iso of=/dev/sdX bs=4M oflag=direct status=progress

It boots on BIOS and UEFI, and on Secure Boot machines through Debian's
Microsoft-signed shim.

  --borgi PATH   the borgi binary to bake in (default: ../borgi next to this tree)
  --out DIR     where the ISO is written (default: recovery/out)
  --suite NAME  Debian suite (default: trixie)

Env: BORGCLJ_SSH_KEY or BORGCLJ_SSH_KEY_FILE puts a public key into the image
so root can be reached over ssh.
  --clean       lb clean --purge before building

Needs: live-build, debootstrap, xorriso, and root (live-build calls mount).
USAGE
}

export PATH="/usr/sbin:/sbin:$PATH"
HERE=$(cd "$(dirname "$0")" && pwd)
BORG_BIN="$HERE/../borgi"
OUT="$HERE/out"
SUITE=trixie
CLEAN=0
NETBOOT=0

while [ $# -gt 0 ]; do
  case "$1" in
    --borgi) BORG_BIN=$2; shift 2;;
    --out) OUT=$2; shift 2;;
    --suite) SUITE=$2; shift 2;;
    --clean) CLEAN=1; shift;;
    --netboot) NETBOOT=1; shift;;
    -h|--help) usage; exit 0;;
    *) echo "unknown option: $1" >&2; usage >&2; exit 2;;
  esac
done

for t in lb debootstrap xorriso; do
  command -v "$t" >/dev/null || { echo "missing: $t (apt install live-build debootstrap xorriso)" >&2; exit 1; }
done
[ "$(id -u)" = 0 ] || { echo "live-build needs root" >&2; exit 1; }
[ -x "$BORG_BIN" ] || { echo "no borgi binary at $BORG_BIN — build it with bin/build-native.sh" >&2; exit 1; }

case "$(file -b "$BORG_BIN" 2>/dev/null)" in
  *x86-64*ELF*|*ELF*x86-64*) ;;
  *) echo "warning: $BORG_BIN does not look like a linux x86-64 executable" >&2;;
esac

# For "borgi gui" borgi loads the AWT libraries native-image puts beside the
# binary (libawt.so, libfontmanager.so, ...). They must sit next to the program,
# hence a directory of its own and only a link in bin. AWT also wants lib/ below
# it as java.home/lib, even if it stays empty.
LIBDIR="$HERE/config/includes.chroot/usr/local/lib/borgi"
rm -rf "$LIBDIR"
install -D -m0755 "$BORG_BIN" "$LIBDIR/borgi"
install -d "$LIBDIR/lib"
for so in "$(dirname "$BORG_BIN")"/lib*.so; do
  [ -e "$so" ] && install -m0644 "$so" "$LIBDIR/"
done
[ -e "$LIBDIR/libawt_xawt.so" ] || \
  echo "warning: no AWT libraries next to $BORG_BIN -- \"borgi gui\" will not start in the image" >&2
install -d "$HERE/config/includes.chroot/usr/local/bin"
ln -sfn /usr/local/lib/borgi/borgi "$HERE/config/includes.chroot/usr/local/bin/borgi"

# Wer per SSH ins Rettungssystem darf. Normalerweise bleibt das hier leer:
# borg-authorized-keys holt die Liste beim Start vom Bootserver, damit sich ein
# Schluessel tauschen laesst, ohne eine Stunde lang ein Abbild neu zu bauen.
# BORGCLJ_SSH_KEY_FILE ist fuer den Fall gedacht, dass das Abbild von einem
# USB-Stick ohne Netz starten soll -- dann muss der Schluessel mit hinein.
# Die Datei liegt nicht im Repository: ein Schluessel gehoert nicht in die
# Versionsgeschichte.
AUTH="$HERE/config/includes.chroot/root/.ssh/authorized_keys"
install -d -m0700 "$(dirname "$AUTH")"
if [ -n "${BORGCLJ_SSH_KEY_FILE:-}" ]; then
  install -m0600 "$BORGCLJ_SSH_KEY_FILE" "$AUTH"
elif [ -n "${BORGCLJ_SSH_KEY:-}" ]; then
  printf '%s\n' "$BORGCLJ_SSH_KEY" > "$AUTH"
else
  : > "$AUTH"
  echo "note: kein Schluessel eingebacken -- beim Netzboot holt ihn borg-authorized-keys" >&2
  echo "      vom Bootserver. Fuer einen USB-Stick ohne Netz BORGCLJ_SSH_KEY_FILE setzen." >&2
fi
chmod 0600 "$AUTH"

cd "$HERE"
[ "$CLEAN" = 1 ] && lb clean --purge
BORGCLJ_SUITE="$SUITE" lb config
lb build

mkdir -p "$OUT"
mv -f live-image-amd64.hybrid.iso "$OUT/" 2>/dev/null || true

# Was der Netzbootserver braucht, liegt danach neben dem ISO: Kernel,
# Wurzeldateisystem und beide initrds. Die kleine ist kein Luxus -- manche
# UEFI-Firmware bekommt die volle nicht am Stueck in den Speicher.
if [ "$NETBOOT" = 1 ] && [ -d binary/live ]; then
  for f in vmlinuz initrd.img filesystem.squashfs; do
    [ -f "binary/live/$f" ] && cp -f "binary/live/$f" "$OUT/$f"
  done
  [ -f binary/live/initrd.img ] && "$HERE/bin/initrd-klein.sh" \
      binary/live/initrd.img "$OUT/initrd-klein.img"
fi

ls -lh "$OUT"/*.iso
# An if, not "[ ... ] && ls": without --netboot the test would be the last
# statement and the script would end with 1 -- a red CI run without an error.
if [ "$NETBOOT" = 1 ]; then
  ls -lh "$OUT"/vmlinuz "$OUT"/initrd*.img "$OUT"/filesystem.squashfs
fi
