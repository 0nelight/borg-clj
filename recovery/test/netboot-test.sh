#!/bin/bash
# Boots the recovery system in QEMU the way a machine on the network does --
# over HTTP only, no TFTP -- and logs the serial console with timestamps until
# the interface is up on tty1.
#
#   UEFI HTTP boot (OVMF) -> grubnetx64.efi -> vmlinuz + initrd over GRUB's http
#   -> live-boot fetches filesystem.squashfs with fetch=http:// -> systemd
#   -> borg-ui.service on tty1 -> "borgi gui: window open"
#
# Input is what "recovery/build.sh --netboot" leaves in recovery/out.
#
#   sudo recovery/test/netboot-test.sh [--dir DIR] [--initrd FILE] [--mem MB]
#                                      [--timeout S] [--append P] [--hold S]
#
#   --dir      kernel, initrds and squashfs (default: recovery/out)
#   --initrd   which initrd GRUB loads (default: initrd-klein.img)
#   --mem      memory of the VM; the squashfs lives entirely in RAM (4096)
#   --timeout  give up if the interface is not up by then (900)
#   --append P more kernel parameters, e.g. borgclj.lang=de
#   --hold S   keep the VM running S seconds after the screenshot, e.g. to
#              look around over ssh (default 0)
#
# If DIR/authorized_keys exists, the web server hands it out next to the
# squashfs; borg-authorized-keys fetches it at boot, and
# "ssh root@192.168.77.50" gets into the VM (the address is fixed, see dnsmasq).
#
# Results in DIR/netboot-test/: timeline.log and tty1.png (tty1.ppm without
# python3-pil), the screenshot of tty1 at the end.
#
# PXE is switched off in the firmware. Otherwise OVMF waits about 60 s each on
# PXEv4 and PXEv6 before it tries HTTP -- the same two silent minutes a real
# machine loses when PXE comes before HTTP in its boot order.
#
# Needs: qemu-system-x86, ovmf, dnsmasq-base, iproute2, python3,
# grub-efi-amd64-signed (for grubnetx64.efi.signed) and root (tap device).
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
DIR="$HERE/../out"
INITRD=initrd-klein.img
MEM=4096
TIMEOUT=900
HOLD=0
APPEND=
NET=${BORGCLJ_TEST_NET:-192.168.77}
TAP=${BORGCLJ_TEST_TAP:-borgtest0}
MAC=52:54:00:b0:4c:01
PORT=8080
GRUB_EFI=${GRUB_EFI:-/usr/lib/grub/x86_64-efi-signed/grubnetx64.efi.signed}
OVMF_CODE=${OVMF_CODE:-/usr/share/OVMF/OVMF_CODE_4M.fd}
OVMF_VARS=${OVMF_VARS:-/usr/share/OVMF/OVMF_VARS_4M.fd}

while [ $# -gt 0 ]; do
  case "$1" in
    --dir) DIR=$2; shift 2 ;;
    --initrd) INITRD=$2; shift 2 ;;
    --mem) MEM=$2; shift 2 ;;
    --timeout) TIMEOUT=$2; shift 2 ;;
    --append) APPEND=$2; shift 2 ;;
    --hold) HOLD=$2; shift 2 ;;
    -h|--help) sed -n '2,35p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done

[ "$(id -u)" = 0 ] || { echo "needs root: tap device and dnsmasq" >&2; exit 1; }
for tool in qemu-system-x86_64 dnsmasq ip python3; do
  command -v "$tool" >/dev/null || { echo "missing: $tool" >&2; exit 1; }
done
DIR=$(cd "$DIR" && pwd)
for f in vmlinuz "$INITRD" filesystem.squashfs; do
  [ -f "$DIR/$f" ] || { echo "missing: $DIR/$f -- run recovery/build.sh --netboot first" >&2; exit 1; }
done
for f in "$GRUB_EFI" "$OVMF_CODE" "$OVMF_VARS"; do
  [ -f "$f" ] || { echo "missing: $f" >&2; exit 1; }
done

RESULT="$DIR/netboot-test"
WWW=$(mktemp -d)
mkdir -p "$RESULT" "$WWW/grub" "$WWW/live"
DNSPID= HTTPPID=
cleanup() {
  [ -n "$HTTPPID" ] && kill "$HTTPPID" 2>/dev/null || true
  [ -n "$DNSPID" ] && kill "$DNSPID" 2>/dev/null || true
  ip link del "$TAP" 2>/dev/null || true
  rm -rf "$WWW"
}
trap cleanup EXIT

# The web server hands out GRUB, its grub.cfg and the image from one tree.
cp "$GRUB_EFI" "$WWW/grubx64.efi"
for f in vmlinuz "$INITRD" filesystem.squashfs; do ln -s "$DIR/$f" "$WWW/live/$f"; done
# borg-authorized-keys looks for the list next to the squashfs (fetch=...)
[ -f "$DIR/authorized_keys" ] && ln -s "$DIR/authorized_keys" "$WWW/live/authorized_keys"
# The same parameters as --bootappend-live in recovery/auto/config, but without
# quiet and with a serial console -- otherwise there would be nothing to log.
cat > "$WWW/grub/grub.cfg" <<EOF
set timeout=0
serial --unit=0 --speed=115200
terminal_output serial console
terminal_input serial console
menuentry recovery {
  echo "loading kernel"
  linux /live/vmlinuz boot=live components noeject fetch=http://$NET.1:$PORT/live/filesystem.squashfs console=tty0 console=ttyS0,115200 $APPEND
  echo "loading $INITRD"
  initrd /live/$INITRD
  echo "starting"
}
EOF

ip tuntap add dev "$TAP" mode tap
ip addr add "$NET.1/24" dev "$TAP"
ip link set "$TAP" up

# Option 60 "HTTPClient" in the offer is how UEFI recognises an HTTP boot
# offer; without it OVMF drops the URL.
dnsmasq --keep-in-foreground --conf-file=/dev/null --interface="$TAP" \
  --bind-interfaces --port=0 --dhcp-range="$NET.50,$NET.99,1h" \
  --dhcp-vendorclass=set:http,HTTPClient \
  --dhcp-option-force=tag:http,60,HTTPClient \
  --dhcp-boot="tag:http,http://$NET.1:$PORT/grubx64.efi" \
  --dhcp-host="$MAC,$NET.50" \
  --log-facility="$RESULT/dnsmasq.log" &
DNSPID=$!
python3 -m http.server "$PORT" --bind "$NET.1" --directory "$WWW" \
  > "$RESULT/http.log" 2>&1 &
HTTPPID=$!
sleep 1

VARS="$RESULT/OVMF_VARS.fd"
cp "$OVMF_VARS" "$VARS"
rm -f "$RESULT/tty1.ppm" "$RESULT/tty1.png"
set +e
python3 "$HERE/serial-timeline.py" \
  --stop 'borgi gui: window open' --linger 3 --shot "$RESULT/tty1.ppm" --hold "$HOLD" \
  "$RESULT/timeline.log" "$TIMEOUT" -- \
  -m "$MEM" -smp 2 -no-reboot \
  -drive if=pflash,format=raw,readonly=on,file="$OVMF_CODE" \
  -drive if=pflash,format=raw,file="$VARS" \
  -fw_cfg name=opt/org.tianocore/IPv4PXESupport,string=no \
  -fw_cfg name=opt/org.tianocore/IPv6PXESupport,string=no \
  -netdev tap,id=n0,ifname="$TAP",script=no,downscript=no \
  -device virtio-net-pci,netdev=n0,mac="$MAC",romfile=
RC=$?
set -e

python3 -c "from PIL import Image; Image.open('$RESULT/tty1.ppm').save('$RESULT/tty1.png')" \
  2>/dev/null && rm -f "$RESULT/tty1.ppm" || true

# The kernel lines with a [ timestamp ] are too many for the overview; the
# full log is in timeline.log.
grep -vE '^ +[0-9.]+s +\[ *[0-9.]+\] ' "$RESULT/timeline.log" \
  | grep -E 'no output|HTTP Boot|loading|starting|Run /init|wget|saved|Welcome|borg|FAILED|TARGET|GAVE UP|QEMU ended|screenshot' || true
echo
echo "log: $RESULT/timeline.log"
exit "$RC"
