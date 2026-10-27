#!/bin/bash
# Baut aus der initrd des Abbilds eine kleine Fassung ohne Grafik- und
# Exoten-Firmware.
#
# Warum ueberhaupt: live-build liefert MODULES=most, und initramfs-tools legt
# dann die Firmware aller mitgenommenen Treiber dazu. Das sind ueber 200 MB,
# davon allein 152 MB nvidia und 35 MB netronome -- Sachen, die beim Starten
# niemand braucht. Manche UEFI-Firmware bekommt eine so grosse initrd nicht am
# Stueck in den Speicher; darum bootet der Netzweg die kleine Fassung.
#
# Was bleibt: alles fuer Netz und Platten. Ohne Netz-Firmware kaeme ein
# Rechner, dessen Karte welche braucht, beim Netzboot gar nicht erst hoch --
# und das Wurzeldateisystem holt er sich ueber genau dieses Netz. Die
# ausgelassene Firmware liegt vollstaendig im squashfs, wo sie spaeter
# gebraucht wird.
#
#   initrd-klein.sh QUELLE ZIEL
set -euo pipefail

SRC=${1:?usage: initrd-klein.sh QUELLE ZIEL}
DST=${2:?usage: initrd-klein.sh QUELLE ZIEL}

[ "$(id -u)" = 0 ] || { echo "braucht root: cpio muss Eigentuemer erhalten" >&2; exit 1; }
command -v unmkinitramfs >/dev/null || { echo "unmkinitramfs fehlt (initramfs-tools)" >&2; exit 1; }

# Grafik, SmartNICs und Geraete, die es in einem Rettungssystem nicht gibt.
# Bewusst NICHT dabei: bnx2*, cxgb*, intel (e1000/iwlwifi), mediatek, brcm,
# realtek, ti-connectivity, qlogic -- daran haengt der Netzboot.
WEG="nvidia netronome i915 radeon amdgpu liquidio LENOVO dell qcom mrvl ath11k ath12k"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

unmkinitramfs "$SRC" "$TMP/aus"

VOR=$(du -sb "$TMP/aus" | cut -f1)
for f in $WEG; do rm -rf "$TMP/aus/main/usr/lib/firmware/$f"; done
NACH=$(du -sb "$TMP/aus" | cut -f1)

# Ohne diese beiden startet gar nichts -- lieber hier abbrechen als ein
# Abbild ausliefern, das im Kernel-Panik endet.
[ -e "$TMP/aus/main/init" ] || { echo "FEHLER: kein /init im Hauptsegment" >&2; exit 1; }
[ -d "$TMP/aus/main/usr/lib/firmware" ] || { echo "FEHLER: Firmware-Verzeichnis weg" >&2; exit 1; }

pack() { ( cd "$1" && find . -print0 | cpio --null -o -H newc --quiet ); }

: > "$DST"
# Die Mikrocode-Segmente bleiben unkomprimiert und muessen auf 4 Byte enden,
# sonst findet der Kernel den Anfang des naechsten Segments nicht.
for seg in "$TMP/aus/early" "$TMP/aus/early2" "$TMP/aus/early3"; do
  [ -d "$seg" ] || continue
  pack "$seg" >> "$DST"
  python3 - "$DST" <<'PY'
import sys, os
p = sys.argv[1]
n = os.path.getsize(p)
if n % 4:
    open(p, "ab").write(b"\0" * (4 - n % 4))
PY
done
pack "$TMP/aus/main" | gzip -9 >> "$DST"

echo "initrd: $(numfmt --to=iec $(stat -c%s "$SRC")) -> $(numfmt --to=iec $(stat -c%s "$DST"))"
echo "  entpackt: $(numfmt --to=iec "$VOR") -> $(numfmt --to=iec "$NACH")"
