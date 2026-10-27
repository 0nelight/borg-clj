#!/usr/bin/env bash
set -euo pipefail
HERE=$(cd "$(dirname "$0")/.." && pwd)

# Reihenfolge: was der Aufrufer vorgibt, dann die Installation im Projekt,
# dann der Suchpfad. Die Fassung muss zu der in .github/workflows/native.yml
# passen (GraalVM Community, JDK 25) -- aeltere koennen borgclj.win.ffm nicht
# bauen: native-image bricht dort an SymbolLookup/findEntry0 ab, auch wenn der
# Windows-Zweig auf Linux nie laeuft.
if [ -n "${GRAALVM_HOME:-}" ]; then
  NATIVE_IMAGE="$GRAALVM_HOME/bin/native-image"
elif [ -x "$HERE/graalvm/bin/native-image" ]; then
  NATIVE_IMAGE="$HERE/graalvm/bin/native-image"
else
  NATIVE_IMAGE=$(command -v native-image || true)
fi
[ -x "$NATIVE_IMAGE" ] || { echo "no native-image: put GraalVM in ./graalvm, set GRAALVM_HOME, or put it on PATH" >&2; exit 1; }
OUT="${1:-borgi}"
cd "$HERE"
rm -rf classes && mkdir -p classes
clojure -M -e "(binding [*compile-path* \"classes\"] (compile 'borgclj.main))"
CP="classes:$(clojure -Spath)"
"$NATIVE_IMAGE" \
  -cp "$CP" \
  -o "$OUT" \
  --no-fallback \
  --enable-native-access=ALL-UNNAMED \
  --features=clj_easy.graal_build_time.InitClojureClasses \
  --initialize-at-build-time=borgclj \
  -march=compatibility \
  -H:+UnlockExperimentalVMOptions \
  -H:IncludeResources='.*\.(properties|so|dll|dylib)$' \
  -H:IncludeResources='app\.(css|js)$' \
  -H:IncludeLocales=de,en \
  -H:ConfigurationFileDirectories=graal \
  borgclj.main
ls -l "$OUT"
