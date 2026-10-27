#!/usr/bin/env bash
# native-image does not cross-compile: an aarch64 binary has to be linked on
# an aarch64 machine.
set -euo pipefail
HOST="${1:?usage: build-native-remote.sh HOST [REMOTE_DIR] [OUT]}"
DIR="${2:-/tmp/borg-imager-build}"
OUT="${3:-borgi}"
cd "$(dirname "$0")/.."

echo "== compiling Clojure to bytecode locally"
rm -rf classes target/bundle && mkdir -p classes target/bundle/jars
clojure -M -e "(binding [*compile-path* \"classes\"] (compile 'borgclj.main))"
for j in $(clojure -Spath | tr ':' '\n' | grep '\.jar$'); do cp "$j" target/bundle/jars/; done
cp -r classes target/bundle/classes
cp -r graal target/bundle/graal
tar -C target/bundle -czf target/bundle.tgz classes jars graal

echo "== shipping $(du -h target/bundle.tgz | cut -f1) to $HOST:$DIR"
ssh "$HOST" "mkdir -p '$DIR'"
scp -q target/bundle.tgz "$HOST:$DIR/"
ssh "$HOST" "cd '$DIR' && rm -rf classes jars graal && tar xzf bundle.tgz"

if [ -n "${GRAALVM_TGZ:-}" ]; then
  if ! ssh "$HOST" "test -x '$DIR'/graalvm-*/bin/native-image"; then
    echo "== shipping GraalVM ($(du -h "$GRAALVM_TGZ" | cut -f1))"
    scp -q "$GRAALVM_TGZ" "$HOST:$DIR/graalvm.tgz"
    ssh "$HOST" "cd '$DIR' && tar xzf graalvm.tgz && rm graalvm.tgz"
  fi
fi

echo "== linking on $HOST (this is the slow part on a small machine)"
ssh "$HOST" "cd '$DIR' && NI=\$(echo graalvm-*/bin/native-image) && \
  \$NI -cp 'classes:'\$(ls jars/*.jar | tr '\n' ':') -o '$OUT' \
    --no-fallback \
    --enable-native-access=ALL-UNNAMED \
    --features=clj_easy.graal_build_time.InitClojureClasses \
    --initialize-at-build-time=borgclj \
    -march=compatibility \
    -H:+UnlockExperimentalVMOptions \
    -H:IncludeResources='.*\.(properties|so|dll|dylib)\$' \
    -H:IncludeResources='app\.(css|js)\$' \
    -H:ConfigurationFileDirectories=graal \
    borgclj.main && ls -l '$OUT'"

echo "== done: $HOST:$DIR/$OUT"
