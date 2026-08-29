#!/usr/bin/env bash
# Usage : bench-lsp-completion.sh <label> [--compare FILE]
# Exemple : ./bench-lsp-completion.sh baseline
#           ./bench-lsp-completion.sh optimized --compare tests/lsp-mode/bench-results/baseline-XXXX.txt

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(dirname "$(dirname "$DIR")")"   # deux parents au-dessus de tests/lsp-mode
LABEL="${1:?usage: $0 <label> [--compare FILE]}"

# refuse arbre sale (sinon avant/après ambigü)
if [ -n "$(git -C "$ROOT" status --porcelain)" ]; then
  echo "ERREUR: arbre git sale, comparaison avant/après impossible" >&2
  exit 1
fi

# libcrypto requis par jsonrpc (dépendance transitives de lem-lsp-mode)
# NB : le hash nix-store est machine-spécifique ; remplacer par le votre si_gc
export LD_LIBRARY_PATH="/nix/store/4nyr7rz1lzlfcn9gs6hc0wimlw3rsagk-openssl-3.6.3/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

OUTDIR="$DIR/bench-results"
mkdir -p "$OUTDIR"
OUT="$OUTDIR/${LABEL}-$(date +%Y%m%d-%H%M%S).txt"

# En-tête
{
  echo "label=$LABEL rev=$(git -C "$ROOT" rev-parse --short HEAD) date=$(date -Iseconds)"
} > "$OUT"

# Bench. Le tail coupe la bannière sbcl ; l'ERREUR (si invariants cassés) est visible
# car run-benchmark assert-bruyamment et sbcl sort non-zéro.
sbcl --dynamic-space-size 4GiB --noinform --no-sysinit --no-userinit \
     --load "$ROOT/.qlot/setup.lisp" \
     --eval '(asdf:load-system "lem-tests")' \
     --load "$DIR/benchmarks.lisp" \
     --eval "(lem-tests/lsp-mode/bench:run-benchmark '(100 1000 4000))" \
     --eval '(uiop:quit 0)' 2>&1 | tee -a "$OUT"

echo "Résultats : $OUT"

# Comparaison côte à côte optionnelle
if [ "${2:-}" = "--compare" ] && [ -n "${3:-}" ]; then
  echo ""
  echo "=== comparaison $(basename "$3") vs courant ==="
  # aligne les lignes 'scenario + métriques' des deux fichiers
  paste "$3" "$OUT" | sed -n 's/^\(B[0-9][^\t]*\)\t\(.*\)$/\2 | \1/p' | column -t
  # NB : le paste assume une tabulation ; adaptez si besoin
fi