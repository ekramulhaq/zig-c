#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────
#  db/run.sh  –  Build and launch SimpleDB
#
#  Usage:
#    cd /path/to/zig-c
#    bash db/run.sh            # build + run interactive REPL
#    bash db/run.sh --build    # build only
# ─────────────────────────────────────────────────────────────
set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DB_DIR="$ROOT/db"
COMPILER="$ROOT/zig-out/bin/compiler"
SRC="$DB_DIR/src/simpledb.simple"
ASM="$DB_DIR/simpledb.s"
BIN="$DB_DIR/simpledb"
INCLUDE="-I $DB_DIR/include"

# Detect arch
ARCH=$(uname -m)
if [ "$ARCH" = "arm64" ]; then
    TARGET="arm64"
else
    TARGET="x86_64"
fi

echo "═══════════════════════════════════════════"
echo "  SimpleDB Build Script"
echo "  Arch   : $TARGET"
echo "  Source : $SRC"
echo "═══════════════════════════════════════════"

# Step 1 – Build the zig-c compiler (if not already built)
if [ ! -f "$COMPILER" ]; then
    echo "[1/3] Building zig-c compiler..."
    (cd "$ROOT" && zig build)
else
    echo "[1/3] Compiler already built – skipping zig build."
fi

# Step 2 – Compile simpledb.simple → assembly
echo "[2/3] Compiling $SRC ..."
"$COMPILER" --arch "$TARGET" $INCLUDE "$SRC"
mv "$ROOT/out.asm" "$ASM"
echo "      Assembly written to $ASM"

# Step 3 – Assemble + link with clang
echo "[3/3] Linking → $BIN ..."
clang -o "$BIN" "$ASM"
echo "      Binary written to $BIN"
echo ""
echo "═══════════════════════════════════════════"
echo "  Build successful!"
echo "═══════════════════════════════════════════"
echo ""

# Run unless --build flag given
if [ "$1" != "--build" ]; then
    echo "  Launching SimpleDB..."
    echo ""
    exec "$BIN"
fi
