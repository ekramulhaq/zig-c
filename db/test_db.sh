#!/usr/bin/env bash
# db/test_db.sh  --  Automated tests for SimpleDB
#
# Usage:
#   cd /path/to/zig-c
#   bash db/run.sh --build
#   bash db/test_db.sh

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/db/simpledb"
# Run from db/ so simpledb.dat lands in db/
RUNDIR="$ROOT/db"
DB_FILE="$RUNDIR/simpledb.dat"
PASS=0
FAIL=0

cd "$RUNDIR"
rm -f "$DB_FILE"

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

pass() { echo -e "${GREEN}  [PASS]${NC} $1"; PASS=$((PASS+1)); }
fail() { echo -e "${RED}  [FAIL]${NC} $1 -- $2"; FAIL=$((FAIL+1)); }

# run_cmd: reset db, run input, check for expected substring
run_cmd() {
    local label="$1"
    local input="$2"
    local expect="$3"
    rm -f "$DB_FILE"
    local out
    out=$(printf '%s\n' "$input" | "$BIN" 2>&1)
    if echo "$out" | grep -qF "$expect"; then
        pass "$label"
    else
        fail "$label" "Expected '$expect' in output. Got: $out"
    fi
}

echo ""
echo "==================================================="
echo "  SimpleDB -- Automated Test Suite"
echo "==================================================="
echo ""

# T1: Basic insert
run_cmd "T1: INSERT row id=1" \
    "INSERT INTO users VALUES (1, alice, alice@example.com);" \
    "Inserted: id=1"

# T2: Insert second row
run_cmd "T2: INSERT row id=2" \
    "INSERT INTO users VALUES (2, bob, bob@test.org);" \
    "Inserted: id=2"

# T3: SELECT ALL shows both rows
run_cmd "T3: SELECT * shows rows" \
    "$(printf 'INSERT INTO users VALUES (1, alice, alice@example.com);\nINSERT INTO users VALUES (2, bob, bob@test.org);\nSELECT * FROM users;')" \
    "2 row(s) returned"

# T4: SELECT WHERE id
run_cmd "T4: SELECT WHERE id=1 finds alice" \
    "$(printf 'INSERT INTO users VALUES (1, alice, alice@ex.com);\nSELECT * FROM users WHERE id = 1;')" \
    "alice"

# T5: SELECT WHERE non-existent
run_cmd "T5: SELECT WHERE id=99 returns 0 rows" \
    "$(printf 'INSERT INTO users VALUES (1, alice, a@a.com);\nSELECT * FROM users WHERE id = 99;')" \
    "No row found"

# T6: Duplicate key is rejected
run_cmd "T6: Duplicate id rejected" \
    "$(printf 'INSERT INTO users VALUES (1, alice, a@a.com);\nINSERT INTO users VALUES (1, bob, b@b.com);')" \
    "duplicate id"

# T7: DELETE
run_cmd "T7: DELETE removes row" \
    "$(printf 'INSERT INTO users VALUES (1, alice, a@a.com);\nDELETE FROM users WHERE id = 1;\nSELECT * FROM users;')" \
    "0 row(s) returned"

# T8: DELETE non-existent
run_cmd "T8: DELETE missing id reports error" \
    "$(printf 'DELETE FROM users WHERE id = 42;')" \
    "No row found"

# T9: COUNT
run_cmd "T9: COUNT after 3 inserts = 3" \
    "$(printf 'INSERT INTO users VALUES (1, a, a@a.com);\nINSERT INTO users VALUES (2, b, b@b.com);\nINSERT INTO users VALUES (3, c, c@c.com);\nCOUNT')" \
    "COUNT(*) = 3"

# T10: COUNT after delete
run_cmd "T10: COUNT after delete = 2" \
    "$(printf 'INSERT INTO users VALUES (1, a, a@a.com);\nINSERT INTO users VALUES (2, b, b@b.com);\nINSERT INTO users VALUES (3, c, c@c.com);\nDELETE FROM users WHERE id = 2;\nCOUNT')" \
    "COUNT(*) = 2"

# T11: Invalid id
run_cmd "T11: Insert id=0 is rejected" \
    "INSERT INTO users VALUES (0, bad, bad@bad.com);" \
    "positive integer"

# T12: .help output
run_cmd "T12: .help shows usage" \
    ".help" \
    "INSERT INTO users"

# T13: Unknown command
run_cmd "T13: Unknown command handled" \
    "FOOBAR;" \
    "Unknown command"

# T14: Case-insensitive keywords
run_cmd "T14: lowercase 'insert' works" \
    "insert into users values (5, eve, eve@eve.com);" \
    "Inserted: id=5"

# T15: Persistence round-trip
rm -f "$DB_FILE"
printf 'INSERT INTO users VALUES (10, persistent, p@p.com);\n.exit\n' | "$BIN" > /dev/null 2>&1 || true
# Second session: verify row survived
run_cmd_persist() {
    local label="$1"
    local input="$2"
    local expect="$3"
    local out
    out=$(printf '%s\n' "$input" | "$BIN" 2>&1)
    if echo "$out" | grep -qF "$expect"; then
        pass "$label"
    else
        fail "$label" "Expected '$expect' in output. Got: $out"
    fi
}
run_cmd_persist "T15: Persistence -- row survives .exit + reopen" \
    "SELECT * FROM users WHERE id = 10;" \
    "persistent"

echo ""
echo "==================================================="
printf "  Results: ${GREEN}%d passed${NC}, ${RED}%d failed${NC}\n" "$PASS" "$FAIL"
echo "==================================================="
echo ""

rm -f "$DB_FILE"

if [ "$FAIL" -gt 0 ]; then exit 1; fi
exit 0
