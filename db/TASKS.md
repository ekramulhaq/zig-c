# SimpleDB – RDBMS Task Tracker

> **Goal**: Build a working Relational Database Management System (RDBMS) written entirely in
> the "Simple" C-subset language, compiled by the `zig-c` compiler. The DB lives in the
> `db/` folder inside the `zig-c` project.

---

## Architecture Overview

```
db/
├── TASKS.md          ← this file
├── include/          ← DB-specific headers (.h)
│   ├── db.h          ← core types: Row, Table, Cursor, Pager, DB
│   └── btree.h       ← B-Tree page layout constants
├── src/              ← implementation files (.simple)
│   ├── pager.simple  ← page cache / file I/O helpers
│   ├── btree.simple  ← B-Tree insert / search / split
│   ├── table.simple  ← Table open / close / row encode-decode
│   ├── cursor.simple ← Cursor (iterator over rows)
│   ├── parser.simple ← SQL mini-parser (SELECT, INSERT, CREATE TABLE)
│   └── main.simple   ← REPL entry point
└── run.sh            ← build + run helper script
```

### Data Model
- **Page size**: 4096 bytes (matching OS page size)
- **Storage**: B-Tree, one file per table (binary format)
- **Row format**: fixed-width binary record
  - `id`   : 4 bytes (int)
  - `name` : 32 bytes (char[32])
  - `email`: 64 bytes (char[64])
  - Total  : 100 bytes per row
- **Max rows per page**: 40 (4000 / 100, leaving header space)
- **Max pages per table**: 100
- **Max rows total**: 4000

### Supported SQL
```sql
CREATE TABLE users (id INT, name TEXT, email TEXT);
INSERT INTO users VALUES (1, 'alice', 'alice@example.com');
SELECT * FROM users;
SELECT * FROM users WHERE id = 1;
.exit
.help
```

---

## Task Breakdown

### Phase 1 – Foundation & Types [ COMPLETE ]
- [x] **T-01** Create `db/` directory structure
- [x] **T-02** Write `db/include/db.h` – core struct definitions (Row, Page, Table, DB)
- [x] **T-03** Write `db/include/btree.h` – B-Tree constants & page header layout

### Phase 2 – Storage Layer
- [ ] **T-04** `db/src/pager.simple` – open file, read/write pages into a page-cache array
- [ ] **T-05** `db/src/btree.simple` – leaf-node insert, split, internal-node forward

### Phase 3 – Table & Cursor
- [ ] **T-06** `db/src/table.simple` – row encode/decode (serialize to 100-byte slot)
- [ ] **T-07** `db/src/cursor.simple` – advance cursor, value-at-cursor

### Phase 4 – SQL Parser
- [ ] **T-08** `db/src/parser.simple` – tokenise + parse INSERT / SELECT / CREATE TABLE

### Phase 5 – REPL & Integration
- [ ] **T-09** `db/src/main.simple` – read-eval-print loop, wire all modules
- [ ] **T-10** `db/run.sh` – compile chain: simple → asm → link → ./simpledb

### Phase 6 – Testing
- [ ] **T-11** Write `db/test_db.sh` – automated test: insert rows, query, verify output
- [ ] **T-12** Edge-case tests: duplicate key, table full, page boundary

---

## Progress Log

| Date       | Task  | Status   | Notes                              |
|------------|-------|----------|------------------------------------|
| 2026-02-24 | T-01  | DONE     | `db/` scaffold created             |
| 2026-02-24 | T-02  | DONE     | `db/include/db.h` written          |
| 2026-02-24 | T-03  | DONE     | `db/include/btree.h` written       |
| 2026-02-24 | T-04  | pending  |                                    |
| 2026-02-24 | T-05  | pending  |                                    |
| 2026-02-24 | T-06  | pending  |                                    |
| 2026-02-24 | T-07  | pending  |                                    |
| 2026-02-24 | T-08  | pending  |                                    |
| 2026-02-24 | T-09  | pending  |                                    |
| 2026-02-24 | T-10  | pending  |                                    |
| 2026-02-24 | T-11  | pending  |                                    |
| 2026-02-24 | T-12  | pending  |                                    |

---

## Design Decisions & Constraints

| Constraint | Reason |
|---|---|
| No `fopen`/`fread` – use `open`/`read`/`write`/`close` | The compiler's stdlib only exposes POSIX primitives |
| Fixed-width rows | Avoids variable-length string complexity |
| In-memory page cache (array of 100 page pointers) | malloc per page; flush on `.exit` |
| Single-file per table | Keeps the implementation simple |
| No transactions / WAL | Out of scope for v1 |
| B-Tree leaf-only for v1 | Internal node promotion can be Phase 2 |
