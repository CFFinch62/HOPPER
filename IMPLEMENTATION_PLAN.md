# HOPPER Implementation Plan
## HOPPER: COBOL Teaching Environment (COBOL IDE)

**Author:** Chuck Finch — Fragillidae Software  
**Reference Design:** FORTE (Fortran IDE) — see `../FORTE`  
**Stack:** Python 3.10+, PyQt6, ptyprocess, pyte  
**Target:** Linux-first, portable to macOS/Windows

> Named in honor of **Rear Admiral Grace Murray Hopper** (1906–1992),  
> the pioneering computer scientist who invented COBOL and the first compiler.

---

## Architecture Overview

```
HOPPER/
├── hopper/
│   ├── main.py                  # Entry point
│   ├── app.py                   # MainWindow (QMainWindow)
│   ├── browser/
│   │   └── file_browser.py      # Left-panel file tree
│   ├── config/
│   │   ├── cobol_detector.py    # Detect gnucobol/cobc compilers
│   │   ├── settings.py          # JSON settings (~/.config/hopper/)
│   │   ├── settings_dialog.py   # Tabbed preferences dialog
│   │   └── themes.py            # Dark/Light theme dataclasses + QSS
│   ├── editor/
│   │   ├── code_editor.py       # QPlainTextEdit + line number gutter
│   │   ├── find_replace.py      # Find/Replace dialog
│   │   ├── highlighter.py       # COBOL syntax highlighter
│   │   └── tab_widget.py        # Multi-tab editor container
│   └── terminal/
│       ├── pty_process.py       # PTY process wrapper (QThread)
│       └── terminal_widget.py   # pyte-based terminal display widget
├── examples/                    # Sample .cob programs
├── images/                      # hopper_icon.png
├── requirements.txt
├── run.sh / setup.sh
└── IMPLEMENTATION_PLAN.md
```

### Key Design Decisions vs FORTE

| Aspect | FORTE | HOPPER |
|--------|-------|--------|
| Language | Fortran (.f90/.f/.for) | COBOL (.cob/.cbl/.cobol) |
| Runtime model | Compile → Run | Compile → Run (identical) |
| Compiler detector | fortran_detector | cobol_detector |
| File extensions | .f90 .f95 .f03 .f .for .f77 | .cob .cbl .cobol .cpy |
| Tab width default | 3 | 4 (free-format) |
| Settings dir | ~/.config/forte | ~/.config/hopper |
| Build system | gfortran | cobc (GnuCOBOL) |
| Case sensitivity | Case-insensitive | Case-insensitive |
| Comment style | `!` line | `*` in col 7 (fixed) or `*>` (free) |
| Source format | Free-form | Free-form (default); fixed-form optional |
| String delimiter | `'` and `"` | `'` and `"` |
| Clean artifacts | binary only | binary only (cobc statically links) |
| Division structure | N/A | IDENTIFICATION / ENVIRONMENT / DATA / PROCEDURE |

---

## Phase Checklist

| Phase | Title | Status |
|-------|-------|--------|
| 1 | Project Bootstrap | ☐ |
| 2 | Config Layer | ☐ |
| 3 | File Browser | ☐ |
| 4 | Editor Layer | ☐ |
| 5 | Terminal Widget | ☐ |
| 6 | Build System | ☐ |
| 7 | Main Window | ☐ |
| 8 | Examples & Polish | ☐ |
| 9 | Packaging | ☐ |

Mark each phase ☑ when all tasks pass acceptance criteria.

---

## Phase 1 — Project Bootstrap

**Goal:** The project runs and shows a placeholder window.

### Tasks

- [ ] 1.1 Verify `hopper/main.py` launches cleanly with `python3 -m hopper.main`
- [ ] 1.2 Confirm `setup.sh` creates venv and installs `requirements.txt` without errors
- [ ] 1.3 Confirm `run.sh` activates venv and launches the app
- [ ] 1.4 Placeholder `MainWindow` appears (title "HOPPER - COBOL Teaching Environment", 1024×768)
- [ ] 1.5 Create `images/` directory; add placeholder `hopper_icon.png`

### Files

| File | Action |
|------|--------|
| `hopper/main.py` | Create — copy FORTE/forte/main.py; rename forte→hopper |
| `hopper/app.py` | Create — stub placeholder window |
| `hopper/__init__.py` | Create — `__version__ = "0.1.0"` |
| `setup.sh` | Create — copy FORTE/setup.sh; rename forte→hopper |
| `run.sh` | Create — copy FORTE/run.sh; rename forte→hopper |
| `requirements.txt` | Create — PyQt6, ptyprocess, pyte, pyinstaller |
| `images/hopper_icon.png` | Create (64×64 PNG placeholder) |

### Acceptance Criteria

- `./run.sh` shows a window titled "HOPPER - COBOL Teaching Environment"
- No import errors in the terminal

---

## Phase 2 — Config Layer

**Goal:** Settings, themes, and compiler detection all work correctly.

### Tasks

#### 2a — `hopper/config/settings.py`
- [ ] 2a.1 Copy `FORTE/forte/config/settings.py` as starting point
- [ ] 2a.2 Change config dir to `~/.config/hopper/`
- [ ] 2a.3 Update "build" section defaults:
  ```python
  "build": {
      "compiler_path": "/usr/bin/cobc",
      "compiler_flags": "-free -x",
      "output_dir": "."
  }
  ```
  > `-free` = free-format source; `-x` = create executable
- [ ] 2a.4 Change `"fortran_filter"` → `"cobol_filter": True`
- [ ] 2a.5 Set default `"tab_width": 4`
- [ ] 2a.6 Add `"source_format": "free"` setting ("free" or "fixed")
- [ ] 2a.7 Write unit test: settings load/save round-trip, defaults applied

#### 2b — `hopper/config/themes.py`
- [ ] 2b.1 Copy `FORTE/forte/config/themes.py` as starting point
- [ ] 2b.2 Rename color fields to COBOL-relevant names:
  - Keep `type_keyword` (PIC, USAGE, COMP, INDEX, etc.)
  - Keep `keyword` (PROGRAM-ID, IDENTIFICATION, PROCEDURE, MOVE, PERFORM, etc.)
  - Rename `intrinsic` → `function_name` (FUNCTION LENGTH, FUNCTION TRIM, etc.)
  - Rename `preprocessor` → `division` (IDENTIFICATION DIVISION highlight)
- [ ] 2b.3 Dark theme: type_keyword `#94e2d5`, function_name `#89dceb`, division `#cba6f7`
- [ ] 2b.4 Light theme: type_keyword `#179299`, function_name `#04a5e5`, division `#8839ef`
- [ ] 2b.5 Update `apply_theme_to_app()` QSS

#### 2c — `hopper/config/cobol_detector.py`
- [ ] 2c.1 Copy `FORTE/forte/config/fortran_detector.py` as starting point
- [ ] 2c.2 Rename: `CobolCompiler` dataclass with `name`, `path`, `version`
- [ ] 2c.3 `KNOWN_COMPILERS` in preference order:
  ```python
  ("cobc",     "GnuCOBOL",       ["--version"], <parser>),
  ("gnucobol", "GnuCOBOL (alt)", ["--version"], <parser>),
  ```
- [ ] 2c.4 Implement `detect_cobol_compilers()`, `get_default_compiler()`, `is_valid_compiler()`
- [ ] 2c.5 Write unit test: mock `shutil.which`, verify detection list

#### 2d — `hopper/config/settings_dialog.py`
- [ ] 2d.1 Copy `FORTE/forte/config/settings_dialog.py` as starting point
- [ ] 2d.2 Rename all "fortran" → "cobol", update imports
- [ ] 2d.3 **Editor tab:** font, font size, tab width (default 4), show line numbers
- [ ] 2d.4 **Build tab:** compiler dropdown (from `cobol_detector`), custom path,
           compiler flags, output directory; add **Source Format** radio:
           ◉ Free-form (`-free`)  ○ Fixed-form (`-fixed`)
- [ ] 2d.5 **Appearance tab:** dark/light theme combo

### Acceptance Criteria

- `Settings()` creates `~/.config/hopper/settings.json` with correct defaults
- `get_theme("dark")` returns a `Theme` with all COBOL fields populated
- `detect_cobol_compilers()` returns `cobc` if GnuCOBOL is installed
- Preferences dialog opens; all tabs render; settings save/load correctly

---

## Phase 3 — File Browser

**Goal:** Left-panel file tree filters COBOL files; double-click opens in editor.

### Tasks

- [ ] 3.1 Copy `FORTE/forte/browser/file_browser.py` as starting point
- [ ] 3.2 Rename filter class `CobolFileFilterProxy`
- [ ] 3.3 Accepted extensions: `.cob`, `.cbl`, `.cobol`, `.cpy` (copybooks), `.cob85`
- [ ] 3.4 Setting key: `browser → cobol_filter`
- [ ] 3.5 Context menu: New File → default name `untitled.cob`
- [ ] 3.6 Emit `file_selected(str)` signal on double-click
- [ ] 3.7 Persist `last_directory` in settings

### Acceptance Criteria

- File tree opens to last-used directory on startup
- Non-COBOL files hidden when filter is enabled
- Double-clicking a `.cob` file emits `file_selected`
- Context menu: New File, New Folder, Rename, Delete all functional

---

## Phase 4 — Editor Layer

**Goal:** Syntax-highlighted, multi-tab code editor with line numbers and find/replace.

### Tasks

#### 4a — `hopper/editor/highlighter.py`
- [ ] 4a.1 Copy `FORTE/forte/editor/highlighter.py` structure as starting point
- [ ] 4a.2 Name class `CobolHighlighter(QSyntaxHighlighter)`
- [ ] 4a.3 Use `QRegularExpression.PatternOption.CaseInsensitiveOption` for ALL word rules
  (COBOL is case-insensitive)
- [ ] 4a.4 Implement rule groups in order:
  1. **Numbers** — integers `42`, signed `+42`, decimals `3.14`
  2. **Division headers** — exact phrases at start of (trimmed) line:
     `IDENTIFICATION DIVISION`, `ENVIRONMENT DIVISION`,
     `DATA DIVISION`, `PROCEDURE DIVISION` → division color (bold)
  3. **Section/Paragraph keywords** — `WORKING-STORAGE SECTION`,
     `FILE SECTION`, `LOCAL-STORAGE SECTION`, `LINKAGE SECTION`,
     `CONFIGURATION SECTION`, `INPUT-OUTPUT SECTION` → division color
  4. **Type/Picture keywords** — `PIC`, `PICTURE`, `USAGE`, `COMP`, `COMP-1`,
     `COMP-2`, `COMP-3`, `COMP-4`, `COMP-5`, `BINARY`, `PACKED-DECIMAL`,
     `INDEX`, `POINTER`, `PROCEDURE-POINTER`, `FUNCTION-POINTER`,
     `OCCURS`, `TIMES`, `INDEXED`, `BY`, `DEPENDING`, `ON`,
     `REDEFINES`, `RENAME`, `RENAMES`, `VALUE`, `VALUES` → type color
  5. **Control/Verb keywords** — `MOVE`, `COMPUTE`, `ADD`, `SUBTRACT`,
     `MULTIPLY`, `DIVIDE`, `REMAINDER`, `GIVING`, `TO`, `FROM`, `INTO`,
     `PERFORM`, `VARYING`, `UNTIL`, `THRU`, `THROUGH`, `AFTER`,
     `IF`, `THEN`, `ELSE`, `END-IF`, `EVALUATE`, `WHEN`, `OTHER`,
     `END-EVALUATE`, `CALL`, `USING`, `RETURNING`, `BY REFERENCE`,
     `BY CONTENT`, `BY VALUE`, `STOP`, `GOBACK`, `EXIT`,
     `OPEN`, `CLOSE`, `READ`, `WRITE`, `REWRITE`, `DELETE`,
     `START`, `SEEK`, `ACCEPT`, `DISPLAY`, `UPON`, `WITH NO ADVANCING`,
     `INITIALIZE`, `SET`, `INSPECT`, `TALLYING`, `REPLACING`,
     `SORT`, `MERGE`, `RELEASE`, `RETURN`, `STRING`, `UNSTRING`,
     `PROGRAM-ID`, `AUTHOR`, `DATE-WRITTEN`,
     `ENVIRONMENT`, `IDENTIFICATION`, `DATA`, `PROCEDURE`,
     `SECTION`, `DIVISION`, `PARAGRAPH`,
     `NOT`, `AND`, `OR`, `EQUAL`, `GREATER`, `LESS`, `THAN`,
     `IS`, `ARE`, `ZERO`, `ZEROS`, `ZEROES`, `SPACE`, `SPACES`,
     `HIGH-VALUE`, `HIGH-VALUES`, `LOW-VALUE`, `LOW-VALUES`,
     `QUOTE`, `QUOTES`, `TRUE`, `FALSE`, `NULL`, `NULLS` → keyword color (bold)
  6. **FUNCTION built-ins** — word-boundary match on:
     `FUNCTION\s+[A-Z][A-Z0-9\-]*` → function_name color
     (FUNCTION LENGTH, FUNCTION TRIM, FUNCTION UPPER-CASE, etc.)
  7. **Strings** — `'[^']*'` and `"[^"]*"` → string color
  8. **Fixed-form comments** — `^\*>.*` or `^\*` in col 7 (fixed: `^.{6}\*.*`) → comment color
  9. **Free-form line comments** — `\*>.*` → comment color *(last, overrides)*
- [ ] 4a.5 Add a `set_source_format(fmt: str)` method to switch between `"free"` and `"fixed"`:
  - In `"fixed"` mode, add an additional rule coloring columns 1–6 (sequence area)
    and column 73+ (identification area) in a muted tone
  - In `"free"` mode, use standard rules above
- [ ] 4a.6 Manual test: open a `.cob` file; confirm coloring for both free and fixed formats

#### 4b — `hopper/editor/code_editor.py`
- [ ] 4b.1 Copy `FORTE/forte/editor/code_editor.py` as starting point
- [ ] 4b.2 Replace `FortranHighlighter` → `CobolHighlighter`
- [ ] 4b.3 Default tab width 4 from settings
- [ ] 4b.4 Auto-indent on Return (same leading whitespace)
- [ ] 4b.5 Line number gutter: copy verbatim
- [ ] 4b.6 Add `set_source_format(fmt)` method that forwards to highlighter

#### 4c — `hopper/editor/tab_widget.py`
- [ ] 4c.1 Copy `FORTE/forte/editor/tab_widget.py`
- [ ] 4c.2 Import `from hopper.editor.code_editor import CodeEditor`
- [ ] 4c.3 `new_file()`: default tab label `"untitled.cob"`

#### 4d — `hopper/editor/find_replace.py`
- [ ] 4d.1 Copy `FORTE/forte/editor/find_replace.py` verbatim
- [ ] 4d.2 Update imports to use `hopper.*` namespace

### Acceptance Criteria

- COBOL divisions, sections, verbs, types, functions, strings, comments all in distinct colors
- Case-insensitive: `MOVE`, `move`, `Move` all highlighted identically
- Free-form `*>` comments and fixed-form col-7 `*` comments both work
- Source format can be switched; fixed-form columns 1–6 muted
- Line numbers, multi-tab, auto-indent, Find/Replace all functional

---

## Phase 5 — Terminal Widget

**Goal:** Working PTY terminal (bash shell) in the lower panel.

### Tasks

- [ ] 5.1 Copy `FORTE/forte/terminal/pty_process.py` verbatim → `hopper/terminal/pty_process.py`
- [ ] 5.2 Copy `FORTE/forte/terminal/terminal_widget.py` → `hopper/terminal/terminal_widget.py`
- [ ] 5.3 Default command: system shell (`$SHELL` or `/bin/bash`) — same as FORTE
- [ ] 5.4 Update all imports to `hopper.*` namespace

### Acceptance Criteria

- Terminal panel shows a working shell on startup
- Keyboard input, Ctrl+C, Ctrl+D, Tab completion all work
- Terminal resizes correctly; `clear()` and `restart()` work

---

## Phase 6 — Build System

**Goal:** Save → Compile → Run workflow with output in the terminal panel.

### Design

```
hopper/build/
    __init__.py
    build_manager.py     # Orchestrates compile + run (cobc)
```

GnuCOBOL-specific: compile flag differs by source format.

### Tasks

- [ ] 6a.1 Create empty `hopper/build/__init__.py`
- [ ] 6b.1 Copy `FORTE/forte/build/build_manager.py` as starting point
- [ ] 6b.2 Adapt compile command:
  - Read `source_format` from settings → prepend `-free` or `-fixed` to flags
  - `[compiler_path, '-x'] + format_flag + flags.split() + ['-o', output_path, source_path]`
- [ ] 6b.3 `compile(source_path)` → QProcess, stream stdout/stderr to terminal
- [ ] 6b.4 `compile_and_run(source_path)` → chain compile → run on success
- [ ] 6b.5 `run(executable_path)` → write to terminal PTY
- [ ] 6b.6 Handle compile errors with coloured HOPPER message in terminal
- [ ] 6b.7 `clean()`: delete binary (cobc produces only the binary with `-x`)
- [ ] 6b.8 Unit test: mock QProcess, verify format flag in command (deferred)

### Menu items (wired in Phase 7):

| Action | Shortcut | Calls |
|--------|----------|-------|
| Compile & Run | Ctrl+R | `build_manager.compile_and_run(current_file)` |
| Compile Only | Ctrl+B | `build_manager.compile(current_file)` |
| Run Last Build | Ctrl+Shift+R | `build_manager.run(last_binary)` |
| Clean | — | Delete binary |

### Acceptance Criteria

- Ctrl+R compiles a `.cob` file and runs it; output visible in terminal
- Compiler errors appear with file/line info (cobc format: `file.cob:5: error: ...`)
- Ctrl+B compiles only; source format flag correctly applied

---

## Phase 7 — Main Window

**Goal:** Fully wired `MainWindow` with all panels, menus, toolbar, and status bar.

### Tasks

- [ ] 7.1 Copy `FORTE/forte/app.py` as starting point
- [ ] 7.2 Replace "fortran"/"FORTE" → "cobol"/"HOPPER"
- [ ] 7.3 Update all imports to `hopper.*` namespace
- [ ] 7.4 Instantiate `BuildManager`; connect signals to status bar and terminal
- [ ] 7.5 **Layout:** same as FORTE — horizontal + vertical splitters,
  default sizes `[200,800]`/`[500,268]`
- [ ] 7.6 **File Menu:** New/Open/Save/Save As/Exit
  - Open filter: `"COBOL Files (*.cob *.cbl *.cobol *.cpy);;All Files (*)"`
  - Save As filter: `"COBOL Source (*.cob);;COBOL Copybook (*.cpy);;All Files (*)"`
- [ ] 7.7 **Edit Menu:** Find/Replace (Ctrl+F), Preferences
- [ ] 7.8 **View Menu:** Show File Browser (checkable), Show Terminal (checkable)
- [ ] 7.9 **Build Menu:** Compile & Run (Ctrl+R), Compile Only (Ctrl+B),
  Run Last Build (Ctrl+Shift+R), Clean Build Output, Clear Terminal Output,
  Restart Terminal, Interrupt (Ctrl+Shift+C)
- [ ] 7.10 **Help Menu:** About HOPPER
- [ ] 7.11 **Toolbar:** New, Open, Save | Compile & Run, Compile Only
- [ ] 7.12 **Status bar:** "Ready" + cursor position (Ln/Col)
  > For fixed-form COBOL, also show which COBOL column area the cursor is in:
  > Sequence (1–6), Indicator (7), Area A (8–11), Area B (12–72), Ident (73+)
- [ ] 7.13 Wire `file_browser.file_selected` → `open_file_from_browser()`
- [ ] 7.14 Wire `editor_tabs.currentChanged` → `update_cursor_position()`
- [ ] 7.15 `apply_theme()` on startup and after preferences save
- [ ] 7.16 `closeEvent()`: save geometry, splitter state, terminate PTY
- [ ] 7.17 **About dialog:** "HOPPER — COBOL Teaching Environment, honoring Grace Hopper"

### Acceptance Criteria

- All menus present and functional; full file→build→terminal workflow works
- Window geometry / splitter positions persist across restarts
- Theme applies correctly; About dialog shows correct information
- Status bar column-area indicator works in fixed-form mode

---

## Phase 8 — Examples & Polish

**Goal:** Include beginner-friendly COBOL example programs; fix rough edges.

### Example Programs (`examples/`)
All examples use **free-format** COBOL for readability.
- [ ] 8a.1 `hello_world.cob` — IDENTIFICATION + PROCEDURE with DISPLAY 'Hello, World!'
- [ ] 8a.2 `variables.cob` — WORKING-STORAGE: PIC 9, PIC X, PIC 9(6)V99, signed, Level 01/05
- [ ] 8a.3 `arithmetic.cob` — ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE
- [ ] 8a.4 `conditionals.cob` — IF/THEN/ELSE/END-IF, EVALUATE/WHEN/END-EVALUATE
- [ ] 8a.5 `loops.cob` — PERFORM VARYING, PERFORM UNTIL, PERFORM N TIMES
- [ ] 8a.6 `strings.cob` — STRING, UNSTRING, INSPECT TALLYING/REPLACING, FUNCTION TRIM
- [ ] 8a.7 `tables.cob` — OCCURS, INDEXED BY, SET, SEARCH, SEARCH ALL
- [ ] 8a.8 `fileio.cob` — SELECT/FD, OPEN/CLOSE/READ/WRITE sequential file

### Polish Tasks
- [ ] 8b.1 Create proper `images/hopper_icon.png` (64×64 and 256×256)
- [ ] 8b.2 Ensure no hardcoded theme colors in `code_editor.py`
- [ ] 8b.3 Add `--version` CLI flag to `main.py`
- [ ] 8b.4 cobc error line parsing — highlight error lines in editor (stretch goal)
- [ ] 8b.5 Status bar build indicator: "Compiling…" / "Build succeeded" / "Build failed"
- [ ] 8b.6 Unsaved file check before compile: auto-save or prompt
- [ ] 8b.7 Fixed-form column ruler — optional visual ruler at col 7, 11, 72, 80

### Acceptance Criteria

- All 8 example files compile cleanly with `cobc -free -x -Wall`
- No hardcoded colors; build status visible in status bar

---

## Phase 9 — Packaging

**Goal:** Distributable standalone binary via PyInstaller.

### Tasks

- [ ] 9.1 Create `HOPPER.spec` (reference: `FORTE/FORTE.spec`)
- [ ] 9.2 Include `images/` and `examples/` in datas
- [ ] 9.3 Test `pyinstaller HOPPER.spec` produces a working binary
- [ ] 9.4 Create `build.py` helper script (reference: `FORTE/build.py`)
- [ ] 9.5 Test built binary: window opens, --version works, assets in _internal/
- [ ] 9.6 Bare-machine test (deferred)

### Acceptance Criteria

- `python3 build.py` produces `dist/HOPPER` executable
- Binary runs standalone; all assets accessible

---

## COBOL-Specific Notes

### GnuCOBOL (cobc) Invocation
```bash
# Free-format, executable output
cobc -free -x -o hello hello.cob

# Fixed-format, executable output
cobc -fixed -x -o hello hello.cob

# Compile to object only (no -x)
cobc -free -o hello.o hello.cob
```

### GnuCOBOL Error Format
```
hello.cob:5: error: 'FOO' undefined
hello.cob:10: warning: missing STOP RUN
```

### COBOL Source Formats

**Free-Form** (modern, recommended for teaching):
- No column restrictions
- Comments: `*>` anywhere on a line
- Continuation: not needed

**Fixed-Form** (traditional ANSI):
- Columns 1–6: Sequence number area (ignored)
- Column 7: Indicator (` ` = code, `*` = comment, `-` = continuation, `/` = page eject)
- Columns 8–11: Area A (divisions, sections, paragraph names, level 01/77)
- Columns 12–72: Area B (statements)
- Columns 73–80: Identification area (ignored)

### Division Structure
Every COBOL program has 4 divisions (some optional):
```
IDENTIFICATION DIVISION.
  PROGRAM-ID. HelloWorld.
ENVIRONMENT DIVISION.
  (optional — file definitions, machine config)
DATA DIVISION.
  WORKING-STORAGE SECTION.
    01 WS-NAME PIC X(30).
PROCEDURE DIVISION.
  DISPLAY 'Hello, World!'
  STOP RUN.
```

---

## Reference Files (by phase)

| Phase | Primary Reference | Notes |
|-------|------------------|-------|
| 1 | `FORTE/forte/main.py`, `setup.sh`, `run.sh` | Rename forte→hopper |
| 2a | `FORTE/forte/config/settings.py` | cobol_filter, tab_width=4, cobc path, source_format |
| 2b | `FORTE/forte/config/themes.py` | Rename fields: function_name, division |
| 2c | `FORTE/forte/config/fortran_detector.py` | Adapt for cobc |
| 2d | `FORTE/forte/config/settings_dialog.py` | Add source format radio; rename fortran→cobol |
| 3 | `FORTE/forte/browser/file_browser.py` | .cob/.cbl/.cpy extensions |
| 4a | `FORTE/forte/editor/highlighter.py` | Structure only; all rules new (COBOL divisions!) |
| 4b–4d | `FORTE/forte/editor/` | Copy; swap import namespaces |
| 5 | `FORTE/forte/terminal/` | Copy verbatim |
| 6 | `FORTE/forte/build/build_manager.py` | Adapt for cobc; add source_format flag |
| 7 | `FORTE/forte/app.py` | Adapt menus/filters; add column indicator |
| 9 | `FORTE/FORTE.spec`, `FORTE/build.py` | Adapt |

---

## Progress Log

| Date | Phase | Notes |
|------|-------|-------|
| 2026-02-23 | 0 | Implementation plan created (adapted from FORTE) |

---

*HOPPER — COBOL Teaching Environment*  
*Named in honor of Rear Admiral Grace Murray Hopper (1906–1992)*  
*(c) Fragillidae Software — Chuck Finch*
