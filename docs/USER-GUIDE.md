# HOPPER User Guide

Welcome to **HOPPER** (COBOL Teaching Environment), a beginner-friendly IDE named in honor of Rear Admiral Grace Murray Hopper, the pioneering computer scientist who helped develop COBOL. This guide covers installation, the interface, and all major features.

---

## 1. Installation and Running

### Prerequisites

- **Python 3.10+** — check with `python3 --version`
- **GnuCOBOL (cobc)** — recommended compiler:

  | Platform         | Command                     |
  | ---------------- | --------------------------- |
  | Debian / Ubuntu  | `sudo apt install gnucobol` |
  | Fedora / RHEL    | `sudo dnf install gnucobol` |
  | macOS (Homebrew) | `brew install gnu-cobol`    |

### Starting HOPPER

1. Open a terminal in the HOPPER project directory.
2. Run the launch script:
   ```bash
   ./run.sh
   ```
   The script automatically creates a Python virtual environment, installs all dependencies, and starts the application.

### Command-Line Options

```bash
python3 -m hopper.main           # launch normally
python3 -m hopper.main --version # print version and exit
```

---

## 2. Interface Overview

The HOPPER window is divided into three panels:

1. **File Browser (Left)** — navigate directories; filtered to COBOL source files by default.
2. **Code Editor (Center/Top)** — multi-tab editor with COBOL syntax highlighting and line numbers.
3. **Terminal (Bottom)** — a full interactive shell for building and running programs.

### Menu Bar

| Menu      | Key actions                                                                                           |
| --------- | ----------------------------------------------------------------------------------------------------- |
| **File**  | New · Open · Save · Save As · Exit                                                                    |
| **Edit**  | Find/Replace · Preferences                                                                            |
| **View**  | Show/Hide File Browser · Show/Hide Terminal                                                           |
| **Build** | Compile & Run · Compile Only · Run Last Build · Clean · Clear Terminal · Restart Terminal · Interrupt |
| **Help**  | About HOPPER                                                                                          |

---

## 3. Code Editor

- **Syntax Highlighting** — COBOL is color-coded by category:
  - **Division headers** (bold pink) — `IDENTIFICATION DIVISION`, `PROCEDURE DIVISION`, etc.
  - **Section keywords** (pink) — `WORKING-STORAGE SECTION`, `FILE SECTION`, etc.
  - **Type / Picture keywords** (teal) — `PIC`, `USAGE`, `COMP`, `OCCURS`, `REDEFINES`, etc.
  - **Verb keywords** (bold purple) — `MOVE`, `PERFORM`, `IF`, `EVALUATE`, `DISPLAY`, etc.
  - **FUNCTION built-ins** (cyan) — `FUNCTION TRIM`, `FUNCTION UPPER-CASE`, etc.
  - **Strings** (green) — quoted text in `'single'` or `"double"` quotes.
  - **Comments** (grey italic) — `*>` anywhere on a line (free-form), or column-7 `*` (fixed-form).
  - All keyword matching is **case-insensitive** (as COBOL itself is).
- **Line Numbers** — displayed in the left gutter.
- **Multi-Tab Editing** — open multiple `.cob` files; tabs show `*` for unsaved changes.
- **Auto-Indent** — pressing Enter preserves the current indentation level.
- **Find & Replace** — open with `Ctrl+F`.

---

## 4. Build System

HOPPER compiles COBOL programs using **cobc** (GnuCOBOL compiler).

### Source Format

COBOL has two source format modes, selectable in **Preferences → Build → Source Format**:

| Mode                    | Flag     | When to use                                                  |
| ----------------------- | -------- | ------------------------------------------------------------ |
| **Free-form** (default) | `-free`  | Modern COBOL — no column restrictions                        |
| **Fixed-form**          | `-fixed` | Traditional ANSI COBOL — columns 1–80 have specific meanings |

The status bar always shows the current mode (`FREE` or `FIXED`). In **Fixed-form** mode, it also shows which COBOL column area the cursor is in (`Seq`, `Ind`, `Area A`, `Area B`, `Ident`).

### Workflow

1. Open or create a `.cob` file in the editor.
2. Press **`Ctrl+R`** (Compile & Run) or choose **Build → Compile & Run**.
   - HOPPER auto-saves the file before compiling.
   - The `cobc` command and its output appear in the terminal.
   - On success, the binary runs immediately in the terminal.
3. Use **`Ctrl+B`** (Compile Only) to check for errors without running.
4. Use **Build → Run Last Build** to re-run the most recently compiled binary.
5. Use **Build → Clean** to delete the compiled binary.

### Compiler Command

```bash
cobc -free -x [-Wall] -o <output> <source.cob>
# or
cobc -fixed -x [-Wall] -o <output> <source.cob>
```

### Compiler Error Format

GnuCOBOL reports errors as:
```
source.cob:5: error: 'FOO' undefined
source.cob:10: warning: missing STOP RUN
```

---

## 5. Integrated Terminal

The bottom panel is a full PTY shell backed by your system's `$SHELL`.

- **Interactive** — type any shell command and press Enter.
- **Ctrl+C** — if text is selected, copies it to clipboard. Otherwise sends SIGINT to the running process.
- **Ctrl+D** — sends EOF.
- **Mouse selection** — click and drag to select; right-click for Copy context menu.
- **Restart Terminal** — **Build → Restart Terminal** kills and relaunches the shell.
- **Clear Terminal** — **Build → Clear Terminal** wipes the visible output.

---

## 6. File Browser

- **Navigation** — double-click folders to navigate into them.
- **Open files** — double-click a COBOL file to open it in a new editor tab.
- **Filtered view** — shows `.cob`, `.cbl`, `.cobol`, `.cpy`, `.cob85` files by default.
- **Context menu** — right-click to create a new file (defaults to `untitled.cob`), new folder, rename, or delete.

---

## 7. Preferences

Open with **Edit → Preferences**.

### Editor Tab
| Setting           | Description                       |
| ----------------- | --------------------------------- |
| Font family       | Monospace font used in the editor |
| Font size         | Point size (default: 14)          |
| Tab width         | Spaces per indent (default: 4)    |
| Show line numbers | Toggle the gutter                 |

### Build Tab
| Setting          | Description                                             |
| ---------------- | ------------------------------------------------------- |
| Compiler         | Auto-detected GnuCOBOL compilers; custom path supported |
| Source Format    | **Free-form** (`-free`) or **Fixed-form** (`-fixed`)    |
| Compiler Flags   | Additional cobc flags (e.g., `-Wall -debug`)            |
| Output directory | Where to place compiled binaries (`.` = same as source) |

### Appearance Tab
| Setting | Description                     |
| ------- | ------------------------------- |
| Theme   | **Dark** (default) or **Light** |

---

## 8. Fixed-Form COBOL Column Areas

When using Fixed-form source format, COBOL columns have specific roles:

| Columns | Area           | Purpose                                                |
| ------- | -------------- | ------------------------------------------------------ |
| 1–6     | Sequence       | Optional line sequence numbers (ignored by compiler)   |
| 7       | Indicator      | `*` = comment line, `-` = continuation, space = code   |
| 8–11    | Area A         | Division/section headers, paragraph names, level 01/77 |
| 12–72   | Area B         | Statements, clauses, and most other code               |
| 73–80   | Identification | Optional label (ignored by compiler)                   |

The status bar shows the cursor's current column area when Fixed-form mode is active.

---

## 9. Example Programs

HOPPER ships with eight ready-to-run programs in the `examples/` folder (all in free-format COBOL). Open any one in the file browser, then press `Ctrl+R`.

| File               | Topic                                                        |
| ------------------ | ------------------------------------------------------------ |
| `hello_world.cob`  | `IDENTIFICATION DIVISION`, `DISPLAY`                         |
| `variables.cob`    | `PIC` clauses, level numbers, 88-level condition names       |
| `arithmetic.cob`   | `ADD`, `SUBTRACT`, `MULTIPLY`, `DIVIDE`, `COMPUTE`           |
| `conditionals.cob` | `IF/THEN/ELSE/END-IF`, `EVALUATE/WHEN/END-EVALUATE`          |
| `loops.cob`        | `PERFORM N TIMES`, `PERFORM VARYING`, `PERFORM UNTIL`        |
| `strings.cob`      | `STRING`, `UNSTRING`, `FUNCTION UPPER-CASE`, `FUNCTION TRIM` |
| `tables.cob`       | `OCCURS`, `INDEXED BY`, `PERFORM VARYING` with index         |
| `fileio.cob`       | `SELECT/FD`, `OPEN/CLOSE/READ/WRITE` sequential file I/O     |

---

## 10. Keyboard Shortcuts

| Action            | Shortcut                 |
| ----------------- | ------------------------ |
| New file          | `Ctrl+N`                 |
| Open file         | `Ctrl+O`                 |
| Save              | `Ctrl+S`                 |
| Save As           | `Ctrl+Shift+S`           |
| Find / Replace    | `Ctrl+F`                 |
| Compile & Run     | `Ctrl+R`                 |
| Compile Only      | `Ctrl+B`                 |
| Run Last Build    | `Ctrl+Shift+R`           |
| Interrupt process | `Ctrl+C` (no selection)  |
| Copy selection    | `Ctrl+C` (text selected) |

---

## 11. Troubleshooting

**"No compilers found" in Preferences**
Ensure `cobc` is installed and on your `PATH`. Run `which cobc` to verify. Click **Refresh** in Build preferences after installing.

**Build fails: "cobc: command not found"**
Install GnuCOBOL: `sudo apt install gnucobol` (Debian/Ubuntu) or `brew install gnu-cobol` (macOS).

**Compile error about column position**
If using Fixed-form mode, your code must respect the column rules. Division headers must start in Area A (column 8). Most statements belong in Area B (column 12+). Switch to Free-form for modern COBOL without column restrictions.

**Window layout not restored**
Delete `~/.config/hopper/settings.json` — HOPPER will recreate it with defaults on the next launch.

---

*HOPPER — COBOL Teaching Environment*
*Named in honor of Rear Admiral Grace Murray Hopper (1906–1992)*
*© 2026 Chuck Finch — Fragillidae Software*
