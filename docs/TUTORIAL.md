# COBOL Tutorial — HOPPER IDE

This tutorial teaches COBOL programming using the HOPPER IDE. By the end you will understand the four COBOL divisions, how to define and manipulate data, and how to write programs that compile and run with GnuCOBOL.

---

## Getting Started

Open HOPPER (`./run.sh`). Press **`Ctrl+N`** to create a new file, or open a bundled example from `examples/`. Press **`Ctrl+R`** to compile and run. All examples in this tutorial use **free-format** COBOL (the default in HOPPER).

---

## Lesson 1 — Hello, World!

Create a new file named `hello.cob`:

```cobol
      *> hello.cob — free-format COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Hello.

       PROCEDURE DIVISION.
           DISPLAY "Hello, World!"
           STOP RUN.
```

Press `Ctrl+R`. You should see:
```
Hello, World!
```

**What you learned:**
- Every COBOL program has an `IDENTIFICATION DIVISION` with a `PROGRAM-ID`
- `PROCEDURE DIVISION` is where executable statements go
- `DISPLAY` prints text to the screen
- Every program ends with `STOP RUN.`
- `*>` starts a line comment (free-form)

---

## Lesson 2 — The Four Divisions

COBOL programs are organised into up to four divisions, always in this order:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyProgram.
       *> Who wrote this program and what it does

       ENVIRONMENT DIVISION.
       *> Optional: file definitions, machine configuration

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> Declare all your variables here

       PROCEDURE DIVISION.
       *> All executable statements go here
           STOP RUN.
```

Most beginner programs only need `IDENTIFICATION` and `PROCEDURE`.

---

## Lesson 3 — Variables (Working-Storage)

All variables are declared in the `WORKING-STORAGE SECTION` of the `DATA DIVISION`. COBOL uses **level numbers** and **PIC clauses** to define data:

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-NAME        PIC X(20)     VALUE "Ada Lovelace".
           01 WS-AGE         PIC 9(3)      VALUE 0.
           01 WS-SALARY      PIC 9(7)V99   VALUE 0.
           01 WS-SIGNED      PIC S9(5)     VALUE -100.
```

**PIC Symbols:**

| Symbol | Meaning                  | Example                 |
| ------ | ------------------------ | ----------------------- |
| `X`    | Any character            | `PIC X(10)` — 10 chars  |
| `9`    | Numeric digit            | `PIC 9(4)` — up to 9999 |
| `V`    | Implied decimal point    | `PIC 9(4)V99` — 9999.99 |
| `S`    | Signed (can be negative) | `PIC S9(5)`             |

**Level numbers:**
- `01` — top-level item
- `05`, `10`, etc. — subordinate items (group records)
- `77` — standalone item (no sub-fields)
- `88` — condition name (Boolean alias)

```cobol
       01 WS-PERSON.
           05 WS-FIRST-NAME  PIC X(15).
           05 WS-LAST-NAME   PIC X(15).
```

---

## Lesson 4 — MOVE and DISPLAY

```cobol
       PROCEDURE DIVISION.
           MOVE "Grace"  TO WS-FIRST-NAME
           MOVE "Hopper" TO WS-LAST-NAME
           MOVE 85       TO WS-AGE

           DISPLAY "Name: " WS-FIRST-NAME " " WS-LAST-NAME
           DISPLAY "Age:  " WS-AGE
           STOP RUN.
```

Key points:
- `MOVE source TO destination` — assign a value
- `DISPLAY` items — concatenate them on one line
- COBOL is case-insensitive: `MOVE`, `move`, and `Move` are identical

---

## Lesson 5 — Arithmetic

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-A      PIC 9(4) VALUE 100.
           01 WS-B      PIC 9(4) VALUE 25.
           01 WS-RESULT PIC 9(8) VALUE 0.

       PROCEDURE DIVISION.
           ADD      WS-A TO WS-B GIVING WS-RESULT
           DISPLAY "100 + 25 = " WS-RESULT

           SUBTRACT WS-B FROM WS-A GIVING WS-RESULT
           DISPLAY "100 - 25 = " WS-RESULT

           MULTIPLY WS-A BY WS-B GIVING WS-RESULT
           DISPLAY "100 * 25 = " WS-RESULT

           DIVIDE   WS-A BY WS-B GIVING WS-RESULT
           DISPLAY "100 / 25 = " WS-RESULT

           COMPUTE WS-RESULT = (WS-A ** 2) + WS-B
           DISPLAY "100^2 + 25 = " WS-RESULT

           STOP RUN.
```

---

## Lesson 6 — Conditionals

### IF / THEN / ELSE
```cobol
           IF WS-AGE >= 65
               DISPLAY "Senior"
           ELSE IF WS-AGE >= 18
               DISPLAY "Adult"
           ELSE
               DISPLAY "Minor"
           END-IF
```

### EVALUATE (switch/case)
```cobol
           EVALUATE WS-GRADE
               WHEN "A" DISPLAY "Excellent"
               WHEN "B" DISPLAY "Good"
               WHEN "C" DISPLAY "Pass"
               WHEN OTHER DISPLAY "Fail"
           END-EVALUATE
```

### Condition Names (88-level)
```cobol
       01 WS-STATUS   PIC X VALUE "A".
           88 IS-ACTIVE   VALUE "A".
           88 IS-INACTIVE VALUE "I".

       PROCEDURE DIVISION.
           IF IS-ACTIVE
               DISPLAY "Account is active"
           END-IF
```

---

## Lesson 7 — Loops (PERFORM)

### PERFORM N TIMES
```cobol
           PERFORM 5 TIMES
               DISPLAY "Hello!"
           END-PERFORM
```

### PERFORM VARYING
```cobol
       01 WS-I PIC 9(3).
       ...
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               DISPLAY "i = " WS-I
           END-PERFORM
```

### PERFORM UNTIL (while-loop)
```cobol
           MOVE 0 TO WS-TOTAL
           PERFORM UNTIL WS-TOTAL >= 100
               ADD 10 TO WS-TOTAL
           END-PERFORM
```

### Named Paragraph (subroutine-style)
```cobol
       PROCEDURE DIVISION.
           PERFORM PRINT-HEADER
           STOP RUN.

       PRINT-HEADER.
           DISPLAY "=== REPORT ==="
```

---

## Lesson 8 — Tables (Arrays)

```cobol
       01 WS-SCORES.
           05 WS-SCORE PIC 9(3) OCCURS 5 TIMES.
       01 WS-I         PIC 9.

       PROCEDURE DIVISION.
           MOVE 90 TO WS-SCORE(1)
           MOVE 85 TO WS-SCORE(2)
           MOVE 78 TO WS-SCORE(3)
           MOVE 92 TO WS-SCORE(4)
           MOVE 88 TO WS-SCORE(5)

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               DISPLAY "Score " WS-I ": " WS-SCORE(WS-I)
           END-PERFORM
           STOP RUN.
```

---

## Lesson 9 — Sequential File I/O

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MYFILE ASSIGN TO "data.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MYFILE.
       01 FILE-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-EOF PIC X VALUE "N".
           88 END-OF-FILE VALUE "Y".

       PROCEDURE DIVISION.
           OPEN OUTPUT MYFILE
           MOVE "Hello from HOPPER!" TO FILE-RECORD
           WRITE FILE-RECORD
           CLOSE MYFILE

           OPEN INPUT MYFILE
           PERFORM UNTIL END-OF-FILE
               READ MYFILE INTO FILE-RECORD
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END DISPLAY FUNCTION TRIM(FILE-RECORD)
               END-READ
           END-PERFORM
           CLOSE MYFILE
           STOP RUN.
```

---

## Next Steps

- Study all 8 examples in the `examples/` folder.
- Read the [COBOL Language Reference](LANGUAGE-REFERENCE.md) for a complete keyword listing.
- Try building a simple report generator that reads data and prints formatted output.
