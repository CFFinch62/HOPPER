# COBOL Language Reference — HOPPER IDE

A concise reference for GnuCOBOL as used in the HOPPER IDE. All examples use **free-format** COBOL.

---

## Program Structure

Every COBOL program has up to four divisions, in this fixed order:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProgramName.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
         FILE-CONTROL. ...

       DATA DIVISION.
       FILE SECTION.         FD ...
       WORKING-STORAGE SECTION.  01 ...
       LOCAL-STORAGE SECTION.    01 ...
       LINKAGE SECTION.          01 ...

       PROCEDURE DIVISION.
           STOP RUN.
```

---

## Level Numbers

| Level     | Purpose                                     |
| --------- | ------------------------------------------- |
| `01`      | Top-level group or elementary item          |
| `02`–`49` | Sub-items within a group                    |
| `66`      | RENAMES clause                              |
| `77`      | Independent elementary item (no sub-fields) |
| `88`      | Condition name (Boolean alias)              |

---

## PIC Clauses

| Symbol              | Meaning                                         |
| ------------------- | ----------------------------------------------- |
| `X`                 | Any character (`PIC X(20)` — 20 chars)          |
| `A`                 | Alphabetic character only                       |
| `9`                 | Numeric digit (`PIC 9(4)` — 4-digit number)     |
| `S`                 | Signed (prefix, `PIC S9(5)`)                    |
| `V`                 | Implied decimal point (`PIC 9(5)V99` = 9999.99) |
| `P`                 | Implied scaling (positions beyond decimal)      |
| `Z`                 | Numeric, replace leading zeroes with spaces     |
| `,` `.` `+` `-` `$` | Edit symbols for display formatting             |

### Examples
```cobol
01 WS-NAME        PIC X(30).          *> 30-char text
01 WS-COUNT       PIC 9(4).           *> up to 9999
01 WS-AMOUNT      PIC 9(7)V99.        *> 7-digit dollars.cents
01 WS-SIGNED      PIC S9(5) COMP.     *> signed binary
01 WS-DISPLAY     PIC ZZZ,ZZ9.99.     *> formatted display
```

---

## VALUE Clause

```cobol
01 WS-ZERO   PIC 9(5) VALUE ZERO.
01 WS-TEXT   PIC X(10) VALUE "Hello".
01 WS-FLAG   PIC X VALUE "Y".
    88 IS-YES    VALUE "Y".
    88 IS-NO     VALUE "N".
```

---

## USAGE Clause

| Clause                       | Storage format         |
| ---------------------------- | ---------------------- |
| `DISPLAY` (default)          | Character string       |
| `BINARY` / `COMP` / `COMP-4` | Native binary integer  |
| `COMP-1`                     | Single-precision float |
| `COMP-2`                     | Double-precision float |
| `PACKED-DECIMAL` / `COMP-3`  | BCD compressed         |
| `INDEX`                      | Table index register   |

---

## Data Manipulation

### MOVE
```cobol
MOVE "Hello"        TO WS-NAME
MOVE 0              TO WS-TOTAL
MOVE SPACES         TO WS-TEXT         *> fill with spaces
MOVE ZEROS          TO WS-COUNT        *> fill with zeros
MOVE WS-A           TO WS-B WS-C      *> move to multiple destinations
```

### INITIALIZE
```cobol
INITIALIZE WS-RECORD                   *> numerics→0, alphanumeric→spaces
INITIALIZE WS-RECORD REPLACING NUMERIC BY 1
```

### MOVE CORRESPONDING
```cobol
MOVE CORRESPONDING GROUP-A TO GROUP-B  *> moves matching field names
```

---

## Arithmetic Statements

```cobol
ADD    5        TO   WS-TOTAL
ADD    WS-A     TO   WS-B  GIVING  WS-C
SUBTRACT WS-TAX FROM WS-GROSS GIVING WS-NET
MULTIPLY WS-RATE BY WS-HOURS GIVING WS-PAY
DIVIDE  WS-TOTAL BY 4  GIVING WS-AVG  REMAINDER WS-REM
COMPUTE WS-C = WS-A ** 2 + WS-B / 2   *> like a math expression
```

**ROUNDED** modifier: `ADD WS-A TO WS-B GIVING WS-C ROUNDED`

---

## Conditional Expressions

```cobol
*> Simple
IF WS-AMOUNT > 1000  DISPLAY "High"  END-IF

*> Multi-branch
IF WS-SCORE >= 90
    DISPLAY "A"
ELSE IF WS-SCORE >= 80
    DISPLAY "B"
ELSE
    DISPLAY "C"
END-IF

*> EVALUATE (switch)
EVALUATE WS-CODE
    WHEN 1          DISPLAY "One"
    WHEN 2 THRU 5   DISPLAY "Two to Five"
    WHEN OTHER      DISPLAY "Other"
END-EVALUATE

*> Condition names (88-level)
IF IS-YES
    PERFORM PROCESS-ACTIVE
END-IF
```

**Relation conditions:** `=` `>` `<` `>=` `<=` `NOT =` `NOT >`
**Sign conditions:** `POSITIVE` `NEGATIVE` `ZERO`
**Class conditions:** `NUMERIC` `ALPHABETIC` `ALPHABETIC-LOWER` `ALPHABETIC-UPPER`

---

## PERFORM (Loops)

```cobol
*> Named paragraph
PERFORM PRINT-HEADER

*> Inline N times
PERFORM 5 TIMES
    DISPLAY "Hello!"
END-PERFORM

*> Inline VARYING
PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
    DISPLAY WS-I
END-PERFORM

*> Two-level nested varying
PERFORM VARYING WS-ROW FROM 1 BY 1 UNTIL WS-ROW > 3
    AFTER WS-COL FROM 1 BY 1 UNTIL WS-COL > 3
        DISPLAY WS-ROW " " WS-COL
END-PERFORM

*> Paragraph range
PERFORM INIT-PARA THRU CLEANUP-PARA
```

---

## DISPLAY and ACCEPT

```cobol
DISPLAY "Enter name: " WITH NO ADVANCING
ACCEPT WS-NAME
DISPLAY "Hello, " WS-NAME

*> Display to stderr
DISPLAY "Error" UPON SYSERR
```

---

## String Manipulation

```cobol
*> STRING (concatenation)
STRING WS-FIRST DELIMITED BY SPACE
       " "      DELIMITED BY SIZE
       WS-LAST  DELIMITED BY SPACE
       INTO WS-FULL

*> UNSTRING (split)
UNSTRING WS-FULL DELIMITED BY ","
    INTO WS-PART1 WS-PART2

*> INSPECT (count/replace characters)
INSPECT WS-TEXT TALLYING WS-COUNT FOR ALL SPACES
INSPECT WS-TEXT REPLACING ALL "X" BY "Y"
```

### Intrinsic Functions
```cobol
FUNCTION TRIM(WS-TEXT)              *> remove leading/trailing spaces
FUNCTION UPPER-CASE(WS-TEXT)        *> convert to upper
FUNCTION LOWER-CASE(WS-TEXT)        *> convert to lower
FUNCTION LENGTH(WS-TEXT)            *> string length
FUNCTION REVERSE(WS-TEXT)           *> reverse characters
FUNCTION CONCATENATE(A B C)         *> concatenate items
FUNCTION SUBSTITUTE(str old new)    *> replace substring
```

---

## Tables (Arrays)

```cobol
01 WS-TABLE.
    05 WS-ITEM  PIC 9(4) OCCURS 10 TIMES.
01 WS-FLEX-TABLE.
    05 WS-ENTRY PIC X(20) OCCURS 1 TO 50 TIMES
                DEPENDING ON WS-COUNT.

*> Access
MOVE 42 TO WS-ITEM(5)
DISPLAY WS-ITEM(WS-I)
```

### SEARCH (linear)
```cobol
01 WS-TABLE.
    05 WS-ENTRY PIC X(10) OCCURS 10 TIMES INDEXED BY WS-IX.

SET WS-IX TO 1
SEARCH WS-ENTRY
    AT END DISPLAY "Not found"
    WHEN WS-ENTRY(WS-IX) = "TARGET"
        DISPLAY "Found at " WS-IX
END-SEARCH
```

---

## File I/O

```cobol
*> ENVIRONMENT DIVISION
FILE-CONTROL.
    SELECT MYFILE ASSIGN TO "data.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE  IS SEQUENTIAL
        FILE STATUS  IS WS-FILE-STATUS.

*> DATA DIVISION
FD MYFILE.
01 MYFILE-RECORD PIC X(80).

*> PROCEDURE DIVISION
OPEN  INPUT  MYFILE
OPEN  OUTPUT MYFILE
OPEN  EXTEND MYFILE     *> append
OPEN  I-O    MYFILE

READ MYFILE INTO WS-BUFFER
    AT END MOVE "Y" TO WS-EOF
END-READ

WRITE MYFILE-RECORD FROM WS-BUFFER
REWRITE MYFILE-RECORD
DELETE MYFILE RECORD

CLOSE MYFILE
```

---

## CALL (Subprogram)

```cobol
CALL "SUBPROG"  USING BY REFERENCE WS-DATA
                      BY CONTENT  WS-CONST
                      BY VALUE    42
                RETURNING WS-RESULT
```

---

## SORT and MERGE

```cobol
SORT SORT-FILE ON ASCENDING KEY SORT-KEY
    INPUT PROCEDURE  IS FILL-SORT
    OUTPUT PROCEDURE IS WRITE-RESULT
```

---

## Special Registers and Figurative Constants

| Name                         | Value                      |
| ---------------------------- | -------------------------- |
| `SPACES` / `SPACE`           | All space characters       |
| `ZEROS` / `ZEROES` / `ZERO`  | Numeric or character zeros |
| `HIGH-VALUES` / `HIGH-VALUE` | Highest possible character |
| `LOW-VALUES` / `LOW-VALUE`   | Lowest possible character  |
| `QUOTES` / `QUOTE`           | Single quote character     |
| `NULL` / `NULLS`             | Null pointer (LINKAGE)     |
| `TRUE` / `FALSE`             | Boolean condition names    |

---

## GnuCOBOL Compiler Reference

```bash
cobc -free -x -Wall -o hello hello.cob    # free-format executable
cobc -fixed -x -Wall -o hello hello.cob   # fixed-format executable
cobc -free -c hello.cob                   # compile to object only
cobc --version                            # show version
```

| Flag                      | Meaning               |
| ------------------------- | --------------------- |
| `-free`                   | Free-format source    |
| `-fixed` / `-std=cobol85` | Fixed-format (ANSI)   |
| `-x`                      | Produce executable    |
| `-Wall`                   | Enable all warnings   |
| `-debug`                  | Add runtime debugging |
| `-O` / `-O2`              | Optimise              |
| `-v`                      | Verbose               |

---

*GnuCOBOL documentation: [gnucobol.sourceforge.io](https://gnucobol.sourceforge.io/)*
