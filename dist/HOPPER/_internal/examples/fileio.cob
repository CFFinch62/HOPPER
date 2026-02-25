      *> fileio.cob - HOPPER Example 8: Sequential File I/O
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FileIO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "hopper_test.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INFILE  ASSIGN TO "hopper_test.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD OUTFILE.
           01 OUT-RECORD PIC X(80).

           FD INFILE.
           01 IN-RECORD  PIC X(80).

       WORKING-STORAGE SECTION.
           01 WS-EOF     PIC X VALUE "N".
               88 END-OF-FILE VALUE "Y".
           01 WS-COUNT   PIC 9(3) VALUE ZERO.

       PROCEDURE DIVISION.
           *> ── Write phase ──
           DISPLAY "Writing to hopper_test.txt..."
           OPEN OUTPUT OUTFILE
           MOVE "Hello from HOPPER!" TO OUT-RECORD
           WRITE OUT-RECORD
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT > 5
               STRING "Line " WS-COUNT ": value = "
                      WS-COUNT DELIMITED BY SIZE
                      INTO OUT-RECORD
               WRITE OUT-RECORD
           END-PERFORM
           CLOSE OUTFILE
           DISPLAY "Write complete."

           *> ── Read phase ──
           DISPLAY "Reading hopper_test.txt back:"
           OPEN INPUT INFILE
           MOVE "N" TO WS-EOF
           PERFORM UNTIL END-OF-FILE
               READ INFILE INTO IN-RECORD
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END DISPLAY "  > " FUNCTION TRIM(IN-RECORD)
               END-READ
           END-PERFORM
           CLOSE INFILE
           DISPLAY "Read complete."

           STOP RUN.
