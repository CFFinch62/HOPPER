      *> arithmetic.cob - HOPPER Example 3: Arithmetic Operations
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Arithmetic.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-A      PIC 9(4)   VALUE 100.
           01 WS-B      PIC 9(4)   VALUE 25.
           01 WS-RESULT PIC 9(8)   VALUE ZERO.
           01 WS-REM    PIC 9(4)   VALUE ZERO.
           01 WS-FMTD   PIC ZZZ,ZZ9 VALUE ZERO.

       PROCEDURE DIVISION.
           DISPLAY "--- Arithmetic ---"
           DISPLAY "A = " WS-A "   B = " WS-B

           ADD WS-A TO WS-B GIVING WS-RESULT
           DISPLAY "ADD:      A + B = " WS-RESULT

           SUBTRACT WS-B FROM WS-A GIVING WS-RESULT
           DISPLAY "SUBTRACT: A - B = " WS-RESULT

           MULTIPLY WS-A BY WS-B GIVING WS-RESULT
           DISPLAY "MULTIPLY: A * B = " WS-RESULT

           DIVIDE WS-A BY WS-B GIVING WS-RESULT REMAINDER WS-REM
           DISPLAY "DIVIDE:   A / B = " WS-RESULT
           DISPLAY "REMAINDER:        " WS-REM

           COMPUTE WS-RESULT = (WS-A * WS-A) + (WS-B * WS-B)
           MOVE WS-RESULT TO WS-FMTD
           DISPLAY "COMPUTE:  A^2 + B^2 = " WS-FMTD

           STOP RUN.
