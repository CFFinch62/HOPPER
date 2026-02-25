      *> loops.cob - HOPPER Example 5: Loop Structures
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Loops.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-COUNT  PIC 9(3) VALUE ZERO.
           01 WS-TOTAL  PIC 9(6) VALUE ZERO.
           01 WS-INDEX  PIC 9(3) VALUE ZERO.

       PROCEDURE DIVISION.
           *> PERFORM N TIMES
           DISPLAY "--- PERFORM 3 TIMES ---"
           PERFORM 3 TIMES
               ADD 1 TO WS-COUNT
               DISPLAY "  Iteration: " WS-COUNT
           END-PERFORM

           *> PERFORM VARYING
           DISPLAY "--- PERFORM VARYING (1 to 5) ---"
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
               DISPLAY "  Index = " WS-INDEX
           END-PERFORM

           *> PERFORM UNTIL (like a while loop)
           DISPLAY "--- PERFORM UNTIL total > 10 ---"
           MOVE 0 TO WS-TOTAL
           MOVE 1 TO WS-COUNT
           PERFORM UNTIL WS-TOTAL > 10
               ADD WS-COUNT TO WS-TOTAL
               ADD 1 TO WS-COUNT
           END-PERFORM
           DISPLAY "  Final total = " WS-TOTAL

           STOP RUN.
