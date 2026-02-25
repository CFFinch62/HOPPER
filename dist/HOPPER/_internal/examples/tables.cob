      *> tables.cob - HOPPER Example 7: Tables (Arrays)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tables.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-SCORES-TABLE.
               05 WS-SCORE    PIC 9(3) OCCURS 5 TIMES INDEXED BY I.
           01 WS-TOTAL   PIC 9(6) VALUE ZERO.
           01 WS-AVERAGE PIC 9(5)V99 VALUE ZERO.
           01 WS-COUNT   PIC 9   VALUE 5.

       PROCEDURE DIVISION.
           *> Populate table
           MOVE 85 TO WS-SCORE(1)
           MOVE 92 TO WS-SCORE(2)
           MOVE 78 TO WS-SCORE(3)
           MOVE 95 TO WS-SCORE(4)
           MOVE 88 TO WS-SCORE(5)

           DISPLAY "--- Scores Table ---"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-COUNT
               DISPLAY "  Score(" I ") = " WS-SCORE(I)
               ADD WS-SCORE(I) TO WS-TOTAL
           END-PERFORM

           COMPUTE WS-AVERAGE = WS-TOTAL / WS-COUNT
           DISPLAY "Total:   " WS-TOTAL
           DISPLAY "Average: " WS-AVERAGE

           STOP RUN.
