      *> conditionals.cob - HOPPER Example 4: Conditionals
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Conditionals.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-SCORE  PIC 9(3) VALUE 85.
           01 WS-GRADE  PIC X    VALUE SPACE.

       PROCEDURE DIVISION.
           DISPLAY "Score: " WS-SCORE

           *> IF / THEN / ELSE
           DISPLAY "--- IF / THEN / ELSE ---"
           IF WS-SCORE >= 90
               DISPLAY "Grade: A — Excellent"
           ELSE IF WS-SCORE >= 80
               DISPLAY "Grade: B — Good"
           ELSE IF WS-SCORE >= 70
               DISPLAY "Grade: C — Satisfactory"
           ELSE
               DISPLAY "Grade: F — Needs improvement"
           END-IF

           *> Determine grade letter for EVALUATE
           IF WS-SCORE >= 90 MOVE "A" TO WS-GRADE
           ELSE IF WS-SCORE >= 80 MOVE "B" TO WS-GRADE
           ELSE IF WS-SCORE >= 70 MOVE "C" TO WS-GRADE
           ELSE MOVE "F" TO WS-GRADE
           END-IF

           *> EVALUATE / WHEN (COBOL's switch)
           DISPLAY "--- EVALUATE / WHEN ---"
           EVALUATE WS-GRADE
               WHEN "A"
                   DISPLAY "Dean's List!"
               WHEN "B"
                   DISPLAY "Well done."
               WHEN "C"
                   DISPLAY "Passed."
               WHEN OTHER
                   DISPLAY "See your advisor."
           END-EVALUATE

           STOP RUN.
