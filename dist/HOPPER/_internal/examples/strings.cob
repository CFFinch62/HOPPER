      *> strings.cob - HOPPER Example 6: String Operations
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Strings.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-FIRST     PIC X(10) VALUE "Grace".
           01 WS-LAST      PIC X(10) VALUE "Hopper".
           01 WS-FULL      PIC X(25) VALUE SPACES.
           01 WS-GREETING  PIC X(40) VALUE SPACES.
           01 WS-UPPER     PIC X(10) VALUE SPACES.
           01 WS-LENGTH    PIC 9(3)  VALUE ZERO.
           01 WS-DELIM     PIC X     VALUE SPACE.
           01 WS-PART1     PIC X(15) VALUE SPACES.
           01 WS-PART2     PIC X(15) VALUE SPACES.

       PROCEDURE DIVISION.
           DISPLAY "--- STRING (concatenation) ---"
           STRING WS-FIRST DELIMITED BY SPACE
                  " "      DELIMITED BY SIZE
                  WS-LAST  DELIMITED BY SPACE
                  INTO WS-FULL
           DISPLAY "Full name: " WS-FULL

           DISPLAY "--- FUNCTION UPPER-CASE ---"
           MOVE FUNCTION UPPER-CASE(WS-FIRST) TO WS-UPPER
           DISPLAY "Upper: " WS-UPPER

           DISPLAY "--- FUNCTION LENGTH ---"
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LAST)) TO WS-LENGTH
           DISPLAY "Length of '" WS-LAST "' (trimmed): " WS-LENGTH

           DISPLAY "--- UNSTRING (split) ---"
           STRING "Ada,Lovelace" DELIMITED BY SIZE INTO WS-GREETING
           UNSTRING WS-GREETING DELIMITED BY ","
               INTO WS-PART1
                    WS-PART2
           DISPLAY "Part 1: " FUNCTION TRIM(WS-PART1)
           DISPLAY "Part 2: " FUNCTION TRIM(WS-PART2)

           STOP RUN.
