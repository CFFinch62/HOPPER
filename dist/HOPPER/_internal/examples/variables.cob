      *> variables.cob - HOPPER Example 2: Variables and Data Types
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Variables.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-INTEGER     PIC 9(4)        VALUE 2024.
           01 WS-DECIMAL     PIC 9(4)V99     VALUE 1234.56.
           01 WS-SIGNED      PIC S9(4)       VALUE -42.
           01 WS-TEXT        PIC X(20)       VALUE "Grace Hopper".
           01 WS-CHAR        PIC X           VALUE "H".
           01 WS-FLAG        PIC X           VALUE "Y".
               88 IS-YES     VALUE "Y".
               88 IS-NO      VALUE "N".
           01 WS-DISPLAY     PIC ZZ,ZZ9.99.

       PROCEDURE DIVISION.
           DISPLAY "--- COBOL Data Types ---"
           DISPLAY "Integer:   " WS-INTEGER
           DISPLAY "Decimal:   " WS-DECIMAL
           DISPLAY "Signed:    " WS-SIGNED
           DISPLAY "Text:      " WS-TEXT
           DISPLAY "Character: " WS-CHAR
           DISPLAY "Flag (88): " WS-FLAG

           IF IS-YES
               DISPLAY "Flag is YES"
           END-IF

           MOVE WS-DECIMAL TO WS-DISPLAY
           DISPLAY "Formatted: " WS-DISPLAY

           STOP RUN.
