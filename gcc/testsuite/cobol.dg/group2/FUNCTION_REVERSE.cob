       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X   PIC   X(10) VALUE "A#B.C%D+E$".
       01  Z   PIC   X(10).
       PROCEDURE        DIVISION.
           MOVE FUNCTION REVERSE ( X ) TO Z.
           IF Z NOT = "$E+D%C.B#A"
              DISPLAY Z
              END-DISPLAY
           END-IF.
           STOP RUN.

