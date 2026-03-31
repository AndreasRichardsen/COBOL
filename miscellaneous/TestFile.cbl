       IDENTIFICATION DIVISION. 
       PROGRAM-ID. TestFile.
       AUTHOR. Andreas Richardsen.

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 Trying             PIC X(5).
           88 Hello          VALUE "Hello".
           88 Bye            VALUE "Bye".

       PROCEDURE DIVISION.
           DISPLAY "Hello OR Bye"
           ACCEPT Trying 
           DISPLAY Trying

           STOP RUN.
           