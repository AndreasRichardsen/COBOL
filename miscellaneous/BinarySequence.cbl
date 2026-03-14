       IDENTIFICATION DIVISION.
       PROGRAM-ID. BinarySequence.
       AUTHOR. Andreas Richardsen.

       ENVIRONMENT DIVISION.
       DATA DIVISION. 
       FILE SECTION. 
       WORKING-STORAGE SECTION. 
       01 SeqNr                PIC 9(2) VALUE 1.
       01 SeqValue             PIC 9(10) VALUE 10.
       01 SeqValueEdited       PIC ZBZZZBZZZBZZZ.

       PROCEDURE DIVISION.
           MOVE SeqValue TO SeqValueEdited 
           DISPLAY SeqNr " - " SeqValueEdited 
           PERFORM UNTIL SeqNr = 30
              ADD 1 TO SeqNr 
              MULTIPLY SeqValue BY 2 GIVING  SeqValue SeqValueEdited 
              DISPLAY SeqNr " - " SeqValueEdited 
              END-PERFORM

           STOP RUN.
