       IDENTIFICATION DIVISION. 
       PROGRAM-ID. DiceDecide.
       AUTHOR. Andreas Richardsen.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT RollFile ASSIGN TO "Rolls.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD RollFile.
       01 DiceChoice            PIC X(10).

       WORKING-STORAGE SECTION. 
       01 DiceRoll              PIC 9(2).
           88 Cobol             VALUE 1.
           88 Math              VALUE 2.
           88 DnD               VALUE 3.

       PROCEDURE DIVISION.
           OPEN EXTEND RollFile

           DISPLAY "Enter roll!: " WITH NO ADVANCING
           ACCEPT DiceRoll

           EVALUATE TRUE
           WHEN Cobol           DISPLAY "COBOL!"
              MOVE "COBOL" TO DiceChoice  
           WHEN Math            DISPLAY "Math!"
              MOVE "Math" TO DiceChoice 
           WHEN DnD             DISPLAY "D&D!"
              MOVE "D&D" TO DiceChoice 
           WHEN OTHER
              DISPLAY "Invalid input!"
           END-EVALUATE 

           WRITE DiceChoice 
           CLOSE RollFile

           STOP RUN.
