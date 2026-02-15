       IDENTIFICATION DIVISION. 
       PROGRAM-ID. TimeToReadWriter.
       AUTHOR. Andreas Richardsen.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT MillionShortFile ASSIGN TO "MillionShortRecords.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MillionLongFile ASSIGN TO "MillionLongRecords.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD MillionShortFile.
       01 MillionShortDetails.
           02 SField1           PIC X(4).
           02 SField2           PIC 9(2).

       FD MillionLongFile.
       01 MillionLongDetails.
           02 LField1           PIC X(4).
           02 LField2           PIC X(3).
           02 LField3           PIC 9(4).
           02 LField4           PIC 9(10).
           02 LField5           PIC 9(4).
           02 LField6           PIC X(5).
      
       WORKING-STORAGE SECTION. 
      * Short VALUE "XXXX99"
      * Long VALUE "XXXXXXX999999999999999999XXXXX"
       01 LoopCounter              PIC 9(7).
       01 TimeTaking.
           02 TimeStart            PIC 9(8).
           02 TimeEnd              PIC 9(8).
           02 TimeTaken            PIC 9(4).                
       01 ShortMod2                PIC X(6) VALUE "asdw35".
       01 ShortMod3                PIC X(6) VALUE "Swer77".
       01 ShortMod5                PIC X(6) VALUE "QWER10".
       01 ShortMod7                PIC X(6) VALUE "SYlY43".
       01 ShortOther               PIC X(6) VALUE "LMNO00".
       01 LongMod2                 PIC X(30) VALUE 
                                "SADWGFF499530212338433382JGHJF".
       01 LongMod3                 PIC X(30) VALUE 
                                "QWERTYU332940031532453332LKJHG".
       01 LongMod5                 PIC X(30) VALUE 
                                "ADSGFSD342014837773647382HJJJH".
       01 LongMod7                 PIC X(30) VALUE 
                                "MNBVCXZ271234567890098765POIUY".
       01 LongOther                PIC X(30) VALUE 
                                "ZAQWSXC135790864214756773OKMNJ".

       PROCEDURE DIVISION.
           ACCEPT TimeStart FROM TIME
            
           OPEN OUTPUT MillionShortFile
           PERFORM ShortWrite VARYING LoopCounter FROM 1 BY 1 UNTIL
              LoopCounter > 1000000
           CLOSE MillionShortFile

           ACCEPT TimeEnd FROM TIME 

           PERFORM TimeTakenCalc

           ACCEPT TimeStart FROM TIME

           OPEN OUTPUT MillionLongFile
           PERFORM LongWrite VARYING LoopCounter FROM 1 BY 1 UNTIL
              LoopCounter > 1000000
           CLOSE MillionLongFile

           ACCEPT TimeEnd FROM TIME 
           
           
           PERFORM TimeTakenCalc 
           STOP RUN.

       TimeTakenCalc.
           COMPUTE TimeTaken = TimeEnd -TimeStart
           DISPLAY TimeTaken.

       ShortWrite.
           IF FUNCTION MOD (LoopCounter, 2) = 0 
                 MOVE ShortMod2 TO MillionShortDetails
              ELSE IF FUNCTION MOD (LoopCounter, 3) = 0
                    MOVE ShortMod3 TO MillionShortDetails
                 ELSE IF FUNCTION MOD (LoopCounter, 5) = 0
                       MOVE ShortMod5 TO MillionShortDetails
                    ELSE IF FUNCTION MOD (LoopCounter, 7) = 0
                          MOVE ShortMod7 TO MillionShortDetails
                       ELSE
                          MOVE ShortOther TO MillionShortDetails 
                       END-IF 
                    END-IF 
                 END-IF 
           END-IF 
           WRITE MillionShortDetails .

       LongWrite.
           IF FUNCTION MOD (LoopCounter, 2) = 0 
                 MOVE LongMod2 TO MillionLongDetails
              ELSE IF FUNCTION MOD (LoopCounter, 3) = 0
                    MOVE LongMod3 TO MillionLongDetails
                 ELSE IF FUNCTION MOD (LoopCounter, 5) = 0
                       MOVE LongMod5 TO MillionLongDetails
                    ELSE IF FUNCTION MOD (LoopCounter, 7) = 0
                          MOVE LongMod7 TO MillionLongDetails
                       ELSE
                          MOVE LongOther TO MillionLongDetails 
                       END-IF 
                    END-IF 
                 END-IF 
           END-IF 
           WRITE MillionLongDetails .
