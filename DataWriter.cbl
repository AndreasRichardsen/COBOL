       IDENTIFICATION DIVISION.
       PROGRAM-ID. DataWriter.
       AUTHOR. Andreas Richardsen.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DataFile ASSIGN TO "xxx.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
      * REPLACE xxx WITH FILENAME 
      

       DATA DIVISION.
       FILE SECTION.
       FD DataFile.
       01 DataDetails.
           88 EndOfDataFile    VALUE HIGH-VALUES.
           02 Input1           PIC X(25).
           02 Input2           PIC X(25).
      * Add the fields here!

       PROCEDURE DIVISION.
       BEGIN.
           OPEN OUTPUT DataFile
           PERFORM InsertData
           PERFORM UNTIL Input1 = "$"
              WRITE DataDetails
              PERFORM InsertData
           END-PERFORM
           CLOSE DataFile
           DISPLAY "*************** End of Input ***************"

           OPEN INPUT DataFile
           READ DataFile
              AT END SET EndOfDataFile TO TRUE
           END-READ
           PERFORM UNTIL EndOfDataFile
              DISPLAY DataDetails
              READ DataFile
                 AT END SET EndOfDataFile TO TRUE
              END-READ
           END-PERFORM
           CLOSE DataFile
           STOP RUN.

       InsertData.
           DISPLAY "Forename"
           ACCEPT Input1
           DISPLAY "Surname"
           ACCEPT Input2.
      * Add the fields here!
