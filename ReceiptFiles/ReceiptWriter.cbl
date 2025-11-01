       IDENTIFICATION DIVISION. 
       PROGRAM-ID. ReceiptWriter.
       AUTHOR. Andreas Richardsen.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT ReceiptFile ASSIGN TO "Receipts.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD ReceiptFile.
       01 ReceiptDetails.
           02 RNr               PIC 9(5).
           02 RDate.
              03 RYear          PIC 9(4).
              03 RMonth         PIC 9(2).
              03 RDay           PIC 9(2).
           02 RName             PIC X(30).
           02 RPrice            PIC 9(4)V99.
           02 RAmount           PIC 9(2).
           02 RType             PIC X(7).
      *    Snack, Protein, Carb, Drink, Green, Sauce

       WORKING-STORAGE SECTION.
       01 ExitRead         PIC X.          

       PROCEDURE DIVISION.

           OPEN INPUT ReceiptFile  
           PERFORM UNTIL ExitRead = 1
              READ ReceiptFile 
                 AT END SET ExitRead TO 1
              END-READ
              DISPLAY "-" RNr 
           END-PERFORM
           DISPLAY RNr 
           CLOSE ReceiptFile 

           OPEN EXTEND ReceiptFile
           DISPLAY RNr 
           PERFORM UNTIL EXIT
              ADD 1 TO RNr
              DISPLAY "Date (YYYYMMDD) (Enter to end!)"
              ACCEPT RDate
              IF RDate = SPACE  
                 EXIT PERFORM
              END-IF
                 PERFORM UNTIL EXIT 
                    DISPLAY "Name (Enter for new receipt!"
                    ACCEPT RName
                    IF RName = SPACE
                       EXIT PERFORM
                    END-IF
                    DISPLAY "Price"
                    ACCEPT RPrice 
                    DISPLAY "Amount"
                    ACCEPT RAmount 
                    DISPLAY "Type (Snack, Protein, Carb, Drink, Green,
      -                     "Sauce, Other)"
                    ACCEPT RType 
                    WRITE ReceiptDetails 
                    END-PERFORM
              END-PERFORM
           CLOSE ReceiptFile
           STOP RUN.
