       IDENTIFICATION DIVISION.
       PROGRAM-ID. WorkHourWriter.
       AUTHOR. Andreas Richardsen.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT WorkHourFile ASSIGN TO "WorkHours.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD WorkHourFile.
       01 WorkHourDetails.
           88 EndOfFileWH       VALUE HIGH-VALUES.
           02 WHDate.
              03 WHYear         PIC 9(4).
              03 WHMonth        PIC 9(2).
              03 WHDay          PIC 9(2).
           02 WHStartTime.
              03 WHStartHour    PIC 9(2).
              03 WHStartMinute  PIC 9(2).
           02 WHEndTime.
              03 WHEndHour      PIC 9(2).
              03 WHEndMinute    PIC 9(2).
           02 WHHoursWorked     PIC 9V99.

       WORKING-STORAGE SECTION.
       01 LineBreak             PIC X(72) VALUE ALL "-".
       01 LastEntry.
           02 LEDate.
              03 LEYear         PIC 9(4).
              03 FILLER         PIC X       VALUE "/".
              03 LEMonth        PIC 9(2).
              03 FILLER         PIC X       VALUE "/".
              03 LEDay          PIC 9(2).
           02 LEStartTime.
              03 LEStartHour    PIC 9(2).
              03 FILLER         PIC X       VALUE ":".
              03 LEStartMinute  PIC 9(2).
           02 LEEndTime.
              03 LEEndHour      PIC 9(2).
              03 FILLER         PIC X       VALUE ":".
              03 LEEndMinute    PIC 9(2).

       PROCEDURE DIVISION.

           OPEN INPUT WorkHourFile 
           PERFORM UNTIL EndOfFileWH
              PERFORM MoveToLastEntry
              READ WorkHourFile
                 AT END SET EndOfFileWH TO TRUE
              END-READ
           END-PERFORM
           CLOSE WorkHourFile

           OPEN EXTEND WorkHourFile
           PERFORM UNTIL EXIT   
              DISPLAY LineBreak  
              DISPLAY "Last entry: " WITH NO ADVANCING 
              DISPLAY LEDate " (" LEStartTime " - " LEEndTime ")"
              DISPLAY "Date (YYYYMMDD)"
              ACCEPT WHDate
              IF WHDate = SPACE 
                 EXIT PERFORM
              END-IF 
              DISPLAY "Start time (HHMM)"
              ACCEPT WHStartTime
              DISPLAY "End time (HHMM)"
              ACCEPT WHEndTime
              COMPUTE WHHoursWorked ROUNDED = 
                 (((WHEndHour * 60) + WHEndMinute) 
                 - ((WHStartHour * 60) + WHStartMinute))/60
              WRITE WorkHourDetails
              PERFORM MoveToLastEntry
              END-PERFORM
           CLOSE WorkHourFile           
           STOP RUN.

       MoveToLastEntry.
              MOVE WHYear TO LEYear 
              MOVE WHMonth TO LEMonth 
              MOVE WHDay TO LEDay 
              MOVE WHStartHour TO LEStartHour 
              MOVE WHStartMinute TO LEStartMinute 
              MOVE WHEndHour TO LEEndHour 
              MOVE WHEndMinute TO LEEndMinute.
           