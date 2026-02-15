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
       01 TypeOfDay             PIC X.
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
       01 StandardHours.
           02 SHStart1          PIC 9(4)    VALUE 0730.
           02 SHEnd1            PIC 9(4)    VALUE 1150.
           02 SHStart2          PIC 9(4)    VALUE 1220.
           02 SHEnd2            PIC 9(4)    VALUE 1600.
       01 StandarHoursFriday.
           02 SHFStart1         PIC 9(4)    VALUE 0800.
           02 SHFEnd1           PIC 9(4)    VALUE 1150.
           02 SHFStart2         PIC 9(4)    VALUE 1220.
           02 SHFEnd2           PIC 9(4)    VALUE 1400.

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
              DISPLAY "Enter = Non standard day"
              DISPLAY "s = Standard day"
              DISPLAY "f = Standard friday"
              ACCEPT TypeOfDay 
              EVALUATE TypeOfDay 
                 WHEN SPACE  PERFORM NonStandardDay
                 WHEN "s"    PERFORM StandardDay
                 WHEN "f"    PERFORM StandardFriday
              END-EVALUATE
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

       StandardDay.
              MOVE SHStart1 TO WHStartTime 
              MOVE SHEnd1 TO WHEndTime 
              COMPUTE WHHoursWorked ROUNDED = 
                 (((WHEndHour * 60) + WHEndMinute) 
                 - ((WHStartHour * 60) + WHStartMinute))/60
              WRITE WorkHourDetails
              MOVE SHStart2 TO WHStartTime 
              MOVE SHEnd2 TO WHEndTime 
              COMPUTE WHHoursWorked ROUNDED = 
                 (((WHEndHour * 60) + WHEndMinute) 
                 - ((WHStartHour * 60) + WHStartMinute))/60.

       StandardFriday.
              MOVE SHFStart1 TO WHStartTime 
              MOVE SHFEnd1 TO WHEndTime 
              COMPUTE WHHoursWorked ROUNDED = 
                 (((WHEndHour * 60) + WHEndMinute) 
                 - ((WHStartHour * 60) + WHStartMinute))/60
              WRITE WorkHourDetails
              MOVE SHFStart2 TO WHStartTime 
              MOVE SHFEnd2 TO WHEndTime 
              COMPUTE WHHoursWorked ROUNDED = 
                 (((WHEndHour * 60) + WHEndMinute) 
                 - ((WHStartHour * 60) + WHStartMinute))/60.

       NonStandardDay.
              DISPLAY "Start time (HHMM)"
              ACCEPT WHStartTime
              DISPLAY "End time (HHMM)"
              ACCEPT WHEndTime
              COMPUTE WHHoursWorked ROUNDED = 
                 (((WHEndHour * 60) + WHEndMinute) 
                 - ((WHStartHour * 60) + WHStartMinute))/60.
           