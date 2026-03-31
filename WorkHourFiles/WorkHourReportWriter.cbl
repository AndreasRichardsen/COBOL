       IDENTIFICATION DIVISION. 
       PROGRAM-ID. WorkHourReportWriter.
       AUTHOR. Andreas Richardsen.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT WorkHourFile ASSIGN TO "WorkHours.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT WorkHourReport ASSIGN TO "WorkHourReport.rpt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD WorkHourFile.
       01 WorkHourDetails.
           88 EndOfFileWH    VALUE HIGH-VALUES.
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

       FD WorkHourReport.
       01 PrintLine             PIC X(55).
       
       WORKING-STORAGE SECTION.
       01 ReportHeading.
           02 FILLER            PIC X(20) VALUE SPACES.
           02 FILLER            PIC X(15) VALUE "Monthly report".

       01 SubjectHeading.
           02 FILLER            PIC X(15) VALUE "Date".
           02 FILLER            PIC X(13) VALUE "Start time".
           02 FILLER            PIC X(13) VALUE "End time".
           02 FILLER            PIC X(15) VALUE "Hours worked".

       01 DetailLine.
           02 PrnDate           PIC X(15).
              88 SuppressDate   VALUE SPACES.
           02 PrnStartHour      PIC X(2).
           02 FILLER            PIC X VALUE ":".
           02 PrnStartMinute    PIC X(10).
           02 PrnEndHour        PIC X(2).
           02 FILLER            PIC X VALUE ":".
           02 PrnEndMinute      PIC X(10).
           02 PrnHoursWorked    PIC 9.99.

       01 DayHoursWorked.
           02 FILLER            PIC X(41) VALUE SPACES.
           02 PrnDayHours       PIC 99.99.

       01 MonthHoursWorked.
           02 FILLER            PIC X(15) VALUE SPACES.
           02 FILLER            PIC X(7) VALUE "Total:".
           02 PrnMonthHours     PIC 999.99.
           02 FILLER            PIC X(5) VALUE SPACES.
           02 FILLER            PIC X(6) VALUE "Days:".
           02 PrnDayCount       PIC X(2).

       01 DayHours              PIC 9(2)V99.
       01 MonthHours            PIC 9(3)V99.
       01 DayCount              PIC 9(2).
       01 Divider               PIC X(55) VALUE ALL "*".

       01 PrevDate              PIC X(8).

       01 StartDate.
           02 StartYear         PIC 9(4).
           02 StartMonth        PIC 9(2).
           02 StartDay          PIC 9(2) VALUE 26.
       01 EndDate.
           02 EndYear           PIC 9(4).
           02 EndMonth          PIC 9(2).
           02 EndtDay           PIC 9(2) VALUE 25.
       01 Months                PIC X(9).
           88 January           VALUE "january".
           88 February          VALUE "february".
           88 March             VALUE "march".
           88 April             VALUE "april".
           88 May               VALUE "may".
           88 June              VALUE "june".
           88 July              VALUE "july".
           88 August            VALUE "august".
           88 September         VALUE "september".
           88 October           VALUE "october".
           88 November          VALUE "november".
           88 December          VALUE "december".

       PROCEDURE DIVISION.
           OPEN INPUT WorkHourFile
           OPEN OUTPUT WorkHourReport 
           WRITE PrintLine FROM ReportHeading AFTER ADVANCING 1 LINE 
           WRITE PrintLine FROM SubjectHeading AFTER ADVANCING 1 LINE 
           WRITE PrintLine FROM Divider AFTER ADVANCING 1 Line

           DISPLAY "Year: " WITH NO ADVANCING 
           ACCEPT EndYear
           DISPLAY "Month: " WITH NO ADVANCING 
           ACCEPT Months
           MOVE FUNCTION LOWER-CASE(Months) TO Months 

           PERFORM ConvertInputToDates 

           READ WorkHourFile 
              AT END SET EndOfFileWH TO TRUE 
           END-READ
           PERFORM UNTIL WHDate >= StartDate OR EndOfFileWH
              READ WorkHourFile 
                 AT END SET EndOfFileWH TO TRUE
              END-READ
           END-PERFORM

           PERFORM UNTIL WHDate > EndDate OR EndOfFileWH
           MOVE WHDate TO PrevDate, PrnDate 
           MOVE ZEROES TO DayHours

           PERFORM SumDayHours 
              UNTIL WHDate NOT = PrevDate 
                 OR WHDate > EndDate OR EndOfFileWH  
           MOVE DayHours TO PrnDayHours 
           WRITE PrintLine FROM DayHoursWorked  AFTER ADVANCING 1 LINE 
           ADD 1 TO DayCount 
           END-PERFORM

           MOVE MonthHours TO PrnMonthHours 
           MOVE DayCount TO PrnDayCount 
           WRITE PrintLine FROM Divider AFTER ADVANCING 1 LINE 
           WRITE PrintLine FROM MonthHoursWorked AFTER ADVANCING 1 LINE 

           CLOSE WorkHourFile 
           CLOSE WorkHourReport 
           DISPLAY "Report made for: " 
              FUNCTION TRIM(Months) " - " EndYear 
           STOP RUN.              
           
       SumDayHours.
           ADD WHHoursWorked TO DayHours, MonthHours

           MOVE WHStartHour TO PrnStartHour 
           MOVE WHStartMinute TO PrnStartMinute 
           MOVE WHEndHour TO PrnEndHour 
           MOVE WHEndMinute TO PrnEndMinute 
           MOVE WHHoursWorked TO PrnHoursWorked

           WRITE PrintLine FROM DetailLine AFTER ADVANCING 1 LINE 
           READ WorkHourFile 
              AT END SET EndOfFileWH TO TRUE 
           END-READ
           SET SuppressDate TO TRUE.

       ConvertInputToDates.
           EVALUATE TRUE 
           WHEN January 
              MOVE EndYear TO StartYear 
              SUBTRACT 1 FROM StartYear 
              MOVE 12 TO StartMonth
              MOVE 1 TO EndMonth 
           WHEN February
              MOVE EndYear TO StartYear
              MOVE 1 TO StartMonth 
              MOVE 2 TO EndMonth 
           WHEN March
              MOVE EndYear TO StartYear
              MOVE 2 TO StartMonth 
              MOVE 3 TO EndMonth
           WHEN April
              MOVE EndYear TO StartYear
              MOVE 3 TO StartMonth 
              MOVE 4 TO EndMonth
           WHEN May
              MOVE EndYear TO StartYear
              MOVE 4 TO StartMonth 
              MOVE 5 TO EndMonth
           WHEN June
              MOVE EndYear TO StartYear
              MOVE 5 TO StartMonth 
              MOVE 6 TO EndMonth
           WHEN July
              MOVE EndYear TO StartYear
              MOVE 6 TO StartMonth 
              MOVE 7 TO EndMonth
           WHEN August
              MOVE EndYear TO StartYear
              MOVE 7 TO StartMonth 
              MOVE 8 TO EndMonth
           WHEN September
              MOVE EndYear TO StartYear
              MOVE 8 TO StartMonth 
              MOVE 9 TO EndMonth
           WHEN October
              MOVE EndYear TO StartYear
              MOVE 9 TO StartMonth 
              MOVE 10 TO EndMonth
           WHEN November
              MOVE EndYear TO StartYear
              MOVE 10 TO StartMonth 
              MOVE 11 TO EndMonth
           WHEN December
              MOVE EndYear TO StartYear
              MOVE 11 TO StartMonth 
              MOVE 12 TO EndMonth
           WHEN OTHER
              DISPLAY "Invalid input!"
           END-EVALUATE.
              
           