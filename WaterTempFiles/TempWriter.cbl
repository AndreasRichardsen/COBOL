       IDENTIFICATION DIVISION. 
       PROGRAM-ID. TempWriter.
       AUTHOR. Andreas Richardsen.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT TempFile ASSIGN TO "Temps.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD TempFile.
       01 TempDetails.
           02 TempDate.
              03 TempYear    PIC 9(4).
              03 TempMonth   PIC 9(2).
              03 TempDay     PIC 9(2).
           02 TempTime.
              03 TempHour    PIC 9(2).
              03 TempMinute  PIC 9(2).
           02 Country         PIC X(13).
           02 Location        PIC X(30).
           02 Temperature     PIC 9(2).

       PROCEDURE DIVISION.
       BEGIN.
           OPEN EXTEND TempFile 
           DISPLAY "Enter:"
           DISPLAY "Date (YYYYMMDD)"
           ACCEPT TempDate.
           DISPLAY "Time (HHMM)"
           ACCEPT TempTime.
           DISPLAY "Country"
           ACCEPT Country.
           DISPLAY "Location"
           ACCEPT Location.
           DISPLAY "Temperature (Celsius)"
           ACCEPT Temperature.
           WRITE TempDetails
           CLOSE TempFile 
           STOP RUN.
