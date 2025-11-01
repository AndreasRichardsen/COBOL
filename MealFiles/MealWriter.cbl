       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MealWriter.
       AUTHOR. Andreas Richardsen.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT MealFile ASSIGN TO "Meals.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD MealFile.
       01 DinnerDetails.
           02 MealDate.
              03 MealYear  PIC 9(4).
              03 MealMonth PIC 9(2).
              03 MealDay   PIC 9(2).
           02 Meat           PIC X(20).
           02 Side           PIC X(20).

       PROCEDURE DIVISION.
       BEGIN.
           OPEN EXTEND MealFile
              IF Dinner
                 PERFORM InsertDinner
                 WRITE DinnerDetails
              END-IF 
              IF Breakfast OR Lunch 
                 PERFORM InsertOtherMeal
                 WRITE OtherMealDetails
              END-IF
              PERFORM StartInput
           END-PERFORM
           CLOSE MealFile 
           STOP RUN.

       StartInput.
           DISPLAY "Enter:"
           DISPLAY "B = Breakfast"
           DISPLAY "L = Lunch"
           DISPLAY "D = Dinner"
           DISPLAY "X = Exit"
           ACCEPT DinnerTypeCode.

       InsertDinner.
           Display "Name"
           ACCEPT DinnerName
           Display "Protein"
           ACCEPT Protein 
           Display "Carb"
           ACCEPT Carb 
           Display "Fiber"
           ACCEPT Fiber 
           Display "Sauce"
           ACCEPT Sauce 
           Display "Info"
           ACCEPT DinnerInfo 
           Display "Date (YYYYMMDD)"
           ACCEPT DinnerDate.

       InsertOtherMeal.
           Display "Name"
           ACCEPT OtherName
           Display "Info"
           ACCEPT OtherInfo 
           Display "Date (YYYYMMDD)"
           ACCEPT OtherDate.
           
                 
              

       






      * 01 SnackDetails.
      *     02 RecTypeCode    PIC X.
      *     02 SnaId          PIC X(6).
      *     02 SnaDate.
      *        03 SnaYear     PIC 9(4).
      *        03 SnaMonth    PIC 9(2).
      *        03 SnaDay      PIC 9(2).

      * 01 LunchDetails.
      *     02 RecTypeCode    PIC X.
      *     02 LunId          PIC X(6).
      *     02 LunName        PIC X(50).
      *     02 LunInfo        PIC X(25).
      *     02 LunDate.
      *        03 LunYear     PIC 9(4).
      *        03 LunMonth    PIC 9(2).
      *        03 LunDay      PIC 9(2). 

      * Cooked, ordered, cooked for
