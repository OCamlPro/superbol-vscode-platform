﻿       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL002A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  
       
       FILE SECTION.  
       
       WORKING-STORAGE SECTION. 
       
           01 DATASRC PIC X(64).
           01 DBUSR  PIC X(64).
           01 DBPWD  PIC X(64).
           
           01 T1     PIC 9(3) VALUE 0.  
          
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         EXEC SQL 
            INCLUDE EMPREC 
         END-EXEC. 
       
         ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.

         EXEC SQL 
            INCLUDE SQLCA 
         END-EXEC. 
         ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
         
         DISPLAY '***************************************'.
         DISPLAY " DATASRC  : " DATASRC.
         DISPLAY " DBUSR    : " DBUSR.
         DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.      
           
           DISPLAY 'CONNECT SQLCODE: ' SQLCODE

           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

           EXEC SQL
              START TRANSACTION
           END-EXEC.                                                    

           EXEC SQL
               SELECT COUNT(*) INTO :T1 FROM EMPTABLE
           END-EXEC. 

           DISPLAY 'SELECT SQLCODE : ' SQLCODE.
           
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.     

           DISPLAY 'RES: ' T1.           

           EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.