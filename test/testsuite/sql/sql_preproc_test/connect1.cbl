       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. connect1. 
       
       
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
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
         ACCEPT DBPWD FROM ENVIRONMENT-VALUE.
         
         DISPLAY '***************************************'.
         DISPLAY " DATASRC  : " DATASRC.
         DISPLAY " DB       : " DBUSR.
         DISPLAY " USER     : " DBPWD.
         DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
           END-EXEC.      
           
           DISPLAY 'CONNECT SQLCODE: ' SQLCODE

           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

      *     EXEC SQL
      *        START TRANSACTION
      *     END-EXEC.                                                    

      *     EXEC SQL
      *         SELECT COUNT(*) INTO :T1 FROM EMPTABLE
      *     END-EXEC. 

      *     DISPLAY 'SELECT SQLCODE : ' SQLCODE.
      *     
      *     IF SQLCODE <> 0 THEN
      *        GO TO 100-EXIT
      *     END-IF.     

      *     DISPLAY 'RES: ' T1.           

          EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
      *       STOP RUN.