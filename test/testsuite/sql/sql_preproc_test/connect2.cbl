       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. connect2. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  

       FILE SECTION.
      
       WORKING-STORAGE SECTION. 

           01 DATASRC         PIC X(64).
           01 DATASRC-FULL    PIC X(64).
           01 DBS             PIC X(64).
           01 DBUSR           PIC X(64).
           01 DBPWD           PIC X(64).
           01 DBUSRPWD        PIC X(128).
           01 DBNAME          PIC X(64).
                              
           01 T1              PIC 9(4).
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC_FULL" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC-FULL FROM ENVIRONMENT-VALUE.

         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
         
         DISPLAY "DBUSR" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
         
         DISPLAY "DBPWD" UPON ENVIRONMENT-NAME.
         ACCEPT DBPWD FROM ENVIRONMENT-VALUE.
         
         DISPLAY "DBNAME" UPON ENVIRONMENT-NAME.
         ACCEPT DBNAME FROM ENVIRONMENT-VALUE.
         
         DISPLAY "DBUSRPWD" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSRPWD FROM ENVIRONMENT-VALUE.

       100-MAIN.
       
         MOVE 'CONN1' TO DBS

      * mode 1 (anonymous)

         EXEC SQL
            CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
         END-EXEC.      
         DISPLAY 'CONNECT 1A SQLCODE: ' SQLCODE.
         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.

         EXEC SQL CONNECT RESET END-EXEC.

      * mode 2 (anonymous)

         EXEC SQL
            CONNECT TO :DATASRC USER :DBUSRPWD
         END-EXEC.    
         DISPLAY 'CONNECT 2A SQLCODE: ' SQLCODE.
         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.         

         EXEC SQL CONNECT RESET END-EXEC. 
         
      * mode 3 (anonymous)       
      
         EXEC SQL 
            CONNECT :DBUSR 
                IDENTIFIED BY :DBPWD 
                USING :DATASRC
         END-EXEC.
         DISPLAY 'CONNECT 3A SQLCODE: ' SQLCODE.
         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.             
         
         EXEC SQL CONNECT RESET END-EXEC. 

      * mode 4 (anonymous) - Unsupported - emit a preproc warning

         EXEC SQL
            CONNECT :DBUSR IDENTIFIED BY :DBPWD
         END-EXEC.
         DISPLAY 'CONNECT 4A SQLCODE: ' SQLCODE.
      
      *  we ignore the error for mode 4
      *  IF SQLCODE <> 0 THEN
      *     GO TO 100-EXIT
      *  END-IF.      
         
      * mode 5 (anonymous) 

         EXEC SQL
            CONNECT USING :DATASRC-FULL
         END-EXEC.
         DISPLAY 'CONNECT 5A SQLCODE: ' SQLCODE.
         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.             

      * mode 6 (anonymous) 

         EXEC SQL
            CONNECT TO            :DBNAME
                    USER          :DBUSR
                    USING         :DATASRC
                    IDENTIFIED BY :DBPWD
         END-EXEC.
         DISPLAY 'CONNECT 6A SQLCODE: ' SQLCODE.
         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.   

      * mode 1 (named)

         EXEC SQL
            CONNECT TO :DATASRC AS :DBS USER :DBUSR USING :DBPWD
         END-EXEC.      
         DISPLAY 'CONNECT 1N SQLCODE: ' SQLCODE.
         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.

         EXEC SQL CONNECT RESET :DBS END-EXEC.

      * mode 2 (named)

         EXEC SQL
            CONNECT TO :DATASRC AS :DBS USER :DBUSRPWD
         END-EXEC.    
         DISPLAY 'CONNECT 2N SQLCODE: ' SQLCODE.
         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.         

         EXEC SQL CONNECT RESET :DBS END-EXEC. 
         
      * mode 3 (named)       
      
         EXEC SQL 
            CONNECT :DBUSR 
                IDENTIFIED BY :DBPWD 
                AT :DBS
                USING :DATASRC
         END-EXEC.
         DISPLAY 'CONNECT 3N SQLCODE: ' SQLCODE.
         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.             
         
         EXEC SQL CONNECT RESET :DBS END-EXEC. 

      * mode 4 (named) - Unsupported - emit a preproc warning

         EXEC SQL
            CONNECT :DBUSR IDENTIFIED BY :DBPWD
                AT :DBS
         END-EXEC.
         DISPLAY 'CONNECT 4N SQLCODE: ' SQLCODE.
      
      *  we ignore the error for mode 4
      *  IF SQLCODE <> 0 THEN
      *     GO TO 100-EXIT
      *  END-IF.      

      * last step, we need to "force" the error code, otherwise the test will fail

        MOVE 0 TO RETURN-CODE.
         
      * mode 5  and 6 do not support named connections
         
       100-EXIT. 
      *      STOP RUN.

       200-END.
