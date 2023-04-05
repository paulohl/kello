       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRDIAS1.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/04/89.
       SECURITY.      *************************************************
                      *                                               *
                      *   Obtem o intervalo de dias entre datas       *
                      *                                               *
                      *   Formato: CALL "GRDIAS"  USING               *
                      *                                               *
                      *                                               *
                      *   01  PARAMETROS-GRDIAS1.                     *
                      *       05 GRDIAS-AAMMDD-INICIAL PIC  9(008).   *
                      *       05 GRDIAS-AAMMDD-FINAL   PIC  9(008).   *
                      *       05 GRDIAS-NUM-DIAS       PIC  9(005).   *
                      *                                               *
                      *                                               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 ULTIMOS-DIAS.
              10 FILLER PIC X(024) VALUE "312831303130313130313031".
           05 FILLER REDEFINES ULTIMOS-DIAS.
              10 ULTIMO-DIA PIC 99 OCCURS 12.
           05 DIAS-1 COMP-3 PIC 9(006) VALUE ZERO.
           05 DIAS-2 COMP-3 PIC 9(006) VALUE ZERO.
           05 LIXO   COMP-3 PIC 9(006) VALUE ZERO.
           05 RESTO  COMP-3 PIC 9(006) VALUE ZERO.
           05 I             PIC 9(002) VALUE ZERO.
           05 J             PIC 9(004) VALUE ZERO.

       LINKAGE SECTION.

       01  PARAMETROS-GRDIAS1.
           05 GRDIAS-AAMMDD-INICIAL PIC  9(008).
           05 FILLER REDEFINES GRDIAS-AAMMDD-INICIAL.
              10 AA-I        PIC  9(004).
              10 MM-I        PIC  9(002).
              10 DD-I        PIC  9(002).
           05 GRDIAS-AAMMDD-FINAL   PIC  9(008).
           05 FILLER REDEFINES GRDIAS-AAMMDD-FINAL.
              10 AA-F        PIC  9(004).
              10 MM-F        PIC  9(002).
              10 DD-F        PIC  9(002).
           05 GRDIAS-NUM-DIAS       PIC  9(005).

       PROCEDURE DIVISION USING PARAMETROS-GRDIAS1.

       000-INICIO.

           CALL "GRIDAT1" USING  GRDIAS-AAMMDD-INICIAL
           CALL "GRIDAT1" USING  GRDIAS-AAMMDD-FINAL
           CALL "GRVDAT1" USING  GRDIAS-AAMMDD-INICIAL
           CALL "GRVDAT1" USING  GRDIAS-AAMMDD-FINAL
           CALL "GRIDAT2" USING  GRDIAS-AAMMDD-INICIAL
           CALL "GRIDAT2" USING  GRDIAS-AAMMDD-FINAL

           IF   GRDIAS-AAMMDD-INICIAL = GRDIAS-AAMMDD-FINAL
           OR   GRDIAS-AAMMDD-INICIAL = ZERO
           OR   GRDIAS-AAMMDD-FINAL   = ZERO
           OR   GRDIAS-AAMMDD-FINAL   < GRDIAS-AAMMDD-INICIAL
                MOVE ZERO TO GRDIAS-NUM-DIAS
                EXIT PROGRAM.

           DIVIDE AA-I BY 4 GIVING LIXO REMAINDER RESTO

           IF   RESTO EQUAL ZERO
                MOVE 29 TO ULTIMO-DIA (2)
           ELSE
                MOVE 28 TO ULTIMO-DIA (2).

           MOVE ZERO TO DIAS-1

           PERFORM VARYING J FROM 1980 BY 1 UNTIL J EQUAL AA-I
                   DIVIDE J BY 4 GIVING LIXO REMAINDER RESTO
                   IF   RESTO EQUAL ZERO
                        ADD 366 TO DIAS-1
                   ELSE
                        ADD 365 TO DIAS-1
                   END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL MM-I
                   ADD ULTIMO-DIA (I) TO DIAS-1
           END-PERFORM

           ADD    DD-I          TO DIAS-1
           DIVIDE AA-F BY 4 GIVING LIXO REMAINDER RESTO

           IF   RESTO EQUAL ZERO
                MOVE 29 TO ULTIMO-DIA (2)
           ELSE
                MOVE 28 TO ULTIMO-DIA (2).

           MOVE ZERO TO DIAS-2

           PERFORM VARYING J FROM 1980 BY 1 UNTIL J EQUAL AA-F
                   DIVIDE J BY 4 GIVING LIXO REMAINDER RESTO
                   IF   RESTO EQUAL ZERO
                        ADD 366 TO DIAS-2
                   ELSE
                        ADD 365 TO DIAS-2
                   END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL MM-F
                   ADD ULTIMO-DIA (I) TO DIAS-2
           END-PERFORM

           ADD DD-F TO DIAS-2

           COMPUTE GRDIAS-NUM-DIAS = DIAS-2 - DIAS-1.

       000-99-FIM. EXIT PROGRAM.

       END PROGRAM GRDIAS1.
