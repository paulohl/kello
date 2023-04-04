       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRADAY1.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/04/89.
       SECURITY.      *************************************************
                      *                                               *
                      *   Adiciona nnnn dias a uma data               *
                      *                                               *
                      *                                               *
                      *   Formato: CALL "GRADAY" USING                *
                      *                                               *
                      *   01  PARAMETROS-GRADAY1                      *
                      *       05 GRADAY-DATA     PIC  9(008) (DDMMAA) *
                      *       05 GRDIAS-NUM-DIAS PIC  9(004)          *
                      *                                               *
                      *                                               *
                      *                                               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
             05 T-DMAX.
                10 FILLER              PIC  X(012) VALUE "312831303130".
                10 FILLER              PIC  X(012) VALUE "313130313031".
             05 RT-DMAX REDEFINES T-DMAX.
                10 DIA-MAX OCCURS 12   PIC  9(002).
             05 W-TP-ANO               PIC  9(001).
                88 ANO-BISSEXTO                    VALUE  0.
             05 W-DIA                  PIC  9(004).
             05 W-NADA                 PIC  9(002).

       LINKAGE SECTION.

       01  PARAMETROS-GRADAY1.
           05 DDMMAA.
              10 DD      PIC 9(02).
              10 MM      PIC 9(02).
              10 AA      PIC 9(04).
           05 GRDIAS-NUM-DIAS   PIC 9(04).

       PROCEDURE DIVISION USING PARAMETROS-GRADAY1.

       000-INICIO.

           COMPUTE W-DIA = DD + GRDIAS-NUM-DIAS.

       000-01-CHECK-ANO.

           DIVIDE AA BY 4
                  GIVING W-NADA
                         REMAINDER W-TP-ANO.

           IF   ANO-BISSEXTO
                MOVE 29 TO DIA-MAX (2)
           ELSE
                MOVE 28 TO DIA-MAX (2).

       000-02-MAIS-DIA.

           IF   W-DIA GREATER DIA-MAX (MM)
                SUBTRACT DIA-MAX (MM) FROM W-DIA
                ADD      1              TO MM
                IF   MM EQUAL 13
                     MOVE 1 TO MM
                     ADD  1 TO AA
                     GO     TO 000-01-CHECK-ANO
                ELSE
                     GO     TO 000-02-MAIS-DIA
           ELSE
                MOVE W-DIA TO DD.

       000-00-FIM. EXIT PROGRAM.

       END PROGRAM GRADAY1.
