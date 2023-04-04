       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRWEEK1.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/03/88.
       SECURITY.      *************************************************
                      *                                               *
                      *   Subrotina para obter dia da semana          *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREA-DE-TRABALHO-1.
           05 GRWEEK-DATA                      PIC  9(008).
           05 R1-DATA-WEEK REDEFINES GRWEEK-DATA.
              10 DM-WEEK                     PIC  9(004).
              10 R-DM-WEEK REDEFINES DM-WEEK.
                 15 DD-WEEK                  PIC  9(002).
                 15 MM-WEEK                  PIC  9(002).
                 15 AA-WEEK                  PIC  9(004).
           05 V-WEEK-5                       PIC  9(005).
           05 R-V-WEEK-5 REDEFINES V-WEEK-5.
              10 V-WEEK-3                    PIC  9(003).
              10 FILLER                      PIC  9(002).
           05 TAB-INC-WEEK                   PIC  X(012) VALUE
              '255136140250'.
           05 R-TAB-INC-WEEK REDEFINES TAB-INC-WEEK.
              10 INC-WEEK OCCURS 12          PIC  9(001).
           05 TAB-NM-WEEK.
              10 F PIC X(007) VALUE "Sabado ".
              10 F PIC X(007) VALUE "Domingo".
              10 F PIC X(007) VALUE "Segunda".
              10 F PIC X(007) VALUE "Terca  ".
              10 F PIC X(007) VALUE "Quarta".
              10 F PIC X(007) VALUE "Quinta".
              10 F PIC X(007) VALUE "Sexta  ".
           05 R-TAB-NM-WEEK REDEFINES TAB-NM-WEEK.
              10 NM-SABADO        PIC X(007).
              10 NM-WEEK OCCURS 6 PIC X(007).

       LINKAGE SECTION.

         01 WEEK-PARAMETROS.
            05 DD-AMD                        PIC  9(002).
            05 MM-AMD                        PIC  9(002).
            05 AA-AMD                        PIC  9(004).
            05 GRWEEK-DIA                    PIC  9(001).
            05 NOME-WEEK                     PIC  X(007).

       PROCEDURE DIVISION USING WEEK-PARAMETROS.

       000-PROCESSAMENTO.

           MOVE    AA-AMD            TO AA-WEEK
           MOVE    MM-AMD            TO MM-WEEK
           MOVE    DD-AMD            TO DD-WEEK
           IF   MM-WEEK > 12 OR MM-WEEK > 31
                MOVE 01 TO MM-WEEK DD-WEEK
           END-IF
           ADD     INC-WEEK(MM-WEEK) TO DD-WEEK
           ADD     2199              TO GRWEEK-DATA
           COMPUTE V-WEEK-5           = AA-WEEK * 125
           ADD     DM-WEEK           TO V-WEEK-5
           DIVIDE  V-WEEK-3 BY 7 GIVING DD-WEEK
           COMPUTE GRWEEK-DIA           = V-WEEK-3 - (DD-WEEK * 7)
           IF   GRWEEK-DIA = 0
                MOVE NM-SABADO            TO NOME-WEEK
           ELSE
                MOVE NM-WEEK (GRWEEK-DIA) TO NOME-WEEK.

       000-99-FIM. EXIT PROGRAM.

       END PROGRAM GRWEEK1.
