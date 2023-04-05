       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRFINA.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/04/89.
       SECURITY.      *************************************************
                      *                                               *
                      *   Encerramento de sistemas                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT USELOG ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-USELOG.

       DATA DIVISION.
       FILE SECTION.

       FD  USELOG
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-USELOG.

       01  USELOG-REG.
           05 USELOG-DATA-DE-HOJE       PIC  X(009).
           05 USELOG-HORA               PIC  X(009).
           05 USELOG-PROGRAMA           PIC  X(008)B.
           05 USELOG-PARTE.
              10 USELOG-FUNCAO-PROGRAMA PIC  X(035).
              10 USELOG-OPERADOR        PIC  X(030).
           05 FILLER REDEFINES USELOG-PARTE.
              10 USELOG-FS              PIC  X(003).
              10 USELOG-MENSAGEM        PIC  X(046).
           05 USELOG-TASK               PIC  9(006).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 MSG                      PIC  X(080) VALUE SPACES.
           05 DATA-DE-HOJE             PIC  X(008) VALUE SPACES.
           05 HORA                     PIC  X(008) VALUE SPACES.
           05 ER-USELOG.
              10 FS-USELOG             PIC  X(002) VALUE "00".
              10 LB-USELOG.
                 15 FILLER             PIC  X(002) VALUE "GR".
                 15 ANO-USELOG         PIC  9(002) VALUE ZERO.
                 15 MES-USELOG         PIC  9(002) VALUE ZERO.
                 15 FILLER             PIC  X(004) VALUE ".LOG".
                 15 FILLER             PIC  X(040) VALUE SPACES.
           05 MOLDURA                  PIC  9(001).
           05 USUARIO                  PIC  X(030).
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008).
           05 LOG                      PIC  X(001).
              88 LOG-ON                            VALUE "1".
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

       COPY CPREFE.

       LINKAGE SECTION.

       01  USUARIO-L            PIC   X(030).
       01  TASK-L               PIC   9(006).
       01  PROGRAMA-L           PIC   X(008).
       01  GRMENU               PIC   X(001).

       PROCEDURE DIVISION USING USUARIO-L TASK-L PROGRAMA-L GRMENU.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF   X91-PARAMETER = 0
                GO TO 000-STOPRUN.

           IF   GRMENU = "M"
                MOVE TASK-L TO MOLDURA
                MOVE SPACE  TO GRMENU
                EXIT PROGRAM.

           IF   GRMENU = "m"
                MOVE MOLDURA TO TASK-L
                MOVE SPACE   TO GRMENU
                EXIT PROGRAM.

           IF   GRMENU = "9"
                MOVE SPACE TO GRMENU
                EXIT PROGRAM.

           IF   GRMENU EQUAL "1" OR "0" OR "2"
                IF   GRMENU NOT = "2"
                     MOVE GRMENU    TO LOG
                     MOVE USUARIO-L TO USUARIO
                ELSE
                     MOVE USUARIO-L TO MSG
                END-IF
                MOVE "N"        TO GRMENU
                MOVE TASK-L     TO TASK
                MOVE PROGRAMA-L TO PROGRAMA
                EXIT PROGRAM
           ELSE
                IF   GRMENU EQUAL "?"
                     MOVE "N"        TO GRMENU
                     MOVE USUARIO    TO USUARIO-L
                     MOVE TASK       TO TASK-L
                     MOVE PROGRAMA   TO PROGRAMA-L
                     EXIT PROGRAM.

       000-STOPRUN.

           IF   TASK = 0
           OR   TASK NOT NUMERIC
                STOP RUN.

           IF   LOG-ON
                CALL  "GRREFE"      USING PARAMETROS-GRREFE
                MOVE   GRREFE-MM       TO MES-USELOG
                MOVE   GRREFE-AA       TO ANO-USELOG
                OPEN  EXTEND USELOG
                MOVE  USUARIO          TO USELOG-OPERADOR
                MOVE  TASK             TO USELOG-TASK
                IF   FS-USELOG EQUAL "00"
                     CALL "GRDATA"     USING DATA-DE-HOJE HORA
                     MOVE DATA-DE-HOJE    TO USELOG-DATA-DE-HOJE
                     MOVE HORA            TO USELOG-HORA
                     MOVE PROGRAMA        TO USELOG-PROGRAMA
                     IF   MSG = SPACES
                          MOVE "Sistema encerrado"
                                   TO USELOG-FUNCAO-PROGRAMA
                     ELSE
                          MOVE MSG TO USELOG-FUNCAO-PROGRAMA
                     END-IF
                     WRITE USELOG-REG
                     CLOSE USELOG.

           DISPLAY (1, 1) ERASE.

       000-99-FIM. STOP RUN.

       END PROGRAM GRFINA.
