       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRHORA.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/03/88.
       SECURITY.      *************************************************
                      *                                               *
                      *   Atualiza data e hora no video               *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 DD-W                     PIC  9(002) VALUE ZEROS.
           05 HOJE.
              10 DD-H-1                PIC  9(002) VALUE ZEROS.
              10 MM-H-1                PIC  9(002) VALUE ZEROS.
              10 AA-H-1                PIC  9(002) VALUE ZEROS.
           05 DATA-DE-HOJE.
              10 WEEK                  PIC  X(003) VALUE SPACES.
              10 FILLER                PIC  X(001) VALUE SPACES.
              10 DD-H-2                PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE "/".
              10 MM-H-2                PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE "/".
              10 AA-H-2                PIC  9(002) VALUE ZEROS.
           05 TEMPO.
              10 HH-1                  PIC  9(002) VALUE ZEROS.
              10 MM-1                  PIC  9(002) VALUE ZEROS.
              10 SS-1                  PIC  9(002) VALUE ZEROS.
              10 DD-1                  PIC  9(002) VALUE ZEROS.
           05 HORA.
              10 FILLER                PIC  X(004) VALUE SPACES.
              10 HH-2                  PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE ":".
              10 MM-2                  PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE ":".
              10 SS-2                  PIC  9(002) VALUE ZEROS.
           05 SALVA-CURSOR             PIC  X(002) VALUE SPACES.
           05 SCREEN-POSITION.
              10 ROW-NUMBER            PIC  9(002) COMP-X VALUE 0.
              10 COLUMN-NUMBER         PIC  9(002) COMP-X VALUE 0.

       COPY CPWEEK.

       PROCEDURE DIVISION.

       000-INICIO.

           ACCEPT HOJE    FROM DATE
           MOVE DD-H-1      TO DD-W
           MOVE AA-H-1      TO DD-H-1
           MOVE DD-W        TO AA-H-1
           ACCEPT TEMPO   FROM TIME
           IF   SS-1 NOT EQUAL SS-2
           OR   MM-1 NOT EQUAL MM-2
           OR   HH-1 NOT EQUAL HH-2
                MOVE HH-1 TO HH-2
                MOVE MM-1 TO MM-2
                MOVE SS-1 TO SS-2
                IF   DD-H-1 NOT EQUAL DD-H-2
                     MOVE   DD-H-1  TO DD-H-2
                     MOVE   MM-H-1  TO MM-H-2
                     MOVE   AA-H-1  TO AA-H-2
                     PERFORM 010-SEMANA THRU 010-99-FIM
                END-IF
                CALL "CBL_GET_CSR_POS" USING SCREEN-POSITION
                MOVE SCREEN-POSITION       TO SALVA-CURSOR
                MOVE 255                   TO ROW-NUMBER
                                             COLUMN-NUMBER
                CALL "CBL_SET_CSR_POS" USING SCREEN-POSITION
      *         DISPLAY DATA-DE-HOJE LINE 2 COLUMN 67
      *                 HORA         LINE 3 COLUMN 67
                MOVE SALVA-CURSOR         TO SCREEN-POSITION
                CALL "CBL_SET_CSR_POS" USING SCREEN-POSITION.

       000-99-FIM. EXIT PROGRAM.

       010-SEMANA.

           MOVE HOJE         TO GRWEEK-DATA
           CALL "GRWEEK"  USING PARAMETROS-GRWEEK
           MOVE GRWEEK-EXTENSO TO WEEK.

       010-99-FIM. EXIT.

       END PROGRAM GRHORA.
