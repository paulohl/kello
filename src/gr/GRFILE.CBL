       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRFILE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/02/90.
       SECURITY.      *************************************************
                      *                                               *
                      *   Fornece label do arquivo                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CONFIG ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CONFIG-CHAVE
                  FILE STATUS   IS FS-CONFIG.

       DATA DIVISION.
       FILE SECTION.

       COPY CPCONF.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 T                   COMP PIC  9(002) VALUE 0.
           05 I                   COMP PIC  9(002) VALUE 0.
           05 OPERADOR                 PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 GRMENU                   PIC  X(001) VALUE "?".
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 ER-CONFIG.
              10 FS-CONFIG             PIC  X(002) VALUE "00".
              10 LB-CONFIG             PIC  X(050) VALUE "CONFIG.CWS".

       LINKAGE SECTION.

       01  ARQUIVO PIC X(50).

       PROCEDURE DIVISION USING ARQUIVO.

       000-INICIO.

           ON   1
                CALL  "GRFINA" USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     GRMENU.

           INSPECT ARQUIVO CONVERTING MINUSCULAS TO MAIUSCULAS

           OPEN INPUT CONFIG

           IF   FS-CONFIG > "09"
                CALL "GRISAM" USING ER-CONFIG
                EXIT PROGRAM.

           PERFORM TEST AFTER UNTIL FS-CONFIG NOT = "9D"
                   MOVE "02"        TO CONFIG-REG02
                   MOVE ARQUIVO     TO CONFIG-ARQUIVO
                   READ CONFIG
                   IF   FS-CONFIG = "9D"
                        CALL "GRISAM" USING ER-CONFIG
                   END-IF
           END-PERFORM

           MOVE 6 TO T
           PERFORM VARYING I FROM 50 BY -1 UNTIL I = 1
                                              OR T = 0
                   IF   CONFIG-LABEL-BYTE (I) = "#" OR "$"
                        MOVE TASK (T: 1) TO CONFIG-LABEL-BYTE (I)
                        SUBTRACT 1 FROM T
                   END-IF
           END-PERFORM

           IF   FS-CONFIG EQUAL "00"
                MOVE    CONFIG-LABEL             TO ARQUIVO
                INSPECT ARQUIVO CONVERTING MINUSCULAS TO MAIUSCULAS
           ELSE
                MOVE ARQUIVO     TO LB-CONFIG (12: )
                CALL "GRISAM" USING ER-CONFIG
                MOVE SPACE       TO LB-CONFIG (12: ).

           CLOSE CONFIG

           EXIT PROGRAM.

       END PROGRAM GRFILE.

