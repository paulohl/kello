       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRWIND.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  18/02/92.
       SECURITY.      *************************************************
                      *                                               *
                      *  Abre/fecha janela free format                *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT WINDOW ASSIGN TO DISK
                  ORGANIZATION  IS RELATIVE
                  RELATIVE KEY  IS RK-WINDOW
                  ACCESS MODE   IS RANDOM
                  FILE STATUS   IS FS-WINDOW.

       DATA DIVISION.
       FILE SECTION.

       FD  WINDOW
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WINDOW.

       01  WINDOW-REG.
           05 WINDOW-DATA              PIC  X(2000).
           05 WINDOW-ATTRIB            PIC  X(2000).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 OPERADOR                PIC  X(030) VALUE SPACES.
           05 TASK                    PIC  9(006) VALUE 0.
           05 PROGRAMA                PIC  X(008) VALUE SPACES.
           05 SET-LOG                 PIC  X(001) VALUE "?".
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 MAXFRAME           COMP PIC  9(003) VALUE 0.
           05 RK-WINDOW          COMP PIC  9(003) VALUE 0.
           05 ER-WINDOW.
              10 FS-WINDOW            PIC  X(002) VALUE "00".
              10 LB-WINDOW            PIC  X(050) VALUE "GRWIND00.TMP".
           05 L                       PIC  9(004) COMP-X VALUE 0.
           05 C                       PIC  9(004) COMP-X VALUE 0.
           05 ATT-T                   PIC  X(001) VALUE X"07".
           05 ATT-B                   PIC  X(001) VALUE X"07".
           05 SOMBRA                  PIC  X(001) VALUE X"08".
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER.
                 15         OCCURS 25.
                   20 BYTE  OCCURS 80 PIC X(001).
              10 ATTRIBUTE-BUFFER.
                 15 OCCURS 25.
                    20 ATRIBUTO OCCURS 80 PIC X(001).
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 2000.
           05 TABELA-MOLDURA.
              10 FILLER PIC X(8) VALUE X"C9CDBBBACCB9C8BC".
              10 FILLER PIC X(8) VALUE X"DAC4BFB3C3B4C0D9".
              10 FILLER PIC X(8) VALUE X"D5CDB8B3C6B5D4BE".
              10 FILLER PIC X(8) VALUE X"D6C4B7BAC7B6D3BD".
              10 FILLER PIC X(8) VALUE ALL X"B0".
              10 FILLER PIC X(8) VALUE ALL X"B1".
              10 FILLER PIC X(8) VALUE ALL X"B2".
              10 FILLER PIC X(8) VALUE "---|----".
              10 FILLER PIC X(8) VALUE "        ".
           05 FILLER REDEFINES TABELA-MOLDURA.
              10 BASE-MOLDURA PIC X(8) OCCURS 9.
           05 MOLDURA                              VALUE SPACES.
              10 M-201                 PIC  X(001).
              10 M-205                 PIC  X(001).
              10 M-187                 PIC  X(001).
              10 M-186                 PIC  X(001).
              10 M-204                 PIC  X(001).
              10 M-185                 PIC  X(001).
              10 M-200                 PIC  X(001).
              10 M-188                 PIC  X(001).
           05 MOLDURA-DEFAULT          PIC  9(001) VALUE 0.

       LINKAGE SECTION.

       01  PARAMETROS-GRWIND.
           05 GRWIND-LINE                PIC  9(002).
           05 GRWIND-COLUMN              PIC  9(002).
           05 GRWIND-TYPE                PIC  9(001).
           05 GRWIND-VERTICAL-LENGTH     PIC  9(002).
           05 GRWIND-HORIZONTAL-LENGTH   PIC  9(002).
           05 GRWIND-COLOR-SCREEN        PIC  X(001).
           05 GRWIND-COLOR-FRAME         PIC  X(001).
           05 GRWIND-COLOR-SHADE         PIC  X(001).
           05 GRWIND-FUNCTION            PIC  X(005).
              88 ABRE      VALUE "OPEN".
              88 FECHA     VALUE "CLOSE" "POPUP".
              88 NAO-APAGA VALUE "POPUP".

       PROCEDURE DIVISION USING PARAMETROS-GRWIND.

       INICIO.

           ON 1
              MOVE "m"                 TO SET-LOG
              CALL "GRFINA"         USING OPERADOR TASK PROGRAMA SET-LOG
              MOVE TASK                TO MOLDURA-DEFAULT.

           IF   GRWIND-TYPE = 0
                MOVE MOLDURA-DEFAULT TO GRWIND-TYPE.

           INSPECT GRWIND-FUNCTION CONVERTING MINUSCULAS TO MAIUSCULAS

           IF   ABRE
                IF  (GRWIND-LINE   + GRWIND-VERTICAL-LENGTH) > 25
                OR  (GRWIND-COLUMN + GRWIND-HORIZONTAL-LENGTH) > 80
                    CALL "GREMSG" USING "Erro nos limites da janela    "
                    EXIT PROGRAM
                ELSE
                    MOVE "?" TO SET-LOG
                    CALL  "GRFINA" USING OPERADOR
                                         TASK
                                         PROGRAMA
                                         SET-LOG
                    MOVE TASK (5: 2) TO LB-WINDOW (7: 2).

           IF   FECHA
                IF   RK-WINDOW = 0
                     CALL "GREMSG"
                     USING "Erro de l¢gica usando a GRWIND"
                ELSE
                     PERFORM CLOSE-WINDOW
           ELSE
                IF   ABRE
                     PERFORM OPEN-WINDOW
                ELSE
                     CALL "GREMSG"
                     USING "Erro de sintaxe no uso GRWIND ".

           IF   GRWIND-LINE - 1 > 3
           OR  (GRWIND-COLUMN + GRWIND-HORIZONTAL-LENGTH + 2) < 67
               CALL "GRHORA"
           END-IF

           EXIT PROGRAM.

       OPEN-WINDOW.

           IF   GRWIND-COLOR-SCREEN NOT = SPACE
                MOVE GRWIND-COLOR-SCREEN TO ATT-T.

           IF   GRWIND-COLOR-FRAME NOT = SPACE
                MOVE GRWIND-COLOR-FRAME TO ATT-B.

           IF   GRWIND-COLOR-SHADE NOT = SPACE
                MOVE GRWIND-COLOR-SHADE TO SOMBRA.

           MOVE BASE-MOLDURA (GRWIND-TYPE + 1) TO MOLDURA

           IF   GRWIND-LINE = 0
           OR   GRWIND-LINE NOT NUMERIC
                MOVE 1 TO GRWIND-LINE.

           IF   GRWIND-COLUMN = 0
           OR   GRWIND-COLUMN NOT NUMERIC
                MOVE 1 TO GRWIND-COLUMN.

           IF   GRWIND-VERTICAL-LENGTH = 0
           OR   GRWIND-VERTICAL-LENGTH NOT NUMERIC
                MOVE 1 TO GRWIND-VERTICAL-LENGTH.

           IF   GRWIND-HORIZONTAL-LENGTH = 0
           OR   GRWIND-HORIZONTAL-LENGTH NOT NUMERIC
                MOVE 1 TO GRWIND-HORIZONTAL-LENGTH.

           CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                             CARACTER-BUFFER
                                             ATTRIBUTE-BUFFER
                                             STRING-LENGTH

           ADD  1                TO RK-WINDOW
           MOVE CARACTER-BUFFER  TO WINDOW-DATA
           MOVE ATTRIBUTE-BUFFER TO WINDOW-ATTRIB

           MOVE M-201         TO BYTE     (GRWIND-LINE GRWIND-COLUMN)
           MOVE ATT-B         TO ATRIBUTO (GRWIND-LINE GRWIND-COLUMN)
           MOVE GRWIND-COLUMN TO C

           PERFORM GRWIND-HORIZONTAL-LENGTH TIMES
                   ADD 1      TO C
                   MOVE M-205 TO BYTE     (GRWIND-LINE C)
                   MOVE ATT-B TO ATRIBUTO (GRWIND-LINE C)
           END-PERFORM

           ADD 1      TO C
           MOVE M-187 TO BYTE     (GRWIND-LINE C)
           MOVE ATT-B TO ATRIBUTO (GRWIND-LINE C)

           MOVE GRWIND-LINE  TO L

           PERFORM GRWIND-VERTICAL-LENGTH TIMES
                   MOVE GRWIND-COLUMN TO C
                   ADD  1             TO L
                   MOVE M-186         TO BYTE     (L C)
                   MOVE ATT-B         TO ATRIBUTO (L C)
                   PERFORM GRWIND-HORIZONTAL-LENGTH TIMES
                           ADD  1     TO C
                           MOVE SPACE TO BYTE     (L C)
                           MOVE ATT-T TO ATRIBUTO (L C)
                   END-PERFORM
                   ADD 1       TO C
                   MOVE M-186  TO BYTE     (L C)
                   MOVE ATT-B  TO ATRIBUTO (L C)
                   IF   C < 79
                        MOVE SOMBRA TO ATRIBUTO (L C + 1)
                        MOVE SOMBRA TO ATRIBUTO (L C + 2)
                   END-IF
           END-PERFORM

           MOVE GRWIND-COLUMN TO C
           ADD  1             TO L
           MOVE M-200         TO BYTE     (L C)
           MOVE ATT-B         TO ATRIBUTO (L C)

           PERFORM GRWIND-HORIZONTAL-LENGTH TIMES
                   ADD 1       TO C
                   MOVE M-205  TO BYTE     (L C)
                   MOVE ATT-B  TO ATRIBUTO (L C)
                   IF   (L + 1) < 25
                   AND  C < 81
                        MOVE SOMBRA TO ATRIBUTO (L + 1 C)
                   END-IF
           END-PERFORM

           ADD 1       TO C
           MOVE M-188  TO BYTE     (L C)
           MOVE ATT-B  TO ATRIBUTO (L C)

           IF   L < 25
                MOVE SOMBRA TO ATRIBUTO (L + 1 C).

           ADD  1      TO C
           IF   L < 25
           AND  C < 80
                MOVE SOMBRA TO ATRIBUTO (L C)
                MOVE SOMBRA TO ATRIBUTO (L C + 1)
                MOVE SOMBRA TO ATRIBUTO (L + 1 C)
                MOVE SOMBRA TO ATRIBUTO (L + 1 C + 1).

           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                              CARACTER-BUFFER
                                              ATTRIBUTE-BUFFER
                                              STRING-LENGTH

           IF   RK-WINDOW = 1
                OPEN OUTPUT WINDOW
           ELSE
                OPEN I-O WINDOW.

           IF   RK-WINDOW > MAXFRAME
                MOVE RK-WINDOW TO MAXFRAME
                WRITE WINDOW-REG
           ELSE
                REWRITE WINDOW-REG.

           CLOSE WINDOW.

       CLOSE-WINDOW.

           IF   NAO-APAGA
                CONTINUE
           ELSE
                OPEN INPUT WINDOW
                READ WINDOW
                MOVE WINDOW-DATA                TO CARACTER-BUFFER
                MOVE WINDOW-ATTRIB              TO ATTRIBUTE-BUFFER
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                   CARACTER-BUFFER
                                                   ATTRIBUTE-BUFFER
                                                   STRING-LENGTH
                CLOSE WINDOW.

           SUBTRACT 1 FROM RK-WINDOW

           IF   RK-WINDOW = 0
                MOVE 0 TO MAXFRAME
                CALL "GRMATA" USING ER-WINDOW.

       END PROGRAM GRWIND.
