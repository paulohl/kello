       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRLINE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  20/06/96.
       SECURITY.      *************************************************
                      *                                               *
                      *  Menu em linha/buttons                        *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 LX                      PIC  9(004) COMP-X.
           05 CX                      PIC  9(004) COMP-X.
           05 FLAG                    PIC  9(002) COMP-X VALUE 0.
           05 ACESO                   PIC  9(002) COMP-X VALUE 0.
           05 SAVE-ATTRIBUTE-BUFFER2  PIC  X(080) VALUE SPACES.
           05 START-STRING            PIC  9(002) COMP-X VALUE 0.
           05 TIPO                    PIC  9(001) VALUE 0.

           05 I                       PIC  9(004) COMP-X.
           05 ATTR-LOW                PIC  X(040) VALUE SPACES.
           05 ATTR-HIGH               PIC  X(040) VALUE SPACES.
           05 OPT-B                   PIC  9(003) VALUE 0.
           05 OP                      PIC  9(003) VALUE 0.
           05 BUTTON-TYPE             PIC  X(001) VALUE SPACES.
           05 TEXTO                   PIC  X(040) VALUE SPACES.
           05 TEXTO-TOP               PIC  X(040) VALUE SPACES.
           05 TEXTO-MID               PIC  X(040) VALUE SPACES.
           05 TEXTO-BOT               PIC  X(040) VALUE SPACES.
           05 ATTRIB-TOP              PIC  X(040) VALUE SPACES.
           05 ATTRIB-MID              PIC  X(040) VALUE SPACES.
           05 ATTRIB-BOT              PIC  X(040) VALUE SPACES.
           05 VEZ                     PIC  9(001) VALUE 1.
           05 TAMANHO                 PIC  9(002) VALUE 0.
           05 MOUSE-LIN               PIC  9(002) VALUE 0.
           05 MOUSE-COL               PIC  9(002) VALUE 0.
           05 TABELA.
              10 LINHA                PIC  9(002) VALUE 0.
              10 COLUNA               PIC  9(002) VALUE 0.
              10 LIMITE               PIC  9(003) VALUE 0.
              10 OPCOES               PIC  9(002) VALUE 0.
              10 OPCAO-NUM            PIC  9(002) VALUE 0.
              10 OPCAO OCCURS 26.
                 15 OPCAO-TXT.
                    20 BYTE-TXT OCCURS 38
                                       PIC X(001).
                 15 POS.
                    20 POS-LIN        PIC 9(02).
                    20 POS-COL        PIC 9(02).
                 15 LENG              PIC 9(02).
              10 OPCOES-CHAR                             VALUE SPACES.
                 15 CHAR OCCURS 26    PIC  X(001).
              10 REDEFINES OPCOES-CHAR.
                 15 CHAR-X OCCURS 26  PIC  9(002) COMP-X.
           05 KEY-STATUS              PIC  9(002) COMP-X VALUE 0.
           05 MOUSE-READ-TYPE         PIC  9(002) COMP-X VALUE 0.
           05 MOUSE-HANDLE            PIC  9(008) COMP-X VALUE 1.
           05 MOUSE-BUTTONS           PIC  9(002) COMP-X VALUE 3.
           05 MOUSE-POSITION.
              10 ROW-MOUSE            PIC  9(004) COMP-X VALUE 0.
              10 COLUMN-MOUSE         PIC  9(004) COMP-X VALUE 0.
           05 MOUSE-DATA.
              10 MOUSE-EVENT-TYPE     PIC  9(004) COMP-X.
              10 MOUSE-EVENT-TIME     PIC  9(008) COMP-X.
              10 MOUSE-EVENT-ROW      PIC  9(004) COMP-X.
              10 MOUSE-EVENT-COL      PIC  9(004) COMP-X.
           05 CBL-READ-WRITE-SCR-ATTRS.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 ATTRIBUTE-BUFFER     PIC  X(001) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 0.
              10 SALVA-ATTRIBUTE      PIC  X(001) VALUE SPACES.
              10 SCREEN-POSITION2.
                 15 ROW-NUMBER2       PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER2    PIC  9(002) COMP-X VALUE 0.
              10 ATTRIBUTE-BUFFER2    PIC  X(080) VALUE SPACES.
              10 STRING-LENGTH2       PIC  9(004) COMP-X VALUE 0.
              10 SALVA-ATTRIBUTE2     PIC  X(001) VALUE SPACES.
           05 CARACTER                PIC  X(001).
           05 CARACTER-X REDEFINES CARACTER
                                      PIC  9(002) COMP-X.
           05 TECLA                   PIC  9(003). COPY CPEDIT.
           05 MATRIZ-CONTROL.
              10 MATRIZ-LINE OCCURS 25.
                 15 MATRIZ-POSIT OCCURS 80 PIC  9(002) COMP-X.
              10 MATRIZ-KEY                PIC  9(002) COMP-X.
           05 SALVA-CURSOR.
              15                      PIC  9(002) COMP-X VALUE 0.
              15                      PIC  9(002) COMP-X VALUE 0.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

       LINKAGE SECTION.

       01  PARAMETROS-GRLINE.
           05 GRLINE-LINE                    PIC  9(002).
           05 GRLINE-COLUMN                  PIC  9(002).
           05 GRLINE-LINE-END                PIC  9(002).
           05 GRLINE-COLUMN-END              PIC  9(002).
           05 GRLINE-TYPE                    PIC  X(001).
              88 GRLINE-BUTTONS                           VALUE "B" "b".
              88 GRLINE-REVERSED                          VALUE "R" "r".
           05 GRLINE-SCREENS.
              10 GRLINE-CHAR   OCCURS 26     PIC  X(001).
              10 GRLINE-SCREEN OCCURS 26     PIC  X(034).
           05 GRLINE-POSITION  OCCURS 26.
              15 GRLINE-POSITION-LINE        PIC  9(002).
              15 GRLINE-POSITION-COLUMN      PIC  9(002).
           05 GRLINE-COLOR-LOW               PIC  X(001).
           05 GRLINE-COLOR-HIGH              PIC  X(001).
           05 GRLINE-OPTION                  PIC  9(003).
           05 GRLINE-OPTION-CHAR             PIC  X(001).
           05 GRLINE-MOUSE                   PIC  X(001).

       01  PARAMETROS-GRMOUS.
           05 GRMOUS-CONTROL.
              10 GRMOUS-LINE OCCURS 25.
                 15 GRMOUS-POSIT OCCURS 80 PIC  9(002) COMP-X.
              10 GRMOUS-MODE               PIC  9(002) COMP-X.
              10 GRMOUS-KEY                PIC  9(002) COMP-X.
              10 GRMOUS-BUTTON             PIC  9(002) COMP-X.
           05 GRMOUS-CURSOR-POSITION.
              10 GRMOUS-CURSOR-LIN         PIC  9(002).
              10 GRMOUS-CURSOR-COL         PIC  9(002).

       PROCEDURE DIVISION USING PARAMETROS-GRLINE PARAMETROS-GRMOUS.

       000-INICIO.

           CALL "CBL_GET_CSR_POS" USING SALVA-CURSOR
           IF   VEZ = 1
                CALL "CBL_INIT_MOUSE"         USING MOUSE-HANDLE
                                                    MOUSE-BUTTONS
                MOVE 2 TO VEZ
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 40
                        MOVE GRLINE-COLOR-LOW  TO ATTR-LOW  (I: 1)
                        MOVE GRLINE-COLOR-HIGH TO ATTR-HIGH (I: 1)
                END-PERFORM
           END-IF
           INITIALIZE            TABELA
           MOVE GRLINE-LINE   TO LINHA  ROW-NUMBER
           MOVE GRLINE-COLUMN TO COLUNA COLUMN-NUMBER
           IF   GRLINE-BUTTONS
                ADD  1 TO LINHA ROW-NUMBER
           END-IF
           SUBTRACT 1                     FROM ROW-NUMBER COLUMN-NUMBER
           SUBTRACT 2                     FROM COLUMN-NUMBER
           MOVE 1                           TO STRING-LENGTH
           CALL "CBL_READ_SCR_ATTRS"     USING SCREEN-POSITION
                                               SALVA-ATTRIBUTE
                                               STRING-LENGTH

           PERFORM VARYING OPCAO-NUM FROM 1 BY 1
                           UNTIL OPCAO-NUM > 26
                              OR LINHA > GRLINE-LINE-END
                   IF   GRLINE-SCREEN (OPCAO-NUM) NOT = SPACES
                        IF   NOT GRLINE-BUTTONS
                             INSPECT GRLINE-SCREEN (OPCAO-NUM)
                                     CONVERTING "_" TO " "
                        END-IF
                        ADD  1 TO OPCOES
                        MOVE GRLINE-SCREEN(OPCAO-NUM)
                          TO OPCAO-TXT(OPCOES)
                        MOVE GRLINE-CHAR (OPCAO-NUM) TO CHAR (OPCOES)
                        IF   CHAR-X (OPCOES) > 96 AND < 123
                             SUBTRACT 32 FROM CHAR-X (OPCOES)
                        END-IF
                        PERFORM VARYING TAMANHO FROM 34 BY -1
                                UNTIL BYTE-TXT (OPCOES TAMANHO)
                                      NOT = SPACE
                                      CONTINUE
                        END-PERFORM
                        IF   GRLINE-BUTTONS
                             ADD 3 TO TAMANHO
                        END-IF
                        COMPUTE LENG (OPCOES) = TAMANHO + 1
                        MOVE LINHA  TO POS-LIN   (OPCOES)
                        MOVE COLUNA TO POS-COL   (OPCOES)
                        COMPUTE LIMITE = POS-COL (OPCOES)
                                       + LENG    (OPCOES)
                                       + 1
                        IF   LIMITE > GRLINE-COLUMN-END
                             ADD  1             TO LINHA
                                                   POS-LIN (OPCOES)
                             IF   GRLINE-BUTTONS
                                  ADD  3        TO LINHA
                                                   POS-LIN (OPCOES)
                             END-IF
                             MOVE GRLINE-COLUMN TO COLUNA
                                                   POS-COL (OPCOES)
                             COMPUTE LIMITE = LENG (OPCOES)
                                            + GRLINE-COLUMN + 1
                        END-IF
                        COMPUTE COLUNA = LIMITE + 1
                        IF   LINHA > GRLINE-LINE-END
                             SUBTRACT 1 FROM OPCOES
                        END-IF
                        INSPECT GRLINE-SCREEN (OPCAO-NUM)
                                CONVERTING "_" TO " "
                   END-IF
           END-PERFORM
           MOVE LOW-VALUES TO MATRIZ-CONTROL
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER = 2
                MOVE 0 TO GRMOUS-KEY
           END-IF
           MOVE 0          TO OPCAO-NUM
           PERFORM OPCOES TIMES
                   ADD 1 TO OPCAO-NUM
                   IF   GRLINE-BUTTONS
                        MOVE OPCAO-NUM TO OPT-B
                        MOVE "D"       TO BUTTON-TYPE
                        PERFORM EXIBE-BUTTON
                   ELSE
                        DISPLAY OPCAO-TXT (OPCAO-NUM)
                                LINE   POS-LIN   (OPCAO-NUM)
                                COLUMN POS-COL   (OPCAO-NUM)
                                WITH SIZE LENG   (OPCAO-NUM)
                        COMPUTE ROW-NUMBER    = POS-LIN
                                                (OPCAO-NUM) - 1
                        COMPUTE COLUMN-NUMBER = POS-COL
                                                (OPCAO-NUM) - 2
                        COMPUTE STRING-LENGTH = LENG(OPCAO-NUM)
                                                + 1
                        CALL "CBL_WRITE_SCR_N_ATTR" USING
                                               SCREEN-POSITION
                                               SALVA-ATTRIBUTE
                                               STRING-LENGTH
                   END-IF
                   MOVE POS-LIN (OPCAO-NUM)
                     TO GRLINE-POSITION-LINE   (OPCAO-NUM)
                   MOVE POS-COL (OPCAO-NUM)
                     TO GRLINE-POSITION-COLUMN (OPCAO-NUM)
                   MOVE POS-LIN (OPCAO-NUM) TO MOUSE-LIN
                   COMPUTE MOUSE-COL = POS-COL (OPCAO-NUM)
                                     - 1
                   COMPUTE LIMITE = MOUSE-COL + LENG (OPCAO-NUM)
                   PERFORM VARYING MOUSE-COL
                      FROM MOUSE-COL BY 1
                     UNTIL MOUSE-COL > LIMITE
                           MOVE OPCAO-NUM TO MATRIZ-POSIT
                                             (MOUSE-LIN MOUSE-COL)
                           IF   X91-PARAMETER = 2
                                COMPUTE GRMOUS-POSIT
                                        (MOUSE-LIN MOUSE-COL) =
                                MATRIZ-POSIT
                                        (MOUSE-LIN MOUSE-COL) + 200
                           END-IF
                   END-PERFORM
           END-PERFORM

           IF   GRLINE-OPTION > 300
                IF   GRLINE-BUTTONS
                     COMPUTE OPT-B = GRLINE-OPTION - 300
                     MOVE "P"       TO BUTTON-TYPE
                     PERFORM EXIBE-BUTTON
                END-IF
                SUBTRACT 100 FROM GRLINE-OPTION
                GO TO 000-99-FIM
           END-IF
           CALL "CBL_SET_CSR_POS" USING X"FFFF"
           IF   GRLINE-OPTION = 0
           OR   GRLINE-OPTION > OPCOES
                MOVE 1             TO OPCAO-NUM MATRIZ-KEY
           ELSE
                MOVE GRLINE-OPTION TO OPCAO-NUM MATRIZ-KEY
           END-IF
           IF   GRLINE-BUTTONS
                COMPUTE ROW-NUMBER    = POS-LIN
                                        (OPCAO-NUM) - 1
                COMPUTE COLUMN-NUMBER = POS-COL
                                        (OPCAO-NUM)
                COMPUTE STRING-LENGTH = LENG(OPCAO-NUM) - 2
                CALL "CBL_WRITE_SCR_N_ATTR" USING
                                            SCREEN-POSITION
                                            ATTR-HIGH
                                            STRING-LENGTH
           ELSE
                COMPUTE ROW-NUMBER    = POS-LIN
                                        (OPCAO-NUM) - 1
                COMPUTE COLUMN-NUMBER = POS-COL
                                        (OPCAO-NUM) - 2
                COMPUTE STRING-LENGTH = LENG(OPCAO-NUM)
                                        + 1
                CALL "CBL_WRITE_SCR_N_ATTR" USING
                                            SCREEN-POSITION
                                            ATTR-LOW
                                            STRING-LENGTH
           END-IF

           MOVE 0 TO TECLA CARACTER
           PERFORM TEST AFTER UNTIL EDIT-ENTER
                                 OR EDIT-ESC
              INITIALIZE MOUSE-POSITION
              PERFORM TEST AFTER UNTIL KEY-STATUS   > 0
                                    OR ROW-MOUSE    > 0
                                    OR COLUMN-MOUSE > 0
                      CALL "GRHORA"
                      CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                      CALL "CBL_GET_MOUSE_POSITION" USING MOUSE-HANDLE
                                                          MOUSE-POSITION
                                                RETURNING RETURN-CODE
                      ON   EXCEPTION
                           CONTINUE
                      END-CALL
                      IF  ROW-MOUSE > 25
                          INITIALIZE MOUSE-POSITION
                      END-IF
              END-PERFORM
              INITIALIZE MOUSE-DATA
              CALL "CBL_READ_MOUSE_EVENT" USING MOUSE-HANDLE
                                                MOUSE-DATA
                                                MOUSE-READ-TYPE
                                      RETURNING RETURN-CODE
              ON   EXCEPTION
                   CONTINUE
              END-CALL
              ADD 1 TO ROW-MOUSE COLUMN-MOUSE
              MOVE 0 TO TIPO
              EVALUATE TRUE
                  WHEN (MATRIZ-POSIT (ROW-MOUSE COLUMN-MOUSE) NOT = 0)
                   AND KEY-STATUS = 0
                       PERFORM APAGAR
                       MOVE MATRIZ-POSIT (ROW-MOUSE COLUMN-MOUSE)
                            TO MATRIZ-KEY
                       IF   MOUSE-EVENT-TYPE > 1
                            IF   MOUSE-EVENT-TYPE = 2 OR 3
                                 SET EDIT-ENTER  TO TRUE
                                 MOVE "P"        TO BUTTON-TYPE
                                 IF  GRLINE-BUTTONS
                                     MOVE MATRIZ-KEY TO OPT-B
                                     PERFORM EXIBE-BUTTON
                                 END-IF
                            ELSE
                                 SET EDIT-ESC TO TRUE
                            END-IF
                            MOVE 0 TO MOUSE-EVENT-TYPE
                       END-IF
                       MOVE 2 TO TIPO
                  WHEN X91-PARAMETER = 2
                   AND KEY-STATUS = 0
                       PERFORM SET-REVERSO THRU FIM-SET-REVERSO
                       MOVE 3 TO TIPO
                  WHEN KEY-STATUS = 1
                       MOVE 0 TO TECLA CARACTER
                       PERFORM APAGAR
                       CALL "CBL_READ_KBD_CHAR" USING CARACTER
                       EVALUATE CARACTER
                         WHEN X"0D"
                            MOVE SPACE TO CARACTER
                            SET EDIT-ENTER TO TRUE
                            IF  GRLINE-BUTTONS
                                MOVE MATRIZ-KEY TO OPT-B
                                MOVE "P"        TO BUTTON-TYPE
                                PERFORM EXIBE-BUTTON
                            END-IF
                         WHEN X"1B"
                            MOVE SPACE TO CARACTER
                            SET EDIT-ESC TO TRUE
                         WHEN X"00"
                            CALL "CBL_READ_KBD_CHAR" USING CARACTER
                            MOVE CARACTER-X TO TECLA
                            MOVE SPACE      TO CARACTER
                         WHEN OTHER
                             IF   CARACTER-X > 96 AND < 123
                                  SUBTRACT 32 FROM CARACTER-X
                             ELSE
                                  MOVE CARACTER-X TO TECLA
                             END-IF
                             IF CARACTER NOT = SPACE
                                PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > 26
                                   IF CARACTER = CHAR (I)
                                      MOVE I  TO MATRIZ-KEY
                                      SET EDIT-ENTER TO TRUE
                                      MOVE 27 TO I
                                      PERFORM CHECK-POSITION
                                      IF  GRLINE-BUTTONS
                                          MOVE MATRIZ-KEY TO OPT-B
                                          MOVE "P"        TO BUTTON-TYPE
                                          PERFORM EXIBE-BUTTON
                                      END-IF
                                   END-IF
                                END-PERFORM
                             END-IF
                       END-EVALUATE
                       EVALUATE TRUE
                                WHEN EDIT-ESC OR EDIT-ENTER
                                     MOVE OPCAO-NUM TO MATRIZ-KEY
                                     MOVE 0         TO TIPO
                                     IF   X91-PARAMETER = 2
                                          ADD 200 TO MATRIZ-KEY
                                     END-IF
                                WHEN EDIT-CURSOR-DOWN
                                  OR EDIT-CURSOR-RIGHT
                                  OR EDIT-TAB
                                     COMPUTE MATRIZ-KEY = OPCAO-NUM + 1
                                     IF   MATRIZ-KEY > OPCOES
                                          MOVE 1 TO MATRIZ-KEY
                                     END-IF
                                     MOVE 1 TO TIPO
                                WHEN EDIT-CURSOR-UP
                                  OR EDIT-CURSOR-LEFT
                                  OR EDIT-SHIFT-TAB
                                     COMPUTE MATRIZ-KEY = OPCAO-NUM - 1
                                     IF   MATRIZ-KEY = 0
                                          MOVE OPCOES TO MATRIZ-KEY
                                     END-IF
                                     MOVE 1 TO TIPO
                                WHEN OTHER
                                     IF   X91-PARAMETER = 2
                                          MOVE TECLA TO GRMOUS-KEY
                                          MOVE 0     TO MATRIZ-KEY
                                          SET EDIT-ENTER TO TRUE
                                          IF   NOT GRLINE-BUTTONS
                                          CALL "CBL_WRITE_SCR_N_ATTR"
                                               USING SCREEN-POSITION
                                                      SALVA-ATTRIBUTE
                                                     STRING-LENGTH
                                          END-IF
                                     END-IF
                       END-EVALUATE
              END-EVALUATE
              IF  (OPCAO-NUM NOT = MATRIZ-KEY)
              AND (TIPO = 1 OR 2)
                  PERFORM CHECK-POSITION
              END-IF
           END-PERFORM

           IF   EDIT-ENTER
                MOVE MATRIZ-KEY TO GRLINE-OPTION OP
                IF   OP > 200
                     SUBTRACT 200 FROM OP
                END-IF
                IF   OP NOT = 0
                     MOVE CHAR (OP) TO GRLINE-OPTION-CHAR
                END-IF
                IF   X91-PARAMETER = 2
                     IF   TIPO = 2
                     AND  GRMOUS-KEY = 0
                          MOVE POS-LIN (MATRIZ-KEY) TO ROW-MOUSE
                          MOVE POS-COL (MATRIZ-KEY) TO COLUMN-MOUSE
                          MOVE GRMOUS-POSIT (ROW-MOUSE COLUMN-MOUSE)
                            TO GRMOUS-KEY
                     END-IF
                     IF   GRMOUS-KEY = 0
                          MOVE GRLINE-OPTION TO GRMOUS-KEY
                     END-IF
                END-IF
           ELSE
                MOVE 0          TO GRLINE-OPTION
                IF   X91-PARAMETER = 2
                     MOVE 0 TO GRMOUS-KEY
                END-IF
           END-IF
           MOVE 0 TO ACESO
           CALL "CBL_SET_CSR_POS" USING SALVA-CURSOR.

       000-99-FIM. GOBACK.

       CHECK-POSITION.

           IF   GRLINE-BUTTONS
                COMPUTE ROW-NUMBER    = POS-LIN
                                        (OPCAO-NUM) - 1
                COMPUTE COLUMN-NUMBER = POS-COL
                                        (OPCAO-NUM)
                COMPUTE STRING-LENGTH = LENG(OPCAO-NUM) - 2
                CALL "CBL_WRITE_SCR_N_ATTR" USING
                                            SCREEN-POSITION
                                            ATTR-LOW
                                            STRING-LENGTH
                MOVE MATRIZ-KEY TO OPCAO-NUM
                COMPUTE ROW-NUMBER    = POS-LIN
                                        (OPCAO-NUM) - 1
                COMPUTE COLUMN-NUMBER = POS-COL
                                        (OPCAO-NUM)
                COMPUTE STRING-LENGTH = LENG(OPCAO-NUM) - 2
                CALL "CBL_WRITE_SCR_N_ATTR" USING
                                            SCREEN-POSITION
                                            ATTR-HIGH
                                            STRING-LENGTH
           ELSE
                COMPUTE ROW-NUMBER    = POS-LIN
                                        (OPCAO-NUM) - 1
                COMPUTE COLUMN-NUMBER = POS-COL
                                        (OPCAO-NUM) - 2
                COMPUTE STRING-LENGTH = LENG(OPCAO-NUM) + 1
                CALL "CBL_WRITE_SCR_N_ATTR" USING
                                            SCREEN-POSITION
                                            SALVA-ATTRIBUTE
                                            STRING-LENGTH
                MOVE MATRIZ-KEY TO OPCAO-NUM
                COMPUTE ROW-NUMBER    = POS-LIN
                                        (OPCAO-NUM) - 1
                COMPUTE COLUMN-NUMBER = POS-COL
                                        (OPCAO-NUM) - 2
                COMPUTE STRING-LENGTH = LENG(OPCAO-NUM) + 1
                CALL "CBL_WRITE_SCR_N_ATTR" USING
                                            SCREEN-POSITION
                                            ATTR-LOW
                                            STRING-LENGTH
           END-IF.

       FIM-CHECK-POSITION. EXIT.

       SET-REVERSO.

           IF  (GRMOUS-POSIT (ROW-MOUSE COLUMN-MOUSE) NOT = 0)
           AND (MOUSE-EVENT-TYPE > 1)
                MOVE GRMOUS-POSIT (ROW-MOUSE COLUMN-MOUSE) TO MATRIZ-KEY
                IF   MOUSE-EVENT-TYPE = 2 OR 3
                     SET EDIT-ENTER TO TRUE
                ELSE
                     SET EDIT-ESC TO TRUE
                END-IF
                GO TO FIM-SET-REVERSO
           END-IF
           MOVE 0 TO MOUSE-EVENT-TYPE

           IF  ACESO NOT = GRMOUS-POSIT (ROW-MOUSE COLUMN-MOUSE)
               PERFORM APAGAR
           END-IF
           IF   GRMOUS-POSIT (ROW-MOUSE COLUMN-MOUSE) = ACESO OR 0
                GO TO FIM-SET-REVERSO
           END-IF
           MOVE GRMOUS-POSIT (ROW-MOUSE COLUMN-MOUSE) TO ACESO
           COMPUTE LX                    = ROW-MOUSE
           COMPUTE CX                    = COLUMN-MOUSE
           MOVE    GRMOUS-POSIT (LX CX) TO FLAG
           COMPUTE ROW-NUMBER2           = LX - 1
           COMPUTE COLUMN-NUMBER2        = CX - 1
           MOVE    0                    TO STRING-LENGTH2

           PERFORM VARYING CX FROM CX BY -1
                   UNTIL CX < 2
                      OR GRMOUS-POSIT (LX CX) NOT = FLAG
           END-PERFORM

           IF   GRMOUS-POSIT (LX CX) NOT = FLAG
                ADD 1 TO CX.

           COMPUTE START-STRING = CX - 1

           PERFORM VARYING CX FROM CX BY 1
                   UNTIL CX > 79
                      OR GRMOUS-POSIT (LX CX) NOT = FLAG
                         ADD 1 TO STRING-LENGTH2
           END-PERFORM

           MOVE START-STRING             TO COLUMN-NUMBER2

           CALL "CBL_READ_SCR_ATTRS"  USING SCREEN-POSITION2
                                            ATTRIBUTE-BUFFER2
                                            STRING-LENGTH2

           MOVE ATTRIBUTE-BUFFER2        TO SAVE-ATTRIBUTE-BUFFER2
           MOVE ATTR-LOW                 TO ATTRIBUTE-BUFFER2

           CALL "CBL_WRITE_SCR_ATTRS" USING SCREEN-POSITION2
                                            ATTRIBUTE-BUFFER2
                                            STRING-LENGTH2.
       FIM-SET-REVERSO. EXIT.

       APAGAR.

           IF   ACESO NOT = 0
                CALL "CBL_WRITE_SCR_ATTRS" USING SCREEN-POSITION2
                                                 SAVE-ATTRIBUTE-BUFFER2
                                                 STRING-LENGTH2
                MOVE 0 TO ACESO
           END-IF.

       FIM-APAGAR. EXIT.

       EXIBE-BUTTON.

           COMPUTE ROW-NUMBER2    = POS-LIN (OPT-B) - 2
           COMPUTE COLUMN-NUMBER2 = POS-COL (OPT-B) - 1
           MOVE LENG          (OPT-B) TO TAMANHO
                                         STRING-LENGTH2
           MOVE "Ú"                   TO TEXTO-TOP
           MOVE ALL "Ä"               TO TEXTO-TOP (2: )
           MOVE "¿"                   TO TEXTO-TOP (TAMANHO: )
           MOVE "³"                   TO TEXTO-MID
           MOVE GRLINE-SCREEN (OPT-B) TO TEXTO-MID (3: )
           MOVE "³"                   TO TEXTO-MID (TAMANHO: )
           MOVE "À"                   TO TEXTO-BOT
           MOVE ALL "Ä"               TO TEXTO-BOT (2: )
           MOVE "Ù"                   TO TEXTO-BOT (TAMANHO: )

           SUBTRACT 1 FROM TAMANHO

           IF  BUTTON-TYPE = "D"
               MOVE ATTR-LOW              TO ATTRIB-TOP
                                             ATTRIB-MID
                                             ATTRIB-BOT
               MOVE ATTR-HIGH             TO ATTRIB-TOP (1: TAMANHO)
               MOVE ATTR-HIGH             TO ATTRIB-MID (1: 1)
                                             ATTRIB-BOT (1: 1)
           ELSE
               MOVE ATTR-HIGH            TO ATTRIB-TOP
                                            ATTRIB-MID
                                            ATTRIB-BOT
               MOVE ATTR-LOW             TO ATTRIB-TOP (1: TAMANHO)
               MOVE ATTR-LOW             TO ATTRIB-MID (1: 1)
                                            ATTRIB-BOT (1: 1)
           END-IF

           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                              TEXTO-TOP
                                              ATTRIB-TOP
                                              STRING-LENGTH2
           ADD 1 TO ROW-NUMBER2
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                              TEXTO-MID
                                              ATTRIB-MID
                                              STRING-LENGTH2
           ADD 1 TO ROW-NUMBER2
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                              TEXTO-BOT
                                              ATTRIB-BOT
                                              STRING-LENGTH2.

       FIM-EXIBE-BUTTON. EXIT.

       END PROGRAM GRLINE.
