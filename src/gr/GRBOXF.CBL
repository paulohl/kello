       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRBOXF.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  29/03/93.
       SECURITY.      *************************************************
                      *                                               *
                      *  Abre janela-menu e retorna a opcao           *
                      *                                               *
                      *  Versao File access                           *
                      *                                               *
                      *         1 - Opcoes ilimitadas                 *
                      *         2 - Rolagem e paginacao               *
                      *         3 - Acesso a arquivo externo          *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 MSG                      PIC  X(030) VALUE
              "Imposs¡vel executar XXXXXXXX  ".
           05 L-UP                     PIC  9(002) COMP-X VALUE 0.
           05 L-DN                     PIC  9(002) COMP-X VALUE 0.
           05 C-UPDN                   PIC  9(002) COMP-X VALUE 0.
           05 NADA                     PIC  X(001) VALUE SPACE.
           05 ROLA                     PIC  X(001) VALUE "S".
           05 CARACTER                 PIC  X(001).
           05 CARACTER-X REDEFINES CARACTER
                                       PIC  9(002) COMP-X.
           COPY CPEDIT.
           05 TECLA                    PIC  9(003) VALUE 0.
           05 VEZ                 COMP PIC  9(001) VALUE 1.
           05 VOLTA                    PIC  9(002) VALUE 0.
           05 C-1                      PIC  9(002) VALUE 0.
           05 C                        PIC  9(002) VALUE 0.
           05 S                        PIC  9(002) VALUE 0.
           05 L                        PIC  9(002) VALUE 0.
           05 L2                       PIC  9(002) VALUE 0.
           05 M-1                      PIC  9(002) VALUE 0.
           05 I                        PIC  9(002) VALUE 0.
           05 T                        PIC  9(002) VALUE 0.
           05 Z                        PIC  9(002) VALUE 0.
           05 ZZ                       PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 M                        PIC  9(002) VALUE 0.
           05 M2                       PIC  9(002) VALUE 0.
           05 OPERADOR                 PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 SET-LOG                  PIC  X(001) VALUE "?".
           05 ABERTO                   PIC  X(001) VALUE "N".
           05 FIM                      PIC  X(001) VALUE ">".
           05 WORK-NAME-A              PIC  X(008) VALUE SPACES.
           05 TEXTO-A                              VALUE SPACES.
              10 BYTE-A                PIC  X(001) OCCURS 80.
           05 TEXTO-B                              VALUE SPACES.
              10 BYTE-B                PIC  X(001) OCCURS 80.
           05 MATRIZ                               VALUE SPACES.
              10 TEXTO-1 OCCURS 25     PIC  X(040).
              10 TEXTO-2 OCCURS 25     PIC  X(040).
              10 TEXTO-L OCCURS 25     PIC  X(080).
           05 TABELA-MOLDURA           PIC  X(009)
              VALUE X"BAB3B3BAB0B1B27C20".
           05 M-186                    PIC  X(001).
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BARR       PIC  X(001) VALUE SPACES.
              10 ATTRIBUTE-BARR-X REDEFINES
                 ATTRIBUTE-BARR       PIC  9(002) COMP-X.
              10 ATTRIBUTE-BUFFER     PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 0.
              10 STRING-START         PIC  9(004) COMP-X VALUE 0.
              10 CURSOR-POSITION.
                 15 ROW-CURSOR        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-CURSOR     PIC  9(002) COMP-X VALUE 0.
              10 SCREEN-POSITION-MOUSE.
                 15 ROW-NUMBER-2      PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER-2   PIC  9(002) COMP-X VALUE 0.
           05 SIZE-BARR-MENU          PIC  9(004) COMP-X VALUE 0.
           05 MOUSE-HANDLE            PIC  9(008) COMP-X VALUE 1.
           05 MOUSE-BUTTONS           PIC  9(002) COMP-X VALUE 3.
           05 MOUSE-POSITION.
              10 ROW-MOUSE            PIC  9(004) COMP-X.
              10 COLUMN-MOUSE         PIC  9(004) COMP-X.
           05 MOUSE-DATA.
              10 MOUSE-EVENT-TYPE     PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-TIME     PIC  9(008) COMP-X VALUE 0.
              10 MOUSE-EVENT-ROW      PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-COL      PIC  9(004) COMP-X VALUE 0.
           05 MOUSE-READ-TYPE         PIC  9(002) COMP-X VALUE 0.
           05 BYTE-MOUSE              PIC  X(001) VALUE SPACE.
           05 BYTE-MOUSE-LENGTH       PIC  9(004) COMP-X VALUE 1.
           05 KEY-STATUS              PIC  9(002) COMP-X VALUE 0.
           05 COLUMN-MAX              PIC  9(002) COMP-X VALUE 0.
           05 COLUMN-MIN              PIC  9(002) COMP-X VALUE 0.
           05 ROW-MIN                 PIC  9(002) COMP-X VALUE 0.
           05 ROW-MAX                 PIC  9(002) COMP-X VALUE 0.
           05 E                COMP-X PIC  9(002) VALUE ZERO.
           05 E-ANT            COMP-X PIC  9(002) VALUE ZERO.
           05 COLOR-BARR-MENU-BLINK.
              10 COLOR-BARR-MENU-BLINK-N PIC  9(002) COMP-X VALUE 0.
           05 MOLDURA-DEFAULT            PIC  9(001) VALUE 0.
           05 USER-IO                    PIC  X(001).
              88 BEGIN-FILE                          VALUE "B" "b".
              88 END-FILE                            VALUE "E" "e".
              88 AT-END                              VALUE "*".
              88 READ-NEXT                           VALUE "N" "n".
              88 READ-PREVIOUS                       VALUE "P" "p".
              88 NOT-LESS                            VALUE ">".
              88 NOT-GREATER                         VALUE "<".

       COPY CPWIND.

       LINKAGE SECTION.

       01  PARAMETROS-GRBOXF.
           05 GRBOXF-FUNCTION                PIC  X(001).
              88 GRBOXF-SHOW                             VALUE "S" "s"
                                                               "P" "p".
              88 GRBOXF-POP-UP                           VALUE "P" "p".
           05 GRBOXF-PROGRAM                 PIC  X(008).
           05 GRBOXF-WORK-AREA               PIC  X(050).
           05 GRBOXF-LINE                    PIC  9(002).
           05 GRBOXF-COLUMN                  PIC  9(002).
           05 GRBOXF-TYPE                    PIC  9(001).
           05 GRBOXF-VERTICAL-LENGTH         PIC  9(002).
           05 GRBOXF-HORIZONTAL-LENGTH       PIC  9(002).
           05 GRBOXF-OPTION                  PIC  X(040).
           05 GRBOXF-TITLE.
              10 GRBOXF-TITLE-BYTE           PIC  X(001) OCCURS 78.
           05 GRBOXF-COLOR-SCREEN            PIC  9(002) COMP-X.
           05 GRBOXF-COLOR-FRAME             PIC  9(002) COMP-X.
           05 GRBOXF-COLOR-SHADE             PIC  9(002) COMP-X.
           05 GRBOXF-COLOR-BARR-MENU         PIC  9(002) COMP-X.
           05 GRBOXF-ORDER                   PIC  9(001).
           05 GRBOXF-RETURN                  PIC  9(001).
           05 GRBOXF-STRING-1                PIC  X(040).
           05 GRBOXF-STRING-1-LENGTH         PIC  9(002).
           05 GRBOXF-STRING-2                PIC  X(040).
           05 GRBOXF-STRING-2-LENGTH         PIC  9(002).

       PROCEDURE DIVISION USING PARAMETROS-GRBOXF.

       000-INICIO.

           IF   VEZ = 1
                MOVE 2            TO VEZ
                MOVE "m"          TO SET-LOG
                CALL  "GRFINA" USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     SET-LOG
                MOVE TASK         TO MOLDURA-DEFAULT.

           IF   GRBOXF-TYPE = 0
                MOVE MOLDURA-DEFAULT TO GRBOXF-TYPE.

           CALL GRBOXF-PROGRAM USING "O" GRBOXF-ORDER
                                         GRBOXF-STRING-1
                                         GRBOXF-STRING-2
                                         GRBOXF-VERTICAL-LENGTH
                                         GRBOXF-WORK-AREA
                ON EXCEPTION
                   MOVE GRBOXF-PROGRAM TO MSG (21: )
                   CALL "GREMSG" USING MSG
                   GOBACK.

           PERFORM 200-SHOW THRU 200-99-FIM
           CALL GRBOXF-PROGRAM USING "C" GRBOXF-ORDER
                                         GRBOXF-STRING-1
                                         GRBOXF-STRING-2
                                         GRBOXF-VERTICAL-LENGTH
                                         GRBOXF-WORK-AREA
           CALL "GRHORA"
           CANCEL GRBOXF-PROGRAM.

       000-99-FIM. GOBACK.

       200-SHOW.

           MOVE GRBOXF-LINE              TO GRWIND-LINE   L
           MOVE GRBOXF-COLUMN            TO GRWIND-COLUMN C
           MOVE GRBOXF-TYPE              TO GRWIND-TYPE
           MOVE GRBOXF-VERTICAL-LENGTH   TO GRWIND-VERTICAL-LENGTH
           MOVE GRBOXF-HORIZONTAL-LENGTH TO GRWIND-HORIZONTAL-LENGTH
           MOVE GRBOXF-COLOR-SCREEN      TO GRWIND-COLOR-SCREEN
           MOVE GRBOXF-COLOR-FRAME       TO GRWIND-COLOR-FRAME
           MOVE GRBOXF-COLOR-SHADE       TO GRWIND-COLOR-SHADE
           MOVE "OPEN"                   TO GRWIND-FUNCTION
           CALL "GRWIND"              USING PARAMETROS-GRWIND

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 78
                   IF   GRBOXF-TITLE-BYTE (I) NOT = SPACE
                        COMPUTE Y = C + I + 1
                        IF   GRBOXF-TITLE-BYTE (I) NOT = "_"
                             DISPLAY GRBOXF-TITLE-BYTE (I)
                                      LINE L COLUMN Y
                        ELSE
                             DISPLAY " " LINE L COLUMN Y
                        END-IF
                   END-IF
           END-PERFORM

           MOVE GRBOXF-COLOR-BARR-MENU TO COLOR-BARR-MENU-BLINK
           ADD  128                    TO COLOR-BARR-MENU-BLINK-N
           COMPUTE ROW-MIN    = GRBOXF-LINE - 1
           COMPUTE ROW-MAX    = ROW-MIN + GRBOXF-VERTICAL-LENGTH + 1
           COMPUTE COLUMN-MIN = GRBOXF-COLUMN + 1
           COMPUTE COLUMN-MAX = COLUMN-MIN
                              + GRBOXF-HORIZONTAL-LENGTH - 1

           ON   1
                CALL "CBL_INIT_MOUSE"         USING MOUSE-HANDLE
                                                    MOUSE-BUTTONS
                     ON OVERFLOW
                        CONTINUE
                END-CALL.

           PERFORM 205-INICIO-BOXWRK THRU 205-99-FIM

           MOVE ">" TO FIM
           ADD  1   TO C

           IF  GRBOXF-STRING-1-LENGTH NOT = 0
               COMPUTE I = 2 + GRBOXF-STRING-1-LENGTH
               COMPUTE Y = I - 3
           ELSE
               MOVE 2 TO I.

           PERFORM 210-MONTA-TELA THRU 210-99-FIM

           COMPUTE S      = GRWIND-TYPE   + 1
           COMPUTE L-UP   = GRBOXF-LINE   + 1
           COMPUTE L-DN   = GRBOXF-LINE   + GRBOXF-VERTICAL-LENGTH
           COMPUTE C-UPDN = GRBOXF-COLUMN + GRBOXF-HORIZONTAL-LENGTH + 1

           MOVE TABELA-MOLDURA (S: 1) TO M-186

           PERFORM TEST AFTER UNTIL EDIT-ESC OR EDIT-ENTER
                   PERFORM UNTIL M = 1
                              OR TEXTO-L (M) NOT = SPACES
                              SUBTRACT 1 FROM M L
                   END-PERFORM
                   EVALUATE TRUE
                      WHEN FIM = "N"
                           DISPLAY X"18" LINE L-UP COLUMN C-UPDN
                           DISPLAY X"19" LINE L-DN COLUMN C-UPDN
                      WHEN FIM = ">"
                           DISPLAY M-186 LINE L-UP COLUMN C-UPDN
                           DISPLAY X"19" LINE L-DN COLUMN C-UPDN
                      WHEN FIM = "<"
                           DISPLAY X"18" LINE L-UP COLUMN C-UPDN
                           DISPLAY M-186 LINE L-DN COLUMN C-UPDN
                   END-EVALUATE
                   MOVE L                        TO ROW-NUMBER
                   MOVE C                        TO COLUMN-NUMBER
                   SUBTRACT 1                  FROM COLUMN-NUMBER
                                                    ROW-NUMBER
                   MOVE 0                        TO RETURN-CODE
                   MOVE GRBOXF-HORIZONTAL-LENGTH TO SIZE-BARR-MENU
                   CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                              GRBOXF-COLOR-BARR-MENU
                                                      SIZE-BARR-MENU
                   MOVE "S" TO ROLA
                   MOVE ROW-NUMBER TO ROW-CURSOR
                   EVALUATE TRUE
                     WHEN GRBOXF-ORDER = 1
                          COMPUTE COLUMN-CURSOR = C + T
                     WHEN GRBOXF-ORDER = 2
                          COMPUTE COLUMN-CURSOR = I + C - 2 + T
                     WHEN OTHER
                          MOVE 255 TO ROW-CURSOR
                                      COLUMN-CURSOR
                   END-EVALUATE
                   CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
                   MOVE 0 TO TECLA
                   MOVE E TO E-ANT
                   PERFORM TEST AFTER UNTIL KEY-STATUS = 1
                                         OR MOUSE-EVENT-TYPE > 1
                           CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                           CALL "CBL_GET_MOUSE_POSITION"
                                 USING MOUSE-HANDLE
                                       MOUSE-POSITION
                             RETURNING RETURN-CODE
                           ON   OVERFLOW
                                CONTINUE
                           END-CALL
                           CALL "GRHORA"
                           CALL "CBL_READ_MOUSE_EVENT" USING
                                                       MOUSE-HANDLE
                                                       MOUSE-DATA
                                                       MOUSE-READ-TYPE
                           ON   OVERFLOW
                                CONTINUE
                           END-CALL
                           IF   MOUSE-EVENT-TYPE = 2 OR 3
                                MOVE ROW-MOUSE    TO ROW-NUMBER-2
                                MOVE COLUMN-MOUSE TO COLUMN-NUMBER-2
                                CALL "CBL_READ_SCR_CHARS"
                                 USING SCREEN-POSITION-MOUSE
                                              BYTE-MOUSE
                                              BYTE-MOUSE-LENGTH
                                   ON   OVERFLOW
                                        CONTINUE
                                   END-CALL
                           END-IF
                           IF (((RETURN-CODE = 0)
                           AND  (COLUMN-MOUSE NOT < (COLUMN-MIN - 1))
                           AND  (COLUMN-MOUSE NOT > (COLUMN-MAX - 1))
                           AND  (ROW-MOUSE    NOT < ROW-MIN)
                           AND  (ROW-MOUSE    NOT > ROW-MAX))
                           OR  (BYTE-MOUSE = X"18" OR X"19"))
                                COMPUTE E = ROW-MOUSE - ROW-MIN
                                IF  (E NOT = E-ANT)
                                AND (BYTE-MOUSE NOT = X"18")
                                AND (BYTE-MOUSE NOT = X"19")
                                AND (TEXTO-L (E) NOT = SPACES)
                                AND (ROW-MOUSE   NOT = ROW-MIN)
                                AND (ROW-MOUSE   NOT = ROW-MAX)
                                    MOVE E TO E-ANT
                                    CALL "CBL_WRITE_SCR_N_ATTR"
                                          USING SCREEN-POSITION
                                                GRBOXF-COLOR-SCREEN
                                                SIZE-BARR-MENU
                                    COMPUTE L          = ROW-MOUSE + 1
                                    MOVE    ROW-MOUSE TO ROW-NUMBER
                                    COMPUTE M          = L - GRBOXF-LINE
                                    CALL "CBL_WRITE_SCR_N_ATTR"
                                          USING SCREEN-POSITION
                                                GRBOXF-COLOR-BARR-MENU
                                                SIZE-BARR-MENU
                                END-IF
                                EVALUATE TRUE
                                    WHEN BYTE-MOUSE = X"18"
                                         SET EDIT-CURSOR-UP   TO TRUE
                                    WHEN BYTE-MOUSE = X"19"
                                         SET EDIT-CURSOR-DOWN TO TRUE
                                    WHEN ROW-MOUSE = ROW-MIN
                                         SET EDIT-PAGE-UP     TO TRUE
                                    WHEN ROW-MOUSE = ROW-MAX
                                         SET EDIT-PAGE-DOWN   TO TRUE
                                END-EVALUATE
                            ELSE
                                MOVE 0 TO MOUSE-EVENT-TYPE
                           END-IF
                   IF   GRBOXF-OPTION NOT = SPACES
                        IF  (GRBOXF-OPTION = TEXTO-1 (M)
                        AND  GRBOXF-RETURN = 1)
                        OR  (GRBOXF-OPTION = TEXTO-2 (M)
                        AND  GRBOXF-RETURN = 2)
                        OR   FIM = "<"
                             MOVE SPACES TO GRBOXF-OPTION
                        ELSE
                             MOVE 1 TO KEY-STATUS
                             SET EDIT-CURSOR-DOWN TO TRUE
                             MOVE 9 TO MOUSE-EVENT-TYPE
                        END-IF
                   END-IF
                   END-PERFORM
                   IF   MOUSE-EVENT-TYPE = 0
                        CALL "CBL_READ_KBD_CHAR" USING CARACTER
                        EVALUATE TRUE
                            WHEN CARACTER = X"0D"
                                 MOVE SPACE TO CARACTER
                                 SET EDIT-ENTER TO TRUE
                                 MOVE 0    TO T
                            WHEN CARACTER = X"1B"
                                 MOVE SPACE TO CARACTER
                                 SET EDIT-ESC TO TRUE
                                 MOVE 0 TO T
                            WHEN CARACTER = X"00"
                                 CALL "CBL_READ_KBD_CHAR" USING CARACTER
                                 MOVE CARACTER-X TO TECLA
                                 MOVE SPACE      TO CARACTER
                            WHEN OTHER
                                 PERFORM 201-LER-TECLA THRU 201-99-FIM
                        END-EVALUATE
                   ELSE
                        IF   MOUSE-EVENT-TYPE = 2 OR 3
                             IF   BYTE-MOUSE NOT = X"18"
                             AND  BYTE-MOUSE NOT = X"19"
                             AND  ROW-MOUSE  NOT = ROW-MIN
                             AND  ROW-MOUSE  NOT = ROW-MAX
                                  SET  EDIT-ENTER TO TRUE
                             END-IF
                             MOVE SPACE TO BYTE-MOUSE
                        ELSE
                             IF   MOUSE-EVENT-TYPE NOT = 9
                                  SET EDIT-ESC TO TRUE
                             END-IF
                        END-IF
                   END-IF
                   CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                                 GRBOXF-COLOR-SCREEN
                                                      SIZE-BARR-MENU
                   CALL "GRHORA"
                   EVALUATE TRUE
                      WHEN EDIT-CURSOR-RIGHT
                       AND GRBOXF-ORDER = 1
                       AND T < GRBOXF-STRING-1-LENGTH
                           ADD 1 TO T
                      WHEN EDIT-CURSOR-RIGHT
                       AND GRBOXF-ORDER = 2
                       AND T < GRBOXF-STRING-2-LENGTH
                           ADD 1 TO T
                      WHEN EDIT-CURSOR-LEFT
                       AND T > 0
                           SUBTRACT 1 FROM T
                   END-EVALUATE
                   IF  ROLA = "S"
                       EVALUATE TRUE
                          WHEN EDIT-HOME
                           AND T > 0
                               MOVE    0 TO T
                          WHEN EDIT-HOME
                               MOVE    1 TO M
                               COMPUTE L  = GRBOXF-LINE + 1
                          WHEN EDIT-END
                               MOVE GRBOXF-VERTICAL-LENGTH TO M
                               COMPUTE L = GRBOXF-LINE
                                         + GRBOXF-VERTICAL-LENGTH
                          WHEN EDIT-CURSOR-DOWN
                           AND M < GRBOXF-VERTICAL-LENGTH
                               ADD 1 TO M L
                          WHEN EDIT-CURSOR-UP
                           AND M > 1
                               SUBTRACT 1 FROM M L
                          WHEN EDIT-CURSOR-DOWN
                           AND FIM NOT = "<"
                           AND M = GRBOXF-VERTICAL-LENGTH
                               MOVE "N"  TO FIM
                               MOVE "N" TO USER-IO
                               PERFORM 250-LER
                               IF  AT-END
                                   MOVE "<" TO FIM
                                   MOVE "P" TO USER-IO
                                   PERFORM 250-LER
                               ELSE
                                   MOVE "N" TO FIM
                                   PERFORM VARYING M FROM 2 BY 1
                                      UNTIL M > GRBOXF-VERTICAL-LENGTH
                                      COMPUTE M-1 = M - 1
                                      MOVE TEXTO-1 (M) TO TEXTO-1 (M-1)
                                      MOVE TEXTO-2 (M) TO TEXTO-2 (M-1)
                                      MOVE TEXTO-L (M) TO TEXTO-L (M-1)
                                   END-PERFORM
                                   MOVE GRBOXF-VERTICAL-LENGTH TO M
                                   PERFORM 215-DISCO-MEMORIA
                                   THRU    215-99-FIM
                                   PERFORM 220-EXIBE-TELA
                                   THRU    220-99-FIM
                               END-IF
                          WHEN EDIT-CONTROL-PAGE-UP
                               PERFORM 205-INICIO-BOXWRK THRU 205-99-FIM
                               PERFORM 210-MONTA-TELA    THRU 210-99-FIM
                               MOVE ">" TO FIM
                          WHEN EDIT-CONTROL-PAGE-DOWN
                               MOVE    "N"              TO FIM
                               PERFORM 206-FIM-BOXWRK THRU 206-99-FIM
                          WHEN ((EDIT-CURSOR-UP AND M = 1)
                            OR EDIT-PAGE-UP)
                           AND FIM NOT = ">"
                               MOVE "N"  TO FIM
                               IF   EDIT-PAGE-UP
                                    ADD GRBOXF-VERTICAL-LENGTH
                                     TO VOLTA
                               ELSE
                                    ADD 1 TO VOLTA
                               END-IF
                               PERFORM VOLTA TIMES
                                       IF   FIM NOT = ">"
                                            MOVE "P" TO USER-IO
                                            PERFORM 250-LER
                                            IF AT-END
                                               PERFORM 205-INICIO-BOXWRK
                                               THRU 205-99-FIM
                                            END-IF
                                       END-IF
                               END-PERFORM
                               PERFORM 210-MONTA-TELA THRU 210-99-FIM
                          WHEN EDIT-PAGE-DOWN
                           AND FIM NOT = "<"
                               MOVE "N" TO FIM
                               PERFORM 210-MONTA-TELA THRU 210-99-FIM
                               MOVE GRBOXF-VERTICAL-LENGTH TO M
                               COMPUTE L = GRBOXF-LINE
                                         + GRBOXF-VERTICAL-LENGTH
                               IF   TEXTO-L (1) = SPACE
                                    MOVE    "N" TO FIM
                                    PERFORM 206-FIM-BOXWRK
                                       THRU 206-99-FIM
                               END-IF
                       END-EVALUATE
                   END-IF
           END-PERFORM

           IF   EDIT-ENTER
                EVALUATE TRUE
                    WHEN GRBOXF-RETURN = 1
                         MOVE TEXTO-1 (M) TO GRBOXF-OPTION
                    WHEN GRBOXF-RETURN = 2
                         MOVE TEXTO-2 (M) TO GRBOXF-OPTION
                END-EVALUATE
           ELSE
                MOVE SPACES TO GRBOXF-OPTION.

           IF   GRBOXF-POP-UP
                MOVE "POPUP"             TO GRWIND-FUNCTION
                IF   GRBOXF-OPTION NOT = SPACES
                     CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                                 COLOR-BARR-MENU-BLINK
                                                       SIZE-BARR-MENU
                END-IF
           ELSE
                MOVE "CLOSE"             TO GRWIND-FUNCTION.

           CALL "GRWIND"              USING PARAMETROS-GRWIND.

       200-99-FIM. EXIT.

       201-LER-TECLA.

           EVALUATE TRUE
             WHEN GRBOXF-ORDER = 1
              AND GRBOXF-STRING-1-LENGTH NOT = 0
                  IF   T < GRBOXF-STRING-1-LENGTH
                       ADD 1 TO T
                  END-IF
                  MOVE TEXTO-L (M) (2: GRBOXF-STRING-1-LENGTH)
                    TO TEXTO-A TEXTO-B
                  COMPUTE C-1 = C + 1
                  IF   CARACTER = X"08"
                       IF   T > 2
                            SUBTRACT 1 FROM T
                            MOVE SPACE TO TEXTO-A (T: 1) CARACTER
                            SUBTRACT 1 FROM T
                       END-IF
                  ELSE
                       MOVE CARACTER TO TEXTO-A (T: 1)
                  END-IF
                  DISPLAY TEXTO-A LINE L COLUMN C-1 WITH SIZE T
                  IF  TEXTO-A NOT = TEXTO-B
                      MOVE "N"            TO FIM
                                             ROLA
                      MOVE TEXTO-A (1: T) TO GRBOXF-STRING-1
                      MOVE ">" TO USER-IO
                      PERFORM 250-LER
                      IF   AT-END
                           PERFORM 206-FIM-BOXWRK
                           THRU    206-99-FIM
                      ELSE
                           PERFORM 210-MONTA-TELA
                           THRU    210-99-FIM
                      END-IF
                  END-IF
                  IF   CARACTER = X"08"
                       MOVE 0 TO T
                  END-IF
             WHEN GRBOXF-ORDER = 2
              AND GRBOXF-STRING-2-LENGTH NOT = 0
                  IF   T < GRBOXF-STRING-2-LENGTH
                       ADD 1 TO T
                  END-IF
                  MOVE TEXTO-L (M) (I: GRBOXF-STRING-2-LENGTH)
                    TO TEXTO-A TEXTO-B
                  COMPUTE C-1 = I + C - 1
                  IF   CARACTER = X"08"
                       IF   T > 2
                            SUBTRACT 1 FROM T
                            MOVE SPACE TO TEXTO-A (T: 1) CARACTER
                            SUBTRACT 1 FROM T
                       END-IF
                  ELSE
                       MOVE CARACTER TO TEXTO-A (T: 1)
                  END-IF
                  DISPLAY TEXTO-A LINE L COLUMN C-1 WITH SIZE T
                  IF  TEXTO-A NOT = TEXTO-B
                      MOVE "N"            TO FIM
                                             ROLA
                      MOVE TEXTO-A (1: T) TO GRBOXF-STRING-2
                      MOVE ">" TO USER-IO
                      PERFORM 250-LER
                      IF   AT-END
                           PERFORM 206-FIM-BOXWRK
                           THRU    206-99-FIM
                      ELSE
                           PERFORM 210-MONTA-TELA
                           THRU    210-99-FIM
                      END-IF
                  END-IF
                  IF   CARACTER = X"08"
                       MOVE 0 TO T
                  END-IF
           END-EVALUATE.

       201-99-FIM. EXIT.

       205-INICIO-BOXWRK.

           MOVE ">" TO FIM
           MOVE "B" TO USER-IO
           PERFORM 250-LER.

       205-99-FIM. EXIT.

       206-FIM-BOXWRK.

           MOVE "E" TO USER-IO
           PERFORM 250-LER

           COMPUTE VOLTA = GRBOXF-VERTICAL-LENGTH + 1
           PERFORM VOLTA TIMES
             IF  FIM NOT = ">"
                 MOVE "P" TO USER-IO
                 PERFORM 250-LER
                 IF   AT-END
                      MOVE ">" TO FIM
                 END-IF
             END-IF
           END-PERFORM

           MOVE "N" TO FIM
           PERFORM 210-MONTA-TELA THRU 210-99-FIM
           MOVE GRBOXF-VERTICAL-LENGTH TO M
           COMPUTE L = GRBOXF-LINE
                     + GRBOXF-VERTICAL-LENGTH
           MOVE "<" TO FIM.

       206-99-FIM. EXIT.

       210-MONTA-TELA.

           MOVE GRBOXF-LINE TO L
           MOVE 0           TO M
                               VOLTA
           MOVE SPACES      TO MATRIZ
           PERFORM GRBOXF-VERTICAL-LENGTH TIMES
             ADD 1 TO M L
             IF FIM NOT = "<"
                 MOVE "N" TO USER-IO
                 PERFORM 250-LER
                 IF   AT-END
                      MOVE "<" TO FIM
                      MOVE "P" TO USER-IO
                      PERFORM 250-LER
                 ELSE
                     ADD 1 TO VOLTA
                     PERFORM 215-DISCO-MEMORIA THRU 215-99-FIM
                END-IF
             END-IF
           END-PERFORM
           COMPUTE L = GRBOXF-LINE + 1
           MOVE    1 TO M
           PERFORM 220-EXIBE-TELA THRU 220-99-FIM.

       210-99-FIM. EXIT.

       215-DISCO-MEMORIA.

           MOVE GRBOXF-STRING-1  TO TEXTO-1 (M)
           MOVE GRBOXF-STRING-2  TO TEXTO-2 (M)

           IF  GRBOXF-STRING-1-LENGTH NOT = 0
               MOVE GRBOXF-STRING-1 TO TEXTO-L (M) (2: )
           END-IF
           IF  GRBOXF-STRING-2-LENGTH NOT = 0
               MOVE GRBOXF-STRING-2 TO TEXTO-L (M) (I: )
           END-IF.

       215-99-FIM. EXIT.

       220-EXIBE-TELA.

           MOVE GRBOXF-LINE TO L2
           MOVE 0           TO M2
           PERFORM GRBOXF-VERTICAL-LENGTH TIMES
             ADD 1 TO M2 L2
             DISPLAY TEXTO-L (M2) LINE L2 COLUMN C
                     WITH SIZE GRBOXF-HORIZONTAL-LENGTH
           END-PERFORM.

       220-99-FIM. EXIT.

       250-LER.

           CALL GRBOXF-PROGRAM USING USER-IO GRBOXF-ORDER
                                             GRBOXF-STRING-1
                                             GRBOXF-STRING-2
                                             GRBOXF-VERTICAL-LENGTH
                                             GRBOXF-WORK-AREA.

       250-99-FIM. EXIT.
       END PROGRAM GRBOXF.

