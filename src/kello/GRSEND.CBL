       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRSEND.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/07/96.
       SECURITY.      *************************************************
                      *                                               *
                      *  Emite mensagen de erro "X(060)"               *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 NADA                     PIC  X(001) VALUE SPACE.
           05 MSG-D                    PIC  X(060) VALUE SPACES.
           05 CX                       PIC  9(002) VALUE 0.
           05 C                        PIC  9(002) VALUE 0.
           05 S                        PIC  9(002) VALUE 0.
           05 LENG                     PIC  9(003) VALUE 0.
           05 TAMANHO                  PIC  9(002) VALUE 0.

       COPY CPWIND.
       COPY CPLINE.

       LINKAGE SECTION.

       01  PARAMETROS-GRSEND.
           05 GRSEND-MSG                     PIC  X(060).
           05 GRSEND-SCREENS.
              10 GRSEND-CHAR   OCCURS 8      PIC  X(001).
              10 GRSEND-SCREEN OCCURS 8.
                20 BYTE-TXT OCCURS 10        PIC  X(001).
           05 GRSEND-OPTION                  PIC  9(001).
           05 GRSEND-OPTION-CHAR             PIC  X(001).

       PROCEDURE DIVISION USING PARAMETROS-GRSEND.

       000-INICIO.

           INITIALIZE AREAS-DE-TRABALHO-1
           CALL "GRHORA"

           IF   GRSEND-MSG NOT EQUAL SPACES
                MOVE SPACE TO NADA
                MOVE 29    TO C
                PERFORM VARYING CX FROM 60 BY -1
                        UNTIL NADA NOT = SPACE
                        MOVE GRSEND-MSG (CX: 1) TO NADA
                END-PERFORM
                MOVE GRSEND-MSG  TO MSG-D
                ADD  2           TO CX
                MOVE 10          TO GRWIND-LINE
                MOVE 26          TO GRWIND-COLUMN
                MOVE "OPEN"      TO GRWIND-FUNCTION
                MOVE 078         TO GRWIND-COLOR-SCREEN
                MOVE 078         TO GRWIND-COLOR-FRAME
                MOVE 7           TO GRWIND-VERTICAL-LENGTH
                MOVE CX          TO GRWIND-HORIZONTAL-LENGTH
                MOVE SPACES      TO GRLINE-SCREENS
                IF   GRSEND-SCREENS = SPACES
                     ADD  3           TO GRWIND-HORIZONTAL-LENGTH
                     PERFORM UNTIL (GRWIND-COLUMN
                                  + GRWIND-HORIZONTAL-LENGTH)
                                  < 78
                             SUBTRACT 1 FROM GRWIND-COLUMN C
                     END-PERFORM
                     CALL "GRWIND" USING PARAMETROS-GRWIND
                     DISPLAY MSG-D LINE 12 COLUMN C WITH SIZE CX
                     CALL X"E5"
                     MOVE "   Ok___"  TO GRLINE-SCREEN (1)
                     MOVE "O"         TO GRLINE-CHAR   (1)
                     COMPUTE GRLINE-COLUMN = GRWIND-COLUMN
                                      + (GRWIND-HORIZONTAL-LENGTH / 2)
                                      - 5
                ELSE
                     MOVE 0           TO LENG
                     PERFORM VARYING S FROM 1 BY 1 UNTIL S > 8
                             MOVE GRSEND-CHAR   (S) TO GRLINE-CHAR   (S)
                             MOVE GRSEND-SCREEN (S) TO GRLINE-SCREEN (S)
                             PERFORM VARYING TAMANHO FROM 10 BY -1
                                     UNTIL BYTE-TXT (S TAMANHO)
                                           NOT = SPACE
                                        OR GRSEND-SCREEN (S) = SPACES
                                           CONTINUE
                             END-PERFORM
                             IF   GRLINE-SCREEN (S) NOT = SPACES
                                  ADD TAMANHO TO LENG
                                  ADD 5       TO LENG
                             END-IF
                     END-PERFORM
                     IF   LENG > 57
                          ADD 4 TO GRWIND-VERTICAL-LENGTH
                     END-IF
                     IF  LENG > CX
                     AND LENG < 60
                         MOVE LENG TO CX
                     END-IF
                     MOVE CX TO GRWIND-HORIZONTAL-LENGTH
                     ADD  3  TO GRWIND-HORIZONTAL-LENGTH
                     COMPUTE GRLINE-COLUMN = GRWIND-COLUMN + 2
                     ADD  1           TO GRWIND-HORIZONTAL-LENGTH
                     PERFORM UNTIL (GRWIND-COLUMN
                                  + GRWIND-HORIZONTAL-LENGTH)
                                   < 78
                             SUBTRACT 1 FROM GRWIND-COLUMN C
                     END-PERFORM
                     COMPUTE GRLINE-COLUMN = GRWIND-COLUMN + 3
                     CALL "GRWIND" USING PARAMETROS-GRWIND
                     DISPLAY MSG-D LINE 12 COLUMN C WITH SIZE CX
                END-IF
                MOVE 14                 TO GRLINE-LINE
                MOVE GRSEND-OPTION      TO GRLINE-OPTION
                CALL "GRLINE"        USING PARAMETROS-GRLINE
                MOVE GRLINE-OPTION      TO GRSEND-OPTION
                MOVE GRLINE-OPTION-CHAR TO GRSEND-OPTION-CHAR
                MOVE "CLOSE"            TO GRWIND-FUNCTION
                CALL "GRWIND"        USING PARAMETROS-GRWIND
             END-IF.

       000-FIM. EXIT PROGRAM.

       END PROGRAM GRSEND.

