       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGP001T.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       WORKING-STORAGE SECTION.
           COPY "CGP001T.CPB".
           COPY "CGP001T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(6)   VALUE SPACES.
           05  LETRA                 PIC X      VALUE SPACES.
           05  LETRA1                PIC X      VALUE SPACES.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
       01  STRING-1               PIC X(65) VALUE SPACES.

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CGP001T-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE "000000" TO STRING-1(33: 6).
           MOVE "00"     TO STRING-1(40: 2).
           INITIALIZE CGP001T-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CGP001T-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CGP001T-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           OPEN INPUT CGD001.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CGP001T-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CGP001T-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CGP001T-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CGP001T-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN CGP001T-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-ULTIMOS SECTION.
      *    MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-CGD001
           MOVE ZEROS TO CGP001T-CONT.
           MOVE CGP001T-LINDET(1: 1) TO NOME-CG01(1: 1) LETRA.
           START CGD001 KEY IS NOT < NOME-CG01
                    INVALID KEY MOVE "10" TO ST-CGD001.
           MOVE SPACES TO CGP001T-LINDET.

           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 NEXT RECORD AT END MOVE "10" TO ST-CGD001
              NOT AT END

               MOVE NOME-CG01(1: 1)   TO LETRA1

               IF LETRA1 <> LETRA CONTINUE
               ELSE
                  IF STRING-1(1:1) <> "R"
                     ADD 1 TO CGP001T-CONT
                     MOVE NOME-CG01         TO CGP001T-LINDET(01: 30)
                     MOVE CODIGO-CG01       TO CGP001T-LINDET(33: 06)
                     MOVE "INSERE-LIST" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  ELSE
                     IF T-REPRES-CG01 = 1
                        ADD 1 TO CGP001T-CONT
                        MOVE NOME-CG01         TO CGP001T-LINDET(01: 30)
                        MOVE CODIGO-CG01       TO CGP001T-LINDET(33: 06)
                        MOVE "INSERE-LIST" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
                  END-IF
                END-IF
              END-READ
           END-PERFORM.

       ITEM-SELECIONADO SECTION.
           MOVE CGP001T-LINDET(1: 40) TO STRING-1.
           MOVE STRING-1(33: 6) TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(33: 6)
                                MOVE ZEROS TO STRING-1(40: 2).
       CLEAR-FLAGS SECTION.
           INITIALIZE CGP001T-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CGP001T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CGP001T-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001.
           move ds-quit-set to ds-control.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
