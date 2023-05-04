       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP030T.
      * AUTORA: MARELI AMÂNCIO VOLPATO
      * DATA: 28/08/1998
      * FUNCAO: CONSULTA POP-UP PORTADOR
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CXPX030.

       DATA DIVISION.
       FILE SECTION.
           COPY CXPW030.

       WORKING-STORAGE SECTION.
           COPY "CXP030T.CPB".
           COPY "CXP030T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD030             PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(2)   VALUE SPACES.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
           COPY "PARAMETR".

       01  STRING-1               PIC X(65) VALUE SPACES.
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP030T-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE "00" TO STRING-1(33: 2).
           INITIALIZE CXP030T-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP030T-DATA-BLOCK-VERSION-NO
                                    TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP030T-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W           TO EMP-REC
           MOVE "CXD030"            TO ARQ-REC.
           MOVE EMPRESA-REF         TO PATH-CXD030.
           OPEN INPUT CXD030
           IF ST-CXD030 <> "00"
              MOVE "ERRO ABERTURA CXD030: "  TO CXP030T-MENSAGEM-ERRO
              MOVE ST-CXD030 TO CXP030T-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP030T-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CXP030T-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN CXP030T-ITEM-SELECIONADO-TRUE
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
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO HISTORICO-CX30.
              START CXD030 KEY IS NOT < HISTORICO-CX30
                    INVALID KEY MOVE "10" TO ST-CXD030.
           MOVE SPACES TO CXP030T-LINDET.
           MOVE ZEROS TO CXP030T-CONT.
           PERFORM UNTIL ST-CXD030 = "10"
              READ CXD030 NEXT RECORD AT END MOVE "10" TO ST-CXD030
              NOT AT END
                MOVE HISTORICO-CX30     TO CXP030T-LINDET(01: 30)
                MOVE COD-HISTORICO-CX30 TO CXP030T-LINDET(33: 02)
                ADD 1 TO CXP030T-CONT
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           MOVE CXP030T-LINDET(1: 40) TO STRING-1.
           MOVE STRING-1(33: 2) TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(33: 2).
       CLEAR-FLAGS SECTION.
           INITIALIZE CXP030T-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP030T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP030T-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD030.
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
