       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP004T.
      * FUNCTION - POP UP - CADASTRO DE BANCOS
      * AUTHOR: MARELI AMANCIO VOLPATO
      * DATE: 26/10/1998
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CBPX004.

       DATA DIVISION.
       FILE SECTION.
       COPY CBPW004.
       WORKING-STORAGE SECTION.
           COPY "CBP004T.CPB".
           COPY "CBP004T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CBD004             PIC XX       VALUE SPACES.
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

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
       01  STRING-1               PIC X(65) VALUE SPACES.

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CBP004T-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE "00" TO STRING-1(40: 2).
           INITIALIZE CBP004T-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CBP004T-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CBP004T-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CBD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CBD004.
           OPEN INPUT CBD004.
           IF ST-CBD004 <> "00"
              MOVE "ERRO ABERTURA CBD004: "  TO CBP004T-MENSAGEM-ERRO
              MOVE ST-CBD004 TO CBP004T-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CBP004T-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CBP004T-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN CBP004T-ITEM-SELECIONADO-TRUE
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
           MOVE ZEROS TO CBP004T-CONT.
           MOVE SPACES TO HISTORICO-CB04.
           START CBD004 KEY IS NOT < HISTORICO-CB04
                    INVALID KEY MOVE "10" TO ST-CBD004.
           MOVE SPACES TO CBP004T-LINDET.
           PERFORM UNTIL ST-CBD004 = "10"
              READ CBD004 NEXT RECORD AT END MOVE "10" TO ST-CBD004
              NOT AT END
                ADD 1 TO CBP004T-CONT
                MOVE HISTORICO-CB04       TO CBP004T-LINDET(01: 31)
      *         MOVE APURACAO-RED-D-CB04  TO CBP004T-LINDET(32: 04)
      *         MOVE APURACAO-RED-C-CB04  TO CBP004T-LINDET(36: 04)
                MOVE CODIGO-CB04          TO CBP004T-LINDET(40: 02)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       ITEM-SELECIONADO SECTION.
           MOVE CBP004T-LINDET(1: 60) TO STRING-1.
           MOVE STRING-1(40: 2)       TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(40: 2).
       CLEAR-FLAGS SECTION.
           INITIALIZE CBP004T-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CBP004T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CBP004T-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CBD004.
           move ds-quit-set to ds-control.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
