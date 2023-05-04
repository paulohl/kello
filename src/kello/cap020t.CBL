       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAP020T.
      * AUTORA: ALFREDO SAVIOLLI NETO
      * DATA: 10/03/2005
      * FUNCAO: CONSULTA POP-UP BANCOS
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX030.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW030.
       WORKING-STORAGE SECTION.
           COPY "CAP020T.CPB".
           COPY "CAP020T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD030             PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(4)   VALUE SPACES.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
       01  STRING-1               PIC X(65) VALUE SPACES.

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CAP018T-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           INITIALIZE CAP018T-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CAP018T-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CAP018T-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD030"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD030.
           OPEN INPUT CAD030
           IF ST-CAD030 <> "00"
              MOVE "ERRO ABERTURA CAD030: "  TO CAP018T-MENSAGEM-ERRO
              MOVE ST-CAD030 TO CAP018T-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CAP018T-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CAP018T-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN CAP018T-ITEM-SELECIONADO-TRUE
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
           MOVE SPACES TO DESCRICAO-CAD30.
              START CAD030 KEY IS NOT < ALT-CAD30
                    INVALID KEY MOVE "10" TO ST-CAD030.
           MOVE SPACES TO CAP018T-LINDET.
           MOVE ZEROS TO CAP018T-CONT.
           PERFORM UNTIL ST-CAD030 = "10"
              READ CAD030 NEXT RECORD AT
                   END MOVE "10" TO ST-CAD030
              NOT AT END
                ADD 1                  TO CAP018T-CONT
                MOVE DESCRICAO-CAD30   TO CAP018T-LINDET(01: 57)
                MOVE CODIGO-CAD30      TO CAP018T-LINDET(59: 04)
                MOVE "INSERE-LIST"     TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM
           MOVE SPACES TO CAP018T-LINDET.
       ITEM-SELECIONADO SECTION.
           MOVE CAP018T-LINDET(1: 65) TO STRING-1.
           MOVE STRING-1(59: 4) TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(59: 4).
       CLEAR-FLAGS SECTION.
           INITIALIZE CAP018T-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CAP020T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CAP018T-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD030.
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
