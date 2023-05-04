       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP008T.
      * AUTORA: ALFREDO SAVIOLLI NETO
      * DATA: 09/02/2005
      * FUNCAO: CONSULTA POP-UP MOTIVOS
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX008.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW008.
       WORKING-STORAGE SECTION.
           COPY "COP008T.CPB".
           COPY "COP008T.CPY".
           COPY "DS-CNTRL.MF".
      *    COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD008             PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(65)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(3)   VALUE SPACES.
           05  WS-TIPO               PIC 9(01)  VALUE ZEROS.

       01 MENSAGEM                   PIC X(200).
       01 RESP-MSG                   PIC X(01).
       01 TIPO-MSG                   PIC X(01).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
       01  STRING-1               PIC X(65) VALUE SPACES.

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE STRING-1(65: 1)          TO WS-TIPO
           MOVE "000"                    TO STRING-1(62: 3).
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "COD008"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD008.
           OPEN INPUT COD008
           IF ST-COD008 <> "00"
              MOVE "ERRO ABERTURA COD008: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD008 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN GS-ITEM-SELECIONADO-TRUE
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
           MOVE SPACES             TO DESCRICAO-CO08
              START COD008 KEY IS NOT < ALT-CO08
                    INVALID KEY MOVE "10" TO ST-COD008.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-COD008 = "10"
              READ COD008 NEXT RECORD AT END MOVE "10" TO ST-COD008
              NOT AT END
                   ADD 1                  TO GS-CONT

                   MOVE CODIGO-CO08       TO GS-LINDET(32:3)
                   MOVE DESCRICAO-CO08    TO GS-LINDET(1:30)

                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       ITEM-SELECIONADO SECTION.
           MOVE GS-LINHA-AUXILIAR      TO STRING-1.
           MOVE STRING-1(32: 3)        TO CODIGO-W.
           IF CODIGO-W = SPACES
              MOVE ZEROS TO STRING-1(32: 3).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP008T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD008.
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
