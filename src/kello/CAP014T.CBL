       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAP014T.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX014.

           SELECT CONTROLE ASSIGN "CONTROLE.DAT"
                  ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CONTROLE.
       01  REG-CONTROLE.
           05  EMPRESA       PIC XXX.
           05  NOME-EMP      PIC X(30).
           05  NOME-EMP-RED  PIC X(15).
       COPY CAPW014.
       WORKING-STORAGE SECTION.
           COPY "CAP014T.CPB".
           COPY "CAP014T.CPY".
           COPY "DS-CNTRL.MF".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD014             PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  CODIGO-W              PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

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
           MOVE GS-LINDET(01: 40) TO STRING-1
           PERFORM FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE "00" TO STRING-1(36: 2)
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD014" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD014.
           OPEN INPUT CAD014
           IF ST-CAD014 <> "00"
              MOVE "ERRO ABERTURA CAD014: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD014 TO GS-MENSAGEM-ERRO(23: 02)
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
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD014.
           move ds-quit-set to ds-control
           CALL "DSRUN" USING DS-CONTROL-BLOCK,
                              GS-DATA-BLOCK.
           EXIT PROGRAM.

       CARREGA-MENSAGEM-ERRO SECTION.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *LIMPAR-DADOS SECTION.
      *    INITIALIZE REG-CAD014
      *    INITIALIZE GS-DATA-BLOCK
      *    PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO DEPARTAMENTO-CA14
              START CAD014 KEY IS NOT < DEPARTAMENTO-CA14
                    INVALID KEY MOVE "10" TO ST-CAD014.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-CAD014 = "10"
              READ CAD014 NEXT RECORD AT END MOVE "10" TO ST-CAD014
              NOT AT END
                ADD 1 TO GS-CONT
                MOVE DEPARTAMENTO-CA14      TO GS-LINDET(01: 35)
                MOVE CODIGO-CA14            TO GS-LINDET(36: 02)
                MOVE "INSERE-LIST"          TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(1: 40) TO STRING-1
           MOVE STRING-1(36: 2) TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(36: 2).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CAP014T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             PERFORM FINALIZAR-PROGRAMA
           END-IF.

