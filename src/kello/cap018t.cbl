       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAP018T.
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
           COPY CAPX018.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW018.
       WORKING-STORAGE SECTION.
           COPY "CAP018T.CPB".
           COPY "CAP018T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
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
       01  STRING-1               PIC X(65) VALUE SPACES.

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CAP018T-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE "00" TO STRING-1(33: 2).
           INITIALIZE CAP018T-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CAP018T-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CAP018T-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           OPEN INPUT CAD018
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO CAP018T-MENSAGEM-ERRO
              MOVE ST-CAD018 TO CAP018T-MENSAGEM-ERRO(23: 02)
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
           MOVE SPACES TO NOME-PORT.
              START CAD018 KEY IS NOT < NOME-PORT
                    INVALID KEY MOVE "10" TO ST-CAD018.
           MOVE SPACES TO CAP018T-LINDET.
           MOVE ZEROS TO CAP018T-CONT.
           PERFORM UNTIL ST-CAD018 = "10"
              READ CAD018 NEXT RECORD AT END MOVE "10" TO ST-CAD018
              NOT AT END
                ADD 1                  TO CAP018T-CONT
                MOVE NOME-PORT         TO CAP018T-LINDET(01: 30)
                MOVE PORTADOR          TO CAP018T-LINDET(33: 04)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           MOVE CAP018T-LINDET(1: 40) TO STRING-1.
           MOVE STRING-1(33: 4) TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(33: 4).
       CLEAR-FLAGS SECTION.
           INITIALIZE CAP018T-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CAP018T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CAP018T-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD018.
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
