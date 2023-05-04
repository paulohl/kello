       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP002T.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
       DATE-WRITTEN. 01/03/2000.
      * FUNCAO: CONSULTA POP-UP DE FUNÇÕES - REPORTAGEM
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY REPX002.

       DATA DIVISION.
       FILE SECTION.
       COPY REPW002.
       WORKING-STORAGE SECTION.
           COPY "REP002T.CPB".
           COPY "REP002T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RED002             PIC XX       VALUE SPACES.
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
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE "00" TO STRING-1(22: 2).
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "RED002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED002.
           OPEN INPUT RED002
           IF ST-RED002 <> "00"
              MOVE "ERRO ABERTURA RED002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED002 TO GS-MENSAGEM-ERRO(23: 02)
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
           MOVE SPACES TO DESCRICAO-RE02.
              START RED002 KEY IS NOT < DESCRICAO-RE02
                    INVALID KEY MOVE "10" TO ST-RED002.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-RED002 = "10"
              READ RED002 NEXT RECORD AT END
                   MOVE "10" TO ST-RED002
              NOT AT END
                   ADD 1                  TO GS-CONT
                   MOVE DESCRICAO-RE02    TO GS-LINDET(01: 20)
                   MOVE CODIGO-RE02       TO GS-LINDET(22: 02)
                   EVALUATE ACUMULAR-RE02
                      WHEN 1 MOVE "1-Coordenação"   TO GS-LINDET(25:20)
                      WHEN 2 MOVE "2-Fotografo"     TO GS-LINDET(25:20)
                      WHEN 3 MOVE "3-Cinegrafista"  TO GS-LINDET(25:20)
                      WHEN 4 MOVE "4-Auxiliar"      TO GS-LINDET(25:20)
                      WHEN 5 MOVE "5-Material"      TO GS-LINDET(25:20)
                      WHEN 6 MOVE "6-Locação KM"    TO GS-LINDET(25:20)
                      WHEN 7 MOVE "7-Aluguel"       TO GS-LINDET(25:20)
                      WHEN 8 MOVE "8-Outros"        TO GS-LINDET(25:20)
                      WHEN OTHER MOVE SPACES        TO GS-LINDET(25:20)
                   END-EVALUATE
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(1: 40) TO STRING-1.
           MOVE STRING-1(22: 2) TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(22: 2).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP002T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE RED002.
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
