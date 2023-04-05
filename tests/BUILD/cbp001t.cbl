       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP001T.
      * FUNCTION - POP UP - CADASTRO DE BANCOS
      * AUTHOR: MARELI AMANCIO VOLPATO
      * DATE: 21/10/1998
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CBPX001.

       DATA DIVISION.
       FILE SECTION.
       COPY CBPW001.
       WORKING-STORAGE SECTION.
           COPY "CBP001T.CPB".
           COPY "CBP001T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CBD001             PIC XX       VALUE SPACES.
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
           PERFORM CORPO-PROGRAMA UNTIL CBP001T-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE "000000" TO STRING-1(49: 6).
           INITIALIZE CBP001T-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CBP001T-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CBP001T-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CBD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CBD001.
           OPEN INPUT CBD001.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO CBP001T-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CBP001T-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CBP001T-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CBP001T-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN CBP001T-ITEM-SELECIONADO-TRUE
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
           MOVE ZEROS TO CBP001T-CONT.
           MOVE SPACES TO TITULAR-CB01.
           START CBD001 KEY IS NOT < TITULAR-CB01
                    INVALID KEY MOVE "10" TO ST-CBD001.
           MOVE SPACES TO CBP001T-LINDET.
           PERFORM UNTIL ST-CBD001 = "10"
              READ CBD001 NEXT RECORD AT END MOVE "10" TO ST-CBD001
              NOT AT END
                ADD 1 TO CBP001T-CONT
                MOVE TITULAR-CB01         TO CBP001T-LINDET(01: 15)
                MOVE NOME-BANCO-CB01      TO CBP001T-LINDET(17: 15)
                MOVE NR-CONTA-CB01        TO CBP001T-LINDET(33: 15)
                MOVE CODIGO-FORN-CB01     TO CBP001T-LINDET(49: 06)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       ITEM-SELECIONADO SECTION.
           MOVE CBP001T-LINDET(1: 60) TO STRING-1.
           MOVE STRING-1(49: 6)       TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(49: 6).
       CLEAR-FLAGS SECTION.
           INITIALIZE CBP001T-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CBP001T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CBP001T-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CBD001.
           move ds-quit-set to ds-control.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
