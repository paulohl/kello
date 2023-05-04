       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP040X.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 07/10/1999
      *DESCRIÇÃO: Gera arquivo de CONTRATO P/ POPUP NO EXTRATO
      *  arquivo   - COD040X
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX040.
           COPY COPX040X.
           COPY IEPX010.
           COPY CAPX010.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW040.
       COPY COPW040X.
       COPY IEPW010.
       COPY CAPW010.

       WORKING-STORAGE SECTION.
           COPY "COP040X.CPB".
           COPY "COP040X.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD040X            PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W(FLAG)- PARA SABER SE OCORREU ERRO-ABERTURA ARQUIVO
      *  ERRO-W = 0 (NÃO)  ERRO-W = 1 (SIM)
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  VALORE-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE ENTRADA
           05  VALORS-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE SAIDA
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD040X" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040X.
           MOVE "IED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           OPEN INPUT IED010 CAD010 COD040.
           OPEN I-O COD040X.
           IF ST-COD040X = "35"
              CLOSE COD040X      OPEN OUTPUT COD040X
              CLOSE COD040X      OPEN I-O COD040X
           END-IF.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040X <> "00"
              MOVE "ERRO ABERTURA COD040X " TO GS-MENSAGEM-ERRO
              MOVE ST-COD040X TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-GERA-FLG-TRUE
                   PERFORM GERA-ARQUIVO
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GERA-ARQUIVO SECTION.
           CLOSE COD040X.  OPEN OUTPUT COD040X.
           MOVE ZEROS TO SEQ-CO40X.
           MOVE "Gerando arquivo: " TO GS-MENSAGEM(01: 17).
           MOVE ZEROS TO NR-CONTRATO-CO40.
           START COD040 KEY IS NOT < NR-CONTRATO-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.
           PERFORM UNTIL ST-COD040 = "10"
             READ COD040 NEXT RECORD AT END MOVE "10" TO ST-COD040
               NOT AT END
                  ADD 1 TO SEQ-CO40X
                  MOVE NR-CONTRATO-CO40   TO GS-MENSAGEM(18: 10)
                  MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  MOVE NR-CONTRATO-CO40   TO NR-CONTRATO-CO40X
                  MOVE IDENTIFICACAO-CO40 TO IDENTIFICACAO-CO40X
                  MOVE MESANO-PREV-CO40(1: 4) TO MESANO-W(3: 4)
                  MOVE MESANO-PREV-CO40(5: 2) TO MESANO-W(1: 2)
                  MOVE MESANO-W               TO MESANO-CO40X
                  MOVE INSTITUICAO-CO40   TO CODIGO-IE10
                  READ IED010 INVALID KEY MOVE SPACES TO SIGLA-IE10
                  END-READ
                  MOVE SIGLA-IE10         TO INSTITUICAO-CO40X
                  MOVE CIDADE-CO40        TO CIDADE
                  READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
                  END-READ
                  MOVE NOME-CID           TO CIDADE-CO40X
                  WRITE REG-COD040X
                  END-WRITE
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-MENSAGEM.
           MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           MOVE 1 TO ERRO-W.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-COD040X.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-COD040       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP040X" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 IED010 CAD010 COD040X.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

