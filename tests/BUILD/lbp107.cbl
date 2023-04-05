       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP107.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 14/11/2000.
      *FUNÇÃO: Movimento de REMESSA DE IDENTIFICAÇÃO


       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY IEPX010.
           COPY COPX040.
           COPY LBPX107.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW040.
       COPY IEPW010.
       COPY LBPW107.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP107.CPB".
           COPY "LBP107.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-LBD107             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.


           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           COPY "PARAMETR".
           05  command-line2       pic x(30).
           05  command-line-len    pic x(4) comp-5.
           05  stack-size          pic x(4) comp-5.
           05  flags               pic x(4) comp-5.
           05  tty-cmd             pic x(8).
           05  tty-cmd-len         pic x(04) comp-5.
           05  status-code         pic 9(5) comp-5.
       01  run-unit-id             pic x(8) comp-5.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 MENU-PROGRAMA                PIC X(8).
       01 mensagem                     pic x(200).
       01 tipo-msg                     pic x(01).
       01 resp-msg                     pic x(01).

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
      *    COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "IED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "LBD107" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD107.
           OPEN I-O LBD107.
           OPEN INPUT IED010 COD040.

           IF ST-LBD107 = "35"
              CLOSE LBD107      OPEN OUTPUT LBD107
              CLOSE LBD107      OPEN I-O LBD107
           END-IF.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD107 <> "00"
              MOVE "ERRO ABERTURA LBD107: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD107 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4)  TO CONTRATO-L107
                   MOVE GS-LINDET(6: 2)  TO ITEM-L107
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
                   PERFORM VERIFICA-ITEM
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
                   PERFORM VERIFICA-ITEM
               WHEN GS-VERIFICA-SALDO-TRUE
                   PERFORM CALCULA-ACUM-SALDO
               WHEN GS-PRINTER-FLG-TRUE
                    PERFORM IMPRIMIR
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIMIR SECTION.
           move "LBP215"            to menu-programa

           move menu-programa       to command-line2
           move PARAMETROS-W        to command-line2(10: 20)
           move 30                  to command-line-len
           call "CBL_EXEC_RUN_UNIT" using        command-line2
                                    by value     command-line-len
                                    by reference run-unit-id
                                    by value     stack-size
                                                 flags
                                    by reference tty-cmd
                                    by value     tty-cmd-len
                                    returning    status-code
           END-CALL
           IF STATUS-CODE NOT = ZEROS
              EVALUATE STATUS-CODE
                WHEN 157
                  MOVE "Falta de memória      "  TO MENSAGEM
                  MOVE "C"                       TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
                WHEN 181
                  MOVE "Parâmetros inválido   "  TO MENSAGEM
                  MOVE "C"                       TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
                WHEN 200
                  MOVE "Erro de lógica interna"  TO MENSAGEM
                  MOVE "C"                       TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
              END-EVALUATE
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO
                    PERFORM LE-CONTRATO
           END-EVALUATE.
      *--------------------------------------------------------------
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY INITIALIZE REG-COD040.
           MOVE INSTITUICAO-CO40   TO CODIGO-IE10.
           READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10.
           MOVE NOME-IE10          TO GS-DESC-CONTRATO(1: 15)
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO(17: 14).
      *--------------------------------------------------------------
       VERIFICA-ITEM SECTION.
           MOVE ZEROS TO GS-ITEM.
           MOVE GS-CONTRATO      TO CONTRATO-L107.
           MOVE ZEROS            TO ITEM-L107.
           START LBD107 KEY IS NOT < CHAVE-L107 INVALID KEY
                 MOVE "10" TO ST-LBD107.
           PERFORM UNTIL ST-LBD107 = "10"
             READ LBD107 NEXT RECORD AT END MOVE "10" TO ST-LBD107
               NOT AT END
                IF CONTRATO-L107 <> GS-CONTRATO MOVE "10" TO ST-LBD107
                ELSE
                  MOVE ITEM-L107  TO GS-ITEM
                END-IF
             END-READ
           END-PERFORM.
           ADD 1 TO GS-ITEM.

       CARREGAR-DADOS SECTION.
           START LBD107 KEY IS = CHAVE-L107 INVALID KEY CONTINUE.
           READ LBD107 INVALID KEY INITIALIZE REG-LBD107.
           MOVE DATA-MOVTO-W         TO GS-DATA-REMESSA
           MOVE CONTRATO-L107        TO GS-CONTRATO
           PERFORM LE-CONTRATO.
           MOVE QTDE-FORM-CO40       TO GS-PREV-FORM
           MOVE ITEM-L107            TO GS-ITEM
           MOVE QTDE-IDENT-L107      TO GS-QTDE-IDENT
           MOVE OBSERVACAO-L107      TO GS-OBS
           PERFORM CALCULA-ACUM-SALDO.
       CALCULA-ACUM-SALDO SECTION.
           MOVE GS-CONTRATO      TO CONTRATO-L107 NR-CONTRATO-CO40.
           READ COD040 INVALID KEY INITIALIZE REG-COD040.
           MOVE ZEROS            TO ITEM-L107
           START LBD107 KEY IS NOT < CHAVE-L107 INVALID KEY
                 MOVE "10" TO ST-LBD107.
           MOVE ZEROS TO GS-QTDE-ACUM
           PERFORM UNTIL ST-LBD107 = "10"
             READ LBD107 NEXT RECORD AT END MOVE "10" TO ST-LBD107
               NOT AT END
                IF CONTRATO-L107 <> GS-CONTRATO MOVE "10" TO ST-LBD107
                ELSE
                  ADD QTDE-IDENT-L107     TO GS-QTDE-ACUM
                END-IF
             END-READ
           END-PERFORM.
           MOVE QTDE-FORM-CO40 TO GS-PREV-FORM.
           COMPUTE GS-SALDO = (GS-QTDE-ACUM + GS-QTDE-IDENT) -
                               GS-PREV-FORM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-LBD107
           INITIALIZE GS-DATA-BLOCK
           MOVE DATA-MOVTO-W TO GS-DATA-REMESSA.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

       EXCLUI SECTION.
           MOVE GS-CONTRATO   TO CONTRATO-L107
           MOVE GS-ITEM       TO ITEM-L107
           READ LBD107 INVALID KEY CONTINUE
             NOT INVALID KEY DELETE LBD107.
           PERFORM LIMPAR-DADOS.
      *    PERFORM CARREGA-ULTIMOS.

       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO           TO CONTRATO-L107
           MOVE GS-ITEM               TO ITEM-L107
           READ LBD107 INVALID KEY
                PERFORM MOVER-DADOS
                WRITE REG-LBD107
                END-WRITE
                PERFORM MOVER-DADOS-LISTA
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
             NOT INVALID KEY
                 PERFORM MOVER-DADOS
                 REWRITE REG-LBD107
                 END-REWRITE
                 PERFORM CARREGA-ULTIMOS
           END-READ.

       MOVER-DADOS SECTION.
           MOVE DATA-MOVTO-I          TO DATA-REMESSA-L107
           MOVE GS-QTDE-IDENT         TO QTDE-IDENT-L107
           MOVE GS-OBS                TO OBSERVACAO-L107.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-DATA-REMESSA TO DATA-MOVTO-W DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV        TO DATA-MOVTO-I DATA-REMESSA-L107.
           MOVE ZEROS           TO CONTRATO-L107 ITEM-L107.
           MOVE SPACES TO GS-LINDET
           START LBD107 KEY IS NOT < ALT-L107
                    INVALID KEY MOVE "10" TO ST-LBD107.
           PERFORM UNTIL ST-LBD107 = "10"
              READ LBD107 NEXT RECORD AT END MOVE "10" TO ST-LBD107
              NOT AT END
                IF DATA-REMESSA-L107 <> DATA-MOVTO-I
                   MOVE "10" TO ST-LBD107
                ELSE
                  PERFORM MOVER-DADOS-LISTA
                  MOVE "INSERE-LIST" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE CONTRATO-L107      TO GS-LINDET(1: 5)
           MOVE ITEM-L107          TO GS-LINDET(6: 3)
           MOVE QTDE-IDENT-L107    TO GS-LINDET(9: 5)
           MOVE OBSERVACAO-L107    TO GS-LINDET(14: 40).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP107" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE IED010 COD040 LBD107.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
