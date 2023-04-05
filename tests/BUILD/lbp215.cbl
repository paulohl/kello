       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP215.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 14/11/2000.
      *FUNÇÃO: GUIA de REMESSA DE IDENTIFICAÇÃO


       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

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
           COPY "LBP215.CPB".
           COPY "LBP215.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
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
           05  LIN                   PIC 9(2)     VALUE ZEROS.
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
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  ITEM-W                PIC 9(2)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  QTDE-ACUM-W           PIC 9(4)     VALUE ZEROS.
           05  SALDO-W               PIC S9(4)    VALUE ZEROS.
           05  SALDO-E               PIC ZZZZ-    BLANK WHEN ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

      *--------------------------------------------------------------
       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(43)   VALUE
           "GUIA DE REMESSA DE IDENTIFICACAO           ".
           05  ORDEM-REL           PIC X(50)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(100) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(100)  VALUE
           "DATA-REMES CONT IT INSTITUICAO     IDEN ACUM PREV SALDO OBSE
      -    "RVACAO                              .".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(100)  VALUE SPACES.
      *----------------------------------------------------------------
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
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
               WHEN GS-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           INITIALIZE QTDE-ACUM-W
           EVALUATE GS-ORDER
             WHEN 1 MOVE GS-DATA-REMESSA-INI  TO DATA-INV
                    CALL "GRIDAT2" USING DATA-INV
                    MOVE DATA-INV  TO DATA-INI
                    MOVE GS-DATA-REMESSA-FIM  TO DATA-INV
                    CALL "GRIDAT2" USING DATA-INV
                    MOVE DATA-INV  TO DATA-FIM
                    MOVE DATA-INI  TO DATA-REMESSA-L107
                    MOVE ZEROS     TO CONTRATO-L107 ITEM-L107
                    START LBD107 KEY IS NOT < ALT-L107 INVALID KEY
                          MOVE "10" TO ST-LBD107
                    END-START
             WHEN 2 MOVE GS-CONTRATO-INI   TO CONTRATO-L107
                    MOVE ZEROS             TO ITEM-L107
                    START LBD107 KEY IS NOT < CHAVE-L107 INVALID KEY
                          MOVE "10" TO ST-LBD107
                    END-START
           END-EVALUATE.
           MOVE ZEROS TO GS-CONT CONTRATO-ANT.
           PERFORM UNTIL ST-LBD107 = "10"
             READ LBD107 NEXT RECORD AT END MOVE "10" TO ST-LBD107
               NOT AT END
                 EVALUATE GS-ORDER
                   WHEN 1 IF DATA-REMESSA-L107 > DATA-FIM
                             MOVE "10" TO ST-LBD107
                          ELSE
                            PERFORM MOVER-DADOS-REL
                            MOVE "INSERE-LIST" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                          END-IF
                   WHEN 2 IF CONTRATO-L107 > GS-CONTRATO-FIM
                             MOVE "10" TO ST-LBD107
                          ELSE
                            PERFORM MOVER-DADOS-REL
                            MOVE "INSERE-LIST" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                          END-IF
                 END-EVALUATE
             END-READ
           END-PERFORM.

       MOVER-DADOS-REL SECTION.
           MOVE DATA-REMESSA-L107     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV              TO DATA-E
           MOVE DATA-E                TO GS-LINDET(1: 11)
           MOVE CONTRATO-L107         TO GS-LINDET(12: 5)
                                         NR-CONTRATO-CO40
           MOVE ITEM-L107             TO GS-LINDET(17: 3)
           READ COD040 INVALID KEY
                MOVE ZEROS            TO INSTITUICAO-CO40.
           MOVE INSTITUICAO-CO40      TO CODIGO-IE10
           READ IED010 INVALID KEY
                MOVE SPACES           TO NOME-IE10.
           MOVE NOME-IE10(1: 15)      TO GS-LINDET(20: 16)
           MOVE QTDE-IDENT-L107       TO GS-LINDET(36: 5)
           MOVE OBSERVACAO-L107       TO GS-LINDET(57: 40)
           MOVE CONTRATO-L107         TO CONTRATO-W
           MOVE ITEM-L107             TO ITEM-W
           IF CONTRATO-L107 = CONTRATO-ANT
              ADD QTDE-IDENT-L107     TO QTDE-ACUM-W
           ELSE
              MOVE QTDE-IDENT-L107    TO QTDE-ACUM-W
              MOVE CONTRATO-L107      TO CONTRATO-ANT
           END-IF
           MOVE QTDE-ACUM-W           TO GS-LINDET(41: 5)
           MOVE QTDE-FORM-CO40        TO GS-LINDET(46: 5)
           COMPUTE SALDO-W = QTDE-ACUM-W - QTDE-FORM-CO40.
           MOVE SALDO-W               TO SALDO-E
           MOVE SALDO-E               TO GS-LINDET(51: 6).

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP215" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           INITIALIZE QTDE-ACUM-W

           COPY CONDENSA.

           EVALUATE GS-ORDER
             WHEN 1 MOVE DATA-INI  TO DATA-REMESSA-L107
                    MOVE ZEROS     TO CONTRATO-L107 ITEM-L107
                    START LBD107 KEY IS NOT < ALT-L107 INVALID KEY
                          MOVE "10" TO ST-LBD107
                    END-START
                    MOVE SPACES              TO ORDEM-REL
                    MOVE "INT.DATA: "        TO ORDEM-REL(1: 10)
                    MOVE GS-DATA-REMESSA-INI TO DATA-E
                    MOVE DATA-E              TO ORDEM-REL(11: 11)
                    MOVE "a "                TO ORDEM-REL(22: 2)
                    MOVE GS-DATA-REMESSA-FIM TO DATA-E
                    MOVE DATA-E              TO GS-LINDET(24: 10)
             WHEN 2 MOVE GS-CONTRATO-INI   TO CONTRATO-L107
                    MOVE ZEROS             TO ITEM-L107
                    START LBD107 KEY IS NOT < CHAVE-L107 INVALID KEY
                          MOVE "10" TO ST-LBD107
                    END-START
                    MOVE SPACES              TO ORDEM-REL
                    MOVE "CONTRATO: "        TO ORDEM-REL(1: 10)
                    MOVE GS-CONTRATO-INI     TO ORDEM-REL(11: 05)
                    MOVE "a "                TO ORDEM-REL(17: 2)
                    MOVE GS-CONTRATO-FIM     TO GS-LINDET(19: 05)
           END-EVALUATE.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-LBD107 = "10"
               READ LBD107 NEXT RECORD AT END
                    MOVE "10" TO ST-LBD107
               NOT AT END
                    EVALUATE GS-ORDER
                      WHEN 1 IF DATA-REMESSA-L107 > DATA-FIM
                                MOVE "10" TO ST-LBD107
                             ELSE
                                PERFORM MOVER-DADOS-REL
                                MOVE GS-LINDET   TO LINDET-REL
                                WRITE REG-RELAT FROM LINDET
                                ADD 1 TO LIN
                                IF LIN > 56
                                   PERFORM CABECALHO
                                END-IF
                             END-IF
                      WHEN 2 IF CONTRATO-L107 > GS-CONTRATO-FIM
                                MOVE "10" TO ST-LBD107
                             ELSE
                                PERFORM MOVER-DADOS-REL
                                MOVE GS-LINDET   TO LINDET-REL
                                WRITE REG-RELAT FROM LINDET
                                ADD 1 TO LIN
                                IF LIN > 56
                                   PERFORM CABECALHO
                                END-IF
                             END-IF
                    END-EVALUATE
               END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
      *----------------------------------------------------------
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
