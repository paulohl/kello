       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP151.
       DATE-WRITTEN. 14/12/1999.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: Triagem de Patrocínio(compromisso contratual)
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX002.
           COPY COPX040.
           COPY COPX050.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS  CHAVE-WK = BRINDE-WK DATA-VENCTO-WK
                  ALTERNATE RECORD KEY IS DESC-BRINDE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-VENCTO-WK
                                          WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW002.
       COPY COPW040.
       COPY COPW050.
       FD  WORK.
       01  REG-WORK.
           05  BRINDE-WK            PIC 9(3).
           05  DESC-BRINDE-WK       PIC X(20).
           05  DATA-VENCTO-WK       PIC 9(8).
           05  QTDE-BRINDE-PAGO-WK  PIC 9(10).
           05  QTDE-BRINDE-PAGAR-WK PIC 9(10).
      *    QTDE-BRINDE = QTDE-POR-FORM-CO50 * QTDE-FORM-CO50
           05  VLR-TOTAL-PAGO-WK    PIC 9(10)V99.
           05  VLR-TOTAL-PAGAR-WK   PIC 9(10)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP151.CPB".
           COPY "COP151.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VALOR-E           PIC Z.ZZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  QTDE-E                PIC Z.ZZZ.ZZZ.ZZZ BLANK WHEN ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.
      *    VARIAVEIS P/ AUXILIAR NA TOTALIZACAO
           05  DESC-BRINDE-ANT       PIC X(20)    VALUE SPACES.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  DATA-VENCTO-ANT       PIC 9(8)     VALUE ZEROS.
           05  QTDE-BRINDE           PIC 9(6)     VALUE ZEROS.
           05  CUSTO-UNITARIO        PIC 9(8)V99  VALUE ZEROS.
           05  QT-BRINDE-PAGO-TOT    PIC 9(10)    VALUE ZEROS.
           05  QT-BRINDE-PAGO-TOTG   PIC 9(10)    VALUE ZEROS.
           05  QT-BRINDE-PAGAR-TOT   PIC 9(10)    VALUE ZEROS.
           05  QT-BRINDE-PAGAR-TOTG  PIC 9(10)    VALUE ZEROS.
           05  VLR-TOTAL-PAGO-TOT    PIC 9(10)V99  VALUE ZEROS.
           05  VLR-TOTAL-PAGO-TOTG   PIC 9(10)V99  VALUE ZEROS.
           05  VLR-TOTAL-PAGAR-TOT   PIC 9(10)V99  VALUE ZEROS.
           05  VLR-TOTAL-PAGAR-TOTG  PIC 9(10)V99  VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(64)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(43)   VALUE
           "TRIAGEM DE COMPROMISSO CONTRATUAL - ORDEM: ".
           05  ORDEM-REL           PIC X(18)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "INT.VENCTO: ".
           05  DATA-VENCTO-INI     PIC ZZ/ZZ/ZZZZ.
           05  FILLER              PIC X(3)    VALUE " a ".
           05  DATA-VENCTO-FIM     PIC ZZ/ZZ/ZZZZ.
       01  CAB03.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(111)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(111)  VALUE
           "DATA-PREV.  COD  COMP.CONTRATUAL                QTDE
      -    " VLR-TOTAL           QTDE         VLR-TOTAL".
       01  LINDET.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  LINDET-REL          PIC X(111)  VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD050.
           OPEN INPUT COD040 COD050 COD002.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD050 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-BRINDE-TRUE
                    PERFORM LER-BRINDE
               WHEN GS-POPUP-BRINDE-TRUE
                    PERFORM POPUP-BRINDE
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LER-BRINDE SECTION.
           MOVE GS-CODIGO    TO CODIGO-CO02.
           READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02.
           MOVE NOME-CO02    TO GS-DESCRICAO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       POPUP-BRINDE SECTION.
           CALL   "COP002T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "COP002T".
           MOVE PASSAR-STRING-1(1: 20)  TO GS-DESCRICAO.
           MOVE PASSAR-STRING-1(33: 3)  TO GS-CODIGO.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-VENCTO-INI  TO DATA-INV DATA-VENCTO-INI
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO VENCTO-INI
           MOVE GS-VENCTO-FIM  TO DATA-INV DATA-VENCTO-FIM
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO VENCTO-FIM
           MOVE VENCTO-INI     TO DATA-VENCTO-CO50
           START COD050 KEY IS NOT < DATA-VENCTO-CO50 INVALID KEY
                    MOVE "10" TO ST-COD050.

           PERFORM UNTIL ST-COD050 = "10"
             READ COD050 NEXT RECORD AT END MOVE "10" TO ST-COD050
              NOT AT END
               IF GS-OPCAO = 3 OR GS-OPCAO = SUSP-PREV-DEF-CO50
                  IF GS-CODIGO = 0 OR GS-CODIGO = CODBRINDE-CO50
                     MOVE DATA-VENCTO-CO50 TO GS-EXIBE-VENCTO
                     MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     IF DATA-VENCTO-CO50 > VENCTO-FIM
                        MOVE "10" TO ST-COD050
                     ELSE
                        IF GS-PAGO = 1 AND REALIZADO-CO50 = 1
                           PERFORM MOVER-DADOS-WORK
                        ELSE
                           IF GS-PAGAR = 1 AND REALIZADO-CO50 <> 1
                              PERFORM MOVER-DADOS-WORK
                           END-IF
                        END-IF
                    END-IF
                  END-IF
               END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           MOVE NR-CONTRATO-CO50 TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE 0 TO STATUS-CO40.
           IF STATUS-CO40 < 50 CONTINUE
           ELSE
              MOVE DATA-VENCTO-CO50   TO DATA-VENCTO-WK
              MOVE CODBRINDE-CO50     TO BRINDE-WK CODIGO-CO02
              READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02
              END-READ
              MOVE NOME-CO02          TO DESC-BRINDE-WK
              IF MULT-FORM-CO02 = 2
                 MOVE QTDE-POR-FORM-CO50 TO QTDE-BRINDE
              ELSE COMPUTE QTDE-BRINDE = QTDE-POR-FORM-CO50 *
                            QTDE-FORM-CO50
              END-IF
              IF CUSTO-UNIT-CO50 <> ZEROS
                 MOVE CUSTO-UNIT-CO50 TO CUSTO-UNITARIO
              ELSE MOVE VALOR-CO02 TO CUSTO-UNITARIO
              END-IF
              IF REALIZADO-CO50 = 1
                 READ WORK
                   INVALID KEY
                      MOVE QTDE-BRINDE   TO QTDE-BRINDE-PAGO-WK
                      COMPUTE VLR-TOTAL-PAGO-WK = QTDE-BRINDE *
                                                  CUSTO-UNITARIO
                      MOVE ZEROS         TO QTDE-BRINDE-PAGAR-WK
                                            VLR-TOTAL-PAGAR-WK
                      WRITE REG-WORK
                      END-WRITE
                   NOT INVALID KEY
                      COMPUTE VLR-TOTAL-PAGO-WK = VLR-TOTAL-PAGO-WK +
                              (QTDE-BRINDE * CUSTO-UNITARIO)
                      ADD QTDE-BRINDE    TO QTDE-BRINDE-PAGO-WK
                      REWRITE REG-WORK
                      END-REWRITE
              ELSE
                 READ WORK
                   INVALID KEY
                      MOVE QTDE-BRINDE   TO QTDE-BRINDE-PAGAR-WK
                      COMPUTE VLR-TOTAL-PAGAR-WK = QTDE-BRINDE *
                                                  CUSTO-UNITARIO
                      MOVE ZEROS         TO QTDE-BRINDE-PAGO-WK
                                            VLR-TOTAL-PAGO-WK
                      WRITE REG-WORK
                      END-WRITE
                   NOT INVALID KEY
                      COMPUTE VLR-TOTAL-PAGAR-WK = VLR-TOTAL-PAGAR-WK +
                              (QTDE-BRINDE * CUSTO-UNITARIO)
                      ADD QTDE-BRINDE    TO QTDE-BRINDE-PAGAR-WK
                      REWRITE REG-WORK
                      END-REWRITE
           END-IF.
      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           INITIALIZE REG-WORK.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA

      *    TOTALIZA GERAL
           MOVE SPACES         TO GS-LINDET

           MOVE "Total Geral..."     TO GS-LINDET(1: 30)
           MOVE QT-BRINDE-PAGO-TOTG  TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(40: 15)
           MOVE VLR-TOTAL-PAGO-TOTG  TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(55: 18)
           MOVE QT-BRINDE-PAGAR-TOTG TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(73: 15)
           MOVE VLR-TOTAL-PAGAR-TOTG TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(88: 16)

           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "COMP.CONTRAT" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DESC-BRINDE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "DATA-VENCTO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DATA-VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF DESC-BRINDE-ANT NOT = SPACES
                 IF DESC-BRINDE-ANT NOT = DESC-BRINDE-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF DATA-VENCTO-ANT  NOT = ZEROS
                 IF DATA-VENCTO-ANT NOT = DATA-VENCTO-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
      *    INSERIR LINHA NA LIST BOX
           MOVE DATA-VENCTO-WK       TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO DATA-E
           MOVE DATA-E               TO GS-LINDET(1: 12)
           MOVE BRINDE-WK            TO GS-LINDET(13: 5)
           MOVE DESC-BRINDE-WK       TO GS-LINDET(18: 22)
           MOVE QTDE-BRINDE-PAGO-WK  TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(40: 15)
           ADD QTDE-BRINDE-PAGO-WK   TO QT-BRINDE-PAGO-TOT
           MOVE VLR-TOTAL-PAGO-WK    TO VALOR-E
           ADD VLR-TOTAL-PAGO-WK     TO VLR-TOTAL-PAGO-TOT
           MOVE VALOR-E              TO GS-LINDET(55: 18)
           MOVE QTDE-BRINDE-PAGAR-WK TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(73: 15)
           ADD QTDE-BRINDE-PAGAR-WK  TO QT-BRINDE-PAGAR-TOT
           MOVE VLR-TOTAL-PAGAR-WK   TO VALOR-E
           ADD VLR-TOTAL-PAGAR-WK    TO VLR-TOTAL-PAGAR-TOT
           MOVE VALOR-E              TO GS-LINDET(88: 16)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-CHAVE-ANT SECTION.
           MOVE DESC-BRINDE-WK       TO DESC-BRINDE-ANT.
           MOVE DATA-VENCTO-WK       TO DATA-VENCTO-ANT.
       TOTALIZA SECTION.
           MOVE SPACES               TO GS-LINDET
           MOVE "Total..."           TO GS-LINDET(1: 30)
           MOVE QT-BRINDE-PAGO-TOT   TO QTDE-E
           ADD QT-BRINDE-PAGO-TOT    TO QT-BRINDE-PAGO-TOTG
           MOVE QTDE-E               TO GS-LINDET(40: 15)
           MOVE VLR-TOTAL-PAGO-TOT   TO VALOR-E
           ADD VLR-TOTAL-PAGO-TOT    TO VLR-TOTAL-PAGO-TOTG
           MOVE VALOR-E              TO GS-LINDET(55: 18)
           MOVE QT-BRINDE-PAGAR-TOT  TO QTDE-E
           ADD QT-BRINDE-PAGAR-TOT   TO QT-BRINDE-PAGAR-TOTG
           MOVE QTDE-E               TO GS-LINDET(73: 15)
           MOVE VLR-TOTAL-PAGAR-TOT  TO VALOR-E
           ADD VLR-TOTAL-PAGAR-TOT   TO VLR-TOTAL-PAGAR-TOTG
           MOVE VALOR-E              TO GS-LINDET(88: 16)

           MOVE ZEROS TO QT-BRINDE-PAGO-TOT QT-BRINDE-PAGAR-TOT
                         VLR-TOTAL-PAGO-TOT VLR-TOTAL-PAGAR-TOT
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO QT-BRINDE-PAGO-TOT QT-BRINDE-PAGAR-TOT
                         VLR-TOTAL-PAGO-TOT VLR-TOTAL-PAGAR-TOT.
           MOVE ZEROS TO QT-BRINDE-PAGO-TOTG QT-BRINDE-PAGAR-TOTG
                         VLR-TOTAL-PAGO-TOTG VLR-TOTAL-PAGAR-TOTG
           MOVE SPACES TO DESC-BRINDE-ANT.
           MOVE ZEROS TO DATA-VENCTO-ANT.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP151" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           INITIALIZE REG-WORK.
           MOVE SPACES TO DESC-BRINDE-ANT.
           MOVE ZEROS TO DATA-VENCTO-ANT CONTRATO-ANT.
           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

      *    TOTALIZA GERAL
           MOVE SPACES           TO LINDET-REL
           MOVE "Total Geral..." TO LINDET-REL(1: 30)

           MOVE QT-BRINDE-PAGO-TOTG  TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(40: 15)
           MOVE VLR-TOTAL-PAGO-TOTG  TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 18)
           MOVE QT-BRINDE-PAGAR-TOTG TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(73: 15)
           MOVE VLR-TOTAL-PAGAR-TOTG TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(88: 16)

           WRITE REG-RELAT FROM LINDET

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF DESC-BRINDE-ANT NOT = SPACES
                 IF DESC-BRINDE-ANT NOT = DESC-BRINDE-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF DATA-VENCTO-ANT  NOT = ZEROS
                 IF DATA-VENCTO-ANT NOT = DATA-VENCTO-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
      *    IMPRIMIR LINHA DETALHE


           MOVE DATA-VENCTO-WK       TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO DATA-E
           MOVE DATA-E               TO LINDET-REL(1: 12)
           MOVE BRINDE-WK            TO LINDET-REL(13: 5)
           MOVE DESC-BRINDE-WK       TO LINDET-REL(18: 22)
           MOVE QTDE-BRINDE-PAGO-WK  TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(40: 15)
           ADD QTDE-BRINDE-PAGO-WK   TO QT-BRINDE-PAGO-TOT
           MOVE VLR-TOTAL-PAGO-WK    TO VALOR-E
           ADD VLR-TOTAL-PAGO-WK     TO VLR-TOTAL-PAGO-TOT
           MOVE VALOR-E              TO LINDET-REL(55: 18)
           MOVE QTDE-BRINDE-PAGAR-WK TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(73: 15)
           ADD QTDE-BRINDE-PAGAR-WK  TO QT-BRINDE-PAGAR-TOT
           MOVE VLR-TOTAL-PAGAR-WK   TO VALOR-E
           ADD VLR-TOTAL-PAGAR-WK    TO VLR-TOTAL-PAGAR-TOT
           MOVE VALOR-E              TO LINDET-REL(88: 16)

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 55 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES        TO LINDET-REL
           MOVE "Total..."    TO LINDET-REL(1: 30)

           MOVE QT-BRINDE-PAGO-TOT   TO QTDE-E
           ADD QT-BRINDE-PAGO-TOT    TO QT-BRINDE-PAGO-TOTG
           MOVE QTDE-E               TO LINDET-REL(40: 15)
           MOVE VLR-TOTAL-PAGO-TOT   TO VALOR-E
           ADD VLR-TOTAL-PAGO-TOT    TO VLR-TOTAL-PAGO-TOTG
           MOVE VALOR-E              TO LINDET-REL(55: 18)
           MOVE QT-BRINDE-PAGAR-TOT  TO QTDE-E
           ADD QT-BRINDE-PAGAR-TOT   TO QT-BRINDE-PAGAR-TOTG
           MOVE QTDE-E               TO LINDET-REL(73: 15)
           MOVE VLR-TOTAL-PAGAR-TOT  TO VALOR-E
           ADD VLR-TOTAL-PAGAR-TOT   TO VLR-TOTAL-PAGAR-TOTG
           MOVE VALOR-E              TO LINDET-REL(88: 16)

           WRITE REG-RELAT FROM LINDET.

           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT
           ADD 2 TO LIN.
           IF LIN > 55 PERFORM CABECALHO.
           MOVE ZEROS TO QT-BRINDE-PAGO-TOT QT-BRINDE-PAGAR-TOT
                         VLR-TOTAL-PAGO-TOT VLR-TOTAL-PAGAR-TOT.

       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 COD050 COD002.
           CLOSE WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
