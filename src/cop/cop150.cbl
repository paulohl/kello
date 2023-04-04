       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP150.
       DATE-WRITTEN. 09/09/1999.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: Relatório de compromisso contratual
      * Por data vencto do brinde - ordem: contrato, brinde, data vencto
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX012.
           COPY COPX002.
           COPY COPX040.
           COPY COPX050.
           COPY CAPX010.

           SELECT WORK     ASSIGN       TO        VARIA-W
                           ORGANIZATION IS        INDEXED
                           ACCESS MODE  IS        DYNAMIC
                           STATUS       IS        ST-WORK
                           RECORD KEY   IS     CHAVE-WK =
                                              CONTRATO-WK
                                                  ITEM-WK
                           ALTERNATE    RECORD    KEY  IS
                           CONTRATO-WK  WITH DUPLICATES
                           ALTERNATE    RECORD    KEY  IS
                           DESC-BRINDE-WK WITH DUPLICATES
                           ALTERNATE    RECORD    KEY  IS
                           DATA-VENCTO-WK WITH DUPLICATES
                           ALTERNATE    RECORD    KEY  IS
                           DATA-PAGTO-WK  WITH DUPLICATES
                           ALTERNATE    RECORD    KEY  IS
                           DATA-ENVIO-WK  WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW012.
       COPY CAPW010.
       COPY COPW002.
       COPY COPW040.
       COPY COPW050.

       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  ITEM-WK             PIC 9(4).
           05  BRINDE-WK           PIC 9(3).
           05  DESC-BRINDE-WK      PIC X(20).
           05  DATA-VENCTO-WK      PIC 9(8).
           05  QTDE-BRINDE-WK      PIC 9(5).
      *    QTDE-BRINDE = QTDE-POR-FORM-CO50 * QTDE-FORM-CO50
           05  VLR-UNIT-WK         PIC 9(8)V99.
           05  VLR-TOTAL-WK        PIC 9(8)V99.
           05  IDENTIFICACAO-WK    PIC X(13).
           05  CIDADE-WK           PIC X(6).
           05  QT-FORM-WK          PIC 9(4).
           05  PADRAO-WK           PIC X.
           05  MESANO-WK           PIC 9(6).
           05  DATA-PAGTO-WK       PIC 9(8).
           05  DATA-ENVIO-WK       PIC 9(8).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP150.CPB".
           COPY "COP150.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
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
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  QTDE-E                PIC ZZ.ZZZ.ZZZ BLANK WHEN ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  QTDE-E1               PIC ZZZZZ.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.
      *    VARIAVEIS P/ AUXILIAR NA TOTALIZACAO
           05  DESC-BRINDE-ANT       PIC X(20)    VALUE SPACES.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  DATA-VENCTO-ANT       PIC 9(8)     VALUE ZEROS.
           05  DATA-PAGTO-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-ENVIO-ANT        PIC 9(8)     VALUE ZEROS.
           05  QT-BRINDE-TOT         PIC 9(8)     VALUE ZEROS.
           05  QT-BRINDE-TOTG        PIC 9(8)     VALUE ZEROS.
           05  QT-FORM-TOT           PIC 9(8)     VALUE ZEROS.
           05  QT-FORM-TOTG          PIC 9(8)     VALUE ZEROS.
           05  VLR-TOTAL-TOT         PIC 9(8)V99  VALUE ZEROS.
           05  VLR-TOTAL-TOTG        PIC 9(8)V99  VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  AUX-DATA              PIC 9(8)     VALUE ZEROS.
           05  AUX-DATA2             PIC 9(8)     VALUE ZEROS.
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
           "RELATORIO COMPROMISSO CONTRATUAL - ORDEM: ".
           05  ORDEM-REL           PIC X(18)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(131)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(131)  VALUE
           "COD COMP-CONTRATUAL      DATA-VECTO  QTDE  VLR-UNITARIO
      -    "VLR-TOTAL CONT IDENTIFICACAO CIDADE FORM P MES/ANO DATA-PGTO
      -    " DATA-ENVIO".
       01  LINDET.
           05  LINDET-REL          PIC X(134)  VALUE SPACES.

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
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD050.
           OPEN INPUT CAD010 COD040 COD050 COD002 CAD012.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
               WHEN GS-LE-REGIAO-TRUE
                    PERFORM LER-REGIAO
               WHEN GS-POPUP-BRINDE-TRUE
                    PERFORM POPUP-BRINDE
               WHEN GS-POPUP-REGIAO-TRUE
                    PERFORM POPUP-REGIAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-REGIAO SECTION.
           MOVE GS-ACP-REGIAO    TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG.
           MOVE NOME-REG    TO GS-DESC-REGIAO.

       POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(1: 30)  TO GS-DESC-REGIAO
           MOVE PASSAR-STRING-1(33: 2)  TO GS-ACP-REGIAO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LER-BRINDE SECTION.
           MOVE GS-CODIGO    TO CODIGO-CO02.
           READ COD002 INVALID KEY
                MOVE SPACES  TO NOME-CO02.

           MOVE NOME-CO02    TO GS-DESCRICAO.
       POPUP-BRINDE SECTION.
           CALL   "COP002T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "COP002T".
           MOVE PASSAR-STRING-1(1: 20)  TO GS-DESCRICAO.
           MOVE PASSAR-STRING-1(33: 3)  TO GS-CODIGO.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME.

           OPEN OUTPUT WORK.
           CLOSE       WORK.
           OPEN I-O    WORK.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           INITIALIZE REG-COD050

           EVALUATE GS-INTERVALO
               WHEN 1 MOVE GS-VENCTO-INI  TO DATA-INV
                      CALL "GRIDAT2" USING DATA-INV
                      MOVE DATA-INV       TO VENCTO-INI
                      MOVE GS-VENCTO-FIM  TO DATA-INV
                      CALL "GRIDAT2" USING DATA-INV
                      MOVE DATA-INV       TO VENCTO-FIM
                      MOVE VENCTO-INI     TO DATA-VENCTO-CO50
                      START COD050 KEY IS NOT < DATA-VENCTO-CO50
                                                             INVALID KEY
                            MOVE "10" TO ST-COD050
                      END-START
                      PERFORM UNTIL ST-COD050 = "10"
                            READ COD050 NEXT RECORD AT END
                                 MOVE "10" TO ST-COD050
                            NOT AT END
                                 IF GS-OPCAO = 3 OR SUSP-PREV-DEF-CO50
                                    IF GS-ORDEM-REL = 1 OR
                                       CODBRINDE-CO50 = GS-CODIGO
                                       MOVE DATA-VENCTO-CO50
                                         TO GS-EXIBE-VENCTO
                                       MOVE "TELA-AGUARDA1"
                                         TO DS-PROCEDURE
                                       PERFORM CALL-DIALOG-SYSTEM
                                       IF DATA-VENCTO-CO50 > VENCTO-FIM
                                          MOVE "10" TO ST-COD050
                                       ELSE
                                          IF GS-PAGO = 1 AND
                                             REALIZADO-CO50 = 1
                                             PERFORM MOVER-DADOS-WORK
                                          ELSE
                                             IF GS-PAGAR = 1 AND
                                                REALIZADO-CO50 <> 1
                                                PERFORM MOVER-DADOS-WORK
                                             END-IF
                                          END-IF
                                       END-IF
                                    END-IF
                                 END-IF
                            END-READ
                      END-PERFORM
               WHEN 2 MOVE GS-VENCTO-INI  TO DATA-INV
                      CALL "GRIDAT2" USING DATA-INV
                      MOVE DATA-INV       TO VENCTO-INI
                      MOVE GS-VENCTO-FIM  TO DATA-INV
                      CALL "GRIDAT2" USING DATA-INV
                      MOVE DATA-INV       TO VENCTO-FIM
                      MOVE VENCTO-INI     TO DATA-PAGTO-CO50
                      START COD050 KEY IS NOT < DATA-PAGTO-CO50
                                                             INVALID KEY
                            MOVE "10" TO ST-COD050
                      END-START
                      PERFORM UNTIL ST-COD050 = "10"
                            READ COD050 NEXT RECORD AT END
                                 MOVE "10" TO ST-COD050
                            NOT AT END
                                 IF GS-OPCAO = 3 OR SUSP-PREV-DEF-CO50
                                    IF GS-ORDEM-REL = 1 OR
                                       CODBRINDE-CO50 = GS-CODIGO
                                       MOVE DATA-PAGTO-CO50
                                         TO GS-EXIBE-VENCTO
                                       MOVE "TELA-AGUARDA1"
                                         TO DS-PROCEDURE
                                       PERFORM CALL-DIALOG-SYSTEM
                                       IF DATA-PAGTO-CO50 > VENCTO-FIM
                                          MOVE "10" TO ST-COD050
                                       ELSE
                                          IF GS-PAGO = 1 AND
                                             REALIZADO-CO50 = 1
                                             PERFORM MOVER-DADOS-WORK
                                          ELSE
                                             IF GS-PAGAR = 1 AND
                                                REALIZADO-CO50 <> 1
                                                PERFORM MOVER-DADOS-WORK
                                             END-IF
                                          END-IF
                                       END-IF
                                    END-IF
                                 END-IF
                            END-READ
                      END-PERFORM
               WHEN 3 MOVE GS-VENCTO-INI  TO DATA-INV
                      CALL "GRIDAT2" USING DATA-INV
                      MOVE DATA-INV       TO VENCTO-INI
                      MOVE GS-VENCTO-FIM  TO DATA-INV
                      CALL "GRIDAT2" USING DATA-INV
                      MOVE DATA-INV       TO VENCTO-FIM
                      MOVE VENCTO-INI     TO DATA-ENVIO-CO50
                      START COD050 KEY IS NOT < CHAVE-ENVIO-CO50
                                                             INVALID KEY
                            MOVE "10" TO ST-COD050
                      END-START
                      PERFORM UNTIL ST-COD050 = "10"
                            READ COD050 NEXT RECORD AT END
                                 MOVE "10" TO ST-COD050
                            NOT AT END
                                 IF GS-OPCAO = 3 OR SUSP-PREV-DEF-CO50
                                    IF GS-ORDEM-REL = 1 OR
                                       CODBRINDE-CO50 = GS-CODIGO
                                       MOVE DATA-PAGTO-CO50
                                         TO GS-EXIBE-VENCTO
                                       MOVE "TELA-AGUARDA1"
                                         TO DS-PROCEDURE
                                       PERFORM CALL-DIALOG-SYSTEM
                                       IF DATA-ENVIO-CO50 > VENCTO-FIM
                                          MOVE "10" TO ST-COD050
                                       ELSE
                                          IF GS-PAGO = 1 AND
                                             REALIZADO-CO50 = 1
                                             PERFORM MOVER-DADOS-WORK
                                          ELSE
                                             IF GS-PAGAR = 1 AND
                                                REALIZADO-CO50 <> 1
                                                PERFORM MOVER-DADOS-WORK
                                             END-IF
                                          END-IF
                                       END-IF
                                    END-IF
                                 END-IF
                            END-READ
                      END-PERFORM
           END-EVALUATE
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-WORK SECTION.
           MOVE NR-CONTRATO-CO50 TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE 0            TO STATUS-CO40.

           IF STATUS-CO40 < 50
              CONTINUE
           ELSE
              MOVE CIDADE-CO40        TO CIDADE
              READ CAD010 NOT INVALID KEY
                   IF GS-ACP-UF = SPACES OR GS-ACP-UF = UF-CID
                      IF GS-ACP-REGIAO = 0 OR GS-ACP-REGIAO = REGIAO-CID
                         MOVE ITEM-CO50          TO ITEM-WK
                         MOVE NR-CONTRATO-CO50   TO CONTRATO-WK
                         MOVE DATA-VENCTO-CO50   TO DATA-VENCTO-WK
                         MOVE CODBRINDE-CO50     TO BRINDE-WK
                                                    CODIGO-CO02
                         READ COD002 INVALID KEY
                              MOVE SPACES TO NOME-CO02
                         END-READ
                         MOVE NOME-CO02             TO DESC-BRINDE-WK
                         IF MULT-FORM-CO02 = 2
                            MOVE QTDE-POR-FORM-CO50 TO QTDE-BRINDE-WK
                         ELSE
                            COMPUTE QTDE-BRINDE-WK = QTDE-POR-FORM-CO50
                                                   * QTDE-FORM-CO50
                         END-IF
                         MOVE MESANO-PREV-CO40   TO MESANO-I
                         MOVE MESANO-I(1: 4)     TO MESANO-W(3: 4)
                         MOVE MESANO-I(5: 2)     TO MESANO-W(1: 2)
                         MOVE MESANO-W           TO MESANO-WK
                         MOVE CIDADE-CO40        TO CIDADE
                         READ CAD010 INVALID KEY
                              MOVE SPACES TO NOME-CID
                         END-READ
                         MOVE NOME-CID            TO CIDADE-WK
                         IF CUSTO-UNIT-CO50 <> ZEROS
                            MOVE CUSTO-UNIT-CO50  TO VLR-UNIT-WK
                         ELSE
                            MOVE VALOR-CO02       TO VLR-UNIT-WK
                         END-IF
                         COMPUTE VLR-TOTAL-WK = VLR-UNIT-WK *
                                                    QTDE-BRINDE-WK
                         MOVE QTDE-FORM-CO40     TO QT-FORM-WK
                         MOVE IDENTIFICACAO-CO40 TO IDENTIFICACAO-WK
                         MOVE PADRAO-CO40        TO PADRAO-WK
                         MOVE DATA-PAGTO-CO50    TO DATA-PAGTO-WK
                         MOVE DATA-ENVIO-CO50    TO DATA-ENVIO-WK
                         WRITE REG-WORK.
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
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA

      *    TOTALIZA GERAL
           MOVE SPACES         TO GS-LINDET
           MOVE "Total Geral..." TO GS-LINDET(1: 30)
           MOVE QT-BRINDE-TOTG TO QTDE-E
           MOVE QTDE-E         TO GS-LINDET(32: 10)
           MOVE VLR-TOTAL-TOTG TO VALOR-E
           MOVE VALOR-E        TO GS-LINDET(57: 14)
           MOVE QT-FORM-TOTG   TO QTDE-E
           MOVE QTDE-E         TO GS-LINDET(91: 10)
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
             WHEN 3
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "DATA-PAGTO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DATA-PAGTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "DATA-ENVIO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DATA-ENVIO-WK INVALID KEY
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
             WHEN 3
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF DATA-PAGTO-ANT NOT = ZEROS
                 IF DATA-PAGTO-ANT NOT = DATA-PAGTO-WK
                    PERFORM TOTALIZA
             WHEN 5
              IF DATA-ENVIO-ANT NOT = ZEROS
                 IF DATA-ENVIO-ANT NOT = DATA-ENVIO-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
      *    INSERIR LINHA NA LIST BOX
           MOVE BRINDE-WK         TO GS-LINDET(1: 4)
           MOVE DESC-BRINDE-WK    TO GS-LINDET(5: 21)
           MOVE DATA-VENCTO-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(26: 11)
           MOVE QTDE-BRINDE-WK    TO QTDE-E1
           MOVE QTDE-E1           TO GS-LINDET(37: 6)
           ADD QTDE-BRINDE-WK     TO QT-BRINDE-TOT
           MOVE VLR-UNIT-WK       TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(43: 14)
           MOVE VLR-TOTAL-WK      TO VALOR-E
           ADD VLR-TOTAL-WK       TO VLR-TOTAL-TOT
           MOVE VALOR-E           TO GS-LINDET(57: 14)
           MOVE CONTRATO-WK       TO GS-LINDET(71: 5)
           MOVE IDENTIFICACAO-WK  TO GS-LINDET(76: 14)
           MOVE CIDADE-WK         TO GS-LINDET(90: 7)
           MOVE QT-FORM-WK        TO GS-LINDET(97: 5)
           ADD QT-FORM-WK         TO QT-FORM-TOT
           MOVE PADRAO-WK         TO GS-LINDET(102: 2)
           MOVE MESANO-WK         TO MESANO-E
           MOVE MESANO-E          TO GS-LINDET(104: 7)
           MOVE DATA-PAGTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(112: 11)
           MOVE DATA-ENVIO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(123: 11)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-CHAVE-ANT SECTION.
           MOVE DESC-BRINDE-WK    TO DESC-BRINDE-ANT.
           MOVE DATA-VENCTO-WK    TO DATA-VENCTO-ANT.
           MOVE CONTRATO-WK       TO CONTRATO-ANT
           MOVE DATA-PAGTO-WK     TO DATA-PAGTO-ANT
           MOVE DATA-ENVIO-WK     TO DATA-ENVIO-ANT.
       TOTALIZA SECTION.
           MOVE SPACES            TO GS-LINDET
           MOVE "Total..."        TO GS-LINDET(1: 30)
           MOVE QT-BRINDE-TOT     TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(32: 10)
           MOVE VLR-TOTAL-TOT     TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(57: 14)
           MOVE QT-FORM-TOT       TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(91: 10)
           ADD QT-BRINDE-TOT      TO QT-BRINDE-TOTG
           ADD QT-FORM-TOT        TO QT-FORM-TOTG
           ADD VLR-TOTAL-TOT      TO VLR-TOTAL-TOTG
           MOVE ZEROS             TO QT-FORM-TOT
                                     QT-BRINDE-TOT
                                     VLR-TOTAL-TOT
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO QT-BRINDE-TOT QT-BRINDE-TOTG
                         QT-FORM-TOT QT-FORM-TOTG
                         VLR-TOTAL-TOT VLR-TOTAL-TOTG.
           MOVE SPACES TO DESC-BRINDE-ANT.
           MOVE ZEROS TO DATA-VENCTO-ANT CONTRATO-ANT DATA-PAGTO-ANT
                         DATA-ENVIO-ANT.

      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP150" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           INITIALIZE REG-WORK.
           MOVE SPACES TO DESC-BRINDE-ANT.
           MOVE ZEROS  TO DATA-VENCTO-ANT
                          CONTRATO-ANT
                          DATA-PAGTO-ANT.
           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

      *    TOTALIZA GERAL
           MOVE SPACES           TO LINDET-REL
           MOVE "Total Geral..." TO LINDET-REL(1: 30)
           MOVE QT-BRINDE-TOTG   TO QTDE-E
           MOVE QTDE-E           TO LINDET-REL(32: 10)
           MOVE VLR-TOTAL-TOTG   TO VALOR-E
           MOVE VALOR-E          TO LINDET-REL(57: 14)
           MOVE QT-FORM-TOTG     TO QTDE-E
           MOVE QTDE-E           TO LINDET-REL(91: 10)
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
             WHEN 3
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF DATA-PAGTO-ANT  NOT = ZEROS
                 IF DATA-PAGTO-ANT NOT = DATA-PAGTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF DATA-ENVIO-ANT  NOT = ZEROS
                 IF DATA-ENVIO-ANT NOT = DATA-ENVIO-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
      *    IMPRIMIR LINHA DETALHE
           MOVE BRINDE-WK         TO LINDET-REL(1: 4)
           MOVE DESC-BRINDE-WK    TO LINDET-REL(5: 21)
           MOVE DATA-VENCTO-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(26: 11)
           MOVE QTDE-BRINDE-WK    TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(37: 6)
           ADD QTDE-BRINDE-WK     TO QT-BRINDE-TOT
           MOVE VLR-UNIT-WK       TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(43: 14)
           MOVE VLR-TOTAL-WK      TO VALOR-E
           ADD VLR-TOTAL-WK       TO VLR-TOTAL-TOT
           MOVE VALOR-E           TO LINDET-REL(57: 14)
           MOVE CONTRATO-WK       TO LINDET-REL(71: 5)
           MOVE IDENTIFICACAO-WK  TO LINDET-REL(76: 14)
           MOVE CIDADE-WK         TO LINDET-REL(90: 7)
           MOVE QT-FORM-WK        TO LINDET-REL(97: 5)
           ADD QT-FORM-WK         TO QT-FORM-TOT
           MOVE PADRAO-WK         TO LINDET-REL(102: 2)
           MOVE MESANO-WK         TO MESANO-E
           MOVE MESANO-E          TO LINDET-REL(104: 7)
           MOVE DATA-PAGTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(112: 11)
      *    MOVE DATA-ENVIO-WK     TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO LINDET-REL(123: 11)
      *    MOVE DATA-E(9:2)       TO LINDET-REL(131:2)
           IF DATA-ENVIO-WK > 0
              STRING DATA-ENVIO-WK(7:2) "/" DATA-ENVIO-WK(5:2) "/"
                     DATA-ENVIO-WK(3:2) INTO LINDET-REL(123:11)
           ELSE
              MOVE SPACES TO LINDET-REL(123:11)
           END-IF
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES        TO LINDET-REL
           MOVE "Total..."    TO LINDET-REL(1: 30)
           MOVE QT-BRINDE-TOT TO QTDE-E
           MOVE QTDE-E        TO LINDET-REL(32: 10)
           MOVE VLR-TOTAL-TOT TO VALOR-E
           MOVE VALOR-E       TO LINDET-REL(57: 14)
           MOVE QT-FORM-TOT   TO QTDE-E
           MOVE QTDE-E        TO LINDET-REL(91: 10)
           ADD QT-BRINDE-TOT  TO QT-BRINDE-TOTG
           ADD QT-FORM-TOT    TO QT-FORM-TOTG
           ADD VLR-TOTAL-TOT  TO VLR-TOTAL-TOTG
           MOVE ZEROS TO QT-FORM-TOT QT-BRINDE-TOT VLR-TOTAL-TOT
           WRITE REG-RELAT FROM LINDET

           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT
           ADD 2 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.

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
           CLOSE COD040 COD050 CAD010 COD002 CAD012.
           CLOSE WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
