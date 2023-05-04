       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP204.
      *DATA: 27/09/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Relatório de comissão por contrato- vendas por vendedor
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY MTPX020.
           COPY RCPX100.
           COPY RCPX101.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = CONTRATO-WK VENDEDOR-WK
                                           VENCTO-WK.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY MTPW020.
       COPY RCPW100.
       COPY RCPW101.
       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  VENDEDOR-WK         PIC 9(6).
           05  NOME-VENDEDOR-WK    PIC X(30).
           05  VENCTO-WK           PIC 9(6).
           05  VALOR-VENDA-WK      PIC 9(10)v99.
           05  VALOR-COMISSAO-WK   PIC 9(10)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP204.CPB".
           COPY "RCP204.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ALBUM-W               PIC 9(8)     VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  MOVTO-INI             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  VENDEDOR-ANT          PIC 9(6)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZZ.ZZZ.ZZZ,ZZ
           BLANK WHEN ZEROS.
           05  VALOR-COMISSAO        PIC 9(8)V99  VALUE ZEROS.
           05  MESANOW.
               10  MES-WW            PIC 9(2).
               10  ANO-WW            PIC 9(4).
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9(4).
               10  MES-II            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  TOTG-VALOR-VENDA      PIC 9(10)V99 VALUE ZEROS.
           05  TOTG-VALOR-COMISSAO   PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VALOR-VENDA       PIC 9(10)V99 VALUE ZEROS.
           05  TOT-VALOR-COMISSAO    PIC 9(8)V99  VALUE ZEROS.
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.
           05  AUX-DATA2             PIC 9(08).
           05  AUX-DATA              PIC 9(08).
           05  AUX-DATA-R REDEFINES AUX-DATA.
               10 AUX-MESANO         PIC 9(06).
               10 AUX-DIA            PIC 9(02).
           05  PASSAR-STRING-1       PIC X(65).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(35)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(55)   VALUE
           "RELATORIO DE COMISSAO POR VENDEDOR/CONTRATO - ORDEM: ".
           05  ORDEM-REL           PIC X(20)   VALUE SPACES.
       01  CAB02A.
           05  FILLER              PIC X(12)   VALUE "INT.VENCTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
           05  FILLER              PIC X(18).
           05  DET-DESCRICAO       PIC X(11).
           05  MOVTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MOVTO-FIM-REL       PIC 99/99/9999.
           05  FILLER              PIC X(03).
           05  FILLER              PIC X(10) VALUE "CONTRATO: ".
           05  CONTRATO-REL        PIC 9999.

       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "CONT  CODIGO NOME-VENDEDOR                  VENCTO   VALOR-V
      -    "ENDA  VLR-COMISSAO".
       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CGD001 MTD020 RCD100 RCD101.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
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
      *             EVALUATE GS-OPCAO
      *                WHEN 1 MOVE "INT.MOVTO: " TO DESCRICAO-REL
      *                WHEN 2 MOVE "INT.VENDA: " TO DESCRICAO-REL
      *             END-EVALUATE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-DATA-MOVTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-VENDEDOR-TRUE
                   PERFORM LE-VENDEDOR
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POPUP SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-VENDEDOR.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR  TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "****" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-VENDEDOR.
       VERIFICA-DATA-MOVTO-ANT SECTION.
      *    IF GS-VECTO-INI NOT = VECTO-INI-ANT
      *       OR GS-VECTO-FIM NOT = VECTO-FIM-ANT
      *          PERFORM GRAVA-WORK.
       GRAVA-WORK SECTION.
           CLOSE WORK
           OPEN OUTPUT
           WORK CLOSE WORK
           OPEN I-O WORK.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-INI.

           MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-FIM.

           MOVE GS-MOVTO-INI TO DATA-INV MOVTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO MOVTO-INI

           MOVE GS-MOVTO-FIM TO DATA-INV MOVTO-FIM-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO MOVTO-FIM

           MOVE GS-CONTRATO  TO CONTRATO-REL

           MOVE GS-DESCRICAO TO DET-DESCRICAO

           INITIALIZE REG-RCD100

           IF GS-CONTRATO > 0
              PERFORM POR-CONTRATO
           ELSE
              EVALUATE GS-OPCAO
                 WHEN 1 PERFORM POR-DTMOVTO
                 WHEN 2 PERFORM POR-DTVECTO
              END-EVALUATE
           END-IF

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       POR-CONTRATO SECTION.
           STRING GS-CONTRATO "0000"       INTO ALBUM-REC
           START RCD100 KEY IS NOT LESS ALBUM-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   IF GS-CONTRATO <> ALBUM-REC(1:4)
                      MOVE "10" TO ST-RCD100
                   ELSE
                      IF GS-OPCAO = 1
                         IF DATA-MOVTO-REC NOT < MOVTO-INI AND
                            DATA-MOVTO-REC NOT > MOVTO-FIM OR
                            MOVTO-INI = 0
                            MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
                            MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                            IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                               VENDEDOR-REC
                               PERFORM VERIFICA-RCD101
                            END-IF
                         END-IF
                      ELSE
                         IF DATAVEN-REC NOT < MOVTO-INI AND
                            DATAVEN-REC NOT > MOVTO-FIM OR
                            MOVTO-INI = 0
                            MOVE DATAVEN-REC     TO GS-EXIBE-MOVTO
                            MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                            IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                               VENDEDOR-REC
                               PERFORM VERIFICA-RCD101
                            END-IF
                         END-IF
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       POR-DTMOVTO SECTION.
           INITIALIZE REG-RCD100
           MOVE MOVTO-INI      TO DATA-MOVTO-REC
           START RCD100 KEY IS NOT LESS ALT-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   IF DATA-MOVTO-REC > MOVTO-FIM
                      MOVE "10" TO ST-RCD100
                   ELSE
                      MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
                      MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                         VENDEDOR-REC
                         PERFORM VERIFICA-RCD101
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       POR-DTVECTO SECTION.
           INITIALIZE REG-RCD100
           MOVE MOVTO-INI      TO DATAVEN-REC
           START RCD100 KEY IS NOT LESS DATAVEN-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   IF DATAVEN-REC > MOVTO-FIM
                      MOVE "10" TO ST-RCD100
                   ELSE
                      MOVE DATAVEN-REC  TO GS-EXIBE-MOVTO
                      MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                         VENDEDOR-REC
                         PERFORM VERIFICA-RCD101
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       VERIFICA-RCD101 SECTION.
           INITIALIZE REG-RCD101

           MOVE ALBUM-REC    TO ALBUM-REC1
           MOVE VECTO-INI    TO VENCTO-REC1

           START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101.

           PERFORM UNTIL ST-RCD101 = "10"

             READ RCD101 NEXT RECORD AT END
                  MOVE "10" TO ST-RCD101
             NOT AT END
                  IF ALBUM-REC <> ALBUM-REC1 OR VENCTO-REC1 > VECTO-FIM
                     MOVE "10" TO ST-RCD101
                  ELSE
                     IF GS-BAIXADAS-TRUE AND GS-NAO-BAIXADAS-TRUE
                        MOVE "BAIXADAS E NAO BAIX."
                                                  TO GS-DESCR-ORDEM
                        PERFORM GRAVA-COMISSAO
                     ELSE
                        IF GS-BAIXADAS-TRUE AND
                                             DTA-BAIXA-REC1 > ZEROS
                           MOVE "BAIXADAS            "
                                                  TO GS-DESCR-ORDEM
                           PERFORM GRAVA-COMISSAO
                        ELSE
                           IF GS-NAO-BAIXADAS-TRUE AND
                                                 DTA-BAIXA-REC1 = 0
                              MOVE "NAO BAIXADAS        "
                                                  TO GS-DESCR-ORDEM
                              PERFORM GRAVA-COMISSAO
                           END-IF
                        END-IF
                     END-IF
                  END-IF
             END-READ
           END-PERFORM.

       GRAVA-COMISSAO SECTION.
           IF DATA-MOVTO-REC < 20050511 AND TIPO-REC1 = 3 *> VENDA ANTECIPADA
      *    IF TIPO-REC1 = 3
              CONTINUE
           ELSE
             MOVE ALBUM-REC1 TO ALBUM-MTG
             READ MTD020 INVALID KEY
                  MOVE ZEROS TO VISITA-MTG
                                FOGO-MTG
             END-READ
             IF GS-SO-FOGO = 1
                IF FOGO-MTG = 8
                   PERFORM MOVER-DADOS-WORK
                END-IF
             ELSE
                IF GS-INCLUIR-FOGO = 1
      *            IF FOGO-MTG = 8
                      PERFORM MOVER-DADOS-WORK
      *            END-IF
                ELSE
                   IF FOGO-MTG <> 8
                      PERFORM MOVER-DADOS-WORK
                   END-IF
                END-IF
             END-IF.
       MOVER-DADOS-WORK SECTION.
           COMPUTE VALOR-COMISSAO = VALOR-REC1 * COMIS-PARC-REC1
           MOVE VENCTO-REC1         TO DATA-INV
           MOVE DATA-INV(1: 6)      TO VENCTO-WK
           MOVE ALBUM-REC1           TO ALBUM-W
           MOVE ALBUM-W(1: 4)        TO CONTRATO-WK
           MOVE VENDEDOR-REC         TO VENDEDOR-WK
           READ WORK INVALID KEY
               MOVE VENDEDOR-REC    TO CODIGO-CG01
               READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
               END-READ
               MOVE NOME-CG01       TO NOME-VENDEDOR-WK
               MOVE VALOR-COMISSAO  TO VALOR-COMISSAO-WK
               MOVE VALOR-REC1      TO VALOR-VENDA-WK
               WRITE REG-WORK
               END-WRITE
             NOT INVALID KEY
               ADD VALOR-COMISSAO   TO VALOR-COMISSAO-WK
               ADD VALOR-REC1       TO VALOR-VENDA-WK
               REWRITE REG-WORK
               END-REWRITE.

       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-VALOR-VENDA TOT-VALOR-COMISSAO
                         TOTG-VALOR-VENDA TOTG-VALOR-COMISSAO
                         VENDEDOR-ANT CONTRATO-ANT.

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-WORK.
           MOVE ZEROS TO REG-WORK
           START WORK KEY IS GREATER THAN CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF VENDEDOR-ANT <> ZEROS AND CONTRATO-ANT <> ZEROS
                  IF CONTRATO-ANT <> CONTRATO-WK OR
                     VENDEDOR-ANT <> VENDEDOR-WK
                       PERFORM SUBTOTAL-TELA
                  END-IF
                END-IF
                MOVE VENDEDOR-WK      TO VENDEDOR-ANT
                MOVE CONTRATO-WK      TO CONTRATO-ANT
                ADD VALOR-VENDA-WK    TO TOT-VALOR-VENDA
                ADD VALOR-COMISSAO-WK TO TOT-VALOR-COMISSAO
                PERFORM MOVER-DADOS-LINDET
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM SUBTOTAL-TELA.
           PERFORM TOTALIZA-TELA.

       MOVER-DADOS-LINDET SECTION.
           MOVE SPACES            TO GS-LINDET
           MOVE CONTRATO-WK       TO GS-LINDET(1: 5)
           MOVE VENDEDOR-WK       TO GS-LINDET(6: 7)
           MOVE NOME-VENDEDOR-WK  TO GS-LINDET(13: 31)
           MOVE VENCTO-WK         TO MESANO-I
           MOVE MES-II TO MES-WW
           MOVE ANO-II TO ANO-WW
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO GS-LINDET(44: 8)
           MOVE VALOR-VENDA-WK    TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(54: 14)
           MOVE VALOR-COMISSAO-WK TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(68: 14).
       SUBTOTAL-TELA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE "Subtotal"         TO GS-LINDET(1: 30)
           MOVE TOT-VALOR-VENDA    TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(54: 14)
           MOVE TOT-VALOR-COMISSAO TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(68: 14).
           ADD TOT-VALOR-VENDA     TO TOTG-VALOR-VENDA
           ADD TOT-VALOR-COMISSAO  TO TOTG-VALOR-COMISSAO.
           MOVE ZEROS TO TOT-VALOR-VENDA TOT-VALOR-COMISSAO.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA-TELA SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "Total Geral... "   TO GS-LINDET
           MOVE TOTG-VALOR-VENDA    TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET(54: 14)
           MOVE TOTG-VALOR-COMISSAO TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET(68: 14).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP204" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO VENDEDOR-ANT CONTRATO-ANT
                         TOT-VALOR-VENDA TOT-VALOR-COMISSAO.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           INITIALIZE REG-WORK.
           MOVE ZEROS TO REG-WORK
           START WORK KEY IS GREATER THAN CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF VENDEDOR-ANT <> ZEROS AND CONTRATO-ANT <> ZEROS
                  IF CONTRATO-ANT <> CONTRATO-WK OR
                     VENDEDOR-ANT <> VENDEDOR-WK
                       PERFORM SUBTOTAL-REL
                  END-IF
                END-IF
                MOVE VENDEDOR-WK      TO VENDEDOR-ANT
                MOVE CONTRATO-WK      TO CONTRATO-ANT
                ADD VALOR-VENDA-WK    TO TOT-VALOR-VENDA
                ADD VALOR-COMISSAO-WK TO TOT-VALOR-COMISSAO
                PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM SUBTOTAL-REL
           PERFORM TOTALIZA-REL

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           PERFORM MOVER-DADOS-LINDET.
           MOVE GS-LINDET    TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       SUBTOTAL-REL SECTION.
           MOVE SPACES             TO LINDET-REL
           MOVE "Subtotal"         TO LINDET-REL(1: 30)
           MOVE TOT-VALOR-VENDA    TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(54: 14)
           MOVE TOT-VALOR-COMISSAO TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(68: 14).
           MOVE ZEROS TO TOT-VALOR-VENDA TOT-VALOR-COMISSAO.
           WRITE REG-RELAT FROM LINDET.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
       TOTALIZA-REL SECTION.
           MOVE SPACES              TO LINDET-REL.
           MOVE "Total Geral... "   TO LINDET-REL
           MOVE TOTG-VALOR-VENDA    TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(54: 14)
           MOVE TOTG-VALOR-COMISSAO TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(68: 14).
           WRITE REG-RELAT FROM LINDET AFTER 2.

       CABECALHO SECTION.
           MOVE GS-DESCRICAO     TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB02A AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 7 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 MTD020 RCD100 RCD101 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

