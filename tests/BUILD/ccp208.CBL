       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP208.
      *DATA: 21/10/1999
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: Relatório de Saldo das Contas corrente
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX030.
           COPY CGPX001.
           COPY CGPX006.
           COPY CCPX010.
           COPY CCPX100.
           COPY CCPX101.
           COPY CCPX105.
           COPY CCPX115.
           COPY CCPX110.
           COPY CCPX120.

           COPY CXPX020.
           COPY CPPX020.
           COPY CPPX021.
           COPY CPPX022.
           COPY CPPX024.
           COPY LOGX001.

           COPY CRPX020.
           COPY CRPX021.


           SELECT WORK ASSIGN TO arquivo-work
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK              =
                                CLASSIF-WK
                                CODIGO-WK
                                SEQ-WK
                  ALTERNATE RECORD KEY IS CHAVE-NOME  =
                                CLASSIF-WK
                                NOME-WK
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CHAVE-VALOR =
                                CLASSIF-WK
                                SALDO-CTACORR-WK
                                NOME-WK
                  WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW030.
       COPY CGPW001.
       COPY CGPW006.
       COPY CCPW010.
       COPY CCPW100.
       COPY CCPW101.
       COPY CCPW105.
       COPY CCPW115.
       COPY CCPW110.
       COPY CCPW120.

       COPY CXPW020.
       COPY CPPW020.
       COPY CPPW021.
       COPY CPPW022.
       COPY CPPW024.
       COPY LOGW001.

       COPY CRPW020.
       COPY CRPW021.

       FD  WORK.
       01  REG-WORK.
           05  CLASSIF-WK          PIC 9(2).
           05  CODIGO-WK           PIC 9(6).
           05  SEQ-WK              PIC 9(5).
           05  NOME-WK             PIC X(30).
           05  SALARIO-FUNC-WK     PIC 9(7)V99.
           05  SALARIO-REPORT-WK   PIC 9(7)V99.
           05  SALARIO-REPRES-WK   PIC 9(7)V99.
           05  SALARIO-VEND-WK     PIC 9(7)V99.
           05  TOT-CREDITO-WK      PIC 9(7)V99.
           05  SALDO-CTACORR-WK    PIC S9(7)V99.
           05  FORMA-PAGTO-WK      PIC X(50).
           05  BANCO-WK            PIC 9(4).
           05  NOME-BANCO-WK       PIC X(20).
           05  AGENCIA-WK          PIC X(9).
           05  NR-CONTA-WK         PIC X(15).
           05  COMPLEMENTO-WK      PIC X(15).
           05  SALDO-ABERTO-WK     PIC 9(07)V99.
           05  COD-RED-WK          PIC 9(05).
           05  SELECAO-WK          PIC X(01).
           05  TITULAR-WK          PIC X(50).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP208.CPB".
           COPY "CCP208.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD030             PIC XX       VALUE SPACES.
           05  ST-CCD010             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD101             PIC XX       VALUE SPACES.
           05  ST-CCD105             PIC XX       VALUE SPACES.
           05  ST-CCD115             PIC XX       VALUE SPACES.
           05  ST-CCD110             PIC XX       VALUE SPACES.
           05  ST-CCD120             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD006             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD021             PIC XX       VALUE SPACES.
           05  ST-CPD022             PIC XX       VALUE SPACES.
           05  ST-CPD024             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD021             PIC XX       VALUE SPACES.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
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
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9(4).
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9(4).
               10  MES-II            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  DATAI.
               10  ANO-I             PIC 9(4).
               10  MES-I             PIC 99.
               10  DIA-I             PIC 99.
           05  DATA-I REDEFINES DATAI PIC 9(8).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           05  CODIGO-W              PIC 9(6)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  VALOR-E               PIC ZZZZ.ZZ9,99.
           05  NUMERO                PIC 9(09)    VALUE ZEROS.
           05  CLASSIFICACAO         PIC 9(02)    VALUE ZEROS.
           05  VALOR-E1              PIC ZZZZ.ZZ9,99-.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  SALDO-APAGAR          PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-CTACORR-W       PIC S9(8)V99 VALUE ZEROS.
           05  SALARIO-FUNC-TOT      PIC 9(8)V99  VALUE ZEROS.
           05  SALARIO-REPORT-TOT    PIC 9(8)V99  VALUE ZEROS.
           05  SALARIO-VEND-TOT      PIC 9(8)V99  VALUE ZEROS.
           05  SALARIO-REPRES-TOT    PIC 9(8)V99  VALUE ZEROS.
           05  TOT-CREDITO-TOT       PIC 9(8)V99  VALUE ZEROS.
           05  SALDO-CTACORR-TOT     PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-ABERTO-TOT      PIC S9(8)V99 VALUE ZEROS.
           05  CTACORR-LIQUIDO       PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-APAGAR-TOT      PIC S9(8)V99 VALUE ZEROS.
           05  VALOR-APAGAR-TOT      PIC S9(8)V99 VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  SALDO-CONTA           PIC S9(09)V99 VALUE ZEROS.
           05  SALDO-ABERTO          PIC S9(09)V99 VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  AUX-CLASSIF           PIC 9(02)    VALUE ZEROS.
           05  MOSTRAR-ERRO          PIC X(01).
           05  VERIFICAR-VALOR       PIC S9(09)V99 VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  CAB01.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(42)   VALUE
           "RELATORIO DE SALDO GERAL - CONTA CORRENTE".
           05  FILLER              PIC X(15)   VALUE "       VENCTO: ".
           05  VENCTO-REL          PIC 99/99/9999.
       01  CAB021.
           05  FILLER              PIC X(16) VALUE "CARACTERISTICA: ".
           05  CARACTERISTICA-REL  PIC X(100).

       01  CAB03.
           05  FILLER              PIC X(132)  VALUE ALL "=".

       01  CAB04.
           05  FILLER              PIC X(132)  VALUE
           "CODIGO NOME                         COD-RED SALDO FINAL  FOR
      -    "MA PAGTO                                     TITULAR".

       01  LINDET.
           05  LINDET-REL          PIC X(131)  VALUE SPACES.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU            PIC 9(04).
             10 WS-MES-CPU            PIC 9(02).
             10 WS-DIA-CPU            PIC 9(02).
          05 FILLER                   PIC X(13).

       01 WS-HORA-SYS.
          05 HH-SYS                   PIC 9(02).
          05 MM-SYS                   PIC 9(02).
          05 SE-SYS                   PIC 9(02).
          05 MS-SYS                   PIC 9(02).

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
           MOVE DATA-INV TO DATA-DIA-INV.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD030"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD030.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD006"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD006.
           MOVE "CCD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD010.
           MOVE "CCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "CCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD101.
           MOVE "CCD105"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD105.
           MOVE "CCD115"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD115.
           MOVE "CCD110"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD110.
           MOVE "CCD120"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD120.
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CPD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD021.
           MOVE "CPD022"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD022.
           MOVE "CPD024"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD024.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD021.
           MOVE "LOG001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOG001.

           OPEN INPUT CGD001 CCD010 CCD100 CCD105 CCD110 CCD115 CCD120
                      CGD006 CPD020 CPD021 CPD022 CPD024 LOG001 CAD030
                      CXD020 CRD020 CRD021 CCD101.
           ACCEPT VARIA-W FROM TIME.
           move spaces to arquivo-work
           string "c:\tempsis\" varia-w into arquivo-work
           OPEN OUTPUT WORK.
           IF ST-CAD030 <> "00"
              MOVE "ERRO ABERTURA CAD030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD010 <> "00"
              MOVE "ERRO ABERTURA CCD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD006 <> "00"
              MOVE "ERRO ABERTURA CGD006: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD006 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD101 <> "00"
              MOVE "ERRO ABERTURA CCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD105 <> "00"
              MOVE "ERRO ABERTURA CCD105: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD105 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD115 <> "00"
              MOVE "ERRO ABERTURA CCD115: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD115 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD110 <> "00"
              MOVE "ERRO ABERTURA CCD110: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD110 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD120 <> "00"
              MOVE "ERRO ABERTURA CCD120: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD120 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD021 <> "00"
              MOVE "ERRO ABERTURA CPD021 "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD022 <> "00"
              MOVE "ERRO ABERTURA CPD022: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD022 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD024 <> "00"
              MOVE "ERRO ABERTURA CPD024: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD024 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD021 <> "00"
              MOVE "ERRO ABERTURA CRD021 "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO GS-VENCTO
           MOVE GS-VENCTO                            TO GS-ACP-VENCTO

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    MOVE "S" TO MOSTRAR-ERRO
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    MOVE "N" TO MOSTRAR-ERRO
                    PERFORM CARREGA-LISTA
               WHEN GS-VERIFICAR-FORMAPAGTO-TRUE
                    PERFORM VERIFICAR-FORMAPAGTO
               WHEN GS-APAGAR-PROGRAMACAO-TRUE
                    PERFORM APAGAR-PROGRAMACAO
               WHEN GS-VERIFICAR-SALDO-TRUE
                    PERFORM VERIFICAR-SALDO
               WHEN GS-IMPRIMIR-LOTE-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CONFIRMAR-LOTE-TRUE
                    PERFORM CONFIRMAR-LOTE
               WHEN GS-LE-CODRED-TRUE
                    PERFORM LER-CODRED
               WHEN GS-POPUP-CODRED-TRUE
                    PERFORM POPUP-CODRED
               WHEN GS-ATUALIZA-CODRED-TRUE
                    PERFORM ATUALIZA-CODRED
                    MOVE "N" TO MOSTRAR-ERRO
                    PERFORM CARREGA-LISTA
               WHEN GS-SELECIONAR-ITEM-TRUE
                    PERFORM SELECIONAR-ITEM
                    MOVE "N" TO MOSTRAR-ERRO
                    PERFORM CARREGA-LISTA
               WHEN GS-TRANSFERIR-SALDOS-TRUE
                    PERFORM TRANSFERIR-SALDOS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       SELECIONAR-ITEM SECTION.
           MOVE GS-LINDET(03:06)   TO CODIGO-WK
           MOVE GS-LINDET(132: 05) TO SEQ-WK
           MOVE GS-LINDET(138: 02) TO CLASSIF-WK
           READ WORK INVALID KEY
               MOVE "Work Não Encontrada Avisar o Programador1" TO
               MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM 140-EXIBIR-MENSAGEM
           NOT INVALID KEY
               IF SELECAO-WK = SPACES
                  MOVE "X" TO SELECAO-WK
                  REWRITE REG-WORK INVALID KEY
                       MOVE "Erro de Regravação...WORK" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM 140-EXIBIR-MENSAGEM
                  END-REWRITE
               ELSE
                  MOVE SPACES TO SELECAO-WK
                  REWRITE REG-WORK INVALID KEY
                       MOVE "Erro de Regravação...WORK" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM 140-EXIBIR-MENSAGEM
                  END-REWRITE.

       LER-CODRED SECTION.
           MOVE GS-COD-REDUZIDO TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY
                MOVE "*****"    TO DESCRICAO-CX20.

           MOVE DESCRICAO-CX20  TO GS-DESC-COD-REDUZIDO.

       POPUP-CODRED SECTION.
           CALL   "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CXP020T"
           MOVE PASSAR-STRING-1(52: 5) TO GS-COD-REDUZIDO
           MOVE GS-COD-REDUZIDO TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY
                MOVE "*****"    TO DESCRICAO-CX20.

           MOVE DESCRICAO-CX20  TO GS-DESC-COD-REDUZIDO.


       ATUALIZA-CODRED SECTION.
           CLOSE      CGD001
           OPEN I-O   CGD001

           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
               MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT WITH IGNORE LOCK AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   IF SELECAO-WK = "X"
                      MOVE GS-COD-REDUZIDO TO COD-RED-WK
                      REWRITE REG-WORK NOT INVALID KEY
                          MOVE CODIGO-WK   TO CODIGO-CG01
                          READ CGD001 INVALID KEY
                              MOVE "Código do CGD0001 Não Encontrado" TO
                              MENSAGEM
                              MOVE "C" TO TIPO-MSG
                              PERFORM 140-EXIBIR-MENSAGEM
                          NOT INVALID KEY
                              MOVE GS-COD-REDUZIDO TO COD-RED-CG01
                              REWRITE REG-CGD001 INVALID KEY
                                  MOVE "Erro de Regravação...CGD001" TO
                                  MENSAGEM
                                  MOVE "C" TO TIPO-MSG
                                  PERFORM 140-EXIBIR-MENSAGEM
                              END-REWRITE
                          END-READ
                      END-REWRITE
                   END-IF
               END-READ
           END-PERFORM

           CLOSE      CGD001
           OPEN INPUT CGD001.

       TRANSFERIR-SALDOS SECTION.
           MOVE "Deseja Consolidar os Saldos ?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM 140-EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              IF GS-FUTUROS = 1
                 PERFORM TRANSFERENCIA-CONF-FUTURA
              ELSE
                 PERFORM TRANSFERENCIA-CONF-SALDOS.

       TRANSFERENCIA-CONF-FUTURA SECTION.
           CLOSE    CPD020 CPD021 CPD022 CPD024 LOG001 CRD020 CRD021
                    CCD100 CCD101
           OPEN I-O CPD020 CPD021 CPD022 CPD024 LOG001 CRD020 CRD021
                    CCD100 CCD101
           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT WITH IGNORE LOCK AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      IF SELECAO-WK = "X"
                         COMPUTE VERIFICAR-VALOR =
                                      SALDO-CTACORR-WK - SALDO-ABERTO-WK
                         PERFORM LANCAR-CONTAS-FUTURAS
                         PERFORM ZERAR-SALDO
                      END-IF
                 END-READ
           END-PERFORM

           MOVE "Transferência de Saldos Realizada" TO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM 140-EXIBIR-MENSAGEM

           CLOSE      CPD020 CPD021 CPD022 CPD024 LOG001 CRD020 CRD021
                      CCD100 CCD101
           OPEN INPUT CPD020 CPD021 CPD022 CPD024 LOG001 CRD020 CRD021
                      CCD100 CCD101.

       TRANSFERENCIA-CONF-SALDOS SECTION.
           CLOSE    CPD020 CPD021 CPD022 CPD024 LOG001 CRD020 CRD021
                    CCD100 CCD101
           OPEN I-O CPD020 CPD021 CPD022 CPD024 LOG001 CRD020 CRD021
                    CCD100 CCD101
           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT WITH IGNORE LOCK AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      IF SELECAO-WK = "X"
                         COMPUTE VERIFICAR-VALOR =
                                      SALDO-CTACORR-WK - SALDO-ABERTO-WK
                         IF VERIFICAR-VALOR > 0
                            PERFORM LANCAR-CONTAS-PAGAR-N
                            PERFORM ZERAR-SALDO
                         ELSE
                            PERFORM LANCAR-CONTAS-RECEBER-N
                            PERFORM ZERAR-SALDO
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           MOVE "Transferência de Saldos Realizada" TO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM 140-EXIBIR-MENSAGEM

           CLOSE      CPD020 CPD021 CPD022 CPD024 LOG001 CRD020 CRD021
                      CCD100 CCD101
           OPEN INPUT CPD020 CPD021 CPD022 CPD024 LOG001 CRD020 CRD021
                      CCD100 CCD101.

       ZERAR-SALDO SECTION.
           MOVE CODIGO-WK         TO FORNEC-CC101.
           PERFORM ATUALIZA-SEQ-CCD101.

           INITIALIZE REG-CCD100

           MOVE GS-VENCTO        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV         TO DATA-MOVTO-CC100
           MOVE CODIGO-WK        TO FORNEC-CC100
           MOVE SEQ-CC101        TO SEQ-CC100
           MOVE GS-VENCTO        TO NR-DOCTO-CC100
           MOVE "CONSOLIDACAO SLD C/C - FINANC."
                                 TO DESCRICAO-CC100
           MOVE 888              TO CODREDUZ-APUR-CC100
           MOVE 0                TO TIPO-LCTO-CC100
           MOVE USUARIO-W        TO RESPONSAVEL-CC100
           MOVE USUARIO-W        TO DIGITADOR-CC100
           MOVE ZEROS            TO SEQ-CAIXA-CC100
           MOVE 1                TO NR-PARC-CC100
           MOVE 1                TO TOT-PARC-CC100
           MOVE ZEROS            TO TIPO-MOEDA-CC100
           MOVE DATA-MOVTO-CC100 TO DATA-EMISSAO-CC100
           MOVE DATA-MOVTO-CC100 TO DATA-VENCTO-CC100
           MOVE ZEROS            TO DATA-PGTO-CC100
           MOVE VERIFICAR-VALOR  TO VALOR-CC100
           MOVE ZEROS            TO JUROS-PAGO-CC100
           MOVE ZEROS            TO MULTA-PAGA-CC100
           MOVE ZEROS            TO DESCONTO-CC100
           MOVE VERIFICAR-VALOR  TO VALOR-PAGO-CC100

           IF VERIFICAR-VALOR > 0
              MOVE 1             TO CRED-DEB-CC100
           ELSE
              MOVE 0             TO CRED-DEB-CC100
           END-IF

      *DEFINIÇÃO DO ANDERSON
           MOVE 16               TO TIPO-FORN-CC100
           MOVE 1                TO LIBERADO-CC100
      *    MOVE 1                TO SITUACAO-CC100
           MOVE 0                TO SITUACAO-CC100

           WRITE REG-CCD100 INVALID KEY
                 MOVE "Erro de Gravação...CCD100" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM.

      *Lançamento de Contas a Pagar Normal
       LANCAR-CONTAS-PAGAR-N SECTION.
           MOVE CODIGO-WK        TO FORNEC-CP21
           PERFORM ATUALIZA-SEQ-CPD021

           INITIALIZE REG-CPD020
           MOVE CODIGO-WK        TO FORNEC-CP20
           MOVE SEQ-CP21         TO SEQ-CP20
           MOVE GS-VENCTO        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV         TO DATA-MOVTO-CP20

           MOVE 16               TO TIPO-FORN-CP20
           MOVE 11               TO PORTADOR-CP20
           MOVE 0                TO TIPO-MOEDA-CP20

           MOVE GS-VENCTO        TO NR-DOCTO-CP20

           MOVE DATA-MOVTO-CP20  TO DATA-EMISSAO-CP20
           MOVE DATA-MOVTO-CP20  TO DATA-VENCTO-CP20
           MOVE "IMPLANTACAO SLD CONTA CORRENTE" TO
                                    DESCRICAO-CP20
           MOVE 1                TO TIPO-CONTA-CP20
           MOVE 0                TO PREV-DEF-CP20
           MOVE 0                TO SITUACAO-CP20
           MOVE 0                TO JUROS-MORA-CP20
           MOVE 0                TO MULTA-ATRASO-CP20
           MOVE 0                TO TAXA-APLIC-CP20
           MOVE 0                TO DESCONTO-CP20
           MOVE 888              TO CODREDUZ-APUR-CP20
           MOVE USUARIO-W        TO RESPONSAVEL-CP20
           MOVE USUARIO-W        TO DIGITADOR-CP20
           MOVE 0                TO SEQ-CAIXA-CP20
           MOVE VERIFICAR-VALOR  TO VALOR-TOT-CP20
           MOVE 0                TO DATA-PGTO-CP20
           MOVE 0                TO VALOR-LIQ-CP20
           MOVE 1                TO NR-PARC-CP20
           MOVE 1                TO TOT-PARC-CP20
           MOVE 1                TO LIBERADO-CP20
           MOVE 0                TO CONTABILIZADO-CP20

           WRITE REG-CPD020 INVALID KEY
               MOVE "Erro de Gravação...CPD020" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM 140-EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE CODIGO-WK        TO FORNEC-CP22
               MOVE SEQ-CP21         TO SEQ-CP22
               MOVE FORMA-PAGTO-WK   TO OBS-CP22

               WRITE REG-CPD022 INVALID KEY
                   MOVE "Erro de Gravação...CPD022" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM 140-EXIBIR-MENSAGEM
               END-WRITE
           END-WRITE.

      *Lançamento de Contas a Receber Normal
       LANCAR-CONTAS-RECEBER-N SECTION.
           MOVE 9                TO CLASS-CLIENTE-CR21
           MOVE CODIGO-WK        TO CLIENTE-CR21
           PERFORM ATUALIZA-SEQ-CRD021

           INITIALIZE REG-CRD020
           MOVE 9                TO CLASS-CLIENTE-CR20
           MOVE CODIGO-WK        TO CLIENTE-CR20
           MOVE SEQ-CR21         TO SEQ-CR20
           MOVE GS-VENCTO        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV         TO DATA-MOVTO-CR20
           MOVE GS-VENCTO        TO NR-DOCTO-CR20
           MOVE "CONSOLIDACAO SLD C/C - FINANC." TO DESCRICAO-CR20
           MOVE 0                TO SITUACAO-CR20
           MOVE 0                TO TIPO-MOEDA-CR20
           MOVE 1                TO CARTEIRA-CR20
           MOVE 1                TO NR-PARC-CR20
           MOVE 1                TO TOT-PARC-CR20
           MOVE GS-VENCTO        TO DATA-EMISSAO-CR20
           MOVE DATA-MOVTO-CR20  TO DATA-VENCTO-CR20
           MOVE 888              TO CODREDUZ-APUR-CR20
           MOVE VERIFICAR-VALOR  TO VALOR-TOT-CR20
           MOVE VERIFICAR-VALOR  TO VALOR-SALDO-CR20
           MOVE 0                TO JURO-RCTO-CR20
           MOVE 0                TO MULTA-RCTO-CR20
           MOVE 0                TO DESCONTO-CR20
           MOVE 0                TO DATA-RCTO-CR20
           MOVE 0                TO VALOR-LIQ-CR20
           MOVE 0                TO TIPO-DOCTO-CR20
           MOVE USUARIO-W        TO RESPONSAVEL-CR20
           MOVE USUARIO-W        TO DIGITADOR-CR20
           MOVE 0                TO SEQ-CAIXA-CR20
           MOVE 0                TO VENDEDOR-CR20
           MOVE 0                TO CONTABILIZADO-CR20
           MOVE SPACES           TO NR-NOTA-FISCAL-CR20
           MOVE 0                TO DATA-NTA-FISCAL-CR20
           MOVE 0                TO CARTAO-CRED-CR20
           MOVE 0                TO TAXA-ADMINIST-CREDITO-CR20
           MOVE 0                TO TAXA-ADMINIST-PARCELA-CR20
           MOVE SPACES           TO FORMA-PAGTO-CR20
           MOVE SPACES           TO DCR-MEM-CR20
           MOVE 6                TO SITUACAO-TIT-CR20
           MOVE 11               TO PORTADOR-CR20

           WRITE REG-CRD020 INVALID KEY
                 MOVE "Erro de Gravação...CRD020" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM.

      *Lançamento de Contas Futuras
       LANCAR-CONTAS-FUTURAS SECTION.
           INITIALIZE REG-CCD100
           MOVE 0              TO SITUACAO-CC100
           MOVE CODIGO-WK      TO FORNEC-CC100
           MOVE GS-VENCTO      TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO DATA-VENCTO-CC100

           START CCD100 KEY IS NOT LESS ALT1-CC100 INVALID KEY
                 MOVE "10" to ST-CCD100.

           PERFORM UNTIL ST-CCD100 = "10"
                 READ CCD100 NEXT WITH IGNORE LOCK AT END
                      MOVE "10" TO ST-CCD100
                 NOT AT END
                      IF SITUACAO-CC100 > 0 OR
                         CODIGO-WK      <>  FORNEC-CC100
                         MOVE "10" TO ST-CCD100
                      ELSE
                         EVALUATE CRED-DEB-CC100
      *Débito
                             WHEN 1 PERFORM LANCAR-CRD020-FUTURO
      *Crédito
                             WHEN 0 PERFORM LANCAR-CPD020-FUTURO
                         END-EVALUATE
                      END-IF
                 END-READ
           END-PERFORM.

       LANCAR-CPD020-FUTURO SECTION.
           MOVE CODIGO-WK        TO FORNEC-CP21
           PERFORM ATUALIZA-SEQ-CPD021

           INITIALIZE REG-CPD020
           MOVE CODIGO-WK         TO FORNEC-CP20
           MOVE SEQ-CP21          TO SEQ-CP20
           MOVE GS-VENCTO         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO DATA-MOVTO-CP20

           MOVE 16                TO TIPO-FORN-CP20
           MOVE 11                TO PORTADOR-CP20
           MOVE 0                 TO TIPO-MOEDA-CP20

           MOVE GS-VENCTO         TO NR-DOCTO-CP20

           MOVE DATA-MOVTO-CP20   TO DATA-EMISSAO-CP20
           MOVE DATA-VENCTO-CC100 TO DATA-VENCTO-CP20
           MOVE DESCRICAO-CC100   TO DESCRICAO-CP20
           MOVE 1                 TO TIPO-CONTA-CP20
           MOVE 0                 TO PREV-DEF-CP20
           MOVE 0                 TO SITUACAO-CP20
           MOVE 0                 TO JUROS-MORA-CP20
           MOVE 0                 TO MULTA-ATRASO-CP20
           MOVE 0                 TO TAXA-APLIC-CP20
           MOVE 0                 TO DESCONTO-CP20
           MOVE 888               TO CODREDUZ-APUR-CP20
           MOVE RESPONSAVEL-CC100 TO RESPONSAVEL-CP20
           MOVE DIGITADOR-CC100   TO DIGITADOR-CP20
           MOVE 0                 TO SEQ-CAIXA-CP20
           MOVE VALOR-CC100       TO VALOR-TOT-CP20
           MOVE 0                 TO DATA-PGTO-CP20
           MOVE 0                 TO VALOR-LIQ-CP20
           MOVE 1                 TO NR-PARC-CP20
           MOVE 1                 TO TOT-PARC-CP20
           MOVE 0                 TO LIBERADO-CP20
           MOVE 0                 TO CONTABILIZADO-CP20

           WRITE REG-CPD020 INVALID KEY
               MOVE "Erro de Gravação...CPD020"
                 TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM 140-EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE CODIGO-WK        TO FORNEC-CP22
               MOVE SEQ-CP21         TO SEQ-CP22
               MOVE FORMA-PAGTO-WK   TO OBS-CP22

               WRITE REG-CPD022 INVALID KEY
                   MOVE "Erro de Gravação...CPD022"
                     TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM 140-EXIBIR-MENSAGEM
               END-WRITE
           END-WRITE.

       LANCAR-CRD020-FUTURO SECTION.
           MOVE 9                TO CLASS-CLIENTE-CR21
           MOVE CODIGO-WK        TO CLIENTE-CR21
           PERFORM ATUALIZA-SEQ-CRD021

           INITIALIZE REG-CRD020
           MOVE 9                TO CLASS-CLIENTE-CR20
           MOVE CODIGO-WK        TO CLIENTE-CR20
           MOVE SEQ-CR21         TO SEQ-CR20
           MOVE GS-VENCTO        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV         TO DATA-MOVTO-CR20
           MOVE GS-VENCTO        TO NR-DOCTO-CR20
           MOVE DESCRICAO-CC100  TO DESCRICAO-CR20
           MOVE 0                TO SITUACAO-CR20
           MOVE 0                TO TIPO-MOEDA-CR20
           MOVE 1                TO CARTEIRA-CR20
           MOVE 1                TO NR-PARC-CR20
           MOVE 1                TO TOT-PARC-CR20
           MOVE GS-VENCTO        TO DATA-EMISSAO-CR20
           MOVE DATA-VENCTO-CC100 TO DATA-VENCTO-CR20
           MOVE 888              TO CODREDUZ-APUR-CR20
           MOVE VALOR-CC100      TO VALOR-TOT-CR20
           MOVE VALOR-CC100      TO VALOR-SALDO-CR20
           MOVE 0                TO JURO-RCTO-CR20
           MOVE 0                TO MULTA-RCTO-CR20
           MOVE 0                TO DESCONTO-CR20
           MOVE 0                TO DATA-RCTO-CR20
           MOVE 0                TO VALOR-LIQ-CR20
           MOVE 0                TO TIPO-DOCTO-CR20
           MOVE USUARIO-W        TO RESPONSAVEL-CR20
           MOVE USUARIO-W        TO DIGITADOR-CR20
           MOVE 0                TO SEQ-CAIXA-CR20
           MOVE 0                TO VENDEDOR-CR20
           MOVE 0                TO CONTABILIZADO-CR20
           MOVE SPACES           TO NR-NOTA-FISCAL-CR20
           MOVE 0                TO DATA-NTA-FISCAL-CR20
           MOVE 0                TO CARTAO-CRED-CR20
           MOVE 0                TO TAXA-ADMINIST-CREDITO-CR20
           MOVE 0                TO TAXA-ADMINIST-PARCELA-CR20
           MOVE SPACES           TO FORMA-PAGTO-CR20
           MOVE SPACES           TO DCR-MEM-CR20
           MOVE 6                TO SITUACAO-TIT-CR20
           MOVE 11               TO PORTADOR-CR20

           WRITE REG-CRD020 INVALID KEY
                 MOVE "Erro de Gravação...CRD020" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM.


       CONFIRMAR-LOTE SECTION.
           CLOSE      CPD020 CPD021 CPD022 CPD024 LOG001
           OPEN I-O   CPD020 CPD021 CPD022 CPD024 LOG001

           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
               MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT WITH IGNORE LOCK AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   IF SELECAO-WK = "X"
                      MOVE 122              TO FORNEC-CP21
                      PERFORM ATUALIZA-SEQ-CPD021

                      INITIALIZE REG-CPD020

                      STRING WS-ANO-CPU WS-MES-CPU WS-DIA-CPU INTO
                             DATA-MOVTO-CP20

                      MOVE 122              TO FORNEC-CP20
                      MOVE SEQ-CP21         TO SEQ-CP20
                      IF FORMA-PAGTO-WK = "EM MAOS"
                         MOVE 11            TO PORTADOR-CP20
                      ELSE
                         MOVE 13            TO PORTADOR-CP20
                      END-IF
                      ACCEPT WS-HORA-SYS FROM TIME

                      STRING CODIGO-WK HH-SYS MM-SYS INTO NR-DOCTO-CP20

                      STRING WS-ANO-CPU WS-MES-CPU WS-DIA-CPU INTO
                             DATA-EMISSAO-CP20

                      STRING GS-ACP-VENCTO(5:4) GS-ACP-VENCTO(3:2)
                             GS-ACP-VENCTO(1:2) INTO DATA-VENCTO-CP20

                      MOVE NOME-WK          TO DESCRICAO-CP20
                      MOVE 0                TO TIPO-CONTA-CP20

                      MOVE USUARIO-W        TO RESPONSAVEL-CP20
                                               DIGITADOR-CP20
                      MOVE 0                TO TIPO-MOEDA-CP20
                      MOVE 1                TO NR-PARC-CP20
                      MOVE 1                TO TOT-PARC-CP20


      * VER COM O ANDERSON
                      MOVE COD-RED-WK       TO CODREDUZ-APUR-CP20

                      MOVE ZEROS            TO SITUACAO-CP20
                                               LIBERADO-CP20
                                               VALOR-LIQ-CP20
                                               JURO-PAGO-CP20
                                               JUROS-MORA-CP20
                                               MULTA-ATRASO-CP20
                                               TAXA-APLIC-CP20
                                               MULTA-PAGA-CP20
                                               DESCONTO-CP20
                                               CONTABILIZADO-CP20
                                               SEQ-CAIXA-CP20
                                               DATA-PGTO-CP20
                                               VALOR-LIQ-CP20

                      COMPUTE VALOR-TOT-CP20 =
                                      SALDO-CTACORR-WK - SALDO-ABERTO-WK

                      MOVE 0                TO PREV-DEF-CP20

      * VER COM O ANDERSON O TIPO-FORN-CP20
                      MOVE 19               TO TIPO-FORN-CP20

                      WRITE REG-CPD020 INVALID KEY
                          MOVE "Erro de Gravação...CPD020" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM 140-EXIBIR-MENSAGEM
                      END-WRITE

                      MOVE 122              TO FORNEC-CP22
                      MOVE SEQ-CP21         TO SEQ-CP22
                      MOVE FORMA-PAGTO-WK   TO OBS-CP22

                      WRITE REG-CPD022 INVALID KEY
                          MOVE "Erro de Gravação...CPD022" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM 140-EXIBIR-MENSAGEM
                      END-WRITE

                   END-IF
               END-READ
           END-PERFORM

           CLOSE      CPD020 CPD021 CPD022 CPD024 LOG001
           OPEN INPUT CPD020 CPD021 CPD022 CPD024 LOG001.

       ATUALIZA-SEQ-CPD021 SECTION.
           READ CPD021 INVALID KEY
                MOVE 1 TO SEQ-CP21
                WRITE REG-CPD021 INVALID KEY
                      MOVE "Erro de Gravação...CPD021" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                NOT INVALID KEY
                        MOVE USUARIO-W   TO LOG1-USUARIO
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        MOVE WS-DATA-CPU TO LOG1-DATA
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE WS-HORA-SYS TO LOG1-HORAS
                        MOVE "I"         TO LOG1-OPERACAO
                        MOVE "CPD021"    TO LOG1-ARQUIVO
                        MOVE "CCP208"    TO LOG1-PROGRAMA
                        MOVE REG-CPD021  TO LOG1-REGISTRO
                        WRITE REG-LOG001
                        END-WRITE

                END-WRITE
              NOT INVALID KEY
                  ADD 1 TO SEQ-CP21
                  REWRITE REG-CPD021 INVALID KEY
                      MOVE "Erro de Regravação...CPD021" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                  NOT INVALID KEY
                        MOVE USUARIO-W   TO LOG1-USUARIO
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        MOVE WS-DATA-CPU TO LOG1-DATA
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE WS-HORA-SYS TO LOG1-HORAS
                        MOVE "A"         TO LOG1-OPERACAO
                        MOVE "CPD021"    TO LOG1-ARQUIVO
                        MOVE "CPP022"    TO LOG1-PROGRAMA
                        MOVE REG-CPD021  TO LOG1-REGISTRO
                        WRITE REG-LOG001
                        END-WRITE
                  END-REWRITE
           END-READ.

       ATUALIZA-SEQ-CRD021 SECTION.
           READ CRD021 INVALID KEY
                MOVE 1 TO SEQ-CR21
                WRITE REG-CRD021 INVALID KEY
                      MOVE "Erro de Gravação...CRD021" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                NOT INVALID KEY
                        MOVE USUARIO-W   TO LOG1-USUARIO
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        MOVE WS-DATA-CPU TO LOG1-DATA
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE WS-HORA-SYS TO LOG1-HORAS
                        MOVE "I"         TO LOG1-OPERACAO
                        MOVE "CRD021"    TO LOG1-ARQUIVO
                        MOVE "CCP208"    TO LOG1-PROGRAMA
                        MOVE REG-CRD021  TO LOG1-REGISTRO
                        WRITE REG-LOG001
                        END-WRITE
                END-WRITE
              NOT INVALID KEY
                  ADD 1 TO SEQ-CR21
                  REWRITE REG-CRD021 INVALID KEY
                      MOVE "Erro de Regravação...CRD021" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                  NOT INVALID KEY
                        MOVE USUARIO-W   TO LOG1-USUARIO
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        MOVE WS-DATA-CPU TO LOG1-DATA
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE WS-HORA-SYS TO LOG1-HORAS
                        MOVE "A"         TO LOG1-OPERACAO
                        MOVE "CRD021"    TO LOG1-ARQUIVO
                        MOVE "CCP208"    TO LOG1-PROGRAMA
                        MOVE REG-CRD021  TO LOG1-REGISTRO
                        WRITE REG-LOG001
                        END-WRITE
                  END-REWRITE
           END-READ.

       ATUALIZA-SEQ-CCD101 SECTION.
           READ CCD101 INVALID KEY
                MOVE 1 TO SEQ-CC101
                WRITE REG-CCD101 INVALID KEY
                      MOVE "Erro de Gravação...CCD101" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                END-WRITE
              NOT INVALID KEY
                  ADD 1 TO SEQ-CC101
                  REWRITE REG-CCD101 INVALID KEY
                      MOVE "Erro de Regravação...CCD101" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                  END-REWRITE
           END-READ.

       VERIFICAR-SALDO SECTION.
           MOVE 0 TO GS-FLAG-CRITICA
           IF SALDO-CTACORR-TOT - SALDO-ABERTO-TOT < 0
              MOVE "Não é Possível Gerar a Programação Financeira, pois
      -       "o Saldo está Negativo" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.

       APAGAR-PROGRAMACAO SECTION.
           MOVE GS-LINDET(03:06)   TO CODIGO-WK
           MOVE GS-LINDET(132: 05) TO SEQ-WK
           MOVE GS-LINDET(138: 02) TO CLASSIF-WK
           READ WORK INVALID KEY
               MOVE "Work Não Encontrada Avisar o Programador2" TO
               MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM 140-EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE "Você Tem Certeza que Deseja apagar a Programação?"
               TO MENSAGEM
               MOVE "Q" TO TIPO-MSG
               PERFORM 140-EXIBIR-MENSAGEM
               IF RESP-MSG EQUAL "S"
                  DELETE WORK INVALID KEY
                       MOVE "Erro de Exclusão...WORK" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM 140-EXIBIR-MENSAGEM
                  NOT INVALID KEY
                       MOVE "N" TO MOSTRAR-ERRO
                       PERFORM CARREGA-LISTA.

       VERIFICAR-FORMAPAGTO SECTION.
           MOVE GS-LINDET(03:06)   TO CODIGO-WK
           MOVE GS-LINDET(132: 05) TO SEQ-WK
           MOVE GS-LINDET(138: 02) TO CLASSIF-WK
           READ WORK INVALID KEY
               MOVE "Work Não Encontrada Avisar o Programador3" TO
               MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM 140-EXIBIR-MENSAGEM
           NOT INVALID KEY
               IF FORMA-PAGTO-WK(1:2) = "BC"
                  MOVE "EM MAOS" TO FORMA-PAGTO-WK
                  INITIALIZE BANCO-WK
                             AGENCIA-WK
                             NR-CONTA-WK
                             COMPLEMENTO-WK
                             NOME-BANCO-WK
                             TITULAR-WK
                  REWRITE REG-WORK
                  END-REWRITE
                  MOVE "N" TO MOSTRAR-ERRO
                  PERFORM CARREGA-LISTA
               ELSE
                  INITIALIZE CHAVE-CG06
                  MOVE CODIGO-WK           TO CODIGO-CG06
                  START CGD006 KEY IS NOT LESS CHAVE-CG06 INVALID KEY
                      MOVE "10" TO ST-CGD006
                  END-START

                  PERFORM UNTIL ST-CGD006 = "10"
                      READ CGD006 NEXT WITH IGNORE LOCK AT END
                          MOVE "10" TO ST-CGD006
                      NOT AT END
                          IF CODIGO-WK <> CODIGO-CG06
                             MOVE "10" TO ST-CGD006
                          ELSE
                             IF PREFERENCIAL-CG06 = 1
                                MOVE BANCO-CG06 TO CODIGO-CAD30
                                READ CAD030 INVALID KEY
                                   MOVE "-----"    TO DESCRICAO-CAD30
                                END-READ
                                MOVE BANCO-CG06    TO BANCO-WK
                                MOVE DESCRICAO-CAD30 TO NOME-BANCO-WK
                                MOVE AGENCIA-CG06  TO AGENCIA-WK
                                MOVE NR-CONTA-CG06 TO NR-CONTA-WK
                                MOVE COMPLEMENTO-CONTA-CG06 TO
                                                      COMPLEMENTO-WK
                                EVALUATE TIPO-DE-CONTA-CG06
                                   WHEN 1 STRING "BC "   BANCO-CG06 "-"
                                                         DESCRICAO-CAD30
                                                 " AG "  AGENCIA-CG06
                                                 " C/C " NR-CONTA-CG06
                                                 " - "
                                                 COMPLEMENTO-CONTA-CG06
                                                 DELIMITED BY
                                                 "  " INTO
                                          FORMA-PAGTO-WK
                                   WHEN 2 STRING "BC "   BANCO-CG06 "-"
                                                         DESCRICAO-CAD30
                                                 " AG "  AGENCIA-CG06
                                                 " C/P " NR-CONTA-CG06
                                                 " - "
                                                 COMPLEMENTO-CONTA-CG06
                                                 DELIMITED BY
                                                 "  " INTO
                                          FORMA-PAGTO-WK
                                END-EVALUATE
                             END-IF
                          END-IF
                      END-READ
                  END-PERFORM
                  IF FORMA-PAGTO-WK = "EM MAOS"
                     MOVE "Cliente Não Possui Conta Preferêncial" TO
                     MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM 140-EXIBIR-MENSAGEM
                  ELSE

                     REWRITE REG-WORK
                     END-REWRITE
                     MOVE "N" TO MOSTRAR-ERRO
                     PERFORM CARREGA-LISTA.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       GRAVA-WORK SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-MESANO-BASE TO MESANO-W.
           MOVE ANO-WW TO ANO-II.
           MOVE MES-WW TO MES-II.

           MOVE ZEROS TO CODIGO-CG01.
           START CGD001 KEY IS NOT < CODIGO-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.

           PERFORM UNTIL ST-CGD001 = "10"
             READ CGD001 NEXT WITH IGNORE LOCK AT END
                  MOVE "10" TO ST-CGD001
             NOT AT END
                  IF (GS-ATIVO = 1 AND SITUACAO-CG01 <> 2) OR
                     (GS-INATIVO = 1 AND SITUACAO-CG01 = 2)
                     MOVE ZEROS TO CLASSIFICACAO
                     IF GS-T-FRANQUIA = 1 AND T-FRANQUIA-CG01 = 1
                        MOVE 1 TO CLASSIFICACAO
                        PERFORM CONT-GRAVA-WORK
                     ELSE
                        IF GS-T-TERCEIRIZADO = 1 AND
                           T-TERCEIRIZADO-CG01 = 1
                           MOVE 2 TO CLASSIFICACAO
                           PERFORM CONT-GRAVA-WORK
                        ELSE
                           IF GS-T-OUTRO = 1 AND OUTRO3-CG01 = 1
                              MOVE 3 TO CLASSIFICACAO
                              PERFORM CONT-GRAVA-WORK
                           ELSE
                              IF GS-T-INVESTIDOR = 1 AND
                                 T-INVESTIDOR-CG01 = 1
                                 MOVE 4 TO CLASSIFICACAO
                                 PERFORM CONT-GRAVA-WORK
                              ELSE
                                 IF GS-T-IMPOSTO = 1 AND
                                    T-IMPOSTO-CG01 = 1
                                    MOVE 5 TO CLASSIFICACAO
                                    PERFORM CONT-GRAVA-WORK
                                 ELSE
                                    IF GS-T-VEND = 1 AND T-VEND-CG01
                                       MOVE 6 TO CLASSIFICACAO
                                       PERFORM CONT-GRAVA-WORK
                                    ELSE
                                       IF GS-T-CINEG = 1 AND
                                          T-CINEG-CG01 = 1
                                          MOVE 7 TO CLASSIFICACAO
                                          PERFORM CONT-GRAVA-WORK
                                       ELSE
                                          IF GS-T-FOTOG = 1 AND
                                             T-FOTOG-CG01 = 1
                                             MOVE 8 TO CLASSIFICACAO
                                             PERFORM CONT-GRAVA-WORK
                                          ELSE
                                             IF GS-T-REPRES = 1 AND
                                                T-REPRES-CG01 = 1
                                                MOVE 9 TO CLASSIFICACAO
                                                PERFORM CONT-GRAVA-WORK
                                             ELSE
                                                IF GS-T-FUNC = 1 AND
                                                   T-FUNC-CG01 = 1
                                                   MOVE 10 TO
                                                        CLASSIFICACAO
                                                   PERFORM

                                                       CONT-GRAVA-WORK
                                                ELSE
                                                   IF GS-T-PES-JUR = 1
                                                      AND T-PESJUR-CG01
                                                          = 1
                                                      MOVE 11 TO
                                                           CLASSIFICACAO
                                                      PERFORM
                                                         CONT-GRAVA-WORK
                                                   ELSE
                                                      IF GS-T-PES-FIS =1
                                                       AND T-PESFIS-CG01
                                                        = 1
                                                        MOVE 12 TO
                                                           CLASSIFICACAO
                                                        PERFORM
                                                        CONT-GRAVA-WORK
                                                      END-IF
                                                   END-IF
                                                END-IF
                                             END-IF
                                          END-IF
                                       END-IF
                                    END-IF
                                 END-IF
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                  END-IF
             END-READ
           END-PERFORM
           IF GS-FUTUROS = 0
              PERFORM GRAVA-SALDO-CTACORR-N
           ELSE
              PERFORM GRAVAR-SALDO-CTACORR-F.
       CONT-GRAVA-WORK SECTION.
           INITIALIZE REG-WORK.
           MOVE CODIGO-CG01 TO GS-EXIBE-PROCESS
           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM CONT-GRAVA1-WORK.

       CONT-GRAVA1-WORK SECTION.
           ADD 1                    TO SEQ-W
           MOVE SEQ-W               TO SEQ-WK


           MOVE CLASSIFICACAO       TO CLASSIF-WK
           MOVE CODIGO-CG01         TO CODIGO-WK
           MOVE NOME-CG01           TO NOME-WK
           MOVE ZEROS               TO SALDO-CTACORR-WK
           MOVE "EM MAOS"           TO FORMA-PAGTO-WK
           MOVE ZEROS               TO TOT-CREDITO-WK

           IF COD-RED-CG01 IS NOT NUMERIC
              MOVE 0 TO COD-RED-CG01
           END-IF
           MOVE COD-RED-CG01 TO COD-RED-WK

           INITIALIZE CHAVE-CG06
           MOVE CODIGO-CG01         TO CODIGO-CG06
           START CGD006 KEY IS NOT LESS CHAVE-CG06 INVALID KEY
               MOVE "10" TO ST-CGD006.

           PERFORM UNTIL ST-CGD006 = "10"
               READ CGD006 NEXT WITH IGNORE LOCK AT END
                   MOVE "10" TO ST-CGD006
               NOT AT END
                   IF CODIGO-CG01 <> CODIGO-CG06
                      MOVE "10" TO ST-CGD006
                   ELSE
                      IF PREFERENCIAL-CG06 = 1
                         MOVE BANCO-CG06    TO CODIGO-CAD30
                         READ CAD030 INVALID KEY
                              MOVE "------" TO DESCRICAO-CAD30
                         END-READ
                         MOVE BANCO-CG06      TO BANCO-WK
                         MOVE DESCRICAO-CAD30 TO NOME-BANCO-WK
                         MOVE AGENCIA-CG06    TO AGENCIA-WK
                         MOVE NR-CONTA-CG06   TO NR-CONTA-WK
                         MOVE COMPLEMENTO-CONTA-CG06 TO COMPLEMENTO-WK
                         MOVE TITULAR-CONTA-CG06 TO TITULAR-WK
                         EVALUATE TIPO-DE-CONTA-CG06
                            WHEN 1 STRING "BC "   BANCO-CG06 "-"
                                                  DESCRICAO-CAD30
                                          " AG "  AGENCIA-CG06
                                          " C/C " NR-CONTA-CG06
                                          " - "
                                          COMPLEMENTO-CONTA-CG06
                                          DELIMITED BY
                                          "  " INTO
                                   FORMA-PAGTO-WK
                            WHEN 2 STRING "BC "   BANCO-CG06 "-"
                                                  DESCRICAO-CAD30
                                          " AG "  AGENCIA-CG06
                                          " C/P " NR-CONTA-CG06
                                          " - "
                                          COMPLEMENTO-CONTA-CG06
                                          DELIMITED BY
                                          "  " INTO
                                   FORMA-PAGTO-WK
                         END-EVALUATE
                      END-IF
                   END-IF
               END-READ
           END-PERFORM

           IF GS-FUTUROS = 0
              INITIALIZE REG-CPD020 SALDO-ABERTO-WK
              MOVE 0   TO SITUACAO-CP20
              MOVE 122 TO FORNEC-CP20
              START CPD020 KEY IS NOT LESS ALT4-CP20 INVALID KEY
                  MOVE "10" TO ST-CPD020
              END-START
              PERFORM UNTIL ST-CPD020 = "10"
                  READ CPD020 NEXT WITH IGNORE LOCK AT END
                      MOVE "10" TO ST-CPD020
                  NOT AT END
                      IF SITUACAO-CP20 <> 0 OR
                         FORNEC-CP20   <> 122
                         MOVE "10" TO ST-CPD020
                      ELSE
                         IF NR-DOCTO-CP20(1:6) = CODIGO-WK
                            ADD VALOR-TOT-CP20 TO SALDO-ABERTO-WK
                         END-IF
                      END-IF
                  END-READ
              END-PERFORM
           END-IF

           MOVE "X" TO SELECAO-WK

           MOVE CODIGO-CG01 TO GS-EXIBE-PROCESS
           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           WRITE REG-WORK
           END-WRITE.

       GRAVA-SALDO-CTACORR-N SECTION.
           MOVE GS-VENCTO(1: 2) TO VENCTO-INV(7: 2)
           MOVE GS-VENCTO(3: 2) TO VENCTO-INV(5: 2)
           MOVE GS-VENCTO(5: 4) TO VENCTO-INV(1: 4).

      *    GRAVAR SALDO CONTA CORRENTE

           INITIALIZE REG-WORK
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           MOVE VENCTO-INV(1: 6) TO MESANO-I

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT WITH IGNORE LOCK AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
      *             pegar saldo anterior (acumulado)
                    MOVE CODIGO-WK   TO FORNEC-CC10
                    MOVE ZEROS       TO ANOMES-VCTO-CC10
                    START CCD010 KEY IS NOT < ALT-CC10 INVALID KEY
                          MOVE "10" TO ST-CCD010
                    END-START
                    PERFORM UNTIL ST-CCD010 = "10"
                          READ CCD010 NEXT WITH IGNORE LOCK AT END
                               MOVE "10" TO ST-CCD010
                          NOT AT END
                             IF ANOMES-VCTO-CC10 NOT < MESANO-I OR
                                FORNEC-CC10 NOT = CODIGO-WK
                                MOVE "10" TO ST-CCD010
                             ELSE
                                ADD SALDOS-CC10 TO SALDO-CTACORR-WK
                                SUBTRACT SALDOE-CC10 FROM
                                         SALDO-CTACORR-WK
                             END-IF
                          END-READ
                    END-PERFORM

                    MOVE CODIGO-WK TO FORNEC-CC100
                    MOVE ZEROS     TO SITUACAO-CC100
                    MOVE VENCTO-INV(1: 6) TO DATA-VENCTO-CC100(1: 6)
                    MOVE 01               TO DATA-VENCTO-CC100(7: 2)
                    START CCD100 KEY IS NOT < ALT1-CC100 INVALID KEY
                          MOVE "10" TO ST-CCD100
                    END-START
                    PERFORM UNTIL ST-CCD100 = "10"
                          READ CCD100 NEXT WITH IGNORE LOCK AT END
                               MOVE "10" TO ST-CCD100
                          NOT AT END
                               IF FORNEC-CC100     <> CODIGO-WK OR
                                  SITUACAO-CC100    > 0         OR
                                  DATA-VENCTO-CC100 > VENCTO-INV
                                  MOVE "10" TO ST-CCD100
                               ELSE
                                  IF CRED-DEB-CC100 = 0
                                     ADD VALOR-CC100 TO
                                         SALDO-CTACORR-WK
                                  ELSE
                                     SUBTRACT VALOR-CC100 FROM
                                              SALDO-CTACORR-WK
                                  END-IF
                               END-IF
                          END-READ
                    END-PERFORM
                    REWRITE REG-WORK
               END-READ
           END-PERFORM.

      *    APAGAR TODOS REGISTROS DO WORK QUE ESTIVEREM
      *    COM TOT-CREDITO-WK = ZEROS E SALDO-CTACORR-WK = ZEROS

           INITIALIZE REG-WORK
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
                READ WORK NEXT WITH IGNORE LOCK AT END
                     MOVE "10" TO ST-WORK
                NOT AT END
                     IF TOT-CREDITO-WK   = ZEROS AND
                        SALDO-CTACORR-WK = ZEROS
                        DELETE WORK
                        END-DELETE
                     ELSE
                        EVALUATE GS-TIPO-SALDO
                           WHEN 1 IF SALDO-CTACORR-WK < 0
                                     DELETE WORK
                                     END-DELETE
                                  ELSE
                                     IF SALDO-ABERTO-WK >
                                        SALDO-CTACORR-WK
                                        DELETE WORK
                                        END-DELETE
                                     END-IF
                                  END-IF
                           WHEN 2 IF SALDO-CTACORR-WK > 0
                                     DELETE WORK
                                     END-DELETE
                                  END-IF
                        END-EVALUATE
                        IF SALDO-CTACORR-WK = SALDO-ABERTO-WK
                           DELETE WORK
                           END-DELETE
                        END-IF
                     END-IF
                END-READ
           END-PERFORM.

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       GRAVAR-SALDO-CTACORR-F SECTION.
           MOVE GS-VENCTO(1: 2) TO VENCTO-INV(7: 2)
           MOVE GS-VENCTO(3: 2) TO VENCTO-INV(5: 2)
           MOVE GS-VENCTO(5: 4) TO VENCTO-INV(1: 4).

      *    GRAVAR SALDO CONTA CORRENTE

           INITIALIZE REG-WORK
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           MOVE VENCTO-INV(1: 6) TO MESANO-I

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT WITH IGNORE LOCK AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    INITIALIZE REG-CCD100
                    MOVE ZEROS            TO SITUACAO-CC100
                    MOVE CODIGO-WK        TO FORNEC-CC100
                    MOVE VENCTO-INV       TO DATA-VENCTO-CC100
                    START CCD100 KEY IS NOT LESS ALT1-CC100 INVALID KEY
                          MOVE "10" TO ST-CCD100
                    END-START
                    PERFORM UNTIL ST-CCD100 = "10"
                          READ CCD100 NEXT WITH IGNORE LOCK AT END
                               MOVE "10" TO ST-CCD100
                          NOT AT END
                               IF FORNEC-CC100     <> CODIGO-WK OR
                                  SITUACAO-CC100    > 0
                                  MOVE "10" TO ST-CCD100
                               ELSE
                                  IF CRED-DEB-CC100 = 0
                                     ADD VALOR-CC100 TO
                                         SALDO-CTACORR-WK
                                  ELSE
                                     SUBTRACT VALOR-CC100 FROM
                                              SALDO-CTACORR-WK
                                  END-IF
                               END-IF
                          END-READ
                    END-PERFORM
                    REWRITE REG-WORK
               END-READ
           END-PERFORM.

      *    APAGAR TODOS REGISTROS DO WORK QUE ESTIVEREM
      *    COM TOT-CREDITO-WK = ZEROS E SALDO-CTACORR-WK = ZEROS

           INITIALIZE REG-WORK
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
                READ WORK NEXT WITH IGNORE LOCK AT END
                     MOVE "10" TO ST-WORK
                NOT AT END
                     IF TOT-CREDITO-WK   = ZEROS AND
                        SALDO-CTACORR-WK = ZEROS
                        DELETE WORK
                        END-DELETE
                     END-IF
                END-READ
           END-PERFORM.

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.


       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           CLOSE WORK.  OPEN I-O WORK.
           MOVE ZEROS TO SALARIO-FUNC-TOT SALARIO-REPORT-TOT
                         SALARIO-REPRES-TOT SALARIO-VEND-TOT
                         TOT-CREDITO-TOT SALDO-ABERTO-TOT
                         SALDO-CTACORR-TOT SALDO-APAGAR-TOT
                         VALOR-APAGAR-TOT CTACORR-LIQUIDO.

           INITIALIZE REG-WORK
                      NOME-WK
                      CODIGO-WK
                      SALDO-CTACORR-WK

           MOVE ZEROS  TO AUX-CLASSIF
                          SALDO-ABERTO
                          SALDO-CONTA

           EVALUATE GS-ORDENACAO
               WHEN 1 START WORK KEY IS NOT < CHAVE-VALOR INVALID KEY
                            MOVE "10" TO ST-WORK
                      END-START
               WHEN 2 START WORK KEY IS NOT < CHAVE-WK    INVALID KEY
                            MOVE "10" TO ST-WORK
                      END-START
               WHEN 3 START WORK KEY IS NOT < CHAVE-NOME  INVALID KEY
                            MOVE "10" TO ST-WORK
                      END-START
           END-EVALUATE

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT WITH IGNORE LOCK AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   IF CLASSIF-WK <> AUX-CLASSIF
                      IF AUX-CLASSIF <> ZEROS
                         MOVE ALL "="        TO GS-LINDET
                         MOVE "INSERE-LIST"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM

                         MOVE SPACES         TO GS-LINDET
                         MOVE SALDO-CONTA    TO VALOR-E1
                         MOVE VALOR-E1       TO GS-LINDET(42:12)

                         MOVE SALDO-ABERTO   TO VALOR-E1
                         MOVE VALOR-E1       TO GS-LINDET(56:12)

                         COMPUTE VALOR-E1 = SALDO-CONTA - SALDO-ABERTO
                         MOVE VALOR-E1       TO GS-LINDET(69:12)

                         MOVE "INSERE-LIST"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM

                         MOVE SPACES         TO GS-LINDET
                         MOVE "INSERE-LIST"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF

                      EVALUATE CLASSIF-WK
                          WHEN 1   MOVE "FRANQUIA"      TO GS-LINDET
                          WHEN 2   MOVE "TERCEIRIZADO"  TO GS-LINDET
                          WHEN 3   MOVE "OUTRO"         TO GS-LINDET
                          WHEN 4   MOVE "PENDENCIAS"    TO GS-LINDET
                          WHEN 5   MOVE "IMPOSTO"       TO GS-LINDET
                          WHEN 6   MOVE "VENDEDOR"      TO GS-LINDET
                          WHEN 7   MOVE "CINEGRAFISTA"  TO GS-LINDET
                          WHEN 8   MOVE "FOTOGRAFO"     TO GS-LINDET
                          WHEN 9   MOVE "REPRESENTANTE" TO GS-LINDET
                          WHEN 10  MOVE "FUNCIONARIO"   TO GS-LINDET
                          WHEN 11  MOVE "JURIDICA"      TO GS-LINDET
                          WHEN 12  MOVE "FISICA"        TO GS-LINDET
                      END-EVALUATE
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM


                      MOVE ZEROS             TO SALDO-ABERTO
                      MOVE ZEROS             TO SALDO-CONTA
                      MOVE CLASSIF-WK        TO AUX-CLASSIF
                   END-IF
                   MOVE SPACES               TO GS-LINDET
                   MOVE SELECAO-WK           TO GS-LINDET(01:01)
                   MOVE CODIGO-WK            TO GS-LINDET(03:06)
                   MOVE NOME-WK              TO GS-LINDET(10:26)
                   MOVE COD-RED-WK           TO GS-LINDET(37:05)

                   IF MOSTRAR-ERRO = "S"
                      IF COD-RED-WK = ZEROS
                         MOVE SPACES TO MENSAGEM
                         STRING NOME-WK x"0da0"
                             "Não possui Código Reduzido" INTO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM 140-EXIBIR-MENSAGEM
                      END-IF
                   END-IF

                   MOVE SALDO-CTACORR-WK     TO VALOR-E1
                   IF SELECAO-WK = "X"
                      ADD SALDO-CTACORR-WK   TO SALDO-CTACORR-TOT
                      ADD SALDO-CTACORR-WK   TO SALDO-CONTA
                   END-IF
                   MOVE VALOR-E1             TO GS-LINDET(42:12)
                   MOVE SALDO-ABERTO-WK      TO VALOR-E1
                   IF SELECAO-WK = "X"
                      ADD  SALDO-ABERTO-WK   TO SALDO-ABERTO-TOT
                      ADD  SALDO-ABERTO-WK   TO SALDO-ABERTO
                   END-IF
                   MOVE VALOR-E1             TO GS-LINDET(56:12)

                   COMPUTE VALOR-E1 = SALDO-CTACORR-WK -
                                      SALDO-ABERTO-WK
                   MOVE VALOR-E1         TO GS-LINDET(69:12)

                   MOVE FORMA-PAGTO-WK   TO GS-LINDET(82:50)

                   MOVE SEQ-WK           TO GS-LINDET(132: 05)

                   MOVE CLASSIF-WK       TO GS-LINDET(138: 02)

                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM

           IF SALDO-ABERTO <> 0 OR SALDO-CONTA <> 0
              MOVE ALL "="        TO GS-LINDET
              MOVE "INSERE-LIST"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM

              MOVE SPACES         TO GS-LINDET
              MOVE SALDO-CONTA    TO VALOR-E1
              MOVE VALOR-E1       TO GS-LINDET(42:12)

              MOVE SALDO-ABERTO   TO VALOR-E1
              MOVE VALOR-E1       TO GS-LINDET(56:12)

              COMPUTE VALOR-E1 = SALDO-CONTA - SALDO-ABERTO
              MOVE VALOR-E1       TO GS-LINDET(69:12)

              MOVE "INSERE-LIST"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM TOTALIZA.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE "TOTAL..."        TO GS-LINTOT(1: 10)

           MOVE SALDO-CTACORR-TOT TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINTOT(42:12)
           MOVE SALDO-ABERTO-TOT  TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINTOT(56:12)

           COMPUTE VALOR-E1 = SALDO-CTACORR-TOT - SALDO-ABERTO-TOT
           MOVE VALOR-E1         TO GS-LINTOT(69:12)

           MOVE "INSERE-TOTAL"    TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP208" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.


           MOVE ZEROS TO LIN.

           INITIALIZE REG-WORK
                      SALDO-CONTA
                      SALDO-ABERTO
                      AUX-CLASSIF

           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < CHAVE-NOME INVALID KEY
                MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT WITH IGNORE LOCK AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   IF SELECAO-WK = "X"
                      IF CLASSIF-WK <> AUX-CLASSIF
                         IF AUX-CLASSIF <> ZEROS
                            MOVE SPACES           TO LINDET-REL
                            MOVE "TOTAL..."       TO LINDET-REL(1: 10)

                            COMPUTE VALOR-E1 = SALDO-CONTA -
                                               SALDO-ABERTO
                            MOVE VALOR-E1         TO LINDET-REL(44:12)

                            WRITE REG-RELAT FROM LINDET-REL
                            MOVE SPACES           TO REG-RELAT
                            WRITE REG-RELAT
                            ADD 1                 TO LIN
                         END-IF
                         MOVE ZEROS               TO SALDO-CONTA
                                                     SALDO-ABERTO
                         MOVE CLASSIF-WK          TO AUX-CLASSIF
                         MOVE ZEROS               TO PAG-W
                         PERFORM CABECALHO
                      END-IF

                      ADD SALDO-CTACORR-WK        TO SALDO-CONTA
                      ADD SALDO-ABERTO-WK         TO SALDO-ABERTO

                      MOVE SPACES TO LINDET-REL
                      MOVE CODIGO-WK              TO LINDET-REL(01:06)
                      MOVE NOME-WK                TO LINDET-REL(08:29)
                      MOVE COD-RED-WK             TO LINDET-REL(37:05)
                      COMPUTE VALOR-E1 = SALDO-CTACORR-WK -
                                         SALDO-ABERTO-WK
                      MOVE VALOR-E1               TO LINDET-REL(45:12)

                      MOVE FORMA-PAGTO-WK         TO LINDET-REL(58:48)
                      MOVE TITULAR-WK             TO LINDET-REL(106:30)

                      WRITE REG-RELAT FROM LINDET
                      ADD 1                       TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                  END-IF
              END-READ
           END-PERFORM
           PERFORM TOTALIZA-REL

           COPY DESCONDENSA.


       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINDET-REL.
           MOVE "TOTAL..."        TO LINDET-REL(1: 10)

      *    MOVE SALDO-CTACORR-TOT TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(47:12)
      *    MOVE SALDO-ABERTO-TOT  TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(61:12)

           COMPUTE VALOR-E1 = SALDO-CTACORR-TOT - SALDO-ABERTO-TOT
      *    MOVE VALOR-E1         TO LINDET-REL(74:12)
           MOVE VALOR-E1         TO LINDET-REL(45:12)

           WRITE REG-RELAT FROM LINDET-REL.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
           ADD 1 TO LIN.

       CABECALHO SECTION.
           MOVE SPACES TO CARACTERISTICA-REL

           EVALUATE CLASSIF-WK
               WHEN 1   MOVE "FRANQUIA"      TO CARACTERISTICA-REL
               WHEN 2   MOVE "TERCEIRIZADO"  TO CARACTERISTICA-REL
               WHEN 3   MOVE "OUTRO"         TO CARACTERISTICA-REL
               WHEN 4   MOVE "PENDENCIAS"    TO CARACTERISTICA-REL
               WHEN 5   MOVE "IMPOSTO"       TO CARACTERISTICA-REL
               WHEN 6   MOVE "VENDEDOR"      TO CARACTERISTICA-REL
               WHEN 7   MOVE "CINEGRAFISTA"  TO CARACTERISTICA-REL
               WHEN 8   MOVE "FOTOGRAFO"     TO CARACTERISTICA-REL
               WHEN 9   MOVE "REPRESENTANTE" TO CARACTERISTICA-REL
               WHEN 10  MOVE "FUNCIONARIO"   TO CARACTERISTICA-REL
               WHEN 11  MOVE "JURIDICA"      TO CARACTERISTICA-REL
               WHEN 12  MOVE "FISICA"        TO CARACTERISTICA-REL
           END-EVALUATE

           MOVE GS-VENCTO TO VENCTO-REL.
           ADD 1          TO LIN PAG-W.
           MOVE PAG-W     TO PG-REL.

           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02
           WRITE REG-RELAT FROM CAB021 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 7 TO LIN.

       140-EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.


       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CCD010 CCD100 CCD105 CCD110 CCD115 CCD120 WORK CGD006
                 CPD020 CPD021 CPD022 CPD024 LOG001 CAD030 CXD020
                 CRD020 CRD021 CCD101.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
