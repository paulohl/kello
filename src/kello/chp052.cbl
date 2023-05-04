       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP052.
      *DATA: 10/06/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Extrato de Cliente individual
      *        Listar todos os cheques do cliente
      *        dentro do intervalo de vencimento solicitado.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CAPX018.
           COPY CGPX010.
           COPY CHPX010.
           COPY CHPX010B.
           COPY CHPX013.
           COPY CRPX001.
           COPY CRPX200.
           COPY CRPX201.
           COPY MTPX019.
           COPY RCPX100.
           COPY CBPX001.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = DATA-MOVTO-WK SEQ-WK
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DESCR-SITUACAO-SIST-WK
                        WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DOCUMENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PORTADOR-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       COPY CAPW004.
       COPY CAPW018.
       COPY CHPW010.
       COPY CHPW010B.
       COPY CHPW013.
       COPY CRPW001.
       COPY CRPW200.
       COPY CRPW201.
       COPY MTPW019.
       COPY RCPW100.
       COPY CBPW001.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  COD-COMPL-WK.
               10  CLASSIF-WK      PIC 9.
               10  CLIENTE-WK      PIC 9(8).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  SEQ-WK              PIC 9(4).
           05  DOCUMENTO-WK        PIC X(10).
           05  PORTADOR-WK         PIC X(10).
           05  CARTEIRA-WK         PIC X(4).
           05  SITUACAO-WK         PIC X(10).
           05  SITUACAO-SIST-WK    PIC 9.
           05  DESCR-SITUACAO-SIST-WK PIC X(15).
           05  VENCTO-WK           PIC 9(8).
           05  VALOR-WK            PIC 9(8)V99.
           05  DATA-BAIXA-DEVOLV-WK PIC 9(8).
           05  VALOR-SALDO-WK      PIC 9(8)V99.
           05  JUROS-DEVOLV-WK     PIC 9(8)V99.
           05  ALINEA-WK           PIC 9(02).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CHP052.CPB".
           COPY "CHP052.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD010B            PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  AUX-SEQUENCIA         PIC 9(03)    VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ACHEI                 PIC X(1)     VALUE SPACES.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZZ.ZZZ,ZZ- BLANK WHEN ZEROS.
           05  VALOR-E2              PIC ZZZ.ZZZ,ZZ   BLANK WHEN ZEROS.
           05  TAXA-E                PIC ZZ,Z     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  VALOR-ACUM            PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-W            PIC 9(8)     VALUE ZEROS.
           05  TOTAL-ACUM            PIC 9(8)V99  VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  TOT-TITULO            PIC 9(3)     VALUE ZEROS.
           05  TOT-VALOR             PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VALOR-REC         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ATRAS-MEDIO       PIC 9(3)V99  VALUE ZEROS.
           05  TOT-VALOR-A-RECEB     PIC 9(8)V99  VALUE ZEROS.
           05  TOT-JUROS-A-RECEB     PIC 9(8)V99  VALUE ZEROS.
           05  ATRASO-MEDIO-E        PIC ZZZ,ZZ.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  COD-COMPL-W           PIC 9(9)     VALUE ZEROS.
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  SEQ-CAIXA             PIC 9(3)     VALUE ZEROS.
      *    VARIAVEIS P/ CALCULAR JUROS A RECEBER SE EM ATRASO
           05  DIAS-ATRASO          PIC 99999     VALUE ZEROS.
           05  MESES-ATRASO         PIC 9999      VALUE ZEROS.
           05  TAXA-W               PIC 9(3)V9(6) VALUE ZEROS.
           05  JUROS-DIARIO         PIC 9(6)V9(4) VALUE ZEROS.
           05  JUROS-ARECEBER       PIC 9(8)V99   VALUE ZEROS.
           05  DIAS-RESTANTE        PIC 9(2)      VALUE ZEROS.
           05  I                    PIC 9999      VALUE ZEROS.
           05  ALINEA-E             PIC ZZ        BLANK WHEN ZEROS.
           05  LIN                  PIC 9(02)     VALUE ZEROS.
           05  DATA-AUX             PIC 9(08)     VALUE ZEROS.
           05  FORMA-PAGTO-AUX      PIC X(10)     VALUE SPACES.
           05  DCR-AUX              PIC X(15)     VALUE SPACES.
           05  TOTAL-JUROS-MULTA    PIC 9(08)v99  VALUE ZEROS.
           05  JUROS                PIC 9(06)V99  VALUE ZEROS.
           05  MULTA                PIC 9(06)V99  VALUE ZEROS.
           05  DESCONTO             PIC 9(06)V99  VALUE ZEROS.
           05  VLR-BAIXA            PIC 9(06)V99  VALUE ZEROS.
           05  SEM-CHD010B          PIC X(01)     VALUE SPACES.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 TOT-BAIXA                 PIC 9(08)V99.

       01  CAB01.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "EXTRATO DE CHEQUE          -ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB02A.
           05  FILLER              PIC X(09)   VALUE "CLIENTE: ".
           05  CLASSIF-REL         PIC 9       VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  CLIENTE-REL         PIC ZZZZ.ZZZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-CLIENTE-REL    PIC X(30)   VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-VECTO NR-CHEQUE  PORTADOR   CART SITUACAO            VA
      -    "LOR SIT-CHEQUE     SALDO-CHEQ    JUROS-RCTO AL".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(100)  VALUE
           "QTDE CHEQUES       VALOR-TOTAL        VALOR RECEBIDOS     VA
      -    "LOR A RECEBER  JUROS-A-RECEB".
       01  LINTOT.
           05  LINTOT-REL          PIC X(100)  VALUE SPACES.
       01  CAB06.
           05  FILLER              PIC X(110)  VALUE
           "CONTATOS EFETUADOS                      ".

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU         PIC 9(04).
             10 WS-MES-CPU         PIC 9(02).
             10 WS-DIA-CPU         PIC 9(02).
          05 FILLER                PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       01 DET-01.
          05 FILLER                    PIC X(10)
             VALUE "DATA RECTO".
          05 FILLER                    PIC X(01).
          05 FILLER                    PIC X(13)
             VALUE "  VLR. TITULO".
          05 FILLER                    PIC X(01).
          05 FILLER                    PIC X(13)
             VALUE "   VLR. BAIXA".
          05 FILLER                    PIC X(01).
          05 FILLER                    PIC X(13)
             VALUE "  VALOR SALDO".
          05 FILLER                    PIC X(01).
          05 FILLER                    PIC X(13)
             VALUE "  VALOR JUROS".
          05 FILLER                    PIC X(01).
          05 FILLER                    PIC X(13)
             VALUE "  VALOR MULTA".
          05 FILLER                    PIC X(01).
          05 FILLER                    PIC X(13)
             VALUE "VLR. DESCONTO".
          05 FILLER                    PIC X(01).
          05 FILLER                    PIC X(13)
             VALUE " VLR. LIQUIDO".
          05 FILLER                    PIC X(01).
          05 FILLER                    PIC X(11)
             VALUE "FORMA PAGTO".
          05 FILLER                    PIC X(01).
          05 FILLER                    PIC X(15)
             VALUE "DCR/MEM".
          05 FILLER                    PIC X(01).
          05 FILLER                    PIC X(21)
             VALUE "IDENT. BAIXA".
          05 FILLER                    PIC X(16)
             VALUE "RECEBEDOR".
          05 FILLER                    PIC X(38)
             VALUE "CONTA CORRENTE".
          05 FILLER                    PIC X(10)
             VALUE "NEGOCIACAO".



       01 DET-01S.
          05 FILLER                    PIC X(222)
             VALUE ALL "-".

       01 DET-CHD010B.
          05 DET-DATA-RCTO             PIC 99/99/9999.
          05 FILLER                    PIC X(01).
          05 DET-VALOR-TITULO          PIC ZZ.ZZZ.ZZ9,99.
          05 FILLER                    PIC X(01).
          05 DET-VALOR-BAIXA           PIC ZZ.ZZZ.ZZ9,99.
          05 FILLER                    PIC X(01).
          05 DET-VALOR-SALDO           PIC ZZ.ZZZ.ZZ9,99.
          05 FILLER                    PIC X(01).
          05 DET-JUROS                 PIC ZZ.ZZZ.ZZ9,99.
          05 FILLER                    PIC X(01).
          05 DET-MULTA                 PIC ZZ.ZZZ.ZZ9,99.
          05 FILLER                    PIC X(01).
          05 DET-DESCONTO              PIC ZZ.ZZZ.ZZ9,99.
          05 FILLER                    PIC X(01).
          05 DET-VLRLIQUIDO            PIC ZZ.ZZZ.ZZ9,99.
          05 FILLER                    PIC X(01).
          05 DET-FORMA-PAGTO-D         PIC X(10).
          05 FILLER                    PIC X(02).
          05 DET-DCR-MEM-R             PIC X(15).
          05 FILLER                    PIC X(01).
          05 DET-CHAVE-BAIXA           PIC X(20).
          05 FILLER                    PIC X(01).
          05 DET-RECEBEDOR             PIC X(15).
          05 FILLER                    PIC X(01).
          05 DET-CONTA-CORRENTE        PIC 9(06).
          05 FILLER                    PIC X(01).
          05 DET-DESC-CONTA            PIC X(30).
          05 FILLER                    PIC X(01).
          05 DET-NEGOCIACAO            PIC 99/99/9999.

       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(65).

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
           MOVE DATA-INV TO DATA-MOVTO-I
                            DATA-DIA-W.


           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CRD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD001.
           MOVE "CHD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD010B" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010B.
           MOVE "CHD013"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD200"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "CBD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CBD001.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           ACCEPT VARIA-W FROM TIME.

           OPEN INPUT CRD001 CAD018 CGD010 CHD013 CAD004 MTD019 RCD100
                      CBD001

           OPEN I-O   CHD010 CHD010B
           CLOSE             CHD010B
           OPEN I-O          CHD010B

           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010B <> "00"
              MOVE "ERRO ABERTURA CHD010B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CHP052"            to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

           CLOSE      CHD010 CHD010B
           OPEN INPUT CHD010 CHD010B.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-PRINTER-FLG-AGEN-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIMIR-CONTATO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-CLIENTE-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-CLIENTE-TRUE
                   PERFORM LE-CLIENTE
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM CHAMA-ALTERACAO
               WHEN GS-GRAVA-ANOTACAO-TRUE
                    PERFORM GRAVA-ANOTACAO
               WHEN GS-LISTA-ANOTACAO-TRUE
                    PERFORM CARREGA-LISTA-ANOTACAO
               WHEN GS-MARCAR-DESMARCAR-TRUE
                    PERFORM MARCAR-DESMARCAR
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-TRANSFERIR-PORTADOR-TRUE
                    PERFORM TRANSFERIR-PORTADOR
               WHEN GS-ITEM-ABAIXAR-TRUE
                    PERFORM ITEM-ABAIXAR
               WHEN GS-VERIFICAR-VLBAIXA-TRUE
                    PERFORM VERIFICAR-VLBAIXA
               WHEN GS-VERIFICAR-DATA-TRUE
                    PERFORM VERIFICAR-DATA
               WHEN GS-REGRAVA-DADOS-TRUE
                    PERFORM REGRAVA-DADOS
               WHEN GS-CALCULA-VLR-LIQ-TRUE
                    PERFORM CALCULA-VLR-LIQUIDO
               WHEN GS-VERIFICA-SITUACAO-TRUE
                    PERFORM VERIFICA-SITUACAO
               WHEN GS-REVERTE-TRUE
                    PERFORM REVERTER-SITUACAO
               WHEN GS-REVERTE-DEVOLUCAO-TRUE
                    PERFORM REVERTE-DEVOLUCAO
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-CONTA-TRUE
                    PERFORM LER-CONTA
               WHEN GS-POPUP-CONTA-TRUE
                    PERFORM POPUP-CONTA
               WHEN GS-RECALCULAR-SALDO-TRUE
                    PERFORM RECALCULAR-SALDO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       RECALCULAR-SALDO SECTION.
           MOVE SPACES TO MENSAGEM
           STRING "Deseja realmente recalcular o saldo do documento?"
             INTO MENSAGEM
             MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM

           IF RESP-MSG = "S"
              CLOSE      CHD010
              OPEN I-O   CHD010


              MOVE "S"   TO SEM-CHD010B

              MOVE ZEROS TO TOT-BAIXA
      *                     JURO-RCTO-CH10
      *                     MULTA-RCTO-CR20
      *                     DESCONTO-CR20
      *                     DATA-RCTO-CR20
      *                     VALOR-LIQ-CR20

              INITIALIZE REG-CHD010B
              MOVE DATA-MOVTO-CH10 TO DATA-MOVTO-CH10B
              MOVE SEQ-CH10        TO SEQ-CH10B
              START CHD010B KEY IS NOT LESS CHAVE-CH10B INVALID KEY
                   MOVE "10" TO ST-CHD010B
              END-START

              PERFORM UNTIL ST-CHD010B = "10"
                   READ CHD010B NEXT AT END
                        MOVE "10" TO ST-CHD010B
                   NOT AT END
                        IF DATA-MOVTO-CH10 <> DATA-MOVTO-CH10B OR
                           SEQ-CH10        <> SEQ-CH10B
                           MOVE "10" TO ST-CHD010B
                        ELSE
                           ADD  VALOR-BAIXA-CH10B TO TOT-BAIXA
      *                    ADD  JURO-RCTO-CH10B   TO JURO-RCTO-CR20
      *                    ADD  MULTA-RCTO-CH10B  TO MULTA-RCTO-CR20
      *                    ADD  DESCONTO-CH10B    TO DESCONTO-CR20
      *                    MOVE DATA-RCTO-CH10B   TO DATA-RCTO-CR20
      *                    ADD  VALOR-LIQ-CH10B   TO VALOR-LIQ-CH10
                           MOVE "N" TO SEM-CHD010B
                        END-IF
                   END-READ
              END-PERFORM

              IF VALOR-SALDO-CH10 > VALOR-CH10
                 MOVE VALOR-CH10 TO VALOR-SALDO-CH10
                 IF SITUACAO-CH10 < 3
                    MOVE 0           TO SITUACAO-CH10
                 END-IF
                 REWRITE REG-CHD010 NOT INVALID KEY
                     MOVE "Registro atualizado com sucesso" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                 END-REWRITE
              END-IF
              IF SEM-CHD010B = "S"
                 MOVE VALOR-CH10     TO VALOR-SALDO-CH10
                 IF SITUACAO-CH10 < 3
                    MOVE 0           TO SITUACAO-CH10
      *                                 JURO-RCTO-CR20
      *                                 MULTA-RCTO-CR20
      *                                 DESCONTO-CR20
      *                                 DATA-RCTO-CR20
      *                                 VALOR-LIQ-CR20
                 END-IF
                 REWRITE REG-CHD010 NOT INVALID KEY
                     MOVE "Registro atualizado com sucesso" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                 END-REWRITE
              ELSE
                 COMPUTE VALOR-SALDO-CH10 = VALOR-CH10 -
                                            TOT-BAIXA
                 IF TOT-BAIXA = VALOR-CH10
                    MOVE 2 TO SITUACAO-CH10
                 ELSE
                    MOVE 1 TO SITUACAO-CH10
                 END-IF
                 REWRITE REG-CHD010 NOT INVALID KEY
                     MOVE "Registro atualizado com sucesso" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                 END-REWRITE
              END-IF

              CLOSE      CHD010
              OPEN INPUT CHD010

              UNSHOW-WINDOW WIN6 PRINCIPAL
              PERFORM GRAVA-WORK
              PERFORM CARREGA-LISTA.

       LER-CONTA SECTION.
           IF GS-CONTA-CORRENTE > 0
              MOVE GS-CONTA-CORRENTE TO CODIGO-FORN-CB01
              READ CBD001 INVALID KEY
                   MOVE "Conta corrente não identificada" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   MOVE TITULAR-CB01 TO GS-DESC-CONTA
                   REFRESH-OBJECT WIN6
              END-READ
           ELSE
              MOVE SPACES TO GS-DESC-CONTA
              REFRESH-OBJECT WIN6.

       POPUP-CONTA SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING
           CANCEL "CBP001T"
           MOVE PASSAR-STRING(49: 6) TO GS-CONTA-CORRENTE
           PERFORM LER-CONTA.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       REVERTE-DEVOLUCAO SECTION.
           CLOSE      CHD010 CHD013
           OPEN I-O   CHD010 CHD013 WORK

           IF GS-LINDET = SPACES
              MOVE ZEROS TO GS-LINDET.

           MOVE GS-LINDET(116: 8) TO DATA-MOVTO-CH10
                                     DATA-MOVTO-WK
           MOVE GS-LINDET(111: 4) TO SEQ-CH10
                                     SEQ-WK
           READ CHD010 INVALID KEY
                MOVE "Cheque Não Encontrado" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                IF SITUACAO-CH10 = 5
                   MOVE "Deseja Reverter o Cheque?" TO MENSAGEM
                   MOVE "Q" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
                   IF RESP-MSG = "S"
                      MOVE DATA-MOVTO-CH10  TO DATA-MOVTO-CH13
                      MOVE SEQ-CH10         TO SEQ-CH13
                      READ CHD013 INVALID KEY
                           MOVE "Dados do Cheque DEVOLVIDO Não Encontrad
      -                         "o" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           DELETE CHD013 INVALID KEY
                               MOVE "Erro de Exclusão...CHD013" TO
                                           MENSAGEM
                               MOVE "C" TO TIPO-MSG
                               PERFORM EXIBIR-MENSAGEM
                           END-DELETE
                           MOVE 0   TO SITUACAO-CH10
                           REWRITE REG-CHD010 INVALID KEY
                               MOVE "Erro de Regravação...CHD010" TO
                               MENSAGEM
                               MOVE "C" TO TIPO-MSG
                               PERFORM EXIBIR-MENSAGEM
                           END-REWRITE
                      END-READ
                      READ WORK INVALID KEY
                           MOVE "Work Não Encontrada" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           MOVE SITUACAO-CH10 TO SITUACAO-SIST-WK
                           MOVE ZEROS         TO JUROS-DEVOLV-WK
                           MOVE ZEROS         TO ALINEA-WK
                           MOVE ZEROS         TO DATA-BAIXA-DEVOLV-WK

                           EVALUATE SITUACAO-CH10
                             WHEN 0 MOVE "OK"
                                      TO DESCR-SITUACAO-SIST-WK
                             WHEN 1 MOVE "PARCIAL"
                                      TO DESCR-SITUACAO-SIST-WK
                             WHEN 2 MOVE "RECEBIDO"
                                      TO DESCR-SITUACAO-SIST-WK
                             WHEN 3 MOVE "ESTORNADO"
                                      TO DESCR-SITUACAO-SIST-WK
                             WHEN 4 MOVE "CANCELADO"
                                      TO DESCR-SITUACAO-SIST-WK
                             WHEN 5 MOVE "DEVOLVIDO"
                                      TO DESCR-SITUACAO-SIST-WK
                             WHEN 6 MOVE "PROBLEMATICO"
                                      TO DESCR-SITUACAO-SIST-WK
                           END-EVALUATE

                           REWRITE REG-WORK INVALID KEY
                                MOVE "Erro de Regravação..WORK" TO
                                MENSAGEM
                                MOVE "C" TO TIPO-MSG
                                PERFORM EXIBIR-MENSAGEM
                           END-REWRITE
                      END-READ
                   END-IF
                ELSE
                   MOVE "Cheque Diferente de DEVOLVIDO" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
                END-IF
           END-READ


           CLOSE      CHD010 CHD013 WORK
           OPEN INPUT CHD010 CHD013.

       REVERTER-SITUACAO SECTION.
           CLOSE    CHD010 CHD010B CHD013
           OPEN I-O CHD010 CHD010B CHD013 WORK

           DELETE CHD010B INVALID KEY
               MOVE "Erro de Exclusão...CHD010B" TO MENSAGEM
               MOVE "C"                          TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           END-DELETE

           INITIALIZE REG-CHD010B
                      JUROS
                      MULTA
                      DESCONTO
                      VLR-BAIXA
           MOVE DATA-MOVTO-CH10  TO DATA-MOVTO-CH10B
           MOVE SEQ-CH10         TO SEQ-CH10B
           START CHD010B KEY IS NOT LESS CHAVE-CH10B INVALID
                                                         KEY
                 MOVE "10" TO ST-CHD010B
           END-START
           PERFORM UNTIL ST-CHD010B = "10"
                 READ CHD010B NEXT AT END
                      MOVE "10" TO ST-CHD010B
                 NOT AT END
                      IF DATA-MOVTO-CH10 <> DATA-MOVTO-CH10B
                         OR SEQ-CH10     <> SEQ-CH10B
                         MOVE "10" TO ST-CHD010B
                      ELSE
                         ADD JURO-RCTO-CH10B  TO JUROS
                         ADD MULTA-RCTO-CH10B TO MULTA
                         ADD DESCONTO-CH10B   TO DESCONTO
                         ADD VALOR-BAIXA-CH10B TO VLR-BAIXA
                      END-IF
                 END-READ
           END-PERFORM

           PERFORM ATUALIZAR-CHD013

           COMPUTE VALOR-SALDO-CH10 = VALOR-CH10
                                    - VLR-BAIXA

           IF VALOR-SALDO-CH10 = 0
              MOVE 2 TO SITUACAO-CH10
           ELSE
              MOVE 1 TO SITUACAO-CH10
           END-IF

           IF VALOR-SALDO-CH10 = VALOR-CH10
              MOVE 5 TO SITUACAO-CH10
              MOVE 0 TO VLR-LIQUIDO-CH13
              MOVE 0 TO VLR-DESCONTO-CH13
              MOVE 0 TO VLR-MULTA-CH13
              MOVE 0 TO VLR-JUROS-CH13
              MOVE 0 TO ALINEA-CH13
              MOVE 0 TO DATA-ENTRADA-CH13
              MOVE 0 TO DATA-COMPRA-CH13
              MOVE 0 TO DATA-APRES-CH13
              MOVE 0 TO DATA-REAPRES-CH13
           ELSE
              MOVE 1 TO SITUACAO-CH10
           END-IF

           REWRITE REG-CHD010

           MOVE DATA-MOVTO-CH10    TO DATA-MOVTO-WK
           MOVE SEQ-CH10           TO SEQ-WK
           READ WORK INVALID KEY
                CONTINUE
           NOT INVALID KEY
                MOVE DATA-RECTO-CH13   TO DATA-BAIXA-DEVOLV-WK
                MOVE TOTAL-JUROS-MULTA TO JUROS-DEVOLV-WK

                EVALUATE SITUACAO-CH10
                  WHEN 0 MOVE "OK"
                           TO DESCR-SITUACAO-SIST-WK
                  WHEN 1 MOVE "PARCIAL"
                           TO DESCR-SITUACAO-SIST-WK
                  WHEN 2 MOVE "RECEBIDO"
                           TO DESCR-SITUACAO-SIST-WK
                  WHEN 3 MOVE "ESTORNADO"
                           TO DESCR-SITUACAO-SIST-WK
                  WHEN 4 MOVE "CANCELADO"
                           TO DESCR-SITUACAO-SIST-WK
                  WHEN 5 MOVE "DEVOLVIDO"
                           TO DESCR-SITUACAO-SIST-WK
                  WHEN 6 MOVE "PROBLEMATICO"
                           TO DESCR-SITUACAO-SIST-WK
                END-EVALUATE
      *         SITUACAO = 0-OK        1-PARCIAL      2-RECEBIDO
      *                    3-ESTONADO  4-CANCELADO
      *                    5-DEVOLVIDO 6-PROBLEMATICO
                MOVE SITUACAO-TIT-CR01   TO SITUACAO-WK

                REWRITE REG-WORK
                END-REWRITE
           END-READ.
           CLOSE      CHD010 CHD010B CHD013 WORK
           OPEN INPUT CHD010 CHD010B CHD013
           PERFORM CARREGA-LISTA.

       VERIFICA-SITUACAO SECTION.
           MOVE 0                      TO GS-SITUACAO
           MOVE GS-LINHA-BAIXA         TO DET-CHD010B
           MOVE DET-CHAVE-BAIXA(1:8)   TO DATA-MOVTO-CH10B
           MOVE DET-CHAVE-BAIXA(9:4)   TO SEQ-CH10B
           MOVE DET-CHAVE-BAIXA(13:8)  TO DATA-RCTO-CH10B
           READ CHD010B INVALID KEY
               MOVE "Baixa do Cheque Não Encontrado" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE DATA-MOVTO-CH10B  TO DATA-MOVTO-CH10
               MOVE SEQ-CH10B         TO SEQ-CH10
               READ CHD010 INVALID KEY
                    MOVE "Cheque Não Encontrado" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               NOT INVALID KEY
                    MOVE DATA-MOVTO-CH10  TO DATA-MOVTO-CH13
                    MOVE SEQ-CH10         TO SEQ-CH10
                    READ CHD013 INVALID KEY
                         INITIALIZE REG-CHD013
                    END-READ
                    MOVE SITUACAO-CH10 TO GS-SITUACAO.

       VERIFICAR-DATA SECTION.
           IF GS-DATA-RCTO = 0
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO GS-DATA-RCTO
           END-IF
           MOVE DATA-MOVTO-CH10            TO DATA-MOVTO-CH10B
           MOVE SEQ-CH10                   TO SEQ-CH10B
           MOVE GS-DATA-RCTO               TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                   TO DATA-RCTO-CH10B
           READ CHD010B NOT INVALID KEY
                MOVE "Data de Recebimento já Informada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.

       VERIFICAR-VLBAIXA SECTION.
           IF GS-VALOR-BAIXA = 0
              MOVE GS-VALOR-SALDO    TO GS-VALOR-BAIXA
           ELSE
              IF GS-VALOR-BAIXA > GS-VALOR-SALDO
                 MOVE "Valor da Baixa Maior que o Valor do Saldo" TO
                 MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM.

       CALCULA-VLR-LIQUIDO SECTION.
           COMPUTE GS-VALOR-LIQUIDO = (GS-VALOR-BAIXA + GS-JUROS +
                                       GS-MULTA) - GS-DESCONTO.
       REGRAVA-DADOS SECTION.
           CLOSE    CHD010 CHD010B CHD013
           OPEN I-O CHD010 CHD010B CHD013 WORK

           MOVE DATA-MOVTO-CH10            TO DATA-MOVTO-CH10B
           MOVE SEQ-CH10                   TO SEQ-CH10B
           MOVE GS-JUROS                   TO JURO-RCTO-CH10B
           MOVE GS-MULTA                   TO MULTA-RCTO-CH10B
           MOVE GS-DESCONTO                TO DESCONTO-CH10B
           MOVE GS-DATA-RCTO               TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                   TO DATA-RCTO-CH10B
           MOVE GS-DCR-MEM-R               TO DCR-MEM-CH10B
           MOVE GS-FORMA-PAGTO-D           TO FORMA-PAGTO-CH10B
           MOVE GS-RECEBEDOR               TO RECEBEDOR-CH10B
           MOVE SEQ-CAIXA-CH10             TO SEQ-CAIXA-CH10B
           MOVE GS-VALOR-BAIXA             TO VALOR-BAIXA-CH10B
           MOVE GS-VALOR-LIQUIDO           TO VALOR-LIQ-CH10B
           MOVE GS-VALOR-TITULO            TO VALOR-TOT-CH10B
           MOVE GS-CONTA-CORRENTE          TO CODIGO-BANCARIO-CH10B
           MOVE GS-DATA-NEGOCIACAO         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                   TO DATA-NEGOCIACAO-CH10B

           WRITE REG-CHD010B INVALID KEY
                MOVE "Erro de Gravação...CHD010B" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE DATA-MOVTO-CH10     TO DATA-MOVTO-WK
                MOVE SEQ-CH10            TO SEQ-WK
                READ WORK INVALID KEY
                     MOVE "Work Não Encontrada" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                NOT INVALID KEY
                     INITIALIZE REG-CHD010B
                                JUROS
                                MULTA
                                DESCONTO
                                VLR-BAIXA
                     MOVE DATA-MOVTO-CH10  TO DATA-MOVTO-CH10B
                     MOVE SEQ-CH10         TO SEQ-CH10B
                     START CHD010B KEY IS NOT LESS CHAVE-CH10B INVALID
                                                                   KEY
                           MOVE "10" TO ST-CHD010B
                     END-START
                     PERFORM UNTIL ST-CHD010B = "10"
                           READ CHD010B NEXT AT END
                                MOVE "10" TO ST-CHD010B
                           NOT AT END
                                IF DATA-MOVTO-CH10 <> DATA-MOVTO-CH10B
                                   OR SEQ-CH10     <> SEQ-CH10B
                                   MOVE "10" TO ST-CHD010B
                                ELSE
                                   ADD JURO-RCTO-CH10B  TO JUROS
                                   ADD MULTA-RCTO-CH10B TO MULTA
                                   ADD DESCONTO-CH10B   TO DESCONTO
                                   ADD VALOR-BAIXA-CH10B TO VLR-BAIXA
                                END-IF
                           END-READ
                     END-PERFORM

                     PERFORM ATUALIZAR-CHD013

                     COMPUTE VALOR-SALDO-CH10 = VALOR-CH10
                                              - VLR-BAIXA

                     IF VALOR-SALDO-CH10 = 0
                        MOVE 2 TO SITUACAO-CH10
                     ELSE
                        MOVE 1 TO SITUACAO-CH10
                     END-IF

                     REWRITE REG-CHD010 INVALID KEY
                         MOVE "Erro de Regravação...CHD010" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                     END-REWRITE

                     EVALUATE SITUACAO-CH10
                       WHEN 0 MOVE "OK"
                                TO DESCR-SITUACAO-SIST-WK
                       WHEN 1 MOVE "PARCIAL"
                                TO DESCR-SITUACAO-SIST-WK
                       WHEN 2 MOVE "RECEBIDO"
                                TO DESCR-SITUACAO-SIST-WK
                       WHEN 3 MOVE "ESTORNADO"
                                TO DESCR-SITUACAO-SIST-WK
                       WHEN 4 MOVE "CANCELADO"
                                TO DESCR-SITUACAO-SIST-WK
                       WHEN 5 MOVE "DEVOLVIDO"
                                TO DESCR-SITUACAO-SIST-WK
                       WHEN 6 MOVE "PROBLEMATICO"
                                TO DESCR-SITUACAO-SIST-WK
                     END-EVALUATE
      *              SITUACAO = 0-OK        1-PARCIAL      2-RECEBIDO
      *                         3-ESTONADO  4-CANCELADO
      *                         5-DEVOLVIDO 6-PROBLEMATICO
                     MOVE SITUACAO-TIT-CR01   TO SITUACAO-WK

                     MOVE DATA-RECTO-CH13 TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV        TO DATA-BAIXA-DEVOLV-WK

                     MOVE GS-JUROS        TO VLR-JUROS-CH13
                     MOVE GS-MULTA        TO VLR-MULTA-CH13
                     MOVE GS-DESCONTO     TO VLR-DESCONTO-CH13

                     COMPUTE JUROS-DEVOLV-WK = VLR-JUROS-CH13 +
                                               VLR-MULTA-CH13
                     REWRITE REG-WORK
                     PERFORM MOVER-DADOS-LINDET
                     MOVE "ATUALIZA-ITEM" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM

                END-READ
           END-WRITE

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           ACCEPT WS-HORA-SYS FROM TIME

           MOVE SPACES TO GS-ANOTACAO
           STRING "BAIXA MANUAL EFETUADA POR " USUARIO-W " EM "
                   WS-DIA-CPU "/" WS-MES-CPU "/" WS-ANO-CPU " AS "
                   WS-HO-SYS ":" WS-MI-SYS ":" WS-SE-SYS
             INTO GS-ANOTACAO
           REFRESH-OBJECT WIN5

           CLOSE      CHD010 CHD010B CHD013 WORK
           OPEN INPUT CHD010 CHD010B CHD013

           PERFORM CARREGA-LISTA.

       ATUALIZAR-CHD013 SECTION.
           INITIALIZE REG-CHD010B
                      VLR-JUROS-CH13
                      VLR-MULTA-CH13
                      VLR-DESCONTO-CH13
                      VLR-LIQUIDO-CH13

           MOVE DATA-MOVTO-CH10  TO DATA-MOVTO-CH13
           MOVE SEQ-CH10         TO SEQ-CH13
           READ CHD013 INVALID KEY
                INITIALIZE ALINEA-CH13
                           DATA-ENTRADA-CH13
                           DATA-COMPRA-CH13
                           DATA-APRES-CH13
                           DATA-REAPRES-CH13
                           DATA-RECTO-CH13
                           VLR-JUROS-CH13
                           VLR-MULTA-CH13
                           VLR-DESCONTO-CH13
                           FORMA-PAGTO-CH13
                           DCR-MEM-CH13
                           DCR-MEM-R-CH13
                           VLR-LIQUIDO-CH13
                           RECEBEDOR-CH13

                WRITE REG-CHD013 INVALID KEY
                    MOVE "Erro de GRAVACAO...CHD013" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-WRITE
           NOT INVALID KEY
                INITIALIZE ALINEA-CH13
                           DATA-ENTRADA-CH13
                           DATA-COMPRA-CH13
                           DATA-APRES-CH13
                           DATA-REAPRES-CH13
                           DATA-RECTO-CH13
                           VLR-JUROS-CH13
                           VLR-MULTA-CH13
                           VLR-DESCONTO-CH13
                           FORMA-PAGTO-CH13
                           DCR-MEM-CH13
                           DCR-MEM-R-CH13
                           VLR-LIQUIDO-CH13
                           RECEBEDOR-CH13

                REWRITE REG-CHD013 INVALID KEY
                    MOVE "Erro de REGRAVACAO...CHD013" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ

           MOVE DATA-MOVTO-CH10    TO DATA-MOVTO-CH10B
           MOVE SEQ-CH10           TO SEQ-CH10B
           START CHD010B KEY IS NOT LESS CHAVE-CH10B INVALID KEY
                 MOVE "10" TO ST-CHD010B.

           PERFORM UNTIL ST-CHD010B = "10"
                 READ CHD010B NEXT AT END
                      MOVE "10" TO ST-CHD010B
                 NOT AT END
                      IF DATA-MOVTO-CH10 <> DATA-MOVTO-CH10B OR
                         SEQ-CH10        <> SEQ-CH10B
                         MOVE "10" TO ST-CHD010B
                      ELSE
                         ADD  JUROS            TO VLR-JUROS-CH13
                         ADD  MULTA            TO VLR-MULTA-CH13
                         ADD  DESCONTO         TO VLR-DESCONTO-CH13
                         ADD  VLR-BAIXA        TO VLR-LIQUIDO-CH13
                         MOVE GS-FORMA-PAGTO-D TO FORMA-PAGTO-CH13
                         MOVE GS-RECEBEDOR     TO RECEBEDOR-CH13
                         MOVE GS-DCR-MEM-R     TO DCR-MEM-R-CH13
                         MOVE GS-DATA-RCTO     TO DATA-INV
                         CALL "GRIDAT2" USING DATA-INV
                         MOVE DATA-INV         TO DATA-RECTO-CH13

                         REWRITE REG-CHD013 INVALID KEY
                             MOVE "Erro de Regravação...CHD013" TO
                             MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                         END-REWRITE

                      END-IF
                 END-READ
           END-PERFORM.
       ATUALIZAR-CHD013-FIM.
           EXIT.

       ITEM-ABAIXAR SECTION.
           MOVE GS-LINDET(116: 8) TO DATA-MOVTO-CH10
           MOVE GS-LINDET(111: 4) TO SEQ-CH10
           READ CHD010 INVALID KEY
                INITIALIZE REG-CHD010.

           MOVE "LIMPAR-LB-BAIXA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-01             TO GS-LINHA-BAIXA
           MOVE "INSERIR-LB-BAIXA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-01S            TO GS-LINHA-BAIXA
           MOVE "INSERIR-LB-BAIXA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-CHD010B
                      TOT-BAIXA

           MOVE DATA-MOVTO-CH10        TO DATA-MOVTO-CH10B
           MOVE SEQ-CH10               TO SEQ-CH10B

           START CHD010B KEY IS NOT LESS CHAVE-CH10B INVALID KEY
                 MOVE "10" TO ST-CHD010B.

           PERFORM UNTIL ST-CHD010B = "10"
                 READ CHD010B NEXT AT END
                      MOVE "10" TO ST-CHD010B
                 NOT AT END
                      IF DATA-MOVTO-CH10    <> DATA-MOVTO-CH10B    OR
                         SEQ-CH10           <> SEQ-CH10B
                         MOVE "10" TO ST-CHD010B
                      ELSE
                         MOVE DATA-RCTO-CH10B    TO DATA-INV
                         CALL "GRIDAT1" USING DATA-INV
                         MOVE DATA-INV           TO DET-DATA-RCTO
                         MOVE VALOR-TOT-CH10B    TO DET-VALOR-TITULO
                         MOVE VALOR-BAIXA-CH10B  TO DET-VALOR-BAIXA
                         ADD  VALOR-BAIXA-CH10B  TO TOT-BAIXA

                         COMPUTE DET-VALOR-SALDO = VALOR-CH10 -
                                                   TOT-BAIXA

                         MOVE JURO-RCTO-CH10B    TO DET-JUROS
                         MOVE MULTA-RCTO-CH10B   TO DET-MULTA
                         MOVE DESCONTO-CH10B     TO DET-DESCONTO
                         MOVE VALOR-LIQ-CH10B    TO DET-VLRLIQUIDO
                         MOVE FORMA-PAGTO-CH10B  TO DET-FORMA-PAGTO-D
                         MOVE RECEBEDOR-CH10B    TO DET-RECEBEDOR
                         MOVE DCR-MEM-CH10B      TO DET-DCR-MEM-R
                         IF DATA-NEGOCIACAO-CH10B IS NOT NUMERIC
                            MOVE 0 TO DATA-NEGOCIACAO-CH10B
                         END-IF

                         IF DATA-NEGOCIACAO-CH10B > 0
                            MOVE DATA-NEGOCIACAO-CH10B TO DATA-INV
                            CALL "GRIDAT1" USING DATA-INV
                         ELSE
                            MOVE 0                     TO DATA-INV
                         END-IF
                         MOVE DATA-INV           TO DET-NEGOCIACAO


                         IF CODIGO-BANCARIO-CH10B IS NOT NUMERIC
                            MOVE 0 TO CODIGO-BANCARIO-CH10B
                         END-IF

                         MOVE CODIGO-BANCARIO-CH10B TO
                              DET-CONTA-CORRENTE
                              CODIGO-FORN-CB01

                         READ CBD001 INVALID KEY
                              INITIALIZE REG-CBD001
                         END-READ
                         MOVE TITULAR-CB01       TO DET-DESC-CONTA

                         STRING DATA-MOVTO-CH10 SEQ-CH10 DATA-RCTO-CH10B
                           INTO DET-CHAVE-BAIXA

                         MOVE DET-CHD010B        TO GS-LINHA-BAIXA
                         MOVE "INSERIR-LB-BAIXA" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM

      *    MOVE DATA-RCTO-CR20    TO GS-DATA-RCTO
           MOVE ZEROS             TO GS-DATA-RCTO
           MOVE VALOR-CH10        TO GS-VALOR-TITULO
           MOVE VALOR-SALDO-CH10  TO GS-VALOR-SALDO
           MOVE VALOR-SALDO-CH10  TO GS-VALOR-BAIXA
           MOVE ZEROS             TO GS-JUROS
           MOVE ZEROS             TO GS-MULTA
           MOVE ZEROS             TO GS-DESCONTO
           MOVE ZEROS             TO GS-VALOR-LIQUIDO
           MOVE SPACES            TO GS-FORMA-PAGTO-D
           MOVE USUARIO-W         TO GS-RECEBEDOR
           MOVE SPACES            TO GS-DCR-MEM-R
           MOVE ZEROS             TO GS-DATA-NEGOCIACAO
           MOVE "REFRESH-WIN3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR TO PORTADOR
           READ CAD018 INVALID KEY
               MOVE "Portador Inválido" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
               MOVE ZEROS     TO GS-PORTADOR
               MOVE SPACES    TO GS-DESC-PORTADOR
           NOT INVALID KEY
               MOVE NOME-PORT TO GS-DESC-PORTADOR.

       POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-PORTADOR.
           MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR.
           PERFORM LER-PORTADOR.

       TRANSFERIR-PORTADOR SECTION.
           CLOSE    CHD010
           OPEN I-O CRD200 CRD201 CHD010

           MOVE 1 TO GS-CONT
           MOVE SPACES TO GS-LINDET
           MOVE "LER-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
                IF GS-LINDET(109:1) = "B"
                   PERFORM TRANSFERIR-CHEQUE
                END-IF
                ADD 1 TO GS-CONT
                MOVE SPACES TO GS-LINDET
                MOVE "LER-LB" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE CRD200 CRD201 CHD010
           OPEN INPUT CHD010.

       TRANSFERIR-CHEQUE SECTION.

           MOVE GS-LINDET(116: 8)      TO DATA-MOVTO-CH10.
           MOVE GS-LINDET(111: 04)     TO SEQ-CH10

           READ CHD010 INVALID KEY
               MOVE "Cheque Selecionado Não Encontrado" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               PERFORM GRAVAR-ANOTACOES
               MOVE GS-PORTADOR        TO PORTADOR-CH10
               REWRITE REG-CHD010 INVALID KEY
                   MOVE "Erro de Regravação...CHD010" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               END-REWRITE
           END-READ.

       GRAVAR-ANOTACOES SECTION.
           INITIALIZE REG-CRD200
           MOVE GS-CLASS(1: 1) TO COD-COMPL-W(1: 1).
           MOVE GS-CLIENTE TO COD-COMPL-W(2: 8)
           MOVE COD-COMPL-W TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
               NOT AT END
                 IF COD-COMPL-CR200 <> COD-COMPL-W
                              MOVE "10" TO ST-CRD200
                 ELSE MOVE SEQ-CR200 TO ULT-SEQ
                      CONTINUE
             END-READ
           END-PERFORM.
           ADD 1 TO ULT-SEQ.
           COPY "CBDATA1.CPY".
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           MOVE ULT-SEQ        TO SEQ-CR200.
           MOVE COD-COMPL-W    TO COD-COMPL-CR200.
           MOVE ZEROS          TO DATA-RETORNO-CR200
           MOVE USUARIO-W      TO USUARIO-CR200
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE WS-DATA-CPU    TO DATA-MOVTO-I
           MOVE DATA-MOVTO-I   TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200
      *    ATUALIZA A LISTA DE ANOTACAO

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200      TO SEQ-CR201.
           MOVE COD-COMPL-W    TO COD-COMPL-CR201.
           MOVE ZEROS          TO SUBSEQ-CR201.

           MOVE PORTADOR-CH10  TO PORTADOR
           READ CAD018 INVALID KEY
               MOVE "******"   TO NOME-PORT
           END-READ


           MOVE SPACES             TO GS-ANOTACAO
      *    STRING "TRANSF.PORTADOR-CHEQUE: " NR-CHEQUE-CH10 "    - "
      *    PORTADOR-CH10 "-" NOME-PORT " P/ " GS-PORTADOR "-"
      *               GS-DESC-PORTADOR INTO GS-ANOTACAO
           STRING "ALTERACAO PORTADOR ATUAL: "
           PORTADOR-CH10 "-" NOME-PORT " P/ " GS-PORTADOR "-"
                      GS-DESC-PORTADOR INTO GS-ANOTACAO

           PERFORM VARYING COL1 FROM 1 BY 80 UNTIL COL1 > 640
              MOVE GS-ANOTACAO(COL1: 80) TO ANOTACAO-W
              MOVE ANOTACAO-W TO ANOTACAO-CR201
              IF ANOTACAO-W <> SPACES
                 ADD 1 TO SUBSEQ-CR201
                 WRITE REG-CRD201 INVALID KEY
                       ADD 1 TO SUBSEQ-CR201
                       WRITE REG-CRD201
                       END-WRITE
                 END-WRITE
              END-IF
           END-PERFORM.

       MARCAR-DESMARCAR SECTION.
           MOVE "SENHA19" TO PROGRAMA-CA004.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
                MOVE "Usuário Sem Permissão" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               IF GS-LINDET(109:1) = "B"
                  MOVE SPACES TO GS-LINDET(109:1)
               ELSE
                  MOVE "B" TO GS-LINDET(109:1).

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.

       CHAMAR-POPUP SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(33: 8) TO GS-CLIENTE
                                          COD-COMPL-CG10(2: 8).
           MOVE PASSAR-STRING-1(42: 1) TO CLASSIF-W GS-CLASS(1: 1)
                                          COD-COMPL-CG10(1: 1).
           EVALUATE CLASSIF-W
              WHEN 0 MOVE "0-Contrato"       TO GS-CLASS
              WHEN 1 MOVE "1-Comum   "       TO GS-CLASS
              WHEN 9 MOVE "9-Unificado"      TO GS-CLASS
           END-EVALUATE.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-CLIENTE.
           MOVE GS-CLIENTE TO ALBUMMT19
           READ MTD019 INVALID KEY
               MOVE "NÃO ENCONTRADO"  TO GS-DESC-FORMANDO
           NOT INVALID KEY
               MOVE NOME-FORM-MT19    TO GS-DESC-FORMANDO.

           MOVE GS-CLIENTE            TO ALBUM-REC
           READ RCD100 INVALID KEY
               MOVE "RECIBO NÃO DIGITADO/NÃO VENDIDO" TO GS-DESC-STATUS
           NOT INVALID KEY
               MOVE SPACES TO GS-DESC-STATUS.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-CLIENTE SECTION.
           MOVE GS-CLASS(1: 1) TO CLASSIF-CG10.
           MOVE GS-CLIENTE     TO CODIGO-CG10.
           READ CGD010 INVALID KEY MOVE "****" TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10     TO GS-NOME-CLIENTE.
           MOVE GS-CLIENTE TO ALBUMMT19
           READ MTD019 INVALID KEY
               MOVE "NÃO ENCONTRADO"  TO GS-DESC-FORMANDO
           NOT INVALID KEY
               MOVE NOME-FORM-MT19    TO GS-DESC-FORMANDO.
           MOVE GS-CLIENTE            TO ALBUM-REC
           READ RCD100 INVALID KEY
               MOVE "RECIBO NÃO DIGITADO/NÃO VENDIDO" TO GS-DESC-STATUS
           NOT INVALID KEY
               MOVE SPACES TO GS-DESC-STATUS.
       GRAVA-ANOTACAO SECTION.
           OPEN I-O CRD200 CRD201.
           IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
                                CLOSE CRD200  OPEN I-O CRD200.
           IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
                                CLOSE CRD201  OPEN I-O CRD201.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE GS-CLASS(1: 1) TO COD-COMPL-W(1: 1).
           MOVE GS-CLIENTE TO COD-COMPL-W(2: 8)
           MOVE COD-COMPL-W TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
               NOT AT END
                 IF COD-COMPL-CR200 <> COD-COMPL-W
                              MOVE "10" TO ST-CRD200
                 ELSE MOVE SEQ-CR200 TO ULT-SEQ
                      CONTINUE
             END-READ
           END-PERFORM.
           ADD 1 TO ULT-SEQ.
           COPY "CBDATA1.CPY".
           MOVE ZEROS        TO SITUACAO-ANOTACAO-CR200
           MOVE ULT-SEQ      TO SEQ-CR200.
           MOVE COD-COMPL-W  TO COD-COMPL-CR200.
           MOVE GS-DATA-AGENDADA TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE WS-DATA-CPU           TO DATA-MOVTO-I
           MOVE DATA-MOVTO-I          TO DATA-MOVTO-CR200

           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200
      *    ATUALIZA A LISTA DE ANOTACAO
           MOVE SPACES           TO GS-LINDET1
           MOVE DATA-MOVTO-CR200 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO GS-LINDET1(1: 15)
           MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
           MOVE ":"                    TO HORA-E(3: 1)
           MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
           MOVE HORA-E           TO GS-LINDET1(16: 10)
           MOVE USUARIO-CR200    TO GS-LINDET1(26: 11)
           MOVE SEQ-CR200        TO GS-LINDET1(36: 10)
           MOVE "DATA AGENDADA: " TO GS-LINDET1(50: 15)
           MOVE DATA-RETORNO-CR200 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET1(65: 10)
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200      TO SEQ-CR201.
           MOVE COD-COMPL-W    TO COD-COMPL-CR201.
           MOVE ZEROS          TO SUBSEQ-CR201.

           PERFORM VARYING COL1 FROM 1 BY 80 UNTIL COL1 > 640
              MOVE GS-ANOTACAO(COL1: 80) TO ANOTACAO-W
              MOVE ANOTACAO-W TO ANOTACAO-CR201
              IF ANOTACAO-W <> SPACES
                 ADD 1 TO SUBSEQ-CR201
                 WRITE REG-CRD201 INVALID KEY
                       ADD 1 TO SUBSEQ-CR201
                       WRITE REG-CRD201
                       END-WRITE
                 END-WRITE
                 MOVE SPACES            TO GS-LINDET1
                 MOVE ANOTACAO-W        TO GS-LINDET1(16: 80)
                 MOVE "INSERE-LIST1"    TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-IF
           END-PERFORM.
           MOVE SPACES TO GS-LINDET1.
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE CRD200 CRD201.
       GRAVA-WORK SECTION.
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE ZEROS TO TOT-VALOR TOT-VALOR-REC TOT-VALOR-A-RECEB
                         TOTAL-ACUM TOT-TITULO TOT-JUROS-A-RECEB.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-ANT
                                     VECTO-INI-REL.

           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-ANT
                                     VECTO-FIM-REL.

           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-FIM.
           MOVE GS-CLASS(1: 1) TO CLASS-CLIENTE-CH10 CLASSIF-W
           MOVE GS-CLIENTE     TO CLIENTE-CH10.
           MOVE VECTO-INI         TO DATA-VENCTO-CH10.

           START CHD010 KEY IS NOT < ALT-CH4 INVALID KEY
                  MOVE "10" TO ST-CHD010.

           PERFORM UNTIL ST-CHD010 = "10"
             READ CHD010 NEXT RECORD AT END
                  MOVE "10" TO ST-CHD010
             NOT AT END
                  IF CLIENTE-CH10 NOT = GS-CLIENTE OR
                     CLASS-CLIENTE-CH10 NOT = CLASSIF-W
                     MOVE "10" TO ST-CHD010
                  ELSE
                     IF DATA-VENCTO-CH10 > VECTO-FIM
                        MOVE "10" TO ST-CHD010
                     ELSE
                        MOVE DATA-VENCTO-CH10    TO GS-EXIBE-VECTO
                        MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                        PERFORM MOVER-DADOS-WORK
                        WRITE REG-WORK
                     END-IF
                 END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE WORK.

       MOVER-DADOS-WORK SECTION.
           MOVE CLASS-CLIENTE-CH10      TO CLASSIF-WK
           MOVE CLIENTE-CH10            TO CLIENTE-WK
           MOVE DATA-MOVTO-CH10         TO DATA-MOVTO-WK.
           MOVE SEQ-CH10                TO SEQ-WK
           EVALUATE CARTEIRA-CH10
              WHEN 1 MOVE "SIMP"        TO CARTEIRA-WK
              WHEN 2 MOVE "CAUC"        TO CARTEIRA-WK
              WHEN 3 MOVE "DESC"        TO CARTEIRA-WK
           END-EVALUATE

           MOVE NR-CHEQUE-CH10          TO DOCUMENTO-WK
           MOVE PORTADOR-CH10           TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*****"            TO NOME-PORT
           END-READ
           MOVE NOME-PORT               TO PORTADOR-WK
           MOVE SITUACAO-TIT-CH10       TO CODIGO-CR01
           READ CRD001 INVALID KEY
                MOVE "******"           TO SITUACAO-TIT-CR01
           END-READ
           MOVE SITUACAO-CH10           TO SITUACAO-SIST-WK
           EVALUATE SITUACAO-CH10
             WHEN 0 MOVE "OK"           TO DESCR-SITUACAO-SIST-WK
             WHEN 1 MOVE "PARCIAL"      TO DESCR-SITUACAO-SIST-WK
             WHEN 2 MOVE "RECEBIDO"     TO DESCR-SITUACAO-SIST-WK
             WHEN 3 MOVE "ESTORNADO"    TO DESCR-SITUACAO-SIST-WK
             WHEN 4 MOVE "CANCELADO"    TO DESCR-SITUACAO-SIST-WK
             WHEN 5 MOVE "DEVOLVIDO"    TO DESCR-SITUACAO-SIST-WK
             WHEN 6 MOVE "PROBLEMATICO" TO DESCR-SITUACAO-SIST-WK
           END-EVALUATE.
      *    SITUACAO = 0-OK  2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           MOVE SITUACAO-TIT-CR01       TO SITUACAO-WK
           MOVE VALOR-CH10              TO VALOR-WK
           MOVE VALOR-SALDO-CH10        TO VALOR-SALDO-WK
           MOVE DATA-VENCTO-CH10        TO VENCTO-WK
           ADD 1                        TO TOT-TITULO
           MOVE ZEROS TO JUROS-DEVOLV-WK DATA-BAIXA-DEVOLV-WK.
           MOVE DATA-MOVTO-CH10         TO DATA-MOVTO-CH13.
           MOVE SEQ-CH10                TO SEQ-CH13.
           READ CHD013 INVALID KEY
                INITIALIZE REG-CHD013.

           INITIALIZE REG-CHD010B
                      JUROS
                      MULTA
                      DESCONTO
           MOVE DATA-MOVTO-CH10  TO DATA-MOVTO-CH10B
           MOVE SEQ-CH10         TO SEQ-CH10B
           START CHD010B KEY IS NOT LESS CHAVE-CH10B INVALID
                                                         KEY
                 MOVE "10" TO ST-CHD010B
           END-START
           PERFORM UNTIL ST-CHD010B = "10"
                 READ CHD010B NEXT AT END
                      MOVE "10" TO ST-CHD010B
                 NOT AT END
                      IF DATA-MOVTO-CH10 <> DATA-MOVTO-CH10B
                         OR SEQ-CH10     <> SEQ-CH10B
                         MOVE "10" TO ST-CHD010B
                      ELSE
                         ADD JURO-RCTO-CH10B  TO JUROS
                         ADD MULTA-RCTO-CH10B TO MULTA
                         ADD DESCONTO-CH10B   TO DESCONTO
                      END-IF
                 END-READ
           END-PERFORM

           IF JUROS    <> VLR-JUROS-CH13     OR
              MULTA    <> VLR-MULTA-CH13     OR
              DESCONTO <> VLR-DESCONTO-CH13

              CLOSE      CHD013
              OPEN I-O   CHD013

              MOVE JUROS     TO VLR-JUROS-CH13
              MOVE MULTA     TO VLR-MULTA-CH13
              MOVE DESCONTO  TO VLR-DESCONTO-CH13

              WRITE REG-CHD013 INVALID KEY
                    REWRITE REG-CHD013 INVALID KEY
                        MOVE "Erro de Regravação...CHD013" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                    END-REWRITE
              END-WRITE

              CLOSE      CHD013
              OPEN INPUT CHD013

           END-IF

           MOVE ALINEA-CH13             TO ALINEA-WK
           IF SITUACAO-CH10 = 2
              ADD VALOR-CH10            TO TOT-VALOR-REC
           ELSE
              ADD VALOR-SALDO-CH10      TO TOT-VALOR-A-RECEB
              IF SITUACAO-CH10 = 1
                 COMPUTE TOT-VALOR-REC = TOT-VALOR-REC +
                                     (VALOR-CH10 - VALOR-SALDO-CH10).

           ADD VALOR-CH10           TO TOT-VALOR
           IF SITUACAO-CH10 = 5
              IF DATA-RECTO-CH13 = ZEROS
                 PERFORM CALCULA-DIAS-ATRASO
                 MOVE ZEROS   TO DATA-BAIXA-DEVOLV-WK
                 ADD JUROS-DEVOLV-WK TO TOT-JUROS-A-RECEB.

           IF SITUACAO-CH10 = 2 OR 1
              IF DATA-RECTO-CH13 <> ZEROS
                 MOVE DATA-RECTO-CH13 TO DATA-INV
                 CALL "GRIDAT1" USING DATA-INV

                 MOVE DATA-INV        TO DATA-BAIXA-DEVOLV-WK
                 COMPUTE JUROS-DEVOLV-WK = VLR-JUROS-CH13 +
                                           VLR-MULTA-CH13.

       CALCULA-JUROS-ARECEBER SECTION.
           COMPUTE MESES-ATRASO = DIAS-ATRASO / 30
           COMPUTE TAXA-W = (GS-TAXA / 100) + 1
           MOVE VALOR-WK TO JUROS-ARECEBER
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MESES-ATRASO
             COMPUTE JUROS-ARECEBER = JUROS-ARECEBER * TAXA-W
           END-PERFORM.
           COMPUTE JUROS-DIARIO = ((TAXA-W * JUROS-ARECEBER) -
                                  JUROS-ARECEBER) / 30
           COMPUTE DIAS-RESTANTE = DIAS-ATRASO - (MESES-ATRASO * 30)
           COMPUTE JUROS-ARECEBER = (JUROS-DIARIO * DIAS-RESTANTE) +
                                     JUROS-ARECEBER
           COMPUTE JUROS-DEVOLV-WK = JUROS-ARECEBER - VALOR-WK.
       CALCULA-DIAS-ATRASO SECTION.
           MOVE DATA-MOVTO-I           TO GRTIME-DATE-FINAL.
           MOVE DATA-VENCTO-CH10       TO GRTIME-DATE.
           MOVE 2                      TO GRTIME-TYPE.
           MOVE 3                      TO GRTIME-FUNCTION.
           IF GRTIME-DATE-FINAL > GRTIME-DATE
              CALL "GRTIME" USING PARAMETROS-GRTIME
              MOVE GRTIME-DAYS-FINAL     TO DIAS-ATRASO
              COMPUTE VALOR-ACUM = DIAS-ATRASO * VALOR-CH10
              PERFORM CALCULA-JUROS-ARECEBER
           ELSE
              MOVE ZEROS TO DIAS-ATRASO.

       CARREGA-LISTA-ANOTACAO SECTION.
           OPEN INPUT CRD200 CRD201.
           IF ST-CRD200 = "35"
              CLOSE CRD200       OPEN OUTPUT CRD200  CLOSE CRD200
              OPEN INPUT CRD200.
           IF ST-CRD201 = "35"
              CLOSE CRD201       OPEN OUTPUT CRD201  CLOSE CRD201
              OPEN INPUT CRD201.
           MOVE COD-COMPL-CG10   TO COD-COMPL-CR200.
           MOVE ZEROS            TO SEQ-CR200
                                    DATA-MOVTO-CR200
                                    HORA-MOVTO-CR200
           START CRD200 KEY IS NOT < ALT1-CR200 INVALID KEY
               MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
                NOT AT END
                  MOVE SPACES TO GS-LINDET1
                  IF COD-COMPL-CR200 <> COD-COMPL-CG10
                     MOVE "10" TO ST-CRD200
                  ELSE
                    MOVE DATA-MOVTO-CR200 TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV         TO DATA-E
                    MOVE DATA-E           TO GS-LINDET1(1: 15)
                    MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
                    MOVE ":"                    TO HORA-E(3: 1)
                    MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
                    MOVE HORA-E           TO GS-LINDET1(16: 10)
                    MOVE USUARIO-CR200    TO GS-LINDET1(26: 11)
                    MOVE SEQ-CR200        TO GS-LINDET1(36: 10)
                    MOVE "DATA AGENDADA: " TO GS-LINDET1(50: 15)
                    MOVE DATA-RETORNO-CR200 TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV           TO DATA-E
                    MOVE DATA-E             TO GS-LINDET1(65: 10)
                    MOVE "INSERE-LIST1" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                    PERFORM CARREGA-CRD201
              END-READ
           END-PERFORM.
           CLOSE CRD200 CRD201.
       CARREGA-CRD201 SECTION.
           MOVE COD-COMPL-CG10   TO COD-COMPL-CR201.
           MOVE SEQ-CR200        TO SEQ-CR201.
           MOVE ZEROS            TO SUBSEQ-CR201.
           START CRD201 KEY IS NOT < CHAVE-CR201 INVALID KEY
                 MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
              READ CRD201 NEXT RECORD AT END MOVE "10" TO ST-CRD201
                   NOT AT END
                     IF COD-COMPL-CR201 <> COD-COMPL-CG10 OR
                        SEQ-CR201 <> SEQ-CR200
                          MOVE "10" TO ST-CRD201
                     ELSE
                        MOVE SPACES TO GS-LINDET1
                        MOVE ANOTACAO-CR201 TO GS-LINDET1(16: 80)
                        MOVE "INSERE-LIST1" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-LINDET1.
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-LISTA SECTION.
           OPEN INPUT WORK
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA

           CLOSE WORK.

       MOVER-DADOS-LINDET SECTION.
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(01: 11)
           MOVE DOCUMENTO-WK      TO GS-LINDET(12: 11)
           MOVE PORTADOR-WK       TO GS-LINDET(23: 11)
           MOVE CARTEIRA-WK       TO GS-LINDET(34: 5)
           MOVE SITUACAO-WK       TO GS-LINDET(39: 15)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(52: 14)
           MOVE DESCR-SITUACAO-SIST-WK TO GS-LINDET(66: 11).
      *    MOVE DATA-BAIXA-DEVOLV-WK TO DATA-E
      *    MOVE DATA-E            TO GS-LINDET(81: 11)
           MOVE VALOR-SALDO-WK    TO VALOR-E2
           MOVE VALOR-E2          TO GS-LINDET(81: 11)
           MOVE JUROS-DEVOLV-WK   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(92: 13)
           MOVE ALINEA-WK         TO ALINEA-E
           MOVE ALINEA-E          TO GS-LINDET(106: 2)
           MOVE SEQ-WK            TO GS-LINDET(111: 04).
           MOVE DATA-MOVTO-WK     TO GS-LINDET(116: 8).
       CHAMA-ALTERACAO SECTION.
           OPEN I-O WORK
           IF GS-LINDET = SPACES MOVE ZEROS TO GS-LINDET.
           MOVE GS-LINDET(116: 8) TO PASSAR-STRING-1(1: 8)
                                     DATA-MOVTO-CH10 DATA-MOVTO-WK
           MOVE GS-LINDET(111: 4) TO PASSAR-STRING-1(10: 4) SEQ-CH10
                                     SEQ-WK.
           move usuario-w         to passar-string-1(20: 5)
           MOVE COD-USUARIO-W     TO COD-USUARIO-CA004
                                     PASSAR-STRING-1(26: 3)
           MOVE "CHP010"          TO PROGRAMA-CA004.

           CALL   "CHP010A" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CHP010A".

      *    READ CAD004 INVALID KEY
      *        CALL "CHP010B" USING PASSAR-STRING-1
      *        CANCEL "CHP010B"
      *      NOT INVALID KEY
      *        CALL "CHP010A" USING PASSAR-STRING-1
      *        CANCEL "CHP010A".

           READ CHD010 INVALID KEY
                      INITIALIZE REG-CHD010
                      MOVE 3 TO SITUACAO-SIST-WK
                      MOVE "ESTORNADO" TO DESCR-SITUACAO-SIST-WK.

           READ WORK.
           PERFORM MOVER-DADOS-WORK.
           REWRITE REG-WORK.

           PERFORM MOVER-DADOS-LINDET.

           CLOSE WORK
           MOVE "ATUALIZA-ITEM" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "VENCTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK
                START WORK KEY IS NOT < VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "DOCUMENTO " TO GS-DESCR-ORDEM
                MOVE ZEROS TO DOCUMENTO-WK
                START WORK KEY IS NOT < DOCUMENTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "PORTADOR" TO GS-DESCR-ORDEM
                MOVE SPACES TO PORTADOR-WK
                START WORK KEY IS NOT < PORTADOR-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "SIT-CHEQUE" TO GS-DESCR-ORDEM
                MOVE ZEROS TO DESCR-SITUACAO-SIST-WK
                START WORK KEY IS NOT < DESCR-SITUACAO-SIST-WK
                      INVALID KEY MOVE "10" TO ST-WORK
           END-EVALUATE.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE TOT-TITULO        TO GS-LINTOT(01: 14)
           MOVE TOT-VALOR         TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(16: 14)
           MOVE TOT-VALOR-REC     TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(39: 14)
      *    DIVIDE TOTAL-ACUM BY TOT-VALOR GIVING TOT-ATRAS-MEDIO
      *    MOVE TOT-ATRAS-MEDIO   TO ATRASO-MEDIO-E
      *    MOVE ATRASO-MEDIO-E    TO GS-LINTOT(54: 10)
           MOVE TOT-VALOR-A-RECEB TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(59: 16)
           MOVE TOT-JUROS-A-RECEB TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(75: 13)
           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CHP052" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN INPUT WORK
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           INITIALIZE TOT-TITULO
                      TOT-VALOR
                      TOT-VALOR-REC
                      TOT-VALOR-A-RECEB
                      TOT-JUROS-A-RECEB


           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   ADD 1              TO TOT-TITULO
                   ADD VALOR-WK       TO TOT-VALOR
                   ADD VALOR-SALDO-WK TO TOT-VALOR-A-RECEB

                   IF SITUACAO-WK = 2
                      ADD VALOR-WK  TO TOT-VALOR-REC
                   ELSE
                      IF SITUACAO-WK = 1
                         COMPUTE TOT-VALOR-REC = TOT-VALOR-REC +
                                     (VALOR-WK - VALOR-SALDO-WK)
                      END-IF
                   END-IF

                   ADD JUROS-DEVOLV-WK TO TOT-JUROS-A-RECEB

                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           COPY DESCONDENSA.

           CLOSE WORK.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01: 11)
           MOVE DOCUMENTO-WK      TO LINDET-REL(12: 11)
           MOVE PORTADOR-WK       TO LINDET-REL(23: 05)
           MOVE CARTEIRA-WK       TO LINDET-REL(34: 5)
           MOVE SITUACAO-WK       TO LINDET-REL(39: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(52: 12)
           MOVE DESCR-SITUACAO-SIST-WK TO LINDET-REL(66: 11)
           MOVE VALOR-SALDO-WK    TO VALOR-E2
           MOVE VALOR-E2          TO LINDET-REL(81:11)
      *    MOVE DATA-BAIXA-DEVOLV-WK TO DATA-E
      *    MOVE DATA-E            TO LINDET-REL(81: 11)
           MOVE JUROS-DEVOLV-WK   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(92: 13)
           MOVE ALINEA-WK         TO ALINEA-E
           MOVE ALINEA-E          TO LINDET-REL(106: 2)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE TOT-TITULO        TO LINTOT-REL(01: 14)
           MOVE TOT-VALOR         TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(16: 14)
           MOVE TOT-VALOR-REC     TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(39: 14)
      *    DIVIDE TOTAL-ACUM BY TOT-VALOR GIVING TOT-ATRAS-MEDIO
      *    MOVE TOT-ATRAS-MEDIO   TO ATRASO-MEDIO-E
      *    MOVE ATRASO-MEDIO-E    TO LINTOT-REL(51: 10)
           MOVE TOT-VALOR-A-RECEB TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(59: 16)
           MOVE TOT-JUROS-A-RECEB TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(75: 13)
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           MOVE GS-CLASS(1: 1) TO CLASSIF-REL.
           MOVE GS-CLIENTE     TO CLIENTE-REL
           MOVE GS-NOME-CLIENTE TO NOME-CLIENTE-REL.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
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
      * imprimir dados agenda
       IMPRIMIR-CONTATO SECTION.
           MOVE ZEROS TO PAG-W LIN.

           COPY CONDENSA.

           OPEN INPUT CRD200 CRD201.
           PERFORM CABECALHO-AGENDA.
           MOVE COD-COMPL-WK      TO COD-COMPL-CR200.
           MOVE ZEROS             TO SEQ-CR200
           DATA-MOVTO-CR200 HORA-MOVTO-CR200
           START CRD200 KEY IS NOT < ALT1-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END
                  MOVE "10" TO ST-CRD200
             NOT AT END
                 IF COD-COMPL-CR200 <> COD-COMPL-WK
                    MOVE "10" TO ST-CRD200
                 ELSE
                    MOVE SPACES    TO LINDET-REL
                    MOVE SEQ-CR200 TO SEQ-CR201 LINDET-REL(1: 5)
                    MOVE COD-COMPL-CR200 TO COD-COMPL-CR201
                    MOVE DATA-MOVTO-CR200 TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV        TO DATA-E
                    MOVE DATA-E          TO LINDET-REL(7: 15)
                    MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
                    MOVE ":"                    TO HORA-E(3: 1)
                    MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
                    MOVE HORA-E           TO LINDET-REL(22: 7)
                    MOVE USUARIO-CR200    TO LINDET-REL(29: 10)
                    WRITE REG-RELAT FROM LINDET AFTER 2
                    ADD 2 TO LIN
                    IF LIN > 56
                       PERFORM CABECALHO-AGENDA
                    END-IF
                    PERFORM IMPRIME-TEXTO
                 END-IF
             END-READ
           END-PERFORM.
           CLOSE CRD200 CRD201.

           COPY DESCONDENSA.

       IMPRIME-TEXTO SECTION.

           MOVE ZEROS TO SUBSEQ-CR201.
           START CRD201 KEY IS NOT < CHAVE-CR201 INVALID KEY
                 MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
              READ CRD201 NEXT RECORD AT END MOVE "10" TO ST-CRD201
                NOT AT END
                    IF COD-COMPL-CR201 <> COD-COMPL-CR200 or
                       SEQ-CR201 <> SEQ-CR200
                       MOVE "10" TO ST-CRD201
                    ELSE
                       MOVE SPACES TO LINDET-REL
                       MOVE ANOTACAO-CR201   TO LINDET-REL
                       WRITE REG-RELAT FROM LINDET-REL
                       ADD 1 TO LIN
                       IF LIN > 56 PERFORM CABECALHO
                       END-IF
                    END-IF
              END-READ
           END-PERFORM.
       CABECALHO-AGENDA SECTION.
           MOVE GS-CLASS(1: 1) TO CLASSIF-REL.
           MOVE GS-CLIENTE     TO CLIENTE-REL
           MOVE GS-NOME-CLIENTE TO NOME-CLIENTE-REL.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.

           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02A AFTER 2.
           WRITE REG-RELAT FROM CAB06 AFTER 2.
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
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CHP052"            to logacess-programa
           move "FECHADO"           to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           CLOSE CGD010 CRD001 CHD010 CHD013 CAD004 CAD018 WORK MTD019
                 RCD100 CHD010B CBD001
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
