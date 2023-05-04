       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP052.
      *DATA: 12/04/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Extrato de Cliente individual
      *        Listar todos os títulos com situação = 0(pagar) e 2(pago)
      *        dentro do intervalo de movimento solicitado.
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
           COPY CRPX020.
           COPY CRPX020B.
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
                  RECORD KEY IS CHAVE-WK = COD-COMPL-WK SEQ-WK
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-PAGTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DOCUMENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PORTADOR-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.
      *                 "CRP052.TXT"
      *                 ORGANIZATION IS LINE SEQUENTIAL
      *                 ACCESS MODE IS SEQUENTIAL.
      *                           PRINTER.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CGPW010.
       COPY CAPW018.
       COPY CRPW020.
       COPY CRPW020B.
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
           05  SEQ-WK              PIC 9(5).
           05  DOCUMENTO-WK        PIC X(10).
           05  PORTADOR-WK         PIC X(10).
           05  CARTEIRA-WK         PIC X(4).
           05  SITUACAO-WK         PIC X(10).
           05  SITUACAO-SIST-WK    PIC 9.
           05  VENCTO-WK           PIC 9(8).
           05  DATA-PAGTO-WK       PIC 9(8).
           05  DIAS-ATRAS-WK       PIC 9(5).
           05  VALOR-BRUTO-WK      PIC 9(8)V99.
           05  VALOR-DESCONTO-WK   PIC 9(8)V99.
           05  VALOR-TAXA-ADM-WK   PIC 9(8)V99.
           05  VALOR-SALDO-WK      PIC 9(8)V99.
           05  JR-MULTA-WK         PIC 9(8)V99.
           05  NOSSO-NR-WK         PIC X(15).
           05  FORMA-PAGTO-WK      PIC X(10).
           05  DCR-MEM-WK          PIC X(15).
           05  PARCELA-WK          PIC 9(02).
           05  QT-PARCELA-WK       PIC 9(02).
           05  TIPO-WK             PIC X(05).
           05  VALOR-LIQUIDO-WK    PIC 9(8)V99.
           05  RECEBEDOR-WK        PIC X(15).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP052.CPB".
           COPY "CRP052.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-CBD001             PIC XX       VALUE SPACES.
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
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E2              PIC ZZ.ZZ9,99.
           05  VALOR-E               PIC ZZZZ.ZZ9,99.
      *    BLANK WHEN ZEROS.
           05  VALOR-E3              PIC ZZ9,99      BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZZ.ZZ9,99- BLANK WHEN ZEROS.
           05  TAXA-E                PIC Z9,9     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  VALOR-ACUM            PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-ACUM            PIC 9(8)V99  VALUE ZEROS.
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  TOT-TITULO            PIC 9(6)     VALUE ZEROS.
           05  TOT-VALOR-BRUTO       PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VALOR-LIQUIDO     PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VALOR-SALDO       PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VALOR-REC         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ATRAS-MEDIO       PIC 9(3)V99  VALUE ZEROS.
           05  TOT-VALOR-A-RECEB     PIC 9(8)V99  VALUE ZEROS.
           05  TOT-JUROS-A-RECEB     PIC 9(8)V99  VALUE ZEROS.
           05  ATRASO-MEDIO-E        PIC Z.ZZZ,ZZ.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  COD-COMPL-W           PIC 9(9)     VALUE ZEROS.
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  SEQ-CAIXA             PIC 9(3)     VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
      *    VARIAVEIS P/ CALCULAR JUROS A RECEBER SE EM ATRASO
           05  MESES-ATRASO         PIC 9(05)        VALUE ZEROS.
           05  TAXA-W               PIC 9(3)V9(6) VALUE ZEROS.
           05  JUROS-DIARIO         PIC 9(6)V9(4) VALUE ZEROS.
           05  JUROS-ARECEBER       PIC 9(8)V99   VALUE ZEROS.
           05  DIAS-RESTANTE        PIC 9(2)      VALUE ZEROS.
           05  I                    PIC 9(05)     VALUE ZEROS.
           05  LIN                  PIC 9(02)     VALUE ZEROS.
           05  DATA-AUX             PIC 9(08)     VALUE ZEROS.
           05  FORMA-PAGTO-AUX      PIC X(10)     VALUE SPACES.
           05  DCR-AUX              PIC X(15)     VALUE SPACES.
           05  ACHEI                PIC X(01)     VALUE SPACES.
           05  SEM-CRD020B          PIC X(01)     VALUE SPACES.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(22)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RELACAO DE CONTAS A RECEBER-ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(27)   VALUE SPACES.
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
           05  FILLER              PIC X(131)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(131)  VALUE
                "DATA-VECTO DOCUMENTO  PAR/QT TIPO PORTADOR   CART SITUA
      -    "CAO VALOR BRUTO VALOR SALDO DATA-RECTO DIAS        JR+MT M
      -    "N-BCO".

       01 DET-03C.
          05 FILLER                PIC X(10)
             VALUE "DATA-VECTO".
          05 FILLER                PIC X(01).
          05 FILLER                PIC X(11)
             VALUE "DOCUMENTO".
          05 FILLER                PIC X(07)
             VALUE "PAR/QT".
          05 FILLER                PIC X(05)
             VALUE "TIPO".
          05 FILLER                PIC X(11)
             VALUE "PORTADOR".
          05 FILLER                PIC X(05)
             VALUE "CART".
          05 FILLER                PIC X(14)
             VALUE "SITUACAO".
          05 FILLER                PIC X(13)
             VALUE " VLR BRUTO".
          05 FILLER                PIC X(11)
             VALUE " VLR SALDO".
          05 FILLER                PIC X(11)
             VALUE " VLR DESC.".
          05 FILLER                PIC X(11)
             VALUE " VLR ML+JR".
          05 FILLER                PIC X(11)
             VALUE " VLR TXADM".
          05 FILLER                PIC X(11)
             VALUE "  VLR LIQ.".
          05 FILLER                PIC X(11)
             VALUE "ULT--RECTO".
          05 FILLER                PIC X(05)
             VALUE "DIAS".
          05 FILLER                PIC X(10)
             VALUE "N-BCO".


       01 DET-03.
          05 DET03-VENCTO          PIC 99/99/9999.
          05 FILLER                PIC X(01).
          05 DET03-DOCUMENTO       PIC X(11).
          05 FILLER                PIC X(01).
          05 DET03-PARCELA         PIC 99/.
          05 DET03-QTPAR           PIC 99.
          05 FILLER                PIC X(01).
          05 DET03-TIPO            PIC X(04).
          05 FILLER                PIC X(01).
          05 DET03-PORTADOR        PIC X(10).
          05 FILLER                PIC X(01).
          05 DET03-CART            PIC X(04).
          05 FILLER                PIC X(01).
          05 DET03-SITUACAO        PIC X(13).
          05 FILLER                PIC X(01).
          05 DET03-VALOR-BRUTO     PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(03).
          05 DET03-VALOR-SALDO     PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 DET03-VALOR-DESC      PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 DET03-VALOR-MULTA     PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 DET03-VALOR-TXADM     PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 DET03-VALOR-LIQUIDO   PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 DET03-DTRECTO         PIC X(10).
          05 FILLER                PIC X(01).
          05 DET03-NUMDIAS         PIC ZZZ9.
          05 FILLER                PIC X(01).
          05 DET03-NUMBANCO        PIC X(20).
          05 FILLER                PIC X(01).
          05 DET03-SEQ             PIC 9(05).
          05 FILLER                PIC X(01).
          05 DET03-CODCOMPL        PIC 9(09).

       01  LINDET.
           05  LINDET-REL          PIC X(131)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(131)  VALUE
           "QT.TITULOS       TOTAL BRUTO   TOTAL LIQUIDO   VALOR REC. AT
      -    "RAS.MED    VALOR A RECEB    JR-A-RECEBER".

       01  LINTOT.
           05  LINTOT-REL          PIC X(131)  VALUE SPACES.

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

       01 TOT-BAIXA                PIC 9(08)V99.

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
          05 FILLER                    PIC X(23)
             VALUE "IDENT. BAIXA".
          05 FILLER                    PIC X(16)
             VALUE "RECEBEDOR".
          05 FILLER                    PIC X(38)
             VALUE "CONTA CORRENTE".
          05 FILLER                    PIC X(10)
             VALUE "NEGOCIAÇÃO".



       01 DET-01S.
          05 FILLER                    PIC X(224)
             VALUE ALL "-".

       01 DET-CRD020B.
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
          05 DET-CHAVE-BAIXA           PIC X(22).
          05 FILLER                    PIC X(01).
          05 DET-RECEBEDOR             PIC X(15).
          05 FILLER                    PIC X(01).
          05 DET-CONTA                 PIC 9(06).
          05 FILLER                    PIC X(01).
          05 DET-NOME-CONTA            PIC X(30).
          05 FILLER                    PIC X(01).
          05 DET-NEGOCIACAO            PIC 99/99/9999.

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
           MOVE DATA-INV TO DATA-MOVTO-I.
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
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD020B" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020B.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD200"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "CBD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD001.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CRD001 CAD018 CGD010 CAD004 MTD019 RCD100 CBD001
           OPEN I-O   CRD020 CRD020B
           CLOSE      CRD020 CRD020B
           OPEN INPUT CRD020 CRD020B
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
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020B <> "00"
              MOVE "ERRO ABERTURA CRD020B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP052"            to logacess-programa
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
               WHEN GS-ITEM-ABAIXAR-TRUE
                    PERFORM ITEM-ABAIXAR
               WHEN GS-VERIFICA-SITUACAO-TRUE
                    PERFORM VERIFICA-SITUACAO
               WHEN GS-REVERTE-TRUE
                    PERFORM REVERTER-SITUACAO
               WHEN GS-REGRAVA-DADOS-TRUE
                    PERFORM REGRAVA-DADOS
               WHEN GS-CALCULA-VLR-LIQ-TRUE
                    PERFORM CALCULA-VLR-LIQUIDO
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
               WHEN GS-VERIFICAR-VLBAIXA-TRUE
                    PERFORM VERIFICAR-VLBAIXA
               WHEN GS-VERIFICAR-DATA-TRUE
                    PERFORM VERIFICAR-DATA
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
              CLOSE      CRD020
              OPEN I-O   CRD020


              MOVE "S"   TO SEM-CRD020B

              MOVE ZEROS TO TOT-BAIXA
                            JURO-RCTO-CR20
                            MULTA-RCTO-CR20
                            DESCONTO-CR20
                            DATA-RCTO-CR20
                            VALOR-LIQ-CR20

              INITIALIZE REG-CRD020B
              MOVE COD-COMPL-CR20         TO COD-COMPL-CR20B
              MOVE SEQ-CR20               TO SEQ-CR20B
              START CRD020B KEY IS NOT LESS CHAVE-CR20B INVALID KEY
                   MOVE "10" TO ST-CRD020B
              END-START

              PERFORM UNTIL ST-CRD020B = "10"
                   READ CRD020B NEXT AT END
                        MOVE "10" TO ST-CRD020B
                   NOT AT END
                        IF COD-COMPL-CR20 <> COD-COMPL-CR20B OR
                           SEQ-CR20       <> SEQ-CR20B
                           MOVE "10" TO ST-CRD020B
                        ELSE
                           ADD  VALOR-BAIXA-CR20B TO TOT-BAIXA
                           ADD  JURO-RCTO-CR20B   TO JURO-RCTO-CR20
                           ADD  MULTA-RCTO-CR20B  TO MULTA-RCTO-CR20
                           ADD  DESCONTO-CR20B    TO DESCONTO-CR20
                           MOVE DATA-RCTO-CR20B   TO DATA-RCTO-CR20
                           ADD  VALOR-LIQ-CR20B   TO VALOR-LIQ-CR20
                           MOVE "N" TO SEM-CRD020B
                        END-IF
                   END-READ
              END-PERFORM

              IF VALOR-SALDO-CR20 > VALOR-TOT-CR20
                 MOVE VALOR-TOT-CR20 TO VALOR-SALDO-CR20
                 IF SITUACAO-CR20 < 3
                    MOVE 0           TO SITUACAO-CR20
                 END-IF
                 REWRITE REG-CRD020 NOT INVALID KEY
                     MOVE "Registro atualizado com sucesso" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                 END-REWRITE
              END-IF
              IF SEM-CRD020B = "S"
                 MOVE VALOR-TOT-CR20 TO VALOR-SALDO-CR20
                 IF SITUACAO-CR20 < 3
                    MOVE 0           TO SITUACAO-CR20
                                        JURO-RCTO-CR20
                                        MULTA-RCTO-CR20
                                        DESCONTO-CR20
                                        DATA-RCTO-CR20
                                        VALOR-LIQ-CR20
                 END-IF
                 REWRITE REG-CRD020 NOT INVALID KEY
                     MOVE "Registro atualizado com sucesso" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                 END-REWRITE
              ELSE
                 IF TOT-BAIXA = VALOR-TOT-CR20
                    MOVE 2 TO SITUACAO-CR20
                 ELSE
                    MOVE 1 TO SITUACAO-CR20
                 END-IF
                 REWRITE REG-CRD020 NOT INVALID KEY
                     MOVE "Registro atualizado com sucesso" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                 END-REWRITE
              END-IF

      *       MOVE DATA-RCTO-CR20    TO GS-DATA-RCTO
              MOVE ZEROS             TO GS-DATA-RCTO
              MOVE VALOR-TOT-CR20    TO GS-VALOR-TITULO
              MOVE VALOR-SALDO-CR20  TO GS-VALOR-SALDO
              MOVE VALOR-SALDO-CR20  TO GS-VALOR-BAIXA
              MOVE ZEROS             TO GS-JUROS
              MOVE ZEROS             TO GS-MULTA
              MOVE ZEROS             TO GS-DESCONTO
              MOVE ZEROS             TO GS-VALOR-LIQUIDO
              MOVE SPACES            TO GS-FORMA-PAGTO-D
              MOVE USUARIO-W         TO GS-RECEBEDOR
              MOVE SPACES            TO GS-DCR-MEM-R
              MOVE ZEROS             TO GS-DATA-NEGOCIACAO
              MOVE "REFRESH-WIN3" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM

              CLOSE      CRD020
              OPEN INPUT CRD020

              UNSHOW-WINDOW WIN3 PRINCIPAL
              PERFORM GRAVA-WORK
              PERFORM CARREGA-LISTA.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VERIFICAR-DATA SECTION.
           MOVE CLASS-CLIENTE-CR20         TO CLASS-CLIENTE-CR20B
           MOVE CLIENTE-CR20               TO CLIENTE-CR20B
           MOVE SEQ-CR20                   TO SEQ-CR20B
           MOVE GS-DATA-RCTO               TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                   TO DATA-RCTO-CR20B
           READ CRD020B NOT INVALID KEY
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

           IF GS-VALOR-BAIXA = 0
              MOVE "Valor da Baixa não informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

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

       LER-CONTA SECTION.
           IF GS-ACP-CONTA > 0
              MOVE GS-ACP-CONTA TO CODIGO-FORN-CB01
              READ CBD001 INVALID KEY
                  MOVE "Conta Corrente Inválida" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
                  MOVE ZEROS        TO GS-ACP-CONTA
                  MOVE SPACES       TO GS-DESC-CONTA
              NOT INVALID KEY
                  MOVE TITULAR-CB01 TO GS-DESC-CONTA
              END-READ
           ELSE
              MOVE SPACE TO GS-DESC-CONTA
           END-IF.

       POPUP-CONTA SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CBP001T".
           MOVE PASSAR-PARAMETROS(49: 6) TO GS-ACP-CONTA
           PERFORM LER-CONTA.


       TRANSFERIR-PORTADOR SECTION.
           CLOSE    CRD020
           OPEN I-O CRD200 CRD201 CRD020

           MOVE 1 TO GS-CONT
           MOVE SPACES TO GS-LINDET
           MOVE "LER-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
                IF GS-LINDET(101:1) = "B"
                   PERFORM TRANSFERIR-CHEQUE
                END-IF
                ADD 1 TO GS-CONT
                MOVE SPACES TO GS-LINDET
                MOVE "LER-LB" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE CRD200 CRD201 CRD020
           OPEN INPUT CRD020.

       TRANSFERIR-CHEQUE SECTION.
           CLOSE CRD020
           OPEN I-O CRD020
           MOVE GS-LINDET(147: 9)  TO COD-COMPL-CR20
           MOVE GS-LINDET(142: 05) TO SEQ-CR20

           READ CRD020 INVALID KEY
               MOVE "Duplicata Não Encontrada" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               PERFORM GRAVAR-ANOTACOES
               MOVE GS-PORTADOR        TO PORTADOR-CR20
               REWRITE REG-CRD020 INVALID KEY
                   MOVE "Erro de Regravação...CRD020" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               END-REWRITE
           END-READ.
           CLOSE CRD020
           OPEN INPUT CRD020.

       GRAVAR-ANOTACOES SECTION.
           INITIALIZE REG-CRD200
           MOVE GS-CLASS(1: 1) TO COD-COMPL-W(1: 1).
           MOVE GS-CLIENTE TO COD-COMPL-W(2: 8)
           MOVE COD-COMPL-W TO COD-COMPL-CR200
           MOVE ZEROS TO ULT-SEQ
           MOVE ALL "9" TO SEQ-CR200 .
           START CRD200 KEY IS LESS THAN CHAVE-CR200 NOT INVALID KEY
                READ CRD200 PREVIOUS NOT AT END
                     IF COD-COMPL-CR200 = COD-COMPL-W
                        MOVE SEQ-CR200 TO ULT-SEQ.

           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1 TO ULT-SEQ.
           COPY "CBDATA1.CPY".
           MOVE ULT-SEQ        TO SEQ-CR200.
           MOVE COD-COMPL-W    TO COD-COMPL-CR200.
           MOVE ZEROS          TO DATA-RETORNO-CR200
           MOVE USUARIO-W      TO USUARIO-CR200
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE WS-DATA-CPU    TO DATA-MOVTO-CR200
      *    MOVE DATA-MOVTO-I   TO DATA-MOVTO-CR200
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

           MOVE PORTADOR-CR20  TO PORTADOR
           READ CAD018 INVALID KEY
               MOVE "******"   TO NOME-PORT
           END-READ


           MOVE SPACES             TO GS-ANOTACAO
      *    STRING "TRANSF.PORTADOR-CHEQUE: " NR-DOCTO-CR20 "    - "
      *    PORTADOR-CR20 "-" NOME-PORT " P/ " GS-PORTADOR "-"
      *               GS-DESC-PORTADOR INTO GS-ANOTACAO
           STRING "ALTERACAO PORTADOR ATUAL: "
           PORTADOR-CR20 "-" NOME-PORT " P/ " GS-PORTADOR "-"
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
           MOVE "SENHA20" TO PROGRAMA-CA004.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
                MOVE "Usuário Sem Permissão" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               IF GS-LINDET(101:1) = "B"
                  MOVE SPACES TO GS-LINDET(101:1)
               ELSE
                  MOVE "B" TO GS-LINDET(101:1).

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to gs-flag-critica
           move spaces to mensagem.

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
       VERIFICA-SITUACAO SECTION.
           MOVE 0                      TO GS-SITUACAO
           MOVE GS-LINHA-BAIXA         TO DET-CRD020B
           MOVE DET-CHAVE-BAIXA(1:9)   TO COD-COMPL-CR20B
           MOVE DET-CHAVE-BAIXA(10:5)  TO SEQ-CR20B
           MOVE DET-CHAVE-BAIXA(15:8)  TO DATA-RCTO-CR20B
           READ CRD020B INVALID KEY
               MOVE "Baixa do Título Não Encontrado" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE COD-COMPL-CR20B   TO COD-COMPL-CR20
               MOVE SEQ-CR20B         TO SEQ-CR20
               READ CRD020 INVALID KEY
                    MOVE "Título Não Encontrado" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               NOT INVALID KEY
                    MOVE SITUACAO-CR20 TO GS-SITUACAO.
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
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           MOVE ULT-SEQ      TO SEQ-CR200.
           MOVE COD-COMPL-W  TO COD-COMPL-CR200.
           MOVE GS-DATA-AGENDADA TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE WS-DATA-CPU    TO DATA-MOVTO-CR200
      *    MOVE DATA-MOVTO-I TO DATA-MOVTO-CR200
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
           CALL "GRIDAT1" USING DATA-INV                                                                                      v
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
       REVERTER-SITUACAO SECTION.
           CLOSE    CRD020 CRD020B
           OPEN I-O CRD020 CRD020B

           DELETE CRD020B INVALID KEY
               MOVE "Erro de Exclusão...CRD020B" TO MENSAGEM
               MOVE "C"                          TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           END-DELETE

           COMPUTE MULTA-RCTO-CR20 = MULTA-RCTO-CR20 - MULTA-RCTO-CR20B
           COMPUTE JURO-RCTO-CR20  = JURO-RCTO-CR20  - JURO-RCTO-CR20B
           COMPUTE DESCONTO-CR20   = DESCONTO-CR20   - DESCONTO-CR20B
           COMPUTE VALOR-LIQ-CR20  = VALOR-LIQ-CR20  - VALOR-LIQ-CR20B

           ADD VALOR-BAIXA-CR20B   TO VALOR-SALDO-CR20

           IF VALOR-SALDO-CR20 = VALOR-TOT-CR20
              MOVE 0               TO SITUACAO-CR20
              MOVE 0               TO VALOR-LIQ-CR20
              MOVE 0               TO DESCONTO-CR20
              MOVE 0               TO MULTA-RCTO-CR20
              MOVE 0               TO JURO-RCTO-CR20
           ELSE
              MOVE 1               TO SITUACAO-CR20
           END-IF

           MOVE "N" TO ACHEI
           INITIALIZE REG-CRD020B
                      DATA-AUX
                      FORMA-PAGTO-AUX
                      DCR-AUX
           MOVE CLASS-CLIENTE-CR20    TO CLASS-CLIENTE-CR20B
           MOVE CLIENTE-CR20          TO CLIENTE-CR20B
           MOVE SEQ-CR20              TO SEQ-CR20B
           START CRD020B KEY IS NOT LESS CHAVE-CR20B INVALID KEY
                MOVE "10" TO ST-CRD020
           END-START

           PERFORM UNTIL ST-CRD020B = "10"
                READ CRD020B NEXT AT END
                     MOVE "10" TO ST-CRD020B
                NOT AT END
                     IF CLASS-CLIENTE-CR20 <> CLASS-CLIENTE-CR20B OR
                        CLIENTE-CR20       <> CLIENTE-CR20B       OR
                        SEQ-CR20           <> SEQ-CR20B
                        MOVE "10" TO ST-CRD020B
                     ELSE
                        MOVE DATA-RCTO-CR20B   TO DATA-AUX
                        MOVE FORMA-PAGTO-CR20B TO FORMA-PAGTO-AUX
                        MOVE DCR-MEM-CR20B     TO DCR-AUX

                        MOVE "S" TO ACHEI
                     END-IF
                END-READ
           END-PERFORM

           IF ACHEI = "N"
              MOVE 0              TO SITUACAO-CR20
              MOVE 0              TO VALOR-LIQ-CR20
              MOVE 0              TO DESCONTO-CR20
              MOVE 0              TO MULTA-RCTO-CR20
              MOVE 0              TO JURO-RCTO-CR20
              MOVE VALOR-TOT-CR20 TO VALOR-SALDO-CR20
           END-IF

           MOVE DATA-AUX               TO DATA-RCTO-CR20
           MOVE FORMA-PAGTO-AUX        TO FORMA-PAGTO-CR20
           MOVE DCR-AUX                TO DCR-MEM-CR20

           REWRITE REG-CRD020


           MOVE CLASS-CLIENTE-CR20 TO CLASSIF-WK
           MOVE CLIENTE-CR20       TO CLIENTE-WK
           MOVE SEQ-CR20           TO SEQ-WK
           READ WORK INVALID KEY
                CONTINUE
           NOT INVALID KEY
                 MOVE ZEROS            TO DATA-PAGTO-WK
                                          JR-MULTA-WK
                 MOVE VALOR-TOT-CR20   TO VALOR-BRUTO-WK
                 MOVE ZEROS            TO VALOR-TAXA-ADM-WK

                 MOVE ZEROS               TO VALOR-TAXA-ADM-WK
                 IF TIPO-DOCTO-CR20 = 4
                    IF TOT-PARC-CR20 > 1
                       COMPUTE VALOR-LIQUIDO-WK = VALOR-TOT-CR20 +
                                                  JURO-RCTO-CR20 +
                                                  MULTA-RCTO-CR20 -
                                                 (DESCONTO-CR20   +
                                                 (VALOR-TOT-CR20 *
                                              TAXA-ADMINIST-PARCELA-CR20
                                                  / 100))
                       COMPUTE VALOR-TAXA-ADM-WK =
                               (VALOR-TOT-CR20 *
                                TAXA-ADMINIST-PARCELA-CR20 / 100)
                    ELSE
                       COMPUTE VALOR-LIQUIDO-WK = VALOR-TOT-CR20 +
                                                  JURO-RCTO-CR20 +
                                                  MULTA-RCTO-CR20 -
                                                 (DESCONTO-CR20   +
                                                 (VALOR-TOT-CR20 *
                                              TAXA-ADMINIST-CREDITO-CR20
                                                  / 100))
                       COMPUTE VALOR-TAXA-ADM-WK =
                               (VALOR-TOT-CR20 *
                                TAXA-ADMINIST-CREDITO-CR20 / 100)
                    END-IF
                 ELSE
                    COMPUTE VALOR-LIQUIDO-WK = VALOR-TOT-CR20 +
                                               JURO-RCTO-CR20 +
                                               MULTA-RCTO-CR20 -
                                               DESCONTO-CR20
                 END-IF
                 MOVE VALOR-SALDO-CR20  TO VALOR-SALDO-WK
                 MOVE DESCONTO-CR20     TO VALOR-DESCONTO-WK
                 MOVE FORMA-PAGTO-CR20  TO FORMA-PAGTO-WK
                 MOVE RECEBEDOR-CR20    TO RECEBEDOR-WK
                 MOVE DCR-MEM-CR20      TO DCR-MEM-WK
                 MOVE ZEROS             TO DIAS-ATRAS-WK
                 REWRITE REG-WORK
                 END-REWRITE
           END-READ.
           CLOSE      CRD020 CRD020B
           OPEN INPUT CRD020 CRD020B
           PERFORM CARREGA-LISTA.
       ITEM-ABAIXAR SECTION.
           MOVE GS-LINDET          TO DET-03
           MOVE DET03-CODCOMPL     TO COD-COMPL-CR20
           MOVE DET03-SEQ          TO SEQ-CR20
           READ CRD020 INVALID KEY
                INITIALIZE REG-CRD020.

           MOVE "LIMPAR-LB-BAIXA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-01             TO GS-LINHA-BAIXA
           MOVE "INSERIR-LB-BAIXA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-01S            TO GS-LINHA-BAIXA
           MOVE "INSERIR-LB-BAIXA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-CRD020B
                      TOT-BAIXA
           MOVE CLASS-CLIENTE-CR20     TO CLASS-CLIENTE-CR20B
           MOVE CLIENTE-CR20           TO CLIENTE-CR20B
           MOVE SEQ-CR20               TO SEQ-CR20B

           START CRD020B KEY IS NOT LESS CHAVE-CR20B INVALID KEY
                 MOVE "10" TO ST-CRD020B.

           PERFORM UNTIL ST-CRD020B = "10"
                 READ CRD020B NEXT AT END
                      MOVE "10" TO ST-CRD020B
                 NOT AT END
                      IF CLASS-CLIENTE-CR20 <> CLASS-CLIENTE-CR20B OR
                         CLIENTE-CR20       <> CLIENTE-CR20B       OR
                         SEQ-CR20           <> SEQ-CR20B
                         MOVE "10" TO ST-CRD020B
                      ELSE
                         MOVE DATA-RCTO-CR20B    TO DATA-INV
                         CALL "GRIDAT1" USING DATA-INV
                         MOVE DATA-INV           TO DET-DATA-RCTO
                         MOVE VALOR-TOT-CR20B    TO DET-VALOR-TITULO
                         MOVE VALOR-BAIXA-CR20B  TO DET-VALOR-BAIXA
                         ADD  VALOR-BAIXA-CR20B  TO TOT-BAIXA

                         COMPUTE DET-VALOR-SALDO = VALOR-TOT-CR20 -
                                                   TOT-BAIXA

                         MOVE JURO-RCTO-CR20B    TO DET-JUROS
                         MOVE MULTA-RCTO-CR20B   TO DET-MULTA
                         MOVE DESCONTO-CR20B     TO DET-DESCONTO
                         MOVE VALOR-LIQ-CR20B    TO DET-VLRLIQUIDO
                         MOVE FORMA-PAGTO-CR20B  TO DET-FORMA-PAGTO-D
                         MOVE RECEBEDOR-CR20B    TO DET-RECEBEDOR
                         MOVE DCR-MEM-CR20B      TO DET-DCR-MEM-R

                         STRING COD-COMPL-CR20B SEQ-CR20 DATA-RCTO-CR20B
                           INTO DET-CHAVE-BAIXA
                         MOVE CONTA-CORRENTE-CR20B TO DET-CONTA
                                                      CODIGO-FORN-CB01
                         READ CBD001 INVALID KEY
                              MOVE SPACES        TO TITULAR-CB01
                         END-READ
                         MOVE TITULAR-CB01       TO DET-NOME-CONTA

                         IF DATA-NEGOCIACAO-CR20B IS NOT NUMERIC
                            MOVE 0 TO DATA-NEGOCIACAO-CR20B
                         END-IF

                         IF DATA-NEGOCIACAO-CR20B > 0
                            MOVE DATA-NEGOCIACAO-CR20B TO DATA-INV
                            CALL "GRIDAT1" USING DATA-INV
                         ELSE
                            MOVE 0                     TO DATA-INV
                         END-IF
                         MOVE DATA-INV           TO DET-NEGOCIACAO

                         MOVE DET-CRD020B        TO GS-LINHA-BAIXA
                         MOVE "INSERIR-LB-BAIXA" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM

      *    MOVE DATA-RCTO-CR20    TO GS-DATA-RCTO
           MOVE ZEROS             TO GS-DATA-RCTO
           MOVE VALOR-TOT-CR20    TO GS-VALOR-TITULO
           MOVE VALOR-SALDO-CR20  TO GS-VALOR-SALDO
           MOVE VALOR-SALDO-CR20  TO GS-VALOR-BAIXA
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
       CALCULA-VLR-LIQUIDO SECTION.
           COMPUTE GS-VALOR-LIQUIDO = (GS-VALOR-BAIXA + GS-JUROS +
                                       GS-MULTA) - GS-DESCONTO.
       REGRAVA-DADOS SECTION.
           CLOSE    CRD020 CRD020B
           OPEN I-O CRD020 CRD020B

           MOVE CLASS-CLIENTE-CR20         TO CLASS-CLIENTE-CR20B
           MOVE CLIENTE-CR20               TO CLIENTE-CR20B
           MOVE SEQ-CR20                   TO SEQ-CR20B
           MOVE GS-JUROS                   TO JURO-RCTO-CR20B
           MOVE GS-MULTA                   TO MULTA-RCTO-CR20B
           MOVE GS-DESCONTO                TO DESCONTO-CR20B
           MOVE GS-DATA-RCTO               TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                   TO DATA-RCTO-CR20B
           MOVE GS-DCR-MEM-R               TO DCR-MEM-CR20B
           MOVE GS-FORMA-PAGTO-D           TO FORMA-PAGTO-CR20B
           MOVE GS-RECEBEDOR               TO RECEBEDOR-CR20B
           MOVE SEQ-CAIXA-CR20             TO SEQ-CAIXA-CR20B
           MOVE GS-VALOR-BAIXA             TO VALOR-BAIXA-CR20B
           MOVE GS-VALOR-LIQUIDO           TO VALOR-LIQ-CR20B
           MOVE GS-VALOR-TITULO            TO VALOR-TOT-CR20B
           MOVE GS-ACP-CONTA               TO CONTA-CORRENTE-CR20B
           MOVE GS-DATA-NEGOCIACAO         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                   TO DATA-NEGOCIACAO-CR20B

           WRITE REG-CRD020B INVALID KEY
                MOVE "Erro de Gravação...CRD020B" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE CLASS-CLIENTE-CR20  TO CLASSIF-WK
                MOVE CLIENTE-CR20        TO CLIENTE-WK
                MOVE SEQ-CR20            TO SEQ-WK
                READ WORK INVALID KEY
                     MOVE "Work Não Encontrada" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                NOT INVALID KEY
                     ADD  GS-JUROS         TO JURO-RCTO-CR20
                     ADD  GS-MULTA         TO MULTA-RCTO-CR20
                     ADD  GS-DESCONTO      TO DESCONTO-CR20
                     ADD  GS-VALOR-LIQUIDO TO VALOR-LIQ-CR20
                     MOVE VALOR-TOT-CR20   TO VALOR-BRUTO-WK
                     MOVE GS-FORMA-PAGTO-D TO FORMA-PAGTO-CR20
                                              FORMA-PAGTO-WK
                     MOVE GS-RECEBEDOR     TO RECEBEDOR-CR20
                                              RECEBEDOR-WK
                     MOVE GS-DCR-MEM-R     TO DCR-MEM-CR20
                                              DCR-MEM-WK
                     MOVE GS-DATA-RCTO     TO DATA-INV
                     CALL "GRIDAT2" USING DATA-INV
                     MOVE DATA-INV         TO DATA-RCTO-CR20
                                              DATA-PAGTO-WK
                     COMPUTE VALOR-SALDO-CR20 = VALOR-SALDO-CR20 -
                                                VALOR-BAIXA-CR20B
                     MOVE VALOR-SALDO-CR20 TO VALOR-SALDO-WK
                     IF VALOR-SALDO-CR20 = 0
                        MOVE 2 TO SITUACAO-CR20
                     ELSE
                        MOVE 1 TO SITUACAO-CR20
                     END-IF
                     PERFORM DIAS-ATRASO
                     REWRITE REG-CRD020 INVALID KEY
                         MOVE "Erro de Regravação...CRD020" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                     END-REWRITE
                     COMPUTE JR-MULTA-WK = JR-MULTA-WK +
                                           MULTA-RCTO-CR20 +
                                           JURO-RCTO-CR20
                     COMPUTE VALOR-DESCONTO-WK = VALOR-DESCONTO-WK +
                                                 DESCONTO-CR20

                     MOVE ZEROS               TO VALOR-TAXA-ADM-WK
                     IF TIPO-DOCTO-CR20 = 4
                        IF TOT-PARC-CR20 > 1
                           COMPUTE VALOR-LIQUIDO-WK = VALOR-TOT-CR20 +
                                                      JURO-RCTO-CR20 +
                                                      MULTA-RCTO-CR20 -
                                                     (DESCONTO-CR20   +
                                                     (VALOR-TOT-CR20 *
                                             TAXA-ADMINIST-PARCELA-CR20
                                                      / 100))
                           COMPUTE VALOR-TAXA-ADM-WK =
                                   (VALOR-TOT-CR20 *
                                    TAXA-ADMINIST-PARCELA-CR20 / 100)
                        ELSE
                           COMPUTE VALOR-LIQUIDO-WK = VALOR-TOT-CR20 +
                                                      JURO-RCTO-CR20 +
                                                      MULTA-RCTO-CR20 -
                                                     (DESCONTO-CR20   +
                                                     (VALOR-TOT-CR20 *
                                             TAXA-ADMINIST-CREDITO-CR20
                                                      / 100))
                           COMPUTE VALOR-TAXA-ADM-WK =
                                   (VALOR-TOT-CR20 *
                                    TAXA-ADMINIST-CREDITO-CR20 / 100)
                        END-IF
                     ELSE
                        COMPUTE VALOR-LIQUIDO-WK = VALOR-TOT-CR20 +
                                                   JURO-RCTO-CR20 +
                                                   MULTA-RCTO-CR20 -
                                                   DESCONTO-CR20
                     END-IF

                     REWRITE REG-WORK
                     END-REWRITE
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

           CLOSE      CRD020 CRD020B
           OPEN INPUT CRD020 CRD020B

           PERFORM CARREGA-LISTA.

       VERIFICA-DATA-MOVTO-ANT SECTION.
      *    IF GS-VECTO-INI NOT = VECTO-INI-ANT
      *       OR GS-VECTO-FIM NOT = VECTO-FIM-ANT
      *          PERFORM GRAVA-WORK.
       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
           MOVE ZEROS TO TOT-VALOR-BRUTO
                         TOT-VALOR-LIQUIDO
                         TOT-VALOR-SALDO
                         TOT-VALOR-REC
                         TOT-VALOR-A-RECEB
                         TOTAL-ACUM
                         TOT-TITULO
                         TOT-JUROS-A-RECEB.
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
           MOVE GS-CLASS(1: 1) TO CLASS-CLIENTE-CR20 CLASSIF-W
           MOVE GS-CLIENTE     TO CLIENTE-CR20.
           MOVE VECTO-INI         TO DATA-VENCTO-CR20.
           START CRD020 KEY IS NOT < ALT1-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD WITH IGNORE LOCK AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      IF CLIENTE-CR20 NOT = GS-CLIENTE OR
                         CLASS-CLIENTE-CR20 NOT = CLASSIF-W
                         MOVE "10" TO ST-CRD020
                      ELSE
                         IF DATA-VENCTO-CR20 > VECTO-FIM
                            MOVE "10" TO ST-CRD020
                         ELSE
                            MOVE DATA-VENCTO-CR20    TO GS-EXIBE-VECTO
                            MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
      *                     IF SITUACAO-CR20 = 0 OR SITUACAO-CR20 = 2 OR
      *                        SITUACAO-CR20 = 4
                               PERFORM MOVER-DADOS-WORK
                               PERFORM TOTALIZA-DADOS
                               WRITE REG-WORK
      *                     ELSE CONTINUE
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2"     TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           MOVE CLASS-CLIENTE-CR20  TO CLASSIF-WK
           MOVE CLIENTE-CR20        TO CLIENTE-WK
           MOVE SEQ-CR20            TO SEQ-WK
           EVALUATE CARTEIRA-CR20
              WHEN 1 MOVE "SIMP"    TO CARTEIRA-WK
              WHEN 2 MOVE "CAUC"    TO CARTEIRA-WK
              WHEN 3 MOVE "DESC"    TO CARTEIRA-WK
           END-EVALUATE

           MOVE NR-DOCTO-CR20       TO DOCUMENTO-WK
           MOVE PORTADOR-CR20       TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*****"        TO NOME-PORT
           END-READ
           MOVE NOME-PORT           TO PORTADOR-WK
           MOVE NR-PARC-CR20        TO PARCELA-WK
           MOVE TOT-PARC-CR20       TO QT-PARCELA-WK

           EVALUATE TIPO-DOCTO-CR20
               WHEN 0 MOVE "0-DUP"  TO TIPO-WK
               WHEN 1 MOVE "1-NTF"  TO TIPO-WK
               WHEN 2 MOVE "2-ORG"  TO TIPO-WK
               WHEN 3 MOVE "3-DEB"  TO TIPO-WK
               WHEN 4 MOVE "4-CAR"  TO TIPO-WK
           END-EVALUATE


           MOVE DATA-RCTO-CR20      TO DATA-PAGTO-WK
           MOVE SITUACAO-TIT-CR20   TO CODIGO-CR01
           READ CRD001 INVALID KEY
                MOVE "******"       TO SITUACAO-TIT-CR01
           END-READ
           MOVE SITUACAO-CR20       TO SITUACAO-SIST-WK
           MOVE SITUACAO-TIT-CR01   TO SITUACAO-WK
           IF SITUACAO-CR20 = 5
              MOVE "COBRANCA"       TO SITUACAO-WK
           END-IF
           MOVE OUTRO-DOCTO-CR20    TO NOSSO-NR-WK

           MOVE VALOR-TOT-CR20      TO VALOR-BRUTO-WK

           MOVE VALOR-SALDO-CR20    TO VALOR-SALDO-WK
           MOVE DESCONTO-CR20       TO VALOR-DESCONTO-WK
           MOVE ZEROS               TO VALOR-TAXA-ADM-WK
           IF TIPO-DOCTO-CR20 = 4
              IF TOT-PARC-CR20 > 1
                 COMPUTE VALOR-LIQUIDO-WK = VALOR-TOT-CR20 +
                                            JURO-RCTO-CR20 +
                                            MULTA-RCTO-CR20 -
                                           (DESCONTO-CR20   +
                                           (VALOR-TOT-CR20 *
                                            TAXA-ADMINIST-PARCELA-CR20
                                            / 100))
                 COMPUTE VALOR-TAXA-ADM-WK =
                         (VALOR-TOT-CR20 *
                          TAXA-ADMINIST-PARCELA-CR20 / 100)
              ELSE
                 COMPUTE VALOR-LIQUIDO-WK = VALOR-TOT-CR20 +
                                            JURO-RCTO-CR20 +
                                            MULTA-RCTO-CR20 -
                                           (DESCONTO-CR20   +
                                           (VALOR-TOT-CR20 *
                                            TAXA-ADMINIST-CREDITO-CR20
                                            / 100))
                 COMPUTE VALOR-TAXA-ADM-WK =
                         (VALOR-TOT-CR20 *
                          TAXA-ADMINIST-CREDITO-CR20 / 100)
              END-IF
           ELSE
              COMPUTE VALOR-LIQUIDO-WK = VALOR-TOT-CR20 +
                                         JURO-RCTO-CR20 +
                                         MULTA-RCTO-CR20 -
                                         DESCONTO-CR20
           END-IF


           MOVE FORMA-PAGTO-CR20    TO FORMA-PAGTO-WK
           MOVE RECEBEDOR-CR20      TO RECEBEDOR-WK
           MOVE DCR-MEM-CR20        TO DCR-MEM-WK
           MOVE DATA-VENCTO-CR20    TO VENCTO-WK
           PERFORM DIAS-ATRASO.
           MOVE ZEROS TO JR-MULTA-WK.
           EVALUATE SITUACAO-CR20
             WHEN 0 IF DIAS-ATRAS-WK <> ZEROS
                       PERFORM CALCULA-JUROS-ARECEBER
             WHEN 1 IF DIAS-ATRAS-WK <> ZEROS
                       PERFORM CALCULA-JUROS-ARECEBER
                    END-IF
                    COMPUTE JR-MULTA-WK = JURO-RCTO-CR20 +
                                          MULTA-RCTO-CR20
             WHEN 2 COMPUTE JR-MULTA-WK = JURO-RCTO-CR20 +
                                          MULTA-RCTO-CR20
             WHEN OTHER MOVE ZEROS TO JR-MULTA-WK
           END-EVALUATE.

       CALCULA-JUROS-ARECEBER SECTION.

      *    DISPLAY "DIAS-ATRAS-WK = " DIAS-ATRAS-WK STOP " "

           COMPUTE MESES-ATRASO = DIAS-ATRAS-WK / 30
           COMPUTE TAXA-W = (GS-TAXA / 100) + 1
           MOVE VALOR-SALDO-WK TO JUROS-ARECEBER
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MESES-ATRASO
                  COMPUTE JUROS-ARECEBER = JUROS-ARECEBER * TAXA-W
           END-PERFORM.
           COMPUTE JUROS-DIARIO = ((TAXA-W * JUROS-ARECEBER) -
                                  JUROS-ARECEBER) / 30
           COMPUTE DIAS-RESTANTE = DIAS-ATRAS-WK - (MESES-ATRASO * 30)
           COMPUTE JUROS-ARECEBER = (JUROS-DIARIO * DIAS-RESTANTE) +
                                     JUROS-ARECEBER
           COMPUTE JR-MULTA-WK = JUROS-ARECEBER - VALOR-SALDO-WK.

       TOTALIZA-DADOS SECTION.
      *    SITUACAO 0 = NORMAL   2=PAGA  3-ESTORNADA  4-CANCELADA
      *             1 = PARCIAL
           EVALUATE SITUACAO-CR20
             WHEN 0 ADD 1                TO TOT-TITULO
                    ADD VALOR-TOT-CR20   TO TOT-VALOR-BRUTO
                    ADD VALOR-LIQUIDO-WK TO TOT-VALOR-LIQUIDO
                    ADD VALOR-SALDO-CR20 TO TOT-VALOR-A-RECEB
                    ADD VALOR-SALDO-CR20 TO TOT-VALOR-SALDO
                    ADD JR-MULTA-WK      TO TOT-JUROS-A-RECEB
             WHEN 1 ADD 1                TO TOT-TITULO
                    ADD VALOR-TOT-CR20   TO TOT-VALOR-BRUTO
                    ADD VALOR-LIQUIDO-WK TO TOT-VALOR-LIQUIDO
                    ADD VALOR-SALDO-CR20   TO TOT-VALOR-A-RECEB
                    ADD VALOR-SALDO-CR20 TO TOT-VALOR-SALDO
                    ADD JR-MULTA-WK      TO TOT-JUROS-A-RECEB
             WHEN 2 ADD 1                TO TOT-TITULO
                    ADD VALOR-LIQ-CR20   TO TOT-VALOR-REC
                    ADD VALOR-TOT-CR20   TO TOT-VALOR-BRUTO
                    ADD VALOR-SALDO-CR20 TO TOT-VALOR-SALDO
                    ADD VALOR-LIQUIDO-WK TO TOT-VALOR-LIQUIDO
             WHEN OTHER CONTINUE
           END-EVALUATE.

           IF DIAS-ATRAS-WK = ZEROS CONTINUE
           ELSE ADD VALOR-ACUM   TO TOTAL-ACUM.

      *    IF DATA-RCTO-CR20 NOT = ZEROS
      *       ADD VALOR-ACUM             TO TOTAL-ACUM
      *    ELSE MOVE ZEROS TO TOTAL-ACUM.

       DIAS-ATRASO SECTION.
           IF DATA-RCTO-CR20 = ZEROS
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              MOVE WS-DATA-CPU           TO DATA-MOVTO-I
              MOVE DATA-MOVTO-I          TO GRTIME-DATE-FINAL
           ELSE
              MOVE DATA-RCTO-CR20        TO GRTIME-DATE-FINAL.

           MOVE DATA-VENCTO-CR20         TO GRTIME-DATE.
           MOVE 2                        TO GRTIME-TYPE.
           MOVE 3                        TO GRTIME-FUNCTION.

           IF GRTIME-DATE-FINAL > GRTIME-DATE
              CALL "GRTIME" USING PARAMETROS-GRTIME
              MOVE GRTIME-DAYS-FINAL     TO DIAS-ATRAS-WK
              COMPUTE VALOR-ACUM = DIAS-ATRAS-WK * VALOR-LIQ-CR20
           ELSE MOVE ZEROS TO DIAS-ATRAS-WK.
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
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE ALL "-" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-03C TO GS-LINDET
      *    MOVE "DATA-VECTO DOCUMENTO  PAR/QT TIPO   PORTADOR CART SITUA
      *    "CAO       VALOR BRUTO VALOR SALDO VLR LIQUIDO DATA-RECTO DIA
      *    "S  JR+MT M N-BCO" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ALL "-" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA.

       MOVER-DADOS-LINDET SECTION.
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DET03-VENCTO
           MOVE DOCUMENTO-WK      TO DET03-DOCUMENTO
           MOVE PARCELA-WK        TO DET03-PARCELA
           MOVE QT-PARCELA-WK     TO DET03-QTPAR
           MOVE TIPO-WK           TO DET03-TIPO
           MOVE PORTADOR-WK       TO DET03-PORTADOR
           MOVE CARTEIRA-WK       TO DET03-CART
           MOVE SITUACAO-WK       TO DET03-SITUACAO
           MOVE VALOR-BRUTO-WK    TO DET03-VALOR-BRUTO
           MOVE VALOR-SALDO-WK    TO DET03-VALOR-SALDO
           MOVE VALOR-DESCONTO-WK TO DET03-VALOR-DESC
           MOVE JR-MULTA-WK       TO DET03-VALOR-MULTA
           MOVE VALOR-TAXA-ADM-WK TO DET03-VALOR-TXADM
           MOVE VALOR-LIQUIDO-WK  TO DET03-VALOR-LIQUIDO
           EVALUATE SITUACAO-SIST-WK
               WHEN 4     MOVE "CANCELADA"   TO DET03-DTRECTO
               WHEN 3     MOVE "ESTORNADA"   TO DET03-DTRECTO
               WHEN OTHER MOVE DATA-PAGTO-WK TO DATA-INV
                          CALL "GRIDAT1" USING DATA-INV
                          MOVE DATA-INV      TO DATA-E
                          MOVE DATA-E        TO DET03-DTRECTO
           END-EVALUATE
           MOVE DIAS-ATRAS-WK     TO DET03-NUMDIAS
           MOVE NOSSO-NR-WK       TO DET03-NUMBANCO
           MOVE SEQ-WK            TO DET03-SEQ
           MOVE COD-COMPL-WK      TO DET03-CODCOMPL

           MOVE DET-03            TO GS-LINDET.

      *    MOVE VENCTO-WK         TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO GS-LINDET(01: 11)
      *    MOVE DOCUMENTO-WK      TO GS-LINDET(12: 11)
      *    MOVE PARCELA-WK        TO GS-LINDET(23:2)
      *    MOVE "/"               TO GS-LINDET(25:1)
      *    MOVE QT-PARCELA-WK     TO GS-LINDET(26:2)
      *    MOVE TIPO-WK           TO GS-LINDET(29:5)
      *
      *    MOVE PORTADOR-WK       TO GS-LINDET(35: 11)
      *    MOVE CARTEIRA-WK       TO GS-LINDET(46: 5)
      *    MOVE SITUACAO-WK       TO GS-LINDET(51: 15)
      *    MOVE VALOR-BRUTO-WK    TO VALOR-E
      *    MOVE VALOR-E           TO GS-LINDET(66: 12)
      *    MOVE VALOR-SALDO-WK    TO VALOR-E
      *    MOVE VALOR-E           TO GS-LINDET(78: 12)
      *    MOVE VALOR-LIQUIDO-WK  TO VALOR-E
      *    MOVE VALOR-E           TO GS-LINDET(90:12)
      *
      *    IF SITUACAO-SIST-WK = 4
      *       MOVE "CANCELADA" TO GS-LINDET(102: 11)
      *    ELSE IF SITUACAO-WK = 3
      *            MOVE "ESTORNADA" TO GS-LINDET(102: 11)
      *         ELSE MOVE DATA-PAGTO-WK     TO DATA-INV
      *              CALL "GRIDAT1" USING DATA-INV
      *              MOVE DATA-INV          TO DATA-E
      *              MOVE DATA-E            TO GS-LINDET(102: 11)
      *         END-IF
      *    END-IF
      *    MOVE DIAS-ATRAS-WK     TO GS-LINDET(113: 5)
      *    MOVE JR-MULTA-WK       TO VALOR-E3
      *    MOVE VALOR-E3          TO GS-LINDET(118: 06)
      *    MOVE NOSSO-NR-WK       TO GS-LINDET(126: 16)
      *    MOVE SEQ-WK            TO GS-LINDET(142: 05).
      *    MOVE COD-COMPL-WK      TO GS-LINDET(147: 9).

       CHAMA-ALTERACAO SECTION.
           IF GS-LINDET = SPACES MOVE
              ZEROS TO GS-LINDET.

           MOVE GS-LINDET          TO DET-03
           MOVE DET03-CODCOMPL     TO PASSAR-STRING-1(01:09)
                                      COD-COMPL-CR20
           MOVE DET03-SEQ          TO PASSAR-STRING-1(10:05)
                                      SEQ-CR20
           move usuario-w         to passar-string-1(20: 5)
           MOVE "CRP020" TO PROGRAMA-CA004.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
                CALL   "CRP020B" USING PARAMETROS-W PASSAR-STRING-1
                CANCEL "CRP020B"
              NOT INVALID KEY
                CALL   "CRP020A" USING PARAMETROS-W PASSAR-STRING-1
                CANCEL "CRP020A".

           READ CRD020.

           MOVE CLASS-CLIENTE-CR20 TO CLASSIF-WK.
           MOVE CLIENTE-CR20       TO CLIENTE-WK.
           MOVE SEQ-CR20           TO SEQ-WK.
           READ WORK.
           PERFORM MOVER-DADOS-WORK.
           REWRITE REG-WORK.

           PERFORM MOVER-DADOS-LINDET.
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
                MOVE "DATA-PAGTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO DATA-PAGTO-WK
                START WORK KEY IS NOT < DATA-PAGTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE TOT-TITULO        TO GS-LINTOT(05: 06)

           MOVE TOT-VALOR-BRUTO   TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(12: 11)

           MOVE TOT-VALOR-SALDO   TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(24: 11)

           MOVE TOT-VALOR-LIQUIDO TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(38: 11)

           MOVE TOT-VALOR-REC     TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(49: 11)

           DIVIDE TOTAL-ACUM BY TOT-VALOR-BRUTO GIVING TOT-ATRAS-MEDIO

           MOVE TOT-ATRAS-MEDIO   TO ATRASO-MEDIO-E
           MOVE ATRASO-MEDIO-E    TO GS-LINTOT(62: 9)

           MOVE TOT-VALOR-A-RECEB TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(76: 11)

           MOVE TOT-JUROS-A-RECEB TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(94:11)

           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
      *CALCULA-DIAS SECTION.
      *    MOVE GS-EMISSAO1     TO GRTIME-DATE.
      *    MOVE GS-VENCTO2      TO GRTIME-DATE-FINAL.
      *    MOVE 3                   TO GRTIME-FUNCTION.
      *    MOVE 1                   TO GRTIME-TYPE.
      *    CALL "GRTIME" USING PARAMETROS-GRTIME.
      *    MOVE GRTIME-DAYS-FINAL   TO GS-DIAS2.
      *CALCULA-JUROS SECTION.
      *    MOVE GS-TAXA2        TO GS-TAXA3.
      *    COMPUTE GS-VLR-JUROS2 = (GS-VLR-RETIR2 *
      *          (GS-TAXA2 / 100) /30) * GS-DIAS2.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP052" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
      *    MOVE VENCTO-WK         TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO LINDET-REL(01: 11)
      *    MOVE DOCUMENTO-WK      TO LINDET-REL(12: 11)
      *    MOVE PARCELA-WK        TO LINDET-REL(23:2)
      *    MOVE "/"               TO LINDET-REL(25:1)
      *    MOVE QT-PARCELA-WK     TO LINDET-REL(26:2)
      *    MOVE TIPO-WK           TO LINDET-REL(29:5)
      *
      *    MOVE PORTADOR-WK       TO LINDET-REL(35: 11)
      *    MOVE CARTEIRA-WK       TO LINDET-REL(46: 5)
      *    MOVE SITUACAO-WK       TO LINDET-REL(51: 15)
      *    MOVE VALOR-BRUTO-WK    TO VALOR-E
      *    MOVE VALOR-E           TO LINDET-REL(66: 12)
      *    IF SITUACAO-SIST-WK = 4
      *       MOVE "CANCELADA" TO LINDET-REL(78: 11)
      *    ELSE IF SITUACAO-WK = 3
      *            MOVE "ESTORNADA" TO LINDET-REL(78: 11)
      *         ELSE MOVE DATA-PAGTO-WK     TO DATA-INV
      *              CALL "GRIDAT1" USING DATA-INV
      *              MOVE DATA-INV          TO DATA-E
      *              MOVE DATA-E            TO LINDET-REL(78: 11)
      *         END-IF
      *    END-IF
      *    MOVE DIAS-ATRAS-WK     TO LINDET-REL(89: 5)
      *    MOVE JR-MULTA-WK       TO VALOR-E
      *    MOVE VALOR-E           TO LINDET-REL(94: 12)
      *    MOVE NOSSO-NR-WK       TO LINDET-REL(106: 16).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 58 PERFORM CABECALHO.

           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01: 11)
           MOVE DOCUMENTO-WK      TO LINDET-REL(12: 11)
           MOVE PARCELA-WK        TO LINDET-REL(23:2)
           MOVE "/"               TO LINDET-REL(25:1)
           MOVE QT-PARCELA-WK     TO LINDET-REL(26:2)
           MOVE TIPO-WK           TO LINDET-REL(29:5)

           MOVE PORTADOR-WK       TO LINDET-REL(35: 11)
           MOVE CARTEIRA-WK       TO LINDET-REL(46: 5)
           MOVE SITUACAO-WK       TO LINDET-REL(51: 08)
           MOVE VALOR-BRUTO-WK    TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(60: 12)
           MOVE VALOR-SALDO-WK    TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(72:11)

           IF SITUACAO-SIST-WK = 4
              MOVE "CANCELADA" TO LINDET-REL(84: 11)
           ELSE IF SITUACAO-WK = 3
                   MOVE "ESTORNADA" TO LINDET-REL(84: 11)
                ELSE MOVE DATA-PAGTO-WK     TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV          TO DATA-E
                     MOVE DATA-E            TO LINDET-REL(84: 11)
                END-IF
           END-IF
           MOVE DIAS-ATRAS-WK     TO LINDET-REL(95: 5)
           MOVE JR-MULTA-WK       TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(101: 12)
           MOVE NOSSO-NR-WK       TO LINDET-REL(114: 16)

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 58 PERFORM CABECALHO.

       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE TOT-TITULO        TO LINTOT-REL(01: 14)
           MOVE TOT-VALOR-BRUTO   TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(18: 13)
           MOVE TOT-VALOR-LIQUIDO TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(34: 13)
           MOVE TOT-VALOR-REC     TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(47: 13)
           DIVIDE TOTAL-ACUM BY TOT-VALOR-BRUTO GIVING TOT-ATRAS-MEDIO
           MOVE TOT-ATRAS-MEDIO   TO ATRASO-MEDIO-E
           MOVE ATRASO-MEDIO-E    TO LINTOT-REL(60: 12)
           MOVE TOT-VALOR-A-RECEB TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(74: 16)
           MOVE TOT-JUROS-A-RECEB TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(90: 11)

      *    MOVE VALOR-E           TO LINTOT-REL(15: 13)
      *    MOVE TOT-VALOR-REC     TO VALOR-E
      *    MOVE VALOR-E           TO LINTOT-REL(33: 13)
      *    DIVIDE TOTAL-ACUM BY TOT-VALOR-BRUTO GIVING TOT-ATRAS-MEDIO
      *    MOVE TOT-ATRAS-MEDIO   TO ATRASO-MEDIO-E
      *    MOVE ATRASO-MEDIO-E    TO LINTOT-REL(51: 10)
      *    MOVE TOT-VALOR-A-RECEB TO VALOR-E
      *    MOVE VALOR-E           TO LINTOT-REL(63: 16)
      *    MOVE TOT-JUROS-A-RECEB TO VALOR-E
      *    MOVE VALOR-E           TO LINTOT-REL(79: 13)
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
           IF LIN > 57 PERFORM CABECALHO.

       IMPRIMIR-CONTATO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           OPEN INPUT CRD200 CRD201.
           PERFORM CABECALHO-AGENDA.
           MOVE COD-COMPL-WK      TO COD-COMPL-CR200.
           MOVE ZEROS             TO SEQ-CR200
                                     DATA-MOVTO-CR200
                                     HORA-MOVTO-CR200
           START CRD200 KEY IS NOT < ALT1-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
                 READ CRD200 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD200
                 NOT AT END
                     IF COD-COMPL-CR200 <> COD-COMPL-WK
                        MOVE "10" TO ST-CRD200
                     ELSE
                        MOVE SPACES           TO LINDET-REL
                        MOVE SEQ-CR200        TO SEQ-CR201
                                                 LINDET-REL(1: 5)
                        MOVE COD-COMPL-CR200  TO COD-COMPL-CR201
                        MOVE DATA-MOVTO-CR200 TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV         TO DATA-E
                        MOVE DATA-E           TO LINDET-REL(7: 15)
                        MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
                        MOVE ":"                    TO HORA-E(3: 1)
                        MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
                        MOVE HORA-E           TO LINDET-REL(22: 7)
                        MOVE USUARIO-CR200    TO LINDET-REL(29: 10)
                        WRITE REG-RELAT FROM LINDET AFTER 2
                        ADD 2 TO LIN
                        IF LIN > 57
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
                    IF COD-COMPL-CR201 <> COD-COMPL-CR200
                       MOVE "10" TO ST-CRD201
                    ELSE
                       MOVE SPACES TO LINDET-REL
                       MOVE ANOTACAO-CR201   TO LINDET-REL
                       WRITE REG-RELAT FROM LINDET-REL
                       ADD 1 TO LIN
                       IF LIN > 57 PERFORM CABECALHO
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
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02A AFTER 2.
           WRITE REG-RELAT FROM CAB06 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.

       CABECALHO SECTION.
           MOVE GS-CLASS(1: 1) TO CLASSIF-REL.
           MOVE GS-CLIENTE     TO CLIENTE-REL
           MOVE GS-NOME-CLIENTE TO NOME-CLIENTE-REL.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
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
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP052"            to logacess-programa
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

           CLOSE CGD010 CRD001 CRD020 CAD004 CAD018 WORK MTD019 RCD100
                 CRD020B CBD001
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
