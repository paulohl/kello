       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP100.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 13/09/2000.
      *FUNÇÃO: Movimento de RECIBOS DE VENDAS

       ENVIRONMENT DIVISION.
       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CAPX010.
           COPY CAPX018.
           COPY CAPX030.
           COPY MTPX019.
           COPY MTPX020.
           COPY MTPX020C.
           COPY MTPX020E.
           copy RCPX001.
           COPY RCPX002.
           COPY RCPX100.
           COPY RCPX100C.
           COPY RCPX100E.
           COPY RCPX101.
           COPY COPX040.
           COPY CHPX010.
           COPY CHPX010B.
           COPY CHPX011.
           COPY CHPX099.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX020.
           COPY IEPX010.
           COPY CRPX020.
           COPY CRPX021.
           COPY CRPX024.
           COPY CRPX200.
           COPY CRPX201.
           COPY PARX001.
           COPY LOGX002.
           COPY LOGX003.
           COPY LOGX004.
           COPY LOGACESS.SEL.

          SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-WK = NR-CHEQUE-WK BANCO-WK
                  ALTERNATE RECORD KEY IS ALT-WK = VENCTO-WK
                  NR-CHEQUE-WK BANCO-WK.

          SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW010.
       COPY CAPW018.
       COPY CAPW030.
       COPY MTPW019.
       COPY MTPW020.
       COPY MTPW020C.
       COPY MTPW020E.
       COPY RCPW001.
       COPY RCPW002.
       COPY RCPW100.
       COPY RCPW100C.
       COPY RCPW100E.
       COPY RCPW101.
       COPY COPW040.
       COPY CHPW010.
       COPY CHPW010B.
       COPY CHPW011.
       COPY CHPW099.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW020.
       COPY IEPW010.
       COPY CRPW020.
       COPY CRPW021.
       COPY CRPW024.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGW002.
       COPY LOGW003.
       COPY LOGW004.
       COPY PARW001.
           COPY LOGACESS.FD.

       FD  WORK.
       01  REG-WORK.
           05  VENCTO-WK                PIC 9(8).
           05  NR-CHEQUE-WK             PIC 9(6).
           05  VALOR-WK                 PIC 9(8)V99.
           05  DATA-BAIXA-WK            PIC 9(08).
           05  BANCO-WK                 PIC 9(3).
           05  TIPO-WK                  PIC X(18).
           05  PARCELA-WK               PIC 9(02).
           05  QT-PARCELA-WK            PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-WK PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-WK PIC 9(03)V99.
           05  CARTAO-WK                PIC 9(02).
           05  DESC-CARTAO-WK           PIC X(20).
           05  COMIS-PARC-WK            PIC 99V999.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP100.CPB".
           COPY "RCP100.CPY".
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
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD030             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-RCD001             PIC XX       VALUE SPACES.
           05  ST-RCD002             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD100C            PIC XX       VALUE SPACES.
           05  ST-RCD100E            PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD020C            PIC XX       VALUE SPACES.
           05  ST-MTD020E            PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD010B            PIC XX       VALUE SPACES.
           05  ST-CHD011             PIC XX       VALUE SPACES.
           05  ST-CHD099             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD021             PIC XX       VALUE SPACES.
           05  ST-CRD024             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ST-LOG003             PIC XX       VALUE SPACES.
           05  ST-LOG004             PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  AUX-CONTRATO          PIC 9(08)    VALUE ZEROS.
           05  AUX-ALBUM-REC         PIC 9(08)    VALUE ZEROS.
           05  EXTRA-ENVIADO1-W      PIC X(01)    VALUE SPACES.
           05  EXTRA-ENVIADO2-W      PIC X(01)    VALUE SPACES.
           05  AUX-COD-COMPL         PIC 9(09)    VALUE ZEROS.
           05  ACHEI                 PIC X(01)    VALUE SPACES.
           05  QUAL-ITEM             PIC 9(02)    VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  NUMERO                PIC 9(09)    VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
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
           05  DATA-W.
               10  DIA-W             PIC 9(02).
               10  MES-W             PIC 9(02).
               10  ANO-W             PIC 9(04).
      *    VARIAVEIS P/ CALCULO DO PRAZO MEDIO
           05  NR-ALBUM-W            PIC 9(4)          VALUE ZEROS.
           05  NR-FORM-W             PIC 9(4)          VALUE ZEROS.
           05  VENDEDOR-W            PIC 9(6)          VALUE ZEROS.
           05  DEFLACIONADO-W        PIC 9(10)V999     VALUE ZEROS.
           05  DEF1-W                PIC 9(10)V999     VALUE ZEROS.
           05  TAXA-W                PIC 9(04)V99999   VALUE ZEROS.
           05  TAXA-W1               PIC 9(2)V99       VALUE ZEROS.
           05  TOTALPM-W             PIC 9(14)V99      VALUE ZEROS.
           05  CONT-W                PIC 9(04)         VALUE ZEROS.
           05  TIPO-W                PIC 9             VALUE ZEROS.
           05  PORTADOR-W            PIC 9(04)         VALUE ZEROS.
           05  DESC-PORTADOR-W       PIC X(30)         VALUE SPACES.
           05  TOT-PARCELA           PIC 9(2)          VALUE ZEROS.
           05  NR-PARCELA            PIC 9(2)          VALUE ZEROS.
      *    VARIAVEIS P/ CONTROLE DE EXCLUSAO DE CHEQUES
           05  NR-CHEQUEW.
               10  NR-CHEQUE1        PIC 9(6).
               10  NR-CHEQUE2        PIC X.
           05  NR-CHEQUE-W REDEFINES NR-CHEQUEW PIC X(7).
           05  CHEQUE-ENCONTRADO     PIC 9        VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  VALOR-EXTENSO         PIC X(13).
           05  VALOR-EXTENSO-R REDEFINES VALOR-EXTENSO.
               10 VALOR-N1           PIC 9(02).
               10 FILLER             PIC X(01).
               10 VALOR-N2           PIC 9(03).
               10 FILLER             PIC X(01).
               10 VALOR-N3           PIC 9(03).
               10 FILLER             PIC X(01).
               10 VALOR-N4           PIC 9(02).
           05  VALOR-EXTENSO2        PIC 9(08)V99.
           05  VALOR-EXTENSO2-R REDEFINES VALOR-EXTENSO2.
               10 VALOR-N11          PIC 9(08).
               10 VALOR-N22          PIC 9(02).
           05  AUX-ALBUM             PIC 9(08).
           05  MASC-PERC             PIC Z9,999.
           05  MASC-PERC1            PIC Z9,999.
           05  MASC-PERC2            PIC Z9,999.
           05  MASC-VALOR            PIC ZZZ.ZZ9,99.
           05  AUX-DIA               PIC 9(02).
           05  AUX-MES               PIC 9(02).
           05  AUX-ANO               PIC 9(04).
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  WS-STATUS-ANALISE     PIC 9(02) VALUE ZEROS.
           05  WS-STATUS-REVENDIDO   PIC 9(02) VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 AUX-VALOR                    PIC 9(09)V99 VALUE ZEROS.
       01 QTDE-MESES                   PIC 9(04)    VALUE ZEROS.
       01 IND                          PIC 9(04)    VALUE ZEROS.
       01 QTDE-DIAS                    PIC 9(02)    VALUE ZEROS.


       01 lnkusu.
          copy usuario.cpy.

       01 lnktabela.
          02 lnkobjetoscol object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.

       01 indice                       pic 9(02).
       01 wssize                       pic 9(09) comp-5 value zeros.
       01 wsIndice                     pic 9(09) comp-5 value zeros.
       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 umitem                       object reference.
       77 umobjeto                     object reference.

       01 QUAL-PRODUTO             PIC X(15).
       01 QTDE-MONTAGEM            PIC 9(06).
       01 MASC-MONTAGEM            PIC ZZZ.ZZZ.
       01 QTDE-RECIBO              PIC 9(06).
       01 MASC-RECIBO              PIC ZZZ.ZZZ.
       01 QTDE-DIFERENCA           PIC 9(06).
       01 MASC-DIFERENCA           PIC ZZZ.ZZZ.
       01 VLR-ESCALA               PIC 9(06)V9999.
       01 MASC-ESCALA              PIC ZZZ.ZZ9,9999 BLANK WHEN ZEROS.
       01 ACIMA-DE                 PIC 9(04).
       01 MASC-ACIMA-DE            PIC Z.ZZ9 BLANK WHEN ZEROS.
       01 PERC-DESC-COMISSAO       PIC 9(03)V9(02).
       01 PERC-DESC-FORMANDO       PIC 9(03)V9(02).
       01 MASC-DESC                PIC ZZ9,99 BLANK WHEN ZEROS.
       01 TOT-ESCALA               PIC 9(06)V99.
       01 MASC-TESCALA             PIC ZZZ.ZZ9,9999 BLANK WHEN ZEROS.

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


       01  CAB01.
           05  EMPRESA-REL         PIC X(80)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(59)   VALUE
           "CONFERENCIA-MOVTO DE REVELACAO DE FILMES".
           05  FILLER              PIC X(07)   VALUE "MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(07)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(100)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(100)  VALUE
           "SEQ FUNCIO-NOME       TURNO      QT-FI TP-FILME   HR-INI HR-
      -    "FIM INTERRUP TP-INTERRUPÇ INTERV".

       01  LINDET.
           05  LINDET-REL          PIC X(100)   VALUE SPACES.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 TAB-VALOR                      PIC 9(8)V99.
       01 TAB-VALOR-R REDEFINES TAB-VALOR.
          05 TAB-VALOR1                  PIC 9(01).
          05 TAB-VALOR2                  PIC 9(01).
          05 TAB-VALOR3                  PIC 9(01).
          05 TAB-VALOR4                  PIC 9(01).
          05 TAB-VALOR5                  PIC 9(01).
          05 TAB-VALOR6                  PIC 9(01).
          05 TAB-VALOR7                  PIC 9(01).
          05 TAB-VALOR8                  PIC 9(01).
          05 TAB-VALOR9                  PIC 9(01).
          05 TAB-VALOR10                 PIC 9(01).


           copy   "ldifdias".

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CAD030"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD030.
           MOVE "CGD001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "RCD001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD001.
           MOVE "RCD002"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD002.
           MOVE "RCD100"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD100C" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100C.
           MOVE "RCD100E" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100E.
           MOVE "RCD101"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD101.
           MOVE "MTD019"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD020C" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020C.
           MOVE "MTD020E" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020E.
           MOVE "IED010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "CHD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD010B" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010B.
           MOVE "CHD011"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD011.
           MOVE "CHD099"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD099.
           MOVE "COD040"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "CRD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD021"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD021.
           MOVE "CRD024"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD024.
           MOVE "CRD200"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "PAR001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "LOG002"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOG003"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG003.
           MOVE "LOG004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG004.

           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario

           OPEN INPUT CGD001 IED010 COD040 CAD004 CAD010 MTD019 RCD001
                      CAD018 CAD030 PAR001 .


           OPEN I-O RCD100 RCD101 CGD010 MTD020 CHD010 CHD011 CHD099
                    CRD020 CRD021 CRD024 CGD020 RCD002 CHD010B RCD100C
                    MTD020C LOG002 LOG003 LOG004 MTD020E RCD100E

           CLOSE      RCD100C MTD020C LOG002 LOG003 LOG004 MTD020E
                      RCD100E
           OPEN INPUT MTD020E
           OPEN I-O   RCD100C MTD020C RCD100E

           CLOSE    CRD024
           OPEN I-O CRD024

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.
           IF ST-RCD002 = "35"
              CLOSE RCD002      OPEN OUTPUT RCD002
              CLOSE RCD002      OPEN I-O RCD002
           END-IF.
           IF ST-RCD100 = "35"
              CLOSE RCD100      OPEN OUTPUT RCD100
              CLOSE RCD100      OPEN I-O RCD100
           END-IF.
           IF ST-RCD100C = "35"
              CLOSE RCD100C     OPEN OUTPUT RCD100C
              CLOSE RCD100C     OPEN I-O RCD100C
           END-IF.
           IF ST-RCD100E = "35"
              CLOSE RCD100E     OPEN OUTPUT RCD100E
              CLOSE RCD100E     OPEN I-O    RCD100E
           END-IF.
           IF ST-RCD101 = "35"
              CLOSE RCD101      OPEN OUTPUT RCD101
              CLOSE RCD101      OPEN I-O RCD101
           END-IF.
           IF ST-CGD010 = "35"
              CLOSE CGD010      OPEN OUTPUT CGD010
              CLOSE CGD010      OPEN I-O CGD010
           END-IF.
           IF ST-CGD020 = "35"
              CLOSE CGD020      OPEN OUTPUT CGD020
              CLOSE CGD020      OPEN I-O CGD020
           END-IF.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.
           IF ST-MTD020C = "35"
              CLOSE MTD020C     OPEN OUTPUT MTD020C
              CLOSE MTD020C     OPEN I-O MTD020C
           END-IF.
           IF ST-MTD020E = "35"
              CLOSE MTD020E     OPEN OUTPUT MTD020E
              CLOSE MTD020E     OPEN I-O MTD020E
           END-IF.
           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O CHD010
           END-IF.
           IF ST-CHD011 = "35"
              CLOSE CHD011      OPEN OUTPUT CHD011
              CLOSE CHD011      OPEN I-O CHD011
           END-IF.
           IF ST-CHD099 = "35"
              CLOSE CHD099      OPEN OUTPUT CHD099
              CLOSE CHD099      OPEN I-O CHD099
           END-IF.
           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O CRD020
           END-IF.
           IF ST-CRD021 = "35"
              CLOSE CRD021      OPEN OUTPUT CRD021
              CLOSE CRD021      OPEN I-O CRD021
           END-IF.
           IF ST-CRD024 = "35"
              CLOSE CRD024      OPEN OUTPUT CRD024
              CLOSE CRD024      OPEN I-O CRD024
           END-IF.
           IF ST-CHD011 = "35"
              CLOSE CHD011      OPEN OUTPUT CHD011
              CLOSE CHD011      OPEN I-O CHD011
           END-IF.
           CLOSE CHD010 CHD011 CHD099 CRD020 CRD021 CHD010B
                 RCD100E

           OPEN INPUT LOG002 LOG003 LOG004

           IF ST-PAR001 <> "00"
              MOVE "ERRO ABERTURA PAR001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PAR001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD002 <> "00"
              MOVE "ERRO ABERTURA RCD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD030 <> "00"
              MOVE "ERRO ABERTURA CAD030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100C <> "00"
              MOVE "ERRO ABERTURA RCD100C: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100C TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100E <> "00"
              MOVE "ERRO ABERTURA RCD100E: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100E TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020C <> "00"
              MOVE "ERRO ABERTURA MTD020C: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020C TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020E <> "00"
              MOVE "ERRO ABERTURA MTD020E: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020E TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010B <> "00"
              MOVE "ERRO ABERTURA CHD010B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD011 <> "00"
              MOVE "ERRO ABERTURA CHD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD099 <> "00"
              MOVE "ERRO ABERTURA CHD099: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD099 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD021 <> "00"
              MOVE "ERRO ABERTURA CHD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD024 <> "00"
              MOVE "ERRO ABERTURA CRD024: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD024 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE 1                         TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                MOVE "Parâmetros Não Encontrado" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                IF STATUS-TIT-FATURAMENTO-PAR001 IS NOT NUMERIC OR
                   STATUS-TIT-FATURAMENTO-PAR001 = 0
                   MOVE "Status Situação do Faturamento Não Cadastrado"
                     TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
                END-IF

                IF CODRED-FATURAMENTO-PAR001 IS NOT NUMERIC OR
                   CODRED-FATURAMENTO-PAR001 = 0
                   MOVE "Código Reduzido do Faturamento Não Cadastrado"
                     TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
                END-IF
                IF STATUS-REVENDIDO-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-REVENDIDO-PAR001
                END-IF
                MOVE STATUS-REVENDIDO-PAR001 TO WS-STATUS-REVENDIDO
                IF STATUS-ANALISE-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-ANALISE-PAR001
                END-IF
                MOVE STATUS-ANALISE-PAR001   TO WS-STATUS-ANALISE
           END-READ


           CLOSE      RCD100 RCD101 CGD010 MTD020 RCD002 CGD020 CRD024
                      RCD100C MTD020C RCD100E
           OPEN INPUT RCD100 RCD101 CGD010 MTD020 RCD002 CGD020 CRD024
                      CHD010B RCD100C MTD020C RCD100E

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "RCP100"            to logacess-programa
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


           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "SENHA11"     TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY MOVE 0 TO GS-LIBERADO
                   NOT INVALID KEY MOVE 1 TO GS-LIBERADO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW
               WHEN GS-VERIFICA-ALBUM-TRUE
                    PERFORM VERIFICA-ALBUM
               WHEN GS-CONFERE-RECIBO-TRUE
                    PERFORM CONFERE-RECIBO
               WHEN GS-SAVE-FLG-TRUE
                    PERFORM CONFERE-RECIBO
                    PERFORM GRAVA-RECIBO
                    PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI
                    PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUI-CHEQUE-TRUE
                    PERFORM EXCLUI-CHEQUE
                    PERFORM SOMAR-VALOR
               WHEN GS-INCLUI-CHEQUE-TRUE
                    PERFORM GRAVAR-CHEQUE-WORK
                    PERFORM SOMAR-VALOR
               WHEN GS-ADICIONA-CHEQUE-TRUE
                    PERFORM GRAVAR-CHEQUE-WORK
                    PERFORM ADICIONA-MES-CHEQUE
                    PERFORM SOMAR-VALOR
               WHEN GS-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
      *        WHEN GS-CARREGA-LIST-BOX-TRUE
      *             MOVE DATA-MOVTO-I     TO DATA-MOVTO-L101
      *             MOVE GS-LINDET(1: 3)  TO SEQ-L101
      *             PERFORM CARREGAR-DADOS
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LE-VENDEDOR
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM CHAMAR-POP-UP
               WHEN GS-LE-CARTAO-TRUE
                    PERFORM LE-CARTAO
               WHEN GS-POPUP-CARTAO-TRUE
                    PERFORM POPUP-CARTAO
               WHEN GS-EXCLUIR-PARCELAS-TRUE
                    PERFORM EXCLUIR-PARCELAS
                    PERFORM SOMAR-VALOR
               WHEN GS-CARREGA-PARCELA-TRUE
                    PERFORM CARREGAR-PARCELA
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-CARREGA-COMISSAO-TRUE
                    PERFORM CARREGAR-COMISSAO
               WHEN GS-IMPRIMIR-PARCELAS-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIMIR-PARCELAS
                    END-IF
               WHEN GS-LER-BANCO-TRUE
                    PERFORM LER-BANCO
               WHEN GS-VERIFICA-CONTRATO-TRUE
                    PERFORM VERIFICA-CONTRATO
               WHEN GS-VERIFICA-PERMISSAO-TRUE
                    MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
                    MOVE "SENHA59"          TO PROGRAMA-CA004
                    READ CAD004 INVALID KEY
                         DISABLE-OBJECT EF1
                    NOT INVALID KEY
                         ENABLE-OBJECT EF1
                    END-READ
               WHEN GS-TRATAR-EVENTO-TRUE
                    PERFORM TRATAR-EVENTO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA59"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT EF1
           NOT INVALID KEY
                ENABLE-OBJECT EF1.


       VERIFICA-CONTRATO SECTION.
           MOVE GS-NR-ALBUM            TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE "Número do Contrato Não Cadastrado no (COP040)"
                  TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                EVALUATE STATUS-CO40
                    WHEN WS-STATUS-REVENDIDO
                         MOVE "Contrato com STATUS Revendido" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                    WHEN WS-STATUS-ANALISE
                         MOVE "Contrato com STATUS Analise" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                    WHEN OTHER
                         MOVE 0 TO GS-FLAG-CRITICA
                END-EVALUATE.

       TRATAR-EVENTO SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34123 perform chamar-colunas-favo
           END-EVALUATE.

       chamar-colunas-favo section.
           move "listview-rcp100-confere" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo.
       chamar-colunas-favo-fim.
           exit.

       CENTRALIZAR SECTION.
          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal
          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win4 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal
          invoke janelaPrincipal "CentralizarNoDesktop".


          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal
          invoke janelaPrincipal "CentralizarNoDesktop".

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Item" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Produto" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Montagem" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Recibo" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Diferença" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Vlr Escala" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Acima de" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"% Desconto" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Total Escala" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke gs-acp-listview "gridLines"
          invoke gs-acp-listview "noBorder".
       CRIAR-LISTVIEW-PRODUTOS-FIM.
           EXIT.

       mostrar-fonte-favo section.
           move "listview-rcp100-confere" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-rcp100-confere" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.


      *----------------------------------------------------------------
       LER-BANCO SECTION.
           MOVE GS-BANCO-CH TO CODIGO-CAD30
           READ CAD030 INVALID KEY
               MOVE "Código do Banco Inválido" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM.

       IMPRIMIR-PARCELAS SECTION.
           OPEN OUTPUT RELAT
           MOVE ALL "-"        TO GS-LINHA-COMISSAO(1:43)
           WRITE REG-RELAT FROM GS-LINHA-COMISSAO
           MOVE SPACES         TO GS-LINHA-COMISSAO
           MOVE "  VENCTO  "   TO GS-LINHA-COMISSAO(1:10)
           MOVE "% COMISSÃO"   TO GS-LINHA-COMISSAO(22:6)
           MOVE "VLR COMISSÃO" TO GS-LINHA-COMISSAO(33:10)
           WRITE REG-RELAT FROM GS-LINHA-COMISSAO
           MOVE ALL "-"        TO GS-LINHA-COMISSAO(1:43)
           WRITE REG-RELAT FROM GS-LINHA-COMISSAO
           INITIALIZE REG-RCD101
           STRING GS-NR-ALBUM GS-NR-FORM INTO AUX-ALBUM
           MOVE AUX-ALBUM TO ALBUM-REC1
           START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
               MOVE "10" TO ST-RCD101.
           PERFORM UNTIL ST-RCD101 = "10"
               READ RCD101 NEXT AT END
                   MOVE "10" TO ST-RCD101
               NOT AT END
                   IF AUX-ALBUM <> ALBUM-REC1
                      MOVE "10" TO ST-RCD101
                   ELSE
                      MOVE SPACES TO GS-LINHA-COMISSAO
                      MOVE VENCTO-REC1(1:4) TO AUX-ANO
                      MOVE VENCTO-REC1(5:2) TO AUX-MES
                      MOVE VENCTO-REC1(7:2) TO AUX-DIA
                      STRING AUX-DIA "/" AUX-MES "/" AUX-ANO INTO
                             GS-LINHA-COMISSAO(1:10)

                      MOVE COMIS-PARC-REC1 TO MASC-PERC
                      MOVE MASC-PERC       TO GS-LINHA-COMISSAO(18:6)

                      COMPUTE MASC-VALOR = VALOR-REC1 * COMIS-PARC-REC1
                      MOVE MASC-VALOR      TO GS-LINHA-COMISSAO(31:10)

                      COMPUTE GS-TOTAL-COMISSAO ROUNDED =
                              GS-TOTAL-COMISSAO + (VALOR-REC1 *
                                                   COMIS-PARC-REC1)
                      WRITE REG-RELAT FROM GS-LINHA-COMISSAO
                   END-IF
               END-READ
           END-PERFORM
           MOVE SPACE TO GS-LINHA-COMISSAO
           STRING "TOTAL COMISSÃO . . ." INTO GS-LINHA-COMISSAO(1:30)
           MOVE GS-TOTAL-COMISSAO TO MASC-VALOR
           MOVE MASC-VALOR               TO GS-LINHA-COMISSAO(31:10)
           WRITE REG-RELAT FROM GS-LINHA-COMISSAO
           CLOSE RELAT.

      *----------------------------------------------------------------
       CARREGAR-COMISSAO SECTION.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "SENHA27"     TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                MOVE "DESABILITA-IMP" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                MOVE "HABILITA-IMP" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM.


           INITIALIZE REG-RCD101 GS-TOTAL-COMISSAO
           STRING GS-NR-ALBUM GS-NR-FORM INTO AUX-ALBUM
           MOVE AUX-ALBUM TO ALBUM-REC1
           START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
               MOVE "10" TO ST-RCD101.
           PERFORM UNTIL ST-RCD101 = "10"
               READ RCD101 NEXT AT END
                   MOVE "10" TO ST-RCD101
               NOT AT END
                   IF AUX-ALBUM <> ALBUM-REC1
                      MOVE "10" TO ST-RCD101
                   ELSE
                      MOVE SPACES TO GS-LINHA-COMISSAO
                      MOVE VENCTO-REC1(1:4) TO AUX-ANO
                      MOVE VENCTO-REC1(5:2) TO AUX-MES
                      MOVE VENCTO-REC1(7:2) TO AUX-DIA
                      STRING AUX-DIA "/" AUX-MES "/" AUX-ANO INTO
                             GS-LINHA-COMISSAO(1:10)

                      MOVE COMIS-PARC-REC1 TO MASC-PERC
                      MOVE MASC-PERC       TO GS-LINHA-COMISSAO(18:6)

                      COMPUTE MASC-VALOR = VALOR-REC1 * COMIS-PARC-REC1
                      MOVE MASC-VALOR      TO GS-LINHA-COMISSAO(31:10)

                      COMPUTE GS-TOTAL-COMISSAO ROUNDED =
                              GS-TOTAL-COMISSAO + (VALOR-REC1 *
                                                   COMIS-PARC-REC1)

                      MOVE "INSERIR-LB2" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.

      *----------------------------------------------------------------
       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR TO PORTADOR
           READ CAD018 INVALID KEY
               MOVE "Portador Inválido" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
               MOVE SPACES    TO GS-DESC-PORTADOR
           NOT INVALID KEY
               MOVE NOME-PORT TO GS-DESC-PORTADOR.

      *----------------------------------------------------------------
       POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-PORTADOR.
           MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR.
           PERFORM LER-PORTADOR.

      *----------------------------------------------------------------
       CARREGAR-PARCELA SECTION.
           IF GS-LINDET(79:08) <> "00000000"
              MOVE "Parcela Não Pode ser Alterada, pois já foi Baixada"
              TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              STRING GS-LINDET(1:2) GS-LINDET(4:2) GS-LINDET(7:4) INTO
              GS-VENCTO-CH
              MOVE GS-LINDET(13:13)             TO VALOR-EXTENSO
              STRING VALOR-N1 VALOR-N2 VALOR-N3 INTO VALOR-N11
              MOVE VALOR-N4                     TO   VALOR-N22
              MOVE VALOR-EXTENSO2               TO   GS-VALOR-CH
              MOVE GS-LINDET(28:6)              TO   GS-NR-CHEQUE
              MOVE GS-LINDET(42:3)              TO   GS-BANCO-CH
              MOVE GS-LINDET(49:2)              TO   GS-PARCELA
              MOVE GS-LINDET(52:2)              TO   GS-QT-PARCELA
              MOVE GS-LINDET(60:18)             TO   GS-TIPO-CH.

      *----------------------------------------------------------------
       EXCLUIR-PARCELAS SECTION.
              MOVE COD-USUARIO-W TO COD-USUARIO-CA004
              MOVE "SENHA18"     TO PROGRAMA-CA004
              READ CAD004 INVALID KEY
                 MOVE "Usuário sem Permissão para Exclusão das Parcelas"
                 TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                 MOVE "Você quer realmente Excluir as Parcelas?" TO
                 MENSAGEM
                 MOVE "Q" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
                 IF RESP-MSG = "S"
                 CLOSE      RCD101 LOG003
                 OPEN I-O   RCD101 LOG003
                 INITIALIZE REG-RCD101
                 STRING GS-NR-ALBUM GS-NR-FORM INTO AUX-CONTRATO
                 MOVE AUX-CONTRATO    TO ALBUM-REC1
                  START RCD101 KEY IS NOT LESS ALBUM-REC1 INVALID KEY
                       MOVE "10" TO ST-RCD101
                  END-START
                  PERFORM UNTIL ST-RCD101 = "10"
                       READ RCD101 NEXT AT END
                           MOVE "10" TO ST-RCD101
                       NOT AT END
                           IF ALBUM-REC1 <> AUX-CONTRATO
                               MOVE "10" TO ST-RCD101
                           ELSE
                               DELETE RCD101 NOT INVALID KEY
                                      MOVE USUARIO-W   TO LOG3-USUARIO
                                      MOVE FUNCTION CURRENT-DATE
                                                       TO WS-DATA-SYS
                                      MOVE WS-DATA-CPU TO LOG3-DATA
                                      ACCEPT WS-HORA-SYS FROM TIME
                                      MOVE WS-HORA-SYS TO LOG3-HORAS
                                      MOVE "E"         TO LOG3-OPERACAO
                                      MOVE "RCD101"    TO LOG3-ARQUIVO
                                      MOVE "RCP100"    TO LOG3-PROGRAMA
                                      MOVE REG-RCD101  TO LOG3-REGISTRO
                                      WRITE REG-LOG003
                                      END-WRITE
                               END-DELETE
                           END-IF
                       END-READ
                  END-PERFORM
                  INITIALIZE REG-WORK
                  START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
                       MOVE "10" TO ST-WORK
                  END-START
                  PERFORM UNTIL ST-WORK = "10"
                       READ WORK NEXT AT END
                           MOVE "10" TO ST-WORK
                       NOT AT END
                           DELETE WORK
                       END-READ
                  END-PERFORM
                  MOVE ZEROS TO GS-TOTAL-REC
                                GS-TOTAL-DESCCOM
                  CLOSE      RCD101 LOG003
                  OPEN INPUT RCD101 LOG003
                  PERFORM CARREGA-CHEQUES
                  PERFORM SET-UP-FOR-REFRESH-SCREEN.
      *----------------------------------------------------------------
       LE-CARTAO SECTION.
           MOVE GS-CARTAO TO CODIGO-CG20
           READ CGD020 INVALID KEY
               INITIALIZE REG-CGD020.
           MOVE CODIGO-CG20              TO GS-CARTAO
           MOVE NOME-CG20                TO GS-DESC-CARTAO
           MOVE TAXA-CREDITO-CG20        TO GS-TAXA-ADMINIST-CREDITO
           MOVE TAXA-PARCELA-CG20        TO GS-TAXA-ADMINIST-PARCELA.

      *----------------------------------------------------------------
       POPUP-CARTAO SECTION.
           CALL   "CGP020T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP020T"
           MOVE PASSAR-STRING-1(1:30)  TO GS-DESC-CARTAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-CARTAO
           PERFORM LE-CARTAO.

      *----------------------------------------------------------------
       CHAMAR-POP-UP SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-VENDEDOR
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.
       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR          TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-VENDEDOR.
      *--------------------------------------------------------------
       VERIFICA-ALBUM SECTION.
           CLOSE WORK.  OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE ZEROS TO TOT-PARCELA GS-FLAG-CRITICA.



           MOVE GS-NR-ALBUM        TO ALBUM-REC(1: 4)
           MOVE GS-NR-FORM         TO ALBUM-REC(5: 4)
           READ RCD100 INVALID KEY
                MOVE GS-NR-ALBUM   TO NR-ALBUM-W
                MOVE GS-NR-FORM    TO NR-FORM-W
                MOVE GS-VENDEDOR   TO VENDEDOR-W
                PERFORM LIMPAR-DADOS
                MOVE NR-ALBUM-W    TO GS-NR-ALBUM ALBUM-REC(1: 4)
                MOVE NR-FORM-W     TO GS-NR-FORM  ALBUM-REC(5: 4)
                MOVE 1             TO GS-VISITA
                MOVE VENDEDOR-W    TO GS-VENDEDOR
                MOVE 1 TO GS-TIPO-GRAVACAO
                MOVE GS-NR-ALBUM   TO CONTRATO-MT19
                MOVE GS-NR-FORM    TO SEQ-MT19
                READ MTD019 INVALID KEY MOVE SPACES TO NOME-FORM-MT19
                  NOT INVALID KEY
                     MOVE NOME-FORM-MT19   TO GS-NOME
                     MOVE CIDADE-MT19      TO CIDADE
                     READ CAD010 INVALID KEY CONTINUE
                       NOT INVALID KEY
                           MOVE NOME-CID   TO GS-CIDADE
                     END-READ
                END-READ
                MOVE COD-USUARIO-W TO COD-USUARIO-CA004
                MOVE "SENHA11"     TO PROGRAMA-CA004
                READ CAD004 INVALID KEY
                    MOVE COD-USUARIO-W TO COD-USUARIO-CA004
                    MOVE "SENHA22"     TO PROGRAMA-CA004
                    READ CAD004 INVALID KEY
                        MOVE "Usuário sem Permissão Para Inclusão" to
                        mensagem
                        move "C" to tipo-msg
                        perform exibir-mensagem
                    END-READ
                NOT INVALID KEY
                      MOVE "Usuário com Permissão apenas para Consulta"
                      TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                      MOVE 1 TO GS-FLAG-CRITICA
                END-READ

      *         CONTINUE
           NOT INVALID KEY
               MOVE COD-USUARIO-W TO COD-USUARIO-CA004
               MOVE "SENHA22"     TO PROGRAMA-CA004
               READ CAD004 INVALID KEY
                    MOVE COD-USUARIO-W TO COD-USUARIO-CA004
                    MOVE "SENHA11"     TO PROGRAMA-CA004
                    READ CAD004 INVALID KEY
                         MOVE "Usuário sem Permissão para Alteração" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                   NOT INVALID KEY
                         MOVE 1 TO GS-FLAG-CRITICA
                         MOVE SPACES TO GS-NOME GS-CIDADE
                         PERFORM CARREGAR-DADOS
                   END-READ
               NOT INVALID KEY
                    MOVE 2      TO GS-TIPO-GRAVACAO
                    MOVE SPACES TO GS-NOME GS-CIDADE
                    PERFORM CARREGAR-DADOS
               END-READ
           END-READ.

           MOVE ALBUM-REC        TO ALBUM-MTG.
           READ MTD020 INVALID KEY
               MOVE "Não Existe Lançamento na Planilha de Montagem (MTP0
      -             "20)" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               IF NAO-GEROU-ALBUM-MTG = 1
                  MOVE "Não Existem Produtos para esse Cliente" TO
                  MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
               ELSE
                  IF FOGO-MTG = 7
                     MOVE SPACES            TO MENSAGEM
                     STRING "Álbum Vendido pelo Vanderlei em "
                            DATA-FOGO-MTG(7:2) "/"
                            DATA-FOGO-MTG(5:2) "/"
                            DATA-FOGO-MTG(1:4) INTO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                  ELSE
                     MOVE QT-ESTOJO-MTG     TO GS-ESTOJO-MTG
                     MOVE QT-ENCADER-MTG    TO GS-ENCADERN-MTG
                     MOVE QT-BOOK-MTG       TO GS-BOOK-MTG
                     MOVE QT-FOLHAS-MTG     TO GS-FOLHA-MTG
                     MOVE QT-FOTOS-MTG      TO GS-FOTO-MTG
                     MOVE QT-FITAS-MTG      TO GS-FITA-MTG
                     MOVE QT-DVD-MTG        TO GS-DVD-MTG
                     MOVE QT-POSTER-MTG     TO GS-POSTER-MTG
                     MOVE QT-PORTA-FITA-MTG TO GS-PFITA-MTG
                     MOVE QT-PORTA-DVD-MTG  TO GS-PORTA-DVD-MTG
                     MOVE QT-FOTO-CD-MTG    TO GS-FOTO-CD-MTG
                     MOVE QT-MOLDURA-MTG    TO GS-MOLDURA-MTG
                     MOVE VISITA-MTG        TO GS-COD-COMISS

                     MOVE ALBUM-MTG         TO ALBUM-MTG-C
                     READ MTD020C INVALID KEY
                          INITIALIZE REG-MTD020C
                     END-READ
                     MOVE QT-PORTA-RETRATO-MTG-C TO
                          GS-PORTA-RETRATO-MTG
                     MOVE QT-PENDRIVE-MTG-C      TO
                          GS-PENDRIVE-MTG
                     MOVE QT-VIDEO-HD-MTG-C      TO
                          GS-VIDEO-HD-MTG
                     MOVE QT-REVISTA-MTG-C       TO
                          GS-REVISTA-MTG
                     MOVE QT-CALENDARIO-MTG-C    TO
                          GS-CALENDARIO-MTG

                     MOVE CONTRATO-MTG           TO CONTRATO-MTG-E
                     READ MTD020E INVALID KEY
                          INITIALIZE REG-MTD020E
                     END-READ

                     MOVE "N" TO ACHEI

                     INVOKE GS-ACP-LISTVIEW "DeleteAll"

                     INITIALIZE REG-RCD100E
                     MOVE ALBUM-REC TO ALBUM-REC-E
                     START RCD100E KEY IS NOT LESS ALBUM-REC-E
                                                             INVALID KEY
                           MOVE "10" TO ST-RCD100E
                     END-START
                     PERFORM UNTIL ST-RCD100E = "10"
                           READ RCD100E NEXT AT END
                                MOVE "10" TO ST-RCD100E
                           NOT AT END
                                IF ALBUM-REC <> ALBUM-REC-E
                                   MOVE "10" TO ST-RCD100E
                                ELSE
                                   MOVE "S" TO ACHEI
                                   PERFORM INSERIR-LISTVIEW-RCD100E
                                END-IF
                           END-READ
                     END-PERFORM

                     IF ACHEI = "N"
                        PERFORM INSERIR-LISTVIEW
                     END-IF

                  END-IF
               END-IF
           END-READ

           COMPUTE GS-TOTAL-ESCALA-LIQUIDO-JR = GS-TOTAL-ESCALA-LIQUIDO
                                              + GS-TOTAL-JUROS.

       INSERIR-LISTVIEW-RCD100E SECTION.
           INITIALIZE INDICE
           INVOKE GS-ACP-LISTVIEW "ADICIONARITEM" RETURNING WSITEM

      *>Item
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING ITEM-REC-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO


           MOVE PRODUTO-REC-E                    TO QUAL-PRODUTO
           MOVE MONTAGEM-REC-E                   TO QTDE-MONTAGEM
           MOVE RECIBO-REC-E                     TO QTDE-RECIBO
           MOVE DIFERENCA-REC-E                  TO QTDE-DIFERENCA
           MOVE PRECO-ESCALA-RCD-E               TO VLR-ESCALA
           MOVE QTDE-ACIMA-DE-RCD-E              TO ACIMA-DE
           MOVE PERC-DESC-FORM-RCD-E             TO PERC-DESC-FORMANDO
           MOVE PERC-DESC-COMISSAO-RCD-E         TO PERC-DESC-COMISSAO
           MOVE TOTAL-ESCALA-RCD-E               TO TOT-ESCALA

           MOVE VLR-ESCALA                       TO MASC-ESCALA
           MOVE QTDE-RECIBO                      TO MASC-RECIBO
           MOVE QTDE-DIFERENCA                   TO MASC-DIFERENCA
           MOVE QTDE-MONTAGEM                    TO MASC-MONTAGEM
           MOVE ACIMA-DE                         TO MASC-ACIMA-DE
           MOVE PERC-DESC-FORMANDO               TO MASC-PERC1
           MOVE PERC-DESC-COMISSAO               TO MASC-PERC2
           MOVE TOT-ESCALA                       TO MASC-TESCALA

           MOVE TAXA-JUROS-RCD-E                 TO GS-ACP-TAXA-JUROS
           MOVE CARENCIA-JUROS-RCD-E             TO GS-ACP-CARENCIA
           MOVE TAXA-DESCONTO-RCD-E              TO GS-ACP-TAXA-DESCONTO


           COMPUTE GS-TOTAL-ESCALA-BRUTO = GS-TOTAL-ESCALA-BRUTO +
                                   (RECIBO-REC-E * PRECO-ESCALA-RCD-E)

           ADD TOT-ESCALA          TO GS-TOTAL-ESCALA-LIQUIDO


      *>Produto
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING QUAL-PRODUTO X"00" DELIMITED BY "  " INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Montagem
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING MASC-MONTAGEM X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Recibo
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING MASC-RECIBO X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Diferenca
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING MASC-DIFERENCA X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor escala
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING MASC-ESCALA X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Acima de
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING MASC-ACIMA-DE X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>%Desconto
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           IF GS-MEMBRO-COMISSAO = 1
              STRING MASC-PERC1 " % + " MASC-PERC2 " %" X"00"
                INTO WSTEXTO
           ELSE
              STRING MASC-PERC1 " %" X"00" INTO WSTEXTO
           END-IF
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Total Escala
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING MASC-TESCALA X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

           perform mostrar-fonte-favo
           perform mostrar-colunas-favo.

       INSERIR-LISTVIEW SECTION.
           MOVE ZEROS TO QUAL-ITEM

           PERFORM UNTIL QUAL-ITEM = 17
               ADD 1  TO QUAL-ITEM

               INITIALIZE INDICE
               INVOKE GS-ACP-LISTVIEW "ADICIONARITEM"
                                       RETURNING WSITEM

      *>Item
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING QUAL-ITEM X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

               EVALUATE  QUAL-ITEM
                   WHEN 1  MOVE "Encadernação"         TO QUAL-PRODUTO
                           MOVE QT-ENCADER-MTG         TO QTDE-MONTAGEM
                           MOVE PRECO-ENCADER-MTG-E    TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-ENCADER  TO ACIMA-DE
                   WHEN 2  MOVE "Estojo"               TO QUAL-PRODUTO
                           MOVE QT-ESTOJO-MTG          TO QTDE-MONTAGEM
                           MOVE PRECO-ESTOJO-MTG-E     TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-ESTOJO   TO ACIMA-DE
                   WHEN 3  MOVE "Folha"                TO QUAL-PRODUTO
                           MOVE QT-FOLHAS-MTG          TO QTDE-MONTAGEM
                           MOVE PRECO-FOLHA-MTG-E      TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-FOLHA    TO ACIMA-DE
                   WHEN 4  MOVE "Foto"                 TO QUAL-PRODUTO
                           MOVE QT-FOTOS-MTG           TO QTDE-MONTAGEM
                           MOVE PRECO-FOTO-MTG-E       TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-FOTO     TO ACIMA-DE
                   WHEN 5  MOVE "Fita"                 TO QUAL-PRODUTO
                           MOVE QT-FITAS-MTG           TO QTDE-MONTAGEM
                           MOVE PRECO-FITA-MTG-E       TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-FITA     TO ACIMA-DE
                   WHEN 6  MOVE "Porta Fita"           TO QUAL-PRODUTO
                           MOVE QT-PORTA-FITA-MTG      TO QTDE-MONTAGEM
                           MOVE PRECO-PORTA-FITA-MTG-E TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-PT-FITA  TO ACIMA-DE
                   WHEN 7  MOVE "Poster"               TO QUAL-PRODUTO
                           MOVE QT-POSTER-MTG          TO QTDE-MONTAGEM
                           MOVE PRECO-POSTER-MTG-E     TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-POSTER   TO ACIMA-DE
                   WHEN 8  MOVE "DVD"                  TO QUAL-PRODUTO
                           MOVE QT-DVD-MTG             TO QTDE-MONTAGEM
                           MOVE PRECO-DVD-MTG-E        TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-DVD      TO ACIMA-DE
                   WHEN 9  MOVE "Porta DVD"            TO QUAL-PRODUTO
                           MOVE QT-PORTA-DVD-MTG       TO QTDE-MONTAGEM
                           MOVE PRECO-PORTA-DVD-MTG-E  TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-PT-DVD   TO ACIMA-DE
                   WHEN 10 MOVE "Foto CD"              TO QUAL-PRODUTO
                           MOVE QT-FOTO-CD-MTG         TO QTDE-MONTAGEM
                           MOVE PRECO-FOTO-CD-MTG-E    TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-FT-CD    TO ACIMA-DE
                   WHEN 11 MOVE "Moldura"              TO QUAL-PRODUTO
                           MOVE QT-MOLDURA-MTG         TO QTDE-MONTAGEM
                           MOVE PRECO-MOLDURA-MTG-E    TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-MOLDURA  TO ACIMA-DE
                   WHEN 12 MOVE "Book"                 TO QUAL-PRODUTO
                           MOVE QT-BOOK-MTG            TO QTDE-MONTAGEM
                           MOVE PRECO-BOOK-MTG-E       TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-BOOK     TO ACIMA-DE
                   WHEN 13 MOVE "Porta Retrato"        TO QUAL-PRODUTO
                           MOVE QT-PORTA-RETRATO-MTG-C TO QTDE-MONTAGEM
                           MOVE PRECO-PORTA-RETRATO-MTG-E  TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-PT-RET   TO ACIMA-DE
                   WHEN 14 MOVE "Pendrive"             TO QUAL-PRODUTO
                           MOVE QT-PENDRIVE-MTG-C      TO QTDE-MONTAGEM
                           MOVE PRECO-PENDRIVE-MTG-E   TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-PENDRIVE TO ACIMA-DE
                   WHEN 15 MOVE "Video HD"             TO QUAL-PRODUTO
                           MOVE QT-VIDEO-HD-MTG-C      TO QTDE-MONTAGEM
                           MOVE PRECO-VIDEO-HD-MTG-E   TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-VIDEO-HD TO ACIMA-DE
                   WHEN 16 MOVE "Revista"              TO QUAL-PRODUTO
                           MOVE QT-REVISTA-MTG-C       TO QTDE-MONTAGEM
                           MOVE PRECO-REVISTA-MTG-E    TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-REVISTA  TO ACIMA-DE
                   WHEN 17 MOVE "Calendário"           TO QUAL-PRODUTO
                           MOVE QT-CALENDARIO-MTG-C    TO QTDE-MONTAGEM
                           MOVE PRECO-CALENDARIO-MTG-E TO VLR-ESCALA
                           MOVE QTDE-ACIMA-DE-CALEND   TO ACIMA-DE
               END-EVALUATE

               MOVE ZEROS          TO PERC-DESC-FORMANDO
               MOVE ZEROS          TO PERC-DESC-COMISSAO
               MOVE ZEROS          TO TOT-ESCALA
               MOVE ZEROS          TO QTDE-RECIBO
               MOVE ZEROS          TO QTDE-DIFERENCA

               MOVE VLR-ESCALA     TO MASC-ESCALA
               MOVE QTDE-RECIBO    TO MASC-RECIBO
               MOVE QTDE-DIFERENCA TO MASC-DIFERENCA
               MOVE QTDE-MONTAGEM  TO MASC-MONTAGEM
               MOVE ACIMA-DE       TO MASC-ACIMA-DE
               MOVE ZEROS          TO MASC-PERC1
               MOVE ZEROS          TO MASC-PERC2
               MOVE ZEROS          TO MASC-TESCALA

      *>Produto
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING QUAL-PRODUTO X"00" DELIMITED BY "  " INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Montagem
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-MONTAGEM X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Recibo
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-RECIBO X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Diferenca
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-DIFERENCA X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor escala
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-ESCALA X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Acima de
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-ACIMA-DE X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>%Desconto
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               IF GS-MEMBRO-COMISSAO = 1
                  STRING MASC-PERC1 " %" " + " MASC-PERC2 " %"
                         X"00" INTO WSTEXTO
               ELSE
                  STRING MASC-PERC1 " %" X"00" INTO WSTEXTO
               END-IF
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Total Escala
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-TESCALA X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
           END-PERFORM

           perform mostrar-fonte-favo
           perform mostrar-colunas-favo.

       CARREGAR-DADOS SECTION.
           MOVE DATA-MOVTO-REC       TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO GS-DATA-MOVTO
           MOVE TAXA-REC             TO GS-TAXA
           MOVE DATAVEN-REC          TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO GS-DATA-VENDA
           MOVE VISITA-REC           TO GS-VISITA
           MOVE ALBUM-REC            TO ALBUM-MTG
           READ MTD020 INVALID KEY MOVE ZEROS TO VISITA-MTG.
           MOVE VISITA-MTG           TO GS-COD-COMISS
           IF FOGO-MTG = 9 MOVE 8 TO FOGO-MTG
           ELSE MOVE 1 TO FOGO-MTG.
           IF PEDIDO-EXTRA-REC IS NOT NUMERIC
              MOVE 0                 TO PEDIDO-EXTRA-REC
           END-IF
           IF MEMBRO-COMISSAO-REC IS NOT NUMERIC
              MOVE 0                 TO MEMBRO-COMISSAO-REC
           END-IF
           MOVE MEMBRO-COMISSAO-REC  TO GS-MEMBRO-COMISSAO
           MOVE PEDIDO-EXTRA-REC     TO GS-PEDIDO-EXTRA
           MOVE QENCADER-REC         TO GS-QT-ENCADERN
           MOVE QFOTOS-REC           TO GS-QT-FOTO
           MOVE QFOLHAS-REC          TO GS-QT-FOLHA
           MOVE QFITAS-REC           TO GS-QT-FITA
           MOVE QDVD-REC             TO GS-QT-DVD
           MOVE QCOMISSAO-REC        TO GS-QT-FOTO-COMISSAO
           MOVE QAVULSAS-REC         TO GS-QT-AVULSA
           MOVE QPOSTER-REC          TO GS-QT-POSTER
           MOVE QPFITA-REC           TO GS-QT-PFITA
           MOVE QCOBERTURA-REC       TO GS-QT-COBERTURA
           MOVE QABERTURA-REC        TO GS-QT-ABERTURA
           MOVE QESTOJO-REC          TO GS-QT-ESTOJO
           MOVE QT-COBERTURA-DVD-REC TO GS-QT-COBERTURA-DVD
           MOVE QPORTA-DVD-REC       TO GS-QT-PORTA-DVD
           MOVE QMOLDURA-REC         TO GS-QT-MOLDURA
           MOVE QFOTO-CD-REC         TO GS-QT-FOTO-CD
           MOVE QBOOK-REC            TO GS-QT-BOOK
           MOVE TOTAL-REC            TO GS-TOTAL-REC
           MOVE QFOTOS-EXTRA1-REC    TO GS-EXTRA-FOTOS1
           MOVE QFOTOS-EXTRA2-REC    TO GS-EXTRA-FOTOS2
           MOVE POSTER-ABERTURA-REC  TO GS-POSTER-ABERTURA
           MOVE POSTER-XEROX-REC     TO GS-POSTER-XEROX
           MOVE EXTRA-ENVIADO1-REC   TO GS-EXTRA-ENVIADO1
           MOVE EXTRA-ENVIADO2-REC   TO GS-EXTRA-ENVIADO2

           MOVE ALBUM-REC            TO ALBUM-REC-C
           READ RCD100C INVALID KEY
                INITIALIZE REG-RCD100C
           END-READ
           MOVE QPORTA-RETRATO-REC-C TO GS-QT-PORTA-RETRATO
           MOVE QPENDRIVE-REC-C      TO GS-QT-PENDRIVE
           MOVE QVIDEO-HD-REC-C      TO GS-QT-VIDEO-HD
           MOVE QREVISTA-REC-C       TO GS-QT-REVISTA
           MOVE QCALENDARIO-REC-C    TO GS-QT-CALENDARIO

           IF PED-EXT-BOOK-C IS NOT NUMERIC
              MOVE 0 TO PED-EXT-BOOK-C
           END-IF
           MOVE PED-EXT-BOOK-C       TO GS-PED-EXT-BOOK

           IF PED-EXT-PORTA-RET-C IS NOT NUMERIC
              MOVE 0 TO PED-EXT-PORTA-RET-C
           END-IF
           MOVE PED-EXT-PORTA-RET-C  TO GS-PED-EXT-PORTA-RET

           IF PED-EXT-PENDRIVE-C IS NOT NUMERIC
              MOVE 0 TO PED-EXT-PENDRIVE-C
           END-IF
           MOVE PED-EXT-PENDRIVE-C   TO GS-PED-EXT-PENDRIVE

           IF PED-EXT-VIDEO-HD-C IS NOT NUMERIC
              MOVE 0 TO PED-EXT-VIDEO-HD-C
           END-IF
           MOVE PED-EXT-VIDEO-HD-C     TO GS-PED-EXT-VIDEO-HD

           IF PED-EXT-REVISTA-C IS NOT NUMERIC
              MOVE 0 TO PED-EXT-REVISTA-C
           END-IF
           MOVE PED-EXT-REVISTA-C      TO GS-PED-EXT-REVISTA

           IF PED-EXT-CALEND-C IS NOT NUMERIC
              MOVE 0 TO PED-EXT-CALEND-C
           END-IF
           MOVE PED-EXT-CALEND-C       TO GS-PED-EXT-CALENDARIO

           REFRESH-OBJECT WIN4


           EVALUATE GS-EXTRA-ENVIADO1
               WHEN "S"   MOVE "SETAR-ENVIADO1" TO DS-PROCEDURE
               WHEN "N"   MOVE "SETAR-ENVIAR1"  TO DS-PROCEDURE
               WHEN OTHER MOVE "DESMARCAR1"     TO DS-PROCEDURE
           END-EVALUATE
           PERFORM CALL-DIALOG-SYSTEM

           EVALUATE GS-EXTRA-ENVIADO2
               WHEN "S"   MOVE "SETAR-ENVIADO2" TO DS-PROCEDURE
               WHEN "N"   MOVE "SETAR-ENVIAR2"  TO DS-PROCEDURE
               WHEN OTHER MOVE "DESMARCAR2"     TO DS-PROCEDURE
           END-EVALUATE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE VENDEDOR-REC         TO GS-VENDEDOR CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01            TO GS-NOME-VENDEDOR.
           PERFORM GRAVA-WORK.
           PERFORM CARREGA-CHEQUES
           PERFORM SOMAR-VALOR.
       GRAVA-WORK SECTION.
           MOVE ZEROS TO REG-RCD101

           MOVE ALBUM-REC     TO ALBUM-REC1
           COMPUTE ALBUM-REC1 = ALBUM-REC1 - 1

           MOVE ALL "9"       TO VENCTO-REC1 BANCO-REC1 NUMERO-REC1.

           START RCD101 KEY IS GREATER THAN CHAVE-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101.

           PERFORM UNTIL ST-RCD101 = "10"
             READ RCD101 NEXT RECORD AT END MOVE "10" TO ST-RCD101
              NOT AT END
                MOVE ALBUM-REC1 TO AUX-ALBUM-REC

                IF ALBUM-REC <> AUX-ALBUM-REC
                   MOVE "10" TO ST-RCD101
                ELSE
                 MOVE VENCTO-REC1     TO VENCTO-WK
                 MOVE VALOR-REC1      TO VALOR-WK
                 MOVE DTA-BAIXA-REC1  TO DATA-BAIXA-WK
                 MOVE NUMERO-REC1     TO NR-CHEQUE-WK
                 MOVE BANCO-REC1      TO BANCO-WK
                 MOVE PARCELA-REC1    TO PARCELA-WK
                 MOVE QT-PARCELA-REC1 TO QT-PARCELA-WK
                 MOVE COMIS-PARC-REC1 TO COMIS-PARC-WK
                 MOVE CARTAO-CRED-REC1 TO CARTAO-WK CODIGO-CG20
                 READ CGD020 INVALID KEY
                      INITIALIZE REG-CGD020
                 END-READ
                 MOVE NOME-CG20 TO DESC-CARTAO-WK
                 MOVE TAXA-ADMINIST-CREDITO-REC1
                                     TO TAXA-ADMINIST-CREDITO-WK
                 MOVE TAXA-ADMINIST-PARCELA-REC1
                                     TO TAXA-ADMINIST-PARCELA-WK
                 EVALUATE TIPO-REC1
                   WHEN 1 MOVE "1-Cheque"        TO TIPO-WK
                   WHEN 2 MOVE "2-Moeda "        TO TIPO-WK
                   WHEN 3 MOVE "3-Antecip."      TO TIPO-WK
                   WHEN 4 MOVE "4-Boleto/Prom"   TO TIPO-WK
                   when 5 move "5-Deb.Autom."    TO tipo-wk
                   when 6 move "6-Cartao Cred."  to tipo-wk
                   when 9 move "9-Desc.Comissão" to tipo-wk
                 END-EVALUATE
                 WRITE REG-WORK INVALID KEY
                     STRING "ERRO DE GRAVAÇÃO WORK ... " NR-CHEQUE-WK
                     " NÚMERO DE DOCUMENTO REPETIDO" INTO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                 END-WRITE
                END-IF
             END-READ
           END-PERFORM.

       EXIBIR-MENSAGEM SECTION.
           move    1      to gs-flag-critica
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CARREGA-CHEQUES SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS           TO VENCTO-WK
           MOVE ZEROS           TO NR-CHEQUE-WK BANCO-WK
           START WORK KEY IS NOT < ALT-WK
                    INVALID KEY MOVE "10" TO ST-WORK.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                PERFORM MOVER-DADOS-LISTA
                ADD 1 TO TOT-PARCELA
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       MOVER-DADOS-LISTA SECTION.
           IF GS-ALTERA = 0
              MOVE SPACES             TO GS-LINDET
           END-IF
           MOVE VENCTO-WK          TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(1: 12)
           MOVE VALOR-WK           TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(13: 15)
           MOVE NR-CHEQUE-WK       TO GS-LINDET(28: 8)
           MOVE BANCO-WK           TO GS-LINDET(42: 5)
           MOVE PARCELA-WK         TO GS-LINDET(49:2)
           MOVE "/"                TO GS-LINDET(51:1)
           MOVE QT-PARCELA-WK      TO GS-LINDET(52:2)
           MOVE TIPO-WK            TO GS-LINDET(60: 18)
           MOVE DATA-BAIXA-WK      TO GS-LINDET(79:10)
           MOVE CARTAO-WK          TO GS-LINDET(92:2)
           MOVE "-"                TO GS-LINDET(94:1)
           MOVE DESC-CARTAO-WK     TO GS-LINDET(95:20).
      *-------------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           MOVE GS-DATA-MOVTO    TO DATA-MOVTO-W
           MOVE GS-TAXA          TO TAXA-W1.
           MOVE GS-NR-ALBUM      TO NR-ALBUM-W
           MOVE GS-VENDEDOR      TO VENDEDOR-W
           MOVE GS-PORTADOR      TO PORTADOR-W
           MOVE GS-DESC-PORTADOR TO DESC-PORTADOR-W
           MOVE GS-EXTRA-ENVIADO1 TO EXTRA-ENVIADO1-W
           MOVE GS-EXTRA-ENVIADO2 TO EXTRA-ENVIADO2-W
           INITIALIZE REG-RCD100
           INITIALIZE REG-RCD101
           INITIALIZE GS-DATA-BLOCK
           MOVE TAXA-W1      TO GS-TAXA
           MOVE DATA-MOVTO-W TO GS-DATA-MOVTO.
           MOVE NR-ALBUM-W   TO GS-NR-ALBUM.
           MOVE VENDEDOR-W   TO GS-VENDEDOR.
           MOVE PORTADOR-W   TO GS-PORTADOR
           MOVE EXTRA-ENVIADO1-W TO GS-EXTRA-ENVIADO1
           MOVE EXTRA-ENVIADO2-W TO GS-EXTRA-ENVIADO2
           MOVE DESC-PORTADOR-W TO GS-DESC-PORTADOR
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           REFRESH-OBJECT WIN4

           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
      *---------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    MTD020 RCD100 RCD100C LOG002 LOG003
           OPEN I-O MTD020 RCD100 RCD100C LOG002 LOG003
           MOVE ALBUM-REC     TO ALBUM-MTG.
           READ MTD020 INVALID KEY CONTINUE
              NOT INVALID KEY
                  MOVE 0 TO FOGO-MTG
                  REWRITE REG-MTD020 INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "A"         TO LOG2-OPERACAO
                       MOVE "MTD020"    TO LOG2-ARQUIVO
                       MOVE "RCP100"    TO LOG2-PROGRAMA
                       MOVE REG-MTD020  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
                  END-REWRITE
           END-READ.


           DELETE RCD100 NOT INVALID KEY
                  MOVE USUARIO-W   TO LOG3-USUARIO
                  MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                  MOVE WS-DATA-CPU TO LOG3-DATA
                  ACCEPT WS-HORA-SYS FROM TIME
                  MOVE WS-HORA-SYS TO LOG3-HORAS
                  MOVE "E"         TO LOG3-OPERACAO
                  MOVE "RCD100"    TO LOG3-ARQUIVO
                  MOVE "RCP100"    TO LOG3-PROGRAMA
                  MOVE REG-RCD100  TO LOG3-REGISTRO
                  WRITE REG-LOG003
                  END-WRITE

                  DELETE RCD100C NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "E"         TO LOG3-OPERACAO
                       MOVE "RCD100C"   TO LOG3-ARQUIVO
                       MOVE "RCP100"    TO LOG3-PROGRAMA
                       MOVE REG-RCD100C TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE
                  END-DELETE
           END-DELETE

           PERFORM EXCLUIR-RCD100E

           PERFORM EXCLUI-RCD101

           CLOSE      MTD020 RCD100 RCD100C LOG002 LOG003
           OPEN INPUT MTD020 RCD100 RCD100C LOG002 LOG003

      *    PERFORM EXCLUI-CHD010.
      *    PERFORM EXCLUI-CRD020.
           PERFORM LIMPAR-DADOS.
      *EXCLUI-CHD010 SECTION.
      *    MOVE ZEROS TO NR-CHEQUE-WK.
      *    START WORK KEY IS NOT < NR-CHEQUE-WK INVALID KEY
      *          MOVE "10" TO ST-WORK.
      *    PERFORM UNTIL ST-WORK = "10"
      *      READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
      *        NOT AT END
      *          PERFORM PROCURA-CHEQUE
      *          IF CHEQUE-ENCONTRADO = 1
      *             OPEN I-O CHD099
      *             MOVE REG-CHD010 TO REG-CHD099
      *             WRITE REG-CHD099
      *             END-WRITE
      *             CLOSE CHD099
      *             DELETE CHD010
      *          ELSE
      *            "APRESENTAR CHEQUE NAO ENCONTRADO"
      *          END-IF
      *      END-READ
      *    END-PERFORM.
      *EXCLUI-CRD020 SECTION.
      *    MOVE ZEROS TO NR-CHEQUE-WK.
      *    START WORK KEY IS NOT < NR-CHEQUE-WK INVALID KEY
      *          MOVE "10" TO ST-WORK.
      *    PERFORM UNTIL ST-WORK = "10"
      *      READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
      *        NOT AT END
      *          PERFORM PROCURA-DUPLICATA
      *          IF CHEQUE-ENCONTRADO = 1
      *             OPEN I-O CRD099
      *             MOVE REG-CRD020 TO REG-CRD099
      *             WRITE REG-CRD099
      *             END-WRITE
      *             CLOSE CRD099
      *             DELETE CRD020
      *          ELSE
      *            "APRESENTAR DUPLICATA NAO ENCONTRADO"
      *          END-IF
      *      END-READ
      *    END-PERFORM.
      *
      *
      *PROCURA-DUPLICATA SECTION.
      *    MOVE ALBUM-REC    TO CLIENTE-CR20.
      *    MOVE 0            TO CLASS-CLIENTE-CR20.
      *    MOVE ZEROS        TO SEQ-CR20.
      *    START CRD020 KEY IS NOT < CHAVE-CR20 INVALID KEY
      *          MOVE "10" TO ST-CRD020.
      *    MOVE 0            TO CHEQUE-ENCONTRADO.
      *    PERFORM UNTIL ST-CRD020 = "10"
      *      READ CRD020 NEXT RECORD AT END MOVE "10" TO ST-CRD020
      *        NOT AT END
      *          IF CLIENTE-CR20 <> ALBUM-REC MOVE "10" TO ST-CRD020
      *          ELSE
      *            MOVE NR-DOCTO-CR20(1: 7) TO NR-CHEQUE-W
      *            IF NR-CHEQUE1 = NR-CHEQUE-WK
      *                MOVE 1 TO CHEQUE-ENCONTRATO
      *                MOVE "10" TO ST-CRD020
      *            END-IF
      *          END-IF
      *      END-READ
      *    END-PERFORM.
      *
      *PROCURA-CHEQUE SECTION.
      *    MOVE ALBUM-REC    TO CLIENTE-CH10.
      *    MOVE 0            TO CLASS-CLIENTE-CH10.
      *    MOVE ZEROS        TO DATA-VENCTO-CH10.
      *    START CHD010 KEY IS NOT < ALT-CH4 INVALID KEY
      *          MOVE "10" TO ST-CHD010.
      *    MOVE 0            TO CHEQUE-ENCONTRADO.
      *    PERFORM UNTIL ST-CHD010 = "10"
      *      READ CHD010 NEXT RECORD AT END MOVE "10" TO ST-CHD010
      *        NOT AT END
      *          IF CLIENTE-CH10 <> ALBUM-REC MOVE "10" TO ST-CHD010
      *          ELSE
      *            MOVE NR-CHEQUE-CH10  TO NR-CHEQUE-W
      *            IF NR-CHEQUE1 = NR-CHEQUE-WK
      *                MOVE 1 TO CHEQUE-ENCONTRATO
      *                MOVE "10" TO ST-CHD010
      *            END-IF
      *          END-IF
      *      END-READ
      *    END-PERFORM.
      *
       EXCLUI-CHEQUE SECTION.
      *    EXCLUSAO ATRAVÉS DA LIST BOX
           IF GS-LINDET(79:08) <> "00000000"
              MOVE "Parcela Não Pode ser Excluída, pois já foi Baixada"
              TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE GS-LINDET(28: 6)   TO NR-CHEQUE-WK
              READ WORK INVALID KEY
                   CONTINUE
              NOT INVALID KEY
                   DELETE WORK
              END-READ.
      *----------------------------------------------------------
       GRAVAR-CHEQUE-WORK SECTION.
           MOVE GS-NR-CHEQUE             TO NR-CHEQUE-WK
           MOVE GS-VENCTO-CH             TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                 TO VENCTO-WK
           MOVE GS-VALOR-CH              TO VALOR-WK
           MOVE GS-BANCO-CH              TO BANCO-WK
           MOVE GS-TIPO-CH               TO TIPO-WK
           MOVE GS-PARCELA               TO PARCELA-WK
           MOVE GS-QT-PARCELA            TO QT-PARCELA-WK
           MOVE GS-CARTAO                TO CARTAO-WK
           MOVE GS-DESC-CARTAO           TO DESC-CARTAO-WK
           MOVE GS-TAXA-ADMINIST-CREDITO TO TAXA-ADMINIST-CREDITO-WK
           MOVE GS-TAXA-ADMINIST-PARCELA TO TAXA-ADMINIST-PARCELA-WK
           MOVE GS-COD-COMISS            TO CODIGO-COMIS-RC01
           READ RCD001 INVALID KEY
               INITIALIZE REG-RCD001
           END-READ
           MOVE GS-TIPO-CH             TO GS-TIPO
           EVALUATE GS-TIPO
               WHEN "1" MOVE COMIS-CHEQUE-RC01         TO COMIS-PARC-WK
                        PERFORM VER-QTDE-DIAS
               WHEN "2" MOVE COMIS-MOEDA-RC01          TO COMIS-PARC-WK
               WHEN "3" MOVE COMIS-ANTECIPADA-RC01     TO COMIS-PARC-WK
                        PERFORM VER-QTDE-DIAS
      *                 MOVE ZEROS                     TO COMIS-PARC-WK
               WHEN "4" MOVE COMIS-DUPLICATA-RC01      TO COMIS-PARC-WK
                        PERFORM VER-QTDE-DIAS
               WHEN "5" MOVE COMIS-DEBITO-AUTOMATICO-RC01
                                                       TO COMIS-PARC-WK
                        PERFORM VER-QTDE-DIAS
               WHEN "6" MOVE COMIS-CARTAO-CREDITO-RC01 TO COMIS-PARC-WK
                        PERFORM VER-QTDE-DIAS
               WHEN "9" MOVE ZEROS                     TO COMIS-PARC-WK
           END-EVALUATE

           STRING GS-NR-ALBUM GS-NR-FORM INTO ALBUM-MTG
           READ MTD020 INVALID KEY
               MOVE ZEROS TO FOGO-MTG.

           IF FOGO-MTG = 8
              MOVE ZEROS TO COMIS-PARC-WK.

           MOVE ZEROS                  TO DATA-BAIXA-WK.
           IF GS-VENCTO-CH = ZEROS OR
              GS-VALOR-CH = ZEROS OR GS-TIPO-CH = SPACES
                CONTINUE
           ELSE
            READ WORK INVALID KEY
                 WRITE REG-WORK
                 END-WRITE
                 PERFORM MOVER-DADOS-LISTA
                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 ADD 1 TO TOT-PARCELA
            NOT INVALID KEY
                 MOVE GS-NR-CHEQUE           TO NR-CHEQUE-WK
                 MOVE GS-VENCTO-CH           TO DATA-INV
                 CALL "GRIDAT2" USING DATA-INV
                 MOVE DATA-INV               TO VENCTO-WK
                 MOVE GS-VALOR-CH            TO VALOR-WK
                 MOVE GS-BANCO-CH            TO BANCO-WK
                 MOVE GS-TIPO-CH             TO TIPO-WK
                 MOVE GS-PARCELA             TO PARCELA-WK
                 MOVE GS-QT-PARCELA          TO QT-PARCELA-WK
                 MOVE GS-CARTAO              TO CARTAO-WK
                 MOVE GS-DESC-CARTAO         TO DESC-CARTAO-WK
                 MOVE GS-TAXA-ADMINIST-CREDITO
                                             TO TAXA-ADMINIST-CREDITO-WK
                 MOVE GS-TAXA-ADMINIST-PARCELA
                                             TO TAXA-ADMINIST-PARCELA-WK
                 MOVE GS-COD-COMISS          TO CODIGO-COMIS-RC01
                 READ RCD001 INVALID KEY
                     INITIALIZE REG-RCD001
                 END-READ
                 MOVE GS-TIPO-CH             TO GS-TIPO
                 EVALUATE GS-TIPO
                     WHEN "1" MOVE COMIS-CHEQUE-RC01  TO COMIS-PARC-WK
                              PERFORM VER-QTDE-DIAS
                     WHEN "2" MOVE COMIS-MOEDA-RC01   TO COMIS-PARC-WK
                     WHEN "3" MOVE COMIS-ANTECIPADA-RC01
                              TO COMIS-PARC-WK
                              PERFORM VER-QTDE-DIAS
                     WHEN "4" MOVE COMIS-DUPLICATA-RC01 TO COMIS-PARC-WK
                              PERFORM VER-QTDE-DIAS
                     WHEN "5" MOVE COMIS-DEBITO-AUTOMATICO-RC01
                              TO COMIS-PARC-WK
                              PERFORM VER-QTDE-DIAS
                     WHEN "6" MOVE COMIS-CARTAO-CREDITO-RC01
                              TO COMIS-PARC-WK
                              PERFORM VER-QTDE-DIAS
                     WHEN "9" MOVE ZEROS TO COMIS-PARC-WK
                 END-EVALUATE
                 REWRITE REG-WORK INVALID KEY
                   MOVE "ERRO DE REGRAVACAO" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
                 END-REWRITE
                 PERFORM MOVER-DADOS-LISTA
                 MOVE "ALTERA-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 MOVE "Documento Já Informado, e Alterado com Sucesso"
                 TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
                 MOVE 0 TO GS-ALTERA
            END-READ
           END-IF.

       VER-QTDE-DIAS SECTION.
           MOVE GS-VENCTO-CH       TO LINK-DATAPE
           MOVE GS-DATA-VENDA      TO LINK-DATABA
           CALL "DIFDIAS" USING LINKA-DIFDIAS
           CANCEL "DIFDIAS"

           EVALUATE GS-TIPO
               WHEN "1" MOVE GS-COD-COMISS     TO CODIGO-COMIS-RC02
                        MOVE "Cheque"          TO TIPO-RC02
                        READ RCD002
      *                    INVALID KEY
      *                    MOVE "Não Achei a Comissão" TO MENSAGEM
      *                    MOVE "C" TO TIPO-MSG
      *                    PERFORM EXIBIR-MENSAGEM
                        NOT INVALID KEY
                           MOVE COMIS-0-30-DIAS TO COMIS-PARC-WK
                           IF LINK-DIASCA > 30
                              MOVE COMIS-31-120-DIAS TO COMIS-PARC-WK
                           END-IF
                           IF LINK-DIASCA > 120
                              MOVE COMIS-121-240-DIAS TO COMIS-PARC-WK
                           END-IF
                           IF LINK-DIASCA > 240
                              MOVE COMIS-241-000-DIAS TO COMIS-PARC-WK
                           END-IF
                        END-READ
               WHEN "3" MOVE GS-COD-COMISS     TO CODIGO-COMIS-RC02
                        MOVE "Antecipada"      TO TIPO-RC02
                        READ RCD002 NOT INVALID KEY
                           MOVE COMIS-0-30-DIAS TO COMIS-PARC-WK
                           IF LINK-DIASCA > 30
                              MOVE COMIS-31-120-DIAS TO COMIS-PARC-WK
                           END-IF
                           IF LINK-DIASCA > 120
                              MOVE COMIS-121-240-DIAS TO COMIS-PARC-WK
                           END-IF
                           IF LINK-DIASCA > 240
                              MOVE COMIS-241-000-DIAS TO COMIS-PARC-WK
                           END-IF
                        END-READ
               WHEN "4" MOVE GS-COD-COMISS     TO CODIGO-COMIS-RC02
                        MOVE "Duplicata"       TO TIPO-RC02
                        READ RCD002
      *                    INVALID KEY
      *                    MOVE "Não Achei a Comissão" TO MENSAGEM
      *                    MOVE "C" TO TIPO-MSG
      *                    PERFORM EXIBIR-MENSAGEM
                        NOT INVALID KEY
                           MOVE COMIS-0-30-DIAS TO COMIS-PARC-WK
                           IF LINK-DIASCA > 30
                              MOVE COMIS-31-120-DIAS TO COMIS-PARC-WK
                           END-IF
                           IF LINK-DIASCA > 120
                              MOVE COMIS-121-240-DIAS TO COMIS-PARC-WK
                           END-IF
                           IF LINK-DIASCA > 240
                              MOVE COMIS-241-000-DIAS TO COMIS-PARC-WK
                           END-IF
                        END-READ
               WHEN "5" MOVE GS-COD-COMISS     TO CODIGO-COMIS-RC02
                        MOVE "Cartão Deb."     TO TIPO-RC02
                        READ RCD002
      *                    INVALID KEY
      *                    MOVE "Não Achei a Comissão" TO MENSAGEM
      *                    MOVE "C" TO TIPO-MSG
      *                    PERFORM EXIBIR-MENSAGEM
                        NOT INVALID KEY
                           MOVE COMIS-0-30-DIAS TO COMIS-PARC-WK
                           IF LINK-DIASCA > 30
                              MOVE COMIS-31-120-DIAS TO COMIS-PARC-WK
                           END-IF
                           IF LINK-DIASCA > 120
                              MOVE COMIS-121-240-DIAS TO COMIS-PARC-WK
                           END-IF
                           IF LINK-DIASCA > 240
                              MOVE COMIS-241-000-DIAS TO COMIS-PARC-WK
                           END-IF
                        END-READ
               WHEN "6"  MOVE GS-COD-COMISS     TO CODIGO-COMIS-RC02
                         MOVE "Cartão Cred."    TO TIPO-RC02
                         READ RCD002
      *                    INVALID KEY
      *                    MOVE "Não Achei a Comissão" TO MENSAGEM
      *                    MOVE "C" TO TIPO-MSG
      *                    PERFORM EXIBIR-MENSAGEM
                         NOT INVALID KEY
                           MOVE COMIS-0-30-DIAS TO COMIS-PARC-WK
                           IF LINK-DIASCA > 30
                              MOVE COMIS-31-120-DIAS TO COMIS-PARC-WK
                           END-IF
                           IF LINK-DIASCA > 120
                              MOVE COMIS-121-240-DIAS TO COMIS-PARC-WK
                           END-IF
                           IF LINK-DIASCA > 240
                              MOVE COMIS-241-000-DIAS TO COMIS-PARC-WK
                           END-IF
                         END-READ
               WHEN "9" MOVE ZEROS TO COMIS-PARC-WK
           END-EVALUATE.

       ADICIONA-MES-CHEQUE SECTION.
           ADD 1                       TO GS-NR-CHEQUE
           ADD 1                       TO GS-PARCELA
           MOVE GS-VENCTO-CH           TO DATA-W
           ADD 1                       TO MES-W
           IF MES-W > 12
              ADD 1                    TO ANO-W
              MOVE 1                   TO MES-W
           END-IF
           MOVE 1         TO GRTIME-TYPE
           MOVE 7         TO GRTIME-FUNCTION
           MOVE DATA-W    TO GRTIME-DATE
           CALL "GRTIME" USING PARAMETROS-GRTIME
           CANCEL "GRTIME"
           IF GRTIME-DATE-FINAL = ZEROS
              MOVE 1      TO DIA-W
              ADD 1       TO MES-W
              IF MES-W = 13 MOVE 01 TO MES-W
                            ADD 1   TO ANO-W
              END-IF
           END-IF
           MOVE DATA-W    TO GS-VENCTO-CH.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CONFERE-RECIBO SECTION.
           PERFORM CALCULA-PRAZO-MEDIO.

           MOVE ZEROS TO GS-TOTAL-ESCALA-BRUTO
           MOVE ZEROS TO GS-TOTAL-ESCALA-LIQUIDO


           MOVE GS-NR-ALBUM            TO CONTRATO-MTG-E

           READ MTD020E
      *         INVALID KEY
      *         MOVE "Não encontrei a Escala de Preço" TO MENSAGEM
      *         MOVE "C" TO TIPO-MSG
      *         PERFORM EXIBIR-MENSAGEM
      *         INITIALIZE REG-MTD020E
           NOT INVALID KEY
                INVOKE GS-ACP-LISTVIEW "DeleteAll"
                MOVE "N" TO ACHEI
                INITIALIZE REG-RCD100E
                MOVE GS-NR-ALBUM      TO AUX-ALBUM-REC(1:4)
                MOVE GS-NR-FORM       TO AUX-ALBUM-REC(5:4)
                MOVE AUX-ALBUM-REC    TO ALBUM-REC-E
                START RCD100E KEY IS NOT LESS CHAVE-ALBUM-REC-E
                                                            INVALID KEY
                      MOVE "10" TO ST-RCD100E
                END-START
                PERFORM UNTIL ST-RCD100E = "10"
                      READ RCD100E NEXT AT END
                           MOVE "10" TO ST-RCD100E
                      NOT AT END
                           IF ALBUM-REC-E <> AUX-ALBUM-REC
                              MOVE "10" TO ST-RCD100E
                           ELSE
                              MOVE "S"  TO ACHEI
                              PERFORM INSERIR-LISTVIEW-RCD100E
                           END-IF
                      END-READ
                END-PERFORM
                IF ACHEI = "N"
                   PERFORM ATUALIZAR-LISTVIEW
                END-IF

                MOVE CARENCIA-JUROS-MTG-E   TO GS-ACP-CARENCIA
                MOVE PM-AVISTA-MTG-E        TO GS-PM-AVISTA
                MOVE TAXA-JUROS-MTG-E       TO GS-ACP-TAXA-JUROS
                MOVE TAXA-DESCONTO-MTG-E    TO GS-ACP-TAXA-DESCONTO

           END-READ

      *    MOVE ZEROS TO GS-TOTAL-DESCONTO

           COMPUTE GS-TOTAL-DESCONTO = GS-TOTAL-ESCALA-BRUTO -
                                       GS-TOTAL-ESCALA-LIQUIDO

           IF PM-AVISTA-MTG-E IS NOT NUMERIC
              MOVE 0 TO PM-AVISTA-MTG-E
           END-IF

           MOVE GS-TOTAL-ESCALA-LIQUIDO TO AUX-VALOR

           MOVE ZEROS                   TO GS-TOTAL-DESCONTO-AV

           IF GS-TOT-PM NOT > PM-AVISTA-MTG-E
              COMPUTE GS-TOTAL-DESCONTO-AV =
              - (AUX-VALOR * GS-ACP-TAXA-DESCONTO / 100)
           END-IF

           COMPUTE GS-TOTAL-LIQUIDO = GS-TOTAL-ESCALA-LIQUIDO -
                                      GS-TOTAL-DESCONTO-AV

           IF GS-TOT-PM > GS-ACP-CARENCIA
              PERFORM CALCULAR-JUROS
           ELSE
              MOVE ZEROS TO GS-TOTAL-JUROS
           END-IF

           COMPUTE GS-TOTAL-DESCONTO = GS-TOTAL-ESCALA-BRUTO -
                                       GS-TOTAL-ESCALA-LIQUIDO

           COMPUTE GS-TOTAL-ESCALA-LIQUIDO-JR = GS-TOTAL-LIQUIDO
                                              + GS-TOTAL-JUROS

           REFRESH-OBJECT WIN1.

       CALCULAR-JUROS SECTION.
           MOVE ZEROS                   TO GS-TOTAL-JUROS


           MOVE GS-TOTAL-LIQUIDO TO AUX-VALOR

           COMPUTE QTDE-MESES = GS-TOT-PM / 30

           MOVE ZEROS TO IND
           PERFORM QTDE-MESES TIMES
               ADD 1 TO IND


               COMPUTE AUX-VALOR ROUNDED = AUX-VALOR +
                      (AUX-VALOR * GS-ACP-TAXA-JUROS / 100)
           END-PERFORM

           COMPUTE QTDE-DIAS = GS-TOT-PM - (QTDE-MESES * 30)

           IF QTDE-DIAS > 0
              COMPUTE TAXA-W = (((GS-ACP-TAXA-JUROS / 30) / 100)
                                        * QTDE-DIAS)
              COMPUTE AUX-VALOR ROUNDED = AUX-VALOR +
                               (AUX-VALOR * TAXA-W)

           END-IF

           COMPUTE GS-TOTAL-JUROS = AUX-VALOR -
                                    GS-TOTAL-LIQUIDO.

       ATUALIZAR-LISTVIEW SECTION.
           INVOKE GS-ACP-LISTVIEW "DeleteAll"

           MOVE ZEROS TO QUAL-ITEM

           PERFORM UNTIL QUAL-ITEM = 17
               ADD 1  TO QUAL-ITEM

               INITIALIZE INDICE
               INVOKE GS-ACP-LISTVIEW "ADICIONARITEM"
                                       RETURNING WSITEM

      *>Item
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING QUAL-ITEM X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

               EVALUATE  QUAL-ITEM
                   WHEN 1  MOVE "Encadernação"         TO QUAL-PRODUTO
                           MOVE QT-ENCADER-MTG         TO QTDE-MONTAGEM
                           MOVE PRECO-ENCADER-MTG-E    TO VLR-ESCALA
                           MOVE GS-QT-ENCADERN         TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-ENCADER  TO ACIMA-DE

                           IF GS-QT-ENCADERN > QTDE-ACIMA-DE-ENCADER
                              MOVE PERC-DESC-COM-ENCADER
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-ENCADER
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 2  MOVE "Estojo"               TO QUAL-PRODUTO
                           MOVE QT-ESTOJO-MTG          TO QTDE-MONTAGEM
                           MOVE PRECO-ESTOJO-MTG-E     TO VLR-ESCALA
                           MOVE GS-QT-ESTOJO           TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-ESTOJO   TO ACIMA-DE
                           IF GS-QT-ESTOJO > QTDE-ACIMA-DE-ESTOJO
                              MOVE PERC-DESC-COM-ESTOJO
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-ESTOJO
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 3  MOVE "Folha"                TO QUAL-PRODUTO
                           MOVE QT-FOLHAS-MTG          TO QTDE-MONTAGEM
                           MOVE PRECO-FOLHA-MTG-E      TO VLR-ESCALA
                           MOVE GS-QT-FOLHA            TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-FOLHA    TO ACIMA-DE
                           IF GS-QT-FOLHA > QTDE-ACIMA-DE-FOLHA
                              MOVE PERC-DESC-COM-FOLHA
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-FOLHA
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 4  MOVE "Foto"                 TO QUAL-PRODUTO
                           MOVE QT-FOTOS-MTG           TO QTDE-MONTAGEM
                           MOVE PRECO-FOTO-MTG-E       TO VLR-ESCALA
                           MOVE GS-QT-FOTO             TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-FOTO     TO ACIMA-DE
                           IF GS-QT-FOTO = QT-FOTOS-MTG
                              IF GS-QT-FOTO > QTDE-ACIMA-DE-FOTO
                                 MOVE PERC-DESC-COM-FOTO
                                   TO PERC-DESC-COMISSAO
                                 MOVE PERC-DESC-FORM-FOTO
                                   TO PERC-DESC-FORMANDO
                              ELSE
                                 MOVE ZEROS TO PERC-DESC-COMISSAO
                                 MOVE ZEROS TO PERC-DESC-FORMANDO
                              END-IF
                           ELSE
                              IF GS-MEMBRO-COMISSAO = 1
                                 MOVE PERC-DESC-COM-FOTO
                                   TO PERC-DESC-COMISSAO
                                 MOVE ZEROS TO PERC-DESC-FORMANDO
                              ELSE
                                 MOVE ZEROS TO PERC-DESC-COMISSAO
                                 MOVE ZEROS TO PERC-DESC-FORMANDO
                              END-IF
                           END-IF
                   WHEN 5  MOVE "Fita"                 TO QUAL-PRODUTO
                           MOVE QT-FITAS-MTG           TO QTDE-MONTAGEM
                           MOVE PRECO-FITA-MTG-E       TO VLR-ESCALA
                           MOVE GS-QT-FITA             TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-FITA     TO ACIMA-DE
                           IF GS-QT-FITA > QTDE-ACIMA-DE-FITA
                              MOVE PERC-DESC-COM-FITA
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-FITA
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 6  MOVE "Porta Fita"           TO QUAL-PRODUTO
                           MOVE QT-PORTA-FITA-MTG      TO QTDE-MONTAGEM
                           MOVE PRECO-PORTA-FITA-MTG-E TO VLR-ESCALA
                           MOVE GS-QT-PFITA            TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-PT-FITA  TO ACIMA-DE
                           IF GS-QT-PFITA > QTDE-ACIMA-DE-PT-FITA
                              MOVE PERC-DESC-COM-PT-FITA
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-PT-FITA
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 7  MOVE "Poster"               TO QUAL-PRODUTO
                           MOVE QT-POSTER-MTG          TO QTDE-MONTAGEM
                           MOVE PRECO-POSTER-MTG-E     TO VLR-ESCALA
                           MOVE GS-QT-POSTER           TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-POSTER   TO ACIMA-DE
                           IF GS-QT-POSTER > QTDE-ACIMA-DE-POSTER
                              MOVE PERC-DESC-COM-POSTER
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-POSTER
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 8  MOVE "DVD"                  TO QUAL-PRODUTO
                           MOVE QT-DVD-MTG             TO QTDE-MONTAGEM
                           MOVE PRECO-DVD-MTG-E        TO VLR-ESCALA
                           MOVE GS-QT-DVD              TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-DVD      TO ACIMA-DE
                           IF GS-QT-DVD > QTDE-ACIMA-DE-DVD
                              MOVE PERC-DESC-COM-DVD
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-DVD
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 9  MOVE "Porta DVD"            TO QUAL-PRODUTO
                           MOVE QT-PORTA-DVD-MTG       TO QTDE-MONTAGEM
                           MOVE PRECO-PORTA-DVD-MTG-E  TO VLR-ESCALA
                           MOVE GS-QT-PORTA-DVD        TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-PT-DVD   TO ACIMA-DE
                           IF GS-QT-PORTA-DVD > QTDE-ACIMA-DE-PT-DVD
                              MOVE PERC-DESC-COM-PT-DVD
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-PT-DVD
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 10 MOVE "Foto CD"              TO QUAL-PRODUTO
                           MOVE QT-FOTO-CD-MTG         TO QTDE-MONTAGEM
                           MOVE PRECO-FOTO-CD-MTG-E    TO VLR-ESCALA
                           MOVE GS-QT-FOTO-CD          TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-FT-CD    TO ACIMA-DE
                           IF GS-QT-FOTO-CD > QTDE-ACIMA-DE-FT-CD
                              MOVE PERC-DESC-COM-FT-CD
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-FT-CD
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 11 MOVE "Moldura"              TO QUAL-PRODUTO
                           MOVE QT-MOLDURA-MTG         TO QTDE-MONTAGEM
                           MOVE PRECO-MOLDURA-MTG-E    TO VLR-ESCALA
                           MOVE GS-QT-MOLDURA          TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-MOLDURA  TO ACIMA-DE
                           IF GS-QT-MOLDURA > QTDE-ACIMA-DE-MOLDURA
                              MOVE PERC-DESC-COM-MOLDURA
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-MOLDURA
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 12 MOVE "Book"                 TO QUAL-PRODUTO
                           MOVE QT-BOOK-MTG            TO QTDE-MONTAGEM
                           MOVE PRECO-BOOK-MTG-E       TO VLR-ESCALA
                           MOVE GS-QT-BOOK             TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-BOOK     TO ACIMA-DE
                           IF GS-QT-BOOK > QTDE-ACIMA-DE-BOOK
                              MOVE PERC-DESC-COM-BOOK
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-BOOK
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 13 MOVE "Porta Retrato"        TO QUAL-PRODUTO
                           MOVE QT-PORTA-RETRATO-MTG-C TO QTDE-MONTAGEM
                           MOVE PRECO-PORTA-RETRATO-MTG-E  TO VLR-ESCALA
                           MOVE GS-QT-PORTA-RETRATO    TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-PT-RET   TO ACIMA-DE
                           IF GS-QT-PORTA-RETRATO > QTDE-ACIMA-DE-PT-RET
                              MOVE PERC-DESC-COM-PT-RET
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-PT-RET
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 14 MOVE "Pendrive"             TO QUAL-PRODUTO
                           MOVE QT-PENDRIVE-MTG-C      TO QTDE-MONTAGEM
                           MOVE PRECO-PENDRIVE-MTG-E   TO VLR-ESCALA
                           MOVE GS-QT-PENDRIVE         TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-PENDRIVE TO ACIMA-DE
                           IF GS-QT-PENDRIVE > QTDE-ACIMA-DE-PENDRIVE
                              MOVE PERC-DESC-COM-PENDRIVE
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-PENDRIVE
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 15 MOVE "Video HD"             TO QUAL-PRODUTO
                           MOVE QT-VIDEO-HD-MTG-C      TO QTDE-MONTAGEM
                           MOVE PRECO-VIDEO-HD-MTG-E   TO VLR-ESCALA
                           MOVE GS-QT-VIDEO-HD         TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-VIDEO-HD TO ACIMA-DE
                           IF GS-QT-VIDEO-HD > QTDE-ACIMA-DE-VIDEO-HD
                              MOVE PERC-DESC-COM-VIDEO-HD
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-VIDEO-HD
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 16 MOVE "Revista"              TO QUAL-PRODUTO
                           MOVE QT-REVISTA-MTG-C       TO QTDE-MONTAGEM
                           MOVE PRECO-REVISTA-MTG-E    TO VLR-ESCALA
                           MOVE GS-QT-REVISTA          TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-REVISTA  TO ACIMA-DE
                           IF GS-QT-REVISTA > QTDE-ACIMA-DE-REVISTA
                              MOVE PERC-DESC-COM-REVISTA
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-REVISTA
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
                   WHEN 17 MOVE "Calendário"           TO QUAL-PRODUTO
                           MOVE QT-CALENDARIO-MTG-C    TO QTDE-MONTAGEM
                           MOVE PRECO-CALENDARIO-MTG-E TO VLR-ESCALA
                           MOVE GS-QT-CALENDARIO       TO QTDE-RECIBO
                           COMPUTE QTDE-DIFERENCA = QTDE-MONTAGEM -
                                                    QTDE-RECIBO
                           MOVE QTDE-ACIMA-DE-CALEND   TO ACIMA-DE
                           IF GS-QT-CALENDARIO > QTDE-ACIMA-DE-CALEND
                              MOVE PERC-DESC-COM-CALEND
                                TO PERC-DESC-COMISSAO
                              MOVE PERC-DESC-FORM-CALEND
                                TO PERC-DESC-FORMANDO
                           ELSE
                              MOVE ZEROS TO PERC-DESC-COMISSAO
                              MOVE ZEROS TO PERC-DESC-FORMANDO
                           END-IF
               END-EVALUATE

               COMPUTE TOT-ESCALA ROUNDED = VLR-ESCALA * QTDE-RECIBO

               ADD TOT-ESCALA TO GS-TOTAL-ESCALA-BRUTO


               IF GS-MEMBRO-COMISSAO = 1
                  COMPUTE TOT-ESCALA ROUNDED =
                               (VLR-ESCALA * QTDE-RECIBO) -
                ((VLR-ESCALA * QTDE-RECIBO) * PERC-DESC-COMISSAO / 100)
               ELSE
                  COMPUTE TOT-ESCALA ROUNDED =
                              ((VLR-ESCALA * QTDE-RECIBO) -
                ((VLR-ESCALA * QTDE-RECIBO) * PERC-DESC-FORMANDO / 100))

               END-IF

               ADD TOT-ESCALA TO GS-TOTAL-ESCALA-LIQUIDO


               MOVE QTDE-MONTAGEM       TO MASC-MONTAGEM
               MOVE QTDE-RECIBO         TO MASC-RECIBO
               MOVE QTDE-DIFERENCA      TO MASC-DIFERENCA
               MOVE VLR-ESCALA          TO MASC-ESCALA
               MOVE TOT-ESCALA          TO MASC-TESCALA

               IF GS-MEMBRO-COMISSAO = 1
                  MOVE 0                TO PERC-DESC-FORMANDO
               END-IF

               MOVE PERC-DESC-FORMANDO  TO MASC-PERC1
               MOVE PERC-DESC-COMISSAO  TO MASC-PERC2
               MOVE ACIMA-DE            TO MASC-ACIMA-DE

      *>Produto
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING QUAL-PRODUTO X"00" DELIMITED BY "  " INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Montagem
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-MONTAGEM X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Recibo
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-RECIBO X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Diferenca
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-DIFERENCA X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor escala
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-ESCALA X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Acima de
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-ACIMA-DE X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>%Desconto
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               IF GS-MEMBRO-COMISSAO = 1
                  STRING MASC-PERC1 " % + " MASC-PERC2
                         " %" X"00" INTO WSTEXTO
               ELSE
                  STRING MASC-PERC1 " %" X"00" INTO WSTEXTO
               END-IF
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Total Escala
               ADD 1 TO INDICE
               INITIALIZE WSTEXTO
               STRING MASC-TESCALA X"00" INTO WSTEXTO
               INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                 USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
           END-PERFORM

           MOVE CARENCIA-JUROS-MTG-E   TO GS-ACP-CARENCIA
           MOVE PM-AVISTA-MTG-E        TO GS-PM-AVISTA
           MOVE TAXA-JUROS-MTG-E       TO GS-ACP-TAXA-JUROS
           MOVE TAXA-DESCONTO-MTG-E    TO GS-ACP-TAXA-DESCONTO

           refresh-object win1

           perform mostrar-fonte-favo
           perform mostrar-colunas-favo.


       GRAVA-RECIBO SECTION.
           CLOSE    RCD100 RCD100C LOG003
           OPEN I-O RCD100 RCD100C LOG003

           MOVE ZEROS TO NR-PARCELA.
           MOVE GS-NR-ALBUM          TO ALBUM-REC(1: 4)
           MOVE GS-NR-FORM           TO ALBUM-REC(5: 4)

           MOVE GS-DATA-MOVTO        TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO DATA-MOVTO-REC
           MOVE GS-TAXA              TO TAXA-REC
           MOVE GS-DATA-VENDA        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO DATAVEN-REC
           MOVE GS-VISITA            TO VISITA-REC
           MOVE GS-QT-ENCADERN       TO QENCADER-REC
           MOVE GS-QT-ESTOJO         TO QESTOJO-REC
           MOVE GS-QT-FOTO           TO QFOTOS-REC
           MOVE GS-QT-FOLHA          TO QFOLHAS-REC
           MOVE GS-QT-FITA           TO QFITAS-REC
           MOVE GS-QT-DVD            TO QDVD-REC
           MOVE GS-QT-PFITA          TO QPFITA-REC
           MOVE GS-QT-POSTER         TO QPOSTER-REC
           MOVE GS-QT-COBERTURA      TO QCOBERTURA-REC
           MOVE GS-QT-ABERTURA       TO QABERTURA-REC
           MOVE GS-QT-FOTO-COMISSAO  TO QCOMISSAO-REC
           MOVE GS-QT-AVULSA         TO QAVULSAS-REC
           MOVE GS-QT-COBERTURA-DVD  TO QT-COBERTURA-DVD-REC
           MOVE GS-QT-PORTA-DVD      TO QPORTA-DVD-REC
           MOVE GS-QT-BOOK           TO QBOOK-REC
           MOVE GS-QT-PORTA-RETRATO  TO QPORTA-RETRATO-REC-C
           MOVE GS-QT-PENDRIVE       TO QPENDRIVE-REC-C
           MOVE GS-QT-VIDEO-HD       TO QVIDEO-HD-REC-C
           MOVE GS-QT-REVISTA        TO QREVISTA-REC-C
           MOVE GS-QT-CALENDARIO     TO QCALENDARIO-REC-C
           IF GS-PEDIDO-EXTRA IS NOT NUMERIC
              MOVE 0 TO GS-PEDIDO-EXTRA
           END-IF
           MOVE GS-PEDIDO-EXTRA      TO PEDIDO-EXTRA-REC

           IF GS-MEMBRO-COMISSAO IS NOT NUMERIC
              MOVE 0 TO GS-MEMBRO-COMISSAO
           END-IF
           MOVE GS-MEMBRO-COMISSAO   TO MEMBRO-COMISSAO-REC

           IF GS-PED-EXT-BOOK IS NOT NUMERIC
              MOVE 0 TO GS-PED-EXT-BOOK
           END-IF
           MOVE GS-PED-EXT-BOOK      TO PED-EXT-BOOK-C

           IF GS-PED-EXT-PORTA-RET IS NOT NUMERIC
              MOVE 0 TO GS-PED-EXT-PORTA-RET
           END-IF
           MOVE GS-PED-EXT-PORTA-RET TO PED-EXT-PORTA-RET-C

           IF GS-PED-EXT-PENDRIVE IS NOT NUMERIC
              MOVE 0 TO GS-PED-EXT-PENDRIVE
           END-IF
           MOVE GS-PED-EXT-PENDRIVE TO PED-EXT-PENDRIVE-C

           IF GS-PED-EXT-VIDEO-HD IS NOT NUMERIC
              MOVE 0 TO GS-PED-EXT-VIDEO-HD
           END-IF
           MOVE GS-PED-EXT-VIDEO-HD TO PED-EXT-VIDEO-HD-C

           IF GS-PED-EXT-REVISTA IS NOT NUMERIC
              MOVE 0 TO GS-PED-EXT-REVISTA
           END-IF
           MOVE GS-PED-EXT-REVISTA TO PED-EXT-REVISTA-C

           IF GS-PED-EXT-CALENDARIO IS NOT NUMERIC
              MOVE 0 TO GS-PED-EXT-CALENDARIO
           END-IF
           MOVE GS-PED-EXT-CALENDARIO TO PED-EXT-CALEND-C

           MOVE GS-QT-MOLDURA        TO QMOLDURA-REC
           MOVE GS-QT-FOTO-CD        TO QFOTO-CD-REC
           MOVE GS-VENDEDOR          TO VENDEDOR-REC
           MOVE GS-TOT-VENDA         TO TOTAL-REC
           MOVE GS-TOT-PM            TO PM-REC
           MOVE GS-TOT-VENDA-DEF     TO TOTAL-DEF-REC

           MOVE GS-EXTRA-FOTOS1      TO QFOTOS-EXTRA1-REC
           MOVE GS-EXTRA-FOTOS2      TO QFOTOS-EXTRA2-REC
           MOVE GS-POSTER-ABERTURA   TO POSTER-ABERTURA-REC
           MOVE GS-POSTER-XEROX      TO POSTER-XEROX-REC
           MOVE GS-EXTRA-ENVIADO1    TO EXTRA-ENVIADO1-REC
           MOVE GS-EXTRA-ENVIADO2    TO EXTRA-ENVIADO2-REC

           PERFORM EXCLUIR-RCD100E
           PERFORM GRAVAR-RCD100E

           IF GS-TIPO-GRAVACAO = 1
              WRITE REG-RCD100 INVALID KEY
                 MOVE "Erro Gravacao RCD100" TO GS-MENSAGEM-ERRO
                 MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "I"         TO LOG3-OPERACAO
                 MOVE "RCD100"    TO LOG3-ARQUIVO
                 MOVE "RCP100"    TO LOG3-PROGRAMA
                 MOVE REG-MTD020  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE

                 MOVE CHAVE-ALBUM-REC TO CHAVE-ALBUM-REC-C
                 WRITE REG-RCD100C INVALID KEY
                       MOVE "Erro Gravacao RCD100C" TO GS-MENSAGEM-ERRO
                       MOVE ST-RCD100C      TO GS-MENSAGEM-ERRO(24: 5)
                       MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "I"         TO LOG3-OPERACAO
                       MOVE "RCD100C"   TO LOG3-ARQUIVO
                       MOVE "RCP100"    TO LOG3-PROGRAMA
                       MOVE REG-RCD100C TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE
                 END-WRITE
              END-WRITE

              PERFORM CADASTRA-CLIENTE
           ELSE
              REWRITE REG-RCD100 INVALID KEY
                 MOVE "Erro Regravacao RCD100" TO GS-MENSAGEM-ERRO
                 MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "A"         TO LOG3-OPERACAO
                 MOVE "RCD100"    TO LOG3-ARQUIVO
                 MOVE "RCP100"    TO LOG3-PROGRAMA
                 MOVE REG-MTD020  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE

                 MOVE CHAVE-ALBUM-REC TO CHAVE-ALBUM-REC-C
                 WRITE REG-RCD100C INVALID KEY
                     REWRITE REG-RCD100C INVALID KEY
                         MOVE "Erro Regravacao RCD100C"
                         TO GS-MENSAGEM-ERRO
                         MOVE ST-RCD100C TO GS-MENSAGEM-ERRO(24: 5)
                         MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                     NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "A"         TO LOG3-OPERACAO
                       MOVE "RCD100C"   TO LOG3-ARQUIVO
                       MOVE "RCP100"    TO LOG3-PROGRAMA
                       MOVE REG-MTD020  TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE
                     END-REWRITE
                 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "I"         TO LOG3-OPERACAO
                       MOVE "RCD100C"   TO LOG3-ARQUIVO
                       MOVE "RCP100"    TO LOG3-PROGRAMA
                       MOVE REG-RCD100C TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE
                 END-WRITE


              END-REWRITE.

           CLOSE      RCD100 RCD100C LOG003
           OPEN INPUT RCD100 RCD100C LOG003

           PERFORM SALVAR-MONTAGEM.
           PERFORM SALVAR-PARCELAS-RECIBO.

       EXCLUIR-RCD100E SECTION.
           CLOSE      RCD100E
           OPEN I-O   RCD100E
           INITIALIZE REG-RCD100E
           MOVE ALBUM-REC      TO ALBUM-REC-E
           START RCD100E KEY IS NOT LESS ALBUM-REC-E INVALID KEY
                 MOVE "10" TO ST-RCD100E.

           PERFORM UNTIL ST-RCD100E = "10"
                 READ RCD100E NEXT AT END
                      MOVE "10" TO ST-RCD100E
                 NOT AT END
                      IF ALBUM-REC <> ALBUM-REC-E
                         MOVE "10" TO ST-RCD100E
                      ELSE
                         DELETE RCD100E INVALID KEY
                             MOVE "Erro de Exclusão...RCD100E" TO
                                         MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                         END-DELETE
                      END-IF
                 END-READ
           END-PERFORM
           CLOSE      RCD100E
           OPEN INPUT RCD100E.
       EXCLUIR-RCD100E-FIM.
           EXIT.

       GRAVAR-RCD100E SECTION.
           CLOSE      RCD100E
           OPEN I-O   RCD100E

           INITIALIZE WSINDICE
           INVOKE GS-ACP-LISTVIEW "SIZE" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
                 INITIALIZE REG-RCD100E

                 ADD 1 TO WSINDICE
                 INVOKE GS-ACP-LISTVIEW "ITEMATINDEX"
                        USING WSINDICE RETURNING UMITEM

                 MOVE ALBUM-REC                    TO ALBUM-REC-E

      *>Item
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(1) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)     TO ITEM-REC-E
      *>Produto
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(2) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE WSTEXTO                      TO PRODUTO-REC-E
      *>Montagem
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(3) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)     TO MONTAGEM-REC-E
      *>Recibo
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(4) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)     TO RECIBO-REC-E
      *>Diferença
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(5) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)     TO DIFERENCA-REC-E
      *>Valor Escala
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(6) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)     TO PRECO-ESCALA-RCD-E
      *>Acima de
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(7) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)    TO QTDE-ACIMA-DE-RCD-E
      *>% Desconto
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(8) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO(1:6))  TO
                      PERC-DESC-FORM-RCD-E
                 MOVE FUNCTION NUMVAL(WSTEXTO(12:6)) TO
                      PERC-DESC-COMISSAO-RCD-E
      *>Total Escala
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(9) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)   TO TOTAL-ESCALA-RCD-E

                 MOVE GS-ACP-CARENCIA            TO CARENCIA-JUROS-RCD-E
                 MOVE GS-ACP-TAXA-JUROS          TO TAXA-JUROS-RCD-E
                 MOVE GS-ACP-TAXA-DESCONTO       TO TAXA-DESCONTO-RCD-E


                 WRITE REG-RCD100E INVALID KEY
                       MOVE "Erro de Gravação...RCD100E" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                 END-WRITE

           END-PERFORM
           CLOSE      RCD100E
           OPEN INPUT RCD100E.
       GRAVAR-RCD100E-FIM.
           EXIT.

       SALVAR-MONTAGEM SECTION.
           CLOSE    MTD020 LOG002
           OPEN I-O MTD020 LOG002
           MOVE ALBUM-REC            TO ALBUM-MTG
           READ MTD020 INVALID KEY MOVE ZEROS TO VISITA-MTG.
           MOVE GS-COD-COMISS        TO VISITA-MTG
           IF FOGO-MTG = 9 MOVE 8 TO FOGO-MTG
           ELSE MOVE 1 TO FOGO-MTG.
           REWRITE REG-MTD020 NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG2-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "A"         TO LOG2-OPERACAO
                   MOVE "MTD020"    TO LOG2-ARQUIVO
                   MOVE "RCP100"    TO LOG2-PROGRAMA
                   MOVE REG-MTD020  TO LOG2-REGISTRO
                   WRITE REG-LOG002
                   END-WRITE
           END-REWRITE
           CLOSE      MTD020 LOG002
           OPEN INPUT MTD020 LOG002.

       SALVAR-PARCELAS-RECIBO SECTION.
           CLOSE    RCD101 LOG003 LOG004
           OPEN I-O RCD101 LOG003 LOG004

           IF GS-TIPO-GRAVACAO = 2
              PERFORM EXCLUI-RCD101.

           MOVE ZEROS TO NR-CHEQUE-WK BANCO-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      MOVE VENCTO-WK           TO VENCTO-REC1
                      MOVE ALBUM-REC           TO ALBUM-REC1
                      MOVE NR-CHEQUE-WK        TO NUMERO-REC1
                      MOVE BANCO-WK            TO BANCO-REC1
                      MOVE TIPO-WK(1: 1)       TO TIPO-REC1
                      MOVE VALOR-WK            TO VALOR-REC1
                      MOVE DATA-BAIXA-WK       TO DTA-BAIXA-REC1
                      MOVE PARCELA-WK          TO PARCELA-REC1
                      MOVE QT-PARCELA-WK       TO QT-PARCELA-REC1
                      MOVE CARTAO-WK           TO CARTAO-CRED-REC1
                      MOVE COMIS-PARC-WK       TO COMIS-PARC-REC1
                      MOVE TAXA-ADMINIST-CREDITO-WK
                                          TO TAXA-ADMINIST-CREDITO-REC1
                      MOVE TAXA-ADMINIST-PARCELA-WK
                                          TO TAXA-ADMINIST-PARCELA-REC1
                      WRITE REG-RCD101 NOT INVALID KEY
                            MOVE USUARIO-W   TO LOG3-USUARIO
                            MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                            MOVE WS-DATA-CPU TO LOG3-DATA
                            ACCEPT WS-HORA-SYS FROM TIME
                            MOVE WS-HORA-SYS TO LOG3-HORAS
                            MOVE "I"         TO LOG3-OPERACAO
                            MOVE "RCD101"    TO LOG3-ARQUIVO
                            MOVE "RCP100"    TO LOG3-PROGRAMA
                            MOVE REG-RCD101  TO LOG3-REGISTRO
                            WRITE REG-LOG003
                            END-WRITE
                      END-WRITE
                      IF GS-TIPO-GRAVACAO = 1
      *                 caso seja regravação os acertos no contas a rece
      *                 ber e nos cheques deverão ser feitos manualmente
                         EVALUATE TIPO-REC1
      *                  1 CHEQUE - 2 ANTECIPADO - 3 MOEDA  -
      *                  4 DUPLI/NT.PROM
                           WHEN 1 PERFORM GRAVA-CHEQUE
                           WHEN 2 CONTINUE
                           WHEN 3 CONTINUE
                           WHEN 4 PERFORM GRAVA-DUPLICATA
                           WHEN 5 PERFORM GRAVA-DUPLICATA
                           WHEN 6 MOVE COD-USUARIO-W TO
                                       COD-USUARIO-CA004
                                  MOVE "SENHA35"     TO PROGRAMA-CA004
                                  READ CAD004 NOT INVALID KEY
                                       PERFORM GRAVA-DUPLICATA
                                  END-READ
                           WHEN 9 CONTINUE
                         END-EVALUATE
                      END-IF
                      PERFORM GRAVAR-ANOTACAO
                 END-READ
           END-PERFORM.

           CLOSE      RCD101 LOG003 LOG004
           OPEN INPUT RCD101 LOG003 LOG004.

       GRAVAR-ANOTACAO SECTION.
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

           STRING "0" ALBUM-REC INTO AUX-COD-COMPL

           MOVE AUX-COD-COMPL TO COD-COMPL-CR200

           MOVE ZEROS   TO ULT-SEQ
           MOVE ALL "9" TO SEQ-CR200
           START CRD200 KEY IS LESS THAN CHAVE-CR200 NOT INVALID KEY
                READ CRD200 PREVIOUS NOT AT END
                     IF COD-COMPL-CR200 = AUX-COD-COMPL
                        MOVE SEQ-CR200 TO ULT-SEQ.

           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1               TO ULT-SEQ.
           MOVE ULT-SEQ        TO SEQ-CR200
           MOVE AUX-COD-COMPL  TO COD-COMPL-CR200.
           MOVE ZEROS          TO DATA-RETORNO-CR200
           MOVE USUARIO-W      TO USUARIO-CR200
           MOVE DATA-DIA-I     TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
              NOT INVALID KEY
                 MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200       TO SEQ-CR201.
           MOVE COD-COMPL-CR200 TO COD-COMPL-CR201.
           MOVE 1               TO SUBSEQ-CR201.
           MOVE SPACES          TO ANOTACAO-CR201
           STRING "DOCUMENTO Nº. " NR-CHEQUE-WK " INCLUIDO ATRAVES DA MO
      -    "VIMENTACAO DE RECIBO" INTO ANOTACAO-CR201
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
               NOT INVALID KEY
                   MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.

           CLOSE CRD200 CRD201.

       EXCLUI-RCD101 SECTION.
           MOVE ZEROS TO REG-RCD101

           MOVE ALBUM-REC     TO ALBUM-REC1
           COMPUTE ALBUM-REC1 = ALBUM-REC1 - 1

           MOVE ALL "9"       TO VENCTO-REC1 BANCO-REC1 NUMERO-REC1.

           START RCD101 KEY IS GREATER THAN CHAVE-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101.

           PERFORM UNTIL ST-RCD101 = "10"
                 READ RCD101 NEXT RECORD AT END
                      MOVE "10" TO ST-RCD101
                 NOT AT END
                      MOVE ALBUM-REC1 TO AUX-ALBUM-REC

                      IF ALBUM-REC <> AUX-ALBUM-REC
                         MOVE "10" TO ST-RCD101
                      ELSE
                         DELETE RCD101 NOT INVALID KEY
                                MOVE USUARIO-W   TO LOG3-USUARIO
                                MOVE FUNCTION CURRENT-DATE
                                                 TO WS-DATA-SYS
                                MOVE WS-DATA-CPU TO LOG3-DATA
                                ACCEPT WS-HORA-SYS FROM TIME
                                MOVE WS-HORA-SYS TO LOG3-HORAS
                                MOVE "E"         TO LOG3-OPERACAO
                                MOVE "RCD101"    TO LOG3-ARQUIVO
                                MOVE "RCP100"    TO LOG3-PROGRAMA
                                MOVE REG-RCD101  TO LOG3-REGISTRO
                                WRITE REG-LOG003
                                END-WRITE
                         END-DELETE
                      END-IF
                 END-READ
           END-PERFORM.


       GRAVA-CHEQUE SECTION.
           OPEN I-O CHD010 CHD011 CHD099 CHD010B.
           INITIALIZE REG-CHD010.
           MOVE TIPO-WK(1: 1)  TO AGENCIA-CH10.
           MOVE DATA-MOVTO-REC TO DATA-MOVTO-CH10 DATA-MOVTO-CH11.
           PERFORM ACHA-SEQ-CHD011.
           MOVE SEQ-CH11       TO SEQ-CH10.
           MOVE VENCTO-WK      TO DATA-VENCTO-CH10.
           IF DATA-VENCTO-CH10 NOT > DATA-MOVTO-CH10
              MOVE 2           TO SITUACAO-CH10
           ELSE
              MOVE 0           TO SITUACAO-CH10.

           MOVE GS-PORTADOR    TO PORTADOR-CH10
           MOVE 1              TO CARTEIRA-CH10.
           MOVE 1              TO ORIGEM-CH10.
           MOVE GS-VENDEDOR    TO VENDEDOR-CH10.
           MOVE 0              TO CLASS-CLIENTE-CH10.
           MOVE ALBUM-REC      TO CLIENTE-CH10.
           MOVE VALOR-WK       TO VALOR-CH10.

           MOVE VALOR-CH10  TO TAB-VALOR

           IF TAB-VALOR1 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR1
           END-IF
           IF TAB-VALOR2 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR2
           END-IF
           IF TAB-VALOR3 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR3
           END-IF
           IF TAB-VALOR4 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR4
           END-IF
           IF TAB-VALOR5 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR5
           END-IF
           IF TAB-VALOR6 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR6
           END-IF
           IF TAB-VALOR7 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR7
           END-IF
           IF TAB-VALOR8 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR8
           END-IF
           IF TAB-VALOR9 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR9
           END-IF
           IF TAB-VALOR10 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR10
           END-IF

           MOVE TAB-VALOR TO VALOR-CH10
           IF SITUACAO-CH10 = 2
              MOVE 0      TO VALOR-SALDO-CH10
              PERFORM GERAR-BAIXA
           ELSE
              MOVE TAB-VALOR TO VALOR-SALDO-CH10
           END-IF

           MOVE GS-NOME                       TO NOME-CH10
           MOVE 9                             TO LOTE-CH10
           MOVE GS-CIDADE                     TO CIDADE-CH10
           MOVE BANCO-WK                      TO BANCO-CH10
           MOVE NR-CHEQUE-WK                  TO NR-CHEQUE-CH10
           MOVE USUARIO-W                     TO DIGITADOR-CH10
           MOVE STATUS-TIT-FATURAMENTO-PAR001 TO SITUACAO-TIT-CH10
           MOVE CODRED-FATURAMENTO-PAR001     TO CODREDUZ-APUR-CH10

           MOVE ZEROS TO ST-CHD010.
           PERFORM UNTIL ST-CHD010 = "10"
                WRITE REG-CHD010 INVALID KEY
                      PERFORM ACHA-SEQ-CHD011
                      MOVE SEQ-CH11 TO SEQ-CH10
                NOT INVALID KEY
                      MOVE USUARIO-W   TO LOG4-USUARIO
                      MOVE FUNCTION CURRENT-DATE
                                       TO WS-DATA-SYS
                      MOVE WS-DATA-CPU TO LOG4-DATA
                      ACCEPT WS-HORA-SYS FROM TIME
                      MOVE WS-HORA-SYS TO LOG4-HORAS
                      MOVE "I"         TO LOG4-OPERACAO
                      MOVE "CHD010"    TO LOG4-ARQUIVO
                      MOVE "RCP100"    TO LOG4-PROGRAMA
                      MOVE REG-CHD010  TO LOG4-REGISTRO
                      WRITE REG-LOG004
                      END-WRITE
                      MOVE "10" TO ST-CHD010
                END-WRITE
           END-PERFORM.
           CLOSE CHD010 CHD011 CHD099 CHD010B.

       GERAR-BAIXA SECTION.
           INITIALIZE REG-CHD010B
           MOVE DATA-MOVTO-CH10                 TO DATA-MOVTO-CH10B
           MOVE SEQ-CH10                        TO SEQ-CH10B
           MOVE DATA-MOVTO-CH10                 TO DATA-RCTO-CH10B
           MOVE VALOR-CH10                      TO VALOR-TOT-CH10B
           MOVE VALOR-CH10                      TO VALOR-BAIXA-CH10B
           MOVE VALOR-CH10                      TO VALOR-LIQ-CH10B
           MOVE SEQ-CAIXA-CH10                  TO SEQ-CAIXA-CH10B
           MOVE "1-Moeda"                       TO FORMA-PAGTO-CH10B

           WRITE REG-CHD010B INVALID KEY
                MOVE "Erro de Gravação...CHD010B" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE USUARIO-W   TO LOG4-USUARIO
                MOVE FUNCTION CURRENT-DATE
                                 TO WS-DATA-SYS
                MOVE WS-DATA-CPU TO LOG4-DATA
                ACCEPT WS-HORA-SYS FROM TIME
                MOVE WS-HORA-SYS TO LOG4-HORAS
                MOVE "I"         TO LOG4-OPERACAO
                MOVE "CHD010B"   TO LOG4-ARQUIVO
                MOVE "RCP100"    TO LOG4-PROGRAMA
                MOVE REG-CHD010B TO LOG4-REGISTRO
                WRITE REG-LOG004
                END-WRITE.

       ACHA-SEQ-CHD011 SECTION.
           READ CHD011 INVALID KEY
                MOVE 1 TO SEQ-CH11
                WRITE REG-CHD011
                END-WRITE
              NOT INVALID KEY ADD 1 TO SEQ-CH11
                              REWRITE REG-CHD011.

      *    GRAVA-CHD099

       GRAVA-DUPLICATA SECTION.
           OPEN I-O CRD020 CRD021 CRD024.

           INITIALIZE REG-CRD020.
           PERFORM ACHAR-SEQUENCIA-CRD021.
           MOVE SEQ-CR21           TO SEQ-CR20
           MOVE COD-COMPL-CG10     TO COD-COMPL-CR20
           MOVE 1                  TO CARTEIRA-CR20
           MOVE GS-VENDEDOR        TO VENDEDOR-CR20
           EVALUATE TIPO-REC1
               WHEN 4     MOVE 0   TO TIPO-DOCTO-CR20
               WHEN 5     MOVE 3   TO TIPO-DOCTO-CR20
               WHEN 6     MOVE 4   TO TIPO-DOCTO-CR20
               WHEN OTHER MOVE 0   TO TIPO-DOCTO-CR20
           END-EVALUATE
           MOVE CARTAO-CRED-REC1   TO CARTAO-CRED-CR20
           MOVE TAXA-ADMINIST-CREDITO-REC1
                                   TO TAXA-ADMINIST-CREDITO-CR20
           MOVE TAXA-ADMINIST-PARCELA-REC1
                                   TO TAXA-ADMINIST-PARCELA-CR20
           MOVE CODIGO-CG10(1: 4)  TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                INITIALIZE REG-COD040
           END-READ
           MOVE INSTITUICAO-CO40   TO CODIGO-IE10.
           READ IED010 INVALID KEY
                MOVE SPACES TO SIGLA-IE10
           END-READ
           MOVE SIGLA-IE10         TO DESCRICAO-CR20(1: 15)
           MOVE " - "              TO DESCRICAO-CR20(16: 3)
           MOVE IDENTIFICACAO-CO40 TO DESCRICAO-CR20(19: 12)
           MOVE 0                  TO SITUACAO-CR20
           MOVE 0                  TO TIPO-MOEDA-CR20
           ADD 1 TO NR-PARCELA.
           MOVE PARCELA-REC1       TO NR-PARC-CR20
           MOVE QT-PARCELA-REC1    TO TOT-PARC-CR20
           MOVE CODRED-FATURAMENTO-PAR001 TO CODREDUZ-APUR-CR20
           MOVE "CPD"              TO RESPONSAVEL-CR20
                                      DIGITADOR-CR20
           MOVE DATA-MOVTO-REC     TO DATA-MOVTO-CR20
           MOVE GS-DATA-VENDA      TO DATA-EMISSAO-CR20
           MOVE VENCTO-WK          TO DATA-VENCTO-CR20
           MOVE GS-PORTADOR        TO PORTADOR-CR20
           MOVE VALOR-WK           TO VALOR-TOT-CR20
           MOVE VALOR-WK           TO VALOR-SALDO-CR20

           MOVE VALOR-TOT-CR20     TO TAB-VALOR

           IF TAB-VALOR1 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR1
           END-IF
           IF TAB-VALOR2 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR2
           END-IF
           IF TAB-VALOR3 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR3
           END-IF
           IF TAB-VALOR4 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR4
           END-IF
           IF TAB-VALOR5 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR5
           END-IF
           IF TAB-VALOR6 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR6
           END-IF
           IF TAB-VALOR7 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR7
           END-IF
           IF TAB-VALOR8 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR8
           END-IF
           IF TAB-VALOR9 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR9
           END-IF
           IF TAB-VALOR10 IS NOT NUMERIC
              MOVE 0 TO TAB-VALOR10
           END-IF

           MOVE TAB-VALOR TO VALOR-TOT-CR20.

           MOVE SPACES            TO NR-DOCTO-CR20
           STRING CODIGO-CG10(1:4) NR-CHEQUE-WK INTO NR-DOCTO-CR20.
           MOVE ZEROS TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                WRITE REG-CRD020 INVALID KEY
                      PERFORM ACHAR-SEQUENCIA-CRD021
                      MOVE SEQ-CR21 TO SEQ-CR20
                NOT INVALID KEY
                      MOVE USUARIO-W   TO LOG4-USUARIO
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      MOVE WS-DATA-CPU TO LOG4-DATA
                      ACCEPT WS-HORA-SYS FROM TIME
                      MOVE WS-HORA-SYS TO LOG4-HORAS
                      MOVE "I"         TO LOG4-OPERACAO
                      MOVE "CRD020"    TO LOG4-ARQUIVO
                      MOVE "RCP100"    TO LOG4-PROGRAMA
                      MOVE REG-CRD020  TO LOG4-REGISTRO
                      WRITE REG-LOG004
                      END-WRITE

                    MOVE ALL "9"            TO CLASS-CLIENTE-CR24
                    MOVE ALL "9"            TO CLIENTE-CR24
                    MOVE ALL "9"            TO SEQ-CR24
                    READ CRD024 INVALID KEY
                         MOVE 1                       TO NUMERO
                    NOT INVALID KEY
                         MOVE NUMERO-PROGRAMACAO-CR24 TO NUMERO
                         ADD  1                       TO NUMERO
                    END-READ

                    MOVE CLASS-CLIENTE-CR21 TO CLASS-CLIENTE-CR24
                    MOVE CLIENTE-CR21       TO CLIENTE-CR24
                    MOVE SEQ-CR21           TO SEQ-CR24

                    READ CRD024 INVALID KEY
                         MOVE NUMERO        TO NUMERO-PROGRAMACAO-CR24
                         WRITE REG-CRD024 NOT INVALID KEY
                              MOVE USUARIO-W   TO LOG4-USUARIO
                              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                              MOVE WS-DATA-CPU TO LOG4-DATA
                              ACCEPT WS-HORA-SYS FROM TIME
                              MOVE WS-HORA-SYS TO LOG4-HORAS
                              MOVE "I"         TO LOG4-OPERACAO
                              MOVE "CRD024"    TO LOG4-ARQUIVO
                              MOVE "RCP100"    TO LOG4-PROGRAMA
                              MOVE REG-CRD024  TO LOG4-REGISTRO
                              WRITE REG-LOG004
                              END-WRITE

                              MOVE ALL "9"  TO CLASS-CLIENTE-CR24
                              MOVE ALL "9"  TO CLIENTE-CR24
                              MOVE ALL "9"  TO SEQ-CR24
                              READ CRD024 INVALID KEY
                                   MOVE NUMERO TO
                                        NUMERO-PROGRAMACAO-CR24
                                   WRITE REG-CRD024 NOT INVALID KEY
                                       MOVE USUARIO-W   TO LOG4-USUARIO
                                       MOVE FUNCTION CURRENT-DATE
                                                        TO WS-DATA-SYS
                                       MOVE WS-DATA-CPU TO LOG4-DATA
                                       ACCEPT WS-HORA-SYS FROM TIME
                                       MOVE WS-HORA-SYS TO LOG4-HORAS
                                       MOVE "I"         TO LOG4-OPERACAO
                                       MOVE "CRD024"    TO LOG4-ARQUIVO
                                       MOVE "RCP100"    TO LOG4-PROGRAMA
                                       MOVE REG-CRD024  TO LOG4-REGISTRO
                                       WRITE REG-LOG004
                                       END-WRITE
                                   END-WRITE
                              NOT INVALID KEY
                                   MOVE NUMERO TO
                                        NUMERO-PROGRAMACAO-CR24
                                   REWRITE REG-CRD024 NOT INVALID KEY
                                       MOVE USUARIO-W   TO LOG4-USUARIO
                                       MOVE FUNCTION CURRENT-DATE
                                                        TO WS-DATA-SYS
                                       MOVE WS-DATA-CPU TO LOG4-DATA
                                       ACCEPT WS-HORA-SYS FROM TIME
                                       MOVE WS-HORA-SYS TO LOG4-HORAS
                                       MOVE "A"         TO LOG4-OPERACAO
                                       MOVE "CRD024"    TO LOG4-ARQUIVO
                                       MOVE "RCP100"    TO LOG4-PROGRAMA
                                       MOVE REG-CRD024  TO LOG4-REGISTRO
                                       WRITE REG-LOG004
                                       END-WRITE
                                   END-REWRITE
                              END-READ
                         END-WRITE
                    END-READ
                    MOVE "10" TO ST-CRD020
                END-WRITE
           END-PERFORM.

           CLOSE CRD020 CRD021 CRD024.
       ACHAR-SEQUENCIA-CRD021 SECTION.
           MOVE COD-COMPL-CG10 TO COD-COMPL-CR21.
           READ CRD021 INVALID KEY
                MOVE 1 TO SEQ-CR21
                WRITE REG-CRD021
                END-WRITE
             NOT INVALID KEY
                 ADD 1 TO SEQ-CR21
                 REWRITE REG-CRD021.
       CADASTRA-CLIENTE SECTION.
           CLOSE    CGD010
           OPEN I-O CGD010
           MOVE 0             TO COD-COMPL-CG10(1: 1)
           MOVE GS-NR-ALBUM   TO COD-COMPL-CG10(2: 4)
           MOVE GS-NR-FORM    TO COD-COMPL-CG10(6: 4).

           READ CGD010 INVALID KEY
                    MOVE GS-NOME   TO COMPRADOR-CG10
                    WRITE REG-CGD010
                    END-WRITE
           NOT INVALID KEY
                    MOVE GS-NOME   TO COMPRADOR-CG10
                    REWRITE REG-CGD010
                    END-REWRITE
           END-READ

           CLOSE CGD010
           OPEN INPUT CGD010.

      *-------------------------------------------------------
       CALCULA-PRAZO-MEDIO SECTION.
           MOVE ZEROS TO GS-TOT-VENDA GS-TOT-VENDA-DEF
                         DEFLACIONADO-W TOTALPM-W.
           MOVE ZEROS TO NR-CHEQUE-WK BANCO-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                 PERFORM CALCULA-DIAS
                 MOVE TIPO-WK(1: 1) TO TIPO-W
                 EVALUATE TIPO-W
                   WHEN 1
      *              COMPUTE VALOR-W = VALOR-WK / VALOR-060
                     ADD VALOR-WK TO GS-TOT-VENDA
                     PERFORM DEFLACIONA
                     ADD DEF1-W TO  DEFLACIONADO-W
                     COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) + TOTALPM-W
                   WHEN 2
                     ADD VALOR-WK TO DEFLACIONADO-W, GS-TOT-VENDA
                     COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) + TOTALPM-W
                   WHEN 3
                     ADD VALOR-WK TO DEFLACIONADO-W, GS-TOT-VENDA
                     COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) + TOTALPM-W
                   WHEN 4
      *              COMPUTE VALOR-W = VALOR-WK / VALOR-060
                     ADD VALOR-WK TO GS-TOT-VENDA
                     PERFORM DEFLACIONA
                     ADD DEF1-W TO  DEFLACIONADO-W
                     COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) + TOTALPM-W
                   WHEN 5
      *              COMPUTE VALOR-W = VALOR-WK / VALOR-060
                     ADD VALOR-WK TO GS-TOT-VENDA
                     PERFORM DEFLACIONA
                     ADD DEF1-W TO  DEFLACIONADO-W
                     COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) + TOTALPM-W
                   WHEN 6
      *              COMPUTE VALOR-W = VALOR-WK / VALOR-060
                     ADD VALOR-WK TO GS-TOT-VENDA
                     PERFORM DEFLACIONA
                     ADD DEF1-W TO  DEFLACIONADO-W
                     COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) + TOTALPM-W
                 END-EVALUATE
             END-READ
           END-PERFORM.
           COMPUTE GS-TOT-PM ROUNDED = TOTALPM-W / GS-TOT-VENDA.
           MOVE DEFLACIONADO-W    TO GS-TOT-VENDA-DEF.
       CALCULA-DIAS SECTION.
           MOVE 2               TO GRTIME-TYPE
           MOVE 3               TO GRTIME-FUNCTION
           MOVE VENCTO-WK       TO GRTIME-DATE-FINAL.
           MOVE GS-DATA-VENDA   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV        TO GRTIME-DATE.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           MOVE GRTIME-DAYS-FINAL TO CONT-W.

       DEFLACIONA SECTION.
           COMPUTE TAXA-W = (((GS-TAXA / 30) / 100) * CONT-W) + 1
           COMPUTE DEF1-W ROUNDED = VALOR-WK / TAXA-W.
      *-----------------------------------------------------------

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       SOMAR-VALOR SECTION.
           INITIALIZE GS-TOTAL-REC
                      GS-TOTAL-DESCCOM

           MOVE ZEROS TO NR-CHEQUE-WK
                         BANCO-WK

           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END
                  MOVE "10" TO ST-WORK
             NOT AT END
                  EVALUATE TIPO-WK(1:1)
                    WHEN "9"   ADD VALOR-WK TO GS-TOTAL-DESCCOM
                    WHEN OTHER ADD VALOR-WK TO GS-TOTAL-REC
                  END-EVALUATE
             END-READ
           END-PERFORM

           REFRESH-OBJECT PRINCIPAL.

       SOMAR-VALOR-FIM.
           EXIT.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD004 CGD001 CGD010 COD040 MTD020 IED010 CAD010 MTD019
                 RCD100 RCD101 WORK CGD020 RCD001 CAD018 RCD002 CAD030
                 CRD024 PAR001 RCD100C MTD020C MTD020E RCD100E
                 LOG002 LOG003 LOG004

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "RCP100"            to logacess-programa
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

           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
