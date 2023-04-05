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
           copy RCPX001.
           COPY RCPX002.
           COPY RCPX100.
           COPY RCPX100P.
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
           COPY CADPRO.SEL.
           COPY CADMOD.SEL.
           COPY MTPX020P.

          SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-WK = NR-CHEQUE-WK BANCO-WK
                  ALTERNATE RECORD KEY IS ALT-WK = VENCTO-WK
                  NR-CHEQUE-WK BANCO-WK.

          SELECT RELAT ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW010.
       COPY CAPW018.
       COPY CAPW030.
       COPY MTPW019.
       COPY MTPW020.
       COPY RCPW001.
       COPY RCPW002.
       COPY RCPW100.
       COPY RCPW100P.
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
       COPY PARW001.
       COPY CADPRO.FD.
       COPY CADMOD.FD.
       COPY MTPW020P.

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
           05  ST-RCD100P            PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
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
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-CADPRO             PIC XX       VALUE SPACES.
           05  ST-CADMOD             PIC XX       VALUE SPACES.
           05  ST-MTD020P            PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  AUX-CONTRATO          PIC 9(08)    VALUE ZEROS.
           05  AUX-ALBUM-REC         PIC 9(11)    VALUE ZEROS.
           05  EXTRA-ENVIADO1-W      PIC X(01)    VALUE SPACES.
           05  EXTRA-ENVIADO2-W      PIC X(01)    VALUE SPACES.
           05  AUX-COD-COMPL         PIC 9(09)    VALUE ZEROS.
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
           05  SEQ-W                 PIC 9(3)          VALUE ZEROS.
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
           05  MASC-VALOR            PIC ZZZ.ZZ9,99 BLANK WHEN ZEROS.
           05  MASC-QTDE             PIC ZZZ.ZZ9    BLANK WHEN ZEROS.
           05  ACHEI                 PIC X(01)      VALUE SPACES.
           05  AUX-DIA               PIC 9(02).
           05  AUX-MES               PIC 9(02).
           05  AUX-ANO               PIC 9(04).
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  WS-STATUS-ANALISE     PIC 9(02) VALUE ZEROS.
           05  WS-STATUS-REVENDIDO   PIC 9(02) VALUE ZEROS.
           05  WS-OK                 PIC X(01) VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

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

       01 lnktabelaPro.
          02 lnkobjetoscolPro  object reference occurs 99 times.
       01 lnktabelaColPro.
          02 lnkcolunasPro pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabelaPagto.
          02 lnkobjetoscolPagto object reference occurs 99 times.
       01 lnktabelaColPagto.
          02 lnkcolunasPagto
                           pic 9(09) comp-5 value zeros occurs 99 times.

       01 indice                       pic 9(02).

       01 wssize                       pic 9(09) comp-5 value zeros.
       01 wsIndice                     pic 9(09) comp-5 value zeros.
       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 umitem                       object reference.
       77 umobjeto                     object reference.
       01 data-baixa                   pic 9(08) value zeros.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU             PIC 9(04).
             10 WS-MES-CPU             PIC 9(02).
             10 WS-DIA-CPU             PIC 9(02).
          05 FILLER                    PIC X(13).

       01 lnkusu.
          copy usuario.cpy.

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
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CAD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD030.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "RCD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD001.
           MOVE "RCD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD002.
           MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD100P" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100P.
           MOVE "RCD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD101.
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "IED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD010B" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010B.
           MOVE "CHD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD011.
           MOVE "CHD099" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD099.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD021" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD021.
           MOVE "CRD024" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD024.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "PAR001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "CADPRO" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADPRO
           MOVE "CADMOD" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADMOD
           MOVE "MTD020P" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020P

           OPEN INPUT CGD001 IED010 COD040 CAD004 CAD010 MTD019 RCD001
                      CAD018 CAD030 PAR001 CADPRO CADMOD.

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario

           OPEN I-O RCD100 RCD101 CGD010 MTD020 CHD010 CHD011 CHD099
                    CRD020 CRD021 CRD024 CGD020 RCD002 CHD010B MTD020P
                    RCD100P.

           CLOSE    CRD024 MTD020P RCD100P
           OPEN I-O CRD024 MTD020P RCD100P

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
           IF ST-RCD100P = "35"
              CLOSE RCD100P     OPEN OUTPUT RCD100P
              CLOSE RCD100P     OPEN I-O RCD100P
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
           CLOSE CHD010 CHD011 CHD099 CRD020 CRD021 CHD010B MTD020P.

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
           IF ST-RCD100P <> "00"
              MOVE "ERRO ABERTURA RCD100P: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100P TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-CADPRO <> "00"
              MOVE "ERRO ABERTURA CADPRO: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADPRO TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADMOD <> "00"
              MOVE "ERRO ABERTURA CADMOD: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADMOD TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020P <> "00"
              MOVE "ERRO ABERTURA MTD020P: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020P TO GS-MENSAGEM-ERRO(23: 02)
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
                      RCD100P
           OPEN INPUT RCD100 RCD101 CGD010 MTD020 RCD002 CGD020 CRD024
                      CHD010B MTD020P RCD100P


           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "SENHA11"     TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY MOVE 0 TO GS-LIBERADO
                   NOT INVALID KEY MOVE 1 TO GS-LIBERADO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
                    PERFORM CRIAR-LISTVIEW-PRODUTOS
                    PERFORM CRIAR-LISTVIEW-PAGTO
               WHEN GS-CONFERE-RECIBO-TRUE
                    PERFORM CONFERE-RECIBO
               WHEN GS-SAVE-FLG-TRUE
                    PERFORM GRAVA-RECIBO
                    PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI
                    PERFORM LIMPAR-DADOS
               WHEN GS-INCLUI-CHEQUE-TRUE
                    PERFORM GRAVAR-CHEQUE-WORK
                    PERFORM MOSTRAR-COLUNAS-FAVOPAGTO
                    PERFORM MOSTRAR-FONTE-FAVOPAGTO
                    PERFORM ZEBRAR-ITENSPAGTO
               WHEN GS-ADICIONA-CHEQUE-TRUE
                    PERFORM GRAVAR-CHEQUE-WORK
                    PERFORM ADICIONA-MES-CHEQUE
                    PERFORM MOSTRAR-COLUNAS-FAVOPAGTO
                    PERFORM MOSTRAR-FONTE-FAVOPAGTO
                    PERFORM ZEBRAR-ITENSPAGTO
               WHEN GS-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUIR-PARCELAS-TRUE
                    PERFORM EXCLUIR-PARCELAS
               WHEN GS-CARREGA-COMISSAO-TRUE
                    PERFORM CARREGAR-COMISSAO
               WHEN GS-IMPRIMIR-PARCELAS-TRUE
                    PERFORM IMPRIMIR-PARCELAS
               WHEN GS-CRITICAR-TRUE
                    PERFORM CRITICAR-CAMPOS
               WHEN GS-SUGESTAO-TRUE
                    PERFORM SUGESTAO
               WHEN GS-TRATAR-EVENTO-TRUE
                    PERFORM TRATAR-EVENTO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       TRATAR-EVENTO SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34013
                    EVALUATE GS-QUAL-LIST
                        WHEN "1" PERFORM ALTERAR-PRODUTO
                        WHEN "2" PERFORM ALTERAR-VENCIMENTO
                    END-EVALUATE
               WHEN 34592
                    EVALUATE GS-QUAL-LIST
                        WHEN "1" PERFORM ALTERAR-PRODUTO
                        WHEN "2" PERFORM ALTERAR-VENCIMENTO
                                 PERFORM SOMAR-PAGAMENTO
                    END-EVALUATE
               WHEN 34046
                    EVALUATE GS-QUAL-LIST
                        WHEN "1" PERFORM EXCLUIR-PRODUTO
                                 PERFORM SOMAR-PRODUTOS
                        WHEN "2" PERFORM EXCLUIR-VENCIMENTO
                                 PERFORM SOMAR-PAGAMENTO
                    END-EVALUATE
               WHEN 34026
                    EVALUATE GS-QUAL-LIST
                        WHEN "1" SET-FOCUS EF13
                        WHEN "2" SET-FOCUS EF14
                    END-EVALUATE
               WHEN 34123
                    EVALUATE GS-QUAL-LIST
                        WHEN "1" perform chamar-colunas-favoPro
                        WHEN "2" perform chamar-colunas-favoPagto
                    END-EVALUATE
           END-EVALUATE.

       ALTERAR-PRODUTO SECTION.
           INITIALIZE WSITEM
           INVOKE GS-LISTVIEW-PRODUTOS "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INVOKE GS-LISTVIEW-PRODUTOS "INDEXOF" USING UMITEM
                                                RETURNING WSITEM
      *>Coluna Identificação
              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(1)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE FUNCTION NUMVAL(WSTEXTO)     TO GS-ACP-IDENTIFICACAO
      *>Coluna Produto
              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(2)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE WSTEXTO                      TO GS-ACP-PRODUTO
      *>Coluna Modelo
              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(3)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE WSTEXTO                      TO GS-ACP-MODELO
      *>Coluna Qtde Planilhada
              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(4)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE FUNCTION NUMVAL(WSTEXTO)    TO GS-ACP-QTDE-PLANILHADA
      *>Coluna Qtde Vendidade
              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(5)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE FUNCTION NUMVAL(WSTEXTO)    TO GS-ACP-QTDE-VENDIDA
      *>Coluna Valor Unitário
              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(6)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE FUNCTION NUMVAL(WSTEXTO)    TO GS-ACP-VLRUNI
      *>Coluna Valor Total
              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(7)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE FUNCTION NUMVAL(WSTEXTO)    TO GS-ACP-VLRTOTAL

              REFRESH-OBJECT WIN5
              SHOW-WINDOW WIN5
              SET-FOCUS EF-IDETIFICACAO.

       ALTERAR-VENCIMENTO SECTION.
           INITIALIZE WSITEM
           INVOKE GS-LISTVIEW-PAGTO "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INVOKE GS-LISTVIEW-PAGTO "INDEXOF" USING UMITEM
                                             RETURNING WSITEM
      *>Coluna data Baixa
              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(8)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE FUNCTION NUMVAL(WSTEXTO)     TO DATA-BAIXA
              IF DATA-BAIXA > 0
                 MOVE "Parcela Não Pode ser Alterada, pois já foi Baixad
      -               "a" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              ELSE
      *>Vencimento
                 INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(1)
                                            RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-VENCTO-CH
      *>Valor
                 INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(2)
                                            RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-VALOR-CH
      *>Número Documento
                 INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(3)
                                            RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-NR-CHEQUE
      *>Banco
                 INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(4)
                                            RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-BANCO-CH
      *>Parcela
                 INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(5)
                                            RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-PARCELA
      *>Qtde Parcela
                 INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(6)
                                            RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-QT-PARCELA
      *>Tipo
                 INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(7)
                                            RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE WSTEXTO                  TO GS-TIPO-CH
      *>Cartão
                 INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(9)
                                            RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO(1:2)) TO GS-CARTAO

                 MOVE WSTEXTO(4:30)                 TO GS-DESC-CARTAO

                 REFRESH-OBJECT PRINCIPAL
                 REFRESH-OBJECT WIN2
                 SET-FOCUS EF14

                 INVOKE UMITEM "FINALIZE" RETURNING UMITEM.

       EXCLUIR-PRODUTO SECTION.
           INITIALIZE WSITEM
           INVOKE GS-LISTVIEW-PRODUTOS "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INVOKE GS-LISTVIEW-PRODUTOS "INDEXOF" USING UMITEM
                                             RETURNING WSITEM
      *>Coluna Qtde Vendida
              INITIALIZE WSTEXTO
              STRING X"00" INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSITEM LNKCOLUNASPRO(5) WSTEXTO

      *>Coluna Valor Unitário
              INITIALIZE WSTEXTO
              STRING X"00" INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSITEM LNKCOLUNASPRO(6) WSTEXTO

      *>Coluna Valor Total
              INITIALIZE WSTEXTO
              STRING X"00" INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSITEM LNKCOLUNASPRO(7) WSTEXTO.

       EXCLUIR-VENCIMENTO SECTION.
           INITIALIZE WSITEM
           INVOKE GS-LISTVIEW-PAGTO "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INVOKE GS-LISTVIEW-PAGTO "INDEXOF" USING UMITEM
                                             RETURNING WSITEM
      *>Coluna data Baixa
              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(8)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE FUNCTION NUMVAL(WSTEXTO)     TO DATA-BAIXA
              IF DATA-BAIXA > 0
                 MOVE "Parcela Não Pode ser Alterada, pois já foi Baixad
      -               "a" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              ELSE
      *>Coluna Nr. Docto
                 INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPAGTO(3)
                                            RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO) TO NR-CHEQUE-WK
                 READ WORK INVALID KEY
                      MOVE "Número de Documento Não Encontrado na WORK"
                               TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                 NOT INVALID KEY
                      DELETE WORK INVALID KEY
                             MOVE "Erro de Exclusão...WORK" TO MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                             INVOKE UMITEM "FINALIZE" RETURNING UMITEM
                      END-DELETE
                 END-READ.

       SUGESTAO SECTION.
           EVALUATE GS-CAMPO-CRITICA
               WHEN "EF-PORTADOR"     PERFORM SUGESTAO-PORTADOR
               WHEN "EF-VENDEDOR"     PERFORM SUGESTAO-VENDEDOR
               WHEN "EF-CARTAO"       PERFORM SUGESTAO-CARTAO
               WHEN OTHER MOVE "Sugestão Inexistente" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.

       SUGESTAO-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(1: 20)  TO GS-DESC-PORTADOR
           MOVE PASSAR-STRING-1(33: 4)  TO GS-PORTADOR
           MOVE GS-PORTADOR             TO PORTADOR
           READ CAD018 NOT INVALID KEY
               MOVE NOME-PORT           TO GS-DESC-PORTADOR
           END-READ
           REFRESH-OBJECT PRINCIPAL.

       SUGESTAO-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-VENDEDOR
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR
           REFRESH-OBJECT PRINCIPAL.

       SUGESTAO-CARTAO SECTION.
           CALL   "CGP020T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP020T"
           MOVE PASSAR-STRING-1(1:30)  TO GS-DESC-CARTAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-CARTAO
           REFRESH-OBJECT WIN2.

       CRITICAR-CAMPOS SECTION.
           EVALUATE GS-CAMPO-CRITICA
               WHEN "EF-MOVTO"        PERFORM CRITICAR-MOVTO
               WHEN "EF-PORTADOR"     PERFORM CRITICAR-PORTADOR
               WHEN "EF-CONTRATO"     PERFORM CRITICAR-CONTRATO
               WHEN "EF-ALBUM"        PERFORM CRITICAR-ALBUM
               WHEN "EF-SEQ"          PERFORM CRITICAR-SEQUENCIA
               WHEN "EF-VENDEDOR"     PERFORM CRITICAR-VENDEDOR
               WHEN "EF-DTVENDA"      PERFORM CRITICAR-DTVENDA
      *FORMA DE PAGAMENTO
               WHEN "EF-VENCTO"       PERFORM CRITICAR-DTVENCTO
               WHEN "EF-BANCO"        PERFORM CRITICAR-BANCO
               WHEN "SB-TIPO"         PERFORM CRITICAR-TIPO
               WHEN "EF-CARTAO"       PERFORM CRITICAR-CARTAO
               WHEN "INCLUIR-VENCTO"  PERFORM CRITICAR-DTVENCTO
                                         THRU CRITICAR-CARTAO
      *PRODUTOS
               WHEN "EF-IDENTIFICACAO"  PERFORM CRITICAR-IDENTIFICACAO
               WHEN "EF-QTDE-VENDIDA"   PERFORM CRITICAR-QTDE-VENDIDA
               WHEN "EF-VLRUNI"         PERFORM CRITICAR-VLRUNI
               WHEN "EF-VLRTOTAL"       PERFORM CRITICAR-VLRTOTAL
               WHEN "VERIFICAR-PRODUTO" PERFORM CRITICAR-IDENTIFICACAO
                                           THRU CRITICAR-VLRTOTAL
                                        IF GS-FLAG-CRITICA = 0
                                           PERFORM INCLUIR-PRODUTO
                                        END-IF
                                        PERFORM SOMAR-PRODUTOS
           END-EVALUATE.

       CRITICAR-MOVTO SECTION.
           IF GS-DATA-MOVTO = ZEROS
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO GS-DATA-MOVTO
              REFRESH-OBJECT PRINCIPAL
           ELSE
              CALL   "UTIVLDT" USING GS-DATA-MOVTO WS-OK
              CANCEL "UTIVLDT"
              IF WS-OK = "N"
                 MOVE "Data de Movimento Inválida" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM.

       CRITICAR-PORTADOR SECTION.
           IF GS-PORTADOR EQUAL ZEROS
              MOVE "Código do Portador Não Informado" TO MENSAGEM
              MOVE "C"                                TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE GS-PORTADOR             TO PORTADOR
              READ CAD018 INVALID KEY
                  MOVE "Portador Inválido" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
                  MOVE SPACES              TO GS-DESC-PORTADOR
              NOT INVALID KEY
                  MOVE NOME-PORT           TO GS-DESC-PORTADOR
              END-READ
              REFRESH-OBJECT PRINCIPAL.

       CRITICAR-CONTRATO SECTION.
           IF GS-NR-ALBUM = ZEROS
              MOVE "Número do Álbum não Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
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
                   END-EVALUATE.

       CRITICAR-ALBUM SECTION.
           IF GS-NR-FORM = ZEROS
              MOVE "Número do Álbum Não Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       CRITICAR-SEQUENCIA SECTION.
           PERFORM VERIFICA-ALBUM.

       CRITICAR-VENDEDOR SECTION.
           IF GS-VENDEDOR EQUAL ZEROS
              MOVE "Vendedor Não Informado" TO MENSAGEM
              MOVE "C"                      TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE GS-VENDEDOR              TO CODIGO-CG01
              READ CGD001 INVALID KEY
                   MOVE "********"          TO NOME-CG01
              END-READ
              MOVE NOME-CG01                TO GS-NOME-VENDEDOR
              REFRESH-OBJECT PRINCIPAL.

       CRITICAR-DTVENDA SECTION.
           IF GS-DATA-VENDA EQUAL ZEROS
              MOVE FUNCTION CURRENT-DATE    TO WS-DATA-SYS
              STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO GS-DATA-VENDA
              REFRESH-OBJECT PRINCIPAL
           ELSE
              CALL   "UTIVLDT" USING GS-DATA-VENDA WS-OK
              CANCEL "UTIVLDT"
              IF WS-OK EQUAL "N"
                 MOVE "Data da Venda Inválida" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DTVENCTO SECTION.
           IF GS-VENCTO-CH EQUAL ZEROS
              MOVE FUNCTION CURRENT-DATE    TO WS-DATA-SYS
              STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO GS-VENCTO-CH
              REFRESH-OBJECT PRINCIPAL
           ELSE
              CALL   "UTIVLDT" USING GS-VENCTO-CH WS-OK
              CANCEL "UTIVLDT"
              IF WS-OK EQUAL "N"
                 MOVE "Data de Vencimento Inválida" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM.

       CRITICAR-BANCO SECTION.
           IF GS-BANCO-CH > ZEROS
              MOVE GS-BANCO-CH TO CODIGO-CAD30
              READ CAD030 INVALID KEY
                  MOVE "Código do Banco Inválido" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM.

       CRITICAR-TIPO SECTION.
           IF GS-TIPO-CH EQUAL SPACES
              MOVE "Tipo do Documento Não Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              IF GS-TIPO-CH(1:1) = "6"
                 IF GS-CARTAO = 0
                    SHOW-WINDOW WIN2
                    DISABLE-OBJECT PB5
                    SET-FOCUS EF57
                 ELSE
                    SET-FOCUS PB4
                 END-IF
              ELSE
                 MOVE 0   TO GS-CARTAO
                 MOVE 0   TO GS-TAXA-ADMINIST-CREDITO
                 MOVE " " TO GS-DESC-CARTAO
                 REFRESH-OBJECT WIN2
                 SET-FOCUS PB4.

       CRITICAR-CARTAO SECTION.
           IF GS-TIPO-CH(1:1) = "6"
              IF GS-CARTAO EQUAL ZEROS
                 MOVE "Cartão de Crédito Não Informado" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
                 DISABLE-OBJECT PB5
              ELSE
                 MOVE GS-CARTAO TO CODIGO-CG20
                 READ CGD020 INVALID KEY
                      MOVE "Cartão de Crédito Inválido" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                      DISABLE-OBJECT PB5
                 NOT INVALID KEY
                      MOVE CODIGO-CG20       TO GS-CARTAO
                      MOVE NOME-CG20         TO GS-DESC-CARTAO
                      MOVE TAXA-CREDITO-CG20 TO GS-TAXA-ADMINIST-CREDITO
                      MOVE TAXA-PARCELA-CG20 TO GS-TAXA-ADMINIST-PARCELA
                      REFRESH-OBJECT WIN2
                      ENABLE-OBJECT PB5
                      SET-FOCUS PB5.

       CRITICAR-IDENTIFICACAO SECTION.
           IF GS-ACP-IDENTIFICACAO EQUAL ZEROS
              MOVE "Identificação do Produto Não Informada" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE GS-ACP-IDENTIFICACAO(1:4)    TO CADPRO-CODIGO
              MOVE GS-ACP-IDENTIFICACAO(5:4)    TO CADPRO-MODELO
              READ CADPRO INVALID KEY
                   MOVE "Produto Inválido" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   MOVE CADPRO-NOME        TO GS-ACP-PRODUTO
                   MOVE CADPRO-MODELO      TO CADMOD-CODIGO
                   READ CADMOD INVALID KEY
                        MOVE SPACES        TO CADMOD-NOME
                   END-READ
                   MOVE CADMOD-NOME        TO GS-ACP-MODELO
                   REFRESH-OBJECT WIN5.

       CRITICAR-QTDE-VENDIDA SECTION.
           IF GS-ACP-QTDE-VENDIDA EQUAL ZEROS
              MOVE GS-ACP-QTDE-PLANILHADA TO GS-ACP-QTDE-VENDIDA
              REFRESH-OBJECT WIN5
           ELSE
              IF GS-ACP-QTDE-VENDIDA > GS-ACP-QTDE-PLANILHADA
                 MOVE "Quantidade Vendida Informada Maior que a Planilha
      -               "da" TO MENSAGEM
                 MOVE "C"  TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM.

       CRITICAR-VLRUNI SECTION.
           IF GS-ACP-VLRUNI > 0
              IF GS-ACP-VLRUNI * GS-ACP-QTDE-VENDIDA > GS-ACP-VLRTOTAL
                 COMPUTE GS-ACP-VLRTOTAL ROUNDED = GS-ACP-VLRUNI *
                                                   GS-ACP-QTDE-VENDIDA
                 REFRESH-OBJECT WIN5.

       CRITICAR-VLRTOTAL SECTION.
           IF GS-ACP-VLRTOTAL EQUAL ZEROS
              MOVE "Valor Total do Produto Não Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              COMPUTE GS-ACP-VLRUNI ROUNDED = GS-ACP-VLRTOTAL /
                                                GS-ACP-QTDE-VENDIDA
              REFRESH-OBJECT WIN5.

       INCLUIR-PRODUTO SECTION.
           MOVE "N" TO ACHEI

           INITIALIZE WSINDICE
           INVOKE GS-LISTVIEW-PRODUTOS "SIZE" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
                 ADD 1 TO WSINDICE
                 INVOKE GS-LISTVIEW-PRODUTOS "ITEMATINDEX"
                        USING WSINDICE RETURNING UMITEM
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNASPRO(1) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 IF GS-ACP-IDENTIFICACAO = WSTEXTO(1:12)
                    MOVE "S" TO ACHEI
                    EXIT PERFORM
                 END-IF
           END-PERFORM

           IF ACHEI = "S"
      *>Coluna Qtde Vendida
              INITIALIZE WSTEXTO
              MOVE GS-ACP-QTDE-VENDIDA      TO MASC-QTDE
              STRING MASC-QTDE X"00"      INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSINDICE LNKCOLUNASPRO(5) WSTEXTO

      *>Coluna Valor Unitário
              INITIALIZE WSTEXTO
              MOVE GS-ACP-VLRUNI            TO MASC-VALOR
              STRING MASC-VALOR X"00"     INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSINDICE LNKCOLUNASPRO(6) WSTEXTO

      *>Coluna Valor Total
              INITIALIZE WSTEXTO
              MOVE GS-ACP-VLRTOTAL          TO MASC-VALOR
              STRING MASC-VALOR X"00"     INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSINDICE LNKCOLUNASPRO(7) WSTEXTO
           ELSE
              INITIALIZE INDICE
              INVOKE GS-LISTVIEW-PRODUTOS "ADICIONARITEM"
                                                  RETURNING WSITEM
              ADD 1 TO INDICE
              INITIALIZE WSTEXTO
              STRING GS-ACP-IDENTIFICACAO X"00"   INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

              ADD 1 TO INDICE
              INITIALIZE WSTEXTO
              STRING GS-ACP-PRODUTO X"00"   INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

              ADD 1 TO INDICE
              INITIALIZE WSTEXTO
              STRING GS-ACP-MODELO X"00"   INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

              ADD 1 TO INDICE
              INITIALIZE WSTEXTO
              MOVE GS-ACP-QTDE-PLANILHADA     TO MASC-QTDE
              STRING MASC-QTDE X"00"        INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

              ADD 1 TO INDICE
              INITIALIZE WSTEXTO
              MOVE GS-ACP-QTDE-VENDIDA        TO MASC-QTDE
              STRING MASC-QTDE X"00"        INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

              ADD 1 TO INDICE
              INITIALIZE WSTEXTO
              MOVE GS-ACP-VLRUNI              TO MASC-VALOR
              STRING MASC-VALOR X"00"       INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

              ADD 1 TO INDICE
              INITIALIZE WSTEXTO
              MOVE GS-ACP-VLRTOTAL            TO MASC-VALOR
              STRING MASC-VALOR X"00"       INTO WSTEXTO
              INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO.


       CRIAR-LISTVIEW-PRODUTOS SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Identificação" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
                 using z"Produto" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---
          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
                using z"Modelo" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Qtde Planilhada" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Qtde Vendida" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Valor Unitário" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Valor Total" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

          perform mostrar-fonte-favoPro
          perform mostrar-colunas-favoPro

          invoke gs-listview-produtos "gridLines"
          invoke gs-listview-produtos "noBorder".
       CRIAR-LISTVIEW-PRODUTOS-FIM.
           EXIT.

       CRIAR-LISTVIEW-PAGTO SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-pagto "adicionarColunaZ"
            using z"Vencimento" returning lnkobjetoscolPagto(indice)
          invoke lnkobjetoscolPagto(indice) "centered"
          move indice to lnkcolunasPagto(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-pagto "adicionarColunaZ"
                 using z"Valor" returning lnkobjetoscolPagto(indice)
          invoke lnkobjetoscolPagto(indice) "RightJustified"
          move indice to lnkcolunasPagto(indice)

      *>---
      *>---
          add 1 to indice
          invoke gs-listview-pagto "adicionarColunaZ"
                using z"Nr. Docto" returning lnkobjetoscolPagto(indice)
          move indice to lnkcolunasPagto(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-pagto "adicionarColunaZ"
           using z"Banco" returning lnkobjetoscolPagto(indice)
          invoke lnkobjetoscolPagto(indice) "centered"
          move indice to lnkcolunasPagto(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-pagto "adicionarColunaZ"
           using z"Parcela" returning lnkobjetoscolPagto(indice)
          invoke lnkobjetoscolPagto(indice) "centered"
          move indice to lnkcolunasPagto(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-pagto "adicionarColunaZ"
           using z"Qtde Parcela" returning lnkobjetoscolPagto(indice)
          invoke lnkobjetoscolPagto(indice) "centered"
          move indice to lnkcolunasPagto(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-pagto "adicionarColunaZ"
           using z"Tipo" returning lnkobjetoscolPagto(indice)
          move indice to lnkcolunasPagto(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-pagto "adicionarColunaZ"
           using z"Data Baixa" returning lnkobjetoscolPagto(indice)
          invoke lnkobjetoscolPagto(indice) "centered"
          move indice to lnkcolunasPagto(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-pagto "adicionarColunaZ"
           using z"Cartão" returning lnkobjetoscolPagto(indice)
          move indice to lnkcolunasPagto(indice)

          perform mostrar-fonte-favoPagto
          perform mostrar-colunas-favoPagto

          invoke gs-listview-pagto "gridLines"
          invoke gs-listview-pagto "noBorder".
       CRIAR-LISTVIEW-PAGTO-FIM.
           EXIT.

       mostrar-colunas-favoPro section.
          initialize wsTexto
          move "listview-rcp100-produtos" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview-produtos
                                  wsTexto
                                  lnktabelaPro.
       mostrar-colunas-favoPro-fim.
           exit.

       mostrar-fonte-favoPro section.
           move "listview-rcp100-produtos" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview-produtos wsTexto.
       mostrar-fonte-favoPro-fim.
           exit.

       EXPORTAR-PARA-EXCEL-PRO section.
           invoke aListview "ExportarParaOExcel"
                    using gs-listview-produtos lnkTabelaPro.
       EXPORTAR-PARA-EXCEL-PRO-fim.
           EXIT.


       zebrar-itensPro section.
           move "listview-rcp100-produtos" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview-produtos wsTexto
           invoke gs-listview-produtos "redrawallitems".
       zebrar-itensPro-fim.
           exit.

       chamar-colunas-favoPro section.
           move "listview-rcp100-produtos" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview-produtos
                               wsTexto
                               lnktabelaPro

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro.
       chamar-colunas-favoPro-fim.
           exit.


       mostrar-colunas-favoPagto section.
          initialize wsTexto
          move "listview-rcp100-pagto" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview-pagto
                                  wsTexto
                                  lnktabelaPagto.
       mostrar-colunas-favoPagto-fim.
           exit.

       mostrar-fonte-favoPagto section.
           move "listview-rcp100-pagto" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview-pagto wsTexto.
       mostrar-fonte-favoPagto-fim.
           exit.

       EXPORTAR-PARA-EXCEL-PAGTO section.
           invoke aListview "ExportarParaOExcel"
                    using gs-listview-pagto lnkTabelaPagto.
       EXPORTAR-PARA-EXCEL-PAGTO-fim.
           EXIT.


       zebrar-itensPagto section.
           move spaces to wsTexto
           move "listview-rcp100-pagto" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview-pagto wsTexto
           invoke gs-listview-pagto "redrawallitems".
       zebrar-itensPagto-fim.
           exit.

       chamar-colunas-favoPagto section.
           move "listview-rcp100-pagto" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview-pagto
                               wsTexto
                               lnktabelaPagto

           perform mostrar-colunas-favoPagto
           perform mostrar-fonte-favoPagto
           perform zebrar-itensPagto.
       chamar-colunas-favoPagto-fim.
           exit.



       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

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
                   CLOSE      RCD101
                   OPEN I-O   RCD101
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
                                DELETE RCD101
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
                   CLOSE      RCD101
                   OPEN INPUT RCD101
                   INVOKE GS-LISTVIEW-PAGTO "DeleteAll".

      *--------------------------------------------------------------
       VERIFICA-ALBUM SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE ZEROS                      TO TOT-PARCELA
                                              GS-FLAG-CRITICA
           MOVE GS-NR-ALBUM                TO ALBUM-REC(1: 4)
           MOVE GS-NR-FORM                 TO ALBUM-REC(5: 4)
           MOVE GS-SEQ                     TO SEQ-REC
           READ RCD100 INVALID KEY
                MOVE GS-NR-ALBUM           TO NR-ALBUM-W
                MOVE GS-NR-FORM            TO NR-FORM-W
                MOVE GS-SEQ                TO SEQ-W
                MOVE GS-VENDEDOR           TO VENDEDOR-W
                PERFORM LIMPAR-DADOS
                MOVE NR-ALBUM-W            TO GS-NR-ALBUM
                                              ALBUM-REC(1: 4)
                MOVE NR-FORM-W             TO GS-NR-FORM
                                              ALBUM-REC(5: 4)
                MOVE SEQ-W                 TO GS-SEQ
                                              SEQ-REC
                MOVE 1                     TO GS-VISITA
                MOVE VENDEDOR-W            TO GS-VENDEDOR
                MOVE 1 TO GS-TIPO-GRAVACAO
                MOVE GS-NR-ALBUM           TO CONTRATO-MT19
                MOVE GS-NR-FORM            TO SEQ-MT19
                READ MTD019 INVALID KEY
                     MOVE SPACES           TO NOME-FORM-MT19
                NOT INVALID KEY
                     MOVE NOME-FORM-MT19   TO GS-NOME
                     MOVE CIDADE-MT19      TO CIDADE
                     READ CAD010 INVALID KEY
                          CONTINUE
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
                     NOT INVALID KEY
                         PERFORM CARREGAR-MTD020P
                     END-READ
                NOT INVALID KEY
                     PERFORM CARREGAR-MTD020P
                     MOVE "Usuário com Permissão apenas para Consulta"
                              TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                END-READ
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
                          MOVE SPACES TO GS-NOME GS-CIDADE
                          PERFORM CARREGAR-DADOS
                     END-READ
                NOT INVALID KEY
                     MOVE 2 TO GS-TIPO-GRAVACAO
                     MOVE SPACES TO GS-NOME GS-CIDADE
                     PERFORM CARREGAR-DADOS
                END-READ
           END-READ.
           MOVE CHAVE-ALBUM-REC      TO ALBUM-MTG.
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
                     MOVE VISITA-MTG        TO GS-COD-COMISS
                  END-IF
               END-IF
           END-READ.

       CARREGAR-DADOS SECTION.
           MOVE DATA-MOVTO-REC       TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO GS-DATA-MOVTO
           MOVE TAXA-REC             TO GS-TAXA
           MOVE DATAVEN-REC          TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO GS-DATA-VENDA
           MOVE VISITA-REC           TO GS-VISITA
           MOVE CHAVE-ALBUM-REC      TO ALBUM-MTG
           READ MTD020 INVALID KEY
                MOVE ZEROS           TO VISITA-MTG
           END-READ
           MOVE VISITA-MTG           TO GS-COD-COMISS
           IF FOGO-MTG = 9
              MOVE 8 TO FOGO-MTG
           ELSE
              MOVE 1 TO FOGO-MTG
           END-IF
           MOVE VENDEDOR-REC         TO GS-VENDEDOR
                                        CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES          TO NOME-CG01
           END-READ
           MOVE NOME-CG01            TO GS-NOME-VENDEDOR
           PERFORM GRAVA-WORK
           PERFORM CARREGA-CHEQUES
           PERFORM CARREGA-RCD100P.

       CARREGA-RCD100P SECTION.
           INVOKE GS-LISTVIEW-PRODUTOS "DELETEALL"

           INITIALIZE REG-RCD100P
           MOVE CHAVE-ALBUM-REC      TO CHAVE-ALBUM-RECP
           START RCD100P KEY IS NOT LESS CHAVE-RECP INVALID KEY
                 MOVE "10" TO ST-RCD100P.

           PERFORM UNTIL ST-RCD100P = "10"
                 READ RCD100P NEXT AT END
                      MOVE "10" TO ST-RCD100P
                 NOT AT END
                      IF CHAVE-ALBUM-REC <> CHAVE-ALBUM-RECP
                         MOVE "10" TO ST-RCD100P
                      ELSE
                         INITIALIZE INDICE
                         INVOKE GS-LISTVIEW-PRODUTOS "ADICIONARITEM"
                                                    RETURNING WSITEM
                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING PRODUTO-RECP MODELO-RECP
                                                     X"00" INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                           USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE PRODUTO-RECP    TO CADPRO-CODIGO
                         MOVE MODELO-RECP     TO CADPRO-MODELO
                         READ CADPRO INVALID KEY
                              MOVE "*****"    TO CADPRO-NOME
                         END-READ
                         STRING CADPRO-NOME X"00" DELIMITED BY "   "
                                            INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                           USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE MODELO-RECP     TO CADMOD-CODIGO
                         READ CADMOD INVALID KEY
                              MOVE "******"   TO CADMOD-NOME
                         END-READ
                         STRING CADMOD-NOME X"00" DELIMITED BY "   "
                                                  INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                           USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE QTDE-PLANILHADA-RECP       TO MASC-QTDE
                         STRING MASC-QTDE X"00"        INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                           USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE QTDE-VENDIDA-RECP          TO MASC-QTDE
                         STRING MASC-QTDE X"00"        INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                           USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE VLRUNI-RECP                TO MASC-VALOR
                         STRING MASC-VALOR X"00"       INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                           USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE VLRTOTAL-RECP              TO MASC-VALOR
                         STRING MASC-VALOR X"00"       INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                           USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO
                      END-IF
                 END-READ
           END-PERFORM

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro

           PERFORM SOMAR-PRODUTOS
           REFRESH-OBJECT PRINCIPAL.

       GRAVA-WORK SECTION.
           MOVE ZEROS             TO REG-RCD101

           MOVE CHAVE-ALBUM-REC   TO CHAVE-ALBUM-REC1
           IF SEQ-REC > 0
              COMPUTE SEQ-REC1 = SEQ-REC1 - 1
           ELSE
              COMPUTE ALBUM-REC1 = ALBUM-REC1 - 1
           END-IF

           MOVE ALL "9"       TO VENCTO-REC1
                                 BANCO-REC1
                                 NUMERO-REC1.

           START RCD101 KEY IS GREATER THAN CHAVE-REC1 INVALID KEY
                 MOVE "10"    TO ST-RCD101.

           PERFORM UNTIL ST-RCD101 = "10"
                 READ RCD101 NEXT RECORD AT END
                      MOVE "10" TO ST-RCD101
                 NOT AT END
                      MOVE CHAVE-ALBUM-REC1  TO AUX-ALBUM-REC
                      IF CHAVE-ALBUM-REC <> AUX-ALBUM-REC
                         MOVE "10"             TO ST-RCD101
                      ELSE
                         MOVE VENCTO-REC1      TO VENCTO-WK
                         MOVE VALOR-REC1       TO VALOR-WK
                         MOVE DTA-BAIXA-REC1   TO DATA-BAIXA-WK
                         MOVE NUMERO-REC1      TO NR-CHEQUE-WK
                         MOVE BANCO-REC1       TO BANCO-WK
                         MOVE PARCELA-REC1     TO PARCELA-WK
                         MOVE QT-PARCELA-REC1  TO QT-PARCELA-WK
                         MOVE COMIS-PARC-REC1  TO COMIS-PARC-WK
                         MOVE CARTAO-CRED-REC1 TO CARTAO-WK
                                                  CODIGO-CG20
                         READ CGD020 INVALID KEY
                              INITIALIZE REG-CGD020
                         END-READ
                         MOVE NOME-CG20        TO DESC-CARTAO-WK
                         MOVE TAXA-ADMINIST-CREDITO-REC1
                                            TO TAXA-ADMINIST-CREDITO-WK
                         MOVE TAXA-ADMINIST-PARCELA-REC1
                                            TO TAXA-ADMINIST-PARCELA-WK
                         EVALUATE TIPO-REC1
                           WHEN 1 MOVE "1-Cheque"       TO TIPO-WK
                           WHEN 2 MOVE "2-Moeda "       TO TIPO-WK
                           WHEN 3 MOVE "3-Antecip."     TO TIPO-WK
                           WHEN 4 MOVE "4-Boleto/Prom"  TO TIPO-WK
                           WHEN 5 MOVE "5-Deb.Autom."   TO TIPO-WK
                           WHEN 6 MOVE "6-Cartao Cred." TO TIPO-WK
                         END-EVALUATE
                         WRITE REG-WORK INVALID KEY
                             STRING "ERRO DE GRAVAÇÃO WORK ... "
                                     NR-CHEQUE-WK
                                    " NÚMERO DE DOCUMENTO REPETIDO"
                               INTO MENSAGEM
                             MOVE   "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                      END-IF
                 END-READ
           END-PERFORM.

       EXIBIR-MENSAGEM SECTION.
           MOVE    1      TO GS-FLAG-CRITICA
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           MOVE SPACES TO MENSAGEM.

       CARREGA-CHEQUES SECTION.
           INVOKE GS-LISTVIEW-PAGTO "DELETEALL"
           MOVE ZEROS           TO VENCTO-WK
           MOVE ZEROS           TO NR-CHEQUE-WK BANCO-WK
           START WORK KEY IS NOT < ALT-WK INVALID KEY
                 MOVE "10"      TO ST-WORK
           END-START
           MOVE SPACES          TO GS-LINDET
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10"    TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LISTA
                   ADD 1 TO TOT-PARCELA
              END-READ
           END-PERFORM

           perform mostrar-colunas-favoPagto
           perform mostrar-fonte-favoPagto
           perform zebrar-itensPagto.

       MOVER-DADOS-LISTA SECTION.
           initialize indice
           invoke gs-listview-pagto "adicionarItem" returning wsItem

           move vencto-wk          to data-inv
           call "GRIDAT1" using data-inv
           move data-inv           to data-e

           add 1 to indice
           initialize wsTexto
           string data-e X"00"   into wsTexto
           invoke gs-listview-pagto "preencherColunaZ"
             using wsItem lnkcolunasPagto(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           move valor-wk           to valor-e
           string valor-e X"00"  into wsTexto
           invoke gs-listview-pagto "preencherColunaZ"
             using wsItem lnkcolunasPagto(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string nr-cheque-wk X"00"  into wsTexto
           invoke gs-listview-pagto "preencherColunaZ"
             using wsItem lnkcolunasPagto(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string banco-wk X"00"  into wsTexto
           invoke gs-listview-pagto "preencherColunaZ"
             using wsItem lnkcolunasPagto(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string parcela-wk X"00"  into wsTexto
           invoke gs-listview-pagto "preencherColunaZ"
             using wsItem lnkcolunasPagto(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string qt-parcela-wk X"00"  into wsTexto
           invoke gs-listview-pagto "preencherColunaZ"
             using wsItem lnkcolunasPagto(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string tipo-wk X"00"  into wsTexto
           invoke gs-listview-pagto "preencherColunaZ"
             using wsItem lnkcolunasPagto(indice) wsTexto

           move data-baixa-wk      to data-inv
           call "GRIDAT1" using data-inv
           move data-inv           to data-e

           add 1 to indice
           initialize wsTexto
           string data-e  X"00"  into wsTexto
           invoke gs-listview-pagto "preencherColunaZ"
             using wsItem lnkcolunasPagto(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string cartao-wk "-" desc-cartao-wk X"00" delimited by "  "
             into wsTexto
           invoke gs-listview-pagto "preencherColunaZ"
             using wsItem lnkcolunasPagto(indice) wsTexto

           PERFORM SOMAR-PAGAMENTO.

      *-------------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           MOVE GS-DATA-MOVTO        TO DATA-MOVTO-W
           MOVE GS-TAXA              TO TAXA-W1
           MOVE GS-NR-ALBUM          TO NR-ALBUM-W
           MOVE GS-SEQ               TO SEQ-W
           MOVE GS-VENDEDOR          TO VENDEDOR-W
           MOVE GS-PORTADOR          TO PORTADOR-W
           MOVE GS-DESC-PORTADOR     TO DESC-PORTADOR-W
           INITIALIZE REG-RCD100
           INITIALIZE REG-RCD101
           INITIALIZE GS-DATA-BLOCK
           MOVE TAXA-W1              TO GS-TAXA
           MOVE DATA-MOVTO-W         TO GS-DATA-MOVTO
           MOVE NR-ALBUM-W           TO GS-NR-ALBUM
           MOVE SEQ-W                TO GS-SEQ
           MOVE VENDEDOR-W           TO GS-VENDEDOR
           MOVE PORTADOR-W           TO GS-PORTADOR

           INVOKE GS-LISTVIEW-PRODUTOS "DeleteAll"
           INVOKE GS-LISTVIEW-PAGTO    "DeleteAll"
           REFRESH-OBJECT PRINCIPAL.

      *---------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    MTD020 RCD100 RCD101
           OPEN I-O MTD020 RCD100 RCD101
           MOVE CHAVE-ALBUM-REC     TO ALBUM-MTG.
           READ MTD020 INVALID KEY CONTINUE
              NOT INVALID KEY
                  MOVE 0 TO FOGO-MTG
                  REWRITE REG-MTD020
                  END-REWRITE
           END-READ.
           DELETE RCD100.

           PERFORM EXCLUI-RCD101

           CLOSE      MTD020 RCD100 RCD101
           OPEN INPUT MTD020 RCD100 RCD101

           PERFORM LIMPAR-DADOS.
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
           END-EVALUATE

           STRING GS-NR-ALBUM GS-NR-FORM GS-SEQ INTO ALBUM-MTG
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
                       WHEN "1" MOVE COMIS-CHEQUE-RC01
                                  TO COMIS-PARC-WK
                                PERFORM VER-QTDE-DIAS
                       WHEN "2" MOVE COMIS-MOEDA-RC01
                                  TO COMIS-PARC-WK
                       WHEN "3" MOVE COMIS-ANTECIPADA-RC01
                                  TO COMIS-PARC-WK
                                PERFORM VER-QTDE-DIAS
                       WHEN "4" MOVE COMIS-DUPLICATA-RC01
                                  TO COMIS-PARC-WK
                                PERFORM VER-QTDE-DIAS
                       WHEN "5" MOVE COMIS-DEBITO-AUTOMATICO-RC01
                                  TO COMIS-PARC-WK
                                PERFORM VER-QTDE-DIAS
                       WHEN "6" MOVE COMIS-CARTAO-CREDITO-RC01
                                  TO COMIS-PARC-WK
                                PERFORM VER-QTDE-DIAS
                   END-EVALUATE
                   REWRITE REG-WORK INVALID KEY
                     MOVE "ERRO DE REGRAVACAO" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                   END-REWRITE
                   PERFORM MOVER-DADOS-LISTA
                   MOVE "Documento Já Informado, e Alterado com Sucesso"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
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
               WHEN "6" MOVE GS-COD-COMISS     TO CODIGO-COMIS-RC02
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
       GRAVA-RECIBO SECTION.
           CLOSE    RCD100 RCD100P
           OPEN I-O RCD100 RCD100P

           MOVE ZEROS TO NR-PARCELA.
           MOVE GS-NR-ALBUM          TO ALBUM-REC(1: 4)
           MOVE GS-NR-FORM           TO ALBUM-REC(5: 4)
           MOVE GS-SEQ               TO SEQ-REC

           MOVE GS-DATA-MOVTO        TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO DATA-MOVTO-REC
           MOVE GS-TAXA              TO TAXA-REC
           MOVE GS-DATA-VENDA        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO DATAVEN-REC
           MOVE GS-VISITA            TO VISITA-REC
           MOVE GS-VENDEDOR          TO VENDEDOR-REC
           MOVE GS-TOT-VENDA         TO TOTAL-REC
           MOVE GS-TOT-PM            TO PM-REC
           MOVE GS-TOT-VENDA-DEF     TO TOTAL-DEF-REC

           IF GS-TIPO-GRAVACAO = 1
              WRITE REG-RCD100 INVALID KEY
                 MOVE "Erro Gravacao RCD100" TO GS-MENSAGEM-ERRO
                 MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                NOT INVALID KEY CONTINUE
              END-WRITE

              PERFORM CADASTRA-CLIENTE
           ELSE
              REWRITE REG-RCD100 INVALID KEY
                 MOVE "Erro Regravacao RCD100" TO GS-MENSAGEM-ERRO
                 MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-REWRITE.

           INITIALIZE REG-RCD100P
           MOVE CHAVE-ALBUM-REC              TO CHAVE-ALBUM-RECP
           START RCD100P KEY IS NOT LESS CHAVE-RECP INVALID KEY
                MOVE "10" TO ST-RCD100P.

           PERFORM UNTIL ST-RCD100P = "10"
                READ RCD100P NEXT AT END
                     MOVE "10" TO ST-RCD100P
                NOT AT END
                     IF CHAVE-ALBUM-REC <> CHAVE-ALBUM-RECP
                        MOVE "10" TO ST-RCD100P
                     ELSE
                        DELETE RCD100P INVALID KEY
                            MOVE "Erro de Exclusão...RCD100P" TO
                                        MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                        END-DELETE
                     END-IF
                END-READ
           END-PERFORM

           INITIALIZE WSINDICE
           INVOKE GS-LISTVIEW-PRODUTOS "SIZE" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
                 INITIALIZE REG-RCD100P

                 ADD 1 TO WSINDICE
                 INVOKE GS-LISTVIEW-PRODUTOS "ITEMATINDEX"
                        USING WSINDICE RETURNING UMITEM

      *>Identificação do Produto
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNASPRO(1) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE CHAVE-ALBUM-REC               TO CHAVE-ALBUM-RECP
                 MOVE FUNCTION NUMVAL(WSTEXTO(1:4)) TO PRODUTO-RECP
                 MOVE FUNCTION NUMVAL(WSTEXTO(5:4)) TO MODELO-RECP

      *>Quantidade Planilhada
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNASPRO(4) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)   TO QTDE-PLANILHADA-RECP

      *>Quantidade Vendida
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNASPRO(5) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)   TO QTDE-VENDIDA-RECP


      *>Valor Unitário
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNASPRO(6) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)   TO VLRUNI-RECP

      *>Valor Total
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNASPRO(7) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO)   TO VLRTOTAL-RECP

                 WRITE REG-RCD100P INVALID KEY
                     MOVE "Erro de Gravação...RCD100P" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                 END-WRITE
           END-PERFORM


           CLOSE      RCD100 RCD100P
           OPEN INPUT RCD100 RCD100P

           PERFORM SALVAR-MONTAGEM.
           PERFORM SALVAR-PARCELAS-RECIBO.

       SALVAR-MONTAGEM SECTION.
           CLOSE      MTD020
           OPEN I-O   MTD020
           MOVE CHAVE-ALBUM-REC      TO ALBUM-MTG
           READ MTD020 INVALID KEY
                MOVE ZEROS           TO VISITA-MTG.

           MOVE GS-COD-COMISS        TO VISITA-MTG
           IF FOGO-MTG = 9
              MOVE 8 TO FOGO-MTG
           ELSE
              MOVE 1 TO FOGO-MTG.

           REWRITE REG-MTD020 INVALID KEY
              MOVE "Erro de Regravação...MTD020" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           CLOSE      MTD020
           OPEN INPUT MTD020.

       SALVAR-PARCELAS-RECIBO SECTION.
           CLOSE    RCD101
           OPEN I-O RCD101

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
                      MOVE CHAVE-ALBUM-REC     TO CHAVE-ALBUM-REC1
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
                      WRITE REG-RCD101
                      END-WRITE
                      IF GS-TIPO-GRAVACAO = 1
      *               caso seja regravação os acertos no contas a receber
      *               e nos cheques deverão ser feitos manualmente
                         EVALUATE TIPO-REC1
      *                           1 CHEQUE - 2 ANTECIPADO - 3 MOEDA
      *                           4 DUPLI/NT.PROM
                             WHEN 1 PERFORM GRAVA-CHEQUE
                             WHEN 2 CONTINUE
                             WHEN 3 CONTINUE
                             WHEN 4 PERFORM GRAVA-DUPLICATA
                             WHEN 5 PERFORM GRAVA-DUPLICATA
                             WHEN 6
                                MOVE COD-USUARIO-W TO COD-USUARIO-CA004
                                MOVE "SENHA35"     TO PROGRAMA-CA004
                                READ CAD004 NOT INVALID KEY
                                     PERFORM GRAVA-DUPLICATA
                                END-READ
                         END-EVALUATE
                      END-IF
                      PERFORM GRAVAR-ANOTACAO
                 END-READ
           END-PERFORM.

           CLOSE      RCD101
           OPEN INPUT RCD101.

       GRAVAR-ANOTACAO SECTION.
           OPEN I-O CRD200 CRD201
           IF ST-CRD200 = "35"
              CLOSE       CRD200
              OPEN OUTPUT CRD200
              CLOSE       CRD200
              OPEN I-O    CRD200.

           IF ST-CRD201 = "35"
              CLOSE       CRD201
              OPEN OUTPUT CRD201
              CLOSE       CRD201
              OPEN I-O    CRD201.

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

           MOVE CHAVE-ALBUM-REC   TO CHAVE-ALBUM-REC1
           IF SEQ-REC > 0
              COMPUTE SEQ-REC1 = SEQ-REC1 - 1
           ELSE
              COMPUTE ALBUM-REC1 = ALBUM-REC1 - 1
           END-IF

           MOVE ALL "9"       TO VENCTO-REC1
                                 BANCO-REC1
                                 NUMERO-REC1.

           START RCD101 KEY IS GREATER THAN CHAVE-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101.

           PERFORM UNTIL ST-RCD101 = "10"
                 READ RCD101 NEXT RECORD AT END
                      MOVE "10" TO ST-RCD101
                 NOT AT END
                      MOVE CHAVE-ALBUM-REC1 TO AUX-ALBUM-REC

                      IF CHAVE-ALBUM-REC <> AUX-ALBUM-REC
                         MOVE "10" TO ST-RCD101
                      ELSE
                         DELETE RCD101 INVALID KEY
                             MOVE "Erro de Exclusão...RCD101" TO
                                         MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
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
                PERFORM EXIBIR-MENSAGEM.

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
                              MOVE ALL "9"  TO CLASS-CLIENTE-CR24
                              MOVE ALL "9"  TO CLIENTE-CR24
                              MOVE ALL "9"  TO SEQ-CR24
                              READ CRD024 INVALID KEY
                                   MOVE NUMERO TO
                                        NUMERO-PROGRAMACAO-CR24
                                   WRITE REG-CRD024
                                   END-WRITE
                              NOT INVALID KEY
                                   MOVE NUMERO TO
                                        NUMERO-PROGRAMACAO-CR24
                                   REWRITE REG-CRD024
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
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      PERFORM CALCULA-DIAS
                      MOVE TIPO-WK(1: 1) TO TIPO-W
                      EVALUATE TIPO-W
                        WHEN 1
      *                   COMPUTE VALOR-W = VALOR-WK / VALOR-060
                          ADD VALOR-WK TO GS-TOT-VENDA
                          PERFORM DEFLACIONA
                          ADD DEF1-W TO  DEFLACIONADO-W
                          COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) +
                                               TOTALPM-W
                        WHEN 2
                          ADD VALOR-WK TO DEFLACIONADO-W, GS-TOT-VENDA
                          COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) +
                                  TOTALPM-W
                        WHEN 3
                          ADD VALOR-WK TO DEFLACIONADO-W, GS-TOT-VENDA
                          COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) +
                                  TOTALPM-W
                        WHEN 4
      *                   COMPUTE VALOR-W = VALOR-WK / VALOR-060
                          ADD VALOR-WK TO GS-TOT-VENDA
                          PERFORM DEFLACIONA
                          ADD DEF1-W TO  DEFLACIONADO-W
                          COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) +
                                  TOTALPM-W
                        WHEN 5
      *                   COMPUTE VALOR-W = VALOR-WK / VALOR-060
                          ADD VALOR-WK TO GS-TOT-VENDA
                          PERFORM DEFLACIONA
                          ADD DEF1-W TO  DEFLACIONADO-W
                          COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) +
                                  TOTALPM-W
                        WHEN 6
      *                   COMPUTE VALOR-W = VALOR-WK / VALOR-060
                          ADD VALOR-WK TO GS-TOT-VENDA
                          PERFORM DEFLACIONA
                          ADD DEF1-W TO  DEFLACIONADO-W
                          COMPUTE TOTALPM-W = (CONT-W * VALOR-WK) +
                                  TOTALPM-W
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

       CARREGAR-MTD020P SECTION.
           INVOKE GS-LISTVIEW-PRODUTOS "DELETEALL"

           INITIALIZE REG-MTD020P
           MOVE GS-NR-ALBUM            TO CONTRATO-MTGP
           MOVE GS-NR-FORM             TO NRALBUM-MTGP
           MOVE GS-SEQ                 TO SEQUEN-MTGP
           START MTD020P KEY IS NOT LESS CHAVE-MTGP INVALID KEY
                MOVE "10" TO ST-MTD020P.

           PERFORM UNTIL ST-MTD020P = "10"
                READ MTD020P NEXT AT END
                     MOVE "10" TO ST-MTD020P
                NOT AT END
                     IF GS-NR-ALBUM <> CONTRATO-MTGP OR
                        GS-NR-FORM  <> NRALBUM-MTGP  OR
                        GS-SEQ      <> SEQUEN-MTGP
                        MOVE "10" TO ST-MTD020P
                     ELSE
                        INITIALIZE INDICE
                        INVOKE GS-LISTVIEW-PRODUTOS "ADICIONARITEM"
                                                       RETURNING WSITEM
      *>Identificação
                        ADD 1 TO INDICE
                        INITIALIZE WSTEXTO
                        STRING PRODUTO-MTGP MODELO-MTGP X"00"
                                                            INTO WSTEXTO
                        INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

      *>Produto
                        ADD 1 TO INDICE
                        INITIALIZE WSTEXTO
                        MOVE PRODUTO-MTGP        TO CADPRO-CODIGO
                        MOVE MODELO-MTGP         TO CADPRO-MODELO
                        READ CADPRO INVALID KEY
                             MOVE "***********"  TO CADPRO-NOME
                        END-READ
                        STRING CADPRO-NOME X"00" DELIMITED BY "   "
                                                            INTO WSTEXTO
                        INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

      *>Modelo
                        ADD 1 TO INDICE
                        INITIALIZE WSTEXTO
                        MOVE MODELO-MTGP         TO CADMOD-CODIGO
                        READ CADMOD INVALID KEY
                             MOVE "***********"  TO CADMOD-NOME
                        END-READ
                        STRING CADMOD-NOME X"00" DELIMITED BY "   "
                                                            INTO WSTEXTO
                        INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

      *>Quantidade Planilhada
                        ADD 1 TO INDICE
                        INITIALIZE WSTEXTO
                        MOVE QTDE-PLANILHA-MTGP TO MASC-QTDE
                        STRING MASC-QTDE X"00" INTO WSTEXTO
                        INVOKE GS-LISTVIEW-PRODUTOS "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNASPRO(INDICE) WSTEXTO

                     END-IF
                END-READ
           END-PERFORM

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro.

       SOMAR-PRODUTOS SECTION.
           INITIALIZE GS-TOTAL-PAGAR

           INITIALIZE WSINDICE
           INVOKE GS-LISTVIEW-PRODUTOS "SIZE" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
                 INITIALIZE REG-RCD100P

                 ADD 1 TO WSINDICE
                 INVOKE GS-LISTVIEW-PRODUTOS "ITEMATINDEX"
                        USING WSINDICE RETURNING UMITEM

      *>Valor total
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNASPRO(7) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO)      TO VLRTOTAL-RECP

                 ADD VLRTOTAL-RECP                  TO GS-TOTAL-PAGAR
           END-PERFORM
           REFRESH-OBJECT PRINCIPAL.

       SOMAR-PAGAMENTO SECTION.
           INITIALIZE GS-TOTAL-REC

           INITIALIZE WSINDICE
           INVOKE GS-LISTVIEW-PAGTO "SIZE" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
                 INITIALIZE REG-RCD100P

                 ADD 1 TO WSINDICE
                 INVOKE GS-LISTVIEW-PAGTO "ITEMATINDEX"
                        USING WSINDICE RETURNING UMITEM

      *>Valor
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNASPAGTO(2) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO)      TO VLRTOTAL-RECP

                 ADD VLRTOTAL-RECP                  TO GS-TOTAL-REC
           END-PERFORM
           REFRESH-OBJECT PRINCIPAL.

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
                 CRD024 PAR001 CADPRO CADMOD MTD020P RCD100P
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
