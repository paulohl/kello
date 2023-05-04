       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP108.
       DATE-WRITTEN. 01/09/2000.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA. Relatório 05 - RESUMO POR CIDADE
      * LISTAR APENAS OS CONTRATOS ATIVOS, OU SEJA, STATUS => 50
      * - CONTRATOS A EXECUTAR SÃO OS CONTRATOS QUE NÃO FORAM VENDIDOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX002.
           COPY COPX005.
           COPY COPX040.
           COPY COPX050.
           COPY COPX051.
           COPY CAPX010.
           COPY CAPX012.
           COPY MTPX001.
           COPY MTPX020.
           COPY RCPX100.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = EXECUTADO-WK SEQ-WK
                  ALTERNATE RECORD KEY IS ALT1-WK = EXECUTADO-WK
                     CIDADE-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW012.
       COPY COPW002.
       COPY COPW005.
       COPY COPW040.
       COPY COPW050.
       COPY COPW051.
       COPY MTPW001.
       COPY MTPW020.
       COPY RCPW100.

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(5).
           05  CIDADE-WK           PIC X(13).
           05  QT-FORM-WK          PIC 9(4).
           05  QT-FOTOS-WK         PIC 9(9).
      *    QT-FOTOS-WK = qt-form-wk * prev-fotos-co05(cadastro padrao)
           05  QT-FITAS-WK         PIC 9(4).
           05  QT-DVD-WK           PIC 9(4).
           05  OBJ-VENDA-WK        PIC 9(8)V99.
      *    OBJ-VENDA-WK = qt-fotos-wk * 0,7 * preco-wk.
           05  PATROCINIO-WK       PIC 9(7)V99.
      *    PATROCINIO-WK = vlr-pago-wk + vlr-pagar-wk
           05  PAT-FORM-WK         PIC 9(6)V99.
      *    PAT-FORM-W = patrocinio-wk / qt-form-wk

           05  ALBPROD-WK           PIC 9999.
           05  ALBVEND-WK           PIC 9999.
           05  FOTPROD-WK           PIC 9(09).
           05  FOTVEND-WK           PIC 9(09).
           05  FOTAVUL-WK           PIC 9(09).
           05  FOTMONT-WK           PIC 9(09).
           05  FOTFOGO-WK           PIC 9(05).
           05  FOTDEVO-WK           PIC S9(5).
           05  FITA-WK              PIC 9(04).
           05  DVD-WK               PIC 9(04).
           05  FATVENBRU-WK         PIC 9(8)V99.
           05  PRAZO-WK             PIC 9(11)V99.
           05  VENLIQDEF-WK         PIC 9(10)V99.
           05  VENLIQIND-WK         PIC 9(10)V99.
           05  ALBPERD-WK           PIC 999.
           05  FOTPERD-WK           PIC 9999.
           05  FOTCOMIS-WK          PIC 9(5).
           05  VENDAVUL-WK          PIC 9(5).
           05  FORMANDOS-WK         PIC 9(04).
           05  EXECUTADO-WK         PIC 9.
           05  REPRESENT-WK         PIC X(8).
           05  QT-FORM-WK-ATUAL     PIC 9(4).
           05  QT-FORM-WK-INI       PIC 9(4).
           05  PADRAO-WK            PIC X.
           05  QT-FOTOS-WK-ATUAL    PIC 9(9).
           05  QT-FOTOS-WK-INI      PIC 9(9).
      *    QT-FOTOS-WK-ATUAL = QT-FORM-WK-ATUAL * prev-fotos-co05(cadastro padrao)
           05  QT-FITAS-WK-INI      PIC 9(4).
           05  QT-FITAS-WK-ATUAL    PIC 9(4).
           05  QT-DVD-WK-INI        PIC 9(4).
           05  QT-DVD-WK-ATUAL      PIC 9(4).
           05  QT-POSTER-WK-INI     PIC 9(4).
           05  QT-POSTER-WK-ATUAL   PIC 9(4).
           05  MESANO-WK            PIC 9(6).
           05  OBJ-VENDA-WK-ATUAL   PIC 9(8)V99.
           05  OBJ-VENDA-WK-INI     PIC 9(8)V99.
      *    OBJ-VENDA-WK = QT-FOTOS-WK-ATUAL * 0,7 * preco-wk.
           05  PATROCINIO-WK-ATUAL        PIC 9(7)V99.
           05  PATROCINIO-WK-INI          PIC 9(7)V99.
           05  PATROCINIO-PAGO-WK         PIC 9(07)V99.
           05  PATROCINIO-N-PAGO-WK       PIC 9(07)V99.
           05  PATROCINIO-PAGO-WK-AEXEC   PIC 9(07)V99.
           05  PATROCINIO-N-PAGO-WK-AEXEC PIC 9(07)V99.
      *    PATROCINIO-WK = vlr-pago-wk + vlr-pagar-wk
           05  PAT-FORM-WK-ATUAL    PIC 9(6)V99.
           05  PAT-FORM-WK-INI      PIC 9(6)V99.
      *    PAT-FORM-W = patrocinio-wk / QT-FORM-WK-ATUAL
           05  PM-WK                PIC 9(3)V99.

           05  DATA-WK              PIC 9(8).
           05  POSTER-WK            PIC 9(04).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP108.CPB".
           COPY "COP108.CPY".
           COPY "CBDATA.CPY".
           copy cpdias1.
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-COD051             PIC XX       VALUE SPACES.
           05  ST-MTD001             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  CONT-W          PIC 9(4)            VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E2              PIC ZZZ,ZZ   BLANK WHEN ZEROS.
           05  VALOR-E3              PIC ZZ.ZZZ,ZZ   BLANK WHEN ZEROS.
           05  PERC-E                PIC ZZZ,Z    BLANK WHEN ZEROS.
           05  PRAZO-E               PIC ZZZ,ZZ   BLANK WHEN ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  EXECUTADO-ANT         PIC 9        VALUE ZEROS.
           05  CIDADE-ANT            PIC X(13)    VALUE SPACES.

           05  QTDE1-E               PIC Z.ZZZ.ZZZ.
           05  QTDE2-E               PIC ZZZZ.ZZZ.ZZZ.
           05  QTDE3-E               PIC Z.ZZZ,ZZ.
           05  QTDE4-E               PIC ZZZZ.ZZZ.
           05  QTDE5-E               PIC ZZZZZ.ZZZ.
           05  QTDE6-E               PIC ZZZ.ZZZ.
           05  QTDE7-E               PIC ZZZ,Z.
           05  QTDE8-E               PIC ZZZ,ZZ.
           05  QTDE9-E               PIC ZZZZ,ZZ.
           05  QTDE10-E              PIC ZZ.ZZZ,ZZ.
           05  VALOR1-E              PIC ZZZ.ZZZ.ZZZ,ZZ.
           05  VALOR2-E              PIC ZZ.ZZZ.ZZZ,ZZ.

           05  DATAI.
               10  ANO-I       PIC 9999.
               10  MES-I       PIC 99.
               10  DIA-I       PIC 99.
           05  DATA-I REDEFINES DATAI PIC 9(08).
           05  MESANO-E              PIC 99/9999.
           05  FOTDEVO-W             PIC S9(5)    VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  ALB-W                 PIC 9(4)     VALUE ZEROS.
           05  VALOR-PM              PIC 9(14)V99 VALUE ZEROS.
           05  DIVISAO-W             PIC 9(6)V9(5)  VALUE ZEROS.
           05  PERC-W                PIC 9(5)V9999 VALUE ZEROS.
           05  PRECO-W               PIC 9(5)V99   VALUE ZEROS.
           05  SALFOT-W        PIC 9(05)         VALUE ZEROS.
           05  SALALB-W        PIC 9(04)         VALUE ZEROS.
           05  DISPONIVEL-W    PIC S9(5)          VALUE ZEROS.
           05  PRECOMED-W      PIC 9(08)V99      VALUE ZEROS.
           05  VENMEDCLI-W     PIC 9(08)V99      VALUE ZEROS.
           05  PRAZO-W         PIC 9(05)V999     VALUE ZEROS.
           05  PRAZO-GERAL     PIC 9(14)V99      VALUE ZEROS.

      *    VARIAVEIS P/ CALCULO DE PATROCINIO PAGOS E A PAGAR
           05  CUSTO-TOTAL           PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-PREVISTO        PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-W               PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-W               PIC 9(8)V99  VALUE ZEROS.
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  TAXA-W                PIC 9(3)V99999 VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  MESES-W               PIC 9(3)     VALUE ZEROS.
           05  VLR-GERAL             PIC 9(8)V99  VALUE ZEROS.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.

           05  REG-CIDADE.
               10  QT-FORM-CID          PIC 9(8).
               10  QT-FOTOS-CID         PIC 9(10).
               10  QT-FITAS-CID         PIC 9(7).
               10  QT-DVD-CID           PIC 9(7).
               10  OBJ-VENDA-CID        PIC 9(9)V99.
               10  PATROCINIO-CID       PIC 9(8)V99.
               10  PAT-FORM-CID         PIC 9(6)V99.
               10  ALBPROD-CID           PIC 9(7).
               10  ALBVEND-CID           PIC 9(7).
               10  FOTPROD-CID           PIC 9(08).
               10  FOTVEND-CID           PIC 9(08).
               10  FOTAVUL-CID           PIC 9(08).
               10  FOTMONT-CID           PIC 9(08).
               10  FOTFOGO-CID           PIC 9(08).
               10  FOTDEVO-CID           PIC S9(8).
               10  FITA-CID              PIC 9(06).
               10  DVD-CID               PIC 9(06).
               10  FATVENBRU-CID         PIC 9(8)V99.
               10  PRAZO-CID             PIC 9(16)V99.
               10  VENLIQDEF-CID         PIC 9(10)V99.
               10  VENLIQIND-CID         PIC 9(10)V99.
               10  ALBPERD-CID           PIC 9(6).
               10  FOTPERD-CID           PIC 9(7).
               10  FOTCOMIS-CID          PIC 9(8).
               10  VENDAVUL-CID          PIC 9(8).
               10  FORMANDOS-CID         PIC 9(08).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(33)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(36)   VALUE
           "RELATORIO 05          - ORDEM: ".
           05  ORDEM-REL           PIC X(18)   VALUE SPACES.
           05  FILLER              PIC X(20)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(120)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(120)   VALUE
           "CIDADE        FORMANDOS  QTDE.FOTOS  QT.FITA     PREV.VENDA
      -    "   PATROCINIO PAT/FORM  PRODUZ.  VENDIDA   SALDO".
       01  CAB05.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(120)   VALUE
           "------------------FOTOGRAFIAS----------------   VEND. -RESUL
      -    "T.FOTOS %--".
       01  CAB06.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(120)   VALUE
           "  DISPON.   COMIS     VENDA  DEVOLV.    SALDO    FITA VENDA
      -    "DEVOL SALDO   P.M. VDA.LIQ.INDEX  P/FOTO V.MED.CLI".
       01  LINDET.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  LINDET-REL          PIC X(120)  VALUE SPACES.
       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC X(97) VALUE
           " ---------------PREVISAO---------------------  ----FOTOGRAFI
      -    "AS-----------------------------------".

       02  LINHA-02.
           05 FILLER                         PIC X(70) VALUE
           "|                                            ||PRODUZIDAS".
           05 FOTO-PRODUZ-REL                PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-FOTO-PRODUZ-REL           PIC ZZZ,ZZ.
           05 FILLER                         PIC X(09) VALUE
           "        |".

       02  LINHA-03.
           05 FILLER                         PIC X(31) VALUE
           "|INICIAIS ALBUM/CLIENTE     ".
           05 ALBUM-CLIENTE-REL-I            PIC ZZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||PERDIDAS              ".
           05 FOTO-PERD-REL                  PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-FOTO-PERD-REL             PIC ZZZ,ZZ.
           05 FILLER                         PIC X(09) VALUE
           "        |".

       02  LINHA-04.
           05 FILLER                         PIC X(31) VALUE
           "|         PRODUCAO FOTOGRAFIAS ".
           05 PROD-FOTOG-REL-I               PIC ZZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||DISPONIVEL            ".
           05 FOTO-DISPON-REL                PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-FOTO-DISPON-REL           PIC ZZZ,ZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-FOTO-DISPON-REL2          PIC ZZZ,ZZ.
           05 FILLER                         PIC X(1) VALUE "|".

       02  LINHA-05.
           05 FILLER                         PIC X(31) VALUE
           "|         PRODUCAO FITAS      ".
           05 PROD-FITA-REL-I                PIC ZZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||VENDIDAS        ".
           05 FOTO-VEND-REL                  PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-FOTO-VEND-REL             PIC ZZZ,ZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-FOTO-VEND-REL2            PIC ZZZ,ZZ.
           05 FILLER                         PIC X(1) VALUE "|".

       02  LINHA-06.
           05 FILLER                         PIC X(31) VALUE
           "|         PRODUCAO DE DVD     ".
           05 PROD-DVD-REL-I                 PIC ZZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||AVULSAS                ".
           05 FOTO-AVULSA-REL                PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(10) VALUE
           "          ".
           05 PERC-FOTO-AVULSA-REL           PIC ZZZ,ZZ.
           05 FILLER                         PIC X(01) VALUE "|".

       02  LINHA-07.
           05 FILLER                         PIC X(31) VALUE
           "|         PRODUCAO DE POSTER  ".
           05 PROD-POSTER-REL-I              PIC ZZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||COMISSAO               ".
           05 FOTO-COMISSAO-REL              PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(10) VALUE
           "          ".
           05 PERC-FOTO-COMISSAO-REL         PIC ZZZ,ZZ.
           05 FILLER                         PIC X(01) VALUE "|".

       02  LINHA-08.
           05 FILLER                         PIC X(31) VALUE
           "|         VENDA LIQUIDA      ".
           05 VENDA-LIQUIDA-REL-I            PIC Z.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||DEVOLVIDAS      ".
           05 FOTO-DEVOL-REL                 PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-FOTO-DEVOL-REL            PIC ZZZ,ZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-FOTO-DEVOL-REL2           PIC ZZZ,ZZ.
           05 FILLER                         PIC X(1) VALUE "|".

       02  LINHA-09.
           05 FILLER                         PIC X(31) VALUE
           "|         PATROCINIO          ".
           05 PATROCINIO-REL-I               PIC Z.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||FOGO                   ".
           05 FOTO-FOGO-REL                  PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(10) VALUE
           "          ".
           05 PERC-FOTO-FOGO-REL             PIC ZZZ,ZZ.
           05 FILLER                         PIC X(01) VALUE "|".

       02  LINHA-10.
           05 FILLER                         PIC X(31) VALUE
           "|         PATROCINIO MEDIO/CLIE".
           05 PAT-MED-CLIENTE-REL-I          PIC Z.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||SALDO                  ".
           05 FOTO-SALDO-REL                 PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(10) VALUE
           "          ".
           05 PERC-FOTO-SALDO-REL            PIC ZZZ,ZZ.
           05 FILLER                         PIC X(01) VALUE "|".

       02  LINHA-11.
           05 FILLER                         PIC X(97) VALUE
           "|                                            ||-------------
      -    "-------------------------------------".

       02  LINHA-12.
           05 FILLER                         PIC X(97) VALUE
           "|                                            ||----ALBUM/CLI
      -    "ENTE---------------------------------".

       02  LINHA-13.
           05 FILLER                         PIC X(31) VALUE
           "|AUTAIS   ALBUM/CLIENTE     ".
           05 ALBUM-CLIENTE-REL-A            PIC ZZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||PRODUZIDOS            ".
           05 ALBUM-PRODUZ-REL               PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-ALBUM-PRODUZ-REL          PIC ZZZ,ZZ.
           05 FILLER                         PIC X(09) VALUE
           "        |".

       02  LINHA-14.
           05 FILLER                         PIC X(31) VALUE
           "|         PRODUCAO FOTOGRAFIAS ".
           05 PROD-FOTOG-REL-A               PIC ZZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||VENDIDOS              ".
           05 ALBUM-VEND-REL                 PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-ALBUM-VEND-REL            PIC ZZZ,ZZ.
           05 FILLER                         PIC X(09) VALUE
           "        |".

       02  LINHA-15.
           05 FILLER                         PIC X(31) VALUE
           "|         PRODUCAO FITAS      ".
           05 PROD-FITA-REL-A                PIC ZZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||SALDO           ".
           05 ALBUM-SALDO-REL                PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-ALBUM-SALDO-REL           PIC ZZZ,ZZ.
           05 FILLER                         PIC X(09) VALUE
           "        |".

       02  LINHA-16.
           05 FILLER                         PIC X(31) VALUE
           "|         PRODUCAO DE DVD     ".
           05 PROD-DVD-REL-A                 PIC ZZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||FITAS VENDIDAS  ".
           05 FITA-VENDIDA-REL               PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-FITA-VENDIDA-REL          PIC ZZZ,ZZ.
           05 FILLER                         PIC X(09) VALUE
           "        |".

       02  LINHA-17.
           05 FILLER                         PIC X(31) VALUE
           "|         PRODUCAO DE POSTER  ".
           05 PROD-POSTER-REL-A              PIC ZZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||DVD VENDIDOS    ".
           05 DVD-VENDIDO-REL                PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-DVD-VENDIDO-REL           PIC ZZZ,ZZ.
           05 FILLER                         PIC X(09) VALUE
           "        |".

       02  LINHA-18.
           05 FILLER                         PIC X(31) VALUE
           "|         VENDA LIQUIDA       ".
           05 VENDA-LIQUIDA-REL-A            PIC Z.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(27) VALUE
           "  ||POSTER VENDIDO  ".
           05 POSTER-VENDIDO-REL             PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(02) VALUE
           "  ".
           05 PERC-POSTER-VENDIDO-REL        PIC ZZZ,ZZ.
           05 FILLER                         PIC X(09) VALUE
           "        |".

       02  LINHA-19.
           05 FILLER                         PIC X(31) VALUE
           "|         PATROCINIO          ".
           05 PATROCINIO-REL-A               PIC Z.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(23) VALUE
           "  ||PATROCINIO PAGO ".
           05 PATROCINIO-P-REL               PIC ZZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(16).
           05 FILLER                         PIC X(01) VALUE
           "|".

       02  LINHA-20.
           05 FILLER                         PIC X(31) VALUE
           "|         PATROCINIO MEDIO/CLIE".
           05 PAT-MED-CLIENTE-REL-A          PIC Z.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(23) VALUE
           "  ||PATROCINIO N PAGO ".
           05 PATROCINIO-N-REL               PIC ZZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(16).
           05 FILLER                         PIC X(01) VALUE
           "|".

       02  LINHA-21.
           05 FILLER                         PIC X(43) VALUE
           "|                             ".
           05 FILLER                         PIC X(23) VALUE
           "  ||PATROCINIO MEDIO".
           05 PATROCINIO-MEDIO-REL           PIC ZZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(16).
           05 FILLER                         PIC X(01) VALUE
           "|".

       02  LINHA-211.
           05 FILLER                         PIC X(98) VALUE
           " -----------------------------------  ----------------------
      -    "------------------------------------- ".


       02  LINHA-22.
           05 FILLER                         PIC X(98) VALUE
           " ----FATURAMENTO--------------------  ----CONTRATOS NAO EXEC
      -    "UTADOS------------------------------- ".

       02  LINHA-23.
           05 FILLER                         PIC X(23) VALUE
           "| VENDA BRUTA          ".
           05 VENDA-BRUTA-REL                PIC ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(22)    VALUE
           "|| ALBUM/CLIENTE      ".
           05 ALBUM-AEXECUTAR-REL            PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(15) VALUE
           " VENDA LIQUIDA ".
           05 VENDA-LIQ-AEXECUTAR-REL        PIC ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(01) VALUE "|".

       02  LINHA-24.
           05 FILLER                         PIC X(23) VALUE
           "| PRAZO MEDIO          ".
           05 PRAZO-MEDIO-REL                PIC ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(22)    VALUE
           "|| PRODUCAO FOTOGRAFIA".
           05 PROD-FOTOGRAFIA-REL            PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(15) VALUE
           " PATROCINIO    ".
           05 PATROCINIO-AEXECUTAR-REL       PIC ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(01) VALUE "|".

       02  LINHA-25.
           05 FILLER                         PIC X(23) VALUE
           "| VENDA LIQUIDA (INDEX)".
           05 VENDA-LIQUIDA-FAT-REL          PIC ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(22)    VALUE
           "|| PRODUCAO FITAS     ".
           05 PROD-FITAS-REL                 PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(15) VALUE
           " PATROCINIO/FOR".
           05 PAT-FORM-AEXECUTAR-REL         PIC ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(01) VALUE "|".

       02  LINHA-26.
           05 FILLER                         PIC X(23) VALUE
           "| PRECO MEDIO POR FOTO ".
           05 PRECO-MEDIO-FOTO-REL           PIC ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(22)    VALUE
           "|| PRODUCAO DVD       ".
           05 PROD-DVD-REL                   PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(15) VALUE
           " PATROC. NAO PG".
           05 PATROCINIO-N-PAGO-AEXEC-REL    PIC ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(01) VALUE "|".

       02  LINHA-27.
           05 FILLER                         PIC X(26) VALUE
           "| PRECO MEDIO POR CLIENTE".
           05 PRECO-MEDIO-CLIENTE-REL        PIC ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC X(22)    VALUE
           "|| PRODUCAO POSTER    ".
           05 PROD-POSTER-REL                PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(28).
           05 FILLER                         PIC X(01) VALUE "|".

       02  LINHA-28.
           05 FILLER                         PIC X(36) VALUE
           "|                        ".
           05 FILLER                         PIC X(22)    VALUE
           "|| PATROCINIO PAGO    ".
           05 PATROCINIO-PAGO-AEXEC-REL      PIC ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC X(28).
           05 FILLER                         PIC X(01) VALUE "|".

       02  LINHA-29.
           05 FILLER                         PIC X(98) VALUE
           " -----------------------------------  ----------------------
      -    "------------------------------------- ".

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
           MOVE "MTD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD001.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "COD051"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD051.
           OPEN INPUT CAD010 COD040 COD005 COD050 COD051
                      COD002 CAD012 MTD001 MTD020 RCD100.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD001 <> "00"
              MOVE "ERRO ABERTURA MTD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD005 <> "00"
              MOVE "ERRO ABERTURA COD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD050 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD051 <> "00"
              MOVE "ERRO ABERTURA COD051: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD051 TO GS-MENSAGEM-ERRO(23: 02)
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
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO SEQ-W.
           MOVE GS-MESANO-INI TO MESANO-W
           MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
           MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I       TO MESANO-INI
           MOVE GS-MESANO-FIM  TO MESANO-W
           MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
           MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I       TO MESANO-FIM

           MOVE MESANO-INI     TO MESANO-PREV-CO40
           MOVE ZEROS         TO NR-CONTRATO-CO40
           START COD040 KEY IS NOT < ALT1-CO40 INVALID KEY
                  MOVE "10" TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
             READ COD040 NEXT RECORD AT END MOVE "10" TO ST-COD040
              NOT AT END
                  MOVE NR-CONTRATO-CO40 TO GS-EXIBE-CONTRATO
                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  IF MESANO-PREV-CO40 > MESANO-FIM
                     MOVE "10" TO ST-COD040
                  ELSE PERFORM MOVER-DADOS-WORK
                  END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           INITIALIZE REG-WORK.
           IF STATUS-CO40 < 50 OR STATUS-CO40 = 56 CONTINUE
           ELSE
              ADD 1                   TO SEQ-W
              MOVE SEQ-W              TO SEQ-WK
              MOVE CIDADE-CO40        TO CIDADE
              READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
              END-READ
              MOVE NOME-CID           TO CIDADE-WK
              ADD VLR-COMISSAO-CO40   TO PATROCINIO-WK

              MOVE DATA-PREV-VENDA-CO40 TO DATA-INV
              CALL "GRIDAT2" USING DATA-INV
              MOVE DATA-INV TO GRDIAS-AAMMDD-FINAL
              MOVE ASSINATURA-CO40 TO GRDIAS-AAMMDD-INICIAL
              CALL "GRDIAS1" USING PARAMETROS-GRDIAS
              MOVE GRDIAS-NUM-DIAS TO CONT-W
              COMPUTE MESES-W = CONT-W / 30
              COMPUTE VLR-GERAL = MESES-W * VLR-COMISSAO-CO40

               MOVE QTDE-FORM-CO40     TO QT-FORM-WK-ATUAL
              IF QTDE-FORM-INI-CO40 NOT > 0
                 MOVE QTDE-FORM-CO40  TO QT-FORM-WK-INI
              ELSE
                 MOVE QTDE-FORM-INI-CO40 TO QT-FORM-WK-INI
              END-IF

              MOVE PADRAO-CO40        TO PADRAO-CO05 PADRAO-WK
              READ COD005 INVALID KEY MOVE ZEROS TO PREV-FOTOS-CO05
              END-READ
              COMPUTE QT-FOTOS-WK-ATUAL = PREV-FOTOS-CO05 *
                                          QT-FORM-WK-ATUAL

              COMPUTE QT-FOTOS-WK-INI   = PREV-FOTOS-CO05 *
                                          QT-FORM-WK-INI

              MOVE ZEROS TO QT-FITAS-WK-ATUAL QT-FITAS-WK-INI
              MOVE ZEROS TO QT-DVD-WK-ATUAL QT-DVD-WK-INI
                            QT-POSTER-WK-ATUAL QT-POSTER-WK-INI

              PERFORM CALCULAR-DADOS-BRINDE
              PERFORM INICIO-MONTAGEM

              MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              WRITE REG-WORK
           END-IF.

       INICIO-MONTAGEM SECTION.
           MOVE NR-CONTRATO-CO40 TO CONTRATO-MT01.
           READ MTD001 INVALID KEY MOVE ZEROS TO FOTPROD-WK
                          FOTPERD-WK FOTMONT-WK FOTAVUL-WK ALBPROD-WK
            NOT INVALID KEY
                    MOVE PRODUZIDA-MT01   TO FOTPROD-WK
                    MOVE PERDIDA-MT01     TO FOTPERD-WK
                    MOVE MONTADA-MT01     TO FOTMONT-WK
                    MOVE AVULSA-MT01      TO FOTAVUL-WK
                    MOVE CLIEN-ALBUM-MT01 TO ALBPROD-WK
           END-READ.
           PERFORM LEITURA-MTD020.
           PERFORM LEITURA-RECIBO.

       LEITURA-MTD020 SECTION.
      *    verifica a qtde album e fotos no fogo
           MOVE NR-CONTRATO-CO40  TO ALBUM-MTG(1: 4)
           MOVE ZEROS             TO ALBUM-MTG(5: 4)
           MOVE ZEROS TO FOTFOGO-WK
           START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                 MOVE "10" TO ST-MTD020.
           PERFORM UNTIL ST-MTD020 = "10"
             READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
              NOT AT END
               IF QT-FOTOS-MTG NOT NUMERIC MOVE ZEROS TO QT-FOTOS-MTG
               END-IF
               IF QT-ENCADER-MTG NOT NUMERIC
                  MOVE ZEROS TO QT-ENCADER-MTG
               END-IF
               MOVE ALBUM-MTG(1: 4) TO CONTRATO-W
               IF FOGO-MTG NOT NUMERIC MOVE 1 TO FOGO-MTG
               END-IF
               IF CONTRATO-W NOT = NR-CONTRATO-CO40
                 MOVE "10" TO ST-MTD020
               ELSE
                  IF FOGO-MTG = 9 ADD QT-ENCADER-MTG TO ALBPERD-WK
                                  ADD QT-FOTOS-MTG TO FOTFOGO-WK
                  END-IF
               END-IF
               ADD QT-FITAS-MTG TO QT-FITAS-WK-ATUAL
               ADD QT-FITAS-MTG TO QT-FITAS-WK-INI
               ADD QT-DVD-MTG   TO QT-DVD-WK-ATUAL
               ADD QT-DVD-MTG   TO QT-DVD-WK-INI
               ADD QT-POSTER-MTG TO QT-POSTER-WK-ATUAL
               ADD QT-POSTER-MTG TO QT-POSTER-WK-INI
             END-READ
           END-PERFORM.

       LEITURA-RECIBO SECTION.
           MOVE 2     TO EXECUTADO-WK.
           MOVE NR-CONTRATO-CO40 TO ALBUM-REC(1: 4)
           MOVE ZEROS            TO ALBUM-REC(5: 4)
           START RCD100 KEY IS NOT < ALBUM-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
            READ RCD100 NEXT RECORD AT END MOVE "10" TO ST-RCD100
             NOT AT END
               MOVE ALBUM-REC(1: 4) TO CONTRATO-W
               IF CONTRATO-W NOT = NR-CONTRATO-CO40
                  MOVE "10" TO ST-RCD100
               ELSE
                MOVE ALBUM-REC       TO ALBUM-MTG
                MOVE 1 TO EXECUTADO-WK
                READ MTD020 INVALID KEY PERFORM CONT-LEIT-RECIBO
                  NOT INVALID KEY
                      IF QT-FOTOS-MTG NOT NUMERIC
                         MOVE ZEROS TO QT-FOTOS-MTG
                      END-IF

                      COMPUTE FOTDEVO-W = QT-FOTOS-MTG -
                               (QFOTOS-REC + QABERTURA-REC
                               + QCOMISSAO-REC +QAVULSAS-REC)

                      ADD FOTDEVO-W TO FOTDEVO-WK
                      PERFORM CONT-LEIT-RECIBO
                END-READ
               END-IF
            END-READ
           END-PERFORM.

       CONT-LEIT-RECIBO SECTION.
           MOVE ALBUM-REC(5: 4) TO ALB-W
           IF ALB-W NOT = ZEROS ADD 1 TO FORMANDOS-WK.
           ADD QCOMISSAO-REC TO FOTCOMIS-WK.
      *    ADD QAVULSAS-REC  TO VENDAVUL-WK
           ADD QAVULSAS-REC  TO FOTAVUL-WK
           ADD QAVULSAS-REC  TO FOTVEND-WK
           ADD QFOTOS-REC    TO FOTVEND-WK.
           ADD QABERTURA-REC TO FOTVEND-WK
           ADD QENCADER-REC  TO ALBVEND-WK.
           ADD QFITAS-REC    TO FITA-WK.
           ADD QDVD-REC      TO DVD-WK
           ADD QPOSTER-REC   TO POSTER-WK
           ADD TOTAL-REC     TO FATVENBRU-WK.
           ADD TOTAL-DEF-REC TO VENLIQDEF-WK VENLIQIND-WK.
           COMPUTE VALOR-PM = (TOTAL-REC * PM-REC).
           ADD VALOR-PM TO PRAZO-WK.
           IF DATA-WK = ZEROS
              MOVE DATAVEN-REC TO DATA-WK.

      * ---------ROTINAS PERFORMADAS(PELA SEÇÃO MOVER-DADOS-WORK)---
       CALCULAR-DADOS-BRINDE SECTION.
           MOVE ZEROS TO CUSTO-TOTAL CUSTO-PREVISTO CUSTO-W JUROS-W
                         CONT TAXA-ACUMULADA.
           MOVE NR-CONTRATO-CO40   TO NR-CONTRATO-CO50.
           MOVE ZEROS              TO ITEM-CO50.
           START COD050 KEY IS NOT < CHAVE-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.
           PERFORM UNTIL ST-COD050 = "10"
              READ COD050 NEXT RECORD AT END MOVE "10" TO ST-COD050
                NOT AT END
                  IF NR-CONTRATO-CO50 <> NR-CONTRATO-CO40
                     MOVE "10" TO ST-COD050
                  ELSE
                     IF SUSP-PREV-DEF-CO50 <> 2
                        MOVE CODBRINDE-CO50 TO CODIGO-CO02
                        READ COD002 INVALID KEY
                             MOVE ZEROS TO VALOR-CO02
                        END-READ

                        MOVE REG-COD050 TO REG-COD051

                        READ COD051 NOT INVALID KEY
                            IF CUSTO-UNIT-CO51 <> ZEROS
                               MOVE CUSTO-UNIT-CO51 TO CUSTO-PREVISTO
                            ELSE
                               MOVE VALOR-CO02 TO CUSTO-PREVISTO
                            END-IF
      * 0 NÃO - 1 SIM
                            IF REALIZADO-CO51 <> 1
      * 1 SIM - 2 NÃO
                               IF MULT-FORM-CO02 = 2
                                  COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                                      QTDE-POR-FORM-CO51
                               ELSE
                                  COMPUTE CUSTO-W = (QTDE-POR-FORM-CO51
                                      * QTDE-FORM-CO51) * CUSTO-PREVISTO
                               END-IF
                            ELSE
                               MOVE VALOR-PAGO-CO51 TO CUSTO-W
                            END-IF

                            ADD CUSTO-W TO PATROCINIO-WK-INI
                            PERFORM CALCULO-JUROS-BRINDE1
                            ADD JUROS-W TO PATROCINIO-WK-INI
                        END-READ


                        IF CUSTO-UNIT-CO50 <> ZEROS
                           MOVE CUSTO-UNIT-CO50 TO CUSTO-PREVISTO
                        ELSE
                           MOVE VALOR-CO02 TO CUSTO-PREVISTO
                        END-IF

      * 0 NÃO - 1 SIM
                        IF REALIZADO-CO50 <> 1
      * 1 SIM - 2 NÃO
                           IF MULT-FORM-CO02 = 2
                              COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                                      QTDE-POR-FORM-CO50
                           ELSE
                              COMPUTE CUSTO-W = (QTDE-POR-FORM-CO50 *
                                        QTDE-FORM-CO50) * CUSTO-PREVISTO
                           END-IF
                           ADD CUSTO-W       TO PATROCINIO-N-PAGO-WK
                           ADD CUSTO-W   TO PATROCINIO-N-PAGO-WK-AEXEC
                        ELSE
                           MOVE VALOR-PAGO-CO50 TO CUSTO-W
                           ADD CUSTO-W       TO PATROCINIO-PAGO-WK
                           ADD CUSTO-W       TO PATROCINIO-PAGO-WK-AEXEC
                        END-IF

                        ADD CUSTO-W TO PATROCINIO-WK-ATUAL

                        PERFORM CALCULO-JUROS-BRINDE

                        ADD JUROS-W TO PATROCINIO-WK-ATUAL

                        IF REALIZADO-CO50 <> 1
                           ADD JUROS-W TO PATROCINIO-N-PAGO-WK
                           ADD JUROS-W TO PATROCINIO-N-PAGO-WK-AEXEC
                        ELSE
                           ADD JUROS-W TO PATROCINIO-PAGO-WK
                           ADD JUROS-W TO PATROCINIO-PAGO-WK-AEXEC
                        END-IF

                        COMPUTE MESES-W = DIAS-PRAZO-CO50 / 30
                        COMPUTE VLR-GERAL = (CUSTO-W * MESES-W)
                                                             + VLR-GERAL
                     END-IF
                  END-IF
              END-READ
           END-PERFORM.
           COMPUTE PRAZO-MEDIO ROUNDED = VLR-GERAL / PATROCINIO-WK-ATUAL
           MOVE PRAZO-MEDIO TO PM-WK.
      *    MOVE 1 TO TAXA-ACUMULADA.
      *    COMPUTE TAXA-W = (GS-TAXA / 100) + 1.
      *    PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
      *        COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA * TAXA-W
      *    END-PERFORM.
           PERFORM VERIFICA-PRECO.

           COMPUTE OBJ-VENDA-WK-ATUAL = QT-FOTOS-WK-ATUAL * 0,7 *
                                                                PRECO-W.

           COMPUTE OBJ-VENDA-WK-INI = QT-FOTOS-WK-INI * 0,7 * PRECO-W.


           COMPUTE PAT-FORM-WK-ATUAL = PATROCINIO-WK-ATUAL
                                                    / QT-FORM-WK-ATUAL.
           COMPUTE PAT-FORM-WK-INI = PATROCINIO-WK-INI / QT-FORM-WK-INI.
       VERIFICA-PRECO SECTION.
           IF MESANO-WK < 199506 MOVE 8 TO PRECO-W
           ELSE MOVE 11 TO PRECO-W.

       CALCULO-JUROS-BRINDE1 SECTION.
           COMPUTE MESES-W = DIAS-PRAZO-CO51 / 30.
           MOVE MESES-W TO PRAZO-MEDIO
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-W * (TAXA-ACUMULADA - 1).

       CALCULO-JUROS-BRINDE SECTION.
           COMPUTE MESES-W = DIAS-PRAZO-CO50 / 30.
           MOVE MESES-W TO PRAZO-MEDIO
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-W * (TAXA-ACUMULADA - 1).

      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           INITIALIZE REG-WORK.
           PERFORM ORDEM.
           INITIALIZE REG-CIDADE.
           MOVE SPACES TO CIDADE-ANT.
           MOVE ZEROS  TO EXECUTADO-ANT.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-EXECUTADO-TRUE AND EXECUTADO-WK = 1 OR
                   GS-AEXECUTAR-TRUE AND EXECUTADO-WK = 2
                      PERFORM TOTALIZA-CIDADE
                ELSE CONTINUE
                END-IF
              END-READ
           END-PERFORM.
           PERFORM MOVER-DADOS-LINDET.
           PERFORM TOTALIZA-TELA.
       TOTALIZA-CIDADE SECTION.
           IF EXECUTADO-ANT <> ZEROS
              IF EXECUTADO-WK <> EXECUTADO-ANT
                 PERFORM MOVER-DADOS-LINDET
                 INITIALIZE REG-CIDADE
                 PERFORM ACUMULA-DADOS-CIDADE
              ELSE
                IF CIDADE-ANT <> SPACES
                   IF CIDADE-WK <> CIDADE-ANT
                      PERFORM MOVER-DADOS-LINDET
                      INITIALIZE REG-CIDADE
                      PERFORM ACUMULA-DADOS-CIDADE
                   ELSE
                      PERFORM ACUMULA-DADOS-CIDADE
                ELSE PERFORM ACUMULA-DADOS-CIDADE
           ELSE PERFORM ACUMULA-DADOS-CIDADE.
       ACUMULA-DADOS-CIDADE SECTION.
           MOVE EXECUTADO-WK   TO EXECUTADO-ANT
           MOVE CIDADE-WK      TO CIDADE-ANT.

           ADD QT-FORM-WK          TO QT-FORM-CID
           ADD QT-FOTOS-WK         TO QT-FOTOS-CID
           ADD QT-FITAS-WK         TO QT-FITAS-CID
           ADD QT-DVD-WK           TO QT-DVD-CID
           ADD OBJ-VENDA-WK        TO OBJ-VENDA-CID
           ADD PATROCINIO-WK       TO PATROCINIO-CID
      *    ADD PAT-FORM-WK         TO PAT-FORM-CID
           ADD ALBPROD-WK          TO ALBPROD-CID
           ADD ALBVEND-WK          TO ALBVEND-CID
           ADD FOTPROD-WK          TO FOTPROD-CID
           ADD FOTVEND-WK          TO FOTVEND-CID
           ADD FOTAVUL-WK          TO FOTAVUL-CID
           ADD FOTMONT-WK          TO FOTMONT-CID
           ADD FOTFOGO-WK          TO FOTFOGO-CID
           ADD FOTDEVO-WK          TO FOTDEVO-CID
           ADD FITA-WK             TO FITA-CID
           ADD FATVENBRU-WK        TO FATVENBRU-CID
           ADD PRAZO-WK            TO PRAZO-CID
           ADD VENLIQDEF-WK        TO VENLIQDEF-CID
           ADD VENLIQIND-WK        TO VENLIQIND-CID
           ADD ALBPERD-WK          TO ALBPERD-CID
           ADD FOTPERD-WK          TO FOTPERD-CID
           ADD FOTCOMIS-WK         TO FOTCOMIS-CID
           ADD VENDAVUL-WK         TO VENDAVUL-CID
           ADD FORMANDOS-WK        TO FORMANDOS-CID.
           IF EXECUTADO-WK = 2
               ADD QT-FORM-WK         TO GS-ALBUM-AEXECUTAR
               ADD QT-FOTOS-WK        TO GS-PROD-FOTOGRAFIA
               ADD QT-FITAS-WK        TO GS-PROD-FITAS
      *        ADD QT-DVD-WK          TO GS-PROD-DVD
               ADD OBJ-VENDA-WK       TO GS-VENDA-LIQ-AEXECUTAR
               ADD PATROCINIO-WK      TO GS-PATROCINIO-AEXECUTAR
           ELSE
               ADD QT-FORM-WK         TO GS-ALBUM-CLIENTE
               ADD QT-FOTOS-WK        TO GS-PROD-FOTOG
               ADD QT-FITAS-WK        TO GS-PROD-FITA
      *        ADD QT-DVD-WK          TO GS-PROD-DVD
               ADD OBJ-VENDA-WK       TO GS-VENDA-LIQUIDA
               ADD PATROCINIO-WK      TO GS-PATROCINIO.

           IF EXECUTADO-WK = 1
               COMPUTE GS-PAT-MED-CLIENTE = GS-PATROCINIO /
                                        GS-ALBUM-CLIENTE
               ADD ALBPROD-WK            TO GS-ALBUM-PRODUZ
               ADD FORMANDOS-WK          TO GS-ALBUM-VEND
               COMPUTE SALALB-W = ALBPROD-WK - FORMANDOS-WK
               ADD SALALB-W              TO GS-ALBUM-SALDO
               ADD FITA-WK               TO GS-FITA-VENDIDA
               ADD FOTPROD-WK            TO GS-FOTO-PRODUZ
               ADD FOTPERD-WK            TO GS-FOTO-PERD
               ADD FOTAVUL-WK            TO GS-FOTO-AVULSA
               ADD FOTCOMIS-WK           TO GS-FOTO-COMISSAO
               ADD FOTVEND-WK            TO GS-FOTO-VEND
               ADD FOTDEVO-WK            TO GS-FOTO-DEVOL
               ADD FOTFOGO-WK            TO GS-FOTO-FOGO
               COMPUTE DISPONIVEL-W = (FOTMONT-WK + FOTAVUL-WK) -
                                  (FOTCOMIS-WK + FOTFOGO-WK)
               ADD DISPONIVEL-W        TO GS-FOTO-DISPON
               COMPUTE SALFOT-W = FOTPROD-WK - (FOTVEND-WK +
                  FOTPERD-WK + FOTDEVO-WK + FOTCOMIS-WK +
                              VENDAVUL-WK + FOTFOGO-WK)
               ADD SALFOT-W              TO GS-FOTO-SALDO
               ADD FATVENBRU-WK          TO GS-VENDA-BRUTA
               ADD PRAZO-WK              TO PRAZO-GERAL
               ADD VENLIQDEF-WK          TO GS-VENDA-LIQUIDA-FAT.


       TOTALIZA-TELA SECTION.
           IF GS-FOTO-PRODUZ > 100
              MOVE 100                  TO GS-PERC-FOTO-PRODUZ

              COMPUTE DIVISAO-W ROUNDED = GS-FOTO-PERD / GS-FOTO-PRODUZ
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-FOTO-PERD

      *       COMPUTE DIVISAO-W ROUNDED= GS-FOTO-AVULSA / GS-FOTO-PRODUZ
      *       COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
      *       MOVE PERC-W               TO GS-PERC-FOTO-AVULSA

              COMPUTE DIVISAO-W ROUNDED = GS-FOTO-COMISSAO /
                                          GS-FOTO-DISPON
              COMPUTE PERC-W ROUNDED = DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-FOTO-COMISSAO

              COMPUTE DIVISAO-W ROUNDED= GS-FOTO-DISPON / GS-FOTO-PRODUZ
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-FOTO-DISPON

              MOVE 100                  TO GS-PERC-FOTO-DISPON1
              COMPUTE DIVISAO-W ROUNDED= GS-FOTO-VEND / GS-FOTO-DISPON
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-FOTO-VEND

              COMPUTE DIVISAO-W ROUNDED= GS-FOTO-AVULSA / GS-FOTO-DISPON
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-FOTO-AVULSA

              COMPUTE DIVISAO-W ROUNDED= GS-FOTO-DEVOL / GS-FOTO-DISPON
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-FOTO-DEVOL

              COMPUTE DIVISAO-W ROUNDED= GS-FOTO-FOGO / GS-FOTO-DISPON
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-FOTO-FOGO

              MOVE 100                  TO GS-PERC-VENDIDOS

              COMPUTE DIVISAO-W ROUNDED = GS-FOTO-DEVOL  / GS-FOTO-VEND
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-DEVOLVIDAS


              COMPUTE DIVISAO-W ROUNDED= GS-FOTO-SALDO / GS-FOTO-DISPON
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-FOTO-SALDO

              MOVE 100                  TO GS-PERC-ALBUM-PRODUZ

              COMPUTE DIVISAO-W ROUNDED= GS-ALBUM-VEND / GS-ALBUM-PRODUZ
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-ALBUM-VEND

              COMPUTE DIVISAO-W ROUNDED= GS-ALBUM-SALDO /
                                         GS-ALBUM-PRODUZ
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-ALBUM-SALDO

              COMPUTE DIVISAO-W ROUNDED= GS-FITA-VENDIDA /
                                         GS-ALBUM-PRODUZ
              COMPUTE PERC-W ROUNDED= DIVISAO-W * 100
              MOVE PERC-W               TO GS-PERC-FITA-VENDIDA

              COMPUTE GS-PRECO-MEDIO-FOTO ROUNDED= GS-VENDA-LIQUIDA-FAT
                                                / GS-FOTO-VEND
              COMPUTE GS-PRECO-MEDIO-CLIENTE ROUNDED=
                              GS-VENDA-LIQUIDA-FAT / GS-ALBUM-VEND

              COMPUTE GS-PRAZO-MEDIO ROUNDED =
                                          PRAZO-GERAL / GS-VENDA-BRUTA

              COMPUTE GS-PATROCINIO-MEDIO ROUNDED=

              (GS-PATROCINIO-PAGO + GS-PATROCINIO-N-PAGO)  /
                                            GS-ALBUM-PRODUZ.

           COMPUTE GS-PAT-MED-CLIENTE-INI   = GS-PATROCINIO-INI /
                                        GS-ALBUM-CLIENTE-INI.

           COMPUTE GS-PAT-MED-CLIENTE-ATUAL = GS-PATROCINIO-ATUAL /
                                        GS-ALBUM-CLIENTE-ATUAL.


           COMPUTE GS-PAT-FORM-AEXECUTAR ROUNDED=
                           GS-PATROCINIO-AEXECUTAR / GS-ALBUM-AEXECUTAR.
       ORDEM SECTION.
           INITIALIZE REG-WORK.
           START WORK KEY IS NOT < ALT1-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
       MOVER-DADOS-LINDET SECTION.
           MOVE CIDADE-ANT        TO GS-LINDET(1: 14)
           MOVE QT-FORM-CID       TO QTDE1-E
           MOVE QTDE1-E           TO GS-LINDET(15: 10)
           MOVE QT-FOTOS-CID      TO QTDE2-E
           MOVE QTDE2-E           TO GS-LINDET(25: 13)
           MOVE QT-FITAS-CID      TO QTDE1-E
           MOVE QTDE1-E           TO GS-LINDET(37: 10)
           MOVE OBJ-VENDA-CID     TO VALOR1-E
           MOVE VALOR1-E          TO GS-LINDET(47: 15)
           MOVE PATROCINIO-CID    TO VALOR2-E
           MOVE VALOR2-E          TO GS-LINDET(62: 14)
           COMPUTE PAT-FORM-CID = PATROCINIO-CID / QT-FORM-CID
           MOVE PAT-FORM-CID      TO QTDE3-E
           MOVE QTDE3-E           TO GS-LINDET(76: 9)

      *    EXECUTADO-WK = 2(NAO)  1(SIM)
           IF EXECUTADO-WK = 2
               ADD QT-FORM-WK-ATUAL     TO GS-ALBUM-AEXECUTAR
               ADD QT-FOTOS-WK-ATUAL    TO GS-PROD-FOTOGRAFIA
               ADD QT-FITAS-WK-ATUAL    TO GS-PROD-FITAS
               ADD QT-DVD-WK-ATUAL      TO GS-PROD-DVD
               ADD QT-POSTER-WK-ATUAL   TO GS-PROD-POSTER
               ADD OBJ-VENDA-WK-ATUAL   TO GS-VENDA-LIQ-AEXECUTAR
               ADD PATROCINIO-WK-ATUAL  TO GS-PATROCINIO-AEXECUTAR
               ADD PATROCINIO-PAGO-WK-AEXEC TO GS-PATROCINIO-PAGO-AEXEC
               ADD PATROCINIO-N-PAGO-WK-AEXEC
                                        TO GS-PATROCINIO-N-PAGO-AEXEC

           ELSE
               ADD QT-FITAS-WK-ATUAL    TO GS-PROD-FITA-ATUAL
               ADD QT-FITAS-WK-INI      TO GS-PROD-FITA-INI
               ADD QT-DVD-WK-ATUAL      TO GS-PROD-DVD-ATUAL
               ADD QT-DVD-WK-INI        TO GS-PROD-DVD-INI
               ADD QT-POSTER-WK-ATUAL   TO GS-PROD-POSTER-ATUAL
               ADD QT-POSTER-WK-INI     TO GS-PROD-POSTER-INI
               ADD PATROCINIO-PAGO-WK   TO GS-PATROCINIO-PAGO
               ADD PATROCINIO-N-PAGO-WK TO GS-PATROCINIO-N-PAGO.

           ADD QT-FOTOS-WK-ATUAL    TO GS-PROD-FOTOG-ATUAL
           ADD QT-FOTOS-WK-INI      TO GS-PROD-FOTOG-INI

           ADD OBJ-VENDA-WK-INI     TO GS-VENDA-LIQUIDA-INI
           ADD OBJ-VENDA-WK-ATUAL   TO GS-VENDA-LIQUIDA-ATUAL

           ADD PATROCINIO-WK-INI    TO GS-PATROCINIO-INI
           ADD PATROCINIO-WK-ATUAL  TO GS-PATROCINIO-ATUAL

           ADD QT-FORM-WK-ATUAL     TO GS-ALBUM-CLIENTE-ATUAL
           ADD QT-FORM-WK-INI       TO GS-ALBUM-CLIENTE-INI


           IF EXECUTADO-WK = 2
              MOVE "INSERE-LIST" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
              MOVE ALBPROD-CID         TO QTDE4-E
              MOVE QTDE4-E             TO GS-LINDET(85: 9)
              MOVE FORMANDOS-CID       TO QTDE4-E
              MOVE QTDE4-E             TO GS-LINDET(94: 9)
              COMPUTE SALALB-W = ALBPROD-CID - FORMANDOS-CID
              MOVE SALALB-W            TO QTDE4-E
              MOVE QTDE4-E             TO GS-LINDET(103: 8)
              MOVE "INSERE-LIST" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              PERFORM MOVER-DADOS-LINDET1.

       MOVER-DADOS-LINDET1 SECTION.
      *    2o. LINHA DA LINDET
           MOVE SPACES              TO GS-LINDET

           COMPUTE DISPONIVEL-W = (FOTMONT-CID + FOTAVUL-CID) -
                                  (FOTCOMIS-CID + FOTFOGO-CID).
           MOVE DISPONIVEL-W        TO QTDE5-E
           MOVE QTDE5-E             TO GS-LINDET(1: 10)
           MOVE FOTCOMIS-CID        TO QTDE6-E
           MOVE QTDE6-E             TO GS-LINDET(11: 8)
           MOVE FOTVEND-CID         TO QTDE5-E
           MOVE QTDE5-E             TO GS-LINDET(19: 10)
           MOVE FOTDEVO-CID         TO QTDE4-E
           MOVE QTDE4-E             TO GS-LINDET(29: 9)
           COMPUTE SALFOT-W = FOTPROD-CID - (FOTVEND-CID + FOTPERD-CID +
                              FOTDEVO-CID + FOTCOMIS-CID + VENDAVUL-CID
                              + FOTFOGO-CID).
           MOVE SALFOT-W            TO QTDE4-E
           MOVE QTDE4-E             TO GS-LINDET(38: 9)
           MOVE FITA-CID            TO QTDE6-E
           MOVE QTDE6-E             TO GS-LINDET(47: 8)

           COMPUTE PERC-W = (FOTVEND-CID / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(56: 6)
           COMPUTE PERC-W = (FOTDEVO-CID / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(62: 6)
           COMPUTE PERC-W = (SALFOT-W / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(68: 6)

           COMPUTE PRAZO-W = PRAZO-CID / FATVENBRU-CID.
           MOVE PRAZO-W             TO QTDE8-E
           MOVE QTDE8-E             TO GS-LINDET(74: 7)

           MOVE VENLIQIND-CID       TO VALOR2-E
           MOVE VALOR2-E            TO GS-LINDET(81: 14)
           COMPUTE PRECOMED-W = VENLIQIND-CID / FOTVEND-CID.
           MOVE PRECOMED-W          TO QTDE9-E
           MOVE QTDE9-E             TO GS-LINDET(95: 8)
           COMPUTE VENMEDCLI-W = VENLIQIND-CID / FORMANDOS-CID.
           MOVE VENMEDCLI-W         TO QTDE10-E
           MOVE QTDE10-E            TO GS-LINDET(102: 9)

           ADD ALBPROD-WK            TO GS-ALBUM-PRODUZ
           ADD FORMANDOS-WK          TO GS-ALBUM-VEND
           ADD SALALB-W              TO GS-ALBUM-SALDO
           ADD FITA-WK               TO GS-FITA-VENDIDA
           ADD DVD-WK                TO GS-DVD-VENDIDO
           ADD POSTER-WK             TO GS-POSTER-VENDIDO
           ADD FOTPROD-WK            TO GS-FOTO-PRODUZ
           ADD FOTPERD-WK            TO GS-FOTO-PERD
           ADD DISPONIVEL-W          TO GS-FOTO-DISPON
           ADD FOTAVUL-WK            TO GS-FOTO-AVULSA
           ADD FOTCOMIS-WK           TO GS-FOTO-COMISSAO.
           ADD FOTVEND-WK            TO GS-FOTO-VEND
           ADD FOTDEVO-WK            TO GS-FOTO-DEVOL
           ADD FOTFOGO-WK            TO GS-FOTO-FOGO
           ADD SALFOT-W              TO GS-FOTO-SALDO
           ADD FATVENBRU-WK          TO GS-VENDA-BRUTA
           ADD PRAZO-WK              TO PRAZO-GERAL
           ADD VENLIQDEF-WK          TO GS-VENDA-LIQUIDA-FAT


           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO GS-ALBUM-CLIENTE-ATUAL GS-PROD-FOTOG-ATUAL
                         GS-PROD-FITA-ATUAL GS-PAT-MED-CLIENTE-ATUAL
                         GS-VENDA-LIQUIDA-ATUAL GS-PATROCINIO-ATUAL

                         GS-ALBUM-CLIENTE-INI GS-PROD-FOTOG-INI
                         GS-PROD-FITA-INI     GS-PAT-MED-CLIENTE-INI
                         GS-VENDA-LIQUIDA-INI GS-PATROCINIO-INI

                         GS-ALBUM-PRODUZ GS-ALBUM-VEND GS-ALBUM-SALDO

                         GS-FITA-VENDIDA GS-DVD-VENDIDO
                         GS-POSTER-VENDIDO

                         GS-FOTO-PRODUZ GS-FOTO-PERD GS-FOTO-DISPON
                         GS-FOTO-AVULSA GS-FOTO-COMISSAO GS-FOTO-VEND
                         GS-FOTO-DEVOL  GS-FOTO-FOGO GS-FOTO-SALDO

                         GS-PERC-FOTO-PRODUZ GS-PERC-FOTO-PERD
                         GS-PERC-FOTO-AVULSA GS-PERC-FOTO-COMISSAO
                         GS-PERC-FOTO-DISPON GS-PERC-FOTO-DISPON1
                         GS-PERC-FOTO-VEND   GS-PERC-FOTO-DEVOL
                         GS-PERC-FOTO-FOGO   GS-PERC-FOTO-SALDO

                         GS-PERC-ALBUM-PRODUZ GS-PERC-ALBUM-VEND
                         GS-PERC-ALBUM-SALDO  GS-PERC-FITA-VENDIDA
                         GS-PERC-DVD-VENDIDO GS-PERC-POSTER-VENDIDO


                         GS-ALBUM-AEXECUTAR GS-PROD-FOTOGRAFIA
                         GS-PROD-FITAS GS-VENDA-LIQ-AEXECUTAR
                         GS-PATROCINIO-AEXECUTAR GS-PAT-FORM-AEXECUTAR

                         GS-PRECO-MEDIO-FOTO     GS-PRECO-MEDIO-CLIENTE
                         GS-PRAZO-MEDIO

                         GS-VENDA-BRUTA PRAZO-GERAL
                         GS-VENDA-LIQUIDA-FAT

                         GS-PATROCINIO-PAGO GS-PATROCINIO-N-PAGO
                         GS-PATROCINIO-MEDIO

                         GS-PERC-DEVOLVIDAS GS-PERC-VENDIDOS

                         GS-PATROCINIO-PAGO-AEXEC
                         GS-PATROCINIO-N-PAGO-AEXEC.

      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP108" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE ZEROS TO EXECUTADO-ANT
           MOVE SPACES TO CIDADE-ANT
           INITIALIZE REG-CIDADE.

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-EXECUTADO-TRUE AND EXECUTADO-WK = 1 OR
                   GS-AEXECUTAR-TRUE AND EXECUTADO-WK = 2
                      PERFORM TOTALIZA-CIDADE-REL
                ELSE CONTINUE
                END-IF
              END-READ
           END-PERFORM.
           PERFORM MOVER-DADOS-RELAT
           PERFORM TOTALIZA-RELAT.

           COPY DESCONDENSA.

       TOTALIZA-CIDADE-REL SECTION.
           IF EXECUTADO-ANT <> ZEROS
              IF EXECUTADO-WK <> EXECUTADO-ANT
                 PERFORM MOVER-DADOS-RELAT
                 INITIALIZE REG-CIDADE
                 PERFORM ACUMULA-DADOS-CIDADE-REL
              ELSE
                IF CIDADE-ANT <> SPACES
                   IF CIDADE-WK <> CIDADE-ANT
                      PERFORM MOVER-DADOS-RELAT
                      INITIALIZE REG-CIDADE
                      PERFORM ACUMULA-DADOS-CIDADE-REL
                   ELSE
                      PERFORM ACUMULA-DADOS-CIDADE-REL
                ELSE PERFORM ACUMULA-DADOS-CIDADE-REL
           ELSE PERFORM ACUMULA-DADOS-CIDADE-REL.
       ACUMULA-DADOS-CIDADE-REL SECTION.
           MOVE EXECUTADO-WK   TO EXECUTADO-ANT
           MOVE CIDADE-WK      TO CIDADE-ANT.

           ADD QT-FORM-WK          TO QT-FORM-CID
           ADD QT-FOTOS-WK         TO QT-FOTOS-CID
           ADD QT-FITAS-WK         TO QT-FITAS-CID
           ADD QT-DVD-WK           TO QT-DVD-CID
           ADD OBJ-VENDA-WK        TO OBJ-VENDA-CID
           ADD PATROCINIO-WK       TO PATROCINIO-CID
      *    ADD PAT-FORM-WK         TO PAT-FORM-CID
           ADD ALBPROD-WK          TO ALBPROD-CID
           ADD ALBVEND-WK          TO ALBVEND-CID
           ADD FOTPROD-WK          TO FOTPROD-CID
           ADD FOTVEND-WK          TO FOTVEND-CID
           ADD FOTAVUL-WK          TO FOTAVUL-CID
           ADD FOTMONT-WK          TO FOTMONT-CID
           ADD FOTFOGO-WK          TO FOTFOGO-CID
           ADD FOTDEVO-WK          TO FOTDEVO-CID
           ADD FITA-WK             TO FITA-CID
           ADD DVD-WK              TO DVD-CID
           ADD FATVENBRU-WK        TO FATVENBRU-CID
           ADD PRAZO-WK            TO PRAZO-CID
           ADD VENLIQDEF-WK        TO VENLIQDEF-CID
           ADD VENLIQIND-WK        TO VENLIQIND-CID
           ADD ALBPERD-WK          TO ALBPERD-CID
           ADD FOTPERD-WK          TO FOTPERD-CID
           ADD FOTCOMIS-WK         TO FOTCOMIS-CID
           ADD VENDAVUL-WK         TO VENDAVUL-CID
           ADD FORMANDOS-WK        TO FORMANDOS-CID.

       TOTALIZA-RELAT SECTION.
      * PREVISOES INICIAIS
           MOVE GS-ALBUM-CLIENTE-INI     TO ALBUM-CLIENTE-REL-I
           MOVE GS-PROD-FOTOG-INI        TO PROD-FOTOG-REL-I
           MOVE GS-PROD-FITA-INI         TO PROD-FITA-REL-I
           MOVE GS-PROD-DVD-INI          TO PROD-DVD-REL-I
           MOVE GS-PROD-POSTER-INI       TO PROD-POSTER-REL-I
           MOVE GS-VENDA-LIQUIDA-INI     TO VENDA-LIQUIDA-REL-I
           MOVE GS-PATROCINIO-INI        TO PATROCINIO-REL-I
           MOVE GS-PAT-MED-CLIENTE-INI   TO PAT-MED-CLIENTE-REL-I
      * PREVISOES ATUAIS
           MOVE GS-ALBUM-CLIENTE-ATUAL   TO ALBUM-CLIENTE-REL-A
           MOVE GS-PROD-FOTOG-ATUAL      TO PROD-FOTOG-REL-A
           MOVE GS-PROD-FITA-ATUAL       TO PROD-FITA-REL-A
           MOVE GS-PROD-DVD-ATUAL        TO PROD-DVD-REL-A
           MOVE GS-PROD-POSTER-ATUAL     TO PROD-POSTER-REL-A
           MOVE GS-VENDA-LIQUIDA-ATUAL   TO VENDA-LIQUIDA-REL-A
           MOVE GS-PATROCINIO-ATUAL      TO PATROCINIO-REL-A
           MOVE GS-PAT-MED-CLIENTE-ATUAL TO PAT-MED-CLIENTE-REL-A
      * FOTOGRAFIAS
           MOVE GS-FOTO-PRODUZ           TO FOTO-PRODUZ-REL
           MOVE GS-PERC-FOTO-PRODUZ      TO PERC-FOTO-PRODUZ-REL
           MOVE GS-FOTO-PERD             TO FOTO-PERD-REL
           MOVE GS-PERC-FOTO-PERD        TO PERC-FOTO-PERD-REL
           MOVE GS-FOTO-DISPON           TO FOTO-DISPON-REL
           MOVE GS-PERC-FOTO-DISPON      TO PERC-FOTO-DISPON-REL
           MOVE GS-PERC-FOTO-DISPON1     TO PERC-FOTO-DISPON-REL2
           MOVE GS-FOTO-VEND             TO FOTO-VEND-REL
           MOVE GS-PERC-VENDIDOS         TO PERC-FOTO-VEND-REL
           MOVE GS-PERC-FOTO-VEND        TO PERC-FOTO-VEND-REL2
           MOVE GS-FOTO-AVULSA           TO FOTO-AVULSA-REL
           MOVE GS-PERC-FOTO-AVULSA      TO PERC-FOTO-AVULSA-REL
           MOVE GS-FOTO-COMISSAO         TO FOTO-COMISSAO-REL
           MOVE GS-PERC-FOTO-COMISSAO    TO PERC-FOTO-COMISSAO-REL
           MOVE GS-FOTO-DEVOL            TO FOTO-DEVOL-REL
           MOVE GS-PERC-DEVOLVIDAS       TO PERC-FOTO-DEVOL-REL
           MOVE GS-PERC-FOTO-DEVOL       TO PERC-FOTO-DEVOL-REL2
           MOVE GS-FOTO-FOGO             TO FOTO-FOGO-REL
           MOVE GS-PERC-FOTO-FOGO        TO PERC-FOTO-FOGO-REL
           MOVE GS-FOTO-SALDO            TO FOTO-SALDO-REL
           MOVE GS-PERC-FOTO-SALDO       TO PERC-FOTO-SALDO-REL
      *ALBUM/CLIENTE
           MOVE GS-ALBUM-PRODUZ          TO ALBUM-PRODUZ-REL
           MOVE GS-PERC-ALBUM-PRODUZ     TO PERC-ALBUM-PRODUZ-REL
           MOVE GS-ALBUM-VEND            TO ALBUM-VEND-REL
           MOVE GS-PERC-ALBUM-VEND       TO PERC-ALBUM-VEND-REL
           MOVE GS-ALBUM-SALDO           TO ALBUM-SALDO-REL
           MOVE GS-PERC-ALBUM-SALDO      TO PERC-ALBUM-SALDO-REL
           MOVE GS-FITA-VENDIDA          TO FITA-VENDIDA-REL
           MOVE GS-PERC-FITA-VENDIDA     TO PERC-FITA-VENDIDA-REL
           MOVE GS-DVD-VENDIDO           TO DVD-VENDIDO-REL
           MOVE GS-PERC-DVD-VENDIDO      TO PERC-DVD-VENDIDO-REL
           MOVE GS-POSTER-VENDIDO        TO POSTER-VENDIDO-REL
           MOVE GS-PERC-POSTER-VENDIDO   TO PERC-POSTER-VENDIDO-REL
           MOVE GS-PATROCINIO-PAGO       TO PATROCINIO-P-REL
           MOVE GS-PATROCINIO-N-PAGO     TO PATROCINIO-N-REL
           MOVE GS-PATROCINIO-MEDIO      TO PATROCINIO-MEDIO-REL
      *FATURAMENTO
           MOVE GS-VENDA-BRUTA           TO VENDA-BRUTA-REL
           MOVE GS-PRAZO-MEDIO           TO PRAZO-MEDIO-REL
           MOVE GS-VENDA-LIQUIDA-FAT     TO VENDA-LIQUIDA-FAT-REL
           MOVE GS-PRECO-MEDIO-FOTO      TO PRECO-MEDIO-FOTO-REL
           MOVE GS-PRECO-MEDIO-CLIENTE   TO PRECO-MEDIO-CLIENTE-REL
      *CONTRATOS NAO EXECUTADOS
           MOVE GS-ALBUM-AEXECUTAR       TO ALBUM-AEXECUTAR-REL
           MOVE GS-VENDA-LIQ-AEXECUTAR   TO VENDA-LIQ-AEXECUTAR-REL
           MOVE GS-PROD-FOTOGRAFIA       TO PROD-FOTOGRAFIA-REL
           MOVE GS-PATROCINIO-AEXECUTAR  TO PATROCINIO-AEXECUTAR-REL
           MOVE GS-PROD-FITAS            TO PROD-FITAS-REL
           MOVE GS-PAT-FORM-AEXECUTAR    TO PAT-FORM-AEXECUTAR-REL
           MOVE GS-PROD-DVD              TO PROD-DVD-REL
           MOVE GS-PATROCINIO-N-PAGO-AEXEC TO
                                            PATROCINIO-N-PAGO-AEXEC-REL
           MOVE GS-PROD-POSTER           TO PROD-POSTER-REL
           MOVE GS-PATROCINIO-PAGO-AEXEC TO PATROCINIO-PAGO-AEXEC-REL

           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT FROM LINHA-01 AFTER PAGE.
           WRITE REG-RELAT FROM LINHA-02.
           WRITE REG-RELAT FROM LINHA-03.
           WRITE REG-RELAT FROM LINHA-04.
           WRITE REG-RELAT FROM LINHA-05.
           WRITE REG-RELAT FROM LINHA-06.
           WRITE REG-RELAT FROM LINHA-07.
           WRITE REG-RELAT FROM LINHA-08.
           WRITE REG-RELAT FROM LINHA-09.
           WRITE REG-RELAT FROM LINHA-10.
           WRITE REG-RELAT FROM LINHA-11.
           WRITE REG-RELAT FROM LINHA-12.
           WRITE REG-RELAT FROM LINHA-13.
           WRITE REG-RELAT FROM LINHA-14.
           WRITE REG-RELAT FROM LINHA-15.
           WRITE REG-RELAT FROM LINHA-16.
           WRITE REG-RELAT FROM LINHA-17.
           WRITE REG-RELAT FROM LINHA-18.
           WRITE REG-RELAT FROM LINHA-19.
           WRITE REG-RELAT FROM LINHA-20.
           WRITE REG-RELAT FROM LINHA-21.
           WRITE REG-RELAT FROM LINHA-211.
           WRITE REG-RELAT FROM LINHA-22.
           WRITE REG-RELAT FROM LINHA-23.
           WRITE REG-RELAT FROM LINHA-24.
           WRITE REG-RELAT FROM LINHA-25.
           WRITE REG-RELAT FROM LINHA-26.
           WRITE REG-RELAT FROM LINHA-27.
           WRITE REG-RELAT FROM LINHA-28.
           WRITE REG-RELAT FROM LINHA-29.

       MOVER-DADOS-RELAT SECTION.
           ADD 3 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.

           MOVE CIDADE-ANT        TO LINDET-REL(1: 14)
           MOVE QT-FORM-CID       TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(15: 10)
           MOVE QT-FOTOS-CID      TO QTDE2-E
           MOVE QTDE2-E           TO LINDET-REL(25: 13)
           MOVE QT-FITAS-CID      TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(37: 10)
           MOVE OBJ-VENDA-CID     TO VALOR1-E
           MOVE VALOR1-E          TO LINDET-REL(47: 15)
           MOVE PATROCINIO-CID    TO VALOR2-E
           MOVE VALOR2-E          TO LINDET-REL(62: 14)
           COMPUTE PAT-FORM-CID = PATROCINIO-CID / QT-FORM-CID
           MOVE PAT-FORM-CID      TO QTDE3-E
           MOVE QTDE3-E           TO LINDET-REL(76: 9)

           IF EXECUTADO-WK = 2
              WRITE REG-RELAT FROM LINDET
              MOVE "INSERE-LIST" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
              MOVE ALBPROD-CID         TO QTDE4-E
              MOVE QTDE4-E             TO LINDET-REL(85: 9)
              MOVE FORMANDOS-CID       TO QTDE4-E
              MOVE QTDE4-E             TO LINDET-REL(94: 9)
              COMPUTE SALALB-W = ALBPROD-CID - FORMANDOS-CID
              MOVE SALALB-W            TO QTDE4-E
              MOVE QTDE4-E             TO LINDET-REL(103: 8)
              WRITE REG-RELAT FROM LINDET
              PERFORM MOVER-DADOS-RELAT1.

       MOVER-DADOS-RELAT1 SECTION.
      *    2o. LINHA DA LINDET
           MOVE SPACES              TO LINDET-REL

           COMPUTE DISPONIVEL-W = (FOTMONT-CID + FOTAVUL-CID) -
                                  (FOTCOMIS-CID + FOTFOGO-CID).
           MOVE DISPONIVEL-W        TO QTDE5-E
           MOVE QTDE5-E             TO LINDET-REL(1: 10)
           MOVE FOTCOMIS-CID        TO QTDE6-E
           MOVE QTDE6-E             TO LINDET-REL(11: 8)
           MOVE FOTVEND-CID         TO QTDE5-E
           MOVE QTDE5-E             TO LINDET-REL(19: 10)
           MOVE FOTDEVO-CID         TO QTDE4-E
           MOVE QTDE4-E             TO LINDET-REL(29: 9)
           COMPUTE SALFOT-W = FOTPROD-CID - (FOTVEND-CID + FOTPERD-CID +
                              FOTDEVO-CID + FOTCOMIS-CID + VENDAVUL-CID
                              + FOTFOGO-CID).
           MOVE SALFOT-W            TO QTDE4-E
           MOVE QTDE4-E             TO LINDET-REL(38: 9)
           MOVE FITA-CID            TO QTDE6-E
           MOVE QTDE6-E             TO LINDET-REL(47: 8)

           COMPUTE PERC-W = (FOTVEND-CID / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(56: 6)
           COMPUTE PERC-W = (FOTDEVO-CID / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(62: 6)
           COMPUTE PERC-W = (SALFOT-W / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(68: 6)

           COMPUTE PRAZO-W = PRAZO-CID / FATVENBRU-CID.
           MOVE PRAZO-W             TO QTDE8-E
           MOVE QTDE8-E             TO LINDET-REL(74: 7)

           MOVE VENLIQIND-CID       TO VALOR2-E
           MOVE VALOR2-E            TO LINDET-REL(81: 14)
           COMPUTE PRECOMED-W = VENLIQIND-CID / FOTVEND-CID.
           MOVE PRECOMED-W          TO QTDE9-E
           MOVE QTDE9-E             TO LINDET-REL(95: 8)
           COMPUTE VENMEDCLI-W = VENLIQIND-CID / FORMANDOS-CID.
           MOVE VENMEDCLI-W         TO QTDE10-E
           MOVE QTDE10-E            TO LINDET-REL(102: 9)

           WRITE REG-RELAT FROM LINDET.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.


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
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM CAB06.
           WRITE REG-RELAT FROM CAB03.
           MOVE 9 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 COD050 CAD010 CAD012 COD051
                 COD005 COD002 MTD001 MTD020 RCD100.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
