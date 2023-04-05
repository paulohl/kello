       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP104.
       DATE-WRITTEN. 23/08/1999.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: Relatório 05
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
           COPY CGPX001.
           COPY CAPX004.
           COPY COPX001.
           COPY COPX002.
           COPY COPX005.
           COPY COPX040.
           COPY COPX050.
           COPY COPX051.
           COPY COPX104.
           COPY CAPX010.
           COPY CAPX012.
           COPY MTPX001.
           COPY MTPX020.
           COPY RCPX100.
           COPY RCPX101.
           COPY REPX100.
           COPY REPX101.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = EXECUTADO-WK CONTRATO-WK
                  ALTERNATE RECORD KEY IS ALT1-WK = EXECUTADO-WK
                     CIDADE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK = EXECUTADO-WK
                     REGIAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-WK = EXECUTADO-WK
                     REPRESENT-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-WK = EXECUTADO-WK
                     MESANO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT5-WK = EXECUTADO-WK
                     VENLIQIND-WK OBJ-VENDA-WK-ATUAL WITH DUPLICATES.

           SELECT AUXILIAR  ASSIGN    TO ARQUIVO-AUXILIAR
                            ORGANIZATION  IS      INDEXED
                            ACCESS MODE   IS      DYNAMIC
                            RECORD KEY    IS    AUX-CHAVE
                            LOCK   MODE   IS    AUTOMATIC
                            WITH   LOCK   ON       RECORD
                            FILE   STATUS IS  FS-AUXILIAR.

           SELECT AUXILIAR2 ASSIGN   TO ARQUIVO-AUXILIAR2
                            ORGANIZATION  IS      INDEXED
                            ACCESS MODE   IS      DYNAMIC
                            RECORD KEY    IS   AUX-CHAVE2
                            LOCK   MODE   IS    AUTOMATIC
                            WITH   LOCK   ON       RECORD
                            FILE   STATUS IS FS-AUXILIAR2.

           SELECT RELAT ASSIGN TO PRINTER.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW004.
       COPY CAPW012.
       COPY CGPW001.
       COPY COPW001.
       COPY COPW002.
       COPY COPW005.
       COPY COPW040.
       COPY COPW050.
       COPY COPW051.
       COPY COPW104.
       COPY MTPW001.
       COPY MTPW020.
       COPY RCPW100.
       COPY RCPW101.
       COPY REPW100.
       COPY REPW101.

       FD  AUXILIAR.
       01  REG-AUXILIAR.
           05 AUX-CHAVE             PIC 9(4).

       FD  AUXILIAR2.
       01  REG-AUXILIAR2.
           05 AUX-CHAVE2            PIC 9(08).

       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK          PIC 9(4).
           05  IDENTIFICACAO-WK     PIC X(10).
           05  CIDADE-WK            PIC X(9).
           05  REGIAO-WK            PIC X(9).
           05  REPRESENT-WK         PIC X(8).
           05  QT-FORM-WK-ATUAL     PIC 9(4).
           05  QT-FORM-WK-INI       PIC 9(4).
           05  PADRAO-WK            PIC X.
           05  QT-FOTOS-WK-ATUAL    PIC 9(5).
           05  QT-FOTOS-WK-INI      PIC 9(5).
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
           05  ALBPROD-WK           PIC 9999.
           05  ALBVEND-WK           PIC 9999.
           05  FOTPROD-WK           PIC 9(06).
           05  FOTVEND-WK           PIC 9(06).
           05  FOTAVUL-WK           PIC 9(05).
           05  FOTMONT-WK           PIC 9(06).
           05  FOTFOGO-WK           PIC 9(04).
           05  FOTDEVO-WK           PIC S9(5).
           05  FITA-WK              PIC 9(04).
           05  DVD-WK               PIC 9(04).
           05  POSTER-WK            PIC 9(04).
           05  FATVENBRU-WK         PIC 9(8)V99.
           05  PRAZO-WK             PIC 9(11)V99.
           05  VENLIQDEF-WK         PIC 9(10)V99.
           05  VENLIQIND-WK         PIC 9(10)V99.
           05  ALBPERD-WK           PIC 999.
           05  FOTPERD-WK           PIC 9999.
           05  FOTCOMIS-WK          PIC 9(4).
           05  VENDAVUL-WK          PIC 9(5).
           05  FORMANDOS-WK         PIC 9(04).
           05  EXECUTADO-WK         PIC 9.
      *    EXECUTADO = 1(SIM)    2(NAO)

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "COP104.CPB".
           COPY "COP104.CPY".
           COPY "CBDATA.CPY".
           copy cpdias1.
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-COD051             PIC XX       VALUE SPACES.
           05  ST-COD104             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-MTD001             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  FS-AUXILIAR           PIC XX       VALUE SPACES.
           05  FS-AUXILIAR2          PIC XX       VALUE SPACES.
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
           05  rc100                 PIC x(12)    VALUE SPACES.
           05  CONT-W          PIC 9(4)            VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  VALOR-E               PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E2              PIC ZZZ,ZZ   BLANK WHEN ZEROS.
           05  VALOR-E3              PIC ZZ,ZZ    BLANK WHEN ZEROS.
           05  VALOR-E4              PIC Z.ZZZ,ZZ  BLANK WHEN ZEROS.
           05  PERC-E                PIC ZZ9,9 BLANK WHEN ZEROS.
           05  PRAZO-E               PIC ZZZ,ZZ   BLANK WHEN ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  DATAI.
               10  ANO-I       PIC 9999.
               10  MES-I       PIC 99.
               10  DIA-I       PIC 99.
           05  DATA-I REDEFINES DATAI PIC 9(08).
           05  MESANO-E              PIC 99/9999.
           05  FOTDEVO-W             PIC S9(5)      VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)       VALUE ZEROS.
           05  ALB-W                 PIC 9(4)       VALUE ZEROS.
           05  VALOR-PM              PIC 9(14)V99   VALUE ZEROS.
           05  DIVISAO-W             PIC 9(6)V9(5)  VALUE ZEROS.
           05  PERC-W                PIC 9(5)V9999  VALUE ZEROS.
           05  PRECO-W               PIC 9(5)V99    VALUE ZEROS.
           05  SALFOT-W              PIC 9(06)      VALUE ZEROS.
           05  SALALB-W              PIC 9(04)      VALUE ZEROS.
           05  DISPONIVEL-W          PIC S9(6)      VALUE ZEROS.
           05  PRECOMED-W            PIC 9(08)V99   VALUE ZEROS.
           05  VENMEDCLI-W           PIC 9(08)V99   VALUE ZEROS.
           05  PRAZO-W               PIC 9(05)V999  VALUE ZEROS.
           05  PRAZO-GERAL           PIC 9(14)V99   VALUE ZEROS.
           05  QUANTIDADE            PIC 9(02)      VALUE ZEROS.

           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).

           05  ACHEI                 PIC X(01).

      *    VARIAVEIS P/ CALCULO DE PATROCINIO PAGOS E A PAGAR
           05  CUSTO-TOTAL           PIC 9(8)V99    VALUE ZEROS.
           05  CUSTO-PREVISTO        PIC 9(8)V99    VALUE ZEROS.
           05  CUSTO-W               PIC 9(8)V99    VALUE ZEROS.
           05  JUROS-W               PIC 9(8)V99    VALUE ZEROS.
           05  CONT                  PIC 9(4)       VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8)  VALUE ZEROS.
           05  TAXA-W                PIC 9(3)V99999 VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)       VALUE ZEROS.
           05  MESES-W               PIC 9(3)       VALUE ZEROS.
           05  VLR-GERAL             PIC 9(8)V99    VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)       VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)       VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)      VALUE SPACES.
           05  LIN                   PIC 9(02)      VALUE ZEROS.
           05  VECTO-FIM             PIC 9(08)      VALUE ZEROS.
           05  VECTO-INI             PIC 9(08)      VALUE ZEROS.

           05  TOT-PARTICIPANTE      PIC 9(08)      VALUE ZEROS.
           05  REPORTAGEM            PIC 9(09)V99   VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(33)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(36)   VALUE
           "RELATORIO 05          - ORDEM: ".
           05  ORDEM-REL           PIC X(18)   VALUE SPACES.
           05  FILLER              PIC X(20)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(126)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(126)   VALUE
           "CONT IDENTIFIC. CIDADE    REGIAO    REPRESEN MES/ANO FORM P
      -    "FOTOS FITA    PREV.VENDA    PATROCINIO   PAT.FORM DATA REAL
      -    "".

       01  CAB05.
           05  FILLER              PIC X(127)  VALUE
           "---FORMANDO--- -----------FOTOGRAFIAS-------------- --VEND -
      -    "RESULT.FOTOS %-----------                                --O
      -    "UTROS--".

       01  CAB06.
           05  FILLER              PIC X(130)   VALUE
           "PROD VEND SALD DISP. COMIS  VENDA DEVOL FOGO  SALDO VENDA DE
      -    "VOL COMIS FOGO  SALDO   P.M. VDA.LIQ.IN P/FOTO    V.MED.  DV
      -    "D POSTER".

       01  LINDET.
           05  LINDET-REL          PIC X(129)  VALUE SPACES.

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
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "COD051"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD051.
           MOVE "COD104"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD104.
           MOVE "MTD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD001.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.

           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           OPEN INPUT CAD010 CGD001 COD040 COD005 COD050 COD051 RED100
                      COD002 CAD012 MTD001 MTD020 RCD100 COD001 RED101
                      RCD101
           OPEN I-O   COD104
           CLOSE      COD104
           OPEN INPUT COD104.

           MOVE "ACOP104"  TO ARQUIVO-AUXILIAR
           MOVE "A2COP104" TO ARQUIVO-AUXILIAR2

           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED101 <> "00"
              MOVE "ERRO ABERTURA RED101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-COD104 <> "00"
              MOVE "ERRO ABERTURA COD104: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD104 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
                    PERFORM VERIFICAR-SENHA-STATUS
               WHEN GS-PRINTER-FLG-TRUE
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    MOVE "Deseja Realmente Carregar os Dados ?" TO
                    MENSAGEM
                    MOVE "Q" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                    IF RESP-MSG = "S"
                       PERFORM GRAVA-WORK
                       PERFORM ZERA-VARIAVEIS
                       PERFORM CARREGA-LISTA
                    END-IF
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-REGIAO-TRUE
                    PERFORM LE-REGIAO
               WHEN GS-CHAMAR-POP-REGIAO-TRUE
                    PERFORM CHAMAR-POPUP-REGIAO
               WHEN GS-LE-REPRES-TRUE
                    PERFORM LE-REPRES
               WHEN GS-CHAMAR-POP-REPRES-TRUE
                    PERFORM CHAMAR-POPUP-REPRES
               WHEN GS-LE-STATUS-TRUE
                    PERFORM LE-STATUS
               WHEN GS-CHAMAR-POP-STATUS-TRUE
                    PERFORM CHAMAR-POPUP-STATUS
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR
               WHEN GS-CARREGAR-STATUS-TRUE
                    PERFORM CARREGAR-STATUS
               WHEN GS-GRAVA-STATUS-TRUE
                    PERFORM GRAVA-STATUS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB9
           NOT INVALID KEY
               ENABLE-OBJECT PB9.

           CLOSE CAD004.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GRAVA-STATUS SECTION.
           CLOSE    COD104
           OPEN I-O COD104

           INITIALIZE REG-COD104
           START COD104 KEY IS NOT LESS CODIGO-COP104 INVALID KEY
                MOVE "10" TO ST-COD104.
           PERFORM UNTIL ST-COD104 = "10"
                READ COD104 NEXT AT END
                     MOVE "10" TO ST-COD104
                NOT AT END
                     DELETE COD104 INVALID KEY
                         MOVE "Erro de Exclusão...COD104" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                     END-DELETE
                END-READ
           END-PERFORM

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-COP104
               WRITE REG-COD104
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      COD104
           OPEN INPUT COD104.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-COD104
           START COD104 KEY IS NOT LESS CODIGO-COP104 INVALID KEY
               MOVE "10" TO ST-COD104.

           PERFORM UNTIL ST-COD104 = "10"
               READ COD104 NEXT AT END
                    MOVE "10" TO ST-COD104
               NOT AT END
                    MOVE CODIGO-COP104 TO CODIGO-CO01
                    READ COD001 NOT INVALID KEY
                         MOVE "S"              TO ACHEI
                         MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                         MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                         MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                    END-READ
               END-READ
           END-PERFORM

           IF ACHEI = "N"
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM.

       INCLUIR SECTION.
           MOVE "Você Deseja Incluir o Status?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE GS-STATUS        TO GS-LINHA-STATUS(1:2)
              MOVE GS-DESC-STATUS   TO GS-LINHA-STATUS(4:30)
              MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       LE-STATUS SECTION.
           MOVE GS-STATUS              TO CODIGO-CO01
           READ COD001 INVALID KEY
                MOVE SPACES            TO STATUS-CO01
           END-READ
           MOVE STATUS-CO01            TO GS-DESC-STATUS.

       CHAMAR-POPUP-STATUS SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP001T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS.

       LE-REPRES SECTION.
           MOVE GS-REPRESENTANTE       TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES            TO NOME-CG01
           END-READ
           MOVE NOME-CG01              TO GS-DESC-REPRESENTANTE.

       CHAMAR-POPUP-REPRES SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-REPRESENTANTE.
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-REPRESENTANTE.

       CHAMAR-POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2)  TO GS-REGIAO.
           MOVE PASSAR-STRING-1(1: 30)  TO GS-DESC-REGIAO.

       LE-REGIAO SECTION.
           MOVE GS-REGIAO       TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG.
           MOVE NOME-REG        TO GS-DESC-REGIAO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           EVALUATE GS-OP-FILTRO
               WHEN 1 PERFORM DATA-VENDA-PRODUCAO
               WHEN 2 PERFORM PREV-CONTRATO
           END-EVALUATE


           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       DATA-VENDA-PRODUCAO SECTION.
           INITIALIZE REG-RCD100

           MOVE GS-DATA-INI TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV    TO VECTO-INI
           MOVE GS-DATA-FIM TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV    TO VECTO-FIM

           OPEN OUTPUT AUXILIAR AUXILIAR2
           CLOSE       AUXILIAR AUXILIAR2
           OPEN I-O    AUXILIAR AUXILIAR2

           EVALUATE GS-OP-DATA
               WHEN 1 PERFORM POR-VENDA
               WHEN 2 PERFORM POR-PRODUCAO
           END-EVALUATE

           CLOSE       AUXILIAR AUXILIAR2
           OPEN INPUT  AUXILIAR AUXILIAR2

           INITIALIZE REG-AUXILIAR
           START AUXILIAR KEY IS NOT LESS AUX-CHAVE INVALID KEY
                 MOVE "10" TO FS-AUXILIAR.

           PERFORM UNTIL FS-AUXILIAR = "10"
                 READ AUXILIAR NEXT AT END
                      MOVE "10" TO FS-AUXILIAR
                 NOT AT END
                      MOVE AUX-CHAVE TO NR-CONTRATO-CO40
                      READ COD040 NOT INVALID KEY
                           INITIALIZE REG-WORK
                           MOVE CIDADE-CO40        TO CIDADE
                           READ CAD010 INVALID KEY
                                MOVE SPACES TO NOME-CID
                                MOVE SPACES TO UF-CID
                                MOVE ZEROS  TO REGIAO-CID
                           END-READ
                           MOVE MESANO-PREV-CO40   TO MESANO-WK
                           MOVE NR-CONTRATO-CO40   TO CONTRATO-WK
                           MOVE NOME-CID           TO CIDADE-WK
                           MOVE REGIAO-CID         TO CODIGO-REG
                           READ CAD012 INVALID KEY
                                MOVE SPACES TO NOME-REG
                           END-READ
                           MOVE NOME-REG           TO REGIAO-WK
                           MOVE REPRESENTANTE-CO40 TO CODIGO-CG01
                           READ CGD001 INVALID KEY
                                MOVE SPACES TO NOME-CG01
                           END-READ
                           MOVE NOME-CG01        TO REPRESENT-WK

                           ADD VLR-COMISSAO-CO40 TO PATROCINIO-WK-ATUAL
                           ADD VLR-COMISSAO-CO40 TO PATROCINIO-WK-INI

                           ADD VLR-COMISSAO-CO40 TO PATROCINIO-N-PAGO-WK

                           MOVE DATA-PREV-VENDA-CO40 TO DATA-INV
                           CALL "GRIDAT2" USING DATA-INV
                           MOVE DATA-INV TO GRDIAS-AAMMDD-FINAL
                           MOVE ASSINATURA-CO40 TO GRDIAS-AAMMDD-INICIAL
                           CALL "GRDIAS1" USING PARAMETROS-GRDIAS
                           MOVE GRDIAS-NUM-DIAS TO CONT-W
                           COMPUTE MESES-W = CONT-W / 30
                           COMPUTE VLR-GERAL = MESES-W *
                                   VLR-COMISSAO-CO40

                           MOVE IDENTIFICACAO-CO40 TO IDENTIFICACAO-WK

                           INITIALIZE REG-MTD020
                                      QT-FORM-WK-ATUAL
                           MOVE NR-CONTRATO-CO40   TO CONTRATO-MTG
                           START MTD020 KEY IS NOT LESS ALBUM-MTG
                                                             INVALID KEY
                                MOVE "10" TO ST-MTD020
                           END-START
                           PERFORM UNTIL ST-MTD020 = "10"
                                READ MTD020 NEXT AT END
                                     MOVE "10" TO ST-MTD020
                                NOT AT END
                                     IF NR-CONTRATO-CO40 <> CONTRATO-MTG
                                        MOVE "10" TO ST-MTD020
                                     ELSE
                                        IF NAO-GEROU-ALBUM-MTG <> 1
                                           ADD 1 TO QT-FORM-WK-ATUAL
                                        END-IF
                                     END-IF
                                END-READ
                           END-PERFORM

                           IF QT-FORM-WK-ATUAL = 0
                              MOVE QTDE-FORM-CO40  TO QT-FORM-WK-ATUAL
                           END-IF

                           IF QTDE-FORM-INI-CO40 NOT > 0
                              MOVE QTDE-FORM-CO40 TO QT-FORM-WK-INI
                           ELSE
                              MOVE QTDE-FORM-INI-CO40 TO QT-FORM-WK-INI
                           END-IF

                           MOVE PADRAO-CO40 TO PADRAO-CO05 PADRAO-WK
                           READ COD005 INVALID KEY
                                MOVE ZEROS TO PREV-FOTOS-CO05
                           END-READ
                           COMPUTE QT-FOTOS-WK-ATUAL = PREV-FOTOS-CO05 *
                                                       QT-FORM-WK-ATUAL

                           COMPUTE QT-FOTOS-WK-INI   = PREV-FOTOS-CO05 *
                                                       QT-FORM-WK-INI

                           MOVE ZEROS TO QT-FITAS-WK-ATUAL
                                         QT-FITAS-WK-INI
                           MOVE ZEROS TO QT-DVD-WK-ATUAL
                                         QT-DVD-WK-INI
                                         QT-POSTER-WK-ATUAL
                                         QT-POSTER-WK-INI

                           IF GS-BRINDES = 1
                              PERFORM CALCULAR-DADOS-BRINDE
                           END-IF

                           IF GS-REPORTAGENS = 1
                              PERFORM CALCULAR-REPORTAGEM
                           END-IF

                           IF GS-ACP-VENDA = 1
                              PERFORM CALCULAR-COMISSAO
                           END-IF

                           PERFORM INICIO-MONTAGEM

                           MOVE CONTRATO-WK     TO GS-EXIBE-CONTRATO
                           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                           WRITE REG-WORK
                      END-READ
                 END-READ
           END-PERFORM

           CLOSE       AUXILIAR AUXILIAR2.

       POR-VENDA SECTION.
           MOVE VECTO-INI     TO DATAVEN-REC
           START RCD100 KEY IS NOT < DATAVEN-REC INVALID KEY
                 MOVE "10" TO ST-RCD100
           END-START
           PERFORM UNTIL ST-RCD100 = "10"
              READ RCD100 NEXT AT END
                  MOVE "10" TO ST-RCD100
              NOT AT END
                  MOVE ALBUM-REC        TO GS-EXIBE-CONTRATO
                  MOVE "TELA-AGUARDA1"  TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  IF DATAVEN-REC > VECTO-FIM
                     MOVE "10" TO ST-RCD100
                  ELSE
                     MOVE ALBUM-REC(1:4)     TO NR-CONTRATO-CO40
                     READ COD040 NOT INVALID KEY
                          IF GS-REPRESENTANTE = 0 OR REPRESENTANTE-CO40
                             MOVE ALBUM-REC(1:4)  TO AUX-CHAVE
                             READ AUXILIAR INVALID KEY
                                  WRITE REG-AUXILIAR
                                  END-WRITE
                             END-READ
                             MOVE ALBUM-REC       TO AUX-CHAVE2
                             WRITE REG-AUXILIAR2
                             END-WRITE
                          END-IF
                     END-READ
                  END-IF
              END-READ
           END-PERFORM.

       POR-PRODUCAO SECTION.
           INITIALIZE REG-MTD020
           MOVE VECTO-INI     TO DATAMOV-MTG
           START MTD020 KEY IS NOT LESS CHAVE-MTG INVALID KEY
                MOVE "10" TO ST-MTD020
           END-START
           PERFORM UNTIL ST-MTD020 = "10"
                READ MTD020 NEXT AT END
                     MOVE "10" TO ST-MTD020
                NOT AT END
                     MOVE DATAMOV-MTG      TO GS-EXIBE-CONTRATO
                     MOVE "TELA-AGUARDA1"  TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     IF DATAMOV-MTG > VECTO-FIM
                        MOVE "10" TO ST-MTD020
                     ELSE
                        MOVE ALBUM-MTG    TO ALBUM-REC
                        READ RCD100 NOT INVALID KEY
                             MOVE ALBUM-REC(1:4) TO NR-CONTRATO-CO40
                             READ COD040 NOT INVALID KEY
                                  IF GS-REPRESENTANTE = 0 OR
                                     REPRESENTANTE-CO40
      *                              IF STATUS-CO40 < 50
      *                                 CONTINUE
      *                              ELSE
                                        PERFORM PESQUISAR-STATUS
                                        IF ACHEI = "S"
                                           MOVE CIDADE-CO40 TO CIDADE
                                           READ CAD010 INVALID KEY
                                                MOVE SPACES TO NOME-CID
                                                MOVE SPACES TO UF-CID
                                                MOVE ZEROS TO REGIAO-CID
                                           END-READ
                                           IF GS-UF = SPACES OR UF-CID
                                              IF GS-REGIAO = 0 OR
                                                 REGIAO-CID
                                                 MOVE ALBUM-REC(1:4)
                                                   TO AUX-CHAVE
                                                 READ AUXILIAR INVALID
                                                                     KEY
                                                      WRITE REG-AUXILIAR
                                                      END-WRITE
                                                 END-READ
                                                 MOVE ALBUM-REC TO
                                                      AUX-CHAVE2
                                                 WRITE REG-AUXILIAR2
                                                 END-WRITE
                                              END-IF
                                           END-IF
      *                                 END-IF
                                     END-IF
                                  END-IF
                             END-READ
                        END-READ
                     END-IF
                END-READ
           END-PERFORM.

       PREV-CONTRATO SECTION.
           MOVE GS-MESANO-INI  TO MESANO-W
           MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
           MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I       TO MESANO-INI
           MOVE GS-MESANO-FIM  TO MESANO-W
           MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
           MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I       TO MESANO-FIM


           MOVE MESANO-INI     TO MESANO-PREV-CO40
           MOVE ZEROS          TO NR-CONTRATO-CO40
           START COD040 KEY IS NOT < ALT1-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
                 READ COD040 NEXT RECORD AT END
                      MOVE "10" TO ST-COD040
                 NOT AT END
                      MOVE NR-CONTRATO-CO40 TO GS-EXIBE-CONTRATO
                      MOVE "TELA-AGUARDA1"  TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF MESANO-PREV-CO40 > MESANO-FIM
                         MOVE "10" TO ST-COD040
                      ELSE
                         IF GS-REPRESENTANTE = 0 OR REPRESENTANTE-CO40
                            PERFORM MOVER-DADOS-WORK
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

       MOVER-DADOS-WORK SECTION.
           INITIALIZE REG-WORK.
      *    IF STATUS-CO40 < 50
      *       CONTINUE
      *    ELSE
              PERFORM PESQUISAR-STATUS
              IF ACHEI = "S"
                 MOVE CIDADE-CO40        TO CIDADE
                 READ CAD010 INVALID KEY
                      MOVE SPACES TO NOME-CID
                      MOVE SPACES TO UF-CID
                      MOVE ZEROS  TO REGIAO-CID
                 END-READ
                 IF GS-UF = SPACES OR UF-CID
                    IF GS-REGIAO = ZEROS OR REGIAO-CID
                       MOVE MESANO-PREV-CO40   TO MESANO-WK
                       MOVE NR-CONTRATO-CO40   TO CONTRATO-WK
                       MOVE NOME-CID           TO CIDADE-WK
                       MOVE REGIAO-CID         TO CODIGO-REG
                       READ CAD012 INVALID KEY
                            MOVE SPACES TO NOME-REG
                       END-READ
                       MOVE NOME-REG           TO REGIAO-WK
                       MOVE REPRESENTANTE-CO40 TO CODIGO-CG01
                       READ CGD001 INVALID KEY
                            MOVE SPACES TO NOME-CG01
                       END-READ
                       MOVE NOME-CG01          TO REPRESENT-WK

                       ADD VLR-COMISSAO-CO40   TO PATROCINIO-WK-ATUAL
                       ADD VLR-COMISSAO-CO40   TO PATROCINIO-WK-INI

                       ADD VLR-COMISSAO-CO40   TO PATROCINIO-N-PAGO-WK

                       MOVE DATA-PREV-VENDA-CO40 TO DATA-INV
                       CALL "GRIDAT2" USING DATA-INV
                       MOVE DATA-INV TO GRDIAS-AAMMDD-FINAL
                       MOVE ASSINATURA-CO40 TO GRDIAS-AAMMDD-INICIAL
                       CALL "GRDIAS1" USING PARAMETROS-GRDIAS
                       MOVE GRDIAS-NUM-DIAS TO CONT-W
                       COMPUTE MESES-W = CONT-W / 30
                       COMPUTE VLR-GERAL = MESES-W * VLR-COMISSAO-CO40

                       MOVE IDENTIFICACAO-CO40 TO IDENTIFICACAO-WK

                       INITIALIZE REG-MTD020
                                  QT-FORM-WK-ATUAL
                       MOVE NR-CONTRATO-CO40   TO CONTRATO-MTG
                       START MTD020 KEY IS NOT LESS ALBUM-MTG INVALID
                                                                     KEY
                            MOVE "10" TO ST-MTD020
                       END-START
                       PERFORM UNTIL ST-MTD020 = "10"
                            READ MTD020 NEXT AT END
                                 MOVE "10" TO ST-MTD020
                            NOT AT END
                                 IF NR-CONTRATO-CO40 <> CONTRATO-MTG
                                    MOVE "10" TO ST-MTD020
                                 ELSE
                                    IF NAO-GEROU-ALBUM-MTG <> 1
                                       ADD 1 TO QT-FORM-WK-ATUAL
                                    END-IF
                                 END-IF
                            END-READ
                       END-PERFORM

      *>Bloco Novo
                       IF QT-FORM-WK-ATUAL = 0
                          MOVE QTDE-FORM-CO40  TO QT-FORM-WK-ATUAL
      *                ELSE
      *                   display "nr-contrato-co40 = " QT-FORM-WK-ATUAL
      *                           " QTDE-FORM-CO40 = " QTDE-FORM-CO40
      *                           STOP " "
                       END-IF

                       IF QTDE-FORM-INI-CO40 NOT > 0
                          MOVE QTDE-FORM-CO40 TO QT-FORM-WK-INI
                       ELSE
                          MOVE QTDE-FORM-INI-CO40 TO QT-FORM-WK-INI
                       END-IF
      *>Fim do Bloco Novo

      *>Alteração a Pedido do Anderson dia 20/04/2010
      *                MOVE QTDE-FORM-CO40        TO QT-FORM-WK-ATUAL
      *                IF QTDE-FORM-INI-CO40 NOT > 0
      *                   MOVE QTDE-FORM-CO40     TO QT-FORM-WK-INI
      *                ELSE
      *                   MOVE QTDE-FORM-INI-CO40 TO QT-FORM-WK-INI
      *                END-IF



                       MOVE PADRAO-CO40        TO PADRAO-CO05 PADRAO-WK
                       READ COD005 INVALID KEY
                            MOVE ZEROS TO PREV-FOTOS-CO05
                       END-READ
                       COMPUTE QT-FOTOS-WK-ATUAL = PREV-FOTOS-CO05 *
                                                   QT-FORM-WK-ATUAL

                       COMPUTE QT-FOTOS-WK-INI   = PREV-FOTOS-CO05 *
                                                   QT-FORM-WK-INI

                       MOVE ZEROS TO QT-FITAS-WK-ATUAL QT-FITAS-WK-INI
                       MOVE ZEROS TO QT-DVD-WK-ATUAL QT-DVD-WK-INI
                                     QT-POSTER-WK-ATUAL QT-POSTER-WK-INI

                       IF GS-BRINDES = 1
                          PERFORM CALCULAR-DADOS-BRINDE
                       END-IF

                       IF GS-REPORTAGENS = 1
                          PERFORM CALCULAR-REPORTAGEM
                       END-IF

                       IF GS-ACP-VENDA = 1
                          PERFORM CALCULAR-COMISSAO
                       END-IF

                       COMPUTE PAT-FORM-WK-ATUAL = PATROCINIO-WK-ATUAL
                                                    / QT-FORM-WK-ATUAL
                       COMPUTE PAT-FORM-WK-INI = PATROCINIO-WK-INI
                                                   / QT-FORM-WK-INI

                       PERFORM INICIO-MONTAGEM

                       MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       WRITE REG-WORK
                    END-IF
                 END-IF
              END-IF.
      *    END-IF.

       PESQUISAR-STATUS SECTION.
           MOVE "N" TO ACHEI

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES OR ACHEI = "S"
               IF GS-LINHA-STATUS(1:2) = STATUS-CO40
                  MOVE "S" TO ACHEI
               END-IF
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       INICIO-MONTAGEM SECTION.
           MOVE ZEROS                     TO FOTAVUL-WK
           MOVE NR-CONTRATO-CO40          TO CONTRATO-MT01
           READ MTD001 INVALID KEY
                MOVE ZEROS                TO FOTPROD-WK
                                             FOTPERD-WK
                                             FOTMONT-WK
                                             ALBPROD-WK
           NOT INVALID KEY
                    MOVE PRODUZIDA-MT01   TO FOTPROD-WK
                    MOVE PERDIDA-MT01     TO FOTPERD-WK
                    MOVE MONTADA-MT01     TO FOTMONT-WK
                    MOVE CLIEN-ALBUM-MT01 TO ALBPROD-WK
           END-READ.

           MOVE 2                TO EXECUTADO-WK.
           IF GS-OP-FILTRO = 2
              PERFORM LEITURA-MTD020
              PERFORM LEITURA-RECIBO
           ELSE
              PERFORM LEITURA-AUX2-MTD020
              PERFORM LEITURA-AUX2-RECIBO.

       LEITURA-AUX2-MTD020 SECTION.
           INITIALIZE REG-AUXILIAR2
           MOVE AUX-CHAVE          TO AUX-CHAVE2(1:4)
           START AUXILIAR2 KEY IS NOT LESS AUX-CHAVE2 INVALID KEY
                MOVE "10" TO FS-AUXILIAR2.

           PERFORM UNTIL FS-AUXILIAR2 = "10"
                READ AUXILIAR2 NEXT AT END
                     MOVE "10" TO FS-AUXILIAR2
                NOT AT END
                     IF AUX-CHAVE <> AUX-CHAVE2(1:4)
                        MOVE "10" TO FS-AUXILIAR2
                     ELSE
                        MOVE AUX-CHAVE2   TO ALBUM-MTG
                        READ MTD020
      *                      INVALID KEY
      *                      MOVE "MTD020 Não Encontrado" TO MENSAGEM
      *                      MOVE "C"                     TO TIPO-MSG
      *                      PERFORM EXIBIR-MENSAGEM
                        NOT INVALID KEY
                             MOVE ZEROS           TO FOTFOGO-WK
                             IF QT-FOTOS-MTG NOT NUMERIC
                                MOVE ZEROS        TO QT-FOTOS-MTG
                             END-IF
                             IF QT-ENCADER-MTG NOT NUMERIC
                                MOVE ZEROS        TO QT-ENCADER-MTG
                             END-IF
                             IF FOGO-MTG NOT NUMERIC
                                MOVE 1 TO FOGO-MTG
                             END-IF
                             IF FOGO-MTG = 9
                                ADD QT-ENCADER-MTG TO ALBPERD-WK
                                ADD QT-FOTOS-MTG   TO FOTFOGO-WK
                             END-IF
                             ADD QT-FITAS-MTG     TO QT-FITAS-WK-ATUAL
                             ADD QT-FITAS-MTG     TO QT-FITAS-WK-INI
                             ADD QT-DVD-MTG       TO QT-DVD-WK-ATUAL
                             ADD QT-DVD-MTG       TO QT-DVD-WK-INI
                             ADD QT-POSTER-MTG    TO QT-POSTER-WK-ATUAL
                             ADD QT-POSTER-MTG    TO QT-POSTER-WK-INI
      *                      MOVE 1               TO EXECUTADO-WK
                        END-READ
                     END-IF
                END-READ
           END-PERFORM.

       LEITURA-AUX2-RECIBO SECTION.
           MOVE 2                  TO EXECUTADO-WK
           INITIALIZE REG-AUXILIAR2
           MOVE AUX-CHAVE          TO AUX-CHAVE2(1:4)
           START AUXILIAR2 KEY IS NOT LESS AUX-CHAVE2 INVALID KEY
                MOVE "10" TO FS-AUXILIAR2.

           PERFORM UNTIL FS-AUXILIAR2 = "10"
                READ AUXILIAR2 NEXT AT END
                     MOVE "10" TO FS-AUXILIAR2
                NOT AT END
                     IF AUX-CHAVE <> AUX-CHAVE2(1:4)
                        MOVE "10" TO FS-AUXILIAR2
                     ELSE
                        MOVE AUX-CHAVE2   TO ALBUM-REC
                        READ RCD100 INVALID KEY
                             MOVE "RCD100 Não Encontrado" TO MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                        NOT INVALID KEY
                             MOVE ALBUM-REC       TO ALBUM-MTG
                             MOVE 1               TO EXECUTADO-WK
                             READ MTD020 INVALID KEY
                                  PERFORM CONT-LEIT-RECIBO
                             NOT INVALID KEY
                                  IF QT-FOTOS-MTG NOT NUMERIC
                                     MOVE ZEROS TO QT-FOTOS-MTG
                                  END-IF

                                  COMPUTE FOTDEVO-W = QT-FOTOS-MTG -
                                         (QFOTOS-REC + QABERTURA-REC
                                        + QCOMISSAO-REC + QAVULSAS-REC)

                                  ADD FOTDEVO-W TO FOTDEVO-WK
                                  PERFORM CONT-LEIT-RECIBO
                             END-READ
                        END-READ
                     END-IF
                END-READ
           END-PERFORM.

       LEITURA-MTD020 SECTION.
      *    verifica a qtde album e fotos no fogo
           MOVE NR-CONTRATO-CO40  TO ALBUM-MTG(1: 4)
           MOVE ZEROS             TO ALBUM-MTG(5: 4)
           MOVE ZEROS TO FOTFOGO-WK
           START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                 MOVE "10" TO ST-MTD020.
           PERFORM UNTIL ST-MTD020 = "10"
             READ MTD020 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD020
              NOT AT END
               IF QT-FOTOS-MTG NOT NUMERIC
                  MOVE ZEROS TO QT-FOTOS-MTG
               END-IF
               IF QT-ENCADER-MTG NOT NUMERIC
                  MOVE ZEROS TO QT-ENCADER-MTG
               END-IF
               MOVE ALBUM-MTG(1: 4) TO CONTRATO-W
               IF FOGO-MTG NOT NUMERIC
                  MOVE 1 TO FOGO-MTG
               END-IF
               IF CONTRATO-W NOT = NR-CONTRATO-CO40
                 MOVE "10" TO ST-MTD020
               ELSE
                  IF FOGO-MTG = 9
                     ADD QT-ENCADER-MTG TO ALBPERD-WK
                     ADD QT-FOTOS-MTG   TO FOTFOGO-WK
                  END-IF
               END-IF
               ADD QT-FITAS-MTG         TO QT-FITAS-WK-ATUAL
               ADD QT-FITAS-MTG         TO QT-FITAS-WK-INI
               ADD QT-DVD-MTG           TO QT-DVD-WK-ATUAL
               ADD QT-DVD-MTG           TO QT-DVD-WK-INI
               ADD QT-POSTER-MTG        TO QT-POSTER-WK-ATUAL
               ADD QT-POSTER-MTG        TO QT-POSTER-WK-INI
      *        MOVE 1                   TO EXECUTADO-WK
             END-READ
           END-PERFORM.

       LEITURA-RECIBO SECTION.
           MOVE 2                TO EXECUTADO-WK.
           MOVE NR-CONTRATO-CO40 TO ALBUM-REC(1: 4)
           MOVE ZEROS            TO ALBUM-REC(5: 4)
           START RCD100 KEY IS NOT < ALBUM-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
                 READ RCD100 NEXT RECORD AT END
                      MOVE "10" TO ST-RCD100
                 NOT AT END
                      MOVE ALBUM-REC(1: 4) TO CONTRATO-W
                      IF CONTRATO-W NOT = NR-CONTRATO-CO40
                         MOVE "10" TO ST-RCD100
                      ELSE
                         MOVE ALBUM-REC       TO ALBUM-MTG
                         MOVE 1 TO EXECUTADO-WK
                         READ MTD020 INVALID KEY
                              PERFORM CONT-LEIT-RECIBO
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
           IF ALB-W NOT = ZEROS
              ADD 1             TO FORMANDOS-WK.

           ADD QCOMISSAO-REC    TO FOTCOMIS-WK
      *    ADD QAVULSAS-REC     TO VENDAVUL-WK
           ADD QAVULSAS-REC     TO FOTAVUL-WK
           ADD QAVULSAS-REC     TO FOTVEND-WK
           ADD QFOTOS-REC       TO FOTVEND-WK
           ADD QABERTURA-REC    TO FOTVEND-WK
           ADD QENCADER-REC     TO ALBVEND-WK
           ADD QFITAS-REC       TO FITA-WK
           ADD QDVD-REC         TO DVD-WK
           ADD QPOSTER-REC      TO POSTER-WK
           ADD TOTAL-REC        TO FATVENBRU-WK
           ADD TOTAL-DEF-REC    TO VENLIQDEF-WK
                                   VENLIQIND-WK
           COMPUTE VALOR-PM = (TOTAL-REC * PM-REC)
           ADD VALOR-PM         TO PRAZO-WK
           IF DATA-WK = ZEROS
              MOVE DATAVEN-REC  TO DATA-WK.

      * ---------ROTINAS PERFORMADAS(PELA SEÇÃO MOVER-DADOS-WORK)---
       CALCULAR-DADOS-BRINDE SECTION.
           MOVE ZEROS TO CUSTO-TOTAL
                         CUSTO-PREVISTO
                         CUSTO-W
                         JUROS-W
                         CONT
                         TAXA-ACUMULADA

           MOVE NR-CONTRATO-CO40   TO NR-CONTRATO-CO50
           MOVE ZEROS              TO ITEM-CO50
           START COD050 KEY IS NOT < CHAVE-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.

           PERFORM UNTIL ST-COD050 = "10"
              READ COD050 NEXT RECORD AT END
                   MOVE "10" TO ST-COD050
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
                               MOVE VALOR-CO02      TO CUSTO-PREVISTO
                            END-IF
      * 0 NÃO - 1 SIM
                            IF REALIZADO-CO51 <> 1
      * 1 SIM - 2 NÃO
                               IF MULT-FORM-CO02 = 2
                                  COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                                    QTDE-POR-FORM-CO51
                               ELSE
                                  COMPUTE CUSTO-W = (QTDE-POR-FORM-CO51
                                                   * QTDE-FORM-CO51) *
                                                     CUSTO-PREVISTO
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
                           ADD CUSTO-W   TO PATROCINIO-N-PAGO-WK
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
           PERFORM VERIFICA-PRECO.

           COMPUTE OBJ-VENDA-WK-ATUAL = QT-FOTOS-WK-ATUAL * 0,7 *
                                                                PRECO-W.
           COMPUTE OBJ-VENDA-WK-INI = QT-FOTOS-WK-INI * 0,7 * PRECO-W.

       CALCULAR-REPORTAGEM SECTION.

           INITIALIZE REG-RED101
           MOVE NR-CONTRATO-CO40       TO CONTRATO-R101
           START RED101 KEY IS NOT LESS CONTRATO-R101 INVALID KEY
                MOVE ZEROS TO DOCTO-R101
           NOT INVALID KEY
                READ RED101 NEXT AT END
                     MOVE ZEROS TO DOCTO-R101
                NOT AT END
                     MOVE DOCTO-R101   TO DOCTO-R100
                     READ RED100 NOT INVALID KEY
                          MOVE ZEROS               TO TOT-PARTICIPANTE
                          MOVE DOCTO-R100          TO DOCTO-R101
                          MOVE ZEROS               TO CONTRATO-R101
                                                      EVENTO-R101
                          START RED101 KEY IS NOT < CHAVE-R101
                                                             INVALID KEY
                                MOVE "10" TO ST-RED101
                          END-START
                          PERFORM UNTIL ST-RED101 = "10"
                                READ RED101 NEXT RECORD AT END
                                     MOVE "10" TO ST-RED101
                                NOT AT END
                                   IF DOCTO-R101 <> DOCTO-R100
                                        MOVE "10" TO ST-RED101
                                   ELSE
                                     ADD QT-PARTIC-R101
                                                     TO TOT-PARTICIPANTE
                                   END-IF
                                END-READ
                          END-PERFORM

                          MOVE DATA-PREV-VENDA-CO40 TO DATA-INV
                          CALL "GRIDAT2" USING DATA-INV
                          MOVE DATA-INV             TO GRTIME-DATE-FINAL
                          MOVE DATA-MOV-R100        TO GRTIME-DATE
                          IF GRTIME-DATE > GRTIME-DATE-FINAL
                             MOVE GRTIME-DATE-FINAL TO GRTIME-DATE
                          END-IF

                          INITIALIZE REG-RED101
                                     REPORTAGEM
                          MOVE NR-CONTRATO-CO40 TO CONTRATO-R101
                          START RED101 KEY IS NOT LESS CONTRATO-R101
                                                             INVALID KEY
                               MOVE "10" TO ST-RED101
                          END-START
                          PERFORM UNTIL ST-RED101 = "10"
                               READ RED101 NEXT AT END
                                    MOVE "10" TO ST-RED101
                               NOT AT END
                                    IF NR-CONTRATO-CO40 <>
                                       CONTRATO-R101
                                       MOVE "10" TO ST-RED101
                                    ELSE
                                       COMPUTE REPORTAGEM ROUNDED =
                                        ((VLR-TOT-REPORT-R100 +
                                          VLR-COMB-R100 +
                                          VLR-HOSP-R100 +
                                          VLR-REFEICAO-R100 +
                                          VLR-PASSAGEM-R100 +
                                          VLR-MAT-R100 +
                                          VLR-DESPESA-REPORT-R100 +
                                          VLR-ALUGUEL-R100 +
                                          VLR-OUTROS-R100) *
                                          QT-PARTIC-R101) /
                                          TOT-PARTICIPANTE
                                       ADD REPORTAGEM TO
                                                     PATROCINIO-WK-ATUAL
                                    END-IF
                               END-READ
                          END-PERFORM

                          MOVE 2 TO GRTIME-TYPE
                          MOVE 3 TO GRTIME-FUNCTION
                          CALL "GRTIME" USING PARAMETROS-GRTIME
                          COMPUTE PM-WK = GRTIME-DAYS-FINAL / 30
                          MOVE PM-WK       TO PRAZO-MEDIO
                          MOVE 1           TO TAXA-ACUMULADA
                          PERFORM VARYING CONT FROM 1 BY 1 UNTIL
                                     CONT > PRAZO-MEDIO
                              COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                                       ((GS-TAXA / 100) + 1)
                          END-PERFORM

      *          COMPUTE JUROS-WK = (VLR-REPORT-WK + VLR-DESPESA-WK) *
      *                             (TAXA-ACUMULADA - 1)
      *          COMPUTE CUSTO-CORRIG-WK = VLR-REPORT-WK +
      *                 VLR-DESPESA-WK + JUROS-WK

                     END-READ
                END-READ
           END-START.

       CALCULAR-COMISSAO SECTION.

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

           MOVE "CONT IDENTIFIC. CIDADE    REGIÃO    REPRESEN MÊS/ANO FO
      -    "RM P FOTOS FITA    PREV.VENDA    PATROCINIO   PAT/FORM DATA
      -    "REAL" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "---FORMANDO--- -----------FOTOGRAFIAS-------------- --V
      -    "END -RESULT.FOTOS %-----------
      -    "  --OUTROS--" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "PROD VEND SALD DISP. COMIS  VENDA DEVOL FOGO  SALDO VEN
      -    "DA DEVOL COMIS FOGO  SALDO   P.M. VDA.LIQ.IN P/FOTO    V.MED
      -    ".  DVD POSTER"   TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "--------------" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
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
                    IF GS-EXECUTADO-TRUE AND EXECUTADO-WK = 1 OR
                       GS-AEXECUTAR-TRUE AND EXECUTADO-WK = 2
                       PERFORM MOVER-DADOS-LINDET
                    ELSE
                       CONTINUE
                    END-IF
               END-READ
           END-PERFORM.
           PERFORM TOTALIZA-TELA.

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
                                          GS-FOTO-VEND
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

              COMPUTE GS-PATROCINIO-MEDIO ROUNDED =
                     (GS-PATROCINIO-PAGO + GS-PATROCINIO-N-PAGO)  /
                                            GS-ALBUM-PRODUZ

              COMPUTE GS-PAT-MED-CLIENTE-INI   = GS-PATROCINIO-INI /
                                                 GS-ALBUM-CLIENTE-INI

              COMPUTE GS-PAT-MED-CLIENTE-ATUAL = GS-PATROCINIO-ATUAL /
                                                 GS-ALBUM-CLIENTE-ATUAL

              COMPUTE GS-PAT-FORM-AEXECUTAR ROUNDED=
                           GS-PATROCINIO-AEXECUTAR / GS-ALBUM-AEXECUTAR.
       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "MES/ANO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT4-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CIDADE" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "REGIÃO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT2-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "REPRESENT" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT3-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "ABC" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT5-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.

       MOVER-DADOS-LINDET SECTION.
           MOVE CONTRATO-WK             TO GS-LINDET(1: 5)
           MOVE IDENTIFICACAO-WK        TO GS-LINDET(6: 11)
           MOVE CIDADE-WK               TO GS-LINDET(17: 10)
           MOVE REGIAO-WK               TO GS-LINDET(27: 10)
           MOVE REPRESENT-WK            TO GS-LINDET(37: 9)
           MOVE MESANO-WK               TO MESANO-I
           MOVE MESANO-I(5: 2)          TO MESANO-W(1: 2)
           MOVE MESANO-I(1: 4)          TO MESANO-W(3: 4)
           MOVE MESANO-W                TO MESANO-E
           MOVE MESANO-E                TO GS-LINDET(46: 8)
           MOVE QT-FORM-WK-ATUAL        TO GS-LINDET(54: 5)
           MOVE PADRAO-WK               TO GS-LINDET(59: 2)
           MOVE QT-FOTOS-WK-ATUAL       TO GS-LINDET(61: 6)
           MOVE QT-FITAS-WK-ATUAL       TO GS-LINDET(67: 5)
           MOVE OBJ-VENDA-WK-ATUAL      TO VALOR-E
           MOVE VALOR-E                 TO GS-LINDET(75: 11)
           MOVE PATROCINIO-WK-ATUAL     TO VALOR-E
           MOVE VALOR-E                 TO GS-LINDET(89: 11)
           MOVE PAT-FORM-WK-ATUAL       TO VALOR-E1
           MOVE VALOR-E1                TO GS-LINDET(100: 10)
      *Data Realização
           MOVE DATA-WK                 TO DATA-I
           MOVE MES-I                   TO GS-LINDET(111: 2)
           MOVE "/"                     TO GS-LINDET(113: 1)
           MOVE ANO-I(3: 2)             TO GS-LINDET(114: 3)


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

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    IF EXECUTADO-WK = 2
      *       CONTINUE
      *    ELSE
              PERFORM MOVER-DADOS-LINDET1.

       MOVER-DADOS-LINDET1 SECTION.
      *    2o. LINHA DA LINDET
           MOVE SPACES              TO GS-LINDET
           MOVE ALBPROD-WK          TO GS-LINDET(1: 5)
           MOVE FORMANDOS-WK        TO GS-LINDET(6: 5)
           COMPUTE SALALB-W = ALBPROD-WK - FORMANDOS-WK
           MOVE SALALB-W            TO GS-LINDET(11: 5)

           COMPUTE DISPONIVEL-W = FOTPROD-WK - FOTPERD-WK
      *    -
      *                            FOTCOMIS-WK
      *                           + FOTFOGO-WK).





           MOVE DISPONIVEL-W        TO GS-LINDET(16: 6)
           MOVE FOTCOMIS-WK         TO GS-LINDET(23: 5)
           MOVE FOTVEND-WK          TO GS-LINDET(28: 6)
           MOVE FOTDEVO-WK          TO GS-LINDET(35: 6)
           MOVE FOTFOGO-WK          TO GS-LINDET(41: 5)


           COMPUTE SALFOT-W = FOTPROD-WK - (FOTVEND-WK + FOTPERD-WK +
                              FOTDEVO-WK + FOTCOMIS-WK +
      *                       VENDAVUL-WK
      *                       +
                              FOTFOGO-WK).

           MOVE SALFOT-W            TO GS-LINDET(46: 6)

      * % Venda
           COMPUTE PERC-W = (FOTVEND-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(53: 6)

      * % Devolução
           COMPUTE PERC-W = (FOTDEVO-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(59: 6)

      * % Comissão
           COMPUTE PERC-W = (FOTCOMIS-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(65: 6)

      * % Fogo
           COMPUTE PERC-W = (FOTFOGO-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(70: 6)

      * % Saldo Foto
           COMPUTE PERC-W = (SALFOT-W / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(77: 6)

      * % Prazo Médio
           IF PRAZO-WK = 0 OR FATVENBRU-WK = 0
              MOVE 0                    TO PRAZO-W
           ELSE
              COMPUTE PRAZO-W = PRAZO-WK / FATVENBRU-WK
           END-IF
           MOVE PRAZO-W             TO PRAZO-E
           MOVE PRAZO-E             TO GS-LINDET(83: 7)
      *    dasdasdasd

      * % Venda Liquida
           MOVE VENLIQIND-WK        TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET(90: 11)

      * Preço Médio
           IF VENLIQIND-WK = 0 OR FOTVEND-WK = 0
              MOVE 0                           TO PRECOMED-W
           ELSE
              COMPUTE PRECOMED-W = VENLIQIND-WK / FOTVEND-WK
           END-IF
           MOVE PRECOMED-W          TO VALOR-E3
           MOVE VALOR-E3            TO GS-LINDET(102: 5)
      *    dasdasdas

      * Preço Médio por Cliente
           IF VENLIQIND-WK = 0 OR FORMANDOS-WK = 0
              MOVE 0                            TO VENMEDCLI-W
           ELSE
              COMPUTE VENMEDCLI-W = VENLIQIND-WK / FORMANDOS-WK
           END-IF
           MOVE VENMEDCLI-W         TO VALOR-E4
           MOVE VALOR-E4            TO GS-LINDET(109: 09)
      *    dasdasdas

      * DVD
           MOVE DVD-WK              TO GS-LINDET(118: 5)

      * Poster
           MOVE POSTER-WK           TO GS-LINDET(125: 5)


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
           MOVE "COP104" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE 0 TO QUANTIDADE
           PERFORM UNTIL QUANTIDADE = GS-COPIAS
               ADD 1 TO QUANTIDADE
               PERFORM IMPRIMIR
           END-PERFORM.

       IMPRIMIR.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL

           IF GS-IMP-ANALITICO = 1
              PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                   IF GS-EXECUTADO-TRUE AND EXECUTADO-WK = 1 OR
                      GS-AEXECUTAR-TRUE AND EXECUTADO-WK = 2
                      PERFORM MOVER-DADOS-RELAT
                   ELSE
                      CONTINUE
                   END-IF
                 END-READ
              END-PERFORM
           END-IF

           IF GS-IMP-RESUMO = 1
              PERFORM TOTALIZA-RELAT
           END-IF

           MOVE SPACES TO REG-RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
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
           IF EXECUTADO-WK = 2
              ADD 1 TO LIN
           ELSE
              ADD 3 TO LIN.

           IF LIN > 56
              PERFORM CABECALHO.

           MOVE CONTRATO-WK         TO LINDET-REL(1: 5)
           MOVE IDENTIFICACAO-WK    TO LINDET-REL(6: 11)
           MOVE CIDADE-WK           TO LINDET-REL(17: 10)
           MOVE REGIAO-WK           TO LINDET-REL(27: 10)
           MOVE REPRESENT-WK        TO LINDET-REL(37: 9)
           MOVE MESANO-WK           TO MESANO-I
           MOVE MESANO-I(5: 2)      TO MESANO-W(1: 2)
           MOVE MESANO-I(1: 4)      TO MESANO-W(3: 4)
           MOVE MESANO-W            TO MESANO-E
           MOVE MESANO-E            TO LINDET-REL(46: 8)
           MOVE QT-FORM-WK-ATUAL    TO LINDET-REL(54: 5)
           MOVE PADRAO-WK           TO LINDET-REL(59: 2)
           MOVE QT-FOTOS-WK-ATUAL   TO LINDET-REL(61: 6)
           MOVE QT-FITAS-WK-ATUAL   TO LINDET-REL(67: 5)
           MOVE OBJ-VENDA-WK-ATUAL  TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(75: 11)
           MOVE PATROCINIO-WK-ATUAL TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(89: 11)
           MOVE PAT-FORM-WK-ATUAL   TO VALOR-E1
           MOVE VALOR-E1            TO LINDET-REL(100: 10)
      *Data Realização
           MOVE DATA-WK             TO DATA-I
           MOVE MES-I               TO LINDET-REL(111: 2)
           MOVE "/"                 TO LINDET-REL(113: 1)
           MOVE ANO-I(3: 2)         TO LINDET-REL(114: 3)
           WRITE REG-RELAT FROM LINDET.

      *    IF EXECUTADO-WK = 2
      *       CONTINUE
      *    ELSE
              PERFORM MOVER-DADOS-RELAT1.

       MOVER-DADOS-RELAT1 SECTION.
      *    2o. LINHA DA LINDET
           MOVE SPACES              TO LINDET-REL

           MOVE ALBPROD-WK          TO LINDET-REL(1: 5)
           MOVE FORMANDOS-WK        TO LINDET-REL(6: 5)

           COMPUTE SALALB-W = ALBPROD-WK - FORMANDOS-WK
           MOVE SALALB-W            TO LINDET-REL(11: 5)

           COMPUTE DISPONIVEL-W = FOTPROD-WK - FOTPERD-WK



           MOVE DISPONIVEL-W        TO LINDET-REL(16: 6)
           MOVE FOTCOMIS-WK         TO LINDET-REL(23: 5)
           MOVE FOTVEND-WK          TO LINDET-REL(28: 6)
           MOVE FOTDEVO-WK          TO LINDET-REL(35: 6)
           MOVE FOTFOGO-WK          TO LINDET-REL(41: 5)


           COMPUTE SALFOT-W = FOTPROD-WK - (FOTVEND-WK + FOTPERD-WK +
                              FOTDEVO-WK + FOTCOMIS-WK +
                              FOTFOGO-WK).

           MOVE SALFOT-W            TO LINDET-REL(46: 6)

      * % Venda
           COMPUTE PERC-W = (FOTVEND-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(53: 6)

      * % Devolução
           COMPUTE PERC-W = (FOTDEVO-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(59: 6)

      * % Comissão
           COMPUTE PERC-W = (FOTCOMIS-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(65: 6)

      * % Fogo
           COMPUTE PERC-W = (FOTFOGO-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(70: 6)

      * % Saldo Foto
           COMPUTE PERC-W = (SALFOT-W / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(77: 6)

      * Prazo Médio
           IF PRAZO-WK = 0 OR FATVENBRU-WK = 0
              MOVE 0                TO PRAZO-W
           ELSE
              COMPUTE PRAZO-W = PRAZO-WK / FATVENBRU-WK
           END-IF
           MOVE PRAZO-W             TO PRAZO-E
           MOVE PRAZO-E             TO LINDET-REL(83: 7)

      * Venda Líquida
           MOVE VENLIQIND-WK        TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(90: 11)

      * Preço Médio
           IF VENLIQIND-WK = 0 OR FOTVEND-WK = 0
              MOVE 0                TO PRECOMED-W
           ELSE
              COMPUTE PRECOMED-W = VENLIQIND-WK / FOTVEND-WK
           END-IF
           MOVE PRECOMED-W          TO VALOR-E3
           MOVE VALOR-E3            TO LINDET-REL(102: 5)

      * Valor Médio Cliente
           IF VENLIQIND-WK = 0 OR FORMANDOS-WK = 0
              MOVE 0                TO VENMEDCLI-W
           ELSE
              COMPUTE VENMEDCLI-W = VENLIQIND-WK / FORMANDOS-WK
           END-IF
           MOVE VENMEDCLI-W         TO VALOR-E4
           MOVE VALOR-E4            TO LINDET-REL(109: 09)

      * DVD
           MOVE DVD-WK              TO LINDET-REL(118: 5)

      * Poster
           MOVE POSTER-WK           TO LINDET-REL(125: 5)

           WRITE REG-RELAT FROM LINDET.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.


       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.

           IF GS-IMP-ANALITICO = 1
              WRITE REG-RELAT FROM CAB04
              WRITE REG-RELAT FROM CAB05 AFTER 2
              WRITE REG-RELAT FROM CAB06
              WRITE REG-RELAT FROM CAB03
              MOVE 9 TO LIN
           ELSE
              MOVE 4 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 COD050 CAD010 CAD012 CGD001 COD051 RCD101
                 COD005 COD002 MTD001 MTD020 RCD100 COD001 COD104
                 RED100 RED101.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
