       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP109.
       DATE-WRITTEN. 13/09/2000.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: Relatório 05 - ORDEM REGIAO
      * LISTAR APENAS OS CONTRATOS ATIVOS, OU SEJA, STATUS => 50
      * - CONTRATOS A EXECUTAR SÃO OS CONTRATOS QUE NÃO FORAM VENDIDOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CGPX001.
           COPY COPX002.
           COPY COPX005.
           COPY COPX040.
           COPY COPX050.
           COPY CAPX010.
           COPY CAPX012.
           COPY MTPX001.
           COPY MTPX020.
           COPY RCPX100.

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
                     VENLIQIND-WK OBJ-VENDA-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CGPW001.
       COPY CAPW010.
       COPY CAPW012.
       COPY COPW002.
       COPY COPW005.
       COPY COPW040.
       COPY COPW050.
       COPY MTPW001.
       COPY MTPW020.
       COPY RCPW100.

       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  IDENTIFICACAO-WK    PIC X(10).
           05  CIDADE-WK           PIC X(9).
           05  REGIAO-WK           PIC X(9).
           05  REPRESENT-WK        PIC X(8).
           05  QT-FORM-WK          PIC 9(4).
           05  PADRAO-WK           PIC X.
           05  QT-FOTOS-WK         PIC 9(5).
      *    QT-FOTOS-WK = qt-form-wk * prev-fotos-co05(cadastro padrao)
           05  QT-FITAS-WK         PIC 9(4).
           05  MESANO-WK           PIC 9(6).
           05  OBJ-VENDA-WK        PIC 9(8)V99.
      *    OBJ-VENDA-WK = qt-fotos-wk * 0,7 * preco-wk.
           05  PATROCINIO-WK       PIC 9(7)V99.
      *    PATROCINIO-WK = vlr-pago-wk + vlr-pagar-wk
           05  PAT-FORM-WK         PIC 9(6)V99.
      *    PAT-FORM-W = patrocinio-wk / qt-form-wk
           05  PM-WK               PIC 9(3)V99.

           05  DATA-WK              PIC 9(8).
           05  ALBPROD-WK           PIC 9999.
           05  ALBVEND-WK           PIC 9999.
           05  FOTPROD-WK           PIC 9(05).
           05  FOTVEND-WK           PIC 9(05).
           05  FOTAVUL-WK           PIC 9(05).
           05  FOTMONT-WK           PIC 9(05).
           05  FOTFOGO-WK           PIC 9(05).
           05  FOTDEVO-WK           PIC S9(5).
           05  FITA-WK              PIC 9(04).
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
      *    EXECUTADO = 1(SIM)    2(NAO)

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP109.CPB".
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
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-MTD001             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02).
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
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

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".
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
           "CONT IDENTIFIC. CIDADE    REGIÃO    REPRESEN MÊS/ANO FORM P
      -    "FOTOS FITA    PREV.VENDA    PATROCINIO   PAT.FORM".
       01  CAB05.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(120)   VALUE
           "DATA ---FORMANDO---- ---------FOTOGRAFIAS--------- VEND -RES
      -    "ULT.FOTOS %--".
       01  CAB06.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(120)   VALUE
           "REAL. PROD VEND SALD DISP. COMIS VENDA DEVOL SALDO FITA VEND
      -    "A DEVOL SALDO   P.M. VDA.LIQ.INDEX P/FOTO V.MED.CLI".
       01  LINDET.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  LINDET-REL          PIC X(120)  VALUE SPACES.
       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(055) VALUE
              "_____________PREVISAO_______________             ______".
           05 FILLER                         PIC  X(036) VALUE
              "________FOTOGRAFIA__________________".
       02  LINHA-02.
           05 FILLER                         PIC  X(055) VALUE
              "|                                  |            |      ".
           05 FILLER                         PIC  X(036) VALUE
              "                                   |".
       02  LINHA-03.
           05 FILLER                         PIC  X(022) VALUE
              "|   ALBUM CLIENTE:    ".
           05 ALBUM-CLIENTE-REL              PIC  ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(032) VALUE
              "   |            |   PRODUZIDAS: ".
           05 FOTO-PRODUZ-REL                PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-FOTO-PRODUZ-REL           PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(007) VALUE
              "      |".
       02  LINHA-04.
           05 FILLER                         PIC  X(007) VALUE
              "|   PRO".
           05 FILLER                         PIC  X(001) VALUE "D".
           05 FILLER                         PIC  X(006) VALUE
              ".FOTOG".
           05 FILLER                         PIC  X(001) VALUE "R".
           05 FILLER                         PIC  X(007) VALUE
              "..:    ".
           05 PROD-FOTOG-REL                 PIC  ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(022) VALUE
              "   |            |   PE".
           05 FILLER                         PIC  X(002) VALUE "RD".
           05 FILLER                         PIC  X(008) VALUE
              "IDAS..: ".
           05 FOTO-PERD-REL                  PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-FOTO-PERD-REL             PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(007) VALUE
              "      |".
       02  LINHA-05.
           05 FILLER                         PIC  X(007) VALUE
              "|   PRO".
           05 FILLER                         PIC  X(001) VALUE "D".
           05 FILLER                         PIC  X(014) VALUE
              ".FITA....:    ".
           05 PROD-FITA-REL                  PIC  ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(032) VALUE
              "   |            |   AVULSAS...: ".
           05 FOTO-AVULSA-REL                PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-FOTO-AVULSA-REL           PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(007) VALUE
              "      |".
       02  LINHA-06.
           05 FILLER                         PIC  X(019) VALUE
              "|   VENDA LIQUIDA: ".
           05 VENDA-LIQUIDA-REL              PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(032) VALUE
              "   |            |   COMISSAO..: ".
           05 FOTO-COMISSAO-REL              PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-FOTO-COMISSAO-REL         PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(007) VALUE
              "      |".
       02  LINHA-07.
           05 FILLER                         PIC  X(019) VALUE
              "|   PATROCINIO...: ".
           05 PATROCINIO-REL                 PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(032) VALUE
              "   |            |   DISPONIVEL: ".
           05 FOTO-DISPON-REL                PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-FOTO-DISPON-REL           PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(007) VALUE
              "      |".
       02  LINHA-08.
           05 FILLER                         PIC  X(010) VALUE
              "|   PAT.ME".
           05 FILLER                         PIC  X(003) VALUE "D/C".
           05 FILLER                         PIC  X(006) VALUE
              "LIEN: ".
           05 PAT-MED-CLIENTE-REL            PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(032) VALUE
              "   |            |   VENDIDA...: ".
           05 FOTO-VEND-REL                  PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-FOTO-VEND-REL             PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(007) VALUE
              "      |".
       02  LINHA-09.
           05 FILLER                         PIC  X(055) VALUE
              "|                                  |            |   DEV".
           05 FILLER                         PIC  X(009) VALUE
              "OLVIDA.: ".
           05 FOTO-DEVOL-REL                 PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-FOTO-DEVOL-REL            PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(007) VALUE
              "      |".
       02  LINHA-10.
           05 FILLER                         PIC  X(055) VALUE
              "|                                  |            |   FOG".
           05 FILLER                         PIC  X(009) VALUE
              "O......: ".
           05 FOTO-FOGO-REL                  PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-FOTO-FOGO-REL             PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(007) VALUE
              "      |".
       02  LINHA-11.
           05 FILLER                         PIC  X      VALUE "|".
           05 FILLER                         PIC  X(034) VALUE ALL "_".
           05 FILLER                         PIC  X      VALUE "|".
           05 FILLER                         PIC  X(012) VALUE SPACES.
           05 FILLER                         PIC  X(016) VALUE
              "|   SALDO.....: ".
           05 FOTO-SALDO-REL                 PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-FOTO-SALDO-REL            PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(007) VALUE
              "      |".

       02  LINHA-12.
           05 FILLER                         PIC  X(055) VALUE
              "                                                |      ".
           05 FILLER                         PIC  X(036) VALUE
              "                                   |".
       02  LINHA-13.
           05 FILLER                         PIC  X(049) VALUE
              "                                                |".
           05 FILLER                         PIC  X(041) VALUE ALL "_".
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-15.
           05 FILLER                         PIC  X(019) VALUE
              "|_____________ALBUM".
           05 FILLER                         PIC  X(002) VALUE "/C".
           05 FILLER                         PIC  X(055) VALUE
              "LIENTE______________|      |_______________FATURAMENTO ".
           05 FILLER                         PIC  X(015) VALUE ALL "_".
       02  LINHA-16.
           05 FILLER                         PIC  X(055) VALUE
              "|                                        |      |      ".
           05 FILLER                         PIC  X(036) VALUE
              "                                   |".
       02  LINHA-17.
           05 FILLER                         PIC  X(018) VALUE
              "|   PRODUZIDO...: ".
           05 ALBUM-PRODUZ-REL               PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-ALBUM-PRODUZ-REL          PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(036) VALUE
              "   |      |   VENDA BRUTA.........: ".
           05 VENDA-BRUTA-REL                PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(004) VALUE "   |".
       02  LINHA-18.
           05 FILLER                         PIC  X(018) VALUE
              "|   VENDIDO.....: ".
           05 ALBUM-VEND-REL                 PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-ALBUM-VEND-REL            PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(036) VALUE
              "   |      |   PRAZO-MEDIO.........: ".
           05 PRAZO-MEDIO-REL                PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(004) VALUE "   |".
       02  LINHA-19.
           05 FILLER                         PIC  X(018) VALUE
              "|   SALDO.......: ".
           05 ALBUM-SALDO-REL                PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-ALBUM-SALDO-REL           PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(036) VALUE
              "   |      |   VENDA LIQUIDA(INDEX): ".
           05 VENDA-LIQUIDA-FAT-REL          PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(004) VALUE "   |".
       02  LINHA-20.
           05 FILLER                         PIC  X(018) VALUE
              "|   FITA VENDIDA: ".
           05 FITA-VENDIDA-REL               PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PERC-FITA-VENDIDA-REL          PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(036) VALUE
              "   |      |   PRECO MEDIO FOTO....: ".
           05 PRECO-MEDIO-FOTO-REL           PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(004) VALUE "   |".
       02  LINHA-21.
           05 FILLER                         PIC  X(055) VALUE
              "|                                        |      |   PRE".
           05 FILLER                         PIC  X(019) VALUE
              "CO MEDIO CLIENTE.: ".
           05 PRECO-MEDIO-CLIENTE-REL        PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(004) VALUE "   |".
       02  LINHA-22.
           05 FILLER                         PIC  X(055) VALUE
              "|________________________________________|      |      ".
           05 FILLER                         PIC  X(036) VALUE
              "                                   |".
       02  LINHA-23.
           05 FILLER                         PIC  X(048) VALUE SPACES.
           05 FILLER                         PIC  X(043) VALUE
              "|_________________________________________|".
       02  LINHA-26.
           05 FILLER                         PIC  X(055) VALUE
              "_______________________________CONTRATOS NAO EXECUTADOS".
           05 FILLER                         PIC  X(036) VALUE
              "____________________________________".
       02  LINHA-27.
           05 FILLER                         PIC  X(055) VALUE
              "|                                                      ".
           05 FILLER                         PIC  X(036) VALUE
              "                                   |".
       02  LINHA-28.
           05 FILLER                         PIC  X(009) VALUE
              "|   ALBUM".
           05 FILLER                         PIC  X(002) VALUE "/C".
           05 FILLER                         PIC  X(014) VALUE
              "LIENTE......: ".
           05 ALBUM-AEXECUTAR-REL            PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(034) VALUE
              "                VENDA LIQUIDA...: ".
           05 VENDA-LIQ-AEXECUTAR-REL        PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              "       |".
       02  LINHA-29.
           05 FILLER                         PIC  X(025) VALUE
              "|   PRODUCAO FOTOGRAFIA: ".
           05 PROD-FOTOGRAFIA-REL            PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(034) VALUE
              "                PATROCINIO......: ".
           05 PATROCINIO-AEXECUTAR-REL       PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              "       |".
       02  LINHA-30.
           05 FILLER                         PIC  X(025) VALUE
              "|   PRODUCAO FITA......: ".
           05 PROD-FITAS-REL                 PIC  ZZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(034) VALUE
              "                PATROCINIO/FORM.: ".
           05 PAT-FORM-AEXECUTAR-REL         PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              "       |".
       02  LINHA-31.
           05 FILLER                         PIC  X(055) VALUE
              "|                                                      ".
           05 FILLER                         PIC  X(036) VALUE
              "                                   |".
       02  LINHA-32.
           05 FILLER                         PIC  X(055) VALUE
              "|______________________________________________________".
           05 FILLER                         PIC  X(036) VALUE
              "___________________________________|".

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
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE NOME-EMP           TO EMPRESA-REL
           MOVE EMPRESA            TO EMP-REC
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "MTD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD001.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           OPEN INPUT CAD010 CGD001 COD040 COD005 COD050
                      COD002 CAD012 MTD001 MTD020 RCD100.
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
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-PRINTER-FLG-TRUE
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-REGIAO-TRUE
                    PERFORM LER-REGIAO
               WHEN GS-POPUP-REGIAO-TRUE
                    PERFORM POPUP-REGIAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LER-REGIAO SECTION.
           MOVE GS-REGIAO       TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG.
           MOVE NOME-REG        TO GS-NOME-REGIAO.
       POPUP-REGIAO SECTION.
           CALL "CAP012T" USING PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2)  TO GS-REGIAO.
           MOVE PASSAR-STRING-1(1: 30)  TO GS-NOME-REGIAO.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
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
           MOVE CIDADE-CO40        TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
                                   MOVE ZEROS TO REGIAO-CID.
           MOVE REGIAO-CID         TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG.
           IF STATUS-CO40 < 50 OR REGIAO-CID <> GS-REGIAO
              CONTINUE
           ELSE
              MOVE MESANO-PREV-CO40   TO MESANO-WK
              MOVE NR-CONTRATO-CO40   TO CONTRATO-WK
              MOVE NOME-CID           TO CIDADE-WK
              MOVE NOME-REG           TO REGIAO-WK
              MOVE REPRESENTANTE-CO40 TO CODIGO-CG01
              READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
              END-READ
              MOVE NOME-CG01          TO REPRESENT-WK
              ADD VLR-COMISSAO-CO40   TO PATROCINIO-WK

              MOVE DATA-PREV-VENDA-CO40 TO DATA-INV
              CALL "GRIDAT2" USING DATA-INV
              MOVE DATA-INV TO GRDIAS-AAMMDD-FINAL
              MOVE ASSINATURA-CO40 TO GRDIAS-AAMMDD-INICIAL
              CALL "GRDIAS1" USING PARAMETROS-GRDIAS
              MOVE GRDIAS-NUM-DIAS TO CONT-W
              COMPUTE MESES-W = CONT-W / 30
              COMPUTE VLR-GERAL = MESES-W * VLR-COMISSAO-CO40

              MOVE IDENTIFICACAO-CO40 TO IDENTIFICACAO-WK

              MOVE QTDE-FORM-CO40     TO QT-FORM-WK
              MOVE PADRAO-CO40        TO PADRAO-CO05 PADRAO-WK
              READ COD005 INVALID KEY MOVE ZEROS TO PREV-FOTOS-CO05
              END-READ
              COMPUTE QT-FOTOS-WK = PREV-FOTOS-CO05 * QT-FORM-WK
              MOVE ZEROS TO QT-FITAS-WK

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
                      COMPUTE FOTDEVO-W = QT-FOTOS-MTG -
                               (QFOTOS-REC + QCOMISSAO-REC)
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
           ADD QAVULSAS-REC  TO VENDAVUL-WK.
           ADD QFOTOS-REC    TO FOTVEND-WK.
           ADD QENCADER-REC  TO ALBVEND-WK.
           ADD QFITAS-REC    TO FITA-WK.
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
                     MOVE CODBRINDE-CO50 TO CODIGO-CO02
                     READ COD002 INVALID KEY MOVE ZEROS TO VALOR-CO02
                     END-READ
                     IF CUSTO-UNIT-CO50 <> ZEROS
                        MOVE CUSTO-UNIT-CO50 TO CUSTO-PREVISTO
                     ELSE MOVE VALOR-CO02 TO CUSTO-PREVISTO
                     END-IF
                     IF REALIZADO-CO50 <> 1
                        IF MULT-FORM-CO02 = 2
                           COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                             QTDE-POR-FORM-CO50
                        ELSE COMPUTE CUSTO-W = (QTDE-POR-FORM-CO50 *
                            QTDE-FORM-CO50) * CUSTO-PREVISTO
                        END-IF
                     ELSE MOVE VALOR-PAGO-CO50 TO CUSTO-W
                     END-IF
                     ADD CUSTO-W TO PATROCINIO-WK
                     PERFORM CALCULO-JUROS-BRINDE
                     ADD JUROS-W TO PATROCINIO-WK
                     COMPUTE MESES-W = DIAS-PRAZO-CO50 / 30
                     COMPUTE VLR-GERAL = (CUSTO-W * MESES-W) + VLR-GERAL
                  END-IF
              END-READ
           END-PERFORM.
           COMPUTE PRAZO-MEDIO ROUNDED = VLR-GERAL / PATROCINIO-WK
           MOVE PRAZO-MEDIO TO PM-WK.
      *    MOVE 1 TO TAXA-ACUMULADA.
      *    COMPUTE TAXA-W = (GS-TAXA / 100) + 1.
      *    PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
      *        COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA * TAXA-W
      *    END-PERFORM.
           PERFORM VERIFICA-PRECO.
           COMPUTE OBJ-VENDA-WK = QT-FOTOS-WK * 0,7 * PRECO-W.
           COMPUTE PAT-FORM-WK = PATROCINIO-WK / QT-FORM-WK.
       VERIFICA-PRECO SECTION.
           IF MESANO-WK < 199506 MOVE 8 TO PRECO-W
           ELSE MOVE 11 TO PRECO-W.
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
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-EXECUTADO-TRUE AND EXECUTADO-WK = 1 OR
                   GS-AEXECUTAR-TRUE AND EXECUTADO-WK = 2
                      PERFORM MOVER-DADOS-LINDET
                ELSE CONTINUE
                END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-TELA.

       TOTALIZA-TELA SECTION.
           MOVE 100                  TO GS-PERC-FOTO-PRODUZ.
           COMPUTE DIVISAO-W = GS-FOTO-PERD / GS-FOTO-PRODUZ.
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-FOTO-PERD
           COMPUTE DIVISAO-W = GS-FOTO-AVULSA / GS-FOTO-PRODUZ
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-FOTO-AVULSA.
           COMPUTE DIVISAO-W = GS-FOTO-COMISSAO / GS-FOTO-PRODUZ
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-FOTO-COMISSAO
           COMPUTE DIVISAO-W = GS-FOTO-DISPON / GS-FOTO-PRODUZ
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-FOTO-DISPON
           MOVE 100                  TO GS-PERC-FOTO-DISPON1
           COMPUTE DIVISAO-W = GS-FOTO-VEND / GS-FOTO-DISPON
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-FOTO-VEND
           COMPUTE DIVISAO-W = GS-FOTO-AVULSA / GS-FOTO-DISPON
           COMPUTE PERC-W = DIVISAO-W * 100.
           COMPUTE DIVISAO-W = GS-FOTO-DEVOL / GS-FOTO-DISPON
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-FOTO-DEVOL
           COMPUTE DIVISAO-W = GS-FOTO-FOGO / GS-FOTO-DISPON
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-FOTO-FOGO.
           COMPUTE DIVISAO-W = GS-FOTO-SALDO / GS-FOTO-DISPON
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-FOTO-SALDO

           MOVE 100                  TO GS-PERC-ALBUM-PRODUZ
           COMPUTE DIVISAO-W = GS-ALBUM-VEND / GS-ALBUM-PRODUZ
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-ALBUM-VEND
           COMPUTE DIVISAO-W = GS-ALBUM-SALDO / GS-ALBUM-PRODUZ
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-ALBUM-SALDO
           COMPUTE DIVISAO-W = GS-FITA-VENDIDA / GS-ALBUM-PRODUZ
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W               TO GS-PERC-FITA-VENDIDA.

           COMPUTE GS-PRECO-MEDIO-FOTO = GS-VENDA-LIQUIDA-FAT /
                                         GS-FOTO-VEND
           COMPUTE GS-PRECO-MEDIO-CLIENTE = GS-VENDA-LIQUIDA-FAT /
                                         GS-ALBUM-VEND
           COMPUTE GS-PRAZO-MEDIO = PRAZO-GERAL / GS-VENDA-BRUTA.
           COMPUTE GS-PAT-FORM-AEXECUTAR =  GS-PATROCINIO-AEXECUTAR
                             / GS-ALBUM-AEXECUTAR.
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
           MOVE CONTRATO-WK       TO GS-LINDET(1: 5)
           MOVE IDENTIFICACAO-WK  TO GS-LINDET(6: 11)
           MOVE CIDADE-WK         TO GS-LINDET(17: 10)
           MOVE REGIAO-WK         TO GS-LINDET(27: 10)
           MOVE REPRESENT-WK      TO GS-LINDET(37: 9)
           MOVE MESANO-WK         TO MESANO-I
           MOVE MESANO-I(5: 2)    TO MESANO-W(1: 2)
           MOVE MESANO-I(1: 4)    TO MESANO-W(3: 4)
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO GS-LINDET(46: 8)
           MOVE QT-FORM-WK        TO GS-LINDET(54: 5)
           MOVE PADRAO-WK         TO GS-LINDET(59: 2)
           MOVE QT-FOTOS-WK       TO GS-LINDET(61: 6)
           MOVE QT-FITAS-WK       TO GS-LINDET(67: 5)
           MOVE OBJ-VENDA-WK      TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(72: 14)
           MOVE PATROCINIO-WK     TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(86: 14)
           MOVE PAT-FORM-WK       TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(100: 10)

      *    EXECUTADO-WK = 2(NAO)  1(SIM)
           IF EXECUTADO-WK = 2
               ADD QT-FORM-WK         TO GS-ALBUM-AEXECUTAR
               ADD QT-FOTOS-WK        TO GS-PROD-FOTOGRAFIA
               ADD QT-FITAS-WK        TO GS-PROD-FITAS
               ADD OBJ-VENDA-WK       TO GS-VENDA-LIQ-AEXECUTAR
               ADD PATROCINIO-WK      TO GS-PATROCINIO-AEXECUTAR
           ELSE
               ADD QT-FORM-WK         TO GS-ALBUM-CLIENTE
               ADD QT-FOTOS-WK        TO GS-PROD-FOTOG
               ADD QT-FITAS-WK        TO GS-PROD-FITA
               ADD OBJ-VENDA-WK       TO GS-VENDA-LIQUIDA
               ADD PATROCINIO-WK      TO GS-PATROCINIO.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           IF EXECUTADO-WK = 2 CONTINUE
           ELSE PERFORM MOVER-DADOS-LINDET1.

       MOVER-DADOS-LINDET1 SECTION.
      *    2o. LINHA DA LINDET
           MOVE SPACES              TO GS-LINDET
           MOVE DATA-WK             TO DATA-I
           MOVE MES-I               TO GS-LINDET(1: 2)
           MOVE "/"                 TO GS-LINDET(3: 1)
           MOVE ANO-I(3: 2)         TO GS-LINDET(4: 3)
           MOVE ALBPROD-WK          TO GS-LINDET(7: 5)
           MOVE FORMANDOS-WK        TO GS-LINDET(12: 5)
           COMPUTE SALALB-W = ALBPROD-WK - FORMANDOS-WK
           MOVE SALALB-W            TO GS-LINDET(17: 5)

           COMPUTE DISPONIVEL-W = (FOTMONT-WK + FOTAVUL-WK) -
                                  (FOTCOMIS-WK + FOTFOGO-WK).
           MOVE DISPONIVEL-W        TO GS-LINDET(22: 6)
           MOVE FOTCOMIS-WK         TO GS-LINDET(28: 6)
           MOVE FOTVEND-WK          TO GS-LINDET(34: 6)
           MOVE FOTDEVO-WK          TO GS-LINDET(40: 6)
           COMPUTE SALFOT-W = FOTPROD-WK - (FOTVEND-WK + FOTPERD-WK +
                              FOTDEVO-WK + FOTCOMIS-WK + VENDAVUL-WK
                              + FOTFOGO-WK).
           MOVE SALFOT-W            TO GS-LINDET(46: 6)
           MOVE FITA-WK             TO GS-LINDET(52: 5)

           COMPUTE PERC-W = (FOTVEND-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(57: 6)
           COMPUTE PERC-W = (FOTDEVO-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(63: 6)
           COMPUTE PERC-W = (SALFOT-W / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET(69: 6)

           COMPUTE PRAZO-W = PRAZO-WK / FATVENBRU-WK.
           MOVE PRAZO-W             TO PRAZO-E
           MOVE PRAZO-E             TO GS-LINDET(75: 7)

           MOVE VENLIQIND-WK        TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET(82: 14)
           COMPUTE PRECOMED-W = VENLIQIND-WK / FOTVEND-WK.
           MOVE PRECOMED-W          TO VALOR-E2
           MOVE VALOR-E2            TO GS-LINDET(96: 7)
           COMPUTE VENMEDCLI-W = VENLIQIND-WK / FORMANDOS-WK.
           MOVE VENMEDCLI-W         TO VALOR-E3
           MOVE VALOR-E3            TO GS-LINDET(103: 10)
           COMPUTE GS-PAT-MED-CLIENTE = GS-PATROCINIO /
                                        GS-ALBUM-CLIENTE.

           ADD ALBPROD-WK            TO GS-ALBUM-PRODUZ
           ADD FORMANDOS-WK          TO GS-ALBUM-VEND
           ADD SALALB-W              TO GS-ALBUM-SALDO
           ADD FITA-WK               TO GS-FITA-VENDIDA
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
           MOVE ZEROS TO GS-ALBUM-CLIENTE GS-PROD-FOTOG GS-PROD-FITA
                         GS-VENDA-LIQUIDA GS-PATROCINIO
                         GS-PAT-MED-CLIENTE GS-FOTO-PRODUZ GS-FOTO-PERD
                         GS-FOTO-AVULSA GS-FOTO-COMISSAO GS-FOTO-DISPON
                         GS-FOTO-VEND GS-FOTO-DEVOL GS-FOTO-FOGO
                         GS-FOTO-SALDO
                         GS-PERC-FOTO-PRODUZ GS-PERC-FOTO-PERD
                         GS-PERC-FOTO-AVULSA GS-PERC-FOTO-COMISSAO
                         GS-PERC-FOTO-DISPON
                         GS-PERC-FOTO-VEND GS-PERC-FOTO-DEVOL
                         GS-PERC-FOTO-FOGO GS-PERC-FOTO-SALDO
                         GS-ALBUM-PRODUZ GS-ALBUM-VEND
                         GS-ALBUM-SALDO GS-FITA-VENDIDA
                         GS-PERC-ALBUM-PRODUZ GS-PERC-ALBUM-VEND
                         GS-PERC-ALBUM-SALDO GS-PERC-FITA-VENDIDA
                         GS-ALBUM-AEXECUTAR GS-PROD-FOTOGRAFIA
                         GS-PROD-FITAS GS-VENDA-LIQ-AEXECUTAR
                         GS-PATROCINIO-AEXECUTAR GS-PAT-FORM-AEXECUTAR.
           MOVE ZEROS TO GS-ALBUM-PRODUZ GS-ALBUM-VEND GS-ALBUM-SALDO
                         GS-FITA-VENDIDA GS-FOTO-PRODUZ GS-FOTO-PERD
                         GS-FOTO-DISPON GS-FOTO-AVULSA GS-FOTO-COMISSAO
                         GS-FOTO-VEND GS-FOTO-DEVOL GS-FOTO-FOGO
                         GS-VENDA-BRUTA GS-VENDA-LIQUIDA-FAT
                         PRAZO-GERAL.

      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP109" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           COPY "COND-IMP".
           OPEN OUTPUT RELAT.
           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-EXECUTADO-TRUE AND EXECUTADO-WK = 1 OR
                   GS-AEXECUTAR-TRUE AND EXECUTADO-WK = 2
                      PERFORM MOVER-DADOS-RELAT
                ELSE CONTINUE
                END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-RELAT.

           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.
           COPY "DESC-IMP".
       TOTALIZA-RELAT SECTION.
           MOVE GS-ALBUM-CLIENTE         TO ALBUM-CLIENTE-REL
           MOVE GS-PROD-FOTOG            TO PROD-FOTOG-REL
           MOVE GS-PROD-FITA             TO PROD-FITA-REL.
           MOVE GS-VENDA-LIQUIDA         TO VENDA-LIQUIDA-REL.
           MOVE GS-PATROCINIO            TO PATROCINIO-REL
           MOVE GS-PAT-MED-CLIENTE       TO PAT-MED-CLIENTE-REL.
           MOVE GS-FOTO-PRODUZ           TO FOTO-PRODUZ-REL.
           MOVE GS-FOTO-PERD             TO FOTO-PERD-REL.
           MOVE GS-FOTO-AVULSA           TO FOTO-AVULSA-REL.
           MOVE GS-FOTO-COMISSAO         TO FOTO-COMISSAO-REL.
           MOVE GS-FOTO-DISPON           TO FOTO-DISPON-REL.
           MOVE GS-FOTO-VEND             TO FOTO-VEND-REL.
           MOVE GS-FOTO-DEVOL            TO FOTO-DEVOL-REL.
           MOVE GS-FOTO-FOGO             TO FOTO-FOGO-REL.
           MOVE GS-FOTO-SALDO            TO FOTO-SALDO-REL.
           MOVE GS-PERC-FOTO-PRODUZ      TO PERC-FOTO-PRODUZ-REL.
           MOVE GS-PERC-FOTO-PERD        TO PERC-FOTO-PERD-REL.
           MOVE GS-PERC-FOTO-AVULSA      TO PERC-FOTO-AVULSA-REL.
           MOVE GS-PERC-FOTO-COMISSAO    TO PERC-FOTO-COMISSAO-REL.
           MOVE GS-PERC-FOTO-DISPON      TO PERC-FOTO-DISPON-REL.
           MOVE GS-PERC-FOTO-VEND        TO PERC-FOTO-VEND-REL.
           MOVE GS-PERC-FOTO-DEVOL       TO PERC-FOTO-DEVOL-REL.
           MOVE GS-PERC-FOTO-FOGO        TO PERC-FOTO-FOGO-REL.
           MOVE GS-PERC-FOTO-SALDO       TO PERC-FOTO-SALDO-REL.
           MOVE GS-ALBUM-PRODUZ          TO ALBUM-PRODUZ-REL.
           MOVE GS-ALBUM-VEND            TO ALBUM-VEND-REL.
           MOVE GS-ALBUM-SALDO           TO ALBUM-SALDO-REL.
           MOVE GS-FITA-VENDIDA          TO FITA-VENDIDA-REL.
           MOVE GS-PERC-ALBUM-PRODUZ     TO PERC-ALBUM-PRODUZ-REL.
           MOVE GS-PERC-ALBUM-VEND       TO PERC-ALBUM-VEND-REL.
           MOVE GS-PERC-ALBUM-SALDO      TO PERC-ALBUM-SALDO-REL.
           MOVE GS-PERC-FITA-VENDIDA     TO PERC-FITA-VENDIDA-REL.
           MOVE GS-VENDA-BRUTA           TO VENDA-BRUTA-REL
           MOVE GS-PRAZO-MEDIO           TO PRAZO-MEDIO-REL
           MOVE GS-VENDA-LIQUIDA-FAT     TO VENDA-LIQUIDA-FAT-REL
           MOVE GS-PRECO-MEDIO-FOTO      TO PRECO-MEDIO-FOTO-REL
           MOVE GS-PRECO-MEDIO-CLIENTE   TO PRECO-MEDIO-CLIENTE-REL
           MOVE GS-ALBUM-AEXECUTAR       TO ALBUM-AEXECUTAR-REL
           MOVE GS-VENDA-LIQ-AEXECUTAR   TO VENDA-LIQ-AEXECUTAR-REL
           MOVE GS-PROD-FOTOGRAFIA       TO PROD-FOTOGRAFIA-REL
           MOVE GS-PATROCINIO-AEXECUTAR  TO PATROCINIO-AEXECUTAR-REL
           MOVE GS-PROD-FITAS            TO PROD-FITAS-REL
           MOVE GS-PAT-FORM-AEXECUTAR    TO PAT-FORM-AEXECUTAR-REL
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
           WRITE REG-RELAT FROM LINHA-15 AFTER 2.
           WRITE REG-RELAT FROM LINHA-16.
           WRITE REG-RELAT FROM LINHA-17.
           WRITE REG-RELAT FROM LINHA-18.
           WRITE REG-RELAT FROM LINHA-19.
           WRITE REG-RELAT FROM LINHA-20.
           WRITE REG-RELAT FROM LINHA-21.
           WRITE REG-RELAT FROM LINHA-22.
           WRITE REG-RELAT FROM LINHA-23.
           WRITE REG-RELAT FROM LINHA-26 AFTER 2.
           WRITE REG-RELAT FROM LINHA-27.
           WRITE REG-RELAT FROM LINHA-28.
           WRITE REG-RELAT FROM LINHA-29.
           WRITE REG-RELAT FROM LINHA-30.
           WRITE REG-RELAT FROM LINHA-31.
           WRITE REG-RELAT FROM LINHA-32.

       MOVER-DADOS-RELAT SECTION.
           ADD 3 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
           MOVE CONTRATO-WK       TO LINDET-REL(1: 5)
           MOVE IDENTIFICACAO-WK  TO LINDET-REL(6: 11)
           MOVE CIDADE-WK         TO LINDET-REL(17: 10)
           MOVE REGIAO-WK         TO LINDET-REL(27: 10)
           MOVE REPRESENT-WK      TO LINDET-REL(37: 9)
           MOVE MESANO-WK         TO MESANO-I
           MOVE MESANO-I(5: 2)    TO MESANO-W(1: 2)
           MOVE MESANO-I(1: 4)    TO MESANO-W(3: 4)
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO LINDET-REL(46: 8)
           MOVE QT-FORM-WK        TO LINDET-REL(54: 5)
           MOVE PADRAO-WK         TO LINDET-REL(59: 2)
           MOVE QT-FOTOS-WK       TO LINDET-REL(61: 6)
           MOVE QT-FITAS-WK       TO LINDET-REL(67: 5)
           MOVE OBJ-VENDA-WK      TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(72: 14)
           MOVE PATROCINIO-WK     TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(86: 14)
           MOVE PAT-FORM-WK       TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(100: 10)
           WRITE REG-RELAT FROM LINDET.

           IF EXECUTADO-WK = 2 CONTINUE
           ELSE PERFORM MOVER-DADOS-RELAT1.

       MOVER-DADOS-RELAT1 SECTION.
      *    2o. LINHA DA LINDET
           MOVE SPACES              TO LINDET-REL
           MOVE DATA-WK             TO DATA-I
           MOVE MES-I               TO LINDET-REL(1: 2)
           MOVE "/"                 TO LINDET-REL(3: 1)
           MOVE ANO-I(3: 2)         TO LINDET-REL(4: 3)
           MOVE ALBPROD-WK          TO LINDET-REL(7: 5)
           MOVE FORMANDOS-WK        TO LINDET-REL(12: 5)
           COMPUTE SALALB-W = ALBPROD-WK - FORMANDOS-WK
           MOVE SALALB-W            TO LINDET-REL(17: 5)

           COMPUTE DISPONIVEL-W = (FOTMONT-WK + FOTAVUL-WK) -
                                  (FOTCOMIS-WK + FOTFOGO-WK).
           MOVE DISPONIVEL-W        TO LINDET-REL(22: 6)
           MOVE FOTCOMIS-WK         TO LINDET-REL(28: 6)
           MOVE FOTVEND-WK          TO LINDET-REL(34: 6)
           MOVE FOTDEVO-WK          TO LINDET-REL(40: 6)
           COMPUTE SALFOT-W = FOTPROD-WK - (FOTVEND-WK + FOTPERD-WK +
                              FOTDEVO-WK + FOTCOMIS-WK + VENDAVUL-WK
                              + FOTFOGO-WK).
           MOVE SALFOT-W            TO LINDET-REL(46: 6)
           MOVE FITA-WK             TO LINDET-REL(52: 5)

           COMPUTE PERC-W = (FOTVEND-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(57: 6)
           COMPUTE PERC-W = (FOTDEVO-WK / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(63: 6)
           COMPUTE PERC-W = (SALFOT-W / DISPONIVEL-W) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET-REL(69: 6)

           COMPUTE PRAZO-W = PRAZO-WK / FATVENBRU-WK.
           MOVE PRAZO-W             TO PRAZO-E
           MOVE PRAZO-E             TO LINDET-REL(75: 7)

           MOVE VENLIQIND-WK        TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(82: 14)
           COMPUTE PRECOMED-W = VENLIQIND-WK / FOTVEND-WK.
           MOVE PRECOMED-W          TO VALOR-E2
           MOVE VALOR-E2            TO LINDET-REL(96: 7)
           COMPUTE VENMEDCLI-W = VENLIQIND-WK / FORMANDOS-WK.
           MOVE VENMEDCLI-W         TO VALOR-E3
           MOVE VALOR-E3            TO LINDET-REL(103: 10)
           WRITE REG-RELAT FROM LINDET.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.


      *    PERFORM UNTIL ST-WORK = "10"
      *       READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
      *       NOT AT END
      *         IF GS-C-EVENTO-TRUE AND ORG-EVENTO-WK = 1 OR
      *            GS-S-EVENTO-TRUE AND ORG-EVENTO-WK = 0
      *               PERFORM MOVER-DADOS-RELATORIO
      *         ELSE CONTINUE
      *         END-IF
      *       END-READ
      *    END-PERFORM.
      *    PERFORM TOTALIZA-REL.
      *TOTALIZA-REL SECTION.
      *    MOVE "OBJ.VENDA: "  TO LINDET-REL(1: 11)
      *    MOVE OBJ-VENDA-G    TO VALOR-E
      *    MOVE VALOR-E        TO LINDET-REL(12: 13)
      *    MOVE "T.PAGO.: "    TO LINDET-REL(29: 9)
      *    MOVE VLR-PAGO-G     TO VALOR-E
      *    MOVE VALOR-E        TO LINDET-REL(38: 13)
      *    MOVE "T.PAGAR: "    TO LINDET-REL(56: 9)
      *    MOVE VLR-PAGAR-G    TO VALOR-E
      *    MOVE VALOR-E        TO LINDET-REL(65: 13)
      *    WRITE REG-RELAT FROM LINDET-REL AFTER 2.
      *
      *    MOVE "PATROCIN.: "  TO LINDET-REL(1: 11)
      *    MOVE PATROCINIO-G   TO VALOR-E
      *    MOVE VALOR-E        TO LINDET-REL(12: 13)
      *    MOVE "PT.FORM: "    TO LINDET-REL(29: 9)
      *    MOVE PAT-FORM-G     TO VALOR-E
      *    MOVE VALOR-E        TO LINDET-REL(38: 13)
      *    WRITE REG-RELAT FROM LINDET-REL.
      *
      *    MOVE "TOT.FOTOS: "  TO LINDET-REL(1: 11)
      *    MOVE QT-FOTO-G      TO QTDE-E
      *    MOVE QTDE-E         TO LINDET-REL(12: 13)
      *    MOVE "T.FORM.: "    TO LINDET-REL(29: 9)
      *    MOVE QT-FORM-G      TO QTDE-E
      *    MOVE QTDE-E         TO LINDET-REL(38: 13)
      *    WRITE REG-RELAT FROM LINDET-REL.
      *
      *MOVER-DADOS-RELATORIO SECTION.
      *    ADD 2 TO LIN.
      *    IF LIN > 56 PERFORM CABECALHO.
      *    MOVE CONTRATO-WK       TO LINDET-REL(1: 5)
      *    MOVE IDENTIFICACAO-WK          TO LINDET-REL(6: 11)
      *    MOVE CIDADE-WK         TO LINDET-REL(17: 11)
      *    MOVE QT-FORM-WK        TO LINDET-REL(28: 5)
      *    MOVE PADRAO-WK         TO LINDET-REL(33: 2)
      *    MOVE QT-FOTOS-WK       TO LINDET-REL(35: 7)
      *    MOVE MESANO-WK         TO MESANO-I
      *    MOVE MESANO-I(5: 2)    TO MESANO-W(1: 2)
      *    MOVE MESANO-I(1: 4)    TO MESANO-W(3: 4)
      *    MOVE MESANO-W          TO MESANO-E
      *    MOVE MESANO-E          TO LINDET-REL(42: 8)
      *    MOVE ASSINATURA-WK     TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO LINDET-REL(50: 11)
      *    MOVE PRECO-WK          TO VALOR-E2
      *    MOVE VALOR-E2          TO LINDET-REL(61: 5)
      *    MOVE OBJ-VENDA-WK      TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(66: 12)
      *    WRITE REG-RELAT FROM LINDET.
      *
      *    MOVE VLR-PAGO-WK       TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(01: 12)
      *    MOVE VLR-PAGAR-WK      TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(13: 12)
      *    MOVE PATROCINIO-WK     TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(25: 12)
      *    MOVE PERC-PATROC-WK    TO PERC-E
      *    MOVE PERC-E            TO LINDET-REL(37: 5)
      *    MOVE PAT-FORM-WK       TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(42: 12)
      *    MOVE PM-WK             TO LINDET-REL(54: 04)
      *    MOVE REPRESENT-WK      TO LINDET-REL(58: 11)
      *    WRITE REG-RELAT FROM LINDET.
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
           CLOSE COD040 COD050 CAD010 CAD012 CGD001
                 COD005 COD002 MTD001 MTD020 RCD100.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
