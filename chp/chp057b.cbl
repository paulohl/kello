       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP057.
       DATE-WRITTEN. 15/06/1999
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *FUNÇÃO: CALCULO DE DESCONTO NOS CHEQUES PRÉ-DATADOS
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
           COPY CHPX010.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGACESS.SEL.
           SELECT CHD041 ASSIGN TO PATH-CHD041
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS NOME-41
                  ALTERNATE RECORD KEY IS DATA-BASE-41
                  WITH DUPLICATES
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CHD041.

           SELECT CHD040 ASSIGN TO PATH-CHD040
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS CHAVE-40 = NOME-40 SEQ-40
                  ALTERNATE RECORD KEY IS ALT-40 =
                           NOME-40 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-40 = VENC-40
                           BANCO-40 NR-CHEQUE-40 WITH DUPLICATES
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CHD040.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = DATA-MOVTO-WK
                                           SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK = VENC-WK
                               BANCO-WK NR-CHEQUE-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW018.
       COPY CHPW010.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGACESS.FD.
       FD  CHD040.
       01  REG-CHD040.
           05  VENC-40            PIC 9(8).
           05  BANCO-40           PIC 9(4).
           05  NR-CHEQUE-40       PIC X(10).
           05  NOME-40            PIC X(16).
           05  SEQ-40             PIC 9(3).
           05  VALOR-40           PIC 9(8)V99.
           05  VENC1-40           PIC 9(8).
           05  DIAS-40            PIC 9(3).
           05  DATA-MOVTO-40      PIC 9(8).
           05  SEQ-CHEQUE-40      PIC 9(4).

       FD  CHD041.
       01  REG-CHD041.
           05  NOME-41               PIC X(16).
           05  CARTEIRA-41           PIC 9(2) OCCURS 6 TIMES.
           05  DATA-BASE-41          PIC 9(8).
           05  TAXA-JUROS-41         PIC 99V99.
           05  DATA-INI-41           PIC 9(8).
           05  DATA-FIM-41           PIC 9(8).
           05  DIAS-INI-41           PIC 9(3).
           05  DIAS-FIM-41           PIC 9(3).
           05  FERIADOS-41           PIC 9(6) OCCURS 10 TIMES.
           05  QTDE-CHEQUES-41       PIC 9(4).
           05  VLR-BRUTO-41          PIC 9(10)V99.
           05  PM-41                 PIC 9(3)V99.
           05  DIAS-30-41            PIC 9(3)V99.
           05  TAXA-30-41            PIC 9V999999.
           05  JURO-30-41            PIC 9(10)V99.
           05  SALDO-30-41           PIC 9(10)V99.
           05  DIAS-60-41            PIC 9(3)V99.
           05  TAXA-60-41            PIC 9V999999.
           05  JURO-60-41            PIC 9(10)V99.
           05  SALDO-60-41           PIC 9(10)V99.
           05  DIAS-90-41            PIC 9(3)V99.
           05  TAXA-90-41            PIC 9V999999.
           05  JURO-90-41            PIC 9(10)V99.
           05  SALDO-90-41           PIC 9(10)V99.
           05  DIAS-120-41           PIC 9(3)V99.
           05  TAXA-120-41           PIC 9V999999.
           05  JURO-120-41           PIC 9(10)V99.
           05  SALDO-120-41          PIC 9(10)V99.
           05  DIAS-150-41           PIC 9(3)V99.
           05  TAXA-150-41           PIC 9V999999.
           05  JURO-150-41           PIC 9(10)V99.
           05  SALDO-150-41          PIC 9(10)V99.
           05  DIAS-180-41           PIC 9(3)V99.
           05  TAXA-180-41           PIC 9V999999.
           05  JURO-180-41           PIC 9(10)V99.
           05  SALDO-180-41          PIC 9(10)V99.
           05  DIAS-210-41           PIC 9(3)V99.
           05  TAXA-210-41           PIC 9V999999.
           05  JURO-210-41           PIC 9(10)V99.
           05  SALDO-210-41          PIC 9(10)V99.
           05  DIAS-240-41           PIC 9(3)V99.
           05  TAXA-240-41           PIC 9V999999.
           05  JURO-240-41           PIC 9(10)V99.
           05  SALDO-240-41          PIC 9(10)V99.
           05  DIAS-270-41           PIC 9(3)V99.
           05  TAXA-270-41           PIC 9V999999.
           05  JURO-270-41           PIC 9(10)V99.
           05  SALDO-270-41          PIC 9(10)V99.
           05  DIAS-300-41           PIC 9(3)V99.
           05  TAXA-300-41           PIC 9V999999.
           05  JURO-300-41           PIC 9(10)V99.
           05  SALDO-300-41          PIC 9(10)V99.
           05  DIAS-330-41           PIC 9(3)V99.
           05  TAXA-330-41           PIC 9V999999.
           05  JURO-330-41           PIC 9(10)V99.
           05  SALDO-330-41          PIC 9(10)V99.
           05  DIAS-360-41           PIC 9(3)V99.
           05  TAXA-360-41           PIC 9V999999.
           05  JURO-360-41           PIC 9(10)V99.
           05  SALDO-360-41          PIC 9(10)V99.
           05  DIAS-390-41           PIC 9(3)V99.
           05  TAXA-390-41           PIC 9V999999.
           05  JURO-390-41           PIC 9(10)V99.
           05  SALDO-390-41          PIC 9(10)V99.
           05  DIAS-420-41           PIC 9(3)V99.
           05  TAXA-420-41           PIC 9V999999.
           05  JURO-420-41           PIC 9(10)V99.
           05  SALDO-420-41          PIC 9(10)V99.
           05  DIAS-450-41           PIC 9(3)V99.
           05  TAXA-450-41           PIC 9V999999.
           05  JURO-450-41           PIC 9(10)V99.
           05  SALDO-450-41          PIC 9(10)V99.
           05  DIAS-480-41           PIC 9(3)V99.
           05  TAXA-480-41           PIC 9V999999.
           05  JURO-480-41           PIC 9(10)V99.
           05  SALDO-480-41          PIC 9(10)V99.
           05  DIAS-510-41           PIC 9(3)V99.
           05  TAXA-510-41           PIC 9V999999.
           05  JURO-510-41           PIC 9(10)V99.
           05  SALDO-510-41          PIC 9(10)V99.
           05  DIAS-540-41           PIC 9(3)V99.
           05  TAXA-540-41           PIC 9V999999.
           05  JURO-540-41           PIC 9(10)V99.
           05  SALDO-540-41          PIC 9(10)V99.
           05  PORTADOR-DESTINO-41   PIC 9(03).

       FD  WORK.
       01  REG-WORK.
           05  DATA-MOVTO-WK      PIC 9(8).
           05  SEQ-WK             PIC 9(4).
           05  VENC-WK            PIC 9(8).
           05  BANCO-WK           PIC 9(4).
           05  NR-CHEQUE-WK       PIC X(10).
           05  VENC1-WK           PIC 9(8).
      *    VENC1-WK - DATA-VENCTO + 2 ou 1 (caso caia -sabado ou doming)
      *    Exemplo: vencto cai no sabado será acrescido 2 dias ao venc1
           05  DIAS-WK            PIC 9(3).
           05  VALOR-WK           PIC 9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CHP057.CPB".
           COPY "CHP057.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPDIAS1.CPY".
      *    COPY "CPWEEK1.CPY".
           COPY "CPTIME.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD040             PIC XX       VALUE SPACES.
           05  ST-CHD041             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
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
           05  VARIA-W          PIC 9(8)          VALUE ZEROS.
           05  DATA-E           PIC 99/99/9999    BLANK WHEN ZEROS.
           05  VENC-W           PIC 9(8)          VALUE ZEROS.
           05  VALOR-E          PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1         PIC ZZZ.ZZZ,ZZ    BLANK WHEN ZEROS.
           05  DATA-DIA-W       PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA         PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI       PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM       PIC 9(8)     VALUE ZEROS.
           05  DATA-BASE-I      PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ          PIC 9(3)     VALUE ZEROS.
           05  DATA-DIA-I       PIC 9(8)     VALUE ZEROS.
           05  QTDE-CHEQUE-W    PIC 9(4)     VALUE ZEROS.
           05  PM-W             PIC 9(3)V99  VALUE ZEROS.
           05  PM-E             PIC ZZZ,ZZ.
           05  VALOR-ACUMULADO  PIC 9(10)V99 VALUE ZEROS.
           05  VALOR-TOTAL      PIC 9(10)V99 VALUE ZEROS.
           05  DIAS-INI         PIC 9(3)     VALUE ZEROS.
           05  DIAS-FIM         PIC 9(3)     VALUE ZEROS.
           05  DIAS-30-W        PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-60-W        PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-90-W        PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-120-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-150-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-180-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-210-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-240-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-270-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-300-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-330-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-360-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-390-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-420-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-450-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-480-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-510-W       PIC 9(3)V99  VALUE ZEROS.
           05  DIAS-540-W       PIC 9(3)V99  VALUE ZEROS.
           05  JURO-30-W        PIC 9(10)V99 VALUE ZEROS.
           05  JURO-60-W        PIC 9(10)V99 VALUE ZEROS.
           05  JURO-90-W        PIC 9(10)V99 VALUE ZEROS.
           05  JURO-120-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-150-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-180-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-210-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-240-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-270-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-300-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-330-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-360-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-390-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-420-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-450-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-480-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-510-W       PIC 9(10)V99 VALUE ZEROS.
           05  JURO-540-W       PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-30-W       PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-60-W       PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-90-W       PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-120-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-150-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-180-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-210-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-240-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-270-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-300-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-330-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-360-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-390-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-420-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-450-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-480-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-510-W      PIC 9(10)V99 VALUE ZEROS.
           05  SALDO-540-W      PIC 9(10)V99 VALUE ZEROS.
           05  TAXA-30-W        PIC 9V999999   VALUE ZEROS.
           05  TAXA-60-W        PIC 9V999999  VALUE ZEROS.
           05  TAXA-90-W        PIC 9V999999   VALUE ZEROS.
           05  TAXA-120-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-150-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-180-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-210-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-240-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-270-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-300-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-330-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-360-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-390-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-420-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-450-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-480-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-510-W       PIC 9V999999   VALUE ZEROS.
           05  TAXA-540-W       PIC 9V999999   VALUE ZEROS.
           05  DIAS-TAXA        PIC 9(2)     VALUE ZEROS.
           05  VLR-TAXA OCCURS 30 TIMES PIC 9V999999.
           05  VLR-TAXA1        PIC 9V9999999 VALUE ZEROS.
           05  INDICE1          PIC 999V9999999 VALUE ZEROS.
           05  INDICE3          PIC 999V9999999 VALUE ZEROS.
           05  INDICE2          PIC 99V9999999 VALUE ZEROS.
           05  TAXA-W           PIC 99V99      VALUE ZEROS.
           05  INDICE-SOMA      PIC 99V9999999 VALUE ZEROS.
           05  NOME-PORT-W      PIC X(10)    VALUE SPACES.
           05  DIAS-E           PIC ZZZ,ZZ.
           05  TAXA-E           PIC Z,ZZZZZZ.
           05  AAAAMMDD         PIC 9(8)     VALUE ZEROS.
           05  DDMMAAAA         PIC 9(8)     VALUE ZEROS.
           05  I                PIC 99       VALUE ZEROS.
           05  SENHA-W1         PIC 9(4)     COMP-3.
           05  LIN              PIC 9(02)    VALUE ZEROS.
           05  AUX-SENHA        PIC 9(4)     VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(93)    VALUE SPACES.
           05  FILLER              PIC X(09)   VALUE "EMISSAO: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(034) VALUE
              "CALCULO DE DESCONTO DE CHEQUES PRE".
           05 FILLER                         PIC  X(002) VALUE "-D".
           05 FILLER                         PIC  X(024) VALUE
              "ATADOS PRAZO MEDIO/JUROS".
           05 FILLER                  PIC  X(12)  VALUE '      NOME: '.
           05 NOME-REL                PIC  X(8)   VALUE SPACES.
       02  LINHA-02                          PIC  X(085) VALUE ALL "=".
       02  LINHA-03.
           05 FILLER                         PIC  X(010) VALUE
              "CARTEIRA: ".
           05 CARTEIRA1-REL                  PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 NOME-CART1-REL                 PIC  X(019) VALUE SPACES.
           05 FILLER                         PIC  X(004) VALUE SPACES.
           05 CARTEIRA2-REL                  PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 NOME-CART2-REL                 PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 CARTEIRA3-REL                  PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 NOME-CART3-REL                 PIC  X(020) VALUE SPACES.
       02  LINHA-031.
           05 FILLER                         PIC  X(010) VALUE
              SPACES.
           05 CARTEIRA4-REL                  PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 NOME-CART4-REL                 PIC  X(019) VALUE SPACES.
           05 FILLER                         PIC  X(004) VALUE SPACES.
           05 CARTEIRA5-REL                  PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 NOME-CART5-REL                 PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 CARTEIRA6-REL                  PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 NOME-CART6-REL                 PIC  X(020) VALUE SPACES.
       02  LINHA-04.
           05 FILLER                         PIC  X(022) VALUE
              "DATA BASE P/ CALCULO: ".
           05 DATA-BASE-REL                  PIC  99/99/9999.
           05 FILLER                         PIC  X(022) VALUE
              "   TAXA JUROS NEGOC.: ".
           05 TAXA-JUROS-REL                 PIC  ZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE " %".
           05 FILLER                         PIC  X(14)  VALUE
           "   TAXA REAL: ".
           05 TAXA-JUROS-REAL-REL            PIC  ZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE " %".
       02  LINHA-05.
           05 FILLER                         PIC  X(032) VALUE
              "PERIODO DOS CHEQUES A CALCULAR: ".
           05 VENCTO-INI-REL                 PIC  99/99/9999.
           05 FILLER                         PIC  X(001) VALUE "(".
           05 DIAS-INI-REL                   PIC  ZZ9.
           05 FILLER                         PIC  X(011) VALUE
              " dias)  A  ".
           05 VENCTO-FIM-REL                 PIC  99/99/9999.
           05 FILLER                         PIC  X(001) VALUE "(".
           05 DIAS-FIM-REL                   PIC  ZZZZ9.
           05 FILLER                         PIC  X(006) VALUE
              " dias)".
      *02  LINHA-06.
      *    05 FILLER                         PIC  X(014) VALUE
      *       "FERIADOS  (1) ".
      *    05 FERIADOS-REL BLANK WHEN ZEROS  PIC  99/99/9999.
      *    05 FILLER                         PIC  X(005) VALUE
      *       " (2) ".
      *    05 FERIADO2-REL BLANK WHEN ZEROS  PIC  99/99/9999.
      *    05 FILLER                         PIC  X(005) VALUE
      *       " (3) ".
      *    05 FERIADO3-REL BLANK WHEN ZEROS  PIC  99/99/9999.
      *    05 FILLER                         PIC  X(005) VALUE
      *       " (4) ".
      *    05 FERIADO4-REL BLANK WHEN ZEROS  PIC  99/99/9999.
      *    05 FILLER                         PIC  X(005) VALUE
      *       " (5) ".
      *    05 FERIADO5-REL BLANK WHEN ZEROS  PIC  99/99/9999.
      *02  LINHA-07.
      *    05 FILLER                         PIC  X(010) VALUE SPACES.
      *    05 FILLER                         PIC  X(004) VALUE "(6) ".
      *    05 FERIADO6-REL BLANK WHEN ZEROS  PIC  99/99/9999.
      *    05 FILLER                         PIC  X(005) VALUE
      *       " (7) ".
      *    05 FERIADO7-REL BLANK WHEN ZEROS  PIC  99/99/9999.
      *    05 FILLER                         PIC  X(005) VALUE
      *       " (8) ".
      *    05 FERIADO8-REL BLANK WHEN ZEROS  PIC  99/99/9999.
      *    05 FILLER                         PIC  X(005) VALUE
      *       " (9) ".
      *    05 FERIADO9-REL BLANK WHEN ZEROS  PIC  99/99/9999.
      *    05 FILLER                         PIC  X(005) VALUE
      *       " (0) ".
      *    05 FERIADO0-REL BLANK WHEN ZEROS  PIC  99/99/9999.
       02  LINHA-08.
           05 FILLER                         PIC  X(025) VALUE
              "TOTAL DE CHEQUES/BRUTO..:".
           05 FILLER                         PIC  X(010) VALUE
              "  (QUANT: ".
           05 QUANT-CH-REL                   PIC  ZZZ9.
           05 FILLER                         PIC  X(015) VALUE
              ")          R$: ".
           05 VLR-BRUTO-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              "   (PM: ".
           05 PM-REL                         PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE ")".
       02  LINHA-09.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 30 DIAS.: ".
           05 DIAS-30-REL                    PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-30-REL                    PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-30-REL                    PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-10.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 30 DIAS..:                         R$: ".
           05 SALDO-30-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-11.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 60 DIAS.: ".
           05 DIAS-60-REL                    PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-60-REL                    PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-60-REL                    PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-12.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 60 DIAS                            R$: ".
           05 SALDO-60-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-13.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 90 DIAS.: ".
           05 DIAS-90-REL                    PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-90-REL                    PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-90-REL                    PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-14.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 90 DIAS                            R$: ".
           05 SALDO-90-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-15.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 120 DIAS: ".
           05 DIAS-120-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-120-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-120-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-16.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 120 DIAS                           R$: ".
           05 SALDO-120-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-17.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 150 DIAS: ".
           05 DIAS-150-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-150-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-150-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-18.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 150 DIAS                           R$: ".
           05 SALDO-150-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-19.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 180 DIAS: ".
           05 DIAS-180-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-180-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-180-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-20.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 180 DIAS                           R$: ".
           05 SALDO-180-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-21.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 210 DIAS: ".
           05 DIAS-210-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-210-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-210-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-22.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 210 DIAS                           R$: ".
           05 SALDO-210-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-23.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 240 DIAS: ".
           05 DIAS-240-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-240-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-240-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-24.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 240 DIAS                           R$: ".
           05 SALDO-240-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-25.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 270 DIAS: ".
           05 DIAS-270-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-270-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-270-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-26.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 270 DIAS                           R$: ".
           05 SALDO-270-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-27.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 300 DIAS: ".
           05 DIAS-300-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-300-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-300-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-28.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 300 DIAS                           R$: ".
           05 SALDO-300-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-29.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 330 DIAS: ".
           05 DIAS-330-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-330-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-330-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-30.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 330 DIAS                           R$: ".
           05 SALDO-330-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-31.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 360 DIAS: ".
           05 DIAS-360-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-360-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-360-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-32.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 360 DIAS                           R$: ".
           05 SALDO-360-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-33.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 390 DIAS: ".
           05 DIAS-390-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-390-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-390-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-34.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 390 DIAS                           R$: ".
           05 SALDO-390-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-35.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 420 DIAS: ".
           05 DIAS-420-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-420-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-420-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-36.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 420 DIAS                           R$: ".
           05 SALDO-420-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-37.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 450 DIAS: ".
           05 DIAS-450-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-450-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-450-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-38.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 450 DIAS                           R$: ".
           05 SALDO-450-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-39.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 480 DIAS: ".
           05 DIAS-480-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-480-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-480-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-40.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 480 DIAS                           R$: ".
           05 SALDO-480-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-41.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 510 DIAS: ".
           05 DIAS-510-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-510-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-510-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-42.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 510 DIAS                           R$: ".
           05 SALDO-510-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-43.
           05 FILLER                         PIC  X(026) VALUE
              "PRAZO MEDIO ATE 540 DIAS: ".
           05 DIAS-540-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE
              " JR(%): ".
           05 TAXA-540-REL                   PIC  Z,ZZZZZZ.
           05 FILLER                         PIC  X(006) VALUE
              "  R$: ".
           05 JURO-540-REL                   PIC  ZZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-44.
           05 FILLER                         PIC  X(054) VALUE
              "SALDO PARA ATE 540 DIAS                           R$: ".
           05 SALDO-540-REL                  PIC  ZZZ.ZZZ.ZZZ,ZZ.

      *---------------------------
       01  CAB10.
           05  FILLER              PIC X(110)   VALUE
           'RELACAO DE CHEQUE'.
       01  CAB11.
           05  FILLER              PIC X(110)   VALUE ALL '='.
       01  CAB12.
           05  FILLER              PIC X(110)   VALUE
           "DATA-VECTO DIAS NR-CHEQ  BANCO CIDADE               NOME
      -    "                               VALOR".

       01  LINDET1.
           05  LINDET1-REL         PIC X(110)   VALUE SPACES.
      *-------------------------------------------------------------

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

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA6-W FROM DATE.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).

           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV       TO EMISSAO-REL DATA-DIA-W.

           ACCEPT HORA-BRA FROM TIME.

           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.

      *    COPY "CBDATA1.CPY".
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CHD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD040.
           MOVE "CHD041"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD041.
           MOVE "CRD200"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CAD004 CAD018 CHD010 CRD200 CRD201.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.

           OPEN I-O CHD040
                    CHD041.

           IF ST-CHD040 = "35" CLOSE CHD040   OPEN OUTPUT CHD040
                               CLOSE CHD040   OPEN I-O    CHD040.

           IF ST-CHD041 = "35" CLOSE CHD041   OPEN OUTPUT CHD041
                               CLOSE CHD041   OPEN I-O    CHD041.

           CLOSE      CHD040 CHD041
           OPEN INPUT CHD040 CHD041

           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD040 <> "00"
              MOVE "ERRO ABERTURA CHD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD041 <> "00"
              MOVE "ERRO ABERTURA CHD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD041 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CHP057"            to logacess-programa
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
               WHEN GS-CALCULA-DIAS-INI-TRUE
                    PERFORM CALCULA-DIAS-INI
               WHEN GS-CALCULA-DIAS-FIM-TRUE
                    PERFORM CALCULA-DIAS-FIM
               WHEN GS-CALCULA-FLG-TRUE
                    PERFORM CALCULA-DESCONTO
               WHEN GS-TRANSF-PORT-TRUE
                    PERFORM TRANSFERE-PORTADOR
               WHEN GS-VERIF-SENHA-PORT-TRUE
                    PERFORM VERIFICA-SENHA-PORTADOR
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LE-PORTADOR
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-CARREGA-LISTA-CH-TRUE
                    PERFORM CARREGA-LISTA-CHEQUE
               WHEN GS-CONSULTAR-DADOS-TRUE
                    PERFORM CONSULTAR-DADOS
               WHEN GS-EXCLUI-CHEQUE-TRUE
                    PERFORM EXCLUI-CHEQUE
               WHEN GS-MOSTRA-CALCULO-TRUE
                    PERFORM MOSTRA-CALCULO
                    MOVE "REFRESH-DATA" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
               WHEN GS-GRAVAR-FLG-TRUE
                    PERFORM GRAVA-CALCULO
               WHEN GS-PRINTER-CH-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-LISTA-CHEQUE
                    END-IF
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RESUMO
                    END-IF
               WHEN GS-PEGAR-NOME-ARQUIVO-TRUE
                    PERFORM PEGAR-NOME-ARQUIVO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       PEGAR-NOME-ARQUIVO SECTION.
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           ACCEPT WS-HORA-SYS FROM TIME.
           STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU "-"
                  WS-HO-SYS WS-MI-SYS WS-SE-SYS
           INTO GS-NOME.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       LE-PORTADOR SECTION.
           MOVE GS-PORTADOR-G TO PORTADOR.
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT TO GS-DESCR-PORTADOR-G.
       POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESCR-PORTADOR-G.
           MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR-G.

       CALCULA-DIAS-INI SECTION.
           MOVE GS-DATA-BASE-CALC TO DATA-INV
           CALL "GRIDAT2"         USING DATA-INV
           MOVE DATA-INV          TO GRDIAS-AAMMDD-INICIAL
           MOVE GS-VENCTO-INI     TO DATA-INV.
           CALL "GRIDAT2"         USING DATA-INV.
           MOVE DATA-INV          TO GRDIAS-AAMMDD-FINAL.
           CALL "GRDIAS1"         USING PARAMETROS-GRDIAS.
           MOVE GRDIAS-NUM-DIAS   TO GS-DIAS-INI.
       CALCULA-DIAS-FIM SECTION.
           MOVE GS-DATA-BASE-CALC TO DATA-INV
           CALL "GRIDAT2"         USING DATA-INV
           MOVE DATA-INV          TO GRDIAS-AAMMDD-INICIAL
           MOVE GS-VENCTO-FIM     TO DATA-INV.
           CALL "GRIDAT2"         USING DATA-INV.
           MOVE DATA-INV          TO GRDIAS-AAMMDD-FINAL.
           CALL "GRDIAS1"         USING PARAMETROS-GRDIAS.
           MOVE GRDIAS-NUM-DIAS   TO GS-DIAS-FIM.

       CALCULA-DESCONTO SECTION.
           PERFORM CRIA-TABELA
           MOVE GS-DATA-BASE-CALC TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV    TO DATA-BASE-I.
           MOVE GS-VENCTO-INI TO DATA-INV.
           CALL "GRIDAT2"   USING DATA-INV.
           MOVE DATA-INV    TO DATA-VENCTO-CH10 VENCTO-INI.
           MOVE GS-VENCTO-FIM TO DATA-INV
           CALL "GRIDAT2"   USING DATA-INV
           MOVE DATA-INV    TO VENCTO-FIM
           MOVE ZEROS    TO SITUACAO-CH10.
           MOVE ZEROS    TO PORTADOR-CH10.
           START CHD010 KEY IS NOT < ALT-CH2 INVALID KEY
                 MOVE "10" TO ST-CHD010.
           CLOSE WORK.  OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE ZEROS TO GS-VALOR-TOT-CHEQUE GS-QTDE-TOT-CHEQUE
                         VALOR-ACUMULADO.
           PERFORM UNTIL ST-CHD010 = "10"
             READ CHD010 NEXT RECORD AT END MOVE "10" TO ST-CHD010
               NOT AT END
                 MOVE "Gravando " TO GS-TEXTO1(1: 9)
                 MOVE DATA-VENCTO-CH10 TO DATA-INV
                 CALL "GRIDAT1" USING DATA-INV
                 MOVE DATA-INV TO DATA-E
                 MOVE DATA-E      TO GS-TEXTO1(10: 10)
                 MOVE "FEEDBACK-CALCULA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 IF SITUACAO-CH10 > 0 OR DATA-VENCTO-CH10 > VENCTO-FIM
                    MOVE "10" TO ST-CHD010
                 ELSE
                  IF PORTADOR-CH10 <> GS-CARTEIRA(1) AND
                     PORTADOR-CH10 <> GS-CARTEIRA(2) AND
                     PORTADOR-CH10 <> GS-CARTEIRA(3) AND
                     PORTADOR-CH10 <> GS-CARTEIRA(4) AND
                     PORTADOR-CH10 <> GS-CARTEIRA(5) AND
                     PORTADOR-CH10 <> GS-CARTEIRA(6)
                     CONTINUE
                  ELSE
                     MOVE DATA-VENCTO-CH10 TO VENC-WK
                     MOVE NR-CHEQUE-CH10   TO NR-CHEQUE-WK
                     MOVE BANCO-CH10       TO BANCO-WK
                     MOVE SEQ-CH10         TO SEQ-WK
                     MOVE DATA-MOVTO-CH10  TO DATA-MOVTO-WK
                     MOVE DATA-VENCTO-CH10 TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV         TO VENC-W
                     PERFORM TESTA-SABADO-DOMINGO
                     PERFORM TESTA-FERIADO
                     MOVE DATA-BASE-I TO GRDIAS-AAMMDD-INICIAL
                                         VENC1-WK
                     MOVE VENC-W TO DATA-INV
                     CALL "GRIDAT2" USING DATA-INV
                     MOVE DATA-INV         TO GRDIAS-AAMMDD-FINAL
                     CALL "GRDIAS1" USING PARAMETROS-GRDIAS
                     COMPUTE VALOR-ACUMULADO = VALOR-ACUMULADO +
                                (GRDIAS-NUM-DIAS * VALOR-CH10)
                     MOVE GRDIAS-NUM-DIAS TO DIAS-WK
                     ADD VALOR-CH10   TO GS-VALOR-TOT-CHEQUE
                     MOVE VALOR-CH10  TO VALOR-WK
                     ADD 1            TO GS-QTDE-TOT-CHEQUE
                     WRITE REG-WORK
                     END-WRITE
                  END-IF
                 END-IF
             END-READ
           END-PERFORM.
           IF VALOR-ACUMULADO <> ZEROS PERFORM MOSTRA-CALCULO.
       TESTA-SABADO-DOMINGO SECTION.
           MOVE VENC-W TO GRTIME-DATE.
           MOVE 1      TO GRTIME-TYPE.
           MOVE 8      TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.

           IF GRTIME-WEEK-NUM = 0 MOVE 2 TO GRADAY-DIAS
                             MOVE GRTIME-DATE TO GRADAY-DATA
                             CALL "GRADAY1" USING PARAMETROS-GRADAY
                             MOVE GRADAY-DATA TO VENC-W.
           IF GRTIME-WEEK-NUM = 1 MOVE 1 TO GRADAY-DIAS
                             MOVE GRTIME-DATE TO GRADAY-DATA
                             CALL "GRADAY1" USING PARAMETROS-GRADAY
                             MOVE GRADAY-DATA TO VENC-W.

       TESTA-FERIADO SECTION.
           ADD 1 TO I.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                IF GS-FERIADO(I) = VENC-W
                             MOVE 1 TO GRADAY-DIAS
                             MOVE VENC-W      TO GRADAY-DATA
                             CALL "GRADAY1" USING PARAMETROS-GRADAY
                             MOVE GRADAY-DATA TO VENC-W
                             PERFORM TESTA-SABADO-DOMINGO.

       MOSTRA-CALCULO SECTION.
           COMPUTE PM-W = VALOR-ACUMULADO / GS-VALOR-TOT-CHEQUE.
           MOVE PM-W                         TO GS-PM-TOT-CHEQUE


           MOVE ZEROS                        TO DIAS-30-W
           MOVE ZEROS                        TO DIAS-60-W
           MOVE ZEROS                        TO DIAS-90-W
           MOVE ZEROS                        TO DIAS-120-W
           MOVE ZEROS                        TO DIAS-150-W
           MOVE ZEROS                        TO DIAS-180-W
           MOVE ZEROS                        TO DIAS-210-W
           MOVE ZEROS                        TO DIAS-240-W
           MOVE ZEROS                        TO DIAS-270-W
           MOVE ZEROS                        TO DIAS-300-W
           MOVE ZEROS                        TO DIAS-330-W
           MOVE ZEROS                        TO DIAS-360-W
           MOVE ZEROS                        TO DIAS-390-W
           MOVE ZEROS                        TO DIAS-420-W
           MOVE ZEROS                        TO DIAS-450-W
           MOVE ZEROS                        TO DIAS-480-W
           MOVE ZEROS                        TO DIAS-510-W
           MOVE ZEROS                        TO DIAS-540-W

           MOVE ZEROS                        TO TAXA-30-W
           MOVE ZEROS                        TO TAXA-60-W
           MOVE ZEROS                        TO TAXA-90-W
           MOVE ZEROS                        TO TAXA-120-W
           MOVE ZEROS                        TO TAXA-150-W
           MOVE ZEROS                        TO TAXA-180-W
           MOVE ZEROS                        TO TAXA-210-W
           MOVE ZEROS                        TO TAXA-240-W
           MOVE ZEROS                        TO TAXA-270-W
           MOVE ZEROS                        TO TAXA-300-W
           MOVE ZEROS                        TO TAXA-330-W
           MOVE ZEROS                        TO TAXA-360-W
           MOVE ZEROS                        TO TAXA-390-W
           MOVE ZEROS                        TO TAXA-420-W
           MOVE ZEROS                        TO TAXA-450-W
           MOVE ZEROS                        TO TAXA-480-W
           MOVE ZEROS                        TO TAXA-510-W
           MOVE ZEROS                        TO TAXA-540-W


           IF PM-W > 540,99
              MOVE 30                        TO DIAS-30-W
                                                DIAS-60-W
                                                DIAS-90-W
                                                DIAS-120-W
                                                DIAS-150-W
                                                DIAS-180-W
                                                DIAS-210-W
                                                DIAS-240-W
                                                DIAS-270-W
                                                DIAS-300-W
                                                DIAS-330-W
                                                DIAS-360-W
                                                DIAS-390-W
                                                DIAS-420-W
                                                DIAS-450-W
                                                DIAS-480-W
                                                DIAS-510-W

              MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                TAXA-60-W
                                                TAXA-90-W
                                                TAXA-120-W
                                                TAXA-150-W
                                                TAXA-210-W
                                                TAXA-240-W
                                                TAXA-270-W
                                                TAXA-300-W
                                                TAXA-330-W
                                                TAXA-360-W
                                                TAXA-390-W
                                                TAXA-420-W
                                                TAXA-450-W
                                                TAXA-480-W
                                                TAXA-510-W

              COMPUTE DIAS-540-W = PM-W - 540
              MOVE DIAS-540-W                TO DIAS-TAXA

      *       PERFORM TABELA-TAXA THRU FIM-TABELA
      *       MOVE VLR-TAXA TO TAXA-180-W

              MOVE GS-TX-JURO-NEGOC          TO TAXA-540-W
            ELSE
              IF PM-W > 510,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W
                                                   DIAS-210-W
                                                   DIAS-240-W
                                                   DIAS-270-W
                                                   DIAS-300-W
                                                   DIAS-330-W
                                                   DIAS-360-W
                                                   DIAS-390-W
                                                   DIAS-420-W
                                                   DIAS-450-W
                                                   DIAS-480-W


                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W
                                                   TAXA-210-W
                                                   TAXA-240-W
                                                   TAXA-270-W
                                                   TAXA-300-W
                                                   TAXA-330-W
                                                   TAXA-360-W
                                                   TAXA-390-W
                                                   TAXA-420-W
                                                   TAXA-450-W
                                                   TAXA-480-W

              MOVE ZEROS                        TO TAXA-540-W

                 COMPUTE DIAS-510-W = PM-W - 510
                 MOVE DIAS-510-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-510-W
            ELSE
              IF PM-W > 480,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W
                                                   DIAS-210-W
                                                   DIAS-240-W
                                                   DIAS-270-W
                                                   DIAS-300-W
                                                   DIAS-330-W
                                                   DIAS-360-W
                                                   DIAS-390-W
                                                   DIAS-420-W
                                                   DIAS-450-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W
                                                   TAXA-210-W
                                                   TAXA-240-W
                                                   TAXA-270-W
                                                   TAXA-300-W
                                                   TAXA-330-W
                                                   TAXA-360-W
                                                   TAXA-390-W
                                                   TAXA-420-W
                                                   TAXA-450-W
                                                   TAXA-480-W

                 COMPUTE DIAS-480-W = PM-W - 480
                 MOVE DIAS-480-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-480-W
            ELSE
              IF PM-W > 450,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W
                                                   DIAS-210-W
                                                   DIAS-240-W
                                                   DIAS-270-W
                                                   DIAS-300-W
                                                   DIAS-330-W
                                                   DIAS-360-W
                                                   DIAS-390-W
                                                   DIAS-420-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W
                                                   TAXA-210-W
                                                   TAXA-240-W
                                                   TAXA-270-W
                                                   TAXA-300-W
                                                   TAXA-330-W
                                                   TAXA-360-W
                                                   TAXA-390-W
                                                   TAXA-420-W

                 COMPUTE DIAS-450-W = PM-W - 450
                 MOVE DIAS-450-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-450-W
            ELSE
              IF PM-W > 420,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W
                                                   DIAS-210-W
                                                   DIAS-240-W
                                                   DIAS-270-W
                                                   DIAS-300-W
                                                   DIAS-330-W
                                                   DIAS-360-W
                                                   DIAS-390-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W
                                                   TAXA-210-W
                                                   TAXA-240-W
                                                   TAXA-270-W
                                                   TAXA-300-W
                                                   TAXA-330-W
                                                   TAXA-360-W
                                                   TAXA-390-W

                 COMPUTE DIAS-420-W = PM-W - 420
                 MOVE DIAS-420-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-420-W
            ELSE
              IF PM-W > 390,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W
                                                   DIAS-210-W
                                                   DIAS-240-W
                                                   DIAS-270-W
                                                   DIAS-300-W
                                                   DIAS-330-W
                                                   DIAS-360-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W
                                                   TAXA-210-W
                                                   TAXA-240-W
                                                   TAXA-270-W
                                                   TAXA-300-W
                                                   TAXA-330-W
                                                   TAXA-360-W

                 COMPUTE DIAS-390-W = PM-W - 390
                 MOVE DIAS-390-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-390-W
            ELSE
              IF PM-W > 360,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W
                                                   DIAS-210-W
                                                   DIAS-240-W
                                                   DIAS-270-W
                                                   DIAS-300-W
                                                   DIAS-330-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W
                                                   TAXA-210-W
                                                   TAXA-240-W
                                                   TAXA-270-W
                                                   TAXA-300-W
                                                   TAXA-330-W

                 COMPUTE DIAS-360-W = PM-W - 360
                 MOVE DIAS-360-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-360-W
            ELSE
              IF PM-W > 330,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W
                                                   DIAS-210-W
                                                   DIAS-240-W
                                                   DIAS-270-W
                                                   DIAS-300-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W
                                                   TAXA-210-W
                                                   TAXA-240-W
                                                   TAXA-270-W
                                                   TAXA-300-W
                                                   TAXA-330-W

                 COMPUTE DIAS-330-W = PM-W - 330
                 MOVE DIAS-330-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-330-W
            ELSE
              IF PM-W > 300,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W
                                                   DIAS-210-W
                                                   DIAS-240-W
                                                   DIAS-270-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W
                                                   TAXA-210-W
                                                   TAXA-240-W
                                                   TAXA-270-W

                 COMPUTE DIAS-300-W = PM-W - 300
                 MOVE DIAS-300-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-300-W
            ELSE
              IF PM-W > 270,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W
                                                   DIAS-210-W
                                                   DIAS-240-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W
                                                   TAXA-210-W
                                                   TAXA-240-W

                 COMPUTE DIAS-270-W = PM-W - 270
                 MOVE DIAS-270-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-270-W
            ELSE
              IF PM-W > 240,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W
                                                   DIAS-210-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W
                                                   TAXA-210-W

                 COMPUTE DIAS-240-W = PM-W - 240
                 MOVE DIAS-240-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-240-W
            ELSE
              IF PM-W > 210,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W
                                                   DIAS-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W
                                                   TAXA-180-W

                 COMPUTE DIAS-210-W = PM-W - 210
                 MOVE DIAS-210-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-210-W
            ELSE
              IF PM-W > 180,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W

                 COMPUTE DIAS-180-W = PM-W - 180
                 MOVE DIAS-180-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-180-W
            ELSE
              IF PM-W > 150,99
                 MOVE 30                        TO DIAS-30-W
                                                   DIAS-60-W
                                                   DIAS-90-W
                                                   DIAS-120-W
                                                   DIAS-150-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-30-W
                                                   TAXA-60-W
                                                   TAXA-90-W
                                                   TAXA-120-W
                                                   TAXA-150-W

                 COMPUTE DIAS-180-W = PM-W - 150
                 MOVE DIAS-180-W                TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA
      *          MOVE VLR-TAXA TO TAXA-180-W

                 MOVE GS-TX-JURO-NEGOC          TO TAXA-180-W
           ELSE
              IF PM-W > 120,99
                 MOVE 30                     TO DIAS-30-W
                                                DIAS-60-W
                                                DIAS-90-W
                                                DIAS-120-W

                 MOVE GS-TX-JURO-NEGOC       TO TAXA-30-W
                                                TAXA-60-W
                                                TAXA-90-W
                                                TAXA-120-W

                 COMPUTE DIAS-150-W = PM-W - 120

                 MOVE DIAS-150-W             TO DIAS-TAXA

      *          PERFORM TABELA-TAXA THRU FIM-TABELA

                 MOVE VLR-TAXA(DIAS-TAXA)    TO TAXA-150-W
            ELSE
                 IF PM-W > 90,99
                    MOVE 30                  TO DIAS-30-W
                                                DIAS-60-W
                                                DIAS-90-W

                    MOVE GS-TX-JURO-NEGOC    TO TAXA-30-W
                                                TAXA-60-W
                                                TAXA-90-W

                    COMPUTE DIAS-120-W = PM-W - 90
                    MOVE DIAS-120-W          TO DIAS-TAXA

      *             PERFORM TABELA-TAXA THRU FIM-TABELA

                    MOVE VLR-TAXA(DIAS-TAXA) TO TAXA-120-W
             ELSE
                 IF PM-W > 60,99
                    MOVE 30    TO DIAS-30-W  DIAS-60-W
                    MOVE GS-TX-JURO-NEGOC TO TAXA-30-W  TAXA-60-W
                    COMPUTE DIAS-90-W = PM-W - 60
                    MOVE DIAS-90-W TO DIAS-TAXA
      *              PERFORM TABELA-TAXA THRU FIM-TABELA
                    MOVE VLR-TAXA(DIAS-TAXA) TO TAXA-90-W
             ELSE
                IF PM-W > 30,99
                   MOVE 30               TO DIAS-30-W
                   MOVE GS-TX-JURO-NEGOC TO TAXA-30-W
                   COMPUTE DIAS-60-W = PM-W - 30
                   MOVE DIAS-60-W TO DIAS-TAXA
      *            PERFORM TABELA-TAXA THRU FIM-TABELA
                   MOVE VLR-TAXA(DIAS-TAXA) TO TAXA-60-W
             ELSE
                   MOVE PM-W  TO DIAS-30-W  DIAS-TAXA
                   COMPUTE TAXA-30-W = (GS-TX-JURO-NEGOC / 30)
                                       * PM-W.
      *                    PERFORM TABELA-TAXA THRU FIM-TABELA
      *                   MOVE VLR-TAXA(DIAS-TAXA) TO TAXA-30-W.

           COMPUTE JURO-30-W = GS-VALOR-TOT-CHEQUE * (TAXA-30-W / 100).
           COMPUTE SALDO-30-W = GS-VALOR-TOT-CHEQUE - JURO-30-W.

           COMPUTE JURO-60-W = SALDO-30-W * (TAXA-60-W / 100).
           COMPUTE JURO-60-W = (JURO-60-W / 30) * DIAS-60-W.
           COMPUTE SALDO-60-W = SALDO-30-W - JURO-60-W.

           COMPUTE JURO-90-W = SALDO-60-W * (TAXA-90-W / 100).
           COMPUTE JURO-90-W = (JURO-90-W / 30) * DIAS-90-W.
           COMPUTE SALDO-90-W = SALDO-60-W - JURO-90-W.

           COMPUTE JURO-120-W = SALDO-90-W * (TAXA-120-W / 100).
           COMPUTE JURO-120-W = (JURO-120-W / 30) * DIAS-120-W.
           COMPUTE SALDO-120-W = SALDO-90-W - JURO-120-W.

           COMPUTE JURO-150-W = SALDO-120-W * (TAXA-150-W / 100).
           COMPUTE JURO-150-W = (JURO-150-W / 30) * DIAS-150-W.
           COMPUTE SALDO-150-W = SALDO-120-W - JURO-150-W.

           COMPUTE JURO-180-W = SALDO-150-W * (TAXA-180-W / 100).
           COMPUTE JURO-180-W = (JURO-180-W / 30) * DIAS-180-W.
           COMPUTE SALDO-180-W = SALDO-150-W - JURO-180-W.

           COMPUTE JURO-210-W = SALDO-180-W * (TAXA-210-W / 100).
           COMPUTE JURO-210-W = (JURO-210-W / 30) * DIAS-210-W.
           COMPUTE SALDO-210-W = SALDO-180-W - JURO-210-W.

           COMPUTE JURO-240-W = SALDO-210-W * (TAXA-240-W / 100).
           COMPUTE JURO-240-W = (JURO-240-W / 30) * DIAS-240-W.
           COMPUTE SALDO-240-W = SALDO-210-W - JURO-240-W.

           COMPUTE JURO-270-W = SALDO-240-W * (TAXA-270-W / 100).
           COMPUTE JURO-270-W = (JURO-270-W / 30) * DIAS-270-W.
           COMPUTE SALDO-270-W = SALDO-240-W - JURO-270-W.

           COMPUTE JURO-300-W = SALDO-270-W * (TAXA-300-W / 100).
           COMPUTE JURO-300-W = (JURO-300-W / 30) * DIAS-300-W.
           COMPUTE SALDO-300-W = SALDO-270-W - JURO-300-W.

           COMPUTE JURO-330-W = SALDO-300-W * (TAXA-330-W / 100).
           COMPUTE JURO-330-W = (JURO-330-W / 30) * DIAS-330-W.
           COMPUTE SALDO-330-W = SALDO-300-W - JURO-330-W.

           COMPUTE JURO-360-W = SALDO-330-W * (TAXA-360-W / 100).
           COMPUTE JURO-360-W = (JURO-360-W / 30) * DIAS-360-W.
           COMPUTE SALDO-360-W = SALDO-330-W - JURO-360-W.

           COMPUTE JURO-390-W = SALDO-360-W * (TAXA-390-W / 100).
           COMPUTE JURO-390-W = (JURO-390-W / 30) * DIAS-390-W.
           COMPUTE SALDO-390-W = SALDO-360-W - JURO-390-W.

           COMPUTE JURO-420-W = SALDO-390-W * (TAXA-420-W / 100).
           COMPUTE JURO-420-W = (JURO-420-W / 30) * DIAS-420-W.
           COMPUTE SALDO-420-W = SALDO-390-W - JURO-420-W.

           COMPUTE JURO-450-W = SALDO-420-W * (TAXA-450-W / 100).
           COMPUTE JURO-450-W = (JURO-450-W / 30) * DIAS-450-W.
           COMPUTE SALDO-450-W = SALDO-420-W - JURO-450-W.

           COMPUTE JURO-480-W = SALDO-450-W * (TAXA-480-W / 100).
           COMPUTE JURO-480-W = (JURO-480-W / 30) * DIAS-480-W.
           COMPUTE SALDO-480-W = SALDO-450-W - JURO-480-W.

           COMPUTE JURO-510-W = SALDO-480-W * (TAXA-510-W / 100).
           COMPUTE JURO-510-W = (JURO-510-W / 30) * DIAS-510-W.
           COMPUTE SALDO-510-W = SALDO-480-W - JURO-510-W.

           COMPUTE JURO-540-W = SALDO-510-W * (TAXA-540-W / 100).
           COMPUTE JURO-540-W = (JURO-540-W / 30) * DIAS-540-W.
           COMPUTE SALDO-540-W = SALDO-510-W - JURO-540-W.

           MOVE "ATE  30 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-30-W TO DIAS-E.   MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-30-W TO TAXA-E.   MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-30-W TO VALOR-E.  MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-30-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE  60 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-60-W TO DIAS-E.   MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-60-W TO TAXA-E.   MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-60-W TO VALOR-E.  MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-60-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-60-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                 PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE  90 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-90-W TO DIAS-E.   MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-90-W TO TAXA-E.   MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-90-W TO VALOR-E.  MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-90-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-90-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                 PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 120 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-120-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-120-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-120-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-120-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-120-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 150 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-150-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-150-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-150-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-150-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-150-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 180 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-180-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-180-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-180-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-180-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-180-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 210 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-210-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-210-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-210-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-210-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-210-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 240 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-240-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-240-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-240-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-240-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-240-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 270 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-270-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-270-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-270-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-270-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-270-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 300 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-300-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-300-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-300-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-300-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-300-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 330 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-330-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-330-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-330-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-330-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-330-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 360 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-360-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-360-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-360-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-360-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-360-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 390 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-390-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-390-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-390-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-390-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-390-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 420 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-420-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-420-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-420-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-420-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-420-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 450 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-450-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-450-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-450-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-450-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-450-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 480 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-480-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-480-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-480-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-480-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-480-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "ATE 510 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-510-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-510-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-510-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-510-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-510-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.

           MOVE "+ 540 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-540-W TO DIAS-E.  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-540-W TO TAXA-E.  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-540-W TO VALOR-E. MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-540-W TO VALOR-E.MOVE VALOR-E TO GS-LINDET(92: 13)
           IF DIAS-540-W <> ZEROS MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       CARREGA-LISTA-CHEQUE SECTION.
           MOVE ZEROS TO VENC-WK BANCO-WK NR-CHEQUE-WK.
           START WORK KEY IS NOT < ALT-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                 MOVE VENC-WK        TO DATA-INV
                 CALL "GRIDAT1" USING DATA-INV
                 MOVE DATA-INV       TO DATA-E
                 MOVE DATA-E         TO GS-LINDET1(1: 11)
                 MOVE DIAS-WK        TO GS-LINDET1(12: 5)
                 MOVE NR-CHEQUE-WK   TO GS-LINDET1(17: 9)
                 MOVE BANCO-WK       TO GS-LINDET1(26: 6)
                 MOVE DATA-MOVTO-WK  TO DATA-MOVTO-CH10
                 MOVE SEQ-WK         TO SEQ-CH10
                 READ CHD010 INVALID KEY INITIALIZE REG-CHD010
                                         PERFORM ERRO-RELACAO-CHEQUE
                 END-READ
                 IF SITUACAO-CH10 <> 0 PERFORM ERRO-RELACAO-CHEQUE
                 END-IF
                 MOVE CIDADE-CH10    TO GS-LINDET1(32: 21)
                 MOVE NOME-CH10      TO GS-LINDET1(53: 31)
                 MOVE VALOR-CH10     TO VALOR-E
                 MOVE VALOR-E        TO GS-LINDET1(84: 13)
                 MOVE DATA-MOVTO-WK  TO GS-LINDET1(100: 8)
                 MOVE SEQ-WK         TO GS-LINDET1(109: 4)
                 MOVE "INSERE-LIST2" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
             END-READ
           END-PERFORM.
       ERRO-RELACAO-CHEQUE SECTION.
           MOVE DATA-VENCTO-CH10  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-TEXTO2(1: 11)
           MOVE NR-CHEQUE-CH10    TO GS-TEXTO2(12: 8)
           MOVE VALOR-CH10        TO VALOR-E
           MOVE VALOR-E           TO GS-TEXTO2(20: 13)
           MOVE "ERRO-RELACAO-CH" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *---------------------------------------------------------------
       VERIFICA-SENHA-PORTADOR SECTION.
           MOVE 0             TO GS-AUTORIZADO.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "SENHA02"     TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                MOVE 2 TO GS-AUTORIZADO
             NOT INVALID KEY
      *        MOVE GS-SENHA TO SENHA-W1
               MOVE SENHA-W  TO AUX-SENHA
               IF GS-SENHA <> AUX-SENHA
      *        IF SENHA-W1 <> SENHA-W
                  MOVE 1 TO GS-AUTORIZADO
               ELSE CONTINUE
           END-READ.
      *---------------------------------------------------------------
       TRANSFERE-PORTADOR SECTION.
           CLOSE    CHD010
      *             CRD200
      *             CRD201

           OPEN I-O CHD010
      *             CRD200
      *             CRD201

           MOVE ZEROS TO DATA-MOVTO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END
                 MOVE "10" TO ST-WORK
             NOT AT END
                 MOVE DATA-MOVTO-WK  TO DATA-MOVTO-CH10
                 MOVE SEQ-WK         TO SEQ-CH10
                 READ CHD010 INVALID KEY
                      CONTINUE
                 NOT INVALID KEY
      *               PERFORM GRAVAR-ANOTACAO
                      MOVE GS-PORTADOR-TRANSF TO PORTADOR-CH10
                      REWRITE REG-CHD010
                      END-REWRITE
                      MOVE DATA-MOVTO-CH10 TO GS-TEXTO
                      MOVE "FEEDBACK-TRANSF-PORTADOR" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
             END-READ
           END-PERFORM.

           CLOSE CHD010
      *          CRD200
      *          CRD201

           OPEN INPUT CHD010.
      *               CRD200
      *               CRD201.
      *---------------------------------------------------------------
       GRAVAR-ANOTACAO SECTION.
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END
                  MOVE "10" TO ST-CRD200
             NOT AT END
                  IF COD-COMPL-CR200 <> COD-COMPL-CH10
                     MOVE "10" TO ST-CRD200
                 ELSE
                     MOVE SEQ-CR200 TO ULT-SEQ
                     CONTINUE
             END-READ
           END-PERFORM.
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ        TO SEQ-CR200.
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200.
           MOVE ZEROS          TO DATA-RETORNO-CR200
           MOVE USUARIO-W      TO USUARIO-CR200
           MOVE DATA-DIA-I     TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200             TO SEQ-CR201.
           MOVE COD-COMPL-CH10        TO COD-COMPL-CR201.
           MOVE "TRANSF.PORTADOR-CHEQUE: XXXXXXXXXX - 01-XXXXXXXXXXXXXXX
      -    "X P/ 99-XXXXXXXXXXXXXXXX" TO ANOTACAO-CR201.
           MOVE NR-CHEQUE-CH10        TO ANOTACAO-CR201(25: 11)
           MOVE PORTADOR-CH10         TO ANOTACAO-CR201(38: 4) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT             TO ANOTACAO-CR201(43: 16)
           MOVE GS-PORTADOR-TRANSF    TO ANOTACAO-CR201(63: 4) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT           TO ANOTACAO-CR201(69: 16)
           MOVE ZEROS TO ST-CRD201.
           MOVE 1              TO SUBSEQ-CR201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
               ADD 1 TO SUBSEQ-CR201
               CONTINUE
              NOT INVALID KEY
                MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.

           MOVE SEQ-CR200             TO SEQ-CR201.
           MOVE COD-COMPL-CH10        TO COD-COMPL-CR201.
           MOVE "ATRAVES DA DEFLACAO DE CHEQUES" TO ANOTACAO-CR201
           MOVE ZEROS                 TO ST-CRD201.
           MOVE 1                     TO SUBSEQ-CR201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
               ADD 1 TO SUBSEQ-CR201
               CONTINUE
              NOT INVALID KEY
                MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.


      *---------------------------------------------------------------
       GRAVA-CALCULO SECTION.
           CLOSE CHD040 CHD041
           OPEN I-O CHD040 CHD041
           MOVE GS-NOME TO NOME-40 NOME-41.
           START CHD040 KEY IS = ALT-40 INVALID KEY
               START CHD041 KEY IS = NOME-41 INVALID KEY
                     PERFORM CONT-GRAVA-CALCULO
                 NOT INVALID KEY
                     MOVE "SHOW-MBOX3"  TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
      *              NOME DE ARQUIVO JÁ EXISTENTE
               END-START
             NOT INVALID KEY
                     MOVE "SHOW-MBOX3"  TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
           END-START.

           CLOSE CHD040 CHD041
           OPEN INPUT CHD040 CHD041.
       CONT-GRAVA-CALCULO SECTION.
           MOVE GS-CARTEIRA (1)     TO CARTEIRA-41 (1).
           MOVE GS-CARTEIRA (2)     TO CARTEIRA-41 (2).
           MOVE GS-CARTEIRA (3)     TO CARTEIRA-41 (3).

           MOVE DATA-BASE-I         TO DATA-BASE-41.

           MOVE GS-TX-JURO-NEGOC    TO TAXA-JUROS-41.

           MOVE VENCTO-INI          TO DATA-INI-41.
           MOVE VENCTO-FIM          TO DATA-FIM-41.

           MOVE GS-DIAS-INI         TO DIAS-INI-41
           MOVE GS-DIAS-FIM         TO DIAS-FIM-41

           MOVE GS-FERIADO (1)      TO FERIADOS-41(1).
           MOVE GS-FERIADO (2)      TO FERIADOS-41(2).
           MOVE GS-FERIADO (3)      TO FERIADOS-41(3).
           MOVE GS-FERIADO (4)      TO FERIADOS-41(4).
           MOVE GS-FERIADO (5)      TO FERIADOS-41(5).
           MOVE GS-FERIADO (6)      TO FERIADOS-41(6).
           MOVE GS-FERIADO (7)      TO FERIADOS-41(7).
           MOVE GS-FERIADO (8)      TO FERIADOS-41(8).
           MOVE GS-FERIADO (9)      TO FERIADOS-41(9).
           MOVE GS-FERIADO (10)     TO FERIADOS-41(10).

           MOVE GS-QTDE-TOT-CHEQUE  TO QTDE-CHEQUES-41.
           MOVE GS-VALOR-TOT-CHEQUE TO VLR-BRUTO-41.
           MOVE GS-PM-TOT-CHEQUE    TO PM-41.

           MOVE DIAS-30-W           TO DIAS-30-41.
           MOVE TAXA-30-W           TO TAXA-30-41.
           MOVE JURO-30-W           TO JURO-30-41.
           MOVE SALDO-30-W          TO SALDO-30-41.

           MOVE DIAS-60-W           TO DIAS-60-41.
           MOVE TAXA-60-W           TO TAXA-60-41.
           MOVE JURO-60-W           TO JURO-60-41.
           MOVE SALDO-60-W          TO SALDO-60-41.

           MOVE DIAS-90-W           TO DIAS-90-41.
           MOVE TAXA-90-W           TO TAXA-90-41.
           MOVE JURO-90-W           TO JURO-90-41.
           MOVE SALDO-90-W          TO SALDO-90-41.

           MOVE DIAS-120-W          TO DIAS-120-41.
           MOVE TAXA-120-W          TO TAXA-120-41.
           MOVE JURO-120-W          TO JURO-120-41.
           MOVE SALDO-120-W         TO SALDO-120-41.

           MOVE DIAS-150-W          TO DIAS-150-41.
           MOVE TAXA-150-W          TO TAXA-150-41.
           MOVE JURO-150-W          TO JURO-150-41.
           MOVE SALDO-150-W         TO SALDO-150-41.

           MOVE DIAS-180-W          TO DIAS-180-41.
           MOVE TAXA-180-W          TO TAXA-180-41.
           MOVE JURO-180-W          TO JURO-180-41.
           MOVE SALDO-180-W         TO SALDO-180-41.

           MOVE DIAS-210-W          TO DIAS-210-41.
           MOVE TAXA-210-W          TO TAXA-210-41.
           MOVE JURO-210-W          TO JURO-210-41.
           MOVE SALDO-210-W         TO SALDO-210-41.

           MOVE DIAS-240-W          TO DIAS-240-41.
           MOVE TAXA-240-W          TO TAXA-240-41.
           MOVE JURO-240-W          TO JURO-240-41.
           MOVE SALDO-240-W         TO SALDO-240-41.

           MOVE DIAS-270-W          TO DIAS-270-41.
           MOVE TAXA-270-W          TO TAXA-270-41.
           MOVE JURO-270-W          TO JURO-270-41.
           MOVE SALDO-270-W         TO SALDO-270-41.

           MOVE DIAS-300-W          TO DIAS-300-41.
           MOVE TAXA-300-W          TO TAXA-300-41.
           MOVE JURO-300-W          TO JURO-300-41.
           MOVE SALDO-300-W         TO SALDO-300-41.

           MOVE DIAS-330-W          TO DIAS-330-41.
           MOVE TAXA-330-W          TO TAXA-330-41.
           MOVE JURO-330-W          TO JURO-330-41.
           MOVE SALDO-330-W         TO SALDO-330-41.

           MOVE DIAS-360-W          TO DIAS-360-41.
           MOVE TAXA-360-W          TO TAXA-360-41.
           MOVE JURO-360-W          TO JURO-360-41.
           MOVE SALDO-360-W         TO SALDO-360-41.

           MOVE DIAS-390-W          TO DIAS-390-41.
           MOVE TAXA-390-W          TO TAXA-390-41.
           MOVE JURO-390-W          TO JURO-390-41.
           MOVE SALDO-390-W         TO SALDO-390-41.

           MOVE DIAS-420-W          TO DIAS-420-41.
           MOVE TAXA-420-W          TO TAXA-420-41.
           MOVE JURO-420-W          TO JURO-420-41.
           MOVE SALDO-420-W         TO SALDO-420-41.

           MOVE DIAS-450-W          TO DIAS-450-41.
           MOVE TAXA-450-W          TO TAXA-450-41.
           MOVE JURO-450-W          TO JURO-450-41.
           MOVE SALDO-450-W         TO SALDO-450-41.

           MOVE DIAS-480-W          TO DIAS-480-41.
           MOVE TAXA-480-W          TO TAXA-480-41.
           MOVE JURO-480-W          TO JURO-480-41.
           MOVE SALDO-480-W         TO SALDO-480-41.

           MOVE DIAS-510-W          TO DIAS-510-41.
           MOVE TAXA-510-W          TO TAXA-510-41.
           MOVE JURO-510-W          TO JURO-510-41.
           MOVE SALDO-510-W         TO SALDO-510-41.

           MOVE DIAS-540-W          TO DIAS-540-41.
           MOVE TAXA-540-W          TO TAXA-540-41.
           MOVE JURO-540-W          TO JURO-540-41.
           MOVE SALDO-540-W         TO SALDO-540-41.

           MOVE GS-PORTADOR-TRANSF  TO PORTADOR-DESTINO-41

           WRITE REG-CHD041.
           MOVE ZEROS TO SEQ-40.
           MOVE ZEROS TO DATA-MOVTO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      ADD 1 TO SEQ-40
                      MOVE GS-NOME      TO NOME-40
                      MOVE VENC-WK      TO VENC-40
                      MOVE BANCO-WK     TO BANCO-40
                      MOVE NR-CHEQUE-WK TO NR-CHEQUE-40
                      MOVE VENC1-WK     TO VENC1-40
                      MOVE DIAS-WK      TO DIAS-40
                      MOVE VALOR-WK     TO VALOR-40
                      MOVE DATA-MOVTO-WK TO DATA-MOVTO-40
                      MOVE SEQ-WK        TO SEQ-CHEQUE-40
                      WRITE REG-CHD040
                      END-WRITE
                 END-READ
           END-PERFORM.
      *---------------------------------------------------------
       IMPRIME-LISTA-CHEQUE SECTION.

           COPY CONDENSA.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO1.
           MOVE ZEROS TO VENC-WK BANCO-WK NR-CHEQUE-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      MOVE VENC-WK        TO DATA-INV
                      CALL "GRIDAT1" USING DATA-INV
                      MOVE DATA-INV       TO DATA-E
                      MOVE DATA-E         TO LINDET1-REL(1: 11)
                      MOVE DIAS-WK        TO LINDET1-REL(12: 5)
                      MOVE NR-CHEQUE-WK   TO LINDET1-REL(17: 9)
                      MOVE BANCO-WK       TO LINDET1-REL(26: 6)
                      MOVE DATA-MOVTO-WK  TO DATA-MOVTO-CH10
                      MOVE SEQ-WK         TO SEQ-CH10
                      READ CHD010 INVALID KEY
                           INITIALIZE REG-CHD010
                      END-READ
                      MOVE CIDADE-CH10    TO LINDET1-REL(32: 21)
                      MOVE NOME-CH10      TO LINDET1-REL(53: 31)
                      MOVE VALOR-CH10     TO VALOR-E
                      MOVE VALOR-E        TO LINDET1-REL(84: 13)
                      WRITE REG-RELAT FROM LINDET1
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO1
                      END-IF
                 END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO1 SECTION.
           IF LIN = ZEROS
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB10 AFTER 2.
           WRITE REG-RELAT FROM CAB11.
           WRITE REG-RELAT FROM CAB12.
           WRITE REG-RELAT FROM CAB11.
           MOVE 6 TO LIN.
      *-------------------------------------------------------
       EXCLUI-CHEQUE SECTION.
           MOVE GS-LINDET1(100: 8) TO DATA-MOVTO-CH10.
           MOVE GS-LINDET1(109: 4) TO SEQ-CH10.
           READ CHD010 INVALID KEY
                PERFORM ERRO-RELACAO-CHEQUE
           NOT INVALID KEY
                 MOVE DATA-MOVTO-CH10 TO DATA-MOVTO-WK
                 MOVE SEQ-CH10        TO SEQ-WK
                 READ WORK INVALID KEY
                      PERFORM ERRO-RELACAO-CHEQUE
                 NOT INVALID KEY
                      SUBTRACT VALOR-WK FROM GS-VALOR-TOT-CHEQUE
                      SUBTRACT 1        FROM GS-QTDE-TOT-CHEQUE
                      COMPUTE VALOR-ACUMULADO = VALOR-ACUMULADO -
                            (DIAS-WK * VALOR-WK)
                      DELETE WORK
                      END-DELETE
                 END-READ
           END-READ.
      *------------------------------------------------------------
       CONSULTAR-DADOS SECTION.
           CLOSE WORK.  OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE GS-NOME  TO NOME-40.
           MOVE ZEROS    TO SEQ-40.
           START CHD040 KEY IS NOT < CHAVE-40 INVALID KEY
                 MOVE "10" TO ST-CHD040.
           PERFORM UNTIL ST-CHD040 = "10"
              READ CHD040 NEXT RECORD AT END
                   MOVE "10" TO ST-CHD040
              NOT AT END
                  IF NOME-40 <>  GS-NOME
                     MOVE "10" TO ST-CHD040
                  ELSE
                     MOVE VENC-40          TO VENC-WK
                     MOVE BANCO-40         TO BANCO-WK
                     MOVE NR-CHEQUE-40     TO NR-CHEQUE-WK
                     MOVE VALOR-40         TO VALOR-WK
                     MOVE VENC1-40         TO VENC1-WK
                     MOVE DIAS-40          TO DIAS-WK
                     MOVE DATA-MOVTO-40    TO DATA-MOVTO-WK
                     MOVE SEQ-CHEQUE-40    TO SEQ-WK
                     WRITE REG-WORK
                     END-WRITE
              END-READ
           END-PERFORM.
           MOVE GS-NOME      TO NOME-41.
           READ CHD041 INVALID KEY
                      MOVE "SHOW-MBOX4" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      INITIALIZE REG-CHD041.
           MOVE CARTEIRA-41(1)     TO GS-CARTEIRA(1) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(1).
           MOVE CARTEIRA-41(2)     TO GS-CARTEIRA(2) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(2).
           MOVE CARTEIRA-41(3)     TO GS-CARTEIRA(3) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(3).
           MOVE CARTEIRA-41(4)     TO GS-CARTEIRA(4) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(4).
           MOVE CARTEIRA-41(5)     TO GS-CARTEIRA(5) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(5).
           MOVE CARTEIRA-41(6)     TO GS-CARTEIRA(6) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(6).
           MOVE DATA-BASE-41       TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO GS-DATA-BASE-CALC
           MOVE TAXA-JUROS-41      TO GS-TX-JURO-NEGOC
           MOVE DATA-INI-41        TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO GS-VENCTO-INI
           MOVE DATA-FIM-41        TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO GS-VENCTO-FIM
           MOVE DIAS-INI-41        TO GS-DIAS-INI
           MOVE DIAS-FIM-41        TO GS-DIAS-FIM
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
             MOVE FERIADOS-41(I) TO GS-FERIADO(I)
           END-PERFORM.
           MOVE QTDE-CHEQUES-41    TO GS-QTDE-TOT-CHEQUE
           MOVE VLR-BRUTO-41       TO GS-VALOR-TOT-CHEQUE
           MOVE PM-41              TO GS-PM-TOT-CHEQUE
           MOVE PORTADOR-DESTINO-41 TO GS-PORTADOR-TRANSF PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT           TO GS-DESCR-PORTADOR-TRANSF
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           IF TAXA-30-41 <> ZEROS
           MOVE "ATE  30 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-30-41 TO DIAS-E  MOVE DIAS-E TO GS-LINDET(30: 20)
           MOVE TAXA-30-41 TO TAXA-E   MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-30-41 TO VALOR-E  MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-30-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-60-41 <> ZEROS
           MOVE "ATE  60 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-60-41 TO DIAS-E   MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-60-41 TO TAXA-E   MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-60-41 TO VALOR-E  MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-60-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-90-41 <> ZEROS
           MOVE "ATE  90 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-90-41 TO DIAS-E   MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-90-41 TO TAXA-E   MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-90-41 TO VALOR-E  MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-90-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-120-41 <> ZEROS
           MOVE "ATE 120 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-120-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-120-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-120-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-120-41 TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-150-41 <> ZEROS
           MOVE "ATE 150 DIAS"         TO GS-LINDET(1: 29)
           MOVE DIAS-150-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-150-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-150-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-150-41 TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-180-41 <> ZEROS
           MOVE "+ 150 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-180-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-180-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-180-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-180-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-210-41 <> ZEROS
           MOVE "+ 180 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-210-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-210-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-210-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-210-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-240-41 <> ZEROS
           MOVE "+ 210 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-240-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-240-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-240-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-240-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-270-41 <> ZEROS
           MOVE "+ 240 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-270-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-270-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-270-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-270-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-300-41 <> ZEROS
           MOVE "+ 270 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-300-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-300-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-300-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-300-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-330-41 <> ZEROS
           MOVE "+ 300 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-330-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-330-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-330-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-330-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-360-41 <> ZEROS
           MOVE "+ 330 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-360-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-360-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-360-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-360-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-390-41 <> ZEROS
           MOVE "+ 360 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-390-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-390-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-390-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-390-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-420-41 <> ZEROS
           MOVE "+ 390 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-420-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-420-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-420-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-420-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-450-41 <> ZEROS
           MOVE "+ 420 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-450-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-450-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-450-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-450-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-480-41 <> ZEROS
           MOVE "+ 450 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-480-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-480-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-480-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-480-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-510-41 <> ZEROS
           MOVE "+ 480 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-510-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-510-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-510-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-510-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           IF TAXA-540-41 <> ZEROS
           MOVE "+ 510 DIAS"           TO GS-LINDET(1: 29)
           MOVE DIAS-540-41 TO DIAS-E  MOVE DIAS-E  TO GS-LINDET(30: 20)
           MOVE TAXA-540-41 TO TAXA-E  MOVE TAXA-E  TO GS-LINDET(50: 15)
           MOVE JURO-540-41 TO VALOR-E MOVE VALOR-E TO GS-LINDET(65: 27)
           MOVE SALDO-540-W TO VALOR-E
           MOVE VALOR-E TO GS-LINDET(92: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

      *-----------------------------------------------------------
       IMPRIME-RESUMO SECTION.
           COPY CONDENSA.


           MOVE GS-CARTEIRA(1)    TO CARTEIRA1-REL PORTADOR.
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT TO NOME-CART1-REL.
           MOVE GS-CARTEIRA(2)    TO CARTEIRA2-REL PORTADOR.
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT TO NOME-CART2-REL.
           MOVE GS-CARTEIRA(3)    TO CARTEIRA3-REL PORTADOR.
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT TO NOME-CART3-REL.
           MOVE GS-CARTEIRA(4)    TO CARTEIRA4-REL PORTADOR.
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT TO NOME-CART4-REL.
           MOVE GS-CARTEIRA(5)    TO CARTEIRA5-REL PORTADOR.
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT TO NOME-CART5-REL.
           MOVE GS-CARTEIRA(6)    TO CARTEIRA6-REL PORTADOR.
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT TO NOME-CART6-REL.
           MOVE GS-DATA-BASE-CALC   TO DATA-BASE-REL.
           MOVE GS-TX-JURO-NEGOC              TO TAXA-JUROS-REL.
           MOVE VLR-TAXA(1)       TO TAXA-JUROS-REAL-REL.

           MOVE GS-VENCTO-INI       TO VENCTO-INI-REL.
           MOVE GS-VENCTO-FIM       TO VENCTO-FIM-REL.
           MOVE GS-DIAS-INI          TO DIAS-INI-REL.
           MOVE GS-DIAS-FIM          TO DIAS-FIM-REL.
      *    MOVE GS-FERIADO(1)     TO FERIADOS-REL.
      *    MOVE GS-FERIADO(2)     TO FERIADO2-REL.
      *    MOVE GS-FERIADO(3)     TO FERIADO3-REL.
      *    MOVE GS-FERIADO(4)     TO FERIADO4-REL.
      *    MOVE GS-FERIADO(5)     TO FERIADO5-REL.
      *    MOVE GS-FERIADO(6)     TO FERIADO6-REL.
      *    MOVE GS-FERIADO(7)     TO FERIADO7-REL.
      *    MOVE GS-FERIADO(8)     TO FERIADO8-REL.
      *    MOVE GS-FERIADO(9)     TO FERIADO9-REL.
      *    MOVE GS-FERIADO(10)    TO FERIADO0-REL.
           MOVE GS-QTDE-TOT-CHEQUE   TO QUANT-CH-REL.
           MOVE GS-VALOR-TOT-CHEQUE  TO VLR-BRUTO-REL.
           MOVE GS-PM-TOT-CHEQUE  TO PM-REL.

           MOVE DIAS-30-W         TO DIAS-30-REL.
           MOVE TAXA-30-W         TO TAXA-30-REL.
           MOVE JURO-30-W         TO JURO-30-REL.
           MOVE SALDO-30-W        TO SALDO-30-REL.

           MOVE DIAS-60-W         TO DIAS-60-REL.
           MOVE TAXA-60-W         TO TAXA-60-REL.
           MOVE JURO-60-W         TO JURO-60-REL.
           MOVE SALDO-60-W        TO SALDO-60-REL.

           MOVE DIAS-90-W         TO DIAS-90-REL.
           MOVE TAXA-90-W         TO TAXA-90-REL.
           MOVE JURO-90-W         TO JURO-90-REL.
           MOVE SALDO-90-W        TO SALDO-90-REL.

           MOVE DIAS-120-W        TO DIAS-120-REL.
           MOVE TAXA-120-W        TO TAXA-120-REL.
           MOVE JURO-120-W        TO JURO-120-REL.
           MOVE SALDO-120-W       TO SALDO-120-REL.

           MOVE DIAS-150-W        TO DIAS-150-REL.
           MOVE TAXA-150-W        TO TAXA-150-REL.
           MOVE JURO-150-W        TO JURO-150-REL.
           MOVE SALDO-150-W       TO SALDO-150-REL.

           MOVE DIAS-180-W        TO DIAS-180-REL.
           MOVE TAXA-180-W        TO TAXA-180-REL.
           MOVE JURO-180-W        TO JURO-180-REL.
           MOVE SALDO-180-W       TO SALDO-180-REL.

           MOVE DIAS-210-W        TO DIAS-210-REL.
           MOVE TAXA-210-W        TO TAXA-210-REL.
           MOVE JURO-210-W        TO JURO-210-REL.
           MOVE SALDO-210-W       TO SALDO-210-REL.

           MOVE DIAS-240-W        TO DIAS-240-REL.
           MOVE TAXA-240-W        TO TAXA-240-REL.
           MOVE JURO-240-W        TO JURO-240-REL.
           MOVE SALDO-240-W       TO SALDO-240-REL.

           MOVE DIAS-270-W        TO DIAS-270-REL.
           MOVE TAXA-270-W        TO TAXA-270-REL.
           MOVE JURO-270-W        TO JURO-270-REL.
           MOVE SALDO-270-W       TO SALDO-270-REL.

           MOVE DIAS-300-W        TO DIAS-300-REL.
           MOVE TAXA-300-W        TO TAXA-300-REL.
           MOVE JURO-300-W        TO JURO-300-REL.
           MOVE SALDO-300-W       TO SALDO-300-REL.

           MOVE DIAS-330-W        TO DIAS-330-REL.
           MOVE TAXA-330-W        TO TAXA-330-REL.
           MOVE JURO-330-W        TO JURO-330-REL.
           MOVE SALDO-330-W       TO SALDO-330-REL.

           MOVE DIAS-360-W        TO DIAS-360-REL.
           MOVE TAXA-360-W        TO TAXA-360-REL.
           MOVE JURO-360-W        TO JURO-360-REL.
           MOVE SALDO-360-W       TO SALDO-360-REL.

           MOVE DIAS-390-W        TO DIAS-390-REL.
           MOVE TAXA-390-W        TO TAXA-390-REL.
           MOVE JURO-390-W        TO JURO-390-REL.
           MOVE SALDO-390-W       TO SALDO-390-REL.

           MOVE DIAS-420-W        TO DIAS-420-REL.
           MOVE TAXA-420-W        TO TAXA-420-REL.
           MOVE JURO-420-W        TO JURO-420-REL.
           MOVE SALDO-420-W       TO SALDO-420-REL.

           MOVE DIAS-450-W        TO DIAS-450-REL.
           MOVE TAXA-450-W        TO TAXA-450-REL.
           MOVE JURO-450-W        TO JURO-450-REL.
           MOVE SALDO-450-W       TO SALDO-450-REL.

           MOVE DIAS-480-W        TO DIAS-480-REL.
           MOVE TAXA-480-W        TO TAXA-480-REL.
           MOVE JURO-480-W        TO JURO-480-REL.
           MOVE SALDO-480-W       TO SALDO-480-REL.

           MOVE DIAS-510-W        TO DIAS-510-REL.
           MOVE TAXA-510-W        TO TAXA-510-REL.
           MOVE JURO-510-W        TO JURO-510-REL.
           MOVE SALDO-510-W       TO SALDO-510-REL.

           MOVE DIAS-540-W        TO DIAS-540-REL.
           MOVE TAXA-540-W        TO TAXA-540-REL.
           MOVE JURO-540-W        TO JURO-540-REL.
           MOVE SALDO-540-W       TO SALDO-540-REL.

           MOVE GS-NOME TO NOME-REL.
           WRITE REG-RELAT FROM CAB01.
           WRITE REG-RELAT FROM LINHA-01  AFTER 2.
           WRITE REG-RELAT FROM LINHA-02.
           WRITE REG-RELAT FROM LINHA-03  AFTER 2.
           WRITE REG-RELAT FROM LINHA-031 AFTER 2.
           WRITE REG-RELAT FROM LINHA-04  AFTER 2.
           WRITE REG-RELAT FROM LINHA-05  AFTER 2.
      *    WRITE REG-RELAT FROM LINHA-06  AFTER 2.
      *    WRITE REG-RELAT FROM LINHA-07  AFTER 2.
           WRITE REG-RELAT FROM LINHA-08  AFTER 2.
           WRITE REG-RELAT FROM LINHA-09  AFTER 2.
           WRITE REG-RELAT FROM LINHA-10  AFTER 2.
           IF DIAS-60-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-11 AFTER 2
              WRITE REG-RELAT FROM LINHA-12 AFTER 2.
           IF DIAS-90-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-13 AFTER 2
              WRITE REG-RELAT FROM LINHA-14 AFTER 2.
           IF DIAS-120-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-15 AFTER 2
              WRITE REG-RELAT FROM LINHA-16 AFTER 2.
           IF DIAS-150-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-17 AFTER 2
              WRITE REG-RELAT FROM LINHA-18 AFTER 2.
           IF DIAS-180-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-19 AFTER 2
              WRITE REG-RELAT FROM LINHA-20 AFTER 2.
           IF DIAS-210-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-21 AFTER 2
              WRITE REG-RELAT FROM LINHA-22 AFTER 2.
           IF DIAS-240-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-23 AFTER 2
              WRITE REG-RELAT FROM LINHA-24 AFTER 2.
           IF DIAS-270-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-25 AFTER 2
              WRITE REG-RELAT FROM LINHA-26 AFTER 2.
           IF DIAS-300-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-27 AFTER 2
              WRITE REG-RELAT FROM LINHA-28 AFTER 2.
           IF DIAS-330-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-29 AFTER 2
              WRITE REG-RELAT FROM LINHA-30 AFTER 2.
           IF DIAS-360-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-31 AFTER 2
              WRITE REG-RELAT FROM LINHA-32 AFTER 2.
           IF DIAS-390-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-33 AFTER 2
              WRITE REG-RELAT FROM LINHA-34 AFTER 2.
           IF DIAS-420-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-35 AFTER 2
              WRITE REG-RELAT FROM LINHA-36 AFTER 2.
           IF DIAS-450-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-37 AFTER 2
              WRITE REG-RELAT FROM LINHA-38 AFTER 2.
           IF DIAS-480-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-39 AFTER 2
              WRITE REG-RELAT FROM LINHA-40 AFTER 2.
           IF DIAS-510-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-41 AFTER 2
              WRITE REG-RELAT FROM LINHA-42 AFTER 2.
           IF DIAS-540-W <> ZEROS
              WRITE REG-RELAT FROM LINHA-43 AFTER 2
              WRITE REG-RELAT FROM LINHA-44 AFTER 2.

           COPY DESCONDENSA.

      *------------------------------------------------------------

      *TABELA-TAXA SECTION.
      *    IF DIAS-TAXA = 29 MOVE 5,0090 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 28 MOVE 5,0181 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 27 MOVE 5,0272 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 26 MOVE 5,0362 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 25 MOVE 5,0453 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 24 MOVE 5,0544 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 23 MOVE 5,0635 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 22 MOVE 5,0725 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 21 MOVE 5,0186 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 20 MOVE 5,0907 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 19 MOVE 5,0998 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 18 MOVE 5,1088 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 17 MOVE 5,1179 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 16 MOVE 5,1270 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 15 MOVE 5,1361 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 14 MOVE 5,1451 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 13 MOVE 5,1542 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 12 MOVE 5,1633 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 11 MOVE 5,1724 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 10 MOVE 5,1814 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 09 MOVE 5,1905 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 08 MOVE 5,1996 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 07 MOVE 5,2087 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 06 MOVE 5,2177 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 05 MOVE 5,2268 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 04 MOVE 5,2359 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 03 MOVE 5,2450 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 02 MOVE 5,2540 TO VLR-TAXA GO FIM-TABELA.
      *    IF DIAS-TAXA = 01 MOVE 5,2631 TO VLR-TAXA GO FIM-TABELA.
      *FIM-TABELA.

       CRIA-TABELA SECTION.
           COMPUTE INDICE1 = 100 - GS-TX-JURO-NEGOC.
           COMPUTE INDICE3 = GS-TX-JURO-NEGOC / INDICE1.
      *    MOVE INDICE1 TO INDICE1A.
      *    MOVE GS-TX-JURO-NEGOC TO TAXA-W.
           COMPUTE INDICE2 = ((INDICE3 * 100) - GS-TX-JURO-NEGOC).
           COMPUTE INDICE-SOMA = INDICE2 / 29.
           MOVE GS-TX-JURO-NEGOC        TO VLR-TAXA(30) VLR-TAXA1.
           PERFORM VARYING I FROM 29 BY -1 UNTIL I = ZEROS
             COMPUTE VLR-TAXA(I) ROUNDED = VLR-TAXA1 + INDICE-SOMA
             ADD INDICE-SOMA TO VLR-TAXA1
           END-PERFORM.
           MOVE VLR-TAXA(1) TO GS-TAXA-REAL.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CHP057" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

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
           move "CHP057"            to logacess-programa
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

           CLOSE CHD040 CHD041 CHD010 CAD004 CAD018 WORK CRD200 CRD201.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
