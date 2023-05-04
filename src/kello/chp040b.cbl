       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP040.
       DATE-WRITTEN. 22-08-2005
       AUTHOR. ALFREDO SAVIOLLI NETO.
      *FUNÇÃO: LANÇAMENTO DE CHEQUES DA DEFLAÇÃO DE CHEQUES MANUAL
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

           SELECT CHD041i ASSIGN TO PATH-CHD041i
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS CHAVE-41i = NOME-41i
                                            PARCELA-41i
                  WITH DUPLICATES
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CHD041i.
       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW018.
       COPY LOGACESS.FD.

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

       FD  CHD041i.
       01  REG-CHD041i.
           05  NOME-41i              PIC X(16).
           05  PARCELA-41i           PIC 9(03).
           05  DIAS-41i              PIC 9(3)V99.
           05  TAXA-41i              PIC 9V999999.
           05  JURO-41i              PIC 9(10)V99.
           05  SALDO-41i             PIC 9(10)V99.

       WORKING-STORAGE SECTION.
           COPY "CHP040.CPB".
           COPY "CHP040.CPY".
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
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD041             PIC XX       VALUE SPACES.
           05  ST-CHD041i            PIC XX       VALUE SPACES.
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

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).


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
       02  LINHA-06.
           05 FILLER                         PIC  X(014) VALUE
              "FERIADOS  (1) ".
           05 FERIADOS-REL BLANK WHEN ZEROS  PIC  99/99/9999.
           05 FILLER                         PIC  X(005) VALUE
              " (2) ".
           05 FERIADO2-REL BLANK WHEN ZEROS  PIC  99/99/9999.
           05 FILLER                         PIC  X(005) VALUE
              " (3) ".
           05 FERIADO3-REL BLANK WHEN ZEROS  PIC  99/99/9999.
           05 FILLER                         PIC  X(005) VALUE
              " (4) ".
           05 FERIADO4-REL BLANK WHEN ZEROS  PIC  99/99/9999.
           05 FILLER                         PIC  X(005) VALUE
              " (5) ".
           05 FERIADO5-REL BLANK WHEN ZEROS  PIC  99/99/9999.
       02  LINHA-07.
           05 FILLER                         PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(004) VALUE "(6) ".
           05 FERIADO6-REL BLANK WHEN ZEROS  PIC  99/99/9999.
           05 FILLER                         PIC  X(005) VALUE
              " (7) ".
           05 FERIADO7-REL BLANK WHEN ZEROS  PIC  99/99/9999.
           05 FILLER                         PIC  X(005) VALUE
              " (8) ".
           05 FERIADO8-REL BLANK WHEN ZEROS  PIC  99/99/9999.
           05 FILLER                         PIC  X(005) VALUE
              " (9) ".
           05 FERIADO9-REL BLANK WHEN ZEROS  PIC  99/99/9999.
           05 FILLER                         PIC  X(005) VALUE
              " (0) ".
           05 FERIADO0-REL BLANK WHEN ZEROS  PIC  99/99/9999.
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
           MOVE "CHD041"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD041.
           MOVE "CHD041i" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD041i.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CAD004 CAD018.
           ACCEPT VARIA-W FROM TIME.

           OPEN I-O CHD041 CHD041i.

           IF ST-CHD041 = "35" CLOSE CHD041   OPEN OUTPUT CHD041
                               CLOSE CHD041   OPEN I-O CHD041.

           IF ST-CHD041i = "35" CLOSE CHD041i   OPEN OUTPUT CHD041i
                                CLOSE CHD041i   OPEN I-O CHD041i.

           CLOSE      CHD041 CHD041i

           OPEN INPUT CHD041 CHD041i

           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD041 <> "00"
              MOVE "ERRO ABERTURA CHD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD041 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD041i <> "00"
              MOVE "ERRO ABERTURA CHD041i: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD041i TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CHP040"            to logacess-programa
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
               WHEN GS-VERIF-SENHA-PORT-TRUE
                    PERFORM VERIFICA-SENHA-PORTADOR
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LE-PORTADOR
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-GRAVAR-FLG-TRUE
                    PERFORM GRAVA-CALCULO
               WHEN GS-PEGAR-NOME-ARQUIVO-TRUE
                    PERFORM PEGAR-NOME-ARQUIVO
               WHEN GS-REGRAVAR-TRUE
                    PERFORM REGRAVAR-CHEQUE
               WHEN GS-CONSULTAR-DADOS-TRUE
                    PERFORM CONSULTAR-DADOS
               WHEN GS-CONSULTAR-GRAVACOES-TRUE
                    PERFORM CONSULTAR-GRAVACOES
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CONSULTAR-GRAVACOES SECTION.
           MOVE "LIMPAR-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-CHD041
           START CHD041 KEY IS NOT LESS NOME-41 INVALID KEY
                 MOVE "10" TO ST-CHD041.

           PERFORM UNTIL ST-CHD041 = "10"
                 READ CHD041 NEXT AT END
                      MOVE "10" TO ST-CHD041
                 NOT AT END
                      MOVE NOME-41         TO GS-LINDET
                      MOVE "INSERIR-LIST"  TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.

       CONSULTAR-DADOS SECTION.
           MOVE GS-NOME      TO NOME-41.
           READ CHD041 INVALID KEY
                MOVE "SHOW-MBOX4" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                INITIALIZE REG-CHD041.

           MOVE CARTEIRA-41(1)     TO GS-CARTEIRA(1) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(1).

           MOVE CARTEIRA-41(2)     TO GS-CARTEIRA(2) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(2).

           MOVE CARTEIRA-41(3)     TO GS-CARTEIRA(3) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES TO NOME-PORT.

           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(3).

           MOVE CARTEIRA-41(4)     TO GS-CARTEIRA(4) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES TO NOME-PORT.

           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(4).
           MOVE CARTEIRA-41(5)     TO GS-CARTEIRA(5) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES TO NOME-PORT.

           MOVE NOME-PORT          TO GS-DESCR-CARTEIRA(5).
           MOVE CARTEIRA-41(6)     TO GS-CARTEIRA(6) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES TO NOME-PORT.

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
           MOVE QTDE-CHEQUES-41     TO GS-QTDE-TOT-CHEQUE
           MOVE VLR-BRUTO-41        TO GS-VALOR-TOT-CHEQUE
           MOVE PM-41               TO GS-PM-TOT-CHEQUE
           MOVE PORTADOR-DESTINO-41 TO GS-PORTADOR-TRANSF PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT           TO GS-DESCR-PORTADOR-TRANSF
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE DIAS-30-41  TO GS-DIAS-30-41
           MOVE TAXA-30-41  TO GS-TAXA-30-41
           MOVE JURO-30-41  TO GS-JURO-30-41
           MOVE SALDO-30-41 TO GS-SALDO-30-41

           MOVE DIAS-60-41  TO GS-DIAS-60-41
           MOVE TAXA-60-41  TO GS-TAXA-60-41
           MOVE JURO-60-41  TO GS-JURO-60-41
           MOVE SALDO-60-41 TO GS-SALDO-60-41

           MOVE DIAS-90-41  TO GS-DIAS-90-41
           MOVE TAXA-90-41  TO GS-TAXA-90-41
           MOVE JURO-90-41  TO GS-JURO-90-41
           MOVE SALDO-90-41 TO GS-SALDO-90-41

           MOVE DIAS-120-41  TO GS-DIAS-120-41
           MOVE TAXA-120-41  TO GS-TAXA-120-41
           MOVE JURO-120-41  TO GS-JURO-120-41
           MOVE SALDO-120-41 TO GS-SALDO-120-41

           MOVE DIAS-150-41  TO GS-DIAS-150-41
           MOVE TAXA-150-41  TO GS-TAXA-150-41
           MOVE JURO-150-41  TO GS-JURO-150-41
           MOVE SALDO-150-41 TO GS-SALDO-150-41

           MOVE DIAS-180-41 TO GS-DIAS-180-41
           MOVE TAXA-180-41 TO GS-TAXA-180-41
           MOVE JURO-180-41 TO GS-JURO-180-41
           MOVE SALDO-180-W TO GS-SALDO-180-41



           .

       REGRAVAR-CHEQUE SECTION.
           CLOSE    CHD041
           OPEN I-O CHD041

           MOVE GS-NOME TO NOME-41
           READ CHD041 INVALID KEY
               MOVE "Arquivo Não Encontrada para a Regravação" TO
               MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE GS-DATA-BASE-CALC TO DATA-INV
               CALL "GRIDAT2" USING DATA-INV
               MOVE DATA-INV    TO DATA-BASE-I
               MOVE GS-VENCTO-INI TO DATA-INV
               CALL "GRIDAT2"   USING DATA-INV
               MOVE DATA-INV    TO VENCTO-INI
               MOVE GS-VENCTO-FIM TO DATA-INV
               CALL "GRIDAT2"   USING DATA-INV
               MOVE DATA-INV    TO VENCTO-FIM
               MOVE GS-CARTEIRA (1)     TO CARTEIRA-41 (1)
               MOVE GS-CARTEIRA (2)     TO CARTEIRA-41 (2)
               MOVE GS-CARTEIRA (3)     TO CARTEIRA-41 (3)
               MOVE GS-CARTEIRA (4)     TO CARTEIRA-41 (4)
               MOVE GS-CARTEIRA (5)     TO CARTEIRA-41 (5)
               MOVE GS-CARTEIRA (6)     TO CARTEIRA-41 (6)
               MOVE DATA-BASE-I         TO DATA-BASE-41
               MOVE GS-TX-JURO-NEGOC    TO TAXA-JUROS-41
               MOVE VENCTO-INI          TO DATA-INI-41
               MOVE VENCTO-FIM          TO DATA-FIM-41
               MOVE GS-DIAS-INI         TO DIAS-INI-41
               MOVE GS-DIAS-FIM         TO DIAS-FIM-41
               MOVE GS-FERIADO (1)      TO FERIADOS-41(1)
               MOVE GS-FERIADO (2)      TO FERIADOS-41(2)
               MOVE GS-FERIADO (3)      TO FERIADOS-41(3)
               MOVE GS-FERIADO (4)      TO FERIADOS-41(4)
               MOVE GS-FERIADO (5)      TO FERIADOS-41(5)
               MOVE GS-FERIADO (6)      TO FERIADOS-41(6)
               MOVE GS-FERIADO (7)      TO FERIADOS-41(7)
               MOVE GS-FERIADO (8)      TO FERIADOS-41(8)
               MOVE GS-FERIADO (9)      TO FERIADOS-41(9)
               MOVE GS-FERIADO (10)     TO FERIADOS-41(10)
               MOVE GS-QTDE-TOT-CHEQUE  TO QTDE-CHEQUES-41
               MOVE GS-VALOR-TOT-CHEQUE TO VLR-BRUTO-41
               MOVE GS-PM-TOT-CHEQUE    TO PM-41
               MOVE GS-DIAS-30-41       TO DIAS-30-41
               MOVE GS-TAXA-30-41       TO TAXA-30-41
               MOVE GS-JURO-30-41       TO JURO-30-41
               MOVE GS-SALDO-30-41      TO SALDO-30-41
               MOVE GS-DIAS-60-41       TO DIAS-60-41
               MOVE GS-TAXA-60-41       TO TAXA-60-41
               MOVE GS-JURO-60-41       TO JURO-60-41
               MOVE GS-SALDO-60-41      TO SALDO-60-41
               MOVE GS-DIAS-90-41       TO DIAS-90-41
               MOVE GS-TAXA-90-41       TO TAXA-90-41
               MOVE GS-JURO-90-41       TO JURO-90-41
               MOVE GS-SALDO-90-41      TO SALDO-90-41
               MOVE GS-DIAS-120-41      TO DIAS-120-41
               MOVE GS-TAXA-120-41      TO TAXA-120-41
               MOVE GS-JURO-120-41      TO JURO-120-41
               MOVE GS-SALDO-120-41     TO SALDO-120-41
               MOVE GS-DIAS-150-41      TO DIAS-150-41
               MOVE GS-TAXA-150-41      TO TAXA-150-41
               MOVE GS-JURO-150-41      TO JURO-150-41
               MOVE GS-SALDO-150-41     TO SALDO-150-41
               MOVE GS-DIAS-180-41      TO DIAS-180-41
               MOVE GS-TAXA-180-41      TO TAXA-180-41
               MOVE GS-JURO-180-41      TO JURO-180-41
               MOVE GS-SALDO-180-41     TO SALDO-180-41
               REWRITE REG-CHD041
               END-REWRITE
           END-READ
           CLOSE      CHD041
           OPEN INPUT CHD041.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

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
       GRAVA-CALCULO SECTION.
           CLOSE    CHD041
           OPEN I-O CHD041
           PERFORM CONT-GRAVA-CALCULO
           CLOSE      CHD041
           OPEN INPUT CHD041

           INITIALIZE GS-TAB-CARTEIRA
                      GS-TAB-DESC-CARTEIRA
                      GS-DATA-BASE-CALC
                      GS-TX-JURO-NEGOC
                      GS-TAXA-REAL
                      GS-VENCTO-INI
                      GS-DIAS-INI
                      GS-VENCTO-FIM
                      GS-DIAS-FIM
                      GS-TAB-FERIADO
                      GS-QTDE-TOT-CHEQUE
                      GS-VALOR-TOT-CHEQUE
                      GS-PM-TOT-CHEQUE
                      GS-PORTADOR-TRANSF
                      GS-DESCR-PORTADOR-TRANSF
                      GS-SENHA-PORT
                      GS-SENHA
                      GS-PORTADOR-G
                      GS-DESCR-PORTADOR-G
                      GS-NOME
                      GS-TEXTO
                      GS-TEXTO1
                      GS-TEXTO2
                      GS-DIAS-30-41
                      GS-TAXA-30-41
                      GS-JURO-30-41
                      GS-SALDO-30-41
                      GS-DIAS-60-41
                      GS-TAXA-60-41
                      GS-JURO-60-41
                      GS-SALDO-60-41
                      GS-DIAS-90-41
                      GS-TAXA-90-41
                      GS-JURO-90-41
                      GS-SALDO-90-41
                      GS-DIAS-120-41
                      GS-TAXA-120-41
                      GS-JURO-120-41
                      GS-SALDO-120-41
                      GS-DIAS-150-41
                      GS-TAXA-150-41
                      GS-JURO-150-41
                      GS-SALDO-150-41
                      GS-DIAS-180-41
                      GS-TAXA-180-41
                      GS-JURO-180-41
                      GS-SALDO-180-41.

       CONT-GRAVA-CALCULO SECTION.
           MOVE GS-DATA-BASE-CALC TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV    TO DATA-BASE-I.
           MOVE GS-VENCTO-INI TO DATA-INV.
           CALL "GRIDAT2"   USING DATA-INV.
           MOVE DATA-INV    TO VENCTO-INI.
           MOVE GS-VENCTO-FIM TO DATA-INV
           CALL "GRIDAT2"   USING DATA-INV
           MOVE DATA-INV    TO VENCTO-FIM

           MOVE GS-NOME             TO NOME-41.
           MOVE GS-CARTEIRA (1)     TO CARTEIRA-41 (1).
           MOVE GS-CARTEIRA (2)     TO CARTEIRA-41 (2).
           MOVE GS-CARTEIRA (3)     TO CARTEIRA-41 (3).
           MOVE GS-CARTEIRA (4)     TO CARTEIRA-41 (4).
           MOVE GS-CARTEIRA (5)     TO CARTEIRA-41 (5).
           MOVE GS-CARTEIRA (6)     TO CARTEIRA-41 (6).
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
           MOVE GS-DIAS-30-41       TO DIAS-30-41.
           MOVE GS-TAXA-30-41          TO TAXA-30-41.
           MOVE GS-JURO-30-41          TO JURO-30-41.
           MOVE GS-SALDO-30-41         TO SALDO-30-41.
           MOVE GS-DIAS-60-41          TO DIAS-60-41.
           MOVE GS-TAXA-60-41          TO TAXA-60-41.
           MOVE GS-JURO-60-41          TO JURO-60-41.
           MOVE GS-SALDO-60-41         TO SALDO-60-41.
           MOVE GS-DIAS-90-41          TO DIAS-90-41.
           MOVE GS-TAXA-90-41          TO TAXA-90-41.
           MOVE GS-JURO-90-41          TO JURO-90-41.
           MOVE GS-SALDO-90-41         TO SALDO-90-41.
           MOVE GS-DIAS-120-41         TO DIAS-120-41.
           MOVE GS-TAXA-120-41         TO TAXA-120-41.
           MOVE GS-JURO-120-41         TO JURO-120-41.
           MOVE GS-SALDO-120-41        TO SALDO-120-41.
           MOVE GS-DIAS-150-41         TO DIAS-150-41.
           MOVE GS-TAXA-150-41         TO TAXA-150-41.
           MOVE GS-JURO-150-41         TO JURO-150-41.
           MOVE GS-SALDO-150-41        TO SALDO-150-41.
           MOVE GS-DIAS-180-41         TO DIAS-180-41.
           MOVE GS-TAXA-180-41         TO TAXA-180-41.
           MOVE GS-JURO-180-41         TO JURO-180-41.
           MOVE GS-SALDO-180-41        TO SALDO-180-41.
           MOVE GS-PORTADOR-TRANSF  TO PORTADOR-DESTINO-41
           WRITE REG-CHD041.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CHP040" TO DS-SET-NAME
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
           move "CHP040"            to logacess-programa
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

           CLOSE CAD004 CAD018 CHD041 CHD041i.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
