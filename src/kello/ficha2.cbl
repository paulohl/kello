       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FICHA.
      *DATA: 07-10-2009
      *AUTOR: ALFREDO SAVIOLLI NETO
      *FUNÇÃO: Impressão Ficha de Clientes

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CAPX010.
           COPY CAPX018.
           COPY CGPX010.
           COPY CGPX001.
           COPY CGPX011.
           COPY CGPX020.
           COPY CRPX020.
           COPY CRPX001.
           COPY CHPX010.
           COPY CHPX013.
           COPY MTPX019.
           COPY MTPX020.
           COPY RCPX100.
           COPY RCPX101.
           COPY IEPX011.
           COPY LOGACESS.SEL.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW010.
       COPY CAPW018.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CGPW020.
       COPY CRPW020.
       COPY CRPW001.
       COPY CHPW010.
       COPY CHPW013.
       COPY MTPW019.
       COPY MTPW020.
       COPY RCPW100.
       COPY RCPW101.
       COPY IEPW011.
       COPY LOGACESS.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "FICHA.CPB".
           COPY "FICHA.CPY".
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
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
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
           05  COL1                  PIC 9(3)     VALUE ZEROS.
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
           05  TOT-TITULO            PIC 9(3)     VALUE ZEROS.
           05  TOT-VALOR-BRUTO       PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VALOR-LIQUIDO     PIC 9(8)V99  VALUE ZEROS.
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
           05  MESES-ATRASO         PIC 99        VALUE ZEROS.
           05  TAXA-W               PIC 9(3)V9(6) VALUE ZEROS.
           05  JUROS-DIARIO         PIC 9(6)V9(4) VALUE ZEROS.
           05  JUROS-ARECEBER       PIC 9(8)V99   VALUE ZEROS.
           05  DIAS-RESTANTE        PIC 9(2)      VALUE ZEROS.
           05  I                    PIC 99        VALUE ZEROS.
           05  LIN                  PIC 9(02)     VALUE ZEROS.
           05  TOTAL-LINHAS         PIC 9(06)     VALUE ZEROS.
           05  TOTAL-CTRECEBE-LIQ   PIC 9(09)V99  VALUE ZEROS.
           05  TOTAL-CTRECEBE-BRU   PIC 9(09)V99  VALUE ZEROS.
           05  TOTAL-JUROS          PIC 9(09)V99  VALUE ZEROS.
           05  TOTAL-CHEQUES        PIC 9(09)V99  VALUE ZEROS.
           05  TOTAL-RECIBO         PIC 9(09)V99  VALUE ZEROS.
           05  ALINEA-E             PIC ZZ        BLANK WHEN ZEROS.
           05  TAXA-ADMINISTRATIVA  PIC 9(09)V99  VALUE ZEROS.

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
       01  CAB02A.
      *    05  FILLER              PIC X(10)   VALUE "CLIENTE.: ".
      *    05  NOME-CLIENTE-REL    PIC X(30)   VALUE ZEROS.
           05  FILLER              PIC X(10)   VALUE "CURSO...: ".
           05  CURSO-REL           PIC X(40).
       01  CAB02B.
           05  FILLER              PIC X(10)   VALUE "FORMANDO: ".
           05  CLASSIF-REL         PIC 9       VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  CLIENTE-REL         PIC ZZZZ.ZZZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-FORMANDO-REL   PIC X(30).

       01  CAB03.
           05  FILLER              PIC X(166)  VALUE ALL "=".

      *01  CAB04-CTR.
      *    05  FILLER              PIC X(122)  VALUE
      *         "DATA-VECTO DOCUMENTO  PAR/QT TIPO PORTADOR   CART SITUA
      *    "CAO       VALOR BRUTO VLR LIQUIDO DATA-RECTO DIAS  JR+MT M N
      *    "-BCO".

       01 CAB04-CTR.
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
          05 FILLER                PIC X(10)
             VALUE "VLR SALDO".
          05 FILLER                PIC X(10)
             VALUE "VLR DESC.".
          05 FILLER                PIC X(10)
             VALUE "VLR ML+JR".
          05 FILLER                PIC X(10)
             VALUE "VLR TXADM".
          05 FILLER                PIC X(10)
             VALUE "  VLR LIQ".
          05 FILLER                PIC X(11)
             VALUE "ULT-RECTO".
          05 FILLER                PIC X(05)
             VALUE "DIAS".
          05 FILLER                PIC X(10)
             VALUE "N-BCO".

       01 CTR-04.
          05 CTR04-VENCTO          PIC 99/99/9999.
          05 FILLER                PIC X(01).
          05 CTR04-DOCUMENTO       PIC X(11).
          05 FILLER                PIC X(01).
          05 CTR04-PARCELA         PIC 99/.
          05 CTR04-QTPAR           PIC 99.
          05 FILLER                PIC X(01).
          05 CTR04-TIPO            PIC X(04).
          05 FILLER                PIC X(01).
          05 CTR04-PORTADOR        PIC X(10).
          05 FILLER                PIC X(01).
          05 CTR04-CART            PIC X(04).
          05 FILLER                PIC X(01).
          05 CTR04-SITUACAO        PIC X(13).
          05 FILLER                PIC X(01).
          05 CTR04-VALOR-BRUTO     PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(02).
          05 CTR04-VALOR-SALDO     PIC ZZZ.ZZ9,99.
          05 CTR04-VALOR-DESC      PIC ZZZ.ZZ9,99.
          05 CTR04-VALOR-MULTA     PIC ZZZ.ZZ9,99.
          05 CTR04-VALOR-TXADM     PIC ZZZ.ZZ9,99.
          05 CTR04-VALOR-LIQUIDO   PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 CTR04-DTRECTO         PIC X(10).
          05 FILLER                PIC X(01).
          05 CTR04-NUMDIAS         PIC ZZZ9.
          05 FILLER                PIC X(01).
          05 CTR04-NUMBANCO        PIC X(20).

       01  CAB04-CHP.
           05  FILLER              PIC X(110)  VALUE
           "DATA-VECTO NR-CHEQUE  PORTADOR   CART SITUACAO           VAL
      -    "OR SIT-CHEQUE     DATA-RECTO    JUROS-RCTO AL".


       01 DET-01.
          05 FILLER                PIC X(10) VALUE "ENDERECO: ".
          05 DET-ENDERECO          PIC X(40).
          05 FILLER                PIC X(10) VALUE "BAIRRO..: ".
          05 DET-BAIRRO            PIC X(30).

       01 DET-02.
          05 FILLER                PIC X(10) VALUE "CIDADE..: ".
          05 DET-CIDADE            PIC X(40).
          05 FILLER                PIC X(10) VALUE "CEP.....: ".
          05 DET-CEP               PIC 99.999.999.

       01 DET-03.
          05 FILLER                PIC X(10) VALUE "FONE....: ".
          05 DET-FONE              PIC X(40).
          05 FILLER                PIC X(10) VALUE "CELULAR.: ".
          05 DET-CELULAR           PIC X(30).

       01 DET-04.
          05 FILLER                PIC X(10) VALUE "CPF.....: ".
          05 DET-CPF               PIC 999.999.999.99.
          05 FILLER                PIC X(26).
          05 FILLER                PIC X(10) VALUE "RG......: ".
          05 DET-RG                PIC X(15).
          05 FILLER                PIC X(01).
          05 DET-ORGAO             PIC X(10).

       01 DET-05.
          05 FILLER                PIC X(10) VALUE "DT.NASC.: ".
          05 DET-DTNASC            PIC 99/99/9999
             BLANK WHEN ZEROS.
          05 FILLER                PIC X(30) VALUE SPACES.
          05 FILLER                PIC X(10) VALUE "SEXO....: ".
          05 DET-SEXO              PIC X(09).
          05 FILLER                PIC X(01).
          05 FILLER                PIC X(10) VALUE "SITUACAO: ".
          05 DET-SITUACAO          PIC X(15).

       01 DET-05A.
          05 FILLER                PIC X(10) VALUE "PAI.....: ".
          05 DET-NOME-PAI          PIC X(40) VALUE SPACES.
          05 FILLER                PIC X(10) VALUE "MAE.....: ".
          05 DET-NOME-MAE          PIC X(30) VALUE SPACES.

       01 DET-05B.
          05 FILLER                PIC X(10) VALUE "ENDERECO: ".
          05 DET-ENDERECO-PAI      PIC X(40) VALUE SPACES.
          05 FILLER                PIC X(10) VALUE "COMPL...: ".
          05 DET-COMPL-PAI         PIC X(30).

       01 DET-05C.
          05 FILLER                PIC X(10) VALUE "BAIRRO..: ".
          05 DET-BAIRRO-PAI        PIC X(40) VALUE SPACES.
          05 FILLER                PIC X(10) VALUE "TELEFONE: ".
          05 DET-TEL-PAI           PIC X(30).

       01 DET-05D.
          05 FILLER                PIC X(10) VALUE "CELULAR.: ".
          05 DET-CELULAR-PAI       PIC X(40).
          05 FILLER                PIC X(10) VALUE "CIDADE..: ".
          05 DET-CIDADE-PAI        PIC X(30).

       01 DET-05E.
          05 FILLER                PIC X(10) VALUE "CEP.....: ".
          05 DET-CEP-PAI           PIC 99.999.999.

       01 DET-05F1.
          05 FILLER                PIC X(18)
             VALUE "DADOS DA REPUBLICA".

       01 DET-05F.
          05 FILLER                PIC X(10) VALUE "ENDERECO: ".
          05 DET-ENDERECO-REP      PIC X(40) VALUE SPACES.
          05 FILLER                PIC X(10) VALUE "BAIRRO..: ".
          05 DET-BAIRRO-REP        PIC X(30) VALUE SPACES.

       01 DET-05G.
          05 FILLER                PIC X(10) VALUE "CIDADE..: ".
          05 DET-CIDADE-REP        PIC X(40) VALUE SPACES.
          05 FILLER                PIC X(10) VALUE "CEP.....: ".
          05 DET-CEP-REP           PIC 99.999.999.

       01 DET-05H.
          05 FILLER                PIC X(20)
             VALUE "DADOS PROFISSIONAIS".

       01 DET-05I.
          05 FILLER                PIC X(10) VALUE "EMPRESA.: ".
          05 DET-EMPRESA-PROF      PIC X(40).
          05 FILLER                PIC X(10) VALUE "FONE....: ".
          05 DET-FONE-PROF         PIC X(40).

       01 DET-05J.
          05 FILLER                PIC X(10) VALUE "ENDERECO: ".
          05 DET-ENDERECO-PROF     PIC X(40).
          05 FILLER                PIC X(10) VALUE "PTO REF.: ".
          05 DET-PONTO-REF-PROF    PIC X(30).

       01 DET-05K.
          05 FILLER                PIC X(10) VALUE "BAIRRO..: ".
          05 DET-BAIRRO-PROF       PIC X(40).
          05 FILLER                PIC X(10) VALUE "RAMAL...: ".
          05 DET-RAMAL-PROF        PIC X(30).

       01 DET-06.
          05 FILLER                PIC X(10) VALUE "VENDEDOR: ".
          05 DET-VENDEDOR          PIC X(40).
          05 FILLER                PIC X(10) VALUE "DT.VENDA: ".
          05 DET-DTVENDA           PIC 99/99/9999.

       01 DET-06a.
          05 FILLER                PIC X(03)
             VALUE "ES".
          05 FILLER                PIC X(03)
             VALUE "EN".
          05 FILLER                PIC X(05)
             VALUE "FOLH".
          05 FILLER                PIC X(05)
             VALUE "FOTO".
          05 FILLER                PIC X(02)
             VALUE "P".
          05 FILLER                PIC X(02)
             VALUE "F".
          05 FILLER                PIC X(03)
             VALUE "DV".
          05 FILLER                PIC X(03)
             VALUE "PD".
          05 FILLER                PIC X(03)
             VALUE "FC".
          05 FILLER                PIC X(03)
             VALUE "MD".
          05 FILLER                PIC X(02)
             VALUE "BK".

       01 DET-06b.
          05 DET-QTDES             PIC Z9.
          05 FILLER                PIC X(01).
          05 DET-QTDEN             PIC Z9.
          05 FILLER                PIC X(01).
          05 DET-QTFOLHA           PIC ZZZ9.
          05 FILLER                PIC X(01).
          05 DET-QTFOTO            PIC ZZZ9.
          05 FILLER                PIC X(01).
          05 DET-QTPOSTER          PIC Z.
          05 FILLER                PIC X(01).
          05 DET-FITA              PIC Z.
          05 FILLER                PIC X(01).
          05 DET-DVD               PIC ZZ9.
          05 FILLER                PIC X(01).
          05 DET-PORTADVD          PIC Z9.
          05 FILLER                PIC X(01).
          05 DET-FOTO-CD           PIC Z9.
          05 FILLER                PIC X(01).
          05 DET-MOLDURA           PIC Z9.
          05 FILLER                PIC X(01).
          05 DET-QT-BOOK           PIC Z9.



       01 DET-07.
          05 FILLER                PIC X(13)
             VALUE "ENCADERNACAO".
          05 FILLER                PIC X(08)
             VALUE " ESTOJO".
          05 FILLER                PIC X(08)
             VALUE "Q.FOTOS".
          05 FILLER                PIC X(09)
             VALUE "Q.FOLHAS".
          05 FILLER                PIC X(08)
             VALUE "FOTO CD".
          05 FILLER                PIC X(08)
             VALUE " POSTER".
          05 FILLER                PIC X(21)
             VALUE "TIPO".
          05 FILLER                PIC X(08)
             VALUE "MOLDURA".
          05 FILLER                PIC X(08)
             VALUE "Q.FITAS".
          05 FILLER                PIC X(08)
             VALUE "Q. DVDS".

       01 DET-07a.
          05 FILLER                PIC X(05).
          05 DET-QTENCAD           PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-QTESTOJO          PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-QTFOTOS           PIC ZZZ.ZZZ.
          05 FILLER                PIC X(02).
          05 DET-QTFOLHAS          PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-FOTOCD            PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-POSTER            PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-TIPO-POSTER       PIC X(20).
          05 FILLER                PIC X(01).
          05 DET-QTMOLDURA         PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-QTFITAS           PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-QTDVD             PIC ZZZ.ZZZ.

       01 DET-08.
          05 FILLER                PIC X(08)
             VALUE "PT FITA".
          05 FILLER                PIC X(08)
             VALUE "PT. DVD".
          05 FILLER                PIC X(08)
             VALUE "AVULSAS".
          05 FILLER                PIC X(08)
             VALUE "COMISS.".
          05 FILLER                PIC X(10)
             VALUE "COB. FITA".
          05 FILLER                PIC X(10)
             VALUE "COB. DVDS".
          05 FILLER                PIC X(12)
             VALUE "EXTRA FOTOS".
          05 FILLER                PIC X(08)
             VALUE "   BOOK".

       01 DET-08a.
          05 DET-QTPTFITA          PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-QTPTDVD           PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-QTAVULSAS         PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-QTCOMIS           PIC ZZZ.ZZZ.
          05 FILLER                PIC X(03).
          05 DET-COBFITA           PIC ZZZ.ZZZ.
          05 FILLER                PIC X(03).
          05 DET-COBDVD            PIC ZZZ.ZZZ.
          05 FILLER                PIC X(05).
          05 DET-EXTRA-FOT         PIC ZZZ.ZZZ.
          05 FILLER                PIC X(01).
          05 DET-BOOK              PIC ZZZ.ZZZ.


       01 DET-09.
          05 FILLER                PIC X(20)
             VALUE "Total a Receber...: ".
          05 DET-TOT-RECEBER       PIC ZZZ.ZZZ.ZZ9,99.

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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "CRD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD001.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CHD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD013"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CRD001
                      CGD001
                      CHD010
                      CHD013
                      CGD010
                      CGD011
                      CGD020
                      CAD004
                      CAD010
                      CAD018
                      MTD019
                      RCD100
                      RCD101
                      MTD020

           OPEN I-O   CRD020 IED011
           CLOSE      CRD020 IED011
           OPEN INPUT CRD020 IED011

           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "FICHA"             to logacess-programa
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
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    MOVE ZEROS            TO LIN
                                             PAG-W
                    MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                    PERFORM SET-UP-FOR-REFRESH-SCREEN
                    PERFORM CALL-DIALOG-SYSTEM
                    MOVE ZEROS            TO TOTAL-LINHAS

                    IF GS-CLIENTE(5:4) <> "0000"
                       PERFORM CARREGA-LISTA
                    ELSE
                       INITIALIZE REG-MTD019
                       MOVE GS-CLIENTE(1:4) TO CONTRATO-MT19
                       START MTD019 KEY IS NOT LESS ALBUM-MT19 INVALID
                       KEY
                            MOVE "10" TO ST-MTD019
                       END-START
                       PERFORM UNTIL ST-MTD019 = "10"
                            READ MTD019 NEXT AT END
                                 MOVE "10" TO ST-MTD019
                            NOT AT END
                                 IF GS-CLIENTE(1:4) <> CONTRATO-MT19
                                    MOVE "10" TO ST-MTD019
                                 ELSE
                                    PERFORM CARREGA-LISTA
                                 END-IF
                            END-READ
                       END-PERFORM
                    END-IF

               WHEN GS-POPUP-CLIENTE-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-CLIENTE-TRUE
                   PERFORM LE-CLIENTE
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
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
           READ CGD010 INVALID KEY
                MOVE "****"    TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10 TO GS-NOME-CLIENTE.

           MOVE COD-COMPL-CG10 TO COD-COMPL-CG11
           READ CGD011 INVALID KEY
                INITIALIZE REG-CGD011.

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

       CARREGA-LISTA SECTION.

           IF GS-CLIENTE(5:4) <> "0000"
              MOVE GS-CLASS(1: 1)  TO CLASSIF-REL
              MOVE GS-CLIENTE      TO CLIENTE-REL
      *       MOVE GS-NOME-CLIENTE TO NOME-CLIENTE-REL

              MOVE GS-CLIENTE      TO ALBUMMT19
              READ MTD019 INVALID KEY
                   MOVE ALL "***"  TO NOME-FORM-MT19
              END-READ
              MOVE NOME-FORM-MT19  TO NOME-FORMANDO-REL
           ELSE
              MOVE 0               TO CLASSIF-REL
              MOVE ALBUMMT19       TO CLIENTE-REL
              MOVE NOME-FORM-MT19  TO NOME-FORMANDO-REL

              MOVE 0               TO CLASSIF-CG10
              MOVE ALBUMMT19       TO CODIGO-CG10
              READ CGD010 INVALID KEY
                   MOVE "****"     TO COMPRADOR-CG10
              END-READ
      *       MOVE COMPRADOR-CG10  TO NOME-CLIENTE-REL

              MOVE COD-COMPL-CG10  TO COD-COMPL-CG11
              READ CGD011 INVALID KEY
                   INITIALIZE REG-CGD011
              END-READ
           END-IF

           MOVE TURMA-MT19      TO CODIGO-IE11
           READ IED011 INVALID KEY
                MOVE "******"   TO NOME-IE11
           END-READ
           MOVE NOME-IE11       TO CURSO-REL

           ADD 1                TO LIN
                                   PAG-W
           MOVE PAG-W           TO PG-REL

           MOVE CAB01           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB02A          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB02B          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES                  TO DET-ENDERECO
           STRING ENDERECO1-CG11 " " COMPLEMENTO1-CG11
             DELIMITED BY "   "       INTO DET-ENDERECO
           MOVE BAIRRO1-CG11            TO DET-BAIRRO
           MOVE CIDADE1-CG11            TO CIDADE
           READ CAD010 INVALID KEY
                MOVE "********"         TO NOME-COMPL-CID
                MOVE "**"               TO UF-CID
           END-READ
           MOVE SPACES                  TO DET-CIDADE
           STRING NOME-COMPL-CID " - " UF-CID DELIMITED BY "   " INTO
                                           DET-CIDADE
           MOVE CEP1-CG11               TO DET-CEP
           MOVE SPACES                  TO DET-FONE
           STRING DDD-CID " - " FONE1-CG11 INTO DET-FONE

           MOVE SPACES                  TO DET-CELULAR
           STRING DDD-CID " - " CELULAR1-CG11 INTO DET-CELULAR

           MOVE CPF1-CG11               TO DET-CPF
           MOVE RG1-CG11                TO DET-RG
           MOVE ORGAO-EXPEDICAO1-CG11   TO DET-ORGAO
           MOVE DATA-NASC1-CG11         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                TO DET-DTNASC

           EVALUATE SITUACAO-CLI-CG11
               WHEN 0 MOVE "OK"         TO DET-SITUACAO
               WHEN 1 MOVE "PROTESTADO" TO DET-SITUACAO
           END-EVALUATE

           EVALUATE SEXO2-CG11
               WHEN "M"   MOVE "MASCULINO" TO DET-SEXO
               WHEN "F"   MOVE "FEMININO"  TO DET-SEXO
               WHEN OTHER MOVE SPACES      TO DET-SEXO
           END-EVALUATE

           MOVE NOME-PAI-CG11          TO DET-NOME-PAI
           MOVE NOME-MAE-CG11          TO DET-NOME-MAE
           MOVE ENDERECO-PAIS-CG11     TO DET-ENDERECO-PAI
           MOVE COMPLEMENTO-PAIS-CG11  TO DET-COMPL-PAI
           MOVE BAIRRO-PAIS-CG11       TO DET-BAIRRO-PAI

           MOVE CIDADE-PAIS-CG11       TO CIDADE
           READ CAD010 INVALID KEY
               MOVE SPACES             TO DET-CIDADE-PAI
           NOT INVALID KEY
               MOVE SPACES                  TO DET-CIDADE-PAI
               STRING NOME-CID "-" UF-CID INTO DET-CIDADE-PAI
           END-READ
           MOVE SPACES                 TO DET-TEL-PAI
           STRING DDD-CID " - " FONE-PAIS-CG11 INTO DET-TEL-PAI

           MOVE SPACES                 TO DET-CELULAR-PAI
           STRING DDD-CID " - " CELULAR-PAIS-CG11 INTO DET-CELULAR-PAI

           MOVE CEP-PAIS-CG11           TO DET-CEP-PAI
           MOVE ENDERECO-REP-CG11       TO DET-ENDERECO-REP
           MOVE BAIRRO-REP-CG11         TO DET-BAIRRO-REP
           MOVE CIDADE-REP-CG11         TO CIDADE
           READ CAD010 INVALID KEY
               MOVE SPACES              TO NOME-CID
               MOVE SPACES              TO UF-CID
           END-READ
           MOVE SPACES                  TO DET-CIDADE-REP
           STRING NOME-CID "-" UF-CID INTO DET-CIDADE-REP
           MOVE CEP-REP-CG11            TO DET-CEP-REP

           MOVE EMPRESA-CG11            TO DET-EMPRESA-PROF
           STRING DDD-CID " - " FONE3-CG11 INTO DET-FONE-PROF

           MOVE ENDERECO3-CG11          TO DET-ENDERECO-PROF
           MOVE PONTO-REFER3-CG11       TO DET-PONTO-REF-PROF

           MOVE BAIRRO3-CG11            TO DET-BAIRRO-PROF
           MOVE RAMAL3-CG11             TO DET-RAMAL-PROF

           MOVE DET-01          TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE DET-02          TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE DET-03          TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE DET-04          TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE DET-05          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-05A         TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE DET-05B         TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE DET-05C         TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE DET-05D         TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE DET-05E         TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-05F1        TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE DET-05F         TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE DET-05G         TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-05H         TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-05I         TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-05J         TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-05K         TO GS-LINDET
           PERFORM INSERIR-DADOS


           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE "Planilha"      TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CODIGO-CG10     TO ALBUM-MTG
           READ MTD020 INVALID KEY
                INITIALIZE REG-MTD020
           END-READ

           MOVE QT-ENCADER-MTG     TO DET-QTDEN
           MOVE QT-ESTOJO-MTG      TO DET-QTDES
           MOVE QT-FOLHAS-MTG      TO DET-QTFOLHA
           MOVE QT-FOTOS-MTG       TO DET-QTFOTO
           MOVE QT-POSTER-MTG      TO DET-QTPOSTER
           MOVE QT-FITAS-MTG       TO DET-FITA
           MOVE QT-DVD-MTG         TO DET-DVD
           MOVE QT-PORTA-DVD-MTG   TO DET-PORTADVD
           MOVE QT-FOTO-CD-MTG     TO DET-FOTO-CD
           MOVE QT-MOLDURA-MTG     TO DET-MOLDURA
           MOVE QT-BOOK-MTG        TO DET-QT-BOOK

           MOVE DET-06a         TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-06b         TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS

           INITIALIZE DET-07A
                      DET-08A

           MOVE SPACES TO GS-LINDET
           PERFORM INSERIR-DADOS

           IF GS-COMPLETO = 1
              PERFORM IMPRIMIR-RECIBO
           END-IF

           MOVE "FIM CONTRATO"  TO GS-LINDET
           PERFORM INSERIR-DADOS.

       IMPRIMIR-RECIBO SECTION.
           MOVE "Recibo de Venda" TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CODIGO-CG10     TO ALBUM-REC
           READ RCD100 INVALID KEY
                INITIALIZE REG-RCD100.

           MOVE VENDEDOR-REC    TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE "*****"    TO NOME-CG01
           END-READ

           MOVE SPACES          TO DET-VENDEDOR
           STRING CODIGO-CG01 "-" NOME-CG01 INTO DET-VENDEDOR

           MOVE DATAVEN-REC       TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DET-DTVENDA


           MOVE QENCADER-REC         TO DET-QTENCAD
           MOVE QESTOJO-REC          TO DET-QTESTOJO
           MOVE QFOTOS-REC           TO DET-QTFOTOS
           MOVE QFOLHAS-REC          TO DET-QTFOLHAS
           MOVE QFITAS-REC           TO DET-QTFITAS
           MOVE QPFITA-REC           TO DET-QTPTFITA
           MOVE QCOBERTURA-REC       TO DET-COBFITA
           MOVE QPOSTER-REC          TO DET-POSTER
           MOVE QT-COBERTURA-DVD-REC TO DET-COBDVD
           MOVE QDVD-REC             TO DET-QTDVD
           MOVE QFOTO-CD-REC         TO DET-FOTOCD
           MOVE QMOLDURA-REC         TO DET-QTMOLDURA
           MOVE QPORTA-DVD-REC       TO DET-QTPTDVD
           MOVE QAVULSAS-REC         TO DET-QTAVULSAS
           MOVE QCOMISSAO-REC        TO DET-QTCOMIS
           MOVE QFOTOS-EXTRA1-REC    TO DET-EXTRA-FOT
           IF POSTER-ABERTURA-REC = 1 AND POSTER-XEROX-REC = 1
              MOVE "ABERTURA, XEROX" TO DET-TIPO-POSTER
           ELSE
              IF POSTER-ABERTURA-REC = 1
                 MOVE "ABERTURA"     TO DET-TIPO-POSTER
              ELSE
                 IF POSTER-XEROX-REC = 1
                    MOVE "XEROX"     TO DET-TIPO-POSTER
                 ELSE
                    MOVE SPACES      TO DET-TIPO-POSTER
                 END-IF
              END-IF
           END-IF
           MOVE QBOOK-REC            TO DET-BOOK

           MOVE TOTAL-REC            TO DET-TOT-RECEBER

           MOVE DET-06          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-07          TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-07a         TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-08          TO GS-LINDET
           PERFORM INSERIR-DADOS
           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-08a         TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE "Recibo"           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           PERFORM CARREGAR-RECIBO

           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE DET-09          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS


           MOVE "Contas a Receber" TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB04-CTR       TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           PERFORM CARREGAR-CTRECEBE

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE "Cheques"       TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB04-CHP       TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           PERFORM CARREGAR-CHEQUES.

           MOVE SPACES          TO GS-LINDET
           PERFORM INSERIR-DADOS.


       INSERIR-DADOS SECTION.
           ADD 1 TO TOTAL-LINHAS
           MOVE "INSERE-LIST"   TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGAR-RECIBO SECTION.
           INITIALIZE REG-RCD101
                      TOTAL-RECIBO

           MOVE ALBUM-REC TO ALBUM-REC1
           START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101.

           PERFORM UNTIL ST-RCD101 = "10"
                 READ RCD101 NEXT AT END
                      MOVE "10" TO ST-RCD101
                 NOT AT END
                      IF ALBUM-REC <> ALBUM-REC1
                         MOVE "10" TO ST-RCD101
                      ELSE
                         MOVE SPACES            TO GS-LINDET

                         MOVE VENCTO-REC1       TO DATA-INV
                         CALL "GRIDAT1" USING DATA-INV
                         MOVE DATA-INV          TO DATA-E
                         MOVE DATA-E            TO GS-LINDET(01: 11)

                         MOVE NUMERO-REC1       TO GS-LINDET(12: 11)

                         MOVE PARCELA-REC1      TO GS-LINDET(23:2)
                         MOVE "/"               TO GS-LINDET(25:1)
                         MOVE QT-PARCELA-REC1   TO GS-LINDET(26:2)

                         EVALUATE TIPO-REC1
                             WHEN 1   MOVE "CHEQUE" TO GS-LINDET(30:15)
                             WHEN 2   MOVE "MOEDA " TO GS-LINDET(30:15)
                             WHEN 3   MOVE "ANTECIPADO"
                                                    TO GS-LINDET(30:15)
                             WHEN 4   MOVE "DUPL/PROMIS"
                                                    TO GS-LINDET(30:15)
                             WHEN 5   MOVE "DEB.AUTOM"
                                                    TO GS-LINDET(30:15)
                             WHEN 6   MOVE "CARTAO CRED"
                                                    TO GS-LINDET(30:15)
                                      MOVE CARTAO-CRED-REC1
                                                    TO CODIGO-CG20
                                      READ CGD020 INVALID KEY
                                           MOVE "******" TO NOME-CG20
                                      END-READ
                                      MOVE NOME-CG20 TO GS-LINDET(53:10)
                             WHEN OTHER MOVE "******"
                                                    TO GS-LINDET(30:15)
                         END-EVALUATE

                         MOVE BANCO-REC1        TO GS-LINDET(48:03)

                         MOVE VALOR-REC1        TO VALOR-E
                         MOVE VALOR-E           TO GS-LINDET(66: 12)
                         PERFORM INSERIR-DADOS

                         ADD  VALOR-REC1        TO TOTAL-RECIBO

                      END-IF
                 END-READ
           END-PERFORM
           MOVE CAB03           TO GS-LINDET
           PERFORM INSERIR-DADOS

           MOVE SPACES          TO GS-LINDET
           MOVE TOTAL-RECIBO    TO VALOR-E
           MOVE VALOR-E         TO GS-LINDET(66: 12)
           PERFORM INSERIR-DADOS.

       CARREGAR-CTRECEBE SECTION.
           INITIALIZE REG-CRD020
                      TOTAL-CTRECEBE-LIQ
                      TOTAL-CTRECEBE-BRU
                      TOTAL-JUROS

           MOVE GS-CLASS(1:1)  TO CLASSIF-W
           MOVE CLASSIF-W      TO CLASS-CLIENTE-CR20
           MOVE CODIGO-CG10    TO CLIENTE-CR20
           START CRD020 KEY IS NOT < ALT1-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      IF CLIENTE-CR20       <> CODIGO-CG10 OR
                         CLASS-CLIENTE-CR20 <> CLASSIF-W
                         MOVE "10" TO ST-CRD020
                      ELSE
                         MOVE SPACES            TO GS-LINDET
                         MOVE DATA-VENCTO-CR20  TO DATA-INV
                         CALL "GRIDAT1" USING DATA-INV
                         MOVE DATA-INV          TO CTR04-VENCTO
                         MOVE NR-DOCTO-CR20     TO CTR04-DOCUMENTO
                         MOVE NR-PARC-CR20      TO CTR04-PARCELA
                         MOVE TOT-PARC-CR20     TO CTR04-QTPAR
                         EVALUATE TIPO-DOCTO-CR20
                             WHEN 0 MOVE "0-DUP" TO CTR04-TIPO
                             WHEN 1 MOVE "1-NTF" TO CTR04-TIPO
                             WHEN 2 MOVE "2-ORG" TO CTR04-TIPO
                             WHEN 3 MOVE "3-DEB" TO CTR04-TIPO
                             WHEN 4 MOVE "4-CAR" TO CTR04-TIPO
                         END-EVALUATE

                         MOVE PORTADOR-CR20     TO PORTADOR
                         READ CAD018 INVALID KEY
                              MOVE "****"       TO NOME-PORT
                         END-READ
                         MOVE NOME-PORT         TO CTR04-PORTADOR
                         EVALUATE CARTEIRA-CR20
                             WHEN 1 MOVE "SIMP" TO CTR04-CART
                             WHEN 2 MOVE "CAUC" TO CTR04-CART
                             WHEN 3 MOVE "DESC" TO CTR04-CART
                         END-EVALUATE

                         MOVE SITUACAO-TIT-CR20 TO CODIGO-CR01
                         READ CRD001 INVALID KEY
                              MOVE "******"     TO SITUACAO-TIT-CR01
                         END-READ
                         MOVE SITUACAO-TIT-CR01 TO CTR04-SITUACAO

                         MOVE VALOR-TOT-CR20    TO CTR04-VALOR-BRUTO
                         MOVE VALOR-SALDO-CR20  TO CTR04-VALOR-SALDO
                         MOVE DESCONTO-CR20     TO CTR04-VALOR-DESC
                         COMPUTE CTR04-VALOR-MULTA = JURO-RCTO-CR20 +
                                                     MULTA-RCTO-CR20
                         IF TIPO-DOCTO-CR20 = 4
                            IF TOT-PARC-CR20 > 1
                               COMPUTE TAXA-ADMINISTRATIVA =
                                      (VALOR-TOT-CR20*
                                       TAXA-ADMINIST-PARCELA-CR20 / 100)
                            ELSE
                               COMPUTE TAXA-ADMINISTRATIVA =
                                      (VALOR-TOT-CR20*
                                       TAXA-ADMINIST-CREDITO-CR20 / 100)
                            END-IF
                         ELSE
                            MOVE ZEROS TO TAXA-ADMINISTRATIVA
                         END-IF
                         MOVE TAXA-ADMINISTRATIVA   TO CTR04-VALOR-TXADM

                         COMPUTE CTR04-VALOR-LIQUIDO =
                                                   VALOR-TOT-CR20  +
                                                   JURO-RCTO-CR20  +
                                                   MULTA-RCTO-CR20 -
                                                  (DESCONTO-CR20   +
                                                   TAXA-ADMINISTRATIVA)

                         EVALUATE SITUACAO-CR20
                             WHEN 4     MOVE "CANCELADA"    TO
                                              CTR04-DTRECTO
                             WHEN 3     MOVE "ESTORNADA"    TO
                                              CTR04-DTRECTO
                             WHEN OTHER MOVE DATA-RCTO-CR20 TO DATA-INV
                                        CALL "GRIDAT1" USING DATA-INV
                                        MOVE DATA-INV       TO DATA-E
                                        MOVE DATA-E         TO
                                             CTR04-DTRECTO
                         END-EVALUATE

                         IF DATA-RCTO-CR20 = ZEROS
                            MOVE DATA-MOVTO-I       TO GRTIME-DATE-FINAL
                         ELSE
                            MOVE DATA-RCTO-CR20     TO GRTIME-DATE-FINAL
                         END-IF
                         MOVE DATA-VENCTO-CR20      TO GRTIME-DATE
                         MOVE 2                     TO GRTIME-TYPE
                         MOVE 3                     TO GRTIME-FUNCTION
                         IF GRTIME-DATE-FINAL > GRTIME-DATE
                            CALL "GRTIME" USING PARAMETROS-GRTIME
                            MOVE GRTIME-DAYS-FINAL  TO CTR04-NUMDIAS
                         ELSE
                            MOVE ZEROS              TO CTR04-NUMDIAS
                         END-IF

                         MOVE OUTRO-DOCTO-CR20      TO CTR04-NUMBANCO

                         MOVE CTR-04                TO GS-LINDET
                         PERFORM INSERIR-DADOS
                      END-IF
                 END-READ
           END-PERFORM

           IF TOTAL-CTRECEBE-LIQ > 0
              MOVE CAB03           TO GS-LINDET
              PERFORM INSERIR-DADOS
              MOVE SPACES          TO GS-LINDET
              MOVE "Total . . ."   TO GS-LINDET

              MOVE TOTAL-CTRECEBE-BRU  TO VALOR-E
              MOVE VALOR-E             TO GS-LINDET(66: 12)
              MOVE TOTAL-CTRECEBE-LIQ  TO VALOR-E
              MOVE VALOR-E             TO GS-LINDET(78:12)
              MOVE TOTAL-JUROS         TO VALOR-E3
              MOVE VALOR-E3            TO GS-LINDET(106: 06)
              PERFORM INSERIR-DADOS.

       CARREGAR-CHEQUES SECTION.
           INITIALIZE REG-CHD010
                      TOTAL-CHEQUES
                      TOTAL-JUROS

           MOVE GS-CLASS(1: 1) TO CLASS-CLIENTE-CH10
                                  CLASSIF-W
           MOVE CODIGO-CG10    TO CLIENTE-CH10

           START CHD010 KEY IS NOT < ALT-CH4 INVALID KEY
                  MOVE "10" TO ST-CHD010.

           PERFORM UNTIL ST-CHD010 = "10"
             READ CHD010 NEXT RECORD AT END
                  MOVE "10" TO ST-CHD010
             NOT AT END
                  IF CLIENTE-CH10       <> CODIGO-CG10 OR
                     CLASS-CLIENTE-CH10 <> CLASSIF-W
                     MOVE "10" TO ST-CHD010
                  ELSE
                     MOVE SPACES                  TO GS-LINDET

                     MOVE DATA-VENCTO-CH10        TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV                TO DATA-E
                     MOVE DATA-E                  TO GS-LINDET(01: 11)
                     MOVE NR-CHEQUE-CH10          TO GS-LINDET(12: 11)
                     MOVE PORTADOR-CH10           TO GS-LINDET(23: 11)
                     EVALUATE CARTEIRA-CH10
                        WHEN 1 MOVE "SIMP"        TO GS-LINDET(34: 5)
                        WHEN 2 MOVE "CAUC"        TO GS-LINDET(34: 5)
                        WHEN 3 MOVE "DESC"        TO GS-LINDET(34: 5)
                     END-EVALUATE
                     MOVE SITUACAO-TIT-CH10       TO CODIGO-CR01
                     READ CRD001 INVALID KEY
                          MOVE "******"           TO SITUACAO-TIT-CR01
                     END-READ
                     MOVE SITUACAO-TIT-CR01       TO GS-LINDET(39: 15)
                     MOVE VALOR-CH10              TO VALOR-E
                     ADD  VALOR-CH10              TO TOTAL-CHEQUES
                     MOVE VALOR-E                 TO GS-LINDET(52: 14)
                     EVALUATE SITUACAO-CH10
                       WHEN 0 MOVE "OK"           TO GS-LINDET(64: 11)
                       WHEN 1 MOVE "PARCIAL"      TO GS-LINDET(64: 11)
                       WHEN 2 MOVE "RECEBIDO"     TO GS-LINDET(64: 11)
                       WHEN 3 MOVE "ESTORNADO"    TO GS-LINDET(64: 11)
                       WHEN 4 MOVE "CANCELADO"    TO GS-LINDET(64: 11)
                       WHEN 5 MOVE "DEVOLVIDO"    TO GS-LINDET(64: 11)
                       WHEN 6 MOVE "PROBLEMATICO" TO GS-LINDET(64: 11)
                     END-EVALUATE
                     MOVE DATA-MOVTO-CH10         TO DATA-MOVTO-CH13
                     MOVE SEQ-CH10                TO SEQ-CH13
                     READ CHD013 INVALID KEY
                          INITIALIZE REG-CHD013
                     END-READ

                     MOVE ZEROS                   TO DATA-E
                     IF SITUACAO-CH10 = 2 OR 1
                        IF DATA-RECTO-CH13 <> ZEROS
                           MOVE DATA-RECTO-CH13   TO DATA-INV
                           CALL "GRIDAT1" USING DATA-INV

                           MOVE DATA-INV          TO DATA-E
                        END-IF
                     END-IF
                     MOVE DATA-E                  TO GS-LINDET(79: 11)
                     COMPUTE VALOR-E = VLR-JUROS-CH13 +
                                       VLR-MULTA-CH13
                     ADD VLR-JUROS-CH13     TO TOTAL-JUROS
                     ADD VLR-MULTA-CH13     TO TOTAL-JUROS
                     MOVE VALOR-E           TO GS-LINDET(92: 13)

                     MOVE ALINEA-CH13             TO ALINEA-E
                     MOVE ALINEA-E                TO GS-LINDET(104: 2)


                     PERFORM INSERIR-DADOS

                  END-IF
             END-READ
           END-PERFORM

           IF TOTAL-CHEQUES > 0
              MOVE CAB03           TO GS-LINDET
              PERFORM INSERIR-DADOS
              MOVE SPACES          TO GS-LINDET
              MOVE "Total . . ."   TO GS-LINDET

              MOVE TOTAL-CHEQUES       TO VALOR-E
              MOVE VALOR-E             TO GS-LINDET(52: 14)
              MOVE TOTAL-JUROS         TO VALOR-E
              MOVE VALOR-E             TO GS-LINDET(92: 13)
              PERFORM INSERIR-DADOS.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "FICHA"  TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE 0                   TO GS-CONT

           PERFORM UNTIL GS-CONT = TOTAL-LINHAS
               ADD  1               TO GS-CONT
               MOVE "LER-LB"        TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM

               IF GS-LINDET <> "FIM CONTRATO"
                  WRITE REG-RELAT FROM GS-LINDET
                  ADD 1 TO LIN
                  IF LIN > 58
                     PERFORM CABECALHO
                  END-IF
               ELSE
                  PERFORM CABECALHO
               END-IF
           END-PERFORM

           COPY DESCONDENSA.


       CABECALHO SECTION.
      *    MOVE GS-CLASS(1: 1)  TO CLASSIF-REL.
      *    MOVE CODIGO-CG10     TO CLIENTE-REL
      *    MOVE COMPRADOR-CG10  TO NOME-CLIENTE-REL.
      *
      *    IF GS-CLIENTE(5:4) <> "0000"
      *       MOVE CODIGO-CG10     TO ALBUMMT19
      *       READ MTD019 INVALID KEY
      *            MOVE ALL "**"   TO NOME-FORM-MT19
      *       END-READ
      *    END-IF
      *    MOVE NOME-FORM-MT19     TO NOME-FORMANDO-REL

           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.

           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

      *    WRITE REG-RELAT FROM CAB02A AFTER 2.
      *    WRITE REG-RELAT FROM CAB02B
      *    WRITE REG-RELAT FROM CAB03.
      *    MOVE 5 TO LIN.

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
           move "FICHA"             to logacess-programa
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

           CLOSE CGD010
                 CGD011
                 CGD020
                 CGD001
                 CRD001
                 CRD020
                 CAD004
                 CAD010
                 CAD018
                 MTD020
                 MTD019
                 RCD100
                 CHD010
                 CHD013
                 MTD020
                 RCD101
                 IED011
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
