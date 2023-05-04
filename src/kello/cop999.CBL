       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP100.
      *AUTHOR: MARELI AMANCIO VOLPATO
      *DATA: 06/08/1999
      *DESCRIÇÃO: Consulta de contrato

      *   a pedido do vonei, cancelar atualização automática no contas
      *   a pagar do brinde
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA
       PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CAPX004.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX011.
           COPY CGPX012.
           COPY COPX001.
           COPY COPX002.
           COPY COPX003.
           COPY COPX005.
           COPY COPX006.
           COPY COPX007.
           COPY COPX040.
           COPY COPX041.
           COPY COPX050.
           COPY COPX060.
           COPY COPX061.
           COPY COPX070.
           COPY COPX500.
           COPY COPX501.
           COPY CAPX010.
           COPY CAPX012.
           COPY CPPX020.
           COPY CPPX021.
           COPY IEPX010.
           COPY IEPX011.
           COPY OEPX010.
           COPY OEPX020.
           COPY CRPX020.
           COPY REPX100.
           COPY REPX101.
           COPY PRPX012.
           COPY MTPX019.
           COPY PARX001.
           COPY LOGACESS.SEL.
           SELECT WORK1 ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK1
                  RECORD KEY IS DOCTO-WK1.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CAPW004.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CGPW012.
       COPY COPW001.
       COPY COPW002.
       COPY COPW003.
       COPY COPW005.
       COPY COPW006.
       COPY COPW007.
       COPY COPW040.
       COPY COPW041.
       COPY COPW050.
       COPY COPW060.
       COPY COPW061.
       COPY COPW070.
       COPY COPW500.
       COPY COPW501.
       COPY CAPW010.
       COPY CAPW012.
       COPY CPPW020.
       COPY CPPW021.
       COPY IEPW010.
       COPY IEPW011.
       COPY OEPW010.
       COPY OEPW020.
       COPY CRPW020.
       COPY REPW100.
       COPY REPW101.
       COPY PRPW012.
       COPY MTPW019.
       COPY PARW001.
       COPY LOGACESS.FD.

       FD  WORK1.
       01  REG-WORK1.
           05  DOCTO-WK1         PIC 9(6).

       FD  RELAT
       LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP100.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDIAS1.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD012             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD006             PIC XX       VALUE SPACES.
           05  ST-COD007             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD041             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-COD061             PIC XX       VALUE SPACES.
           05  ST-COD070             PIC XX       VALUE SPACES.
           05  ST-COD500             PIC XX       VALUE SPACES.
           05  ST-COD501             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD021             PIC XX       VALUE SPACES.
           05  ST-OED010             PIC XX       VALUE SPACES.
           05  ST-OED020             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-PRD012             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  AUX-ITEM              PIC 9(04)    VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  NR-CONTRATO-W         PIC 9(4)     VALUE ZEROS.
           05  WS-CODRED-BRINDE      PIC 9(05)    VALUE ZEROS.
           05  NR-INIC-PAD           PIC X(6)     VALUE SPACES.
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
           05  COMISSAO  OCCURS 3 TIMES PIC X(15).
           05  FONE      OCCURS 3 TIMES PIC 9(8).
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  CUSTO-TOTAL           PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-PREVISTO        PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-W               PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-W               PIC 9(8)V99  VALUE ZEROS.
           05  SOMACUSTO-W           PIC 9(8)V99  VALUE ZEROS.
           05  PREV-FOTOS-W          PIC 9(8)     VALUE ZEROS.
           05  DIAS-W                PIC 9(6)     VALUE ZEROS.
           05  DIAS-PAGOS            PIC 9(6)     VALUE ZEROS.
           05  DIAS-ATRASADO         PIC 9(6)     VALUE ZEROS.
           05  DIAS-RESTANTE         PIC 9(6)     VALUE ZEROS.
      *    VARIAVEIS P/ CALCULO DO PRAZO MÉDIO
           05  TOTAL-PAGOS-OE        PIC 9(10)V99 VALUE ZEROS.
           05  TOTAL-RESTANTE-OE     PIC 9(10)V99 VALUE ZEROS.
           05  TOTAL-ATRASADO-OE     PIC 9(10)V99 VALUE ZEROS.
           05  ENTREI                PIC X(01)    VALUE SPACES.

           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  DATA-VENC             PIC 9(08).
           05  FONE-E                PIC ZZZZBZZZZ.
           05  TAXA-E                PIC ZZ,ZZ.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  QTDE4-E               PIC ZZZZ.
           05  QTDE5-E               PIC ZZZZZ.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  PM-W                  PIC 9(3)V99  VALUE ZEROS.
           05  PM-E                  PIC ZZZ,ZZ   BLANK WHEN ZEROS.
           05  MESES-W               PIC 9(3)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  I                     PIC 9        VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  DDD-W                 PIC 9(3)     VALUE ZEROS.
      *    VARIAVEIS P/ RESUMO
           05  VLR-PAGO-RES          PIC 9(8)V99  VALUE ZEROS.
           05  VLR-PAGAR-RES         PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGO-RES        PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGAR-RES       PIC 9(8)V99  VALUE ZEROS.
           05  VLR-PAGO-RES-OE       PIC 9(8)V99  VALUE ZEROS.
           05  VLR-PAGAR-RES-OE      PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGO-RES-OE     PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGAR-RES-OE    PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-BRINDE-FORM-RES PIC 9(8)V99  VALUE ZEROS.
           05  CRED-RECEB-RES        PIC 9(8)V99  VALUE ZEROS.
           05  CRED-FUT-RES          PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-CRED-REC-RES    PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-CRED-FUT-RES    PIC 9(8)V99  VALUE ZEROS.
           05  DIAS-PAGO-RES         PIC 9(8)     VALUE ZEROS.
           05  DIAS-PAGAR-RES        PIC 9(8)     VALUE ZEROS.
           05  DIAS-PAGO-RES-OE      PIC 9(8)     VALUE ZEROS.
           05  DIAS-PAGAR-RES-OE     PIC 9(8)     VALUE ZEROS.
           05  DIAS-CRED-REC-RES     PIC 9(8)     VALUE ZEROS.
           05  DIAS-CRED-FUT-RES     PIC 9(8)     VALUE ZEROS.
           05  VLR-REPORTAGEM-RES    PIC 9(8)V99  VALUE ZEROS.
           05  DIAS-REPORTAGEM-RES   PIC 9(8)     VALUE ZEROS.
           05  JUROS-REPORTAGEM-RES  PIC 9(8)V99  VALUE ZEROS.
           05  TOT-FITA-RES          PIC 9(4)V99  VALUE ZEROS.
           05  TOT-FILME-RES         PIC 9(4)V99  VALUE ZEROS.
      *    VARIAVEIS P/ REPORTAGEM
           05  QT-FILME-REP          PIC 9(3)V99  VALUE ZEROS.
           05  QT-FITA-REP           PIC 9(3)V99  VALUE ZEROS.
           05  QT-EQUIPE-REP         PIC 9(3)V99  VALUE ZEROS.
           05  TOT-PARTICIPANTE      PIC 9(4)     VALUE ZEROS.
           05  DESPESA-W             PIC 9(8)V99  VALUE ZEROS.
           05  VLR-DESPESA-REP       PIC 9(8)V99  VALUE ZEROS.
           05  VLR-REPORT-REP        PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-REPORT          PIC 9(8)V99  VALUE ZEROS.
           05  QTDE-E                PIC ZZZZ,ZZ.
      *    VARIAVEIS P/ CONTATO
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-ITEM              PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.

           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  NAO-IDENTIFICADO      PIC 9(09).
           05  MASC-NUMERO           PIC ZZ.ZZ9.
           COPY "PARAMETR".
      *------------------------------------------------------
       01  CAB01.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(105)  VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(108)  VALUE
           "DADOS DO CONTRATO                    ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "CONT TAXA  ORI IDENTIFICACAO   TU INSTITUIC. CIDADE       ME
      -    "S/ANO FORM PAD".
       01  CAB05.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "STATUS-ATUAL                   MENSAGEM
      -    "            SERVICOS       ".       1  CAB06.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "==========INFORMACOES DO CONTRATO=============
      -    "======================CONTATOS=======================".

       01  CAB07.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "/*****/  DADOS DO BRINDE  \*****\".
       01  CAB08.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "IT BRINDE           QT.P/FORM FORM  $VLR-UNIT $VLR-TOTAL DAT
      -    "A-VECTO DATA-PAGTO DIAS $VLR-JUROS CUST-TOTAL FORNEC".
       01  CAB08A.
           05  FILLER              PIC X(94)   VALUE SPACES.
           05  SOMACUSTO-REL       PIC ZZ.ZZZ.ZZZ,99.
       01  CAB09.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "/*****/  DADOS DO EVENTO  \*****\".

       01  CAB10.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "IT. DESCRICAO-EVENTO     NR-FORM DATA-PREV. HORARIO  FO VI T
      -    "EL BE CL FX AP".
       01  CAB10A.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "/*****/  DADOS DE CREDITO \*****\".

       01  CAB10B.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "IT DESCRICAO                                  $VLR-PREV. DAT
      -    "A-VECTO DATA-RECTO DIAS $VLR-JUROS CUST-TOTAL ".

       01  CAB11.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "/*****/  DADOS DO CONTATO  \*****\".
       01  CAB12.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "/*****/  DADOS DE TURMA/CURSO  \*****\".
       01  CAB13.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(125)  VALUE
           "DESCRICAO-CURSO           TURMA FORM L  RESPONSAV. FT MB COR
      -    "-FAIXA  COR-CANUDO T-ALB".

       01  LINDET.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  LINDET-REL          PIC X(125)  VALUE SPACES.
      *------------------------------------------------------
      *******(LINHAS DETALHES - PREVISÃO FINANCEIRA)***********
       01  LINDET00.
           05  FILLER      PIC X(10)   VALUE SPACES.
           05  FILLER      PIC X(89)   VALUE "K E L L O   F O T O   &
      -     "V I D E O".
           05  FILLER      PIC X(14)   VALUE "DATA EMISSAO: ".
           05  EMISSAO-REL1 PIC 99/99/9999 BLANK WHEN ZEROS.
       01  TRACO9.
           05  FILLER      PIC X(10)    VALUE SPACES.
           05  FILLER      PIC X(112)   VALUE ALL '='.
       01  TRACO10.
           05  FILLER      PIC X(10)    VALUE SPACES.
           05  FILLER      PIC X(112)   VALUE ALL '-'.
       01  LINDET01.
           05  FILLER      PIC X(10)   VALUE SPACES.
           05  FILLER      PIC X(110)   VALUE 'P R O G R A M A C A O
      -         'F I N A N C E I R A           - LANCADO AUTOMATICO NO
      -         'CONTAS A PAGAR' .
       01  LINDET02.
           05  FILLER      PIC X(10)   VALUE SPACES.
           05  FILLER      PIC X(60)   VALUE
               'DE:   DEPTO OPERACIONAL CONTRATO   -   DOC'.
       01  LINDET03.
           05  FILLER      PIC X(10)   VALUE SPACES.
           05  FILLER      PIC X(60)   VALUE
               'P/:   DEPTO FINANCEIRO             -   ADM'.
       01  LINDET03A.
           05  FILLER      PIC X(20)   VALUE SPACES.
           05  FILLER      PIC X(60)   VALUE
               'TIPO DE BRINDE: BRINDE DE CONTRATO'.
       01  LINDET03AA.
           05  FILLER      PIC X(20)   VALUE SPACES.
           05  FILLER      PIC X(60)   VALUE
               'TIPO DE BRINDE: COMPROMISSO DE ORGANIZACAO DE EVENTOS'.
       01  LINDET03B.
           05  FILLER      PIC X(20)   VALUE SPACES.
           05  FILLER      PIC X(10)   VALUE  'BRINDE: '.
           05  BRINDE1-REL PIC 999     BLANK WHEN ZEROS.
           05  FILLER      PIC XX      VALUE SPACES.
           05  DESC-BRINDE-REL PIC X(20) VALUE SPACES.
           05  FILLER      PIC X(10)   VALUE SPACES.
       01  LINDET03C.
           05  FILLER      PIC X(20)   VALUE SPACES.
           05  FILLER      PIC X(06)   VALUE 'ITEM: '.
           05  ITEM-REL    PIC 999     VALUE ZEROS.
       01  LINDET04.
           05  FILLER      PIC X(20)   VALUE SPACES.
           05  FILLER      PIC X(10)   VALUE 'VALOR R$:'.
           05  VALOR-REL   PIC ZZZ.ZZZ.ZZZ,99 BLANK WHEN ZEROS.
           05  FILLER      PIC X(24)   VALUE
               '             VALOR US$: '.
           05  DOLAR-REL   PIC ZZZ.ZZZ.ZZZ,99 BLANK WHEN ZEROS.
       01  LINDET05.
           05  FILLER      PIC X(20)   VALUE SPACES.
           05  FILLER      PIC X(12)   VALUE 'VENCIMENTO: '.
           05  VENC-REL    PIC X(48)   VALUE SPACES.
       01  LINDET06.
           05  FILLER      PIC X(20)   VALUE SPACES.
           05  FILLER      PIC X(16)   VALUE 'FORMA DE PAGTO: '.
           05  PGTO-REL    PIC X(64)   VALUE SPACES.
       01  LINDET06A.
           05  FILLER      PIC X(20)   VALUE SPACES.
           05  FILLER      PIC X(16)   VALUE SPACES.
           05  PGTO1-REL   PIC X(64)   VALUE SPACES.
       01  LINDET07.
           05  FILLER      PIC X(20)   VALUE SPACES.
           05  FILLER      PIC X(14)   VALUE 'REF.CONTRATO: '.
           05  CONT-REL    PIC 9(4)    VALUE ZEROS.
           05  FILLER      PIC XXX     VALUE ' - '.
           05  CURSO1-REL  PIC X(27)   VALUE SPACES.
           05  FILLER      PIC XXX     VALUE ' - '.
           05  IDENT-REL   PIC X(31)   VALUE SPACES.
           05  FILLER      PIC XX      VALUE SPACES.
           05  MESANO-REL1 PIC 99/9999 VALUE ZEROS.
           05  FILLER      PIC X(3)    VALUE ' - '.
           05  CIDADE1-REL PIC X(15)   VALUE SPACES.
       01  LINDET09.
           05  FILLER      PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X          VALUE SPACES.
           05  FILLER          PIC X(110)      VALUE ALL '_'.
           05  FILLER          PIC X          VALUE SPACES.
       01  LINDET09B.
           05  FILLER      PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X          VALUE 'I'.
           05  FILLER          PIC X(54)      VALUE SPACES.
           05  FILLER          PIC X          VALUE 'I'.
           05  FILLER          PIC X(55)      VALUE SPACES.
           05  FILLER          PIC X          VALUE 'I'.
       01  LINDET09C.
           05  FILLER      PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X          VALUE 'I'.
           05  FILLER          PIC X(54)      VALUE ALL '_'.
           05  FILLER          PIC X          VALUE 'I'.
           05  FILLER          PIC X(55)      VALUE ALL '_'.
           05  FILLER          PIC X          VALUE 'I'.
       01  LINDET09D.
           05  FILLER      PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X          VALUE 'I'.
           05  FILLER          PIC X(54)      VALUE ' DPTO FINANCEIRO '.
           05  FILLER          PIC X          VALUE 'I'.
           05  FILLER          PIC X(55)      VALUE ' DPTO OP.CONTRATO'.
           05  FILLER          PIC X          VALUE 'I'.
       01  TRACO-SEP.
           05  FILLER          PIC X(10)    VALUE SPACES.
           05  FILLER          PIC X(112)   VALUE ALL '- '.
      *------------------------------------------------------------

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU                 PIC 9(04).
             10 WS-MES-CPU                 PIC 9(02).
             10 WS-DIA-CPU                 PIC 9(02).
          05 FILLER                        PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV       TO EMISSAO-REL1.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV TO DATA-DIA-I
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE NOME-EMP           TO EMPRESA-REL
           MOVE EMPRESA            TO EMP-REC
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD012.
           MOVE "IED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD006" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD006.
           MOVE "COD007" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD007.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD041" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD041.
           MOVE "COD050" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "COD060" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "COD061" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD061.
           MOVE "COD070" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD070.
           MOVE "COD500" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD500.
           MOVE "COD501" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD501.
           MOVE "CPD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CPD021" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD021.
           MOVE "OED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-OED010.
           MOVE "OED020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-OED020.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "RED100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED101.
           MOVE "PRD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PRD012.
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "PAR001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "LOGACESS"TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           MOVE ZEROS TO ERRO-W.
           OPEN INPUT CAD010 IED010 IED011 CGD010 COD001 COD002 CAD004
                      COD003 COD040 COD050 COD060 CAD012 COD041 CGD001
                      COD070 COD007 COD061 PRD012 MTD019
           OPEN I-O COD500 COD501 PAR001.
           CLOSE CONTROLE PAR001.
           OPEN INPUT PAR001
           ACCEPT VARIA-W1 FROM TIME.
           OPEN OUTPUT WORK1  CLOSE WORK1  OPEN I-O WORK1.

           IF ST-COD500 = "35"  CLOSE COD500  OPEN OUTPUT COD500
                                CLOSE COD500  OPEN I-O COD500.
           IF ST-COD501 = "35"  CLOSE COD501  OPEN OUTPUT COD501
                                CLOSE COD501  OPEN I-O COD501.
           IF ST-COD061 <> "00"
              MOVE "ERRO ABERTURA COD061: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD061 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD500 <> "00"
              MOVE "ERRO ABERTURA COD500: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD500 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD501 <> "00"
              MOVE "ERRO ABERTURA COD501: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD501 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD007 <> "00"
              MOVE "ERRO ABERTURA COD007: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD007 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD041 <> "00"
              MOVE "ERRO ABERTURA COD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD041 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD050 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD070 <> "00"
              MOVE "ERRO ABERTURA COD070: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD070 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD012 <> "00"
              MOVE "ERRO ABERTURA PRD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE 1 TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                MOVE "Parametrização do Brinde Não Cadastrada"
                TO GS-MENSAGEM-ERRO
                PERFORM CARREGA-MENSAGEM-ERRO
           NOT INVALID KEY
                MOVE CODRED-BRINDE-PAR001 TO WS-CODRED-BRINDE
           END-READ

           CLOSE PAR001

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "COP100"            to logacess-programa
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

           IF ERRO-W = ZEROS
              MOVE 1 TO GS-ORDER
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
               EVALUATE TRUE
               WHEN GS-CARREGAR-DADOS-TRUE
                    PERFORM CARREGAR-DADOS
               WHEN GS-PRINTER-PREV-FIN-TRUE
                    PERFORM IMPRIMIR-PREVISAO-FINANC
               WHEN GS-PRINTER-FLG-TRUE
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-PRINTER-BRINDE-TRUE
                    PERFORM IMPRIME-BRINDE
               WHEN GS-GRAVA-CONTATO-TRUE
                    PERFORM GRAVA-CONTATO
               WHEN GS-CHAMAR-BRINDE-TRUE
                    PERFORM CHAMAR-BRINDE
               WHEN GS-CHAMAR-BRINDE-OE-TRUE
                    PERFORM CHAMAR-BRINDE-OE
               WHEN GS-CHAMAR-EVENTO-TRUE
                    PERFORM CHAMAR-EVENTO
               WHEN GS-CHAMAR-CREDITO-TRUE
                    PERFORM CHAMAR-CREDITO
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM POPUP-CONTRATO
               WHEN GS-VERIFICA-ACESSO-TRUE
                    PERFORM VERIFICA-ACESSO
               WHEN GS-TRAZER-CURSO-TRUE
                    PERFORM TRAZER-CURSO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.
       VERIFICA-ACESSO SECTION.
      * VERIFICAR SE A PESSOA TEM ACESSO P/ EMISSAO DE PREV.FINANCEIRA
           MOVE "SENHA04" TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
              MOVE 0 TO GS-LIBERA-ACESSO
              MOVE "ACESSO-NAO-PERMITIDO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
            NOT INVALID KEY MOVE 1 TO GS-LIBERA-ACESSO.

       POPUP-CONTRATO SECTION.
           CALL "COP040T" USING PASSAR-PARAMETROS.
           CANCEL "COP040T".
           MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO.
       CHAMAR-BRINDE SECTION.
           MOVE GS-CONTRATO        TO PASSAR-STRING-1(1: 4)
           MOVE GS-LINDET(1: 2)    TO PASSAR-STRING-1(5: 2)
           MOVE IMPRESSORA-W       TO PASSAR-STRING-1(9: 2)
           MOVE COD-USUARIO-W      TO PASSAR-STRING-1(11: 3)
      *    MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
      *    MOVE "SENHA04"          TO PROGRAMA-CA004.
      *    READ CAD004 INVALID KEY
      *        MOVE "ACESSO-NAO-PERMITIDO" TO DS-PROCEDURE
      *        PERFORM CALL-DIALOG-SYSTEM
      *    NOT INVALID KEY
            CALL "COP050X" USING PASSAR-PARAMETROS
            CANCEL "COP050X"
            PERFORM CARREGAR-DADOS-BRINDE.
       CHAMAR-BRINDE-OE SECTION.
           MOVE GS-CONTRATO        TO PASSAR-STRING-1(1: 4)
           MOVE GS-LINDET(1: 2)    TO PASSAR-STRING-1(5: 2)
           MOVE IMPRESSORA-W       TO PASSAR-STRING-1(9: 2)
           MOVE COD-USUARIO-W      TO PASSAR-STRING-1(11: 3)
      *    MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
      *    MOVE "SENHA04"          TO PROGRAMA-CA004.
      *    READ CAD004 INVALID KEY
      *        MOVE "ACESSO-NAO-PERMITIDO" TO DS-PROCEDURE
      *        PERFORM CALL-DIALOG-SYSTEM
      *    NOT INVALID KEY
            CALL "OEP020X" USING PASSAR-PARAMETROS
            CANCEL "OEP020X"
            PERFORM CARREGAR-DADOS-BRINDE-OE.
       CHAMAR-EVENTO SECTION.
           MOVE GS-CONTRATO        TO PASSAR-STRING-1(1: 4)
           MOVE GS-LINDET(1: 3)    TO PASSAR-STRING-1(5: 3)
           MOVE IMPRESSORA-W       TO PASSAR-STRING-1(9: 2)
           MOVE COD-USUARIO-W      TO PASSAR-STRING-1(11:3).
      *    MOVE "SENHA04"          TO PROGRAMA-CA004.
      *    READ CAD004 INVALID KEY
      *        MOVE "ACESSO-NAO-PERMITIDO" TO DS-PROCEDURE
      *        PERFORM CALL-DIALOG-SYSTEM
      *    NOT INVALID KEY
            CALL "COP060X" USING PASSAR-PARAMETROS
            CANCEL "COP060X"
            PERFORM CARREGAR-DADOS-EVENTO.
       CHAMAR-CREDITO SECTION.
           MOVE GS-CONTRATO        TO PASSAR-STRING-1(1: 4)
           MOVE GS-LINDET(1: 2)    TO PASSAR-STRING-1(5: 2)
           MOVE IMPRESSORA-W       TO PASSAR-STRING-1(9: 2)
           MOVE COD-USUARIO-W      TO PASSAR-STRING-1(11: 3)
           CALL "COP070X" USING PASSAR-PARAMETROS
           CANCEL "COP070X"
           PERFORM CARREGAR-DADOS-CREDITO.

       GRAVA-CONTATO SECTION.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO500.
           MOVE ZEROS       TO ITEM-CO500 ULT-ITEM.
           START COD500 KEY IS NOT < CHAVE-CO500 INVALID KEY
                 MOVE "10" TO ST-COD500.
           PERFORM UNTIL ST-COD500 = "10"
             READ COD500 NEXT RECORD AT END MOVE "10" TO ST-COD500
               NOT AT END
                 IF NR-CONTRATO-CO500 <> GS-CONTRATO
                              MOVE "10" TO ST-COD500
                 ELSE MOVE ITEM-CO500 TO ULT-ITEM
                      CONTINUE
             END-READ
           END-PERFORM.
           ADD 1 TO ULT-ITEM.
           MOVE ULT-ITEM     TO ITEM-CO500
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO500

           MOVE USUARIO-W      TO USUARIO-CO500
           MOVE 0              TO SITUACAO-CO500.
           MOVE GS-DATA-AGENDADA TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO DATA-AGENDA-CO500
           MOVE DATA-DIA-I     TO DATA-CO500 DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV       TO DATA-E
           MOVE SPACES         TO GS-LINDET
           MOVE DATA-E         TO GS-LINDET(1: 10)
           ACCEPT HORA-BRA FROM TIME.
           MOVE HORA-BRA(1: 4) TO HORA-CO500
           MOVE HORA-CO500(1: 2) TO GS-LINDET(13: 2)
           MOVE ":"              TO GS-LINDET(15: 1)
           MOVE HORA-CO500(3: 2) TO GS-LINDET(16: 4)
           MOVE USUARIO-CO500    TO GS-LINDET(20: 5)
           MOVE GS-DATA-AGENDADA TO DATA-E
           MOVE DATA-E           TO GS-LINDET(30: 10)
           MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS TO ST-COD500.
           PERFORM UNTIL ST-COD500 = "10"
              WRITE REG-COD500 INVALID KEY
                 ADD 1 TO ITEM-CO500
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-COD500
           END-PERFORM.

           MOVE NR-CONTRATO-CO500 TO NR-CONTRATO-CO501.
           MOVE ITEM-CO500        TO ITEM-CO501.
           MOVE 0                 TO SUBITEM-CO501.

           PERFORM VARYING COL1 FROM 1 BY 80 UNTIL COL1 > 640
              MOVE GS-ANOTACAO(COL1: 80) TO ANOTACAO-W
              MOVE ANOTACAO-W TO ANOTACAO-CO501
              IF ANOTACAO-W <> SPACES
                 ADD 1 TO SUBITEM-CO501
                 MOVE SPACES         TO GS-LINDET
                 MOVE ANOTACAO-CO501 TO GS-LINDET
                 MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 MOVE ZEROS TO ST-COD501
                 PERFORM UNTIL ST-COD501 = "10"
                  WRITE REG-COD501 INVALID KEY
                       ADD 1 TO SUBITEM-CO501
                     NOT INVALID KEY MOVE "10" TO ST-COD501
                  END-WRITE
                 END-PERFORM
              END-IF
           END-PERFORM.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGAR-DADOS SECTION.
           MOVE GS-CONTRATO      TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                PERFORM LER-NAO-IDENTIFICADO
                PERFORM CONT-CARREGAR-DADOS.

       LER-NAO-IDENTIFICADO SECTION.
            INITIALIZE REG-MTD019 NAO-IDENTIFICADO
            MOVE GS-CONTRATO      TO CONTRATO-MT19
            START MTD019 KEY IS NOT LESS ALBUMMT19 INVALID KEY
               MOVE "10" TO ST-MTD019.
            PERFORM UNTIL ST-MTD019 = "10"
               READ MTD019 NEXT AT END
                   MOVE "10" TO ST-MTD019
               NOT AT END
                   IF GS-CONTRATO <> CONTRATO-MT19
                      MOVE "10" TO ST-MTD019
                   ELSE
                       IF IDENTIFICADO-MT19 = 0
                          ADD 1 TO NAO-IDENTIFICADO
                       END-IF
                   END-IF
               END-READ
            END-PERFORM
            MOVE SPACES TO GS-NAO-IDENTIFICADO
            IF NAO-IDENTIFICADO > 0
               MOVE NAO-IDENTIFICADO TO MASC-NUMERO
               STRING "FORMANDOS NÃO IDENTIFICADOS..: " MASC-NUMERO INTO
               GS-NAO-IDENTIFICADO.

       CONT-CARREGAR-DADOS SECTION.
           EVALUATE ORIGEM-CO40
             WHEN 1 MOVE "KEL"   TO GS-ORIGEM
             WHEN 2 MOVE "MIK"   TO GS-ORIGEM
           END-EVALUATE.

           MOVE QTDE-TURMAS-CO40 TO GS-TURMAS
           MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO
           MOVE INSTITUICAO-CO40 TO CODIGO-IE10
           READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10.
           MOVE NOME-IE10        TO GS-INSTITUICAO
           MOVE CIDADE-CO40      TO CIDADE
           READ CAD010 INVALID KEY INITIALIZE REG-CAD010.
           MOVE NOME-CID         TO GS-CIDADE
           MOVE DDD-CID(2: 3)    TO DDD-W
           MOVE MESANO-PREV-CO40(1: 4) TO MESANO-W(3: 4)
           MOVE MESANO-PREV-CO40(5: 2) TO MESANO-W(1: 2)
           MOVE MESANO-W         TO GS-MESANO
           MOVE QTDE-FORM-CO40   TO GS-NR-FORM
           MOVE PADRAO-CO40      TO GS-PADRAO
           EVALUATE COBERTURA-CO40
             WHEN 1 MOVE "F/V/O" TO GS-SERVICOS
             WHEN 2 MOVE "F/V  " TO GS-SERVICOS
             WHEN 3 MOVE "F/O  " TO GS-SERVICOS
             WHEN 4 MOVE "V/O  " TO GS-SERVICOS
             WHEN 5 MOVE "F    " TO GS-SERVICOS
             WHEN 6 MOVE "V    " TO GS-SERVICOS
             WHEN 7 MOVE "O    " TO GS-SERVICOS
           END-EVALUATE.
           MOVE STATUS-CO40      TO CODIGO-CO01.
           READ COD001 INVALID KEY MOVE SPACES TO STATUS-CO01.
           MOVE STATUS-CO01      TO GS-STATUS.
           MOVE OBS-IMPORTANTE-CO40 TO GS-MENSAGEM.
           MOVE REGIAO-CID       TO CODIGO-REG
           READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG.
           MOVE NOME-REG         TO GS-REGIAO
           MOVE REPRESENTANTE-CO40 TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01        TO GS-REPRESENTANTE
           MOVE ASSINATURA-CO40  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO GS-DATA-ASSINATURA
           MOVE MULTA-CONTRAT-CO40 TO GS-MULTA-CONT
           MOVE CIDADE-FORUM-CO40  TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID         TO GS-FORUM
           MOVE COMERCIALIZACAO-CO40 TO GS-COMERCIALIZACAO
           MOVE QTDE-FORM-INI-CO40 TO NR-INIC-PAD(1: 5)
           MOVE PADRAO-INI-CO40    TO NR-INIC-PAD(6: 1)
           MOVE NR-INIC-PAD        TO GS-NR-INIC-PAD
           MOVE ORGANIZADOR-CO40   TO GS-ORGANIZER(1: 19)
           MOVE "(   )"            TO GS-ORGANIZER(21: 5)
           MOVE DDD-W              TO GS-ORGANIZER(22: 3)
           MOVE FONE-ORGANIZ-CO40 TO FONE-E
           MOVE FONE-E             TO GS-ORGANIZER(27: 9)

      *   CALCULA MESES QUE FALTAM P/ A FORMATURA
           MOVE DATA-DIA-I         TO GRDIAS-AAMMDD-INICIAL
           MOVE DATA-PREV-VENDA-CO40 TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV           TO GRDIAS-AAMMDD-FINAL
           CALL "GRDIAS1" USING PARAMETROS-GRDIAS
           CANCEL "GRDIAS1".
           COMPUTE GS-MESES = GRDIAS-NUM-DIAS / 30


           OPEN INPUT CGD011 CGD012.
           MOVE 0                  TO CLASSIF-CG10.
           MOVE GS-CONTRATO        TO CODIGO-CG10(1: 4)
           MOVE ZEROS              TO CODIGO-CG10(5: 4).
           MOVE SPACES TO COMISSAO(1) COMISSAO(2) COMISSAO(3).
           MOVE ZEROS TO FONE(1) FONE(2) FONE(3) I.
           MOVE CODIGO-CG10        TO CODIGO-CG12
           START CGD012 KEY IS NOT < CODIGO-CG12 INVALID KEY
                 MOVE "10" TO ST-CGD012.
           PERFORM UNTIL ST-CGD012 = "10"
             READ CGD012 NEXT RECORD AT END MOVE "10" TO ST-CGD012
               NOT AT END
                 ADD 1 TO I
                 MOVE CODIGO-CG12(1: 4) TO NR-CONTRATO-W
                 IF NR-CONTRATO-W <> GS-CONTRATO OR I > 3
                    MOVE "10" TO ST-CGD012
                 ELSE
                   IF CARGO-COMISSAO-CG12 < 1 OR > 4 CONTINUE
                   ELSE
                     MOVE CODIGO-CG12 TO CODIGO-CG10
                     READ CGD010 INVALID KEY SUBTRACT 1 FROM I
                                             CONTINUE
                      NOT INVALID KEY
                         MOVE COMPRADOR-CG10 TO COMISSAO(I)
                         MOVE CODIGO-CG10 TO CODIGO-CG11
                         READ CGD011 INVALID KEY INITIALIZE REG-CGD011
                         END-READ
                         MOVE FONE1-CG11 TO FONE(I)
                     END-READ
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.
           CLOSE CGD011 CGD012.

           MOVE COMISSAO(1)        TO GS-COMISSAO1(1: 19)
           MOVE "(   )"            TO GS-COMISSAO1(21: 5)
           MOVE DDD-W              TO GS-COMISSAO1(22: 3)
           MOVE FONE(1)            TO FONE-E
           MOVE FONE-E             TO GS-COMISSAO1(27: 9)
           MOVE COMISSAO(2)        TO GS-COMISSAO2(1: 19)
           MOVE "(   )"            TO GS-COMISSAO2(21: 5)
           MOVE DDD-W              TO GS-COMISSAO2(22: 3)
           MOVE FONE(2)            TO FONE-E
           MOVE FONE-E             TO GS-COMISSAO2(27: 9)
           MOVE COMISSAO(3)        TO GS-COMISSAO3(1: 19)
           MOVE "(   )"            TO GS-COMISSAO3(21: 5)
           MOVE DDD-W              TO GS-COMISSAO3(22: 3)
           MOVE FONE(3)            TO FONE-E
           MOVE FONE-E             TO GS-COMISSAO3(27: 9)
           MOVE CONTATO-CONVITE-CO40 TO GS-CONVITE
           MOVE CONTATO-BECA-CO40  TO GS-BECA
           MOVE CONTATO-OUTRO-CO40 TO GS-OUTRO.
           display "brinde " stop " "
           PERFORM CARREGAR-DADOS-BRINDE.
           display "sai brinde " stop " "

           display "brinde oe" stop " "
           PERFORM CARREGAR-DADOS-BRINDE-OE.
           display "sai brinde oe" stop " "
           display "evento " stop " "
           PERFORM CARREGAR-DADOS-EVENTO.
           display "sai evento " stop " "
           display "cursos " stop " "
           PERFORM CARREGAR-DADOS-CURSOS.
           display "sai cursos" stop " "
           display "contatos " stop " "
           PERFORM CARREGAR-DADOS-CONTATOS.
           display "sai contatos " stop " "
           display "credito" stop " "
           PERFORM CARREGAR-DADOS-CREDITO.
           display "sai credito" stop " "
           display "resumo " stop " "
           PERFORM CARREGAR-DADOS-RESUMO-OE.
           display "sai resumo" stop " "
           display "05     " stop " "
           PERFORM CARREGAR-DADOS-05.
           display "sai 05 " stop " "
           display "report " stop " "
           PERFORM CARREGAR-DADOS-REPORTAGEM.
           display "sai report" stop " "
           display "resumo " stop " "
           PERFORM CARREGAR-DADOS-RESUMO.
           display "sai resumo " stop " "
           display "convite" stop " "
           PERFORM CARREGAR-CONVITES
           display "sai convi " stop " "
           MOVE "SENHA05" TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
                MOVE "NAO-AUTORIZADO" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGAR-CONVITES SECTION.
           MOVE 0 TO GS-CONT-CONVITE
           MOVE "CLEAR-LIST-CONVITE" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-PRD012
           MOVE GS-CONTRATO TO CONTRATO-PR12
           START PRD012 KEY IS NOT LESS CHAVE-PR12 INVALID KEY
               MOVE "10" TO ST-PRD012.

           PERFORM UNTIL ST-PRD012 = "10"
               READ PRD012 NEXT RECORD AT END
                   MOVE "10" TO ST-PRD012
               NOT AT END
                   IF GS-CONTRATO <> CONTRATO-PR12
                      MOVE "10" TO ST-PRD012
                   ELSE
                      MOVE OBSERVACAO-PR12 TO GS-LINDET
                      MOVE "INSERE-LIST-CONVITE" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.
       CARREGAR-DADOS-BRINDE SECTION.
           MOVE ZEROS TO CUSTO-TOTAL CUSTO-PREVISTO CUSTO-W JUROS-W
                         CONT TAXA-ACUMULADA SOMACUSTO-W.
           MOVE "CLEAR-LIST-BOX-BRINDE" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
      *    ZERA VARIAVEIS DO RESUMO
           MOVE ZEROS TO VLR-PAGO-RES VLR-PAGAR-RES JUROS-PAGO-RES
                         JUROS-PAGAR-RES CUSTO-BRINDE-FORM-RES
                         CRED-RECEB-RES CRED-FUT-RES DIAS-CRED-FUT-RES
                         JUROS-CRED-REC-RES JUROS-CRED-FUT-RES
                         DIAS-PAGO-RES DIAS-PAGAR-RES DIAS-CRED-REC-RES.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO50.
           MOVE ZEROS              TO ITEM-CO50 GS-CONT-BRINDE
                                      DATA-VENCTO-CO50 REALIZADO-CO50.
           MOVE "N" TO ENTREI
           START COD050 KEY IS NOT < ALT1-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.

           PERFORM UNTIL ST-COD050 = "10"
              READ COD050 NEXT RECORD AT END
                   MOVE "10" TO ST-COD050
                   IF ENTREI = "S"
                      MOVE SPACES TO GS-LINDET
                      MOVE SOMACUSTO-W TO VALOR-E1
                      MOVE VALOR-E1 TO GS-LINDET(90: 11)
                      MOVE "INSERE-LIST-BRINDE" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
                NOT AT END
                  IF NR-CONTRATO-CO50 <> GS-CONTRATO
                     MOVE "10" TO ST-COD050
                     MOVE SPACES TO GS-LINDET
                     MOVE SOMACUSTO-W TO VALOR-E1
                     MOVE VALOR-E1 TO GS-LINDET(90: 11)
                     MOVE "INSERE-LIST-BRINDE" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  ELSE
                     IF SUSP-PREV-DEF-CO50 <> 2
                        move "S" to entrei
                        MOVE SPACES       TO GS-LINDET
                        MOVE ITEM-CO50    TO GS-LINDET(1: 3)
                        MOVE CODBRINDE-CO50 TO CODIGO-CO02
                        READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02
                        END-READ
                        MOVE NOME-CO02    TO GS-LINDET(4: 14)
                        MOVE QTDE-POR-FORM-CO50 TO QTDE5-E
                        MOVE QTDE5-E            TO GS-LINDET(19: 6)
                        MOVE QTDE-FORM-CO50     TO QTDE4-E
                        MOVE QTDE4-E            TO GS-LINDET(25: 5)
                        PERFORM CALCULA-CUSTO-BRINDE
                        MOVE CUSTO-PREVISTO     TO VALOR-E1
                        MOVE VALOR-E1           TO GS-LINDET(30: 11)
                        MOVE CUSTO-W            TO VALOR-E1
                        MOVE VALOR-E1           TO GS-LINDET(41: 11)
                        MOVE DATA-VENCTO-CO50   TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV           TO DATA-E
                        MOVE DATA-E             TO GS-LINDET(52: 11)
                        MOVE DATA-PAGTO-CO50    TO DATA-E
                        MOVE DATA-E             TO GS-LINDET(63: 11)
                        MOVE DIAS-PRAZO-CO50    TO GS-LINDET(74: 5)
                        PERFORM CALCULO-JUROS-BRINDE
                        MOVE JUROS-W            TO VALOR-E1
                        MOVE VALOR-E1           TO GS-LINDET(79: 11)
                        IF DATA-PAGTO-CO50 <> ZEROS
                           ADD CUSTO-W          TO VLR-PAGO-RES
                           ADD JUROS-W          TO JUROS-PAGO-RES
                           ADD DIAS-PRAZO-CO50  TO DIAS-PAGO-RES
                        ELSE
                           ADD CUSTO-W         TO VLR-PAGAR-RES
                           ADD JUROS-W         TO JUROS-PAGAR-RES
                           ADD DIAS-PRAZO-CO50 TO DIAS-PAGAR-RES
                        END-IF
                        COMPUTE CUSTO-TOTAL = CUSTO-W + JUROS-W
                        MOVE CUSTO-TOTAL        TO VALOR-E1
                        ADD CUSTO-TOTAL         TO SOMACUSTO-W
                        MOVE VALOR-E1           TO GS-LINDET(90: 11)

                        MOVE CURSO-CO50     TO CODIGO-IE11
                        READ IED011 INVALID KEY
                           MOVE SPACES      TO NOME-REDUZ-IE11
                        END-READ
                        MOVE NOME-REDUZ-IE11  TO GS-LINDET(102:13)
                        MOVE TURMA-CO50       TO GS-LINDET(115:2)


                        MOVE "INSERE-LIST-BRINDE" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
                  END-IF
              END-READ
           END-PERFORM.
       CALCULA-CUSTO-BRINDE SECTION.
      *  se o brinde já foi pago então o custo será conforme o
      *  valor-pago, senão verifica se o brinde é do tipo mult-por-form
      *  então multiplica qtde-form pela qtde-por-form e custo-previsto,
      *  onde o custo-previsto é o custo unitário do brinde caso ele
      *  tenha sido preenchido, senão o custo-previsto virá do cadastro
      *  do brinde
           IF CUSTO-UNIT-CO50 <> ZEROS
              MOVE CUSTO-UNIT-CO50 TO CUSTO-PREVISTO
           ELSE MOVE VALOR-CO02    TO CUSTO-PREVISTO
           END-IF.

           IF REALIZADO-CO50 <> 1
              IF MULT-FORM-CO02 = 1
                 COMPUTE CUSTO-W = (QTDE-POR-FORM-CO50 *
                    QTDE-FORM-CO50) * CUSTO-PREVISTO
              ELSE
                 COMPUTE CUSTO-W = QTDE-POR-FORM-CO50 * CUSTO-PREVISTO
              END-IF
           ELSE MOVE VALOR-PAGO-CO50   TO CUSTO-W
           END-IF.

       CALCULO-JUROS-BRINDE SECTION.
           COMPUTE MESES-W = DIAS-PRAZO-CO50 / 30.
           MOVE MESES-W TO PRAZO-MEDIO
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-W * (TAXA-ACUMULADA - 1).
       CARREGAR-DADOS-BRINDE-OE SECTION.
           OPEN INPUT OED020.
           IF ST-OED020 <> "00"
              MOVE "ERRO ABERTURA OED020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE ZEROS TO CUSTO-TOTAL CUSTO-PREVISTO CUSTO-W JUROS-W
                         CONT TAXA-ACUMULADA.
           MOVE ZEROS TO VLR-PAGO-RES-OE VLR-PAGAR-RES-OE
                         JUROS-PAGO-RES-OE JUROS-PAGAR-RES-OE

           MOVE "CLEAR-LIST-BOX-BRINDE-OE" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
      *    ZERA VARIAVEIS DO RESUMO
           MOVE GS-CONTRATO        TO NR-CONTRATO-OE20.
           MOVE ZEROS              TO ITEM-OE20 GS-CONT-BRINDE
                                      DATA-VENCTO-OE20 REALIZADO-OE20.
           START OED020 KEY IS NOT < ALT1-OE20 INVALID KEY
                 MOVE "10" TO ST-OED020.
           PERFORM UNTIL ST-OED020 = "10"
              READ OED020 NEXT RECORD AT END MOVE "10" TO ST-OED020
                NOT AT END
                  IF NR-CONTRATO-OE20 <> GS-CONTRATO
                     MOVE "10" TO ST-OED020
                  ELSE
                     MOVE SPACES       TO GS-LINDET
                     MOVE ITEM-OE20    TO GS-LINDET(1: 3)
                     MOVE CODBRINDE-OE20 TO CODIGO-CO02
                     READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02
                     END-READ
                     MOVE NOME-CO02    TO GS-LINDET(4: 14)
                     MOVE QTDE-POR-FORM-OE20 TO QTDE5-E
                     MOVE QTDE5-E            TO GS-LINDET(19: 6)
                     MOVE QTDE-FORM-OE20     TO QTDE4-E
                     MOVE QTDE4-E            TO GS-LINDET(25: 5)
                     PERFORM CALCULA-CUSTO-BRINDE-OE
                     MOVE CUSTO-PREVISTO     TO VALOR-E1
                     MOVE VALOR-E1           TO GS-LINDET(30: 11)
                     MOVE CUSTO-W            TO VALOR-E1
                     MOVE VALOR-E1           TO GS-LINDET(41: 11)
                     MOVE DATA-VENCTO-OE20   TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV            TO DATA-E
                     MOVE DATA-E              TO GS-LINDET(52: 11)
                     MOVE DATA-PAGTO-OE20     TO DATA-E
                     MOVE DATA-E              TO GS-LINDET(63: 11)
                     MOVE DIAS-PRAZO-OE20     TO GS-LINDET(74: 5)
                     PERFORM CALCULO-JUROS-BRINDE-OE
                     MOVE JUROS-W             TO VALOR-E1
                     MOVE VALOR-E1            TO GS-LINDET(79: 11)
                     IF DATA-PAGTO-OE20 <> ZEROS
                        ADD CUSTO-W           TO VLR-PAGO-RES-OE
                        ADD JUROS-W           TO JUROS-PAGO-RES-OE
                        ADD DIAS-PRAZO-OE20   TO DIAS-PAGO-RES-OE
                     ELSE ADD CUSTO-W         TO VLR-PAGAR-RES-OE
                          ADD JUROS-W         TO JUROS-PAGAR-RES-OE
                          ADD DIAS-PRAZO-OE20 TO DIAS-PAGAR-RES-OE
                     END-IF
                     COMPUTE CUSTO-TOTAL = CUSTO-W + JUROS-W
                     MOVE CUSTO-TOTAL         TO VALOR-E1
                     MOVE VALOR-E1            TO GS-LINDET(90: 11)

                     MOVE CURSO-OE20          TO CODIGO-IE11
                     READ IED011 INVALID KEY
                        MOVE SPACES      TO NOME-REDUZ-IE11
                     END-READ
                     MOVE NOME-REDUZ-IE11  TO GS-LINDET(102:13)
                     MOVE TURMA-OE20       TO GS-LINDET(115:2)

                     MOVE "INSERE-LIST-BRINDE-OE" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-READ
           END-PERFORM.
           CLOSE OED020.
       CALCULA-CUSTO-BRINDE-OE SECTION.
      *  se o brinde já foi pago então o custo será conforme o
      *  valor-pago, senão verifica se o brinde é do tipo mult-por-form
      *  então multiplica qtde-form pela qtde-por-form e custo-previsto,
      *  onde o custo-previsto é o custo unitário do brinde caso ele
      *  tenha sido preenchido, senão o custo-previsto virá do cadastro
      *  do brinde
           IF CUSTO-UNIT-OE20 <> ZEROS
              MOVE CUSTO-UNIT-OE20 TO CUSTO-PREVISTO
           ELSE MOVE VALOR-CO02    TO CUSTO-PREVISTO
           END-IF.

           IF REALIZADO-OE20 <> 1
              IF MULT-FORM-CO02 = 1
                 COMPUTE CUSTO-W = (QTDE-POR-FORM-OE20 *
                    QTDE-FORM-OE20) * CUSTO-PREVISTO
              ELSE
                 COMPUTE CUSTO-W = QTDE-POR-FORM-OE20 * CUSTO-PREVISTO
              END-IF
           ELSE MOVE VALOR-PAGO-OE20   TO CUSTO-W
           END-IF.

       CALCULO-JUROS-BRINDE-OE SECTION.
           COMPUTE MESES-W = DIAS-PRAZO-OE20 / 30.
           COMPUTE PRAZO-MEDIO ROUNDED = (CUSTO-PREVISTO * MESES-W) /
                                  CUSTO-PREVISTO.
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-W * (TAXA-ACUMULADA - 1).
       CALCULO-JUROS-CREDITO SECTION.
           COMPUTE MESES-W = DIAS-PRAZO-CO70 / 30.
           COMPUTE PRAZO-MEDIO ROUNDED = (VALOR-PREVISTO-CO70
                * MESES-W) / VALOR-PREVISTO-CO70.
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = VALOR-PREVISTO-CO70 * (TAXA-ACUMULADA - 1).
       CARREGAR-DADOS-EVENTO SECTION.

           MOVE "CLEAR-LIST-BOX-EVENTO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-CONTRATO   TO NR-CONTRATO-CO60.
           MOVE ZEROS      TO ITEM-CO60 GS-CONT-EVENTO DATAREALIZA-CO60.
           START COD060 KEY IS NOT < ALT1-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.
           PERFORM UNTIL ST-COD060 = "10"
              READ COD060 NEXT RECORD AT END MOVE "10" TO ST-COD060
                NOT AT END
                  IF NR-CONTRATO-CO60 <> GS-CONTRATO
                     MOVE "10" TO ST-COD060
                  ELSE
                    MOVE SPACES           TO GS-LINDET
                    MOVE ITEM-CO60        TO GS-LINDET(1: 4)
                    MOVE CODEVENTO-CO60   TO CODIGO-CO03
                    READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03
                    END-READ
                    MOVE NOME-CO03            TO GS-LINDET(5: 20)
                    MOVE QT-PARTICIPANTE-CO60 TO QTDE4-E
                    MOVE QTDE4-E              TO GS-LINDET(27: 7)
                    MOVE DATAREALIZA-CO60     TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV             TO DATA-E
                    MOVE DATA-E               TO GS-LINDET(34: 11)
                    MOVE HORARIO-CO60         TO GS-LINDET(45: 9)
                    EVALUATE FOTO-CO60
                      WHEN 0 MOVE "N"         TO GS-LINDET(54: 3)
                      WHEN 1 MOVE "S"         TO GS-LINDET(54: 3)
                    END-EVALUATE
                    EVALUATE VIDEO-CO60
                      WHEN 0 MOVE "N"         TO GS-LINDET(57: 3)
                      WHEN 1 MOVE "S"         TO GS-LINDET(57: 3)
                    END-EVALUATE
                    MOVE QT-TELAO-CO60        TO GS-LINDET(60: 4)
                    EVALUATE BECA-CO60
                      WHEN 0 MOVE "N"         TO GS-LINDET(64: 3)
                      WHEN 1 MOVE "S"         TO GS-LINDET(64: 3)
                    END-EVALUATE
                    EVALUATE CLIP-CO60
                      WHEN 0 MOVE "N"         TO GS-LINDET(67: 3)
                      WHEN 1 MOVE "S"         TO GS-LINDET(67: 3)
                    END-EVALUATE
                    EVALUATE FAX-CO60
                      WHEN 0 MOVE " N"        TO GS-LINDET(70: 3)
                      WHEN 1 MOVE " S"        TO GS-LINDET(70: 3)
                    END-EVALUATE
                    EVALUATE APROVACAO-CO60
                      WHEN 0 MOVE "N"         TO GS-LINDET(73: 3)
                      WHEN 1 MOVE "S"         TO GS-LINDET(73: 3)
                    END-EVALUATE

                    MOVE "F4 - CONSULTA"      TO GS-LINDET(78:15)

                    MOVE "INSERE-LIST-EVENTO" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-READ
           END-PERFORM.
       TRAZER-CURSO SECTION.
           INITIALIZE REG-COD061 GS-CONT-CURTUR
           MOVE GS-CONTRATO     TO NR-CONTRATO-CO61
           MOVE GS-LINDET(1: 3) TO ITEM-CO61 AUX-ITEM
           COMPUTE ITEM-CO61 = ITEM-CO61 - 1

           START COD061 KEY IS NOT LESS CHAVE-CO61 INVALID KEY
                 MOVE "10" TO ST-COD061.
           PERFORM UNTIL ST-COD061 = "10"
              READ COD061 NEXT RECORD AT END
                   MOVE "10" TO ST-COD061
              NOT AT END
                   IF GS-CONTRATO <> NR-CONTRATO-CO61
                      MOVE "10" TO ST-COD061
                   ELSE
                      IF AUX-ITEM = ITEM-CO61
                         ADD 1 TO GS-CONT-CURTUR
                         MOVE CURSO-CO61          TO CODIGO-IE11
                         READ IED011 INVALID KEY
                              MOVE SPACES      TO NOME-REDUZ-IE11
                         END-READ
                         MOVE NOME-REDUZ-IE11 TO
                                           GS-TAB-CURSO(GS-CONT-CURTUR)
                         MOVE TURMA-CO61      TO
                                          GS-TAB-TURMA(GS-CONT-CURTUR)
                      END-IF
                   END-IF
              END-READ
           END-PERFORM

           MOVE "INSERIR-CUR" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


       CARREGAR-DADOS-CURSOS SECTION.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO41.
           MOVE ZEROS       TO CURSO-CO41 GS-CONT-CURSO.
           MOVE SPACES      TO TURMA-CO41.
           START COD041 KEY IS NOT < CHAVE-CO41 INVALID KEY
                 MOVE "10" TO ST-COD041.
           PERFORM UNTIL ST-COD041 = "10"
              READ COD041 NEXT RECORD AT END MOVE "10" TO ST-COD041
                NOT AT END
                  IF NR-CONTRATO-CO41 <> GS-CONTRATO
                     MOVE "10" TO ST-COD041
                  ELSE
                    MOVE SPACES        TO GS-LINDET
                    MOVE CURSO-CO41    TO CODIGO-IE11
                    READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11
                    END-READ
                    MOVE NOME-IE11         TO GS-LINDET(1: 25)
                    MOVE TURMA-CO41        TO GS-LINDET(27: 6)
                    MOVE NR-PREV-FORM-CO41 TO GS-LINDET(33: 5)
                    MOVE LISTA-ALUNOS-CO41 TO GS-LINDET(38: 2)
                    MOVE GS-CONTRATO       TO CODIGO-CG10(1: 4)
                    MOVE RESPONSAVEL-CO41  TO CODIGO-CG10(5: 4)
                    MOVE 0                 TO CLASSIF-CG10
                    READ CGD010 INVALID KEY MOVE SPACES TO
                         COMPRADOR-CG10
                    END-READ
                    MOVE COMPRADOR-CG10    TO GS-LINDET(40: 10)
                    MOVE FOTO-TURMA-CO41   TO GS-LINDET(51: 3)
                    MOVE MEDIDA-BECA-CO41  TO GS-LINDET(54: 3)
                    MOVE COR-FAIXA-CO41    TO GS-LINDET(57: 11)
                    MOVE COR-CANUDO-CO41   TO GS-LINDET(68: 11)
                    MOVE TIPO-ALBUM-CO41   TO GS-LINDET(79: 6)
                    MOVE "INSERE-LIST-CURSOS" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-READ.
       CARREGAR-DADOS-CREDITO SECTION.
           MOVE ZEROS TO CUSTO-TOTAL CUSTO-PREVISTO CUSTO-W JUROS-W
                         CONT TAXA-ACUMULADA.
           MOVE "CLEAR-LIST-BOX-CREDITO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO70.
           MOVE ZEROS              TO ITEM-CO70 GS-CONT-CREDITO
                                      DATA-VENCTO-CO70 REALIZADO-CO70.
           START COD070 KEY IS NOT < ALT1-CO70 INVALID KEY
                 MOVE "10" TO ST-COD070.
           PERFORM UNTIL ST-COD070 = "10"
              READ COD070 NEXT RECORD AT END MOVE "10" TO ST-COD070
                NOT AT END
                  IF NR-CONTRATO-CO70 <> GS-CONTRATO
                     MOVE "10" TO ST-COD070
                  ELSE
                     MOVE SPACES       TO GS-LINDET
                     MOVE ITEM-CO70    TO GS-LINDET(1: 3)
                     MOVE DESCRICAO-CO70 TO GS-LINDET(4: 41)
                     MOVE VALOR-PREVISTO-CO70 TO VALOR-E1
                     MOVE VALOR-E1           TO GS-LINDET(45: 11)
                     MOVE DATA-VENCTO-CO70   TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV           TO DATA-E
                     MOVE DATA-E             TO GS-LINDET(56: 11)
                     MOVE DATA-RECTO-CO70    TO DATA-E
                     MOVE DATA-E             TO GS-LINDET(67: 11)
                     MOVE DIAS-PRAZO-CO70    TO GS-LINDET(78: 5)
                     PERFORM CALCULO-JUROS-CREDITO
                     MOVE JUROS-W            TO VALOR-E1
                     MOVE VALOR-E1           TO GS-LINDET(83: 11)
                     IF DATA-RECTO-CO70 <> ZEROS
                        ADD VALOR-PREVISTO-CO70 TO CRED-RECEB-RES
                        ADD JUROS-W             TO JUROS-CRED-REC-RES
                        ADD DIAS-PRAZO-CO70     TO DIAS-CRED-REC-RES
                     ELSE ADD VALOR-PREVISTO-CO70 TO CRED-FUT-RES
                          ADD JUROS-W             TO JUROS-CRED-FUT-RES
                          ADD DIAS-PRAZO-CO70     TO DIAS-CRED-FUT-RES
                     END-IF
                     COMPUTE CUSTO-TOTAL = VALOR-PREVISTO-CO70 + JUROS-W
                     MOVE CUSTO-TOTAL        TO VALOR-E1
                     MOVE VALOR-E1           TO GS-LINDET(94: 11)
                     MOVE "INSERE-LIST-CREDITO" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-READ
           END-PERFORM.
       CARREGAR-DADOS-05 SECTION.
           MOVE "CLEAR-LIST-BOX-05" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 9999 TO CONTRATO-CO007.
           READ COD007 INVALID KEY MOVE SPACES TO PART1-CO007.
           MOVE PART1-CO007 TO GS-LINDET.
           MOVE "INSERE-LIST-05" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST-05" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-CIDADE           TO NOME-CID-CO007.
           START COD007 KEY IS NOT < NOME-CID-CO007 INVALID KEY
                 MOVE "10" TO ST-COD007.
           PERFORM UNTIL ST-COD007 = "10"
              READ COD007 NEXT RECORD AT END MOVE "10" TO ST-COD007
                NOT AT END
                  IF NOME-CID-CO007 <> GS-CIDADE
                     MOVE "10" TO ST-COD007
                  ELSE
                     MOVE SPACES       TO GS-LINDET
                     MOVE PART1-CO007  TO GS-LINDET
                     MOVE REPRESENTANTE-CO007 TO GS-LINDET(20: 9)
                     MOVE "INSERE-LIST-05" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     MOVE SPACES       TO GS-LINDET
                     MOVE PART2-CO007  TO GS-LINDET
                     MOVE "INSERE-LIST-05" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     MOVE SPACES       TO GS-LINDET
                     MOVE "INSERE-LIST-05" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-READ
           END-PERFORM.
       CARREGAR-DADOS-REPORTAGEM SECTION.
           OPEN INPUT RED100 RED101.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           PERFORM GRAVA-DOCTO-REPORTAGEM.
           MOVE ZEROS TO VLR-REPORTAGEM-RES TOT-FITA-RES TOT-FILME-RES
                         DIAS-REPORTAGEM-RES JUROS-REPORTAGEM-RES.

           MOVE "CLEAR-LIST-BOX-REPORTAGEM" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
      *    ZERA VARIAVEIS DO RESUMO
           MOVE ZEROS TO GS-CONT-REPORTAGEM.

           MOVE ZEROS TO DOCTO-WK1.
           START WORK1 KEY IS NOT < DOCTO-WK1 INVALID KEY
                       MOVE "10" TO ST-WORK1.
           PERFORM UNTIL ST-WORK1 = "10"
             READ WORK1 NEXT RECORD AT END MOVE "10" TO ST-WORK1
              NOT AT END
               MOVE DOCTO-WK1   TO DOCTO-R100
               READ RED100 INVALID KEY INITIALIZE REG-RED100
               END-READ
               PERFORM CALCULA-DIAS-REPORTAGEM
               PERFORM VERIFICA-CONT-EVENTO
             END-READ
           END-PERFORM.
           CLOSE RED100 RED101.
       GRAVA-DOCTO-REPORTAGEM SECTION.
           CLOSE WORK1  OPEN OUTPUT WORK1  CLOSE WORK1  OPEN I-O WORK1.
           MOVE GS-CONTRATO    TO CONTRATO-R101.
           START RED101 KEY IS NOT < CONTRATO-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END MOVE "10" TO ST-RED101
               NOT AT END
                 IF GS-CONTRATO <> CONTRATO-R101
                    MOVE "10" TO ST-RED101
                 ELSE
                    MOVE DOCTO-R101  TO DOCTO-WK1
                    READ WORK1 INVALID KEY WRITE REG-WORK1
                    END-READ
                 END-IF
             END-READ
           END-PERFORM.
       VERIFICA-CONT-EVENTO SECTION.
           PERFORM VERIFICA-TOTAL-PARTICIPANTE
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE GS-CONTRATO         TO CONTRATO-R101.
           MOVE ZEROS               TO EVENTO-R101.
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END MOVE "10" TO ST-RED101
               NOT AT END
                IF DOCTO-R101 <> DOCTO-R100 OR
                   CONTRATO-R101 <> GS-CONTRATO
                     MOVE "10" TO ST-RED101
                ELSE
                 MOVE DOCTO-R101          TO GS-LINDET(1: 7)
                 MOVE DATA-MOV-R100       TO DATA-INV
                 CALL "GRIDAT1" USING DATA-INV
                 MOVE DATA-INV            TO DATA-E GRTIME-DATE
                 MOVE DATA-E              TO GS-LINDET(8: 11)
                 MOVE EVENTO-R101         TO CODIGO-CO03
                 READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03
                 END-READ
                 MOVE NOME-CO03           TO GS-LINDET(19: 21)

                 COMPUTE QT-FILME-REP ROUNDED = (TOT-FILME-REPORT-R100
                                 * QT-PARTIC-R101) / TOT-PARTICIPANTE
                 ADD QT-FILME-REP         TO TOT-FILME-RES
                 MOVE QT-FILME-REP        TO QTDE-E
                 MOVE QTDE-E              TO GS-LINDET(40: 8)
                 COMPUTE QT-FITA-REP ROUNDED = (TOT-FITA-REPORT-R100
                          * QT-PARTIC-R101) / TOT-PARTICIPANTE
                 ADD QT-FITA-REP          TO TOT-FITA-RES
                 MOVE QT-FITA-REP         TO QTDE-E
                 MOVE QTDE-E              TO GS-LINDET(48: 8)
                 COMPUTE QT-EQUIPE-REP ROUNDED = (QTDE-PESSOAS-R100 *
                                   QT-PARTIC-R101) / TOT-PARTICIPANTE
      *          MOVE QT-EQUIPE-REP       TO QTDE-E
      *          MOVE QTDE-E              TO GS-LINDET(51: 8)
                 COMPUTE DESPESA-W = VLR-COMB-R100 + VLR-HOSP-R100 +
                   VLR-REFEICAO-R100 + VLR-PASSAGEM-R100 + VLR-MAT-R100
                   + VLR-DESPESA-REPORT-R100 +
                   VLR-ALUGUEL-R100 + VLR-OUTROS-R100
                 COMPUTE VLR-DESPESA-REP ROUNDED =
                     (DESPESA-W * QT-PARTIC-R101) / TOT-PARTICIPANTE
                 MOVE VLR-DESPESA-REP     TO VALOR-E1
                 MOVE VALOR-E1            TO GS-LINDET(56: 11)
                 COMPUTE VLR-REPORT-REP ROUNDED = (VLR-TOT-REPORT-R100 *
                                   QT-PARTIC-R101) / TOT-PARTICIPANTE
                 MOVE VLR-REPORT-REP      TO VALOR-E1
                 MOVE VALOR-E1            TO GS-LINDET(67: 11)
                 COMPUTE VLR-REPORTAGEM-RES = VLR-REPORTAGEM-RES +
                     (VLR-REPORT-REP + VLR-DESPESA-REP)


      *          CALCULA JUROS DE REPORTAGEM

                 COMPUTE MESES-W = GRTIME-DAYS-FINAL / 30
                 COMPUTE PM-W = GRTIME-DAYS-FINAL / 30
                 MOVE MESES-W     TO PRAZO-MEDIO
                 MOVE 1           TO TAXA-ACUMULADA
                 PERFORM VARYING CONT FROM 1 BY 1 UNTIL
                            CONT > PRAZO-MEDIO
                     COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                              ((GS-TAXA / 100) + 1)
                 END-PERFORM

                 COMPUTE JUROS-W = (VLR-REPORT-REP + VLR-DESPESA-REP) *
                                   (TAXA-ACUMULADA - 1)
                 ADD JUROS-W      TO JUROS-REPORTAGEM-RES

                 MOVE PM-W                TO PM-E
                 MOVE PM-E                TO GS-LINDET(78: 8)
                 MOVE JUROS-W             TO VALOR-E1
                 MOVE VALOR-E1            TO GS-LINDET(86: 11)

                 COMPUTE TOTAL-REPORT = VLR-REPORT-REP + VLR-DESPESA-REP
                                        + JUROS-W

                 MOVE TOTAL-REPORT        TO VALOR-E
                 MOVE VALOR-E             TO GS-LINDET(97: 13)

                 MOVE "INSERE-LIST-REPORTAGEM" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM

             END-READ
           END-PERFORM.
       CALCULA-DIAS-REPORTAGEM SECTION.
           MOVE 1                    TO GRTIME-TYPE
           MOVE 3                    TO GRTIME-FUNCTION
           MOVE DATA-MOV-R100        TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO GRTIME-DATE
           MOVE DATA-PREV-VENDA-CO40 TO GRTIME-DATE-FINAL
           CALL "GRTIME" USING PARAMETROS-GRTIME
           CANCEL "GRTIME"
           ADD GRTIME-DAYS-FINAL     TO DIAS-REPORTAGEM-RES.

       VERIFICA-TOTAL-PARTICIPANTE SECTION.
           MOVE ZEROS               TO TOT-PARTICIPANTE.
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE ZEROS               TO CONTRATO-R101.
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END MOVE "10" TO ST-RED101
               NOT AT END
                IF DOCTO-R101 <> DOCTO-R100
                     MOVE "10" TO ST-RED101
                ELSE
                  ADD QT-PARTIC-R101   TO TOT-PARTICIPANTE
                END-IF
             END-READ
           END-PERFORM.
       CARREGAR-DADOS-RESUMO SECTION.
           MOVE "CLEAR-LIST-BOX-RESUMO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS  TO GS-CONT-RESUMO.
           MOVE "Brindes pagos"    TO GS-LINDET(1: 41)
           MOVE VLR-PAGO-RES       TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(42: 16)
           COMPUTE PM-W = ((VLR-PAGO-RES * DIAS-PAGO-RES) /
                          VLR-PAGO-RES) / 30
           MOVE PM-W               TO PM-E
           MOVE PM-E               TO GS-LINDET(61: 08)
           MOVE JUROS-PAGO-RES     TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(69: 16)
           COMPUTE CUSTO-TOTAL = VLR-PAGO-RES + JUROS-PAGO-RES
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(85: 16)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "Brindes a pagar"  TO GS-LINDET(1: 41)
           MOVE VLR-PAGAR-RES      TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(42: 16)
           COMPUTE PM-W = ((VLR-PAGAR-RES * DIAS-PAGAR-RES) /
                          VLR-PAGAR-RES) / 30
           MOVE PM-W               TO PM-E
           MOVE PM-E               TO GS-LINDET(61: 8)
           MOVE JUROS-PAGAR-RES    TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(69: 16)
           COMPUTE CUSTO-TOTAL = VLR-PAGAR-RES + JUROS-PAGAR-RES
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(85: 16)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "TOTAL-R$....."    TO GS-LINDET(1: 41)
           COMPUTE CUSTO-TOTAL = VLR-PAGO-RES + VLR-PAGAR-RES
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(42: 16)
           COMPUTE JUROS-W = JUROS-PAGO-RES + JUROS-PAGAR-RES
           MOVE JUROS-W            TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(69: 16)
           COMPUTE CUSTO-TOTAL = JUROS-W + CUSTO-TOTAL
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(85: 16)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "Custo brindes/form" TO GS-LINDET(1: 41)
           COMPUTE CUSTO-TOTAL = CUSTO-TOTAL / GS-NR-FORM
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(85: 16)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           ADD 1 TO GS-CONT-RESUMO
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           OPEN INPUT COD005 COD006.
           MOVE GS-CONTRATO   TO NR-CONTRATO-CO06.
           READ COD006 INVALID KEY INITIALIZE REG-COD006.
           MOVE GS-PADRAO          TO PADRAO-CO05.
           READ COD005 INVALID KEY INITIALIZE REG-COD005.
           IF COBERTURA-CO40 = 1 OR 2 OR 4 OR 6 CONTINUE
           ELSE MOVE ZEROS TO FITA-CO06.
      *    NÃO TEM COBERTURA DE FITA
           IF COBERTURA-CO40 = 1 OR 2 OR 3 OR 5 CONTINUE
           ELSE MOVE ZEROS TO ALBUM-CO06 FOTO-CO06.
      *    NÃO TEM COBERTURA DE FOTO CONSEQUENTEMENTE DE ALBUM
           COMPUTE PREV-FOTOS-W = PREV-FOTOS-CO05 * GS-NR-FORM.
           COMPUTE CUSTO-TOTAL =
              (((IND-ALBUM-CO06 / 100) * ALBUM-CO06) * GS-NR-FORM) +
              (((IND-FITA-CO06 / 100) * FITA-CO06) * GS-NR-FORM) +
              (((IND-FOTO-CO06 / 100) * FOTO-CO06) * PREV-FOTOS-W).
           MOVE CUSTO-TOTAL            TO VALOR-E
           MOVE "Faturamento previsto" TO GS-LINDET(1: 41)
           MOVE VALOR-E                TO GS-LINDET(85: 16)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO"   TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE COD005 COD006.

           MOVE SPACES TO GS-LINDET
           MOVE "(Taxa " TO GS-LINDET(1: 41)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           ADD 1 TO GS-CONT-RESUMO
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "Creditos recebidos"    TO GS-LINDET(1: 41)
           MOVE CRED-RECEB-RES     TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(42: 16)
           COMPUTE PM-W = ((CRED-RECEB-RES * DIAS-CRED-REC-RES) /
                           CRED-RECEB-RES) / 30
           MOVE PM-W               TO PM-E
           MOVE PM-E               TO GS-LINDET(61: 8)
           MOVE JUROS-CRED-REC-RES TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(69: 16)
           COMPUTE CUSTO-TOTAL = CRED-RECEB-RES + JUROS-CRED-REC-RES
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(85: 16)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "Creditos futuros" TO GS-LINDET(1: 41)
           MOVE CRED-FUT-RES       TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(42: 16)
           COMPUTE PM-W = ((CRED-FUT-RES * DIAS-CRED-FUT-RES) /
                           CRED-FUT-RES) / 30
           MOVE PM-W               TO PM-E
           MOVE PM-E               TO GS-LINDET(61: 8)
           MOVE JUROS-CRED-FUT-RES TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(69: 16)
           COMPUTE CUSTO-TOTAL = CRED-FUT-RES + JUROS-CRED-FUT-RES
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(85: 16)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "TOTAL-R$....."    TO GS-LINDET(1: 41)
           COMPUTE CUSTO-TOTAL = CRED-RECEB-RES + CRED-FUT-RES
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(42: 16)
           COMPUTE JUROS-W = JUROS-CRED-REC-RES + JUROS-CRED-FUT-RES
           MOVE JUROS-W            TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(69: 16)
           COMPUTE CUSTO-TOTAL = CUSTO-TOTAL + JUROS-W
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(85: 16)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           ADD 1 TO GS-CONT-RESUMO
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "Custo reportagens" TO GS-LINDET(1: 41)
           MOVE VLR-REPORTAGEM-RES  TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET(42: 16)
           COMPUTE PM-W = ((VLR-REPORTAGEM-RES * DIAS-REPORTAGEM-RES) /
                          VLR-REPORTAGEM-RES) / 30
           MOVE PM-W               TO PM-E
           MOVE PM-E               TO GS-LINDET(61: 08)
           MOVE JUROS-REPORTAGEM-RES TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(69: 16)
           COMPUTE CUSTO-TOTAL = VLR-REPORTAGEM-RES +
                                 JUROS-REPORTAGEM-RES
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(85: 16)


           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "Quantia de filmes" TO GS-LINDET(1: 41)
           MOVE TOT-FILME-RES       TO QTDE-E
           MOVE QTDE-E              TO GS-LINDET(48: 7)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "Quantia de fitas" TO GS-LINDET(1: 41)
           MOVE TOT-FITA-RES        TO QTDE-E
           MOVE QTDE-E              TO GS-LINDET(48: 7)
           ADD 1 TO GS-CONT-RESUMO.
           MOVE "INSERE-LIST-RESUMO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGAR-DADOS-RESUMO-OE SECTION.
           OPEN INPUT OED010 CRD020 CGD011.
           IF ST-OED010 <> "00"
              MOVE "ERRO ABERTURA OED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE GS-CONTRATO TO NR-CONTRATO-OE10.
           READ OED010 INVALID KEY INITIALIZE REG-OED010.
           MOVE ASSINATURA-OE10 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV               TO GS-OE-ASSINATURA.
           MOVE DATA-ULT-EVENTO-OE10   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV               TO GS-OE-DATA-ULT-EVENTO.
           MOVE REPRESENTANTE-OE10     TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01              TO GS-OE-REPRESENTANTE
           EVALUATE TIPO-RECTO-OE10
             WHEN 1 MOVE "PARCELAMENTO INDIVIDUAL" TO GS-OE-TIPO-RECTO
             WHEN 2 MOVE "DIRETO DA COMISSAO     " TO GS-OE-TIPO-RECTO
           END-EVALUATE.
           MOVE CONDICAO-RECTO-OE10    TO GS-OE-COND-RECTO
           MOVE CONDICAO-REAJUSTE-OE10 TO GS-OE-COND-REAJUSTE
           MOVE MULTA-RESCISAO-OE10    TO GS-OE-MULTA
           MOVE 0                      TO CLASSIF-CG10
           MOVE GS-CONTRATO            TO CODIGO-CG10(1: 4)
           MOVE ALUNO-PRES-OE10        TO CODIGO-CG10(5: 4)
           READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10         TO GS-OE-PRESIDENTE.
           MOVE COD-COMPL-CG10         TO COD-COMPL-CG11.
           READ CGD011 INVALID KEY INITIALIZE REG-CGD011.
           MOVE FONE1-CG11             TO FONE-E
           MOVE FONE-E                 TO GS-OE-FONE-PRESIDENTE.
           MOVE 0                      TO CLASSIF-CG10
           MOVE GS-CONTRATO            TO CODIGO-CG10(1: 4)
           MOVE ALUNO-TESOU-OE10       TO CODIGO-CG10(5: 4)
           READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10         TO GS-OE-TESOUREIRO.
           MOVE COD-COMPL-CG10         TO COD-COMPL-CG11.
           READ CGD011 INVALID KEY INITIALIZE REG-CGD011.
           MOVE FONE1-CG11             TO FONE-E
           MOVE FONE-E                 TO GS-OE-FONE-TESOUREIRO.

           MOVE ZEROS TO DIAS-PAGOS DIAS-ATRASADO DIAS-RESTANTE.
           MOVE ZEROS TO GS-OE-PAGOS GS-OE-ATRASADO GS-OE-RESTANTE
                         GS-OE-TOTAL TOTAL-PAGOS-OE TOTAL-ATRASADO-OE
                         TOTAL-RESTANTE-OE.
           MOVE 0                      TO COD-COMPL-CR20(1: 1)
           MOVE GS-CONTRATO            TO COD-COMPL-CR20(2: 4)
           MOVE ZEROS                  TO COD-COMPL-CR20(6: 4) SEQ-CR20
           START CRD020 KEY IS NOT < CHAVE-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
             READ CRD020 NEXT RECORD AT END
                  MOVE "10" TO ST-CRD020
             NOT AT END
                  MOVE COD-COMPL-CR20(2: 4) TO CONTRATO-W
                  IF CONTRATO-W <> GS-CONTRATO
                     MOVE "10" TO ST-CRD020
                  ELSE
                     IF TIPO-DOCTO-CR20 <> 2
                        CONTINUE
                     ELSE
                        PERFORM CALCULA-DIAS-OE
                        IF DATA-RCTO-CR20 <> ZEROS
                           COMPUTE GS-OE-PAGOS = GS-OE-PAGOS +
                                   (VALOR-TOT-CR20 - VALOR-SALDO-CR20)
                           MOVE GS-OE-PAGOS      TO GS-OE-TOTAL

      *                    ADD VALOR-LIQ-CR20    TO GS-OE-PAGOS
      *                                             GS-OE-TOTAL

                           ADD GRTIME-DAYS-FINAL TO DIAS-PAGOS
                           COMPUTE TOTAL-PAGOS-OE = (GRTIME-DAYS-FINAL *
                                   VALOR-LIQ-CR20) + TOTAL-PAGOS-OE
                        ELSE
                           IF DATA-VENCTO-CR20 < DATA-DIA-I
                              ADD VALOR-TOT-CR20    TO GS-OE-ATRASADO
                                                       GS-OE-TOTAL
                              ADD GRTIME-DAYS-FINAL TO DIAS-ATRASADO
                              COMPUTE TOTAL-ATRASADO-OE =
                                        (GRTIME-DAYS-FINAL
                                   * VALOR-TOT-CR20) + TOTAL-ATRASADO-OE
                           ELSE
                              ADD VALOR-TOT-CR20    TO GS-OE-RESTANTE
                                                       GS-OE-TOTAL
                              ADD GRTIME-DAYS-FINAL TO DIAS-RESTANTE
                              COMPUTE TOTAL-RESTANTE-OE =
                                     (GRTIME-DAYS-FINAL*
                                     VALOR-TOT-CR20) + TOTAL-RESTANTE-OE
                           END-IF
                        END-IF

                     END-IF
                  END-IF
             END-READ
           END-PERFORM.
           COMPUTE GS-OE-MES-PAGOS = (TOTAL-PAGOS-OE / GS-OE-PAGOS) / 30
           MOVE GS-OE-MES-PAGOS TO PRAZO-MEDIO
           MOVE GS-OE-PAGOS     TO CUSTO-PREVISTO
           PERFORM CALCULO-JUROS-OE
           MOVE JUROS-W       TO GS-OE-JR-PAGOS
           COMPUTE GS-OE-MES-ATRASADO =
              (TOTAL-ATRASADO-OE / GS-OE-ATRASADO) / 30
           MOVE GS-OE-MES-ATRASADO TO PRAZO-MEDIO
           MOVE GS-OE-ATRASADO     TO CUSTO-PREVISTO
           PERFORM CALCULO-JUROS-OE
           MOVE JUROS-W            TO GS-OE-JR-ATRASADO
           COMPUTE GS-OE-MES-RESTANTE =
              (TOTAL-RESTANTE-OE / GS-OE-RESTANTE) / 30.
           MOVE GS-OE-MES-RESTANTE   TO PRAZO-MEDIO
           MOVE GS-OE-RESTANTE       TO CUSTO-PREVISTO
           PERFORM CALCULO-JUROS-OE.
           MOVE JUROS-W       TO GS-OE-JR-RESTANTE
           COMPUTE GS-OE-TOT-PAGOS = GS-OE-PAGOS + GS-OE-JR-PAGOS
           COMPUTE GS-OE-TOT-RESTANTE = GS-OE-RESTANTE +
                                        GS-OE-JR-RESTANTE
           COMPUTE GS-OE-TOT-ATRASADO = GS-OE-ATRASADO +
                                        GS-OE-JR-ATRASADO
           COMPUTE GS-OE-TOT-TOTAL = GS-OE-TOT-PAGOS +
                               GS-OE-TOT-RESTANTE + GS-OE-TOT-ATRASADO
           COMPUTE GS-OE-JR-TOTAL = GS-OE-JR-PAGOS + GS-OE-JR-ATRASADO
                                    + GS-OE-JR-RESTANTE
           CLOSE OED010 CRD020 CGD011.

      * resumo de compromisso de org.de eventos
           MOVE VLR-PAGO-RES-OE    TO GS-OE-PAGOS-COMP
           COMPUTE PM-W = ((VLR-PAGO-RES-OE * DIAS-PAGO-RES-OE) /
                          VLR-PAGO-RES-OE) / 30
           MOVE PM-W               TO GS-OE-MES-PAGOS-COMP.
           MOVE JUROS-PAGO-RES-OE  TO GS-OE-JR-PAGOS-COMP.
           COMPUTE CUSTO-TOTAL = VLR-PAGO-RES-OE + JUROS-PAGO-RES-OE
           MOVE CUSTO-TOTAL        TO GS-OE-TOT-PAGOS-COMP

           MOVE VLR-PAGAR-RES-OE   TO GS-OE-ATRASADO-COMP
           COMPUTE PM-W = ((VLR-PAGAR-RES-OE * DIAS-PAGAR-RES-OE) /
                          VLR-PAGAR-RES-OE) / 30
           MOVE PM-W               TO GS-OE-MES-ATRASADO-COMP
           MOVE JUROS-PAGAR-RES-OE TO GS-OE-JR-ATRASADO-COMP
           COMPUTE CUSTO-TOTAL = VLR-PAGAR-RES-OE + JUROS-PAGAR-RES-OE
           MOVE CUSTO-TOTAL        TO GS-OE-TOT-ATRASADO-COMP.

           COMPUTE CUSTO-TOTAL = VLR-PAGO-RES-OE + VLR-PAGAR-RES-OE
           MOVE CUSTO-TOTAL        TO GS-OE-TOTAL-COMP
           COMPUTE JUROS-W = JUROS-PAGO-RES-OE + JUROS-PAGAR-RES-OE
           MOVE JUROS-W            TO GS-OE-JR-TOTAL-COMP
           COMPUTE CUSTO-TOTAL = JUROS-W + CUSTO-TOTAL
           MOVE CUSTO-TOTAL        TO GS-OE-TOT-TOTAL-COMP.

       CALCULA-DIAS-OE SECTION.
           IF DATA-RCTO-CR20 <> ZEROS
              MOVE DATA-RCTO-CR20      TO GRTIME-DATE
           ELSE MOVE DATA-VENCTO-CR20 TO GRTIME-DATE.
           MOVE 2                     TO GRTIME-TYPE
           MOVE 7                     TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           IF GRTIME-DATE-FINAL = 0
              MOVE "DATA-INVALIDA"    TO GS-MENSAGEM-ERRO(1: 14)
              MOVE CLIENTE-CR20       TO GS-MENSAGEM-ERRO(15: 10)
              MOVE GRTIME-DATE        TO GS-MENSAGEM-ERRO(25: 10)
              MOVE "ERRO-DATA-RECEBER" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              MOVE ZEROS              TO GRTIME-DAYS-FINAL
           ELSE
             MOVE DATA-ULT-EVENTO-OE10   TO GRTIME-DATE-FINAL
             MOVE 2                     TO GRTIME-TYPE
             MOVE 3                     TO GRTIME-FUNCTION
             CALL "GRTIME" USING PARAMETROS-GRTIME.

       CALCULO-JUROS-OE SECTION.
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-PREVISTO * (TAXA-ACUMULADA - 1).

       CARREGAR-DADOS-CONTATOS SECTION.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO500.
           MOVE ZEROS       TO ITEM-CO500 GS-CONT-CONTATO.
           START COD500 KEY IS NOT < CHAVE-CO500 INVALID KEY
                 MOVE "10" TO ST-COD500.
           PERFORM UNTIL ST-COD500 = "10"
              READ COD500 NEXT RECORD AT END MOVE "10" TO ST-COD500
                NOT AT END
                  IF NR-CONTRATO-CO500 <> GS-CONTRATO
                     MOVE "10" TO ST-COD500
                  ELSE
                    MOVE SPACES        TO GS-LINDET
                    MOVE DATA-CO500    TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV      TO DATA-E
                    MOVE DATA-E        TO GS-LINDET(1: 12)
                    MOVE HORA-CO500(1: 2) TO GS-LINDET(13: 2)
                    MOVE ":"              TO GS-LINDET(15: 1)
                    MOVE HORA-CO500(3: 2) TO GS-LINDET(16: 4)
                    MOVE USUARIO-CO500    TO GS-LINDET(20: 5)
                    MOVE DATA-AGENDA-CO500 TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV          TO DATA-E
                    MOVE DATA-E            TO GS-LINDET(30: 10)
                    MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                    PERFORM DADOS-CONTATOS-501
                  END-IF
              END-READ.
       DADOS-CONTATOS-501 SECTION.
           MOVE NR-CONTRATO-CO500 TO NR-CONTRATO-CO501.
           MOVE ITEM-CO500        TO ITEM-CO501.
           MOVE ZEROS             TO SUBITEM-CO501.
           START COD501 KEY IS NOT < CHAVE-CO501 INVALID KEY
                 MOVE "10" TO ST-COD501.
           PERFORM UNTIL ST-COD501 = "10"
              READ COD501 NEXT RECORD AT END MOVE "10" TO ST-COD501
                NOT AT END
                  IF NR-CONTRATO-CO501 <> NR-CONTRATO-CO500 OR
                     ITEM-CO501 <> ITEM-CO500
                     MOVE "10" TO ST-COD501
                  ELSE
                    MOVE SPACES         TO GS-LINDET
                    MOVE ANOTACAO-CO501 TO GS-LINDET(1: 80)
                    MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-READ.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "COP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------------
      *imprime somente dados do contrato e brinde
       IMPRIME-BRINDE SECTION.
      *    COPY "COND-IMP".

           OPEN OUTPUT RELAT.

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP
           ELSE
              WRITE REG-RELAT FROM COND-EP.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE GS-CONTRATO       TO LINDET-REL(1: 5)
           MOVE GS-TAXA           TO TAXA-E
           MOVE TAXA-E            TO LINDET-REL(6: 6)
           MOVE GS-ORIGEM         TO LINDET-REL(12: 4)
           MOVE GS-IDENTIFICACAO  TO LINDET-REL(16: 16)
           MOVE GS-TURMAS         TO LINDET-REL(32: 3)
           MOVE GS-INSTITUICAO    TO LINDET-REL(35: 11)
           MOVE GS-CIDADE         TO LINDET-REL(46: 13)
           MOVE GS-MESANO         TO MESANO-E
           MOVE MESANO-E          TO LINDET-REL(59: 7)
           MOVE GS-NR-FORM        TO LINDET-REL(66: 5)
           MOVE GS-PADRAO         TO LINDET-REL(71: 1)
           WRITE REG-RELAT FROM CAB04 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM LINDET.
           ADD 4 TO LIN.

           WRITE REG-RELAT FROM CAB05 AFTER 2
           WRITE REG-RELAT FROM CAB03
           MOVE SPACES            TO LINDET-REL
           MOVE GS-STATUS         TO LINDET-REL(1: 31)
           MOVE GS-MENSAGEM       TO LINDET-REL(32: 41)
           MOVE GS-SERVICOS       TO LINDET-REL(73: 5)
           WRITE REG-RELAT FROM LINDET
           ADD 4 TO LIN.

           PERFORM CARREGAR-COMISSAO-REL.
           PERFORM CARREGAR-DADOS-BRINDE-REL.

           MOVE SPACES TO REG-RELAT.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL AFTER PAGE.

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP.

           CLOSE RELAT.
      *    COPY "DESC-IMP".
      *-------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
      *    COPY "COND-IMP".
           OPEN OUTPUT RELAT.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP
           ELSE
              WRITE REG-RELAT FROM COND-EP.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE GS-CONTRATO       TO LINDET-REL(1: 5)
           MOVE GS-TAXA           TO TAXA-E
           MOVE TAXA-E            TO LINDET-REL(6: 6)
           MOVE GS-ORIGEM         TO LINDET-REL(12: 4)
           MOVE GS-IDENTIFICACAO  TO LINDET-REL(16: 16)
           MOVE GS-TURMAS         TO LINDET-REL(32: 3)
           MOVE GS-INSTITUICAO    TO LINDET-REL(35: 11)
           MOVE GS-CIDADE         TO LINDET-REL(46: 13)
           MOVE GS-MESANO         TO MESANO-E
           MOVE MESANO-E          TO LINDET-REL(59: 8)
           MOVE GS-NR-FORM        TO LINDET-REL(67: 5)
           MOVE GS-PADRAO         TO LINDET-REL(72: 1)
           WRITE REG-RELAT FROM CAB04 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM LINDET.
           ADD 4 TO LIN.

           WRITE REG-RELAT FROM CAB05 AFTER 2
           WRITE REG-RELAT FROM CAB03
           MOVE SPACES            TO LINDET-REL
           MOVE GS-STATUS         TO LINDET-REL(1: 31)
           MOVE GS-MENSAGEM       TO LINDET-REL(32: 41)
           MOVE GS-SERVICOS       TO LINDET-REL(73: 5)
           WRITE REG-RELAT FROM LINDET
           ADD 4 TO LIN.
           PERFORM CARREGAR-COMISSAO-REL
           PERFORM CARREGAR-DADOS-BRINDE-REL
           PERFORM CARREGAR-DADOS-EVENTO-REL
           PERFORM CARREGAR-DADOS-CONTATOS-REL
           PERFORM CARREGAR-DADOS-CURSOS-REL
           PERFORM CARREGAR-DADOS-CREDITO-REL.

           MOVE SPACES TO REG-RELAT.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL AFTER PAGE.

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP.

           CLOSE RELAT.
      *    COPY "DESC-IMP".
       CARREGAR-COMISSAO-REL SECTION.
           ADD 9 TO LIN.
           IF LIN > 56 PERFORM CABECALHO
                       ADD 9 TO LIN.
           WRITE REG-RELAT FROM CAB06 AFTER 2
           MOVE "REGIAO:"          TO LINDET-REL(01: 17)
           MOVE GS-REGIAO          TO LINDET-REL(18: 40)
           MOVE "ORGANIZER:"       TO LINDET-REL(50: 17)
           MOVE GS-ORGANIZER       TO LINDET-REL(67: 40)
           WRITE REG-RELAT FROM LINDET
           MOVE "REPRESENTANTE:"   TO LINDET-REL(01: 17)
           MOVE GS-REPRESENTANTE   TO LINDET-REL(18: 40)
           MOVE "MEMBRO COMISSAO:" TO LINDET-REL(50: 17)
           MOVE GS-COMISSAO1       TO LINDET-REL(67: 40)
           WRITE REG-RELAT FROM LINDET
           MOVE "DATA ASSINATURA:" TO LINDET-REL(01: 17)
           MOVE GS-DATA-ASSINATURA TO LINDET-REL(18: 40)
           MOVE "MEMBRO COMISSAO:" TO LINDET-REL(50: 17)
           MOVE GS-COMISSAO2       TO LINDET-REL(67: 40)
           WRITE REG-RELAT FROM LINDET
           MOVE "MULTA CONTRATUAL:" TO LINDET-REL(01: 17)
           MOVE GS-MULTA-CONT      TO LINDET-REL(18: 40)
           MOVE "MEMBRO COMISSAO:" TO LINDET-REL(50: 17)
           MOVE GS-COMISSAO3       TO LINDET-REL(67: 40)
           WRITE REG-RELAT FROM LINDET
           MOVE "FORUM:"           TO LINDET-REL(01: 17)
           MOVE GS-FORUM           TO LINDET-REL(18: 40)
           MOVE "       CONVITES:" TO LINDET-REL(50: 17)
           MOVE GS-CONVITE         TO LINDET-REL(67: 40)
           WRITE REG-RELAT FROM LINDET
           MOVE "COMERCIALIZACAO:" TO LINDET-REL(01: 17)
           MOVE GS-COMERCIALIZACAO TO LINDET-REL(18: 40)
           MOVE "          BECAS:" TO LINDET-REL(50: 17)
           MOVE GS-BECA            TO LINDET-REL(67: 40)
           WRITE REG-RELAT FROM LINDET
           MOVE "NR.INIC.FORMANDO:" TO LINDET-REL(01: 17)
           MOVE GS-NR-INIC-PAD     TO LINDET-REL(18: 40)
           MOVE "         OUTROS:" TO LINDET-REL(50: 17)
           MOVE GS-OUTRO           TO LINDET-REL(67: 40)
           WRITE REG-RELAT FROM LINDET.

       CARREGAR-DADOS-BRINDE-REL SECTION.
           MOVE ZEROS TO SOMACUSTO-W.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO50.
           MOVE ZEROS              TO ITEM-CO50 GS-CONT-BRINDE
                                      DATA-VENCTO-CO50 REALIZADO-CO50.
           WRITE REG-RELAT FROM CAB07 AFTER 2
           WRITE REG-RELAT FROM CAB08
           WRITE REG-RELAT FROM CAB03.
           ADD 4 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
           START COD050 KEY IS NOT < ALT1-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.
           PERFORM UNTIL ST-COD050 = "10"
              READ COD050 NEXT RECORD AT END MOVE "10" TO ST-COD050
                    MOVE SOMACUSTO-W TO SOMACUSTO-REL
                    WRITE REG-RELAT FROM CAB08A

                NOT AT END
                  IF NR-CONTRATO-CO50 <> GS-CONTRATO
                     MOVE "10" TO ST-COD050
                     MOVE SOMACUSTO-W TO SOMACUSTO-REL
                     WRITE REG-RELAT FROM CAB08A
                  ELSE
                     IF SUSP-PREV-DEF-CO50 <> 2
                        MOVE SPACES       TO LINDET-REL
                        MOVE ITEM-CO50    TO LINDET-REL(1: 3)
                        MOVE CODBRINDE-CO50 TO CODIGO-CO02
                        READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02
                        END-READ
                        MOVE NOME-CO02    TO LINDET-REL(4: 14)
                        MOVE QTDE-POR-FORM-CO50 TO QTDE5-E
                        MOVE QTDE5-E            TO LINDET-REL(19: 6)
                        MOVE QTDE-FORM-CO50     TO QTDE4-E
                        MOVE QTDE4-E            TO LINDET-REL(30: 5)
                        PERFORM CALCULA-CUSTO-BRINDE
                        MOVE CUSTO-PREVISTO     TO VALOR-E1
                        MOVE VALOR-E1           TO LINDET-REL(35: 11)
                        MOVE CUSTO-W            TO VALOR-E1
                        MOVE VALOR-E1           TO LINDET-REL(46: 11)
                        MOVE DATA-VENCTO-CO50   TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV           TO DATA-E
                        MOVE DATA-E             TO LINDET-REL(57: 11)
                        MOVE DATA-PAGTO-CO50    TO DATA-E
                        MOVE DATA-E             TO LINDET-REL(68: 11)
                        MOVE DIAS-PRAZO-CO50    TO LINDET-REL(79: 5)
                        PERFORM CALCULO-JUROS-BRINDE
                        MOVE JUROS-W            TO VALOR-E1
                        MOVE VALOR-E1           TO LINDET-REL(84: 11)
                        COMPUTE CUSTO-TOTAL = CUSTO-W + JUROS-W
                        MOVE CUSTO-TOTAL        TO VALOR-E1
                        ADD  CUSTO-TOTAL        TO SOMACUSTO-W
                        MOVE VALOR-E1           TO LINDET-REL(95: 11)
                        MOVE COD-FORNEC-CO50    TO LINDET-REL(107: 6)
                        WRITE REG-RELAT FROM LINDET
                        ADD 1 TO LIN
                        IF LIN > 58 PERFORM CABECALHO
                        END-IF
                     END-IF
                  END-IF
              END-READ
           END-PERFORM.
       CARREGAR-DADOS-EVENTO-REL SECTION.
           ADD 4 TO LIN.
           IF LIN > 56 PERFORM CABECALHO
                       ADD 4 TO LIN.
           WRITE REG-RELAT FROM CAB09 AFTER 2.
           WRITE REG-RELAT FROM CAB10
           WRITE REG-RELAT FROM CAB03.


           MOVE GS-CONTRATO   TO NR-CONTRATO-CO60.
           MOVE ZEROS      TO ITEM-CO60 GS-CONT-EVENTO DATAREALIZA-CO60.
           START COD060 KEY IS NOT < ALT1-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.
           PERFORM UNTIL ST-COD060 = "10"
              READ COD060 NEXT RECORD AT END MOVE "10" TO ST-COD060
                NOT AT END
                  IF NR-CONTRATO-CO60 <> GS-CONTRATO
                     MOVE "10" TO ST-COD060
                  ELSE
                    MOVE SPACES           TO LINDET-REL
                    MOVE ITEM-CO60        TO LINDET-REL(1: 4)
                    MOVE CODEVENTO-CO60   TO CODIGO-CO03
                    READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03
                    END-READ
                    MOVE NOME-CO03            TO LINDET-REL(5: 20)
                    MOVE QT-PARTICIPANTE-CO60 TO QTDE4-E
                    MOVE QTDE4-E              TO LINDET-REL(27: 7)
                    MOVE DATAREALIZA-CO60     TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV             TO DATA-E
                    MOVE DATA-E               TO LINDET-REL(34: 11)
                    MOVE HORARIO-CO60         TO LINDET-REL(45: 11)
                    EVALUATE FOTO-CO60
                      WHEN 0 MOVE "N"         TO LINDET-REL(54: 3)
                      WHEN 1 MOVE "S"         TO LINDET-REL(54: 3)
                    END-EVALUATE
                    EVALUATE VIDEO-CO60
                      WHEN 0 MOVE "N"         TO LINDET-REL(57: 3)
                      WHEN 1 MOVE "S"         TO LINDET-REL(57: 3)
                    END-EVALUATE
                    MOVE QT-TELAO-CO60        TO LINDET-REL(60: 4)
                    EVALUATE BECA-CO60
                      WHEN 0 MOVE "N"         TO LINDET-REL(64: 3)
                      WHEN 1 MOVE "S"         TO LINDET-REL(64: 3)
                    END-EVALUATE
                    EVALUATE CLIP-CO60
                      WHEN 0 MOVE "N"         TO LINDET-REL(67: 3)
                      WHEN 1 MOVE "S"         TO LINDET-REL(67: 3)
                    END-EVALUATE
                    EVALUATE FAX-CO60
                      WHEN 0 MOVE " N"        TO LINDET-REL(70: 3)
                      WHEN 1 MOVE " S"        TO LINDET-REL(70: 3)
                    END-EVALUATE
                    EVALUATE APROVACAO-CO60
                      WHEN 0 MOVE "N"         TO LINDET-REL(73: 3)
                      WHEN 1 MOVE "S"         TO LINDET-REL(73: 3)
                    END-EVALUATE

                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 58 PERFORM CABECALHO
                    END-IF
                  END-IF
              END-READ
           END-PERFORM.

       CARREGAR-DADOS-CREDITO-REL SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO70.
           MOVE ZEROS              TO ITEM-CO70
                                      DATA-VENCTO-CO70 REALIZADO-CO70.
           WRITE REG-RELAT FROM CAB10A AFTER 2
           WRITE REG-RELAT FROM CAB10B
           WRITE REG-RELAT FROM CAB03.
           ADD 4 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
           START COD070 KEY IS NOT < ALT1-CO70 INVALID KEY
                 MOVE "10" TO ST-COD070.
           PERFORM UNTIL ST-COD070 = "10"
              READ COD070 NEXT RECORD AT END MOVE "10" TO ST-COD070
                NOT AT END
                  IF NR-CONTRATO-CO70 <> GS-CONTRATO
                     MOVE "10" TO ST-COD070
                  ELSE
                     MOVE SPACES       TO LINDET-REL
                     MOVE ITEM-CO70    TO LINDET-REL(1: 3)
                     MOVE DESCRICAO-CO70     TO LINDET-REL(4: 35)
                     MOVE VALOR-PREVISTO-CO70 TO VALOR-E1
                     MOVE VALOR-E1           TO LINDET-REL(41: 11)
                     MOVE DATA-VENCTO-CO70   TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV           TO DATA-E
                     MOVE DATA-E             TO LINDET-REL(52: 11)
                     MOVE DATA-RECTO-CO70    TO DATA-E
                     MOVE DATA-E             TO LINDET-REL(63: 11)
                     MOVE DIAS-PRAZO-CO70    TO LINDET-REL(74: 5)
                     PERFORM CALCULO-JUROS-CREDITO
                     MOVE JUROS-W            TO VALOR-E1
                     MOVE VALOR-E1           TO LINDET-REL(79: 11)
                     COMPUTE CUSTO-TOTAL = VALOR-PREVISTO-CO70 + JUROS-W
                     MOVE CUSTO-TOTAL        TO VALOR-E1
                     MOVE VALOR-E1           TO LINDET-REL(90: 11)
                     WRITE REG-RELAT FROM LINDET
                     ADD 1 TO LIN
                     IF LIN > 56 PERFORM CABECALHO
                     END-IF
                  END-IF
              END-READ
           END-PERFORM.
       CARREGAR-DADOS-CONTATOS-REL SECTION.
           WRITE REG-RELAT FROM CAB11 AFTER 2
           WRITE REG-RELAT FROM CAB03
           ADD 3 TO LIN
           IF LIN > 58 PERFORM CABECALHO.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO500.
           MOVE ZEROS       TO ITEM-CO500 GS-CONT-CONTATO.
           START COD500 KEY IS NOT < CHAVE-CO500 INVALID KEY
                 MOVE "10" TO ST-COD500.
           PERFORM UNTIL ST-COD500 = "10"
              READ COD500 NEXT RECORD AT END MOVE "10" TO ST-COD500
                NOT AT END
                  IF NR-CONTRATO-CO500 <> GS-CONTRATO
                     MOVE "10" TO ST-COD500
                  ELSE
                    MOVE SPACES        TO LINDET-REL
                    MOVE DATA-CO500    TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV      TO DATA-E
                    MOVE DATA-E           TO LINDET-REL(1: 12)
                    MOVE HORA-CO500(1: 2) TO LINDET-REL(13: 2)
                    MOVE ":"              TO LINDET-REL(15: 1)
                    MOVE HORA-CO500(3: 2) TO LINDET-REL(16: 4)
                    MOVE USUARIO-CO500    TO LINDET-REL(20: 5)
                    MOVE DATA-AGENDA-CO500 TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV          TO DATA-E
                    MOVE DATA-E            TO LINDET-REL(30: 10)
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 56 PERFORM CABECALHO
                    END-IF
                    PERFORM DADOS-CONTATOS-REL1
                  END-IF
              END-READ.
       DADOS-CONTATOS-REL1 SECTION.
           MOVE NR-CONTRATO-CO500 TO NR-CONTRATO-CO501.
           MOVE ITEM-CO500        TO ITEM-CO501.
           MOVE ZEROS             TO SUBITEM-CO501.
           START COD501 KEY IS NOT < CHAVE-CO501 INVALID KEY
                 MOVE "10" TO ST-COD501.
           PERFORM UNTIL ST-COD501 = "10"
              READ COD501 NEXT RECORD AT END MOVE "10" TO ST-COD501
                NOT AT END
                  IF NR-CONTRATO-CO501 <> NR-CONTRATO-CO500 OR
                     ITEM-CO501 <> ITEM-CO500
                     MOVE "10" TO ST-COD501
                  ELSE
                    MOVE SPACES         TO LINDET-REL
                    MOVE ANOTACAO-CO501 TO LINDET-REL(1: 80)
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 58 PERFORM CABECALHO
                    END-IF
                  END-IF
              END-READ.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CARREGAR-DADOS-CURSOS-REL SECTION.
           WRITE REG-RELAT FROM CAB12 AFTER 2.
           WRITE REG-RELAT FROM CAB13.
           WRITE REG-RELAT FROM CAB03.
           ADD 4 TO LIN
           IF LIN > 58 PERFORM CABECALHO.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO41.
           MOVE ZEROS       TO CURSO-CO41 GS-CONT-CURSO.
           MOVE SPACES      TO TURMA-CO41.
           START COD041 KEY IS NOT < CHAVE-CO41 INVALID KEY
                 MOVE "10" TO ST-COD041.
           PERFORM UNTIL ST-COD041 = "10"
              READ COD041 NEXT RECORD AT END MOVE "10" TO ST-COD041
                NOT AT END
                  IF NR-CONTRATO-CO41 <> GS-CONTRATO
                     MOVE "10" TO ST-COD041
                  ELSE
                    MOVE SPACES        TO LINDET-REL
                    MOVE CURSO-CO41    TO CODIGO-IE11
                    READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11
                    END-READ
                    MOVE NOME-IE11         TO LINDET-REL(1: 25)
                    MOVE TURMA-CO41        TO LINDET-REL(27: 6)
                    MOVE NR-PREV-FORM-CO41 TO LINDET-REL(33: 5)
                    MOVE LISTA-ALUNOS-CO41 TO LINDET-REL(38: 2)
                    MOVE GS-CONTRATO       TO CODIGO-CG10(1: 4)
                    MOVE RESPONSAVEL-CO41  TO CODIGO-CG10(5: 4)
                    MOVE 0                 TO CLASSIF-CG10
                    READ CGD010 INVALID KEY MOVE SPACES TO
                         COMPRADOR-CG10
                    END-READ
                    MOVE COMPRADOR-CG10    TO LINDET-REL(40: 10)
                    MOVE FOTO-TURMA-CO41   TO LINDET-REL(51: 3)
                    MOVE MEDIDA-BECA-CO41  TO LINDET-REL(54: 3)
                    MOVE COR-FAIXA-CO41    TO LINDET-REL(57: 11)
                    MOVE COR-CANUDO-CO41   TO LINDET-REL(68: 11)
                    MOVE TIPO-ALBUM-CO41   TO LINDET-REL(79: 6)
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    if lin > 58 PERFORM CABECALHO
                    END-IF
                  END-IF
              END-READ.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           MOVE 3 TO LIN.
      *------------------------------------------------------
       IMPRIMIR-PREVISAO-FINANC SECTION.
      *    COPY "COND-IMP".
           OPEN OUTPUT RELAT.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP
           ELSE
              WRITE REG-RELAT FROM COND-EP.
           OPEN INPUT OED020.
           IF GS-TIPO-BRINDE = 1
              PERFORM MOVER-PREV-FINANC-CONTRATO
           ELSE
              PERFORM MOVER-PREV-FINANC-EVENTO.

           PERFORM CONT-IMPRIMIR-PREV-FINANC.
           PERFORM GRAVA-CONTATO-PREV-FINANC.
           PERFORM GRAVA-PAGAR-PREV-FINANC.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP.

           CLOSE RELAT OED020.
      *    COPY "DESC-IMP".

       MOVER-PREV-FINANC-CONTRATO SECTION.
           MOVE GS-CONTRATO     TO NR-CONTRATO-CO50.
           MOVE GS-ITEM-PF      TO ITEM-CO50.
           START COD050 KEY IS = CHAVE-CO50 INVALID KEY
                 CONTINUE
           NOT INVALID KEY
               READ COD050
               END-READ
               MOVE GS-VALOR-DOLAR-PF    TO DOLAR-REL
               MOVE DATA-VENCTO-CO50     TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO DATA-E    DATA-VENC
               MOVE DATA-E               TO VENC-REL
               MOVE GS-FORMA-PAGTO1-PF   TO PGTO-REL
               MOVE GS-FORMA-PAGTO2-PF   TO PGTO1-REL
               MOVE GS-CONTRATO          TO CONT-REL
               MOVE CODBRINDE-CO50       TO BRINDE1-REL CODIGO-CO02
               READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02
               END-READ
               PERFORM CALCULA-CUSTO-BRINDE
               MOVE CUSTO-W              TO VALOR-REL
               MOVE MESANO-PREV-CO40(1: 4)  TO MESANO-W(3: 4)
               MOVE MESANO-PREV-CO40(5: 2)  TO MESANO-W(1: 2)
               MOVE MESANO-W             TO MESANO-REL1
               MOVE NOME-CO02            TO DESC-BRINDE-REL
               MOVE ITEM-CO50            TO ITEM-REL
               MOVE INSTITUICAO-CO40   TO CODIGO-IE10
               READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10
               END-READ
               MOVE NOME-IE10            TO IDENT-REL
               MOVE IDENTIFICACAO-CO40   TO CURSO1-REL
               MOVE CIDADE-CO40          TO CIDADE
               READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
               END-READ
               MOVE NOME-CID             TO CIDADE1-REL
           END-START.

       MOVER-PREV-FINANC-EVENTO SECTION.
           MOVE GS-CONTRATO     TO NR-CONTRATO-OE20.
           MOVE GS-ITEM-PF      TO ITEM-OE20.
           START OED020 KEY IS = CHAVE-OE20 INVALID KEY
             CONTINUE
              NOT INVALID KEY
               READ OED020
               END-READ
               MOVE GS-VALOR-DOLAR-PF    TO DOLAR-REL
               MOVE DATA-VENCTO-OE20     TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO DATA-VENC DATA-E
               MOVE DATA-E               TO VENC-REL

               MOVE GS-FORMA-PAGTO1-PF   TO PGTO-REL
               MOVE GS-FORMA-PAGTO2-PF   TO PGTO1-REL
               MOVE GS-CONTRATO          TO CONT-REL
               MOVE CODBRINDE-OE20       TO BRINDE1-REL CODIGO-CO02
               READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02
               END-READ
               PERFORM CALCULA-CUSTO-BRINDE-OE
               MOVE CUSTO-W              TO VALOR-REL
               MOVE MESANO-PREV-CO40(1: 4)  TO MESANO-W(3: 4)
               MOVE MESANO-PREV-CO40(5: 2)  TO MESANO-W(1: 2)
               MOVE MESANO-W             TO MESANO-REL1
               MOVE NOME-CO02            TO DESC-BRINDE-REL
               MOVE ITEM-OE20            TO ITEM-REL
               MOVE INSTITUICAO-CO40     TO CODIGO-IE10
               READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10
               END-READ
               MOVE NOME-IE10            TO IDENT-REL
               MOVE IDENTIFICACAO-CO40   TO CURSO1-REL
               MOVE CIDADE-CO40          TO CIDADE
               READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
               END-READ
               MOVE NOME-CID             TO CIDADE1-REL
           END-START.

       CONT-IMPRIMIR-PREV-FINANC SECTION.
           WRITE REG-RELAT FROM TRACO9 AFTER 0
           WRITE REG-RELAT FROM LINDET00
           WRITE REG-RELAT FROM LINDET01
           WRITE REG-RELAT FROM TRACO9
           WRITE REG-RELAT FROM LINDET02
           WRITE REG-RELAT FROM LINDET03
           WRITE REG-RELAT FROM TRACO10
           EVALUATE GS-TIPO-BRINDE
             WHEN 1 WRITE REG-RELAT FROM LINDET03A AFTER 2
             WHEN 2 WRITE REG-RELAT FROM LINDET03AA AFTER 2
           END-EVALUATE
           WRITE REG-RELAT FROM LINDET07 AFTER 2
           WRITE REG-RELAT FROM LINDET03C AFTER 2
           WRITE REG-RELAT FROM LINDET03B
           WRITE REG-RELAT FROM LINDET04 AFTER 2
           WRITE REG-RELAT FROM LINDET05 AFTER 2
           WRITE REG-RELAT FROM LINDET06 AFTER 2
           WRITE REG-RELAT FROM LINDET06A
           WRITE REG-RELAT FROM LINDET09 AFTER 3
           WRITE REG-RELAT FROM LINDET09D
           WRITE REG-RELAT FROM LINDET09B
           WRITE REG-RELAT FROM LINDET09C
           WRITE REG-RELAT FROM TRACO-SEP AFTER 2
           WRITE REG-RELAT FROM TRACO9 AFTER 2

           WRITE REG-RELAT FROM LINDET00
           WRITE REG-RELAT FROM LINDET01
           WRITE REG-RELAT FROM TRACO9
           WRITE REG-RELAT FROM LINDET02
           WRITE REG-RELAT FROM LINDET03
           WRITE REG-RELAT FROM TRACO10
           EVALUATE GS-TIPO-BRINDE
             WHEN 1 WRITE REG-RELAT FROM LINDET03A AFTER 2
             WHEN 2 WRITE REG-RELAT FROM LINDET03AA AFTER 2
           END-EVALUATE
           WRITE REG-RELAT FROM LINDET07 AFTER 2
           WRITE REG-RELAT FROM LINDET03C AFTER 2
           WRITE REG-RELAT FROM LINDET03B
           WRITE REG-RELAT FROM LINDET04 AFTER 2
           WRITE REG-RELAT FROM LINDET05 AFTER 2
           WRITE REG-RELAT FROM LINDET06 AFTER 2
           WRITE REG-RELAT FROM LINDET06A

           WRITE REG-RELAT FROM LINDET09 AFTER 3
           WRITE REG-RELAT FROM LINDET09D
           WRITE REG-RELAT FROM LINDET09B
           WRITE REG-RELAT FROM LINDET09C
           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT AFTER PAGE.

       GRAVA-CONTATO-PREV-FINANC SECTION.
      *    grava contato da previsão financeira.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO500.
           MOVE ZEROS       TO ITEM-CO500 ULT-ITEM.
           START COD500 KEY IS NOT < CHAVE-CO500 INVALID KEY
                 MOVE "10" TO ST-COD500.
           PERFORM UNTIL ST-COD500 = "10"
             READ COD500 NEXT RECORD AT END
                  MOVE "10" TO ST-COD500
             NOT AT END
                 IF NR-CONTRATO-CO500 <> GS-CONTRATO
                              MOVE "10" TO ST-COD500
                 ELSE
                    MOVE ITEM-CO500 TO ULT-ITEM
                 CONTINUE
             END-READ
           END-PERFORM.
           ADD 1 TO ULT-ITEM.
           MOVE ULT-ITEM     TO ITEM-CO500
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO500

           MOVE USUARIO-W      TO USUARIO-CO500
           MOVE 0              TO SITUACAO-CO500.
           MOVE ZEROS          TO DATA-AGENDA-CO500
           MOVE DATA-DIA-I     TO DATA-CO500 DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV       TO DATA-E
           MOVE SPACES         TO GS-LINDET
           MOVE DATA-E         TO GS-LINDET(1: 10)
           ACCEPT HORA-BRA FROM TIME.
           MOVE HORA-BRA(1: 4) TO HORA-CO500
           MOVE HORA-CO500(1: 2) TO GS-LINDET(13: 2)
           MOVE ":"              TO GS-LINDET(15: 1)
           MOVE HORA-CO500(3: 2) TO GS-LINDET(16: 4)
           MOVE USUARIO-CO500    TO GS-LINDET(20: 5)
           MOVE GS-DATA-AGENDADA TO DATA-E
           MOVE DATA-E           TO GS-LINDET(30: 10)
           MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS TO ST-COD500.
           PERFORM UNTIL ST-COD500 = "10"
              WRITE REG-COD500 INVALID KEY
                 ADD 1 TO ITEM-CO500
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-COD500
           END-PERFORM.

           MOVE NR-CONTRATO-CO500 TO NR-CONTRATO-CO501.
           MOVE ITEM-CO500        TO ITEM-CO501.
           MOVE 0                 TO SUBITEM-CO501.

           MOVE "Previsao financeira efetuada p/ o brinde "
                    TO ANOTACAO-W(1: 41)
           MOVE NOME-CO02 TO ANOTACAO-W(42: 18)
           ADD 1 TO SUBITEM-CO501
           MOVE ANOTACAO-W     TO ANOTACAO-CO501
           MOVE SPACES         TO GS-LINDET
           MOVE ANOTACAO-CO501 TO GS-LINDET
           MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           WRITE REG-COD501 INVALID KEY
                 ADD 1 TO SUBITEM-CO501
                 WRITE REG-COD501
                 END-WRITE
           END-WRITE

           MOVE "Item"               TO ANOTACAO-W(1: 5)
           MOVE ITEM-CO50            TO ANOTACAO-W(6: 4)
           MOVE "no valor de: "      TO ANOTACAO-W(10: 13)
           MOVE CUSTO-W              TO VALOR-E
           MOVE VALOR-E              TO ANOTACAO-W(23: 14)
           MOVE "com vencto "        TO ANOTACAO-W(37: 11)
           MOVE DATA-VENC            TO DATA-E
           MOVE DATA-E               TO ANOTACAO-W(48: 11)
           ADD 1 TO SUBITEM-CO501
           MOVE ANOTACAO-W     TO ANOTACAO-CO501
           MOVE SPACES         TO GS-LINDET
           MOVE ANOTACAO-CO501 TO GS-LINDET
           MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           WRITE REG-COD501 INVALID KEY
                 ADD 1 TO SUBITEM-CO501
                 WRITE REG-COD501
                 END-WRITE
           END-WRITE
           MOVE "Forma de pagto: " TO ANOTACAO-W(1: 16)
           IF GS-FORMA-PAGTO1-PF <> SPACES
              MOVE GS-FORMA-PAGTO1-PF TO ANOTACAO-W(17: 60)
              ADD 1 TO SUBITEM-CO501
              MOVE ANOTACAO-W     TO ANOTACAO-CO501
              MOVE SPACES         TO GS-LINDET
              MOVE ANOTACAO-CO501 TO GS-LINDET
              MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              WRITE REG-COD501 INVALID KEY
                    ADD 1 TO SUBITEM-CO501
                    WRITE REG-COD501
                    END-WRITE
              END-WRITE.
           IF GS-FORMA-PAGTO2-PF <> SPACES
              MOVE SPACES TO ANOTACAO-W
              MOVE GS-FORMA-PAGTO2-PF TO ANOTACAO-W(17: 60)
              ADD 1 TO SUBITEM-CO501
              MOVE ANOTACAO-W     TO ANOTACAO-CO501
              MOVE SPACES         TO GS-LINDET
              MOVE ANOTACAO-CO501 TO GS-LINDET
              MOVE "INSERE-LIST-CONTATO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              WRITE REG-COD501 INVALID KEY
                    ADD 1 TO SUBITEM-CO501
                    WRITE REG-COD501
                    END-WRITE
              END-WRITE.
       GRAVA-PAGAR-PREV-FINANC SECTION.
      *    a pedido do vonei, cancelar atualização automática

           OPEN I-O CPD020 CPD021.
           IF ST-CPD020 <> "00"
              CONTINUE
           ELSE
              PERFORM CONT-GRAVA-PAGAR-PREV-FINANC.

       CONT-GRAVA-PAGAR-PREV-FINANC SECTION.
           PERFORM ACHA-SEQ-PAGAR.
           INITIALIZE REG-CPD020.
           MOVE SEQ-CP21             TO SEQ-CP20.
           MOVE FORNEC-CP21          TO FORNEC-CP20.
           MOVE DATA-DIA-I           TO DATA-MOVTO-CP20
                                        DATA-EMISSAO-CP20
           MOVE GS-CONTRATO          TO NR-DOCTO-CP20(1: 4)
           MOVE "-"                  TO NR-DOCTO-CP20(5: 1)
           MOVE 0                    TO NR-DOCTO-CP20(6: 1)
           IF GS-TIPO-BRINDE = 1
                MOVE ITEM-CO50        TO NR-DOCTO-CP20(7: 2)
                MOVE DATA-VENCTO-CO50 TO DATA-VENCTO-CP20
           ELSE MOVE ITEM-OE20        TO NR-DOCTO-CP20(7: 2)
                MOVE DATA-VENCTO-OE20 TO DATA-VENCTO-CP20
           END-IF
           MOVE 4                    TO TIPO-FORN-CP20
           MOVE 13                   TO PORTADOR-CP20
           MOVE 0                    TO PREV-DEF-CP20
           MOVE 1                    TO LIBERADO-CP20
           MOVE 0                    TO TIPO-MOEDA-CP20
           MOVE 0101                 TO NR-PARCELA-CP20
           MOVE WS-CODRED-BRINDE     TO CODREDUZ-APUR-CP20
      *    CONTA: LUVAS, BRINDES E DOAÇÕES CONTRATUAIS NÃO IMPORTA
      *           SE É 123-DESPESAS CONTRATO OU 2123-DESP.CONT.ORG.EVEN

           MOVE CUSTO-W              TO VALOR-TOT-CP20
           MOVE "VANDE"              TO RESPONSAVEL-CP20
           MOVE USUARIO-W            TO DIGITADOR-CP20
           MOVE NOME-CO02            TO DESCRICAO-CP20(1: 10)
           MOVE IDENTIFICACAO-CO40   TO DESCRICAO-CP20(12: 9)
           MOVE NOME-CID             TO DESCRICAO-CP20(22: 8)
           WRITE REG-CPD020 INVALID KEY
                 PERFORM ACHA-SEQ-PAGAR
                 MOVE SEQ-CP21       TO SEQ-CP20
                 WRITE REG-CPD020.
           CLOSE CPD020 CPD021.


       ACHA-SEQ-PAGAR SECTION.
           IF GS-TIPO-BRINDE = 1
              MOVE 123               TO FORNEC-CP21
           ELSE MOVE 2123            TO FORNEC-CP21.
           READ CPD021 INVALID KEY MOVE 1 TO SEQ-CP21
                                   WRITE REG-CPD021
                                   END-WRITE
                NOT INVALID KEY ADD 1 TO SEQ-CP21
                                REWRITE REG-CPD021
                                END-REWRITE.
      *------------------------------------------------------

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE IED010 IED011 CGD010 CAD010 CAD012 COD001 COD002
                 COD003 COD040 COD050 COD060 COD070 COD041 COD500
                 COD501 CGD001 CAD004 COD005 COD006 COD007 COD061
                 PRD012 MTD019

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "COP100"            to logacess-programa
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

           EXIT PROGRAM.
