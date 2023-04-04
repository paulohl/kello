       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP100.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 03/08/1998
      *DESCRIÇÃO: Movimento de caixa
      * A alteração não será permitida, devido a integração de sistemas.
      * Caso alguma digitação saia errada, exclua e digite-a novamente.
      * Em caso de estorno, no contas a pagar o lançamento relacionado
      * com o registro, voltará a ser considerado um título em aberto
      * (ou seja a pagar, o mesmo acontece no contas correntes).

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CBPX001.
           COPY CBPX100.
           COPY COPX040.
           COPY COPX050.
           COPY COPX002.
           COPY CGPX001.
           COPY CPPX020.
           COPY CPPX021.
           COPY CPPX022.
           COPY CPPX023.
           COPY CXPX100.
           COPY CXPX020.
           COPY CXPX030.
           COPY CXPX031.
           COPY CCPX100.
           COPY CCPX101.
           COPY OEPX020.
           COPY CXPX200.
           COPY LOGCAIXA.SEL.
           COPY LOGCCD.SEL.
           COPY CAPX018.
           COPY CGPX010.
           COPY CRPX020.
           COPY CHPX010.
           COPY PARX001.

           SELECT AUXILIAR ASSIGN       TO   ARQUIVO-AUXILIAR
                           ORGANIZATION IS            INDEXED
                           ACCESS MODE  IS            DYNAMIC
                           RECORD KEY   IS CHAVE = FORNEC-AUX
                                                      SEQ-AUX
                           ALTERNATE    RECORD   KEY       IS
                                     CHAVE1 = DATA-VENCTO-AUX
                                                   FORNEC-AUX
                                                      SEQ-AUX
                           ALTERNATE    RECORD   KEY       IS
                                     CHAVE2 =   DESCRICAO-AUX
                                                   FORNEC-AUX
                                                      SEQ-AUX
                           ALTERNATE    RECORD   KEY       IS
                                     CHAVE3 =   VALOR-TOT-AUX
                                                   FORNEC-AUX
                                                      SEQ-AUX
                           LOCK MODE    IS          AUTOMATIC
                           WITH LOCK    ON             RECORD
                           STATUS       IS        FS-AUXILIAR.


           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CBPW001.
       COPY CBPW100.
       COPY COPW040.
       COPY COPW050.
       COPY COPW002.
       COPY CGPW001.
       COPY CPPW020.
       COPY CPPW021.
       COPY CPPW022.
       COPY CXPW020.
       COPY CPPW023.
       COPY CXPW030.
       COPY CXPW031.
       COPY CXPW100.
       COPY CCPW100.
       COPY CCPW101.
       COPY OEPW020.
       COPY CXPW200.
       COPY LOGCAIXA.FD.
       COPY LOGCCD.FD.
       COPY CAPW018.
       COPY CGPW010.
       COPY CRPW020.
       COPY CHPW010.
       COPY PARW001.

       FD AUXILIAR.
       01 REG-AUX.
          05 FORNEC-AUX                  PIC 9(6).
          05 SEQ-AUX                     PIC 9(5).
          05 DATA-VENCTO-AUX             PIC 9(8).
          05 DESCRICAO-AUX               PIC X(30).
          05 VALOR-TOT-AUX               PIC 9(8)V99.
          05 PREV-DEF-AUX                PIC 9.
          05 LIBERADO-AUX                PIC 9.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP100.CPB".
           COPY "CXP100.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CPDIAS1".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(55).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(65).
       01  PASSAR-USUARIO            PIC X(20)    VALUE SPACES.
       01  VARIAVEIS.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-CBD100             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD021             PIC XX       VALUE SPACES.
           05  ST-CPD022             PIC XX       VALUE SPACES.
           05  ST-CPD023             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CXD030             PIC XX       VALUE SPACES.
           05  ST-CXD031             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD101             PIC XX       VALUE SPACES.
           05  ST-OED020             PIC XX       VALUE SPACES.
           05  ST-CXD200             PIC XX       VALUE SPACES.
           05  ST-LOGCCD             PIC XX       VALUE SPACES.
           05  FS-LOGCAIXA           PIC XX       VALUE SPACES.
           05  FS-AUXILIAR           PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  ULT-SEQUENCIA         PIC 9(4)     VALUE ZEROS.
      *    Ult-SEQUENCIA-Será utilizado p/ encontrar a última sequencia
      *    utilizada do movto diario
           05  ULT-SEQ-APAGAR        PIC 9(5)     VALUE ZEROS.
      * ULT-SEQ-APAGAR - ultima sequencia dentro do codigo-geral
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W = 0 (não ocorreu erro abertura) erro-w=1 (houve erro)
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  I                     PIC 9999     VALUE ZEROS.
           05  J                     PIC 9999     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  CODIGO-ACESSO         PIC 9(8)     VALUE ZEROS.
           05  DESC-W                PIC X(20)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC Z.ZZZ.ZZZ,ZZ-.
           05  TOTAL-SALDO           PIC 9(08)V99 VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  RESTO                 PIC 9(4)     VALUE ZEROS.
           05  RESTO1                PIC 9        VALUE ZEROS.
           05  QTDE-FORM             PIC 9(5)     VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  OK-ESTORNADO          PIC X        VALUE SPACES.
           05  TOTAL-SELECAO         PIC 9(09)V99 VALUE ZEROS.
           05  MASC-COD              PIC 9(05)    BLANK WHEN ZEROS.
           05  IND                   PIC 9(03)    VALUE ZEROS.
           05  DATA-INI              PIC 9(08)    VALUE ZEROS.
           05  DATA-FIM              PIC 9(08)    VALUE ZEROS.
           05  HIST-WW.
               10 HIST-W1            PIC X(25).
               10 HIST-W2            PIC X(5).
           05  HIST-W REDEFINES HIST-WW PIC X(30).
           05  MES-WS                PIC 99       VALUE ZEROS.
           05  ANO-WS                PIC 9(4)     VALUE ZEROS.
           05  SEQ-DESM-W            PIC 9(4)     VALUE ZEROS.
           05  SEQ-ANT-CIE           PIC 9(3)     VALUE ZEROS.
           05  TIPO-LCTO-W           PIC 99       VALUE ZEROS.
      *    PARA SABER A ULTIMA FUNCAO NO CONTAS CORRENTES
           05  TOTAL-BAIXA-CC        PIC 9(8)V99  VALUE ZEROS.
           05  VLR-RESTANTE-CC       PIC 9(8)V99  VALUE ZEROS.
      *    variaveis p/ ajudar no controle de baixa do contas correntes
           05  QTDE-DESMEMBRADA      PIC 9(2)     VALUE ZEROS.
      *   Verificar quantas parcelas foi desmembrado um título a pagar
           05  VLR-TOT-DESMEMBRADO   PIC 9(8)V99  VALUE ZEROS.
      * variável p/ auxiliar no lcto da ultima parcela desmembrada
           05  PERC-DESCONTO         PIC 9(3)V9(6) VALUE ZEROS.
      * calcular percentagem p/ desconto das parcelas desmembradas
           05  SEQ-SELECIONADA OCCURS 50 TIMES PIC 9(5).
           05  STRING-BANCO1.
               10  VALOR-BANC        PIC 9(8)V99.
               10  NOME-BANC         PIC X(30).
           05  STRING-BANCO REDEFINES STRING-BANCO1 PIC X(40).
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-AUX              PIC 9(08).
           05  JA-MENSAGEM           PIC X(01).
           05  LOTE-W                PIC 9(02).
           05  MASC-VALOR            PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.
       01 lsSemana                     pic 9(01).
       01 OK                           pic x(01) value spaces.
       01 qtde-dias                    pic 9(01) value zeros.
       01 DATA-MOVTO                   PIC 9(08) VALUE ZEROS.
       77 wsTexto                      pic x(255) value spaces.

       01 wsSize                       pic 9(09)    comp-5 value zeros.
       01 wsIndice                     pic 9(09)    comp-5 value zeros.
       01 umItem                       object reference.
       77 wsCheckEnable                pic 99       comp-5 value 0.
       01 umTexto                      object reference.
       01 aux-valor                    pic 9(09)v99 value zeros.
       01 colChave                     pic 9(09) comp-5 value 1.
       01 colValor                     pic 9(09) comp-5 value 6.

       01 lnkusu.
          copy usuario.cpy.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(53)   VALUE
           "CONFERENCIA DO MOVIMENTO DE CAIXA DO DIA: ".
           05  DATA-MOV-REL        PIC 99/99/9999.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(83)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(83)   VALUE
           "SEQ.  LT TP   HISTORICO                      DOCUMENTO
      -    "   VALOR    C.PAR   RED".
       01  LINDET.
           05  LINDET-REL          PIC X(83)   VALUE SPACES.

       01 TABELA-DIAS        PIC X(024) VALUE
          "312831303130313130313031".
       01 TAB-DIAS           PIC 9(002) OCCURS 12 TIMES
                             REDEFINES TABELA-DIAS.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 AUX-USUARIO         PIC X(05).
       01 AUX-OPERACAO        PIC X(01).

       01 WS-DATA-SYS.
          05 WS-DATA-CPU      PIC 9(08).
          05 FILLER REDEFINES WS-DATA-CPU.
             10 WS-ANO-CPU    PIC 9(04).
             10 WS-MES-CPU    PIC 9(02).
             10 WS-DIA-CPU    PIC 9(02).
          05 FILLER           PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       01 lnkIP                pic x(20).
       01 lnkMac               pic x(20).
       01 lnkTipoIp            pic x(20).
       01 lnkHost              pic x(20).

       01 indice               pic 9(02).
       01 wsitem               pic 9(09) comp-5 value zeros.

       01 lnktabelaChq.
          02 lnkobjetoscolChq  object reference occurs 99 times.

       01 lnktabelaColChq.
          02 lnkcolunasChq pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabelaCtr.
          02 lnkobjetoscolCtr  object reference occurs 99 times.

       01 lnktabelaColCtr.
          02 lnkcolunasCtr pic 9(09) comp-5 value zeros occurs 99 times.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP100-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.

           call   "PEGAIP" using lnkIp lnkMac lnkTipoIp lnkHost
           cancel "PEGAIP"

           COPY "CBDATA1.CPY".
           MOVE DATA-INV           TO DATA-INV DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV
           CANCEL "GRIDAT2"
           MOVE DATA-INV           TO DATA-DIA-INV.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CXP100-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP100-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP100-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CBD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD001.
           MOVE "CBD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD100.
           MOVE "CPD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CPD021" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD021.
           MOVE "CPD022" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD022.
           MOVE "CPD023" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD023.
           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CXD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD030.
           MOVE "CXD031" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD031.
           MOVE "CXD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD100.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "CCD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD101.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "COD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "OED020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-OED020.
           MOVE "CXD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD200.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "PAR001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "LOGCAIX" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGCAIXA
           MOVE "LOGCCD"  TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       PATH-LOGCCD

           OPEN I-O CXD100    CPD020 CPD021   COD050 OED020 CBD100
                    CCD100    CCD101 LOGCAIXA LOGCCD CAD018 CGD010
                    CRD020    CHD010 PAR001
           CLOSE    LOGCAIXA  LOGCCD CAD018   CGD010 CRD020 CHD010
                    PAR001
           OPEN I-O LOGCAIXA  LOGCCD.

           OPEN INPUT CGD001 CXD031 CPD023 CAD004 CAD018 CGD010
                      CRD020 CHD010 PAR001

           IF ST-CPD023 = "35"
              CLOSE CPD023    OPEN OUTPUT CPD023  CLOSE CPD023
              OPEN INPUT CPD023
           END-IF

           IF ST-CXD100 = "35"
              CLOSE CXD100      OPEN OUTPUT CXD100
              CLOSE CXD100      OPEN I-O    CXD100
           END-IF

           IF FS-LOGCAIXA = "35"
              CLOSE LOGCAIXA    OPEN OUTPUT LOGCAIXA
              CLOSE LOGCAIXA    OPEN I-O    LOGCAIXA
           END-IF

           IF ST-LOGCCD = "35"
              CLOSE LOGCCD      OPEN OUTPUT LOGCCD
              CLOSE LOGCCD      OPEN I-O    LOGCCD
           END-IF


           IF ST-CBD100 = "35"
              CLOSE CBD100      OPEN OUTPUT CBD100
              CLOSE CBD100      OPEN I-O    CBD100
           END-IF

           IF ST-CCD100 = "35"
              CLOSE CCD100      OPEN OUTPUT CCD100
              CLOSE CCD100      OPEN I-O    CCD100
           END-IF

           IF ST-CCD101 = "35"
              CLOSE CCD101      OPEN OUTPUT CCD101
              CLOSE CCD101      OPEN I-O CCD101
           END-IF

           CLOSE      CXD100 CPD020 CPD021 COD050 OED020 CBD100
                      CCD100 CCD101 LOGCAIXA LOGCCD

           OPEN INPUT CBD001 LOGCAIXA LOGCCD

           OPEN INPUT CXD100 CPD020 CPD021 COD050 OED020 CBD100
                      CCD100 CCD101 CPD022

           IF FS-LOGCAIXA <> "00"
              MOVE "ERRO ABERTURA LOGCAIXA: "  TO CXP100-MENSAGEM-ERRO
              MOVE FS-LOGCAIXA TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF

           IF ST-LOGCCD <> "00"
              MOVE "ERRO ABERTURA LOGCCD: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-LOGCCD TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF


           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF

           CLOSE CBD001

           IF ST-CBD100 <> "00"
              MOVE "ERRO ABERTURA CBD100: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CBD100 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-COD050 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF
           IF ST-OED020 <> "00"
              MOVE "ERRO ABERTURA OED020: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-OED020 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CPD020 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF
           IF ST-CPD021 <> "00"
              MOVE "ERRO ABERTURA CPD021: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CPD021 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF
           IF ST-CPD022 <> "00"
              MOVE "ERRO ABERTURA CPD022: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CPD022 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF
           IF ST-CPD023 <> "00"
              MOVE "ERRO ABERTURA CPD023: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CPD023 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF

           OPEN INPUT CXD020
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF

           CLOSE      CXD020

           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CCD100 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF

           IF ST-CCD101 <> "00"
              MOVE "ERRO ABERTURA CCD101: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CCD101 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF

           OPEN INPUT CXD030
           IF ST-CXD030 <> "00"
              MOVE "ERRO ABERTURA CXD030: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CXD030 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF
           CLOSE      CXD030

           IF ST-CXD031 <> "00"
              MOVE "ERRO ABERTURA CXD031: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CXD031 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF

           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-CXD100 TO CXP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP100-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario.

           MOVE 1                   TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                MOVE "Parametrizações não Encontrada"
                  TO CXP100-MENSAGEM-ERRO
                PERFORM CARREGA-MENSAGEM-ERRO
           NOT INVALID KEY
                IF FORN-CHEQUE-PAR001 IS NOT NUMERIC
                   MOVE 0 TO FORN-CHEQUE-PAR001
                END-IF
                IF CODRED-CHEQUE-PAR001 IS NOT NUMERIC
                   MOVE 0 TO CODRED-CHEQUE-PAR001
                END-IF
                IF FORN-RECEBER-PAR001 IS NOT NUMERIC
                   MOVE 0 TO FORN-RECEBER-PAR001
                END-IF
                IF CODRED-RECEBER-PAR001 IS NOT NUMERIC
                   MOVE 0 TO CODRED-RECEBER-PAR001
                END-IF

                IF FORN-CHEQUE-PAR001    NOT > 0 OR
                   CODRED-CHEQUE-PAR001  NOT > 0 OR
                   FORN-RECEBER-PAR001   NOT > 0 OR
                   CODRED-RECEBER-PAR001 NOT > 0
                   MOVE "Campos necessários sem parametros PAR001"
                     TO CXP100-MENSAGEM-ERRO
                   PERFORM CARREGA-MENSAGEM-ERRO
                ELSE
                   MOVE FORN-CHEQUE-PAR001    TO CXP100-FORN-CHEQUE
                   MOVE FORN-RECEBER-PAR001   TO CXP100-FORN-CTRECEBE
                   MOVE CODRED-CHEQUE-PAR001  TO CXP100-CODRED-CHEQUE
                   MOVE CODRED-RECEBER-PAR001 TO CXP100-CODRED-CTRECEBE.

           MOVE "N" TO JA-MENSAGEM

           IF ERRO-W = 0
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP100-CENTRALIZA-TRUE
                   PERFORM CRIAR-LISTVIEW-CHEQUE
                   PERFORM CRIAR-LISTVIEW-RECEBER
                   PERFORM CENTRALIZAR
               WHEN CXP100-VERIF-EMPRESTIMO-TRUE
      *             PERFORM VERIFICA-EMPRESTIMO
                    PERFORM VERIFICA-PRE-DATADO
               WHEN CXP100-SAVE-FLG-TRUE
                    PERFORM SALVAR-DADOS
                    PERFORM LIMPAR-DADOS
                    PERFORM CARREGA-ULTIMOS
                    PERFORM ACHAR-SEQUENCIA
                    PERFORM MOSTRA-ULT-SEQUENCIA
                    ENABLE-OBJECT D-CONTAPART
                    ENABLE-OBJECT D-TIPO-LCTO
               WHEN CXP100-LOAD-FLG-TRUE
                    PERFORM CARREGAR-DADOS
                    MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CXP100-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI-RECORD
                    PERFORM LIMPAR-DADOS
                    PERFORM CARREGA-ULTIMOS
                    PERFORM ACHAR-SEQUENCIA
                    PERFORM MOSTRA-ULT-SEQUENCIA
                    ENABLE-OBJECT D-CONTAPART
                    ENABLE-OBJECT D-TIPO-LCTO
               WHEN CXP100-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
                    PERFORM MOSTRA-ULT-SEQUENCIA
                    ENABLE-OBJECT D-CONTAPART
                    ENABLE-OBJECT D-TIPO-LCTO
               WHEN CXP100-PERMISSAO-FLG-TRUE
                    PERFORM PERMISSAO-ALTERACAO
               WHEN CXP100-BAIXA-BRINDE-TRUE
                    PERFORM BAIXA-BRINDE THRU FIM-BAIXA-BRINDE
               WHEN CXP100-BAIXA-BRINDE-OE-TRUE
                    PERFORM BAIXA-BRINDE-OE THRU FIM-BAIXA-BRINDE-OE
               WHEN CXP100-INVERTE-DATA-TRUE
                    PERFORM INVERTE-DATA-MOVTO
               WHEN CXP100-CARREGA-A-PAGAR-TRUE
                    PERFORM LER-A-PAGAR
               WHEN CXP100-TELA-VALOR-TRUE
                    PERFORM TELA-VALOR-A-PAGAR
      *        WHEN CXP100-TOTALIZA-VLR-TOTAL-TRUE
      *             PERFORM ATUALIZA-VALOR-TOTAL
               WHEN CXP100-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    PERFORM MOSTRA-ULT-SEQUENCIA
               WHEN CXP100-CARREGA-ULT-TRUE
                    PERFORM CARREGA-ULTIMOS
                    MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CXP100-TELA-POP-UP-TRUE
                    PERFORM TELA-POP-UP
               WHEN CXP100-CARREGA-LIST-BOX-TRUE
                    MOVE CXP100-LINDET(1: 4) TO SEQ-CX100
                    PERFORM CARREGAR-DADOS
               WHEN CXP100-LE-APURACAO-TRUE
                    PERFORM LE-APURACAO
               WHEN CXP100-LER-DESCRICAO-TRUE
                    PERFORM LER-DESCRICAO
               WHEN CXP100-VERIFICA-3DIAS-TRUE
                    PERFORM VERIFICA-3DIAS
               WHEN CXP100-OBTER-DATA-MOVTO-TRUE
                    PERFORM OBTER-DATA-MOVTO
               WHEN CXP100-ITEM-SELEC-FORN-TRUE
                    PERFORM ITEM-SELECIONADO-FORN
               WHEN CXP100-POPUP-BANCO-TRUE
                    PERFORM CHAMAR-POPUP-BANCO
               WHEN CXP100-POPUP-FORNEC-TRUE
                    PERFORM CHAMAR-POPUP-FORNEC
               WHEN CXP100-IMPRIMIR-CHEQUE-TRUE
                    PERFORM IMPRIMIR-CHEQUE
               WHEN CXP100-GRAVAR-CHEQUE-TRUE
                    PERFORM GRAVA-CHEQUE
               WHEN CXP100-VERIF-NR-CHEQUE-TRUE
                    PERFORM VERIF-NR-CHEQUE
               WHEN CXP100-VERIF-TIPO-SAIDA-TRUE
                    PERFORM VERIF-TIPO-SAIDA
               WHEN CXP100-LER-BANCO-TRUE
                    PERFORM LER-BANCO
               WHEN CXP100-LE-FORNEC-TRUE
                    PERFORM LER-FORNEC
               WHEN CXP100-SELECAO-PAGAR-TRUE
                    PERFORM DOCTO-SELECIONADO-PAGAR
               WHEN CXP100-ZERA-SELECAO-TRUE
                    PERFORM ZERA-DOCTO-SELECIONADO
               WHEN CXP100-TOT-DESC-ACRES-TRUE
                    PERFORM TOTALIZA-DESC-ACRES
               WHEN CXP100-CHAMAR-APURACAO-TRUE
                    PERFORM CHAMAR-APURACAO
               WHEN CXP100-VERIF-TOT-DESM-TRUE
                    PERFORM VERIF-TOT-DESMEMBRADO
               WHEN CXP100-CARREGAR-OBSERVACAO-TRUE
                    PERFORM CARREGAR-OBSERVACAO
               WHEN CXP100-VERIF-ESTORNO-TRUE
                    PERFORM VERIF-ESTORNO
               WHEN CXP100-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN CXP100-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN CXP100-LE-ALBUM-TRUE
                    PERFORM LER-ALBUM
               WHEN CXP100-POPUP-ALBUM-TRUE
                    PERFORM POPUP-ALBUM
               WHEN CXP100-CARREGAR-CHEQUES-TRUE
                    PERFORM CARREGAR-CHEQUES
               WHEN CXP100-CARREGAR-RECEBER-TRUE
                    PERFORM CARREGAR-RECEBER
               WHEN CXP100-TRATAR-EVENTO-TRUE
                    PERFORM TRATAR-EVENTO
               WHEN CXP100-TRATAR-EVENTO2-TRUE
                    PERFORM TRATAR-EVENTO2
           END-EVALUATE
           PERFORM VERIF-ESTORNO
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       TRATAR-EVENTO SECTION.
           EVALUATE CXP100-EVENTO
               WHEN 34027 SET-FOCUS EF11
               WHEN 34594 PERFORM CALCULAR-SELECIONADO
               WHEN 34596 PERFORM CALCULAR-SELECIONADO
               WHEN 34013 PERFORM ITEM-SELECIONADO
               WHEN 34592 PERFORM ITEM-SELECIONADO
           END-EVALUATE.

       ITEM-SELECIONADO SECTION.
           unshow-window win-cheques principal
           enable-object principal
           set-focus d-documento
           if cxp100-qtde-selecionada > 1
              move "VÁRIOS"                      to cxp100-historico
                                                    cxp100-documento
              move cxp100-codred-cheque          to cxp100-conta-reduz
              move cxp100-total-selecionado      to cxp100-valor
           else
              invoke cxp100-listview-cheques "size" returning wsSize

              move 0   to wsIndice
              perform wsSize times
                 add 1 to wsIndice
                 invoke cxp100-listview-cheques "itemAtIndex"
                                                   using wsIndice
                                               returning umItem
                 if umItem <> null
                    initialize wsCheckEnable
                    invoke umItem "getCheckBoxValue"
                           returning wsCheckEnable

                    if wsCheckEnable = 1

                       invoke umItem "getColumnValue"
                                         using colChave
                                     returning umTexto
                       initialize wsTexto
                       invoke umTexto "getValue" returning wsTexto
                       move wstexto(1:8) to data-movto-ch10
                       move wstexto(9:4) to seq-ch10
                       read chd010 invalid key
                            string "Cheque Não Encontrado" x"0da0"
                                   wsTexto into mensagem
                              move "C" to tipo-msg
                            perform exibir-mensagem
                       not invalid key
                            move nome-ch10                to
                                 cxp100-historico
                            move nr-cheque-ch10           to
                                 cxp100-documento
                            move cxp100-codred-cheque     to
                                 cxp100-conta-reduz
                            move cxp100-total-selecionado to
                                 cxp100-valor
                       end-read
                    end-if
                 end-if
              end-perform
           end-if
           refresh-object principal.

       TRATAR-EVENTO2 SECTION.
           EVALUATE CXP100-EVENTO
               WHEN 34027 SET-FOCUS EF23
               WHEN 34594 PERFORM CALCULAR-SELECIONADO2
               WHEN 34596 PERFORM CALCULAR-SELECIONADO2
               WHEN 34013 PERFORM ITEM-SELECIONADO2
               WHEN 34592 PERFORM ITEM-SELECIONADO2
           END-EVALUATE.

       ITEM-SELECIONADO2 SECTION.
           UNSHOW-WINDOW WIN-RECEBER PRINCIPAL
           ENABLE-OBJECT PRINCIPAL
           SET-FOCUS D-DOCUMENTO

           if cxp100-qtde-selecionada > 1
              move "VÁRIOS"                      to cxp100-historico
                                                    cxp100-documento
              move cxp100-codred-ctrecebe        to cxp100-conta-reduz
              move cxp100-total-selecionado      to cxp100-valor
           else
              invoke cxp100-listview-receber "size" returning wsSize

              move 0   to wsIndice
              perform wsSize times
                 add 1 to wsIndice
                 invoke cxp100-listview-receber "itemAtIndex"
                                                   using wsIndice
                                               returning umItem
                 if umItem <> null
                    initialize wsCheckEnable
                    invoke umItem "getCheckBoxValue"
                           returning wsCheckEnable

                    if wsCheckEnable = 1
                       invoke umItem "getColumnValue"
                                         using colChave
                                     returning umTexto
                       initialize wsTexto
                       invoke umTexto "getValue" returning wsTexto
                       move wstexto (1:9) to cod-compl-cr20
                       move wstexto(10:5) to seq-cr20
                       read crd020 invalid key
                            string "Contas a Receber Não Encontrado"
                                   x"0da0"
                                   wsTexto into mensagem
                              move "C" to tipo-msg
                            perform exibir-mensagem
                       not invalid key
                            move descricao-cr20           to
                                 cxp100-historico
                            move nr-docto-cr20            to
                                 cxp100-documento
                            move cxp100-codred-ctrecebe   to
                                 cxp100-conta-reduz
                            move cxp100-total-selecionado to
                                 cxp100-valor
                       end-read
                    end-if
                 end-if
              end-perform
           end-if
           refresh-object principal.

       CALCULAR-SELECIONADO SECTION.
          invoke cxp100-listview-cheques "size" returning wsSize

          move 0   to wsIndice
                      cxp100-total-selecionado
                      cxp100-qtde-selecionada
          perform wsSize times
             add 1 to wsIndice
             invoke cxp100-listview-cheques "itemAtIndex" using wsIndice
                                                      returning umItem
             if umItem <> null
                initialize wsCheckEnable
                invoke umItem "getCheckBoxValue"
                       returning wsCheckEnable

                if wsCheckEnable = 1
                   invoke umItem "getColumnValue"
                                     using colValor
                                 returning umTexto
                   initialize wsTexto
                   invoke umTexto "getValue" returning wsTexto
                   move function numval(wsTexto) to aux-valor

                   add aux-valor to cxp100-total-selecionado

                   add 1         to cxp100-qtde-selecionada
                end-if
             end-if
          end-perform
          refresh-object win-cheques.

       CALCULAR-SELECIONADO2 SECTION.
          invoke cxp100-listview-receber "size" returning wsSize

          move 0   to wsIndice
                      cxp100-total-selecionado
                      cxp100-qtde-selecionada

          perform wsSize times
             add 1 to wsIndice
             invoke cxp100-listview-receber "itemAtIndex" using wsIndice
                                                      returning umItem
             if umItem <> null
                initialize wsCheckEnable
                invoke umItem "getCheckBoxValue"
                       returning wsCheckEnable

                if wsCheckEnable = 1
                   invoke umItem "getColumnValue"
                                     using colValor
                                 returning umTexto
                   initialize wsTexto
                   invoke umTexto "getValue" returning wsTexto
                   move function numval(wsTexto) to aux-valor

                   add aux-valor to cxp100-total-selecionado
                   add 1         to cxp100-qtde-selecionada
                end-if
             end-if
          end-perform
          refresh-object win-receber.

       CRIAR-LISTVIEW-CHEQUE SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-cheques "adicionarColunaZ"
            using z"Chave" returning lnkobjetoscolChq(indice)
          invoke lnkobjetoscolChq(indice) "centered"
          move indice to lnkcolunasChq(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-cheques "adicionarColunaZ"
            using z"Contrato" returning lnkobjetoscolChq(indice)
          move indice to lnkcolunasChq(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-cheques "adicionarColunaZ"
            using z"Álbum" returning lnkobjetoscolChq(indice)
          move indice to lnkcolunasChq(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-cheques "adicionarColunaZ"
            using z"Nome" returning lnkobjetoscolChq(indice)
          move indice to lnkcolunasChq(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-cheques "adicionarColunaZ"
            using z"Data Vencimento" returning lnkobjetoscolChq(indice)
          invoke lnkobjetoscolChq(indice) "centered"
          move indice to lnkcolunasChq(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-cheques "adicionarColunaZ"
            using z"Valor Principal" returning lnkobjetoscolChq(indice)
          invoke lnkobjetoscolChq(indice) "RightJustified"
          move indice to lnkcolunasChq(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-cheques "adicionarColunaZ"
            using z"Valor Saldo" returning lnkobjetoscolChq(indice)
          invoke lnkobjetoscolChq(indice) "RightJustified"
          move indice to lnkcolunasChq(indice)

          perform mostrar-fonte-favo-chq
          perform mostrar-colunas-favo-chq

          invoke cxp100-listview-cheques "gridLines"
          invoke cxp100-listview-cheques "noBorder"
          invoke cxp100-listview-cheques "checkboxes".

       mostrar-colunas-favo-chq section.
          initialize wsTexto
          move "listview-cheques" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  cxp100-listview-cheques
                                  wsTexto
                                  lnktabelaChq.
       mostrar-colunas-favo-chq-fim.
           exit.

       mostrar-fonte-favo-chq section.
           move "listview-cheques" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu cxp100-listview-cheques wsTexto.
       mostrar-fonte-favo-chq-fim.
           exit.

       exportar-para-excel-chq section.
           invoke aListview "ExportarParaOExcel"
                    using cxp100-listview-cheques lnkTabelaChq.
       exportar-para-excel-chq-fim.
           EXIT.


       zebrar-itens-chq section.
           move "listview-cheques" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu cxp100-listview-cheques wsTexto
           invoke cxp100-listview-cheques "redrawallitems".
       zebrar-itens-chq-fim.
           exit.

       chamar-colunas-favo-chq section.
           move "listview-cheques" to wsTexto
           call "COLFAV" using lnkusu
                               cxp100-listview-cheques
                               wsTexto
                               lnktabelaChq

           perform mostrar-colunas-favo-chq
           perform mostrar-fonte-favo-chq
           perform zebrar-itens-chq.
       chamar-colunas-favo-chq-fim.
           exit.

       CRIAR-LISTVIEW-RECEBER SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-receber "adicionarColunaZ"
            using z"Chave" returning lnkobjetoscolCtr(indice)
          invoke lnkobjetoscolCtr(indice) "centered"
          move indice to lnkcolunasCtr(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-receber "adicionarColunaZ"
            using z"Contrato" returning lnkobjetoscolCtr(indice)
          move indice to lnkcolunasCtr(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-receber "adicionarColunaZ"
            using z"Album" returning lnkobjetoscolCtr(indice)
          move indice to lnkcolunasCtr(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-receber "adicionarColunaZ"
            using z"Nome" returning lnkobjetoscolCtr(indice)
          move indice to lnkcolunasCtr(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-receber "adicionarColunaZ"
            using z"Data Vencimento" returning lnkobjetoscolCtr(indice)
          invoke lnkobjetoscolCtr(indice) "centered"
          move indice to lnkcolunasCtr(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-receber "adicionarColunaZ"
            using z"Valor Principal" returning lnkobjetoscolCtr(indice)
          invoke lnkobjetoscolCtr(indice) "RightJustified"
          move indice to lnkcolunasCtr(indice)

      *>---
      *>---

          add 1 to indice
          invoke cxp100-listview-receber "adicionarColunaZ"
            using z"Valor Saldo" returning lnkobjetoscolCtr(indice)
          invoke lnkobjetoscolCtr(indice) "RightJustified"
          move indice to lnkcolunasCtr(indice)

          perform mostrar-fonte-favo-ctr
          perform mostrar-colunas-favo-ctr

          invoke cxp100-listview-receber "gridLines"
          invoke cxp100-listview-receber "noBorder"
          invoke cxp100-listview-receber "checkboxes".

       mostrar-colunas-favo-ctr section.
          initialize wsTexto
          move "listview-receber" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  cxp100-listview-receber
                                  wsTexto
                                  lnktabelaCtr.
       mostrar-colunas-favo-ctr-fim.
           exit.

       mostrar-fonte-favo-ctr section.
           move "listview-receber" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu cxp100-listview-receber wsTexto.
       mostrar-fonte-favo-ctr-fim.
           exit.

       exportar-para-excel-ctr section.
           invoke aListview "ExportarParaOExcel"
                    using cxp100-listview-receber lnkTabelaCtr.
       exportar-para-excel-ctr-fim.
           EXIT.

       zebrar-itens-ctr section.
           move "listview-receber" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu cxp100-listview-receber wsTexto
           invoke cxp100-listview-receber "redrawallitems".
       zebrar-itens-ctr-fim.
           exit.

       chamar-colunas-favo-ctr section.
           move "listview-receber" to wsTexto
           call "COLFAV" using lnkusu
                               cxp100-listview-receber
                               wsTexto
                               lnktabelaCtr

           perform mostrar-colunas-favo-ctr
           perform mostrar-fonte-favo-ctr
           perform zebrar-itens-ctr.
       chamar-colunas-favo-ctr-fim.
           exit.

       LER-PORTADOR SECTION.
           MOVE CXP100-PORTADOR    TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES        TO NOME-PORT.
           MOVE NOME-PORT          TO CXP100-DESC-PORTADOR
           REFRESH-OBJECT WIN-CHEQUES.

       POPUP-PORTADOR SECTION.
           CALL "CAP018T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(1: 30) TO CXP100-DESC-PORTADOR
           MOVE PASSAR-STRING-1(33: 4) TO CXP100-PORTADOR
           REFRESH-OBJECT WIN-CHEQUES.

       LER-ALBUM SECTION.
           MOVE 0                  TO CLASSIF-CG10
           MOVE CXP100-CONTRATO    TO CODIGO-CG10(1:4)
           MOVE CXP100-ALBUM       TO CODIGO-CG10(5:4)
           READ CGD010 INVALID KEY
                MOVE SPACES        TO COMPRADOR-CG10
           END-READ
           MOVE COMPRADOR-CG10     TO CXP100-DESC-ALBUM
           REFRESH-OBJECT WIN-RECEBER.

       POPUP-ALBUM SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(33: 8) TO CODIGO-CG10
           MOVE PASSAR-STRING-1(42: 1) TO CLASSIF-CG10
           READ CGD010 INVALID KEY
                INITIALIZE COMPRADOR-CG10.

           MOVE CODIGO-CG10(1:4)       TO CXP100-CONTRATO
           MOVE CODIGO-CG10(5:4)       TO CXP100-ALBUM
           MOVE COMPRADOR-CG10         TO CXP100-DESC-ALBUM
           REFRESH-OBJECT WIN-RECEBER.

       CARREGAR-CHEQUES SECTION.
           INVOKE CXP100-LISTVIEW-CHEQUES "DeleteAll"

           STRING CXP100-VENCTO-INI(5:4)
                  CXP100-VENCTO-INI(3:2)
                  CXP100-VENCTO-INI(1:2) INTO DATA-INI

           STRING CXP100-VENCTO-FIM(5:4)
                  CXP100-VENCTO-FIM(3:2)
                  CXP100-VENCTO-FIM(1:2) INTO DATA-FIM

      ****************CHEQUES EM ABERTO
           INITIALIZE REG-CHD010
           MOVE 0                  TO SITUACAO-CH10
           MOVE DATA-INI           TO DATA-VENCTO-CH10
           START CHD010 KEY IS NOT LESS ALT-CH2 INVALID KEY
                 MOVE "10" TO ST-CHD010.

           PERFORM UNTIL ST-CHD010 = "10"
                 READ CHD010 NEXT AT END
                      MOVE "10" TO ST-CHD010
                 NOT AT END
                      IF SITUACAO-CH10 <> 0 OR
                         DATA-VENCTO-CH10 > DATA-FIM
                         MOVE "10" TO ST-CHD010
                      ELSE
                         IF CXP100-PORTADOR = 0 OR PORTADOR-CH10
      *                     IF VALOR-SALDO-CH10 > 0
                               PERFORM MOVER-DADOS-CHEQUES
      *                     END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

      ****************CHEQUES PAGOS PARCIAL
           INITIALIZE REG-CHD010
           MOVE 1                  TO SITUACAO-CH10
           MOVE DATA-INI           TO DATA-VENCTO-CH10
           START CHD010 KEY IS NOT LESS ALT-CH2 INVALID KEY
                 MOVE "10" TO ST-CHD010.

           PERFORM UNTIL ST-CHD010 = "10"
                 READ CHD010 NEXT AT END
                      MOVE "10" TO ST-CHD010
                 NOT AT END
                      IF SITUACAO-CH10 <> 1 OR
                         DATA-VENCTO-CH10 > DATA-FIM
                         MOVE "10" TO ST-CHD010
                      ELSE
                         IF CXP100-PORTADOR = 0 OR PORTADOR-CH10
      *                     IF VALOR-SALDO-CH10 > 0
                               PERFORM MOVER-DADOS-CHEQUES
      *                     END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

      ****************CHEQUES PAGOS
           INITIALIZE REG-CHD010
           MOVE 2                  TO SITUACAO-CH10
           MOVE DATA-INI           TO DATA-VENCTO-CH10
           START CHD010 KEY IS NOT LESS ALT-CH2 INVALID KEY
                 MOVE "10" TO ST-CHD010.

           PERFORM UNTIL ST-CHD010 = "10"
                 READ CHD010 NEXT AT END
                      MOVE "10" TO ST-CHD010
                 NOT AT END
                      IF SITUACAO-CH10 <> 2 OR
                         DATA-VENCTO-CH10 > DATA-FIM
                         MOVE "10" TO ST-CHD010
                      ELSE
                         IF CXP100-PORTADOR = 0 OR PORTADOR-CH10
      *                     IF VALOR-SALDO-CH10 > 0
                               PERFORM MOVER-DADOS-CHEQUES
      *                     END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

           PERFORM MOSTRAR-FONTE-FAVO-CHQ
           PERFORM MOSTRAR-COLUNAS-FAVO-CHQ.


       MOVER-DADOS-CHEQUES SECTION.
           INITIALIZE INDICE
           INVOKE CXP100-LISTVIEW-CHEQUES "ADICIONARITEM"
                                          RETURNING WSITEM

      *--- Chave
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING DATA-MOVTO-CH10 SEQ-CH10 X"00" INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-CHEQUES "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASChq(INDICE) WSTEXTO

      *--- Contrato
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING CLIENTE-CH10(1:4) X"00" INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-CHEQUES "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASChq(INDICE) WSTEXTO

      *--- Álbum
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING CLIENTE-CH10(5:4) X"00" INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-CHEQUES "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASChq(INDICE) WSTEXTO

      *--- Nome
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING NOME-CH10 X"00" DELIMITED BY "   " INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-CHEQUES "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASChq(INDICE) WSTEXTO

      *--- Data Vencimento
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING DATA-VENCTO-CH10(7:2) "/"
                  DATA-VENCTO-CH10(5:2) "/"
                  DATA-VENCTO-CH10(1:4) X"00"
                  INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-CHEQUES "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASChq(INDICE) WSTEXTO

      *--- Valor Principal
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE VALOR-CH10 TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-CHEQUES "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASChq(INDICE) WSTEXTO

      *--- Valor Saldo
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE VALOR-SALDO-CH10 TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-CHEQUES "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASChq(INDICE) WSTEXTO.

       CARREGAR-RECEBER SECTION.
           INVOKE CXP100-LISTVIEW-RECEBER "DeleteAll"

           STRING CXP100-VENCTO-INI(5:4)
                  CXP100-VENCTO-INI(3:2)
                  CXP100-VENCTO-INI(1:2) INTO DATA-INI

           STRING CXP100-VENCTO-FIM(5:4)
                  CXP100-VENCTO-FIM(3:2)
                  CXP100-VENCTO-FIM(1:2) INTO DATA-FIM

           INITIALIZE REG-CRD020

           IF CXP100-CONTRATO > 0
              MOVE CLASSIF-CG10             TO CLASS-CLIENTE-CR20
              MOVE CXP100-CONTRATO          TO CLIENTE-CR20(1:4)
              MOVE CXP100-ALBUM             TO CLIENTE-CR20(5:4)
              START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                   MOVE "10" TO ST-CRD020
              END-START
              PERFORM UNTIL ST-CRD020 = "10"
                   READ CRD020 NEXT AT END
                        MOVE "10" TO ST-CRD020
                   NOT AT END
                        IF CLASSIF-CG10    <> CLASS-CLIENTE-CR20 OR
                           CXP100-CONTRATO <> CLIENTE-CR20(1:4)  OR
                          (CXP100-ALBUM     > 0 AND
                           CXP100-ALBUM    <> CLIENTE-CR20(5:4))
                           MOVE "10" TO ST-CRD020
                        ELSE
      *                    IF SITUACAO-CR20 = 0 OR 1 OR
      *                       VALOR-SALDO-CR20 > 0
                              IF DATA-VENCTO-CR20 NOT < DATA-INI AND
                                 DATA-VENCTO-CR20 NOT > DATA-FIM
                                 PERFORM MOVER-DADOS-RECEBER
                              END-IF
      *                    END-IF
                        END-IF
                   END-READ
              END-PERFORM
           ELSE
              MOVE 0           TO SITUACAO-CR20
              MOVE DATA-INI    TO DATA-VENCTO-CR20
              START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID KEY
                    MOVE "10" TO ST-CRD020
              END-START
              PERFORM UNTIL ST-CRD020 = "10"
                    READ CRD020 NEXT AT END
                         MOVE "10" TO ST-CRD020
                    NOT AT END
                         IF SITUACAO-CR20 <> 0 OR
                            DATA-VENCTO-CR20 > DATA-FIM
                            MOVE "10" TO ST-CRD020
                         ELSE
      *                     IF VALOR-SALDO-CR20 > 0
                               PERFORM MOVER-DADOS-RECEBER
      *                     END-IF
                         END-IF
                    END-READ
              END-PERFORM
              MOVE 1           TO SITUACAO-CR20
              MOVE DATA-INI    TO DATA-VENCTO-CR20
              START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID KEY
                    MOVE "10" TO ST-CRD020
              END-START
              PERFORM UNTIL ST-CRD020 = "10"
                    READ CRD020 NEXT AT END
                         MOVE "10" TO ST-CRD020
                    NOT AT END
                         IF SITUACAO-CR20 <> 1 OR
                            DATA-VENCTO-CR20 > DATA-FIM
                            MOVE "10" TO ST-CRD020
                         ELSE
      *                     IF VALOR-SALDO-CR20 > 0
                               PERFORM MOVER-DADOS-RECEBER
      *                     END-IF
                         END-IF
                    END-READ
              END-PERFORM
              MOVE 2           TO SITUACAO-CR20
              MOVE DATA-INI    TO DATA-VENCTO-CR20
              START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID KEY
                    MOVE "10" TO ST-CRD020
              END-START
              PERFORM UNTIL ST-CRD020 = "10"
                    READ CRD020 NEXT AT END
                         MOVE "10" TO ST-CRD020
                    NOT AT END
                         IF SITUACAO-CR20 <> 2 OR
                            DATA-VENCTO-CR20 > DATA-FIM
                            MOVE "10" TO ST-CRD020
                         ELSE
      *                     IF VALOR-SALDO-CR20 > 0
                               PERFORM MOVER-DADOS-RECEBER
      *                     END-IF
                         END-IF
                    END-READ
              END-PERFORM
           END-IF

           PERFORM MOSTRAR-FONTE-FAVO-CTR
           PERFORM MOSTRAR-COLUNAS-FAVO-CTR.

       MOVER-DADOS-RECEBER SECTION.
           INITIALIZE INDICE
           INVOKE CXP100-LISTVIEW-RECEBER "ADICIONARITEM"
                                          RETURNING WSITEM

      *--- Chave
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING COD-COMPL-CR20 SEQ-CR20 X"00" INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-RECEBER "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASCtr(INDICE) WSTEXTO

      *--- Contrato
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING CLIENTE-CR20(1:4) X"00" INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-RECEBER "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASCtr(INDICE) WSTEXTO

      *--- Álbum
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING CLIENTE-CR20(5:4) X"00" INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-RECEBER "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASCtr(INDICE) WSTEXTO

      *--- Nome
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE COD-COMPL-CR20 TO COD-COMPL-CG10
           READ CGD010 INVALID KEY
                MOVE "***********" TO COMPRADOR-CG10
           END-READ
           STRING COMPRADOR-CG10 X"00" DELIMITED BY "   " INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-RECEBER "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASCtr(INDICE) WSTEXTO

      *--- Data Vencimento
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING DATA-VENCTO-CR20(7:2) "/"
                  DATA-VENCTO-CR20(5:2) "/"
                  DATA-VENCTO-CR20(1:4) X"00"
                  INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-RECEBER "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASCtr(INDICE) WSTEXTO

      *--- Valor Principal
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE VALOR-TOT-CR20 TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-RECEBER "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASCtr(INDICE) WSTEXTO

      *--- Valor Saldo
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE VALOR-SALDO-CR20 TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE CXP100-LISTVIEW-RECEBER "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNASCtr(INDICE) WSTEXTO.


       CENTRALIZAR SECTION.
          move-object-handle win-cheques handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal
          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win-receber handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal
          invoke janelaPrincipal "CentralizarNoDesktop".


          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GRAVAR-LOGCAIXA SECTION.
           CLOSE      LOGCAIXA
           OPEN I-O   LOGCAIXA

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE WS-DATA-CPU           TO DATA-LANCAMENTO-LOGCAIXA
           ACCEPT WS-HORA-SYS FROM TIME
           STRING WS-HO-SYS WS-MI-SYS INTO HORA-LANCAMENTO-LOGCAIXA
           MOVE 1                     TO SEQUENCIA-LANCAMENTO-LOGCAIXA
           MOVE REG-CXD100            TO REGISTRO-CX100
           MOVE AUX-USUARIO           TO USUARIO-LOGCAIXA
           MOVE AUX-OPERACAO          TO OPERACAO-LOGCAIXA
           MOVE lnkIp                 TO IP-MAQUINA-LOGCAIXA
           MOVE lnkMAC                TO MAC-MAQUINA-LOGCAIXA
           MOVE lnkHost               TO HOST-LOGCAIXA
           WRITE REG-LOGCAIXA INVALID KEY
               MOVE "00" TO FS-LOGCAIXA
           NOT INVALID KEY
               MOVE "10" TO FS-LOGCAIXA
           END-WRITE

           PERFORM UNTIL FS-LOGCAIXA = "10"
               MOVE WS-DATA-CPU         TO DATA-LANCAMENTO-LOGCAIXA
               STRING WS-HO-SYS WS-MI-SYS INTO HORA-LANCAMENTO-LOGCAIXA
               ADD 1                    TO SEQUENCIA-LANCAMENTO-LOGCAIXA
               MOVE REG-CXD100          TO REGISTRO-CX100
               MOVE AUX-USUARIO         TO USUARIO-LOGCAIXA
               MOVE AUX-OPERACAO        TO OPERACAO-LOGCAIXA
               MOVE lnkIp               TO IP-MAQUINA-LOGCAIXA
               MOVE lnkMAC              TO MAC-MAQUINA-LOGCAIXA
               MOVE lnkHost             TO HOST-LOGCAIXA
               WRITE REG-LOGCAIXA INVALID KEY
                   MOVE "00" TO FS-LOGCAIXA
               NOT INVALID KEY
                   MOVE "10" TO FS-LOGCAIXA
               END-WRITE
           END-PERFORM

           CLOSE      LOGCAIXA
           OPEN INPUT LOGCAIXA.
       GRAVAR-LOGCAIXA-FIM.
           EXIT.

       VERIF-ESTORNO SECTION.
           MOVE "SENHA29"     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
      *        IF CXP100-ALTERACAO = "S"
                  DISABLE-OBJECT PB1
      *        ELSE
      *           ENABLE-OBJECT PB1
      *        END-IF
           NOT INVALID KEY
               ENABLE-OBJECT PB1
           END-READ

           MOVE "SENHA39"     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
               IF CXP100-ALTERACAO = "S"
                  PERFORM VERIFICAR-NUMERO-DIAS
               ELSE
                  ENABLE-OBJECT PB3
               END-IF
           NOT INVALID KEY
               IF CXP100-ALTERACAO = "S"
                  PERFORM VERIFICAR-NUMERO-DIAS
               ELSE
                  ENABLE-OBJECT PB3
               END-IF
           END-READ.

       VERIFICAR-NUMERO-DIAS SECTION.
           IF JA-MENSAGEM = "N"
              MOVE "S" TO JA-MENSAGEM
              MOVE ZEROS                 TO QTDE-DIAS

              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              MOVE "N"                   TO OK
              PERFORM UNTIL OK = "S"
                  COMPUTE LSSEMANA   =   FUNCTION REM
                         (FUNCTION INTEGER-OF-DATE (WS-DATA-CPU) , 7)

                  evaluate lssemana
                     when 1 add 1 to qtde-dias
                     when 2 add 1 to qtde-dias
                     when 3 add 1 to qtde-dias
                     when 4 add 1 to qtde-dias
                     when 5 add 1 to qtde-dias
                  end-evaluate

                  if ws-dia-cpu = 1
                     if ws-mes-cpu = 1
                        move 13               to ws-mes-cpu
                        compute ws-ano-cpu = ws-ano-cpu - 1
                     end-if
                     compute ws-mes-cpu = ws-mes-cpu - 1
                     move tab-dias(ws-mes-cpu) to ws-dia-cpu
                     add 1                    to ws-dia-cpu
                  end-if
                  compute ws-dia-cpu = ws-dia-cpu - 1

                  if qtde-dias = 7
                     move "S" to ok
                  end-if
              END-PERFORM

              STRING CXP100-DATA-MOV(5:4)
                     CXP100-DATA-MOV(3:2)
                     CXP100-DATA-MOV(1:2) INTO DATA-MOVTO

              IF DATA-MOVTO < WS-DATA-CPU
                 MOVE "SENHA53"     TO PROGRAMA-CA004
                 MOVE COD-USUARIO-W TO COD-USUARIO-CA004
                 READ CAD004 INVALID KEY
                      MOVE "Liberado até 7 Dias uteis" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                      DISABLE-OBJECT PB3
                      DISABLE-OBJECT PB22
                 NOT INVALID KEY
                      ENABLE-OBJECT PRINCIPAL
                      ENABLE-OBJECT PB3.

       CARREGAR-OBSERVACAO SECTION.
           MOVE CXP100-LINDET1(75: 05) TO SEQ-CP22
           MOVE CXP100-CONTAPART       TO FORNEC-CP22
           READ CPD022 INVALID KEY
                MOVE SPACES   TO CXP100-OBSERVACAO-PAGAR
           NOT INVALID KEY
                MOVE OBS-CP22 TO CXP100-OBSERVACAO-PAGAR.


       LER-BANCO SECTION.
           OPEN INPUT CBD001.

           MOVE CXP100-BANCOW TO CODIGO-FORN-CB01

           READ CBD001 INVALID KEY
                MOVE SPACES   TO TITULAR-CB01
           END-READ

           MOVE TITULAR-CB01  TO CXP100-NOME-BANCOW

           CLOSE      CBD001.

       LER-FORNEC SECTION.
           MOVE CXP100-NOMINAL-A-CH TO CODIGO-CG01

           READ CGD001 INVALID KEY
                MOVE SPACES         TO NOME-CG01
           END-READ

           MOVE NOME-CG01           TO CXP100-NOME-NOMINAL-A-CH.

       VERIF-TOT-DESMEMBRADO SECTION.
           MOVE ZEROS TO CXP100-TOTAL-DESMEMBRADO I.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                ADD CXP100-VALOR-GR(I) TO CXP100-TOTAL-DESMEMBRADO
           END-PERFORM.

       ZERA-DOCTO-SELECIONADO SECTION.
           MOVE 0 TO IND
           PERFORM 50 TIMES
               ADD 1 TO IND
               MOVE ZEROS TO SEQ-SELECIONADA(IND)
           END-PERFORM

           MOVE 0 TO IND
           PERFORM 200 TIMES
               ADD 1 TO IND
               MOVE SPACES TO CXP100-DOCTO-GR(IND)
               MOVE ZEROS  TO CXP100-VALOR-PGTO-GR(IND)
               MOVE ZEROS  TO CXP100-PARCIAL-GR(IND)
               MOVE ZEROS  TO CXP100-JUROS-GR(IND)
               MOVE ZEROS  TO CXP100-MULTA-GR(IND)
               MOVE ZEROS  TO CXP100-DESCONTO-GR(IND)
               MOVE ZEROS  TO CXP100-VALOR-TOTAL-GR(IND)
           END-PERFORM

           MOVE 0 TO IND
           PERFORM 10 TIMES
               ADD 1 TO IND
               MOVE ZEROS   TO CXP100-COD-APUR-GR(IND)
               MOVE SPACES  TO CXP100-DESC-APUR-GR(IND)
               MOVE ZEROS   TO CXP100-VALOR-GR(IND)
           END-PERFORM

           MOVE ZEROS TO I CXP100-TOT-VALOR-SELECIONADO.

       DOCTO-SELECIONADO-PAGAR SECTION.
           ADD 1                        TO I

           MOVE CXP100-LINDET1(75: 05)  TO SEQ-SELECIONADA(I) SEQ-CP20
           MOVE CXP100-CONTAPART        TO FORNEC-CP20

           READ CPD020 INVALID KEY
                MOVE ZEROS              TO VALOR-TOT-CP20
           END-READ

           ADD VALOR-TOT-CP20           TO CXP100-TOT-VALOR-SELECIONADO.

       VERIF-TIPO-SAIDA SECTION.
           MOVE CXP100-TIPO-SAIDA(1: 1) TO CXP100-TIPO-SAIDA1.

       VERIF-NR-CHEQUE SECTION.
           MOVE CXP100-BANCO-CH           TO CODIGO-FORN-CB100
           MOVE CXP100-NR-CHEQUE-CH       TO NR-CHEQUE-CB100
           READ CBD100 INVALID KEY
                MOVE 1                    TO CXP100-ACHOU-NR-CH
           NOT INVALID KEY
                IF SITUACAO-CB100 NOT = 1
                   MOVE 1                 TO CXP100-ACHOU-NR-CH
                ELSE
                   MOVE 0                 TO CXP100-ACHOU-NR-CH
                END-IF
           END-READ.

       GRAVA-CHEQUE SECTION.
           CLOSE      CBD100
           OPEN I-O   CBD100
           MOVE CXP100-BANCO-CH           TO CODIGO-FORN-CB100
           MOVE CXP100-NR-CHEQUE-CH       TO NR-CHEQUE-CB100
           MOVE CXP100-NOMINAL-A-CH       TO NOMINAL-A-CB100
           MOVE CXP100-VALOR-CH           TO VALOR-CB100
           MOVE 2                         TO SITUACAO-CB100
           MOVE DATA-MOVTO-W              TO DATA-EMISSAO-CB100
           MOVE SEQ-CP20                  TO SEQ-CTA-PAGAR-CB100
           MOVE ZEROS                     TO DATA-VENCTO-CB100
           REWRITE REG-CBD100 INVALID KEY
               MOVE "Erro de Regravação...CBD100" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           END-REWRITE
           CLOSE      CBD100
           OPEN INPUT CBD100.

       OBTER-DATA-MOVTO SECTION.
           MOVE DATA-MOVTO-W TO CXP100-DATA-MOV.

       LER-DESCRICAO SECTION.
           EVALUATE CXP100-OPCAO-POP-UP
              WHEN 1 PERFORM LER-TIPO-LCTO
              WHEN 2 PERFORM LER-CONTAPART
              WHEN 3 PERFORM LER-HISTORICO
              WHEN 4 PERFORM LER-CONTAREDUZ
           END-EVALUATE.

       LE-APURACAO SECTION.
           OPEN INPUT CXD020

           MOVE CXP100-CODIGO-APURACAO TO CODIGO-REDUZ-CX20

           READ CXD020 INVALID KEY
                MOVE SPACES            TO DESCRICAO-CX20
           END-READ

           MOVE DESCRICAO-CX20         TO CXP100-DESCR-APURACAO
                                          CXP100-DESCR-APURACAO

           MOVE TIPO-CONTA-CX20        TO CXP100-TIPO-CONTAW

           CLOSE CXD020.

       CHAMAR-APURACAO SECTION.
           CALL   "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CXP020T"
           MOVE PASSAR-STRING-1(52: 5) TO CXP100-CODIGO-APURACAO
           PERFORM LE-APURACAO.

       IMPRIMIR-CHEQUE SECTION.
           MOVE CXP100-NOME-NOMINAL-A-CH TO NOME-BANC
           MOVE CXP100-VALOR-CH          TO VALOR-BANC
           CALL   "CBP200" USING PARAMETROS-W STRING-BANCO.
           CANCEL "CBP200".

       CHAMAR-POPUP-BANCO SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING
           CANCEL "CBP001T"
           MOVE PASSAR-STRING(49: 6) TO CXP100-BANCOW
           PERFORM LER-BANCO.

       CHAMAR-POPUP-FORNEC SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING
           CANCEL "CBP001T"
           MOVE PASSAR-STRING(49: 6) TO CXP100-NOMINAL-A-CH
           PERFORM LER-FORNEC.

       TELA-POP-UP SECTION.
           EVALUATE CXP100-OPCAO-POP-UP
             WHEN 1 CALL "CXP031T" USING PARAMETROS-W PASSAR-PARAMETROS
                    MOVE PASSAR-PARAMETROS(33: 2) TO CXP100-TIPO-LCTO
                    CANCEL "CXP031T"
                    PERFORM LER-DESCRICAO
             WHEN 2 PERFORM CARREGA-POP-UP-FORNEC
             WHEN 3 CALL "CXP030T" USING PARAMETROS-W PASSAR-PARAMETROS
                    MOVE PASSAR-PARAMETROS(33: 2)
                         TO CXP100-COD-HISTORICO
                    CANCEL "CXP030T"
                    PERFORM LER-DESCRICAO
             WHEN 4 CALL "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
                    MOVE PASSAR-PARAMETROS(52: 5) TO CXP100-CONTA-REDUZ
                    CANCEL "CXP020T"
                    PERFORM LER-DESCRICAO
           END-EVALUATE.

       ITEM-SELECIONADO-FORN SECTION.
           MOVE CXP100-LINDET-POPUP(33: 6) TO CXP100-CONTAPART
           MOVE CXP100-LINDET-POPUP(1: 30) TO CXP100-DESCR-CONTAPART.

       CARREGA-POP-UP-FORNEC SECTION.
           MOVE CXP100-LINDET-POPUP(1: 1) TO NOME-CG01 LETRA
      *    MOVE SPACES TO NOME-CG01.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.

           MOVE ZEROS TO CXP100-CONT-POPUP
           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD001
              NOT AT END
                  MOVE NOME-CG01(1: 1) TO LETRA1
                  IF LETRA1 NOT = LETRA
                     MOVE "10" TO ST-CGD001
                  ELSE
                     CONTINUE
                     MOVE NOME-CG01     TO CXP100-LINDET-POPUP(1: 32)
                     MOVE CODIGO-CG01   TO CXP100-LINDET-POPUP(33: 06)
                     MOVE "INSERE-LIST-POP-UP" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       CARREGAR-DADOS SECTION.
           MOVE DATA-MOVTO-I        TO DATA-MOV-CX100
           READ CXD100 INVALID KEY
               MOVE "N"             TO CXP100-ALTERACAO
               INITIALIZE REG-CXD100
           NOT INVALID KEY
               MOVE "S"             TO CXP100-ALTERACAO
           END-READ
           MOVE SEQ-CX100           TO CXP100-SEQ
           MOVE HISTORICO-CX100     TO CXP100-HISTORICO
           MOVE DOCUMENTO-CX100     TO CXP100-DOCUMENTO
           MOVE TIPO-LCTO-CX100     TO CXP100-TIPO-LCTO
                                       TIPO-LCTO-CX31
           READ CXD031 INVALID KEY
                MOVE SPACES TO DESCRICAO-CX31
           END-READ
           MOVE DESCRICAO-CX31      TO CXP100-DESCR-TIPO-LCTO
           MOVE VALOR-CX100         TO CXP100-VALOR

           IF TIPO-LCTO-CX100 < 50
              MOVE "SAÍDA"          TO CXP100-TIPO-VALOR
           ELSE
              MOVE "ENTRADA"        TO CXP100-TIPO-VALOR
           END-IF
           MOVE CONTROLE-LOTE-CX100 TO CXP100-LOTE
           MOVE CONTAPART-CX100     TO CXP100-CONTAPART
                                       CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES         TO NOME-CG01
           END-READ
           MOVE NOME-CG01           TO CXP100-DESCR-CONTAPART

           OPEN INPUT CXD020

           MOVE CONTA-REDUZ-CX100   TO CXP100-CONTA-REDUZ
                                       CODIGO-REDUZ-CX20

           READ CXD020 INVALID KEY
                MOVE SPACES TO DESCRICAO-CX20
           END-READ
           MOVE DESCRICAO-CX20      TO CXP100-DESCR-CONTAREDUZ.

           IF SEQ-DESM-CX100 NOT NUMERIC
              MOVE 0 TO SEQ-DESM-CX100.

           IF SEQ-DESM-CX100 NOT = ZEROS
              MOVE 1 TO CXP100-PARC-DESMEMBRADA
           ELSE
              MOVE 0 TO CXP100-PARC-DESMEMBRADA
           END-IF

           DISABLE-OBJECT D-CONTAPART
           DISABLE-OBJECT D-TIPO-LCTO

           CLOSE CXD020.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CXD100
           MOVE CXP100-LOTE                TO LOTE-W
           INITIALIZE CXP100-DATA-BLOCK

           MOVE 0 TO IND
           PERFORM 50 TIMES
               ADD 1 TO IND
               MOVE ZEROS TO SEQ-SELECIONADA(IND)
           END-PERFORM

           MOVE 0 TO IND
           PERFORM 200 TIMES
               ADD 1 TO IND
               MOVE SPACES TO CXP100-DOCTO-GR(IND)
               MOVE ZEROS  TO CXP100-VALOR-PGTO-GR(IND)
               MOVE ZEROS  TO CXP100-PARCIAL-GR(IND)
               MOVE ZEROS  TO CXP100-JUROS-GR(IND)
               MOVE ZEROS  TO CXP100-MULTA-GR(IND)
               MOVE ZEROS  TO CXP100-DESCONTO-GR(IND)
               MOVE ZEROS  TO CXP100-VALOR-TOTAL-GR(IND)
           END-PERFORM

           MOVE 0 TO IND
           PERFORM 10 TIMES
               ADD 1 TO IND
               MOVE ZEROS   TO CXP100-COD-APUR-GR(IND)
               MOVE SPACES  TO CXP100-DESC-APUR-GR(IND)
               MOVE ZEROS   TO CXP100-VALOR-GR(IND)
           END-PERFORM

           MOVE LOTE-W                     TO CXP100-LOTE
           MOVE "N" TO JA-MENSAGEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           CLOSE    CXD100
           OPEN I-O CXD100
           MOVE DATA-MOV-CX100 TO DATA-AUX
           DELETE CXD100 INVALID KEY
               MOVE "Erro de Exclusão...CXD100" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE USUARIO-W TO AUX-USUARIO
               MOVE "E"       TO AUX-OPERACAO
               PERFORM GRAVAR-LOGCAIXA
           END-DELETE
           MOVE SEQ-CX100 TO SEQ-DESM-W.

           IF CXP100-TIPO-LCTO = 02
               PERFORM EXCLUI-A-PAGAR.

           IF CXP100-TIPO-LCTO = 03
               PERFORM EXCLUI-A-PAGAR
               PERFORM EXCLUI-BRINDE-A-PAGAR.

           IF CXP100-TIPO-LCTO = 06
               PERFORM EXCLUI-A-PAGAR
               PERFORM EXCLUI-BRINDE-A-PAGAR-OE.

           IF CXP100-TIPO-LCTO = 31
              PERFORM EXCLUI-A-PAGAR
              PERFORM EXCLUI-CONTA-CORRENTE.

           IF CXP100-TIPO-LCTO = 21 OR 68
               PERFORM EXCLUI-CONTA-CORRENTE.

           IF SEQ-DESM-W > 0
              INITIALIZE REG-CXD100
              MOVE DATA-AUX   TO DATA-MOV-CX100
              MOVE SEQ-DESM-W TO SEQ-DESM-CX100
              START CXD100 KEY IS NOT < ALT-CX300 INVALID KEY
                    MOVE "10" TO ST-CXD100
              END-START

              PERFORM UNTIL ST-CXD100 = "10"
                    READ CXD100 NEXT RECORD AT END
                         MOVE "10" TO ST-CXD100
                    NOT AT END
                         IF DATA-AUX <> DATA-MOV-CX100
                            MOVE "10" TO ST-CXD100
                         ELSE
                            IF SEQ-DESM-CX100 NOT NUMERIC
                               MOVE ZEROS TO SEQ-DESM-CX100
                            END-IF
                            IF SEQ-DESM-CX100 NOT = SEQ-DESM-W
                               MOVE "10" TO ST-CXD100
                            ELSE
                               DELETE CXD100 INVALID KEY
                                     MOVE "Erro de Exclusão...CXD100"
                                       TO MENSAGEM
                                     MOVE "C" TO TIPO-MSG
                                     PERFORM EXIBIR-MENSAGEM
                               NOT INVALID KEY
                                     MOVE USUARIO-W TO AUX-USUARIO
                                     MOVE "E"       TO AUX-OPERACAO
                                     PERFORM GRAVAR-LOGCAIXA
                               END-DELETE
                            END-IF
                         END-IF
                    END-READ
              END-PERFORM.

           CLOSE      CXD100
           OPEN INPUT CXD100.
      *    IF CXP100-ALTERACAO
      *    PERFORM LIMPAR-DADOS.
       EXCLUI-A-PAGAR SECTION.
           CLOSE    CPD020
           OPEN I-O CPD020

           MOVE SEQ-CX100           TO SEQ-CAIXA-CP20
           MOVE DATA-MOV-CX100      TO DATA-PGTO-CP20
           START CPD020 KEY IS = ALT6-CP20 INVALID KEY
              MOVE "Docto não consta no Contas a Pagar, verifique"
                  TO CXP100-MENSAGEM-ERRO
              MOVE "EXIBE-ERRO-GRAVACAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
              PERFORM UNTIL ST-CPD020 = "10"
                  READ CPD020 NEXT RECORD AT END
                       MOVE "10" TO ST-CPD020
                  NOT AT END
                       IF PORTADOR-CP20 = 49
                          PERFORM EXCLUI-PGTO-CTACORR
                       END-IF
                       IF DATA-MOV-CX100 NOT = DATA-PGTO-CP20 OR
                          SEQ-CX100      NOT = SEQ-CAIXA-CP20
                          MOVE "10" TO ST-CPD020
                       ELSE
                          MOVE ZEROS TO SITUACAO-CP20, JURO-PAGO-CP20,
                                        MULTA-ATRASO-CP20
                                        DESCONTO-CP20, VALOR-LIQ-CP20,
                                        DATA-PGTO-CP20, SEQ-CAIXA-CP20
                          REWRITE REG-CPD020 INVALID KEY
                               MOVE "Erro de Regravação...CPD020"
                                 TO MENSAGEM
                               MOVE "C" TO TIPO-MSG
                               PERFORM EXIBIR-MENSAGEM
                          END-REWRITE
                       END-IF
                  END-READ
              END-PERFORM
           END-START
           CLOSE      CPD020
           OPEN INPUT CPD020.
       EXCLUI-PGTO-CTACORR SECTION.
           CLOSE      CCD100
           OPEN I-O   CCD100

           MOVE NR-DOCTO-CP20(1: 5) TO SEQ-CC100
           MOVE FORNEC-CP20         TO FORNEC-CC100

           READ CCD100 INVALID KEY
                CONTINUE
           NOT INVALID KEY
               MOVE "SOMENTE PROGRAMADO - CTA.PAGAR" TO DESCRICAO-CC100
               MOVE ZEROS TO SITUACAO-CC100
                             DATA-PGTO-CC100
                             JUROS-PAGO-CC100
                             MULTA-PAGA-CC100
                             DESCONTO-CC100
                             VALOR-PAGO-CC100
                             SEQ-CAIXA-CC100
               REWRITE REG-CCD100 INVALID KEY
                   MOVE "Erro de Regravação...CCD100" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               END-REWRITE
           END-READ
           CLOSE      CCD100
           OPEN INPUT CCD100.

       EXCLUI-CONTA-CORRENTE SECTION.
           CLOSE    CCD100 LOGCCD
           OPEN I-O CCD100 LOGCCD

           MOVE "N"              TO OK-ESTORNADO

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           ACCEPT WS-HORA-SYS FROM TIME
           INITIALIZE REG-CCD100
           MOVE DATA-MOV-CX100      TO DATA-MOVTO-CC100
           MOVE SEQ-CX100           TO SEQ-CAIXA-CC100
           START CCD100 KEY IS NOT LESS ALT2-CC100 INVALID KEY
                 MOVE "Docto não consta no Conta Corrente, verifique"
                   TO CXP100-MENSAGEM-ERRO
                 MOVE "EXIBE-ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 PERFORM UNTIL ST-CCD100 = "10"
                     READ CCD100 NEXT RECORD AT END
                          MOVE "10" TO ST-CCD100
                     NOT AT END
                          IF SEQ-CX100      <> SEQ-CAIXA-CC100  OR
                             DATA-MOV-CX100 <> DATA-MOVTO-CC100
                             MOVE "10" TO ST-CCD100
                          ELSE
                             MOVE 2 TO SITUACAO-CC100
                             REWRITE REG-CCD100 INVALID KEY
                                  MOVE "Erro de Regravação...CCD100"
                                    TO MENSAGEM
                                  MOVE "C" TO TIPO-MSG
                                  PERFORM EXIBIR-MENSAGEM
                             NOT INVALID KEY
                                  MOVE "S"           TO OK-ESTORNADO
                                  MOVE FORNEC-CC100  TO LOGCCD-FORNEC
                                  MOVE SEQ-CC100     TO LOGCCD-SEQ
                                  MOVE USUARIO-W     TO LOGCCD-USUARIO
                                  MOVE WS-DATA-CPU   TO LOGCCD-DATA
                                  MOVE WS-HORA-SYS   TO LOGCCD-HORA
                                  MOVE "CXP100"      TO LOGCCD-PROGRAMA
                                  MOVE lnkIp     TO LOGCCD-IP-MAQUINA
                                  MOVE lnkMAC    TO LOGCCD-MAC-MAQUINA
                                  MOVE LNKHOST   TO LOGCCD-HOST
                                  WRITE REG-LOGCCD INVALID KEY
                                     REWRITE REG-LOGCCD INVALID KEY
                                        MOVE "Erro de Gravação...LOGCCD"
                                          TO MENSAGEM
                                        MOVE "C" TO TIPO-MSG
                                        PERFORM EXIBIR-MENSAGEM
                                     END-REWRITE
                                  END-WRITE
                             END-REWRITE
                          END-IF
                     END-READ
                 END-PERFORM
           END-START

           IF OK-ESTORNADO = "N"
              MOVE "Lançamento Não Estornado no Conta Corrente"
                TO CXP100-MENSAGEM-ERRO
              MOVE "EXIBE-ERRO-GRAVACAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

           CLOSE      CCD100 LOGCCD
           OPEN INPUT CCD100 LOGCCD.

       EXCLUI-BRINDE-A-PAGAR SECTION.
           CLOSE    COD050
           OPEN I-O COD050
           MOVE FUNCTION NUMVAL(DOCUMENTO-CX100(1: 4)) TO
                                NR-CONTRATO-CO50
           MOVE FUNCTION NUMVAL(DOCUMENTO-CX100(6: 5)) TO
                                ITEM-CO50
           READ COD050 INVALID KEY
               MOVE "BRINDE INEXISTENTE" TO CXP100-MENSAGEM-ERRO
               MOVE "EXIBE-ERRO-GRAVACAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               MOVE ZEROS TO DATA-PAGTO-CO50
                             VALOR-PAGO-CO50
               MOVE 0     TO REALIZADO-CO50
               REWRITE REG-COD050 INVALID KEY
                   MOVE "Erro de Regravação...COD050" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               END-REWRITE
           END-READ
           CLOSE      COD050
           OPEN INPUT COD050.

       EXCLUI-BRINDE-A-PAGAR-OE SECTION.
           CLOSE      OED020
           OPEN I-O   OED020
           MOVE FUNCTION NUMVAL(DOCUMENTO-CX100(1:4)) TO
                NR-CONTRATO-OE20
           MOVE FUNCTION NUMVAL(DOCUMENTO-CX100(6:3)) TO
                ITEM-OE20
           READ OED020 INVALID KEY
               MOVE "BRINDE INEXISTENTE EM OE" TO CXP100-MENSAGEM-ERRO
               MOVE "EXIBE-ERRO-GRAVACAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               MOVE ZEROS TO DATA-PAGTO-OE20 VALOR-PAGO-OE20
               MOVE 0 TO REALIZADO-OE20
               REWRITE REG-OED020 INVALID KEY
                   MOVE "Erro de Regravação...OED020" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               END-REWRITE
           END-READ
           CLOSE      OED020
           OPEN INPUT OED020.

       SALVAR-DADOS SECTION.
           IF CXP100-ALTERACAO = "S"
              MOVE ZEROS TO I
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
                   MOVE ZEROS TO SEQ-SELECIONADA(I)
              END-PERFORM
              MOVE ZEROS TO I
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 200
                   INITIALIZE CXP100-DOCTO-GR(I)
                              CXP100-VALOR-PGTO-GR(I)
                              CXP100-PARCIAL-GR(I)
                              CXP100-JUROS-GR(I)
                              CXP100-MULTA-GR(I)
                              CXP100-DESCONTO-GR(I)
                              CXP100-VALOR-TOTAL-GR(I)
              END-PERFORM

              MOVE ZEROS TO I CXP100-TOT-VALOR-SELECIONADO

              IF CXP100-TIPO-LCTO = 2 OR 3 OR 6 OR 31
                 MOVE CXP100-VALOR        TO AUX-VALOR

                 INITIALIZE REG-CPD020
                 MOVE SEQ-CX100           TO SEQ-CAIXA-CP20
                 MOVE DATA-MOV-CX100      TO DATA-PGTO-CP20
                 START CPD020 KEY IS NOT LESS ALT6-CP20 INVALID KEY
                    MOVE "Docto não consta no Contas a Pagar, verifique"
                    TO CXP100-MENSAGEM-ERRO
                    MOVE "EXIBE-ERRO-GRAVACAO" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 NOT INVALID KEY
                    PERFORM UNTIL ST-CPD020 = "10"
                        READ CPD020 NEXT RECORD AT END
                             MOVE "10" TO ST-CPD020
                        NOT AT END
                             IF DATA-MOV-CX100 NOT = DATA-PGTO-CP20 OR
                                SEQ-CX100      NOT = SEQ-CAIXA-CP20
                                MOVE "10" TO ST-CPD020
                             ELSE
                                ADD 1 TO I
                                MOVE SEQ-CP20          TO
                                     SEQ-SELECIONADA(I)
                                ADD  VALOR-TOT-CP20    TO
                                     CXP100-TOT-VALOR-SELECIONADO

                                IF AUX-VALOR > VALOR-TOT-CP20
                                   MOVE VALOR-TOT-CP20 TO
                                        CXP100-PARCIAL-GR(I)
                                   COMPUTE AUX-VALOR = AUX-VALOR -
                                           VALOR-TOT-CP20
                                ELSE
                                   MOVE AUX-VALOR TO
                                        CXP100-PARCIAL-GR(I)
                                END-IF




                                MOVE JURO-PAGO-CP20   TO
                                     CXP100-JUROS-GR(I)
                                MOVE MULTA-PAGA-CP20  TO
                                     CXP100-MULTA-GR(I)
                                MOVE DESCONTO-CP20    TO
                                     CXP100-DESCONTO-GR(I)
                                MOVE CXP100-PARCIAL-GR(I)   TO
                                     CXP100-VALOR-TOTAL-GR(I)

                             END-IF
                        END-READ
                    END-PERFORM
                 END-START
                 MOVE I TO CXP100-CONT-INS
              END-IF

              MOVE CXP100-DATA-MOV TO DATA-INV DATA-MOVTO-W
              CALL "GRIDAT2" USING DATA-INV
              CANCEL "GRIDAT2"
              MOVE DATA-INV        TO DATA-MOVTO-I
              MOVE CXP100-SEQ      TO ULT-SEQUENCIA

              PERFORM EXCLUI-RECORD
              MOVE ULT-SEQUENCIA   TO CXP100-SEQ
           ELSE
              PERFORM ACHAR-SEQUENCIA
              MOVE CXP100-SEQ    TO ULT-SEQUENCIA
              MOVE ULT-SEQUENCIA TO CXP100-SEQ
              INITIALIZE REG-CXD100
           END-IF

           CLOSE    CXD100
           OPEN I-O CXD100
           OPEN INPUT COD040 OED020

           IF CXP100-TIPO-LCTO = 02 OR 03 OR 06 OR 31
              PERFORM GRAVA-VARIOS-LCTOS
      *           SAIDA DO CONTAS A PAGAR E BRINDES
           ELSE
              IF CXP100-TIPO-LCTO = 21 OR 68
      *          SAIDA/ENTRADA NO CONTAS CORRENTE
                 PERFORM  GRAVA-CONTA-CORRENTE
              ELSE
                 MOVE DATA-MOVTO-I        TO DATA-MOV-CX100
                 MOVE CXP100-HISTORICO    TO HISTORICO-CX100
                 MOVE CXP100-DOCUMENTO    TO DOCUMENTO-CX100
                 MOVE CXP100-TIPO-LCTO    TO TIPO-LCTO-CX100
                 MOVE CXP100-CONTAPART    TO CONTAPART-CX100
                 MOVE CXP100-CONTA-REDUZ  TO CONTA-REDUZ-CX100
                 MOVE CXP100-LOTE         TO CONTROLE-LOTE-CX100
                 MOVE CXP100-VALOR        TO VALOR-CX100
                 MOVE ZEROS               TO SEQ-DESM-CX100
                 MOVE ULT-SEQUENCIA       TO SEQ-CX100 SEQ-DESM-W
                 IF CXP100-DESMEMBRAR = 1
                    PERFORM GRAVAR-DESMEMBRADO
                 ELSE
                    WRITE REG-CXD100 INVALID KEY
                        ADD 1 TO SEQ-CX100 ULT-SEQUENCIA
                        WRITE REG-CXD100 INVALID KEY
                            PERFORM ERRO-GRAVACAO
                        NOT INVALID KEY
                            MOVE USUARIO-W TO AUX-USUARIO
                            MOVE "I"       TO AUX-OPERACAO
                            PERFORM GRAVAR-LOGCAIXA
                        END-WRITE
                    NOT INVALID KEY
                        MOVE USUARIO-W TO AUX-USUARIO
                        MOVE "I"       TO AUX-OPERACAO
                        PERFORM GRAVAR-LOGCAIXA
                    END-WRITE
                    PERFORM MOVER-DADOS-LISTA
                    PERFORM INCREMENTA-SEQUENCIA
      *          PERFORM GERA-CIE
                 END-IF
           END-IF.
      *    EMISSÃO DE CHEQUES
           IF CXP100-TIPO-SAIDA1 = 1
              MOVE 50                     TO TIPO-LCTO-CX100
              MOVE CXP100-BANCO-CH        TO CONTAPART-CX100
              MOVE 023                    TO CONTA-REDUZ-CX100
              MOVE CXP100-NOME-BANCO-CH   TO HISTORICO-CX100
              MOVE CXP100-NR-CHEQUE-CH    TO DOCUMENTO-CX100
              MOVE CXP100-VALOR-CH        TO VALOR-CX100
              MOVE ULT-SEQUENCIA          TO SEQ-CX100
              MOVE ZEROS                  TO SEQ-DESM-CX100
              WRITE REG-CXD100 INVALID KEY
                   ADD 1 TO SEQ-CX100 ULT-SEQUENCIA
                   WRITE REG-CXD100 INVALID KEY
                         PERFORM ERRO-GRAVACAO
                   NOT INVALID KEY
                         MOVE USUARIO-W TO AUX-USUARIO
                         MOVE "I"       TO AUX-OPERACAO
                         PERFORM GRAVAR-LOGCAIXA
                   END-WRITE
              NOT INVALID KEY
                   MOVE USUARIO-W TO AUX-USUARIO
                   MOVE "I"       TO AUX-OPERACAO
                   PERFORM GRAVAR-LOGCAIXA
              END-WRITE
              PERFORM MOVER-DADOS-LISTA
              PERFORM INCREMENTA-SEQUENCIA
           END-IF
      *  PAGTO VIA HOME BANK/DOC
           IF CXP100-TIPO-SAIDA1 = 2
              MOVE 56                     TO TIPO-LCTO-CX100
              MOVE CXP100-BANCO-HOME      TO CONTAPART-CX100
              MOVE 023                    TO CONTA-REDUZ-CX100
              MOVE CXP100-HISTORICO-HOME  TO HISTORICO-CX100
              MOVE CXP100-VALOR-HOME      TO VALOR-CX100
              MOVE ULT-SEQUENCIA          TO SEQ-CX100
              WRITE REG-CXD100 INVALID KEY
                   ADD 1 TO SEQ-CX100 ULT-SEQUENCIA
                   WRITE REG-CXD100 INVALID KEY
                         PERFORM ERRO-GRAVACAO
                   NOT INVALID KEY
                         MOVE USUARIO-W TO AUX-USUARIO
                         MOVE "I"       TO AUX-OPERACAO
                         PERFORM GRAVAR-LOGCAIXA
                   END-WRITE
              NOT INVALID KEY
                   MOVE USUARIO-W TO AUX-USUARIO
                   MOVE "I"       TO AUX-OPERACAO
                   PERFORM GRAVAR-LOGCAIXA
              END-WRITE
              PERFORM MOVER-DADOS-LISTA
              PERFORM INCREMENTA-SEQUENCIA
           END-IF

           CLOSE      CXD100 COD040 OED020
           OPEN INPUT CXD100 OED020.

           IF CXP100-ALTERACAO = "S"
              PERFORM LIMPAR-DADOS.

           MOVE "N" TO CXP100-ALTERACAO.


       GRAVAR-DESMEMBRADO SECTION.
      * quando um título do caixa foi desmembrado p/vários dptos

      * lançamento geral da conta que será desmembrada - p/ conferencia
      * Quando excluir este tipo de lcto todas as contas relacionadas
      * deverá ser excluída também (seq-cx100 = seq-desm-cx100).
             MOVE ZEROS           TO SEQ-DESM-CX100
             MOVE CXP100-VALOR    TO VALOR-CX100
             MOVE 888             TO CONTA-REDUZ-CX100
             MOVE ULT-SEQUENCIA   TO SEQ-CX100
             WRITE REG-CXD100 INVALID KEY
                  ADD 1 TO SEQ-CX100 ULT-SEQUENCIA
                  WRITE REG-CXD100 INVALID KEY
                        PERFORM ERRO-GRAVACAO
                  NOT INVALID KEY
                        MOVE USUARIO-W TO AUX-USUARIO
                        MOVE "I"       TO AUX-OPERACAO
                        PERFORM GRAVAR-LOGCAIXA
                  END-WRITE
             NOT INVALID KEY
                  MOVE USUARIO-W TO AUX-USUARIO
                  MOVE "I"       TO AUX-OPERACAO
                  PERFORM GRAVAR-LOGCAIXA
             END-WRITE
             PERFORM MOVER-DADOS-LISTA
             PERFORM INCREMENTA-SEQUENCIA
      *      PERFORM GERA-CIE

             MOVE HISTORICO-CX100 TO HIST-W
             MOVE "DESM-"         TO HISTORICO-CX100(1: 5)
             MOVE HIST-W1         TO HISTORICO-CX100(6: 25)
             MOVE SEQ-DESM-W      TO SEQ-DESM-CX100
             MOVE CXP100-VALOR    TO VALOR-CX100
             MOVE 57              TO TIPO-LCTO-CX100
             MOVE 888             TO CONTA-REDUZ-CX100
             MOVE ULT-SEQUENCIA   TO SEQ-CX100
             WRITE REG-CXD100 INVALID KEY
                  ADD 1 TO SEQ-CX100 ULT-SEQUENCIA
                  WRITE REG-CXD100 INVALID KEY
                        PERFORM ERRO-GRAVACAO
                  NOT INVALID KEY
                        MOVE USUARIO-W TO AUX-USUARIO
                        MOVE "I"       TO AUX-OPERACAO
                        PERFORM GRAVAR-LOGCAIXA
                  END-WRITE
             NOT INVALID KEY
                  MOVE USUARIO-W TO AUX-USUARIO
                  MOVE "I"       TO AUX-OPERACAO
                  PERFORM GRAVAR-LOGCAIXA
             END-WRITE
             PERFORM MOVER-DADOS-LISTA
             PERFORM INCREMENTA-SEQUENCIA

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
             IF CXP100-COD-APUR-GR(I) = ZEROS
                MOVE 10 TO I
             ELSE
              MOVE ULT-SEQUENCIA         TO SEQ-CX100
              MOVE CXP100-COD-APUR-GR(I) TO CONTA-REDUZ-CX100
              MOVE CXP100-VALOR-GR(I)    TO VALOR-CX100
              MOVE CXP100-TIPO-LCTO      TO TIPO-LCTO-CX100
              MOVE SEQ-DESM-W            TO SEQ-DESM-CX100
              WRITE REG-CXD100 INVALID KEY
                  ADD 1 TO SEQ-CX100 ULT-SEQUENCIA
                  WRITE REG-CXD100 INVALID KEY
                        PERFORM ERRO-GRAVACAO
                  NOT INVALID KEY
                        MOVE USUARIO-W TO AUX-USUARIO
                        MOVE "I"       TO AUX-OPERACAO
                        PERFORM GRAVAR-LOGCAIXA
                  END-WRITE
              NOT INVALID KEY
                  MOVE USUARIO-W TO AUX-USUARIO
                  MOVE "I"       TO AUX-OPERACAO
                  PERFORM GRAVAR-LOGCAIXA
              END-WRITE
              PERFORM MOVER-DADOS-LISTA
              PERFORM INCREMENTA-SEQUENCIA
             END-IF
           END-PERFORM.

       GRAVA-VARIOS-LCTOS SECTION.
           IF CXP100-TIPO-LCTO = 31
              CLOSE    CCD100 CCD101
              OPEN I-O CCD100 CCD101.

           MOVE ZEROS TO I.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CXP100-CONT-INS
             MOVE SEQ-SELECIONADA(I) TO SEQ-CP20
             MOVE CXP100-CONTAPART   TO FORNEC-CP20
             READ CPD020 INVALID KEY
                  CONTINUE
             NOT INVALID KEY
                 MOVE DATA-MOVTO-I        TO DATA-MOV-CX100
                 MOVE CXP100-SEQ          TO SEQ-CX100
                 MOVE DESCRICAO-CP20      TO HISTORICO-CX100
                 MOVE CXP100-TIPO-LCTO    TO TIPO-LCTO-CX100
                 MOVE NR-DOCTO-CP20       TO DOCUMENTO-CX100

                 MOVE CXP100-CONTAPART    TO CONTAPART-CX100

                 MOVE CXP100-CONTA-REDUZ  TO CONTA-REDUZ-CX100

      * Feito a pedido do Anderson no dia 08/06/2009
                 IF CXP100-TIPO-LCTO = 2 or 3 or 6 or 31
                    MOVE CODREDUZ-APUR-CP20  TO CONTA-REDUZ-CX100
                 END-IF

                 MOVE CXP100-LOTE         TO CONTROLE-LOTE-CX100
                 IF CXP100-PARCIAL-GR(I) NOT = ZEROS
                    MOVE CXP100-PARCIAL-GR(I) TO VALOR-CX100
                 ELSE
                    COMPUTE VALOR-CX100 =
                            VALOR-TOT-CP20 - CXP100-DESCONTO-GR(I)
                 END-IF
                 PERFORM DESMEMBRADO-APAGAR
                 PERFORM GRAVA-A-PAGAR
                 IF CXP100-TIPO-LCTO = 03
                    MOVE NR-DOCTO-CP20(01: 04) TO NR-CONTRATO-CO50
                                                  NR-CONTRATO-CO40

                    MOVE FUNCTION NUMVAL(NR-DOCTO-CP20(06: 05))
                                               TO ITEM-CO50

                    READ COD040 INVALID KEY
                         MOVE SPACES TO CXP100-MENSAGEM-ERRO
                         STRING "Contrato não cadastrado" X"0DA0"
                                "NR-CONTRATO = " NR-CONTRATO-CO40
                           INTO CXP100-MENSAGEM-ERRO
                         EXIT PERFORM
                    END-READ

                    READ COD050 INVALID KEY
                         MOVE SPACES TO CXP100-MENSAGEM-ERRO
                         STRING "Brinde Inexistente " NR-CONTRATO-CO50
                         "-"  ITEM-CO50 INTO CXP100-MENSAGEM-ERRO
                         EXIT PERFORM
                    END-READ

                    IF REALIZADO-CO50 = 1
                       MOVE "Brinde já baixado1"
                       TO CXP100-MENSAGEM-ERRO
                       EXIT PERFORM
                    END-IF

                    MOVE CODBRINDE-CO50 TO CODIGO-CO02
                    READ COD002 INVALID KEY
                         MOVE ZEROS     TO VALOR-CO02
                         MOVE 0         TO MULT-FORM-CO02
                    END-READ

                    IF MULT-FORM-CO02 = "S" OR "s"
                       COMPUTE QTDE-FORM = QTDE-FORM-CO50 *
                                           QTDE-FORM-CO40
                    ELSE
                       MOVE QTDE-FORM-CO50  TO QTDE-FORM
                    END-IF

                    PERFORM GRAVA-BRINDE
                 END-IF
                 IF CXP100-TIPO-LCTO = 06
      *INICIO DA FUNCAO ADICIONADA
                    MOVE NR-DOCTO-CP20(01: 04) TO NR-CONTRATO-OE20
                                                  NR-CONTRATO-CO40

                    MOVE FUNCTION NUMVAL(NR-DOCTO-CP20(06: 03))
                                               TO ITEM-OE20

                    READ COD040 INVALID KEY
                         MOVE SPACES TO CXP100-MENSAGEM-ERRO
                         STRING "Contrato não cadastrado" X"0DA0"
                                "NR-CONTRATO = " NR-CONTRATO-CO40
                           INTO CXP100-MENSAGEM-ERRO
                         EXIT PERFORM
                    NOT INVALID KEY
                         READ OED020 INVALID KEY
                              MOVE SPACES TO CXP100-MENSAGEM-ERRO
                              STRING "Brinde Inexistente " NR-DOCTO-CP20
                                INTO CXP100-MENSAGEM-ERRO
                              EXIT PERFORM
                         NOT INVALID KEY
                              IF CXP100-ALTERACAO <> "S"
                                 IF REALIZADO-OE20 = 1
                                    MOVE SPACES TO CXP100-MENSAGEM-ERRO
                                    STRING
                                    "Organizacao de evento ja baixado "
      -                              NR-CONTRATO-OE20 "-" ITEM-OE20
                                         INTO CXP100-MENSAGEM-ERRO
                                    EXIT PERFORM
                                 ELSE
                                    MOVE CODBRINDE-OE20 TO CODIGO-CO02
                                    READ COD002 INVALID KEY
                                         MOVE ZEROS    TO VALOR-CO02
                                         MOVE 0        TO MULT-FORM-CO02
                                    END-READ

                                    IF MULT-FORM-CO02 = "S" OR "s"
                                       COMPUTE QTDE-FORM =
                                               QTDE-FORM-OE20 *
                                               QTDE-FORM-CO40
                                    ELSE
                                       MOVE QTDE-FORM-OE20 TO QTDE-FORM
                                    END-IF
                                 END-IF
                              END-IF
                         END-READ
                         PERFORM GRAVA-BRINDE-OE
                    END-READ
      *FIM DA FUNCAO ADICIONADA
                 END-IF
                 IF CXP100-TIPO-LCTO = 31
                    PERFORM GRAVAR-CONTA-CORRENTE
                 END-IF
             END-READ
           END-PERFORM

           IF CXP100-TIPO-LCTO = 31
              CLOSE      CCD100 CCD101
              OPEN INPUT CCD100 CCD101.

       GRAVAR-CONTA-CORRENTE SECTION.
      *  GRAVAR LANÇAMENTO NO CONTAS CORRENTES - VALOR TOTAL PAGO

      * Feito a pedido do Anderson no dia 08/06/2009
      *          MOVE CODREDUZ-APUR-CP20  TO CONTA-REDUZ-CX100
      *    MOVE CXP100-CONTA-REDUZ           TO CODREDUZ-APUR-CC100

           MOVE CODREDUZ-APUR-CP20           TO CODREDUZ-APUR-CC100
           MOVE "PAGTO SLD LIQ CTA CORRENTE" TO DESCRICAO-CC100
           MOVE VALOR-CX100                  TO VALOR-CC100
                                                VALOR-PAGO-CC100
           MOVE SEQ-DESM-W                   TO SEQ-CAIXA-CC100
           MOVE ZEROS                        TO JUROS-PAGO-CC100
                                                MULTA-PAGA-CC100
                                                DESCONTO-CC100
                                                DATA-PGTO-CC100
                                                VALOR-PAGO-CC100.

           MOVE 1                            TO CRED-DEB-CC100

           MOVE USUARIO-W                    TO RESPONSAVEL-CC100
                                                DIGITADOR-CC100
           MOVE NR-DOCTO-CP20(1:6) TO FORNEC-CC100 FORNEC-CC101
           READ CCD101 INVALID KEY
                MOVE ZEROS         TO SEQ-CC101
                WRITE REG-CCD101 INVALID KEY
                   MOVE "Erro de Gravação...CCD101" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
                END-WRITE
           END-READ
           ADD 1                      TO SEQ-CC101
           MOVE SEQ-CC101             TO SEQ-CC100
           MOVE NR-DOCTO-CP20         TO NR-DOCTO-CC100
           MOVE DATA-MOVTO-I          TO DATA-EMISSAO-CC100
                                         DATA-VENCTO-CC100

           MOVE DATA-MOV-CX100        TO DATA-MOVTO-CC100
           MOVE 19                    TO TIPO-FORN-CC100
           MOVE ZEROS                 TO TIPO-LCTO-CC100
           MOVE ZEROS                 TO SITUACAO-CC100
                                         LIBERADO-CC100
                                         TIPO-MOEDA-CC100
                                         NR-PARCELA-CC100
           MOVE ZEROS TO ST-CCD100

           PERFORM UNTIL ST-CCD100 = "10"
              WRITE REG-CCD100 INVALID KEY
                   ADD 1 TO SEQ-CC101 SEQ-CC100
              NOT INVALID KEY
                   MOVE "10" TO ST-CCD100
              END-WRITE
           END-PERFORM

           REWRITE REG-CCD101 INVALID KEY
               MOVE "Erro de Regravação...CCD101" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM.

       DESMEMBRADO-APAGAR SECTION.
      *    Valor desmembrado quando no contas a pagar, existe um título
      *    para pagamento, mas foi desmembrado por departamento na conta
      *    reduzida de apuração(cpd023) para lançamento no caixa.
           MOVE FORNEC-CP20     TO FORNEC-CP23
           MOVE SEQ-CP20        TO SEQ-CP23
           MOVE ZEROS           TO ITEM-CP23
           START CPD023 KEY IS NOT < CHAVE-CP23 INVALID KEY
                 MOVE "10" TO ST-CPD023.

           MOVE ZEROS TO QTDE-DESMEMBRADA
           PERFORM UNTIL ST-CPD023 = "10"
                 READ CPD023 NEXT RECORD AT END
                      MOVE "10" TO ST-CPD023
                 NOT AT END
                      IF FORNEC-CP20 NOT = FORNEC-CP23 OR
                         SEQ-CP20 NOT = SEQ-CP23
                         MOVE "10" TO ST-CPD023
                      ELSE
                         ADD 1 TO QTDE-DESMEMBRADA
                      END-IF
                 END-READ
           END-PERFORM.

           MOVE ULT-SEQUENCIA TO SEQ-CX100
                                 SEQ-DESM-W.

           IF QTDE-DESMEMBRADA = ZEROS
              MOVE ZEROS TO SEQ-DESM-CX100
              WRITE REG-CXD100 INVALID KEY
                   ADD 1 TO SEQ-CX100
                            ULT-SEQUENCIA
                   WRITE REG-CXD100 INVALID KEY
                         PERFORM ERRO-GRAVACAO
                   NOT INVALID KEY
                         MOVE USUARIO-W TO AUX-USUARIO
                         MOVE "I"       TO AUX-OPERACAO
                         PERFORM GRAVAR-LOGCAIXA
                   END-WRITE
              NOT INVALID KEY
                   MOVE USUARIO-W       TO AUX-USUARIO
                   MOVE "I"             TO AUX-OPERACAO
                   PERFORM GRAVAR-LOGCAIXA
              END-WRITE
              PERFORM MOVER-DADOS-LISTA
              PERFORM INCREMENTA-SEQUENCIA
      *       PERFORM GERA-CIE

      * lançamento geral da conta que será desmembrada - p/ conferencia
      * Quando excluir este tipo de lcto todas as contas relacionadas
      * deverá ser excluída também (seq-cx100 = seq-desm-cx100)
           ELSE
             MOVE ZEROS           TO SEQ-DESM-CX100
             MOVE VALOR-TOT-CP20  TO VALOR-CX100
             MOVE 888             TO CONTA-REDUZ-CX100
             WRITE REG-CXD100 INVALID KEY
                  ADD 1           TO SEQ-CX100
                                     ULT-SEQUENCIA
                  WRITE REG-CXD100 INVALID KEY
                        PERFORM ERRO-GRAVACAO
                  NOT INVALID KEY
                        MOVE USUARIO-W TO AUX-USUARIO
                        MOVE "I"       TO AUX-OPERACAO
                        PERFORM GRAVAR-LOGCAIXA
                  END-WRITE
             NOT INVALID KEY
                  MOVE USUARIO-W TO AUX-USUARIO
                  MOVE "I"       TO AUX-OPERACAO
                  PERFORM GRAVAR-LOGCAIXA
             END-WRITE
             PERFORM MOVER-DADOS-LISTA
             PERFORM INCREMENTA-SEQUENCIA
      *      PERFORM GERA-CIE

             MOVE HISTORICO-CX100      TO HIST-W
             MOVE "DESM-"              TO HISTORICO-CX100(1: 5)
             MOVE HIST-W1              TO HISTORICO-CX100(6: 25)
             MOVE ULT-SEQUENCIA        TO SEQ-CX100
             MOVE SEQ-DESM-W           TO SEQ-DESM-CX100
             MOVE VALOR-TOT-CP20       TO VALOR-CX100
             MOVE 57                   TO TIPO-LCTO-CX100
             MOVE 888                  TO CONTA-REDUZ-CX100
             WRITE REG-CXD100 INVALID KEY
                  ADD 1                TO SEQ-CX100
                                          ULT-SEQUENCIA
                  WRITE REG-CXD100 INVALID KEY
                        PERFORM ERRO-GRAVACAO
                  NOT INVALID KEY
                        MOVE USUARIO-W TO AUX-USUARIO
                        MOVE "I"       TO AUX-OPERACAO
                        PERFORM GRAVAR-LOGCAIXA
                  END-WRITE
             NOT INVALID KEY
                  MOVE USUARIO-W       TO AUX-USUARIO
                  MOVE "I"             TO AUX-OPERACAO
                  PERFORM GRAVAR-LOGCAIXA
             END-WRITE
             PERFORM MOVER-DADOS-LISTA
             PERFORM INCREMENTA-SEQUENCIA

             COMPUTE PERC-DESCONTO = CXP100-JUROS-GR(I) / VALOR-TOT-CP20

             MOVE FORNEC-CP20          TO FORNEC-CP23
             MOVE SEQ-CP20             TO SEQ-CP23
             MOVE ZEROS                TO ITEM-CP23
                                          VLR-TOT-DESMEMBRADO
             MOVE ZEROS                TO J

             START CPD023 KEY IS NOT < CHAVE-CP23 INVALID KEY
                   MOVE "10" TO ST-CPD023
             END-START
             PERFORM UNTIL ST-CPD023 = "10"
               READ CPD023 NEXT RECORD AT END
                    MOVE "10" TO ST-CPD023
               NOT AT END
                    MOVE CXP100-TIPO-LCTO TO TIPO-LCTO-CX100
                    IF FORNEC-CP20 NOT = FORNEC-CP23 OR
                       SEQ-CP20 NOT = SEQ-CP23
                       MOVE "10" TO ST-CPD023
                    ELSE
                       MOVE CODREDUZ-APUR-CP23 TO CONTA-REDUZ-CX100
                       IF CXP100-DESCONTO-GR(I) = ZEROS
                          MOVE VALOR-CP23   TO VALOR-CX100
                       ELSE
                          ADD 1 TO J
                          IF J = QTDE-DESMEMBRADA
      *  (se for a ultima parcela, jogar o restante do valor, devido
      *  diferença de arredondamento)
                             COMPUTE VALOR-CX100 = VALOR-CP23 -
                                     VLR-TOT-DESMEMBRADO
                          ELSE
                             COMPUTE VALOR-CX100 =
                                     VALOR-CP23 * PERC-DESCONTO
                          END-IF
                       END-IF
                       MOVE SEQ-DESM-W TO SEQ-DESM-CX100
                       MOVE ULT-SEQUENCIA TO SEQ-CX100
                       WRITE REG-CXD100 INVALID KEY
                            ADD 1 TO SEQ-CX100 ULT-SEQUENCIA
                            WRITE REG-CXD100 INVALID KEY
                                  PERFORM ERRO-GRAVACAO
                            NOT INVALID KEY
                                  MOVE USUARIO-W TO AUX-USUARIO
                                  MOVE "I"       TO AUX-OPERACAO
                                  PERFORM GRAVAR-LOGCAIXA
                            END-WRITE
                       NOT INVALID KEY
                            MOVE USUARIO-W TO AUX-USUARIO
                            MOVE "I"       TO AUX-OPERACAO
                            PERFORM GRAVAR-LOGCAIXA
                       END-WRITE
                       PERFORM MOVER-DADOS-LISTA
                       PERFORM INCREMENTA-SEQUENCIA
                    END-IF
                 END-READ
             END-PERFORM.
       GRAVA-CONTA-CORRENTE SECTION.
             CLOSE    CXD100
             OPEN I-O CXD100

             MOVE DATA-MOVTO-I        TO DATA-MOV-CX100
             MOVE ULT-SEQUENCIA       TO SEQ-CX100
                                         SEQ-DESM-W
             READ CXD100 INVALID KEY
                  MOVE CXP100-HISTORICO    TO HISTORICO-CX100
                  MOVE CXP100-DOCUMENTO    TO DOCUMENTO-CX100
                  MOVE CXP100-TIPO-LCTO    TO TIPO-LCTO-CX100
                  MOVE CXP100-CONTAPART    TO CONTAPART-CX100
                  MOVE CXP100-CONTA-REDUZ  TO CONTA-REDUZ-CX100
                  MOVE CXP100-LOTE         TO CONTROLE-LOTE-CX100
                  MOVE CXP100-VALOR        TO VALOR-CX100
                  MOVE ZEROS               TO SEQ-DESM-CX100
                  WRITE REG-CXD100 INVALID KEY
                       ADD 1 TO SEQ-CX100 ULT-SEQUENCIA
                       WRITE REG-CXD100 INVALID KEY
                             PERFORM ERRO-GRAVACAO
                       NOT INVALID KEY
                             MOVE USUARIO-W TO AUX-USUARIO
                             MOVE "I"       TO AUX-OPERACAO
                             PERFORM GRAVAR-LOGCAIXA
                       END-WRITE
                  NOT INVALID KEY
                       MOVE USUARIO-W TO AUX-USUARIO
                       MOVE "I"       TO AUX-OPERACAO
                       PERFORM GRAVAR-LOGCAIXA
                  END-WRITE
             NOT INVALID KEY
                  MOVE CXP100-HISTORICO    TO HISTORICO-CX100
                  MOVE CXP100-DOCUMENTO    TO DOCUMENTO-CX100
                  MOVE CXP100-TIPO-LCTO    TO TIPO-LCTO-CX100
                  MOVE CXP100-CONTAPART    TO CONTAPART-CX100
                  MOVE CXP100-CONTA-REDUZ  TO CONTA-REDUZ-CX100
                  MOVE CXP100-LOTE         TO CONTROLE-LOTE-CX100
                  MOVE CXP100-VALOR        TO VALOR-CX100
                  MOVE ZEROS               TO SEQ-DESM-CX100
                  WRITE REG-CXD100 INVALID KEY
                       ADD 1 TO SEQ-CX100 ULT-SEQUENCIA
                       WRITE REG-CXD100 INVALID KEY
                             PERFORM ERRO-GRAVACAO
                       NOT INVALID KEY
                             MOVE USUARIO-W TO AUX-USUARIO
                             MOVE "I"       TO AUX-OPERACAO
                             PERFORM GRAVAR-LOGCAIXA
                       END-WRITE
                  NOT INVALID KEY
                       MOVE USUARIO-W TO AUX-USUARIO
                       MOVE "I"       TO AUX-OPERACAO
                       PERFORM GRAVAR-LOGCAIXA
                  END-WRITE
             END-READ

             PERFORM MOVER-DADOS-LISTA
             PERFORM INCREMENTA-SEQUENCIA.

      *  GRAVAR LANÇAMENTO NO CONTAS CORRENTES - VALOR TOTAL PAGO
             CLOSE      CCD100 CCD101
             OPEN I-O   CCD100 CCD101

             MOVE CXP100-CONTA-REDUZ       TO CODREDUZ-APUR-CC100
             MOVE CXP100-HISTORICO         TO DESCRICAO-CC100
             MOVE CXP100-VALOR             TO VALOR-CC100
                                              VALOR-PAGO-CC100

             MOVE SEQ-CX100                TO SEQ-CAIXA-CC100
             MOVE ZEROS                    TO JUROS-PAGO-CC100
                                              MULTA-PAGA-CC100
                                              DESCONTO-CC100
                                              DATA-PGTO-CC100
                                              VALOR-PAGO-CC100
             IF CXP100-TIPO-LCTO = 21
                MOVE 1 TO CRED-DEB-CC100
             ELSE
                MOVE 0 TO CRED-DEB-CC100
             END-IF

             MOVE USUARIO-W             TO RESPONSAVEL-CC100
                                           DIGITADOR-CC100

             MOVE CXP100-CONTAPART      TO FORNEC-CC100
                                           FORNEC-CC101
             READ CCD101 INVALID KEY
                  MOVE ZEROS TO SEQ-CC101
                  WRITE REG-CCD101 INVALID KEY
                     MOVE "Erro de Gravação...CCD101" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                  END-WRITE
             END-READ

             ADD 1                      TO SEQ-CC101
             MOVE SEQ-CC101             TO SEQ-CC100
             MOVE CXP100-DOCUMENTO      TO NR-DOCTO-CC100

             MOVE DATA-MOVTO-I          TO DATA-EMISSAO-CC100
                                           DATA-VENCTO-CC100
                                           DATA-MOVTO-CC100

             MOVE ZEROS TO TIPO-LCTO-CC100
                           TIPO-FORN-CC100

             MOVE ZEROS TO SITUACAO-CC100
                           LIBERADO-CC100
                           TIPO-MOEDA-CC100
                           NR-PARCELA-CC100

             MOVE ZEROS TO ST-CCD100

             PERFORM UNTIL ST-CCD100 = "10"
                WRITE REG-CCD100 INVALID KEY
                     ADD 1    TO SEQ-CC101
                                 SEQ-CC100
                NOT INVALID KEY
                     MOVE "Lançamento Adicionado no Conta Corrente" TO
                     MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                     MOVE "10" TO ST-CCD100
                END-WRITE
             END-PERFORM

             REWRITE REG-CCD101 INVALID KEY
                 MOVE "Erro de Regravação...CCD101" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
             END-REWRITE

             CLOSE      CCD100 CCD101
             OPEN INPUT CCD100 CCD101.

       GRAVA-A-PAGAR SECTION.
           CLOSE      CPD020
           OPEN I-O   CPD020
           MOVE "2"                         TO SITUACAO-CP20
           EVALUATE CXP100-TIPO-LCTO
            WHEN 2
              MOVE CXP100-JUROS-GR(I)       TO JURO-PAGO-CP20
              MOVE CXP100-MULTA-GR(I)       TO MULTA-PAGA-CP20
              MOVE CXP100-DESCONTO-GR(I)    TO DESCONTO-CP20
              MOVE CXP100-VALOR-TOTAL-GR(I) TO VALOR-LIQ-CP20
            WHEN 3
              MOVE ZEROS                    TO JURO-PAGO-CP20
                                               MULTA-PAGA-CP20
                                               DESCONTO-CP20

              MOVE VALOR-TOT-CP20           TO VALOR-LIQ-CP20
            WHEN 6
              MOVE ZEROS                    TO JURO-PAGO-CP20
                                               MULTA-PAGA-CP20
                                               DESCONTO-CP20

              MOVE VALOR-TOT-CP20           TO VALOR-LIQ-CP20
            WHEN 31
              MOVE ZEROS                    TO JURO-PAGO-CP20
                                               MULTA-PAGA-CP20
                                               DESCONTO-CP20

              MOVE VALOR-TOT-CP20           TO VALOR-LIQ-CP20
           END-EVALUATE

           MOVE DATA-MOVTO-I                TO DATA-PGTO-CP20

           IF QTDE-DESMEMBRADA NOT = ZEROS
              MOVE SEQ-DESM-W  TO SEQ-CX100
           END-IF

           MOVE SEQ-CX100      TO SEQ-CAIXA-CP20
                                  SEQ-DESM-W

           REWRITE REG-CPD020 INVALID KEY
               MOVE "Erro de Regravação...CPD020" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           END-REWRITE

           CLOSE      CPD020
           OPEN INPUT CPD020
      *    PORTADOR = 49 "PROGRAMACAO PELO CONTA CORRENTE"
           IF PORTADOR-CP20 = 49
              PERFORM GRAVA-PGTO-CTA-CORR.

           IF CXP100-PARCIAL-GR(I) NOT = ZEROS
              PERFORM GRAVA-VLR-PARCIAL
           END-IF

           IF CXP100-JUROS-GR(I) NOT = ZEROS
              PERFORM GRAVA-JUROS
           END-IF

           IF CXP100-MULTA-GR(I) NOT = ZEROS
              PERFORM GRAVA-MULTA
           END-IF.

       GRAVA-PGTO-CTA-CORR SECTION.
           CLOSE    CCD100
           OPEN I-O CCD100

           MOVE NR-DOCTO-CP20(1: 5)      TO SEQ-CC100
           MOVE FORNEC-CP20              TO FORNEC-CC100
           READ CCD100 INVALID KEY
                CONTINUE
           NOT INVALID KEY
               MOVE 0                                TO SITUACAO-CC100
               MOVE SEQ-CX100                        TO SEQ-CAIXA-CC100
               MOVE CXP100-JUROS-GR(I)               TO JUROS-PAGO-CC100
               MOVE CXP100-MULTA-GR(I)               TO MULTA-PAGA-CC100
               MOVE CXP100-DESCONTO-GR(I)            TO DESCONTO-CC100
               MOVE CXP100-VALOR-TOTAL-GR(I)         TO VALOR-PAGO-CC100
               MOVE zeros                            TO DATA-PGTO-CC100
               MOVE "PGTO EFETUADO CX- PROGRAMACAO"  TO DESCRICAO-CC100
               REWRITE REG-CCD100 INVALID KEY
                   MOVE "Erro de Regravação...CCD100" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               END-REWRITE
           END-READ

           CLOSE      CCD100
           OPEN INPUT CCD100.

       GRAVA-VLR-PARCIAL SECTION.
           CLOSE      CPD021 CPD020
           OPEN I-O   CPD021 CPD020

           MOVE CXP100-CONTAPART TO FORNEC-CP21
           READ CPD021 INVALID KEY
                MOVE 1 TO SEQ-CP21
                WRITE REG-CPD021 INVALID KEY
                   MOVE "Erro de Gravação...CPD021" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
                END-WRITE
             NOT INVALID KEY
                ADD 1 TO SEQ-CP21
                REWRITE REG-CPD021 INVALID KEY
                  MOVE "Erro de Regravação...CPD021" TO
                  MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ

           MOVE SEQ-CP21            TO SEQ-CP20
           MOVE "0"                 TO SITUACAO-CP20
           MOVE ZEROS               TO SEQ-CAIXA-CP20

           COMPUTE VALOR-TOT-CP20 = VALOR-TOT-CP20 -
                (CXP100-PARCIAL-GR(I) + CXP100-DESCONTO-GR(I))

           MOVE ZEROS TO JURO-PAGO-CP20
                         MULTA-PAGA-CP20
                         DESCONTO-CP20
                         DATA-PGTO-CP20
                         VALOR-LIQ-CP20

           MOVE PARAMETROS-W(1: 5)  TO DIGITADOR-CP20

           WRITE REG-CPD020 INVALID KEY
                 PERFORM ERRO-GRAVACAO-CPD020
           END-WRITE.

           CLOSE      CPD021 CPD020
           OPEN INPUT CPD021 CPD020.

       GRAVA-JUROS SECTION.
           CLOSE      CXD100
           OPEN I-O   CXD100
      *    ADD 1                   TO ULT-SEQUENCIA.
           MOVE ULT-SEQUENCIA      TO SEQ-CX100
           MOVE SEQ-DESM-W         TO SEQ-DESM-CX100
           MOVE 241                TO CONTA-REDUZ-CX100
           MOVE 10                 TO TIPO-LCTO-CX100
           MOVE CXP100-JUROS-GR(I) TO VALOR-CX100
           WRITE REG-CXD100 INVALID KEY
                 ADD 1               TO ULT-SEQUENCIA SEQ-CX100
                 WRITE REG-CXD100 INVALID KEY
                      PERFORM ERRO-GRAVACAO
                 NOT INVALID KEY
                      MOVE USUARIO-W TO AUX-USUARIO
                      MOVE "I"       TO AUX-OPERACAO
                      PERFORM GRAVAR-LOGCAIXA
                 END-WRITE
           NOT INVALID KEY
                 MOVE USUARIO-W TO AUX-USUARIO
                 MOVE "I"       TO AUX-OPERACAO
                 PERFORM GRAVAR-LOGCAIXA
           END-WRITE.
           PERFORM MOVER-DADOS-LISTA
           PERFORM INCREMENTA-SEQUENCIA
           CLOSE      CXD100
           OPEN INPUT CXD100.

       GRAVA-MULTA SECTION.
           CLOSE      CXD100
           OPEN I-O   CXD100
      *    ADD 1                   TO ULT-SEQUENCIA.
           MOVE ULT-SEQUENCIA      TO SEQ-CX100
           MOVE SEQ-DESM-W         TO SEQ-DESM-CX100
           MOVE 716                TO CONTA-REDUZ-CX100
           MOVE 10                 TO TIPO-LCTO-CX100
           MOVE CXP100-MULTA-GR(I) TO VALOR-CX100
           WRITE REG-CXD100 INVALID KEY
                 ADD 1               TO ULT-SEQUENCIA SEQ-CX100
                 WRITE REG-CXD100 INVALID KEY
                      PERFORM ERRO-GRAVACAO
                 NOT INVALID KEY
                      MOVE USUARIO-W TO AUX-USUARIO
                      MOVE "I"       TO AUX-OPERACAO
                      PERFORM GRAVAR-LOGCAIXA
                 END-WRITE
           NOT INVALID KEY
                 MOVE USUARIO-W TO AUX-USUARIO
                 MOVE "I"       TO AUX-OPERACAO
                 PERFORM GRAVAR-LOGCAIXA
           END-WRITE
           PERFORM MOVER-DADOS-LISTA
           PERFORM INCREMENTA-SEQUENCIA
           CLOSE      CXD100
           OPEN INPUT CXD100.

       BAIXA-BRINDE SECTION.
           OPEN INPUT COD040
                      COD002.

           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA CAD040: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-COD040 TO CXP100-MENSAGEM-ERRO(23: 02)
              GO FIM-BAIXA-BRINDE.

           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-COD002 TO CXP100-MENSAGEM-ERRO(23: 02)
              GO FIM-BAIXA-BRINDE.

           IF CXP100-DOCUMENTO = SPACES
              MOVE "0000000000" TO CXP100-DOCUMENTO.


           MOVE SPACES TO CXP100-MENSAGEM-ERRO


           MOVE ZEROS TO I
                         TOTAL-SELECAO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CXP100-CONT-INS
             MOVE SEQ-SELECIONADA(I) TO SEQ-CP20
             MOVE CXP100-CONTAPART   TO FORNEC-CP20
             READ CPD020 INVALID KEY
                  CONTINUE
             NOT INVALID KEY
                  MOVE NR-DOCTO-CP20(01: 04) TO NR-CONTRATO-CO50
                                                NR-CONTRATO-CO40

                  MOVE FUNCTION NUMVAL(NR-DOCTO-CP20(06: 05))
                                             TO ITEM-CO50

                  READ COD040 INVALID KEY
                       MOVE SPACES TO CXP100-MENSAGEM-ERRO
                       STRING "Contrato não cadastrado" X"0DA0"
                              "NR-CONTRATO = " NR-CONTRATO-CO40
                         INTO CXP100-MENSAGEM-ERRO
                       EXIT PERFORM
                  END-READ

                  READ COD050 INVALID KEY
                       MOVE "Brinde Inexistente!!!"
                         TO CXP100-MENSAGEM-ERRO
                       EXIT PERFORM
                  END-READ

                  IF CXP100-ALTERACAO <> "S"
                     IF REALIZADO-CO50 = 1
                        MOVE "Brinde já baixado2"
                          TO CXP100-MENSAGEM-ERRO
                        EXIT PERFORM
                     END-IF
                  END-IF

                  ADD VALOR-TOT-CP20  TO TOTAL-SELECAO

             END-READ
           END-PERFORM

           IF CXP100-MENSAGEM-ERRO <> SPACES
              GO FIM-BAIXA-BRINDE.

           IF CXP100-TOT-VALOR-SELECIONADO <> TOTAL-SELECAO
              MOVE "Total Selecionado Não Bate com o Valor Informado"
                TO CXP100-MENSAGEM-ERRO
              GO FIM-BAIXA-BRINDE.

           MOVE 1 TO CXP100-ERRO.
       FIM-BAIXA-BRINDE SECTION.
           CLOSE COD040 COD002.

       BAIXA-BRINDE-OE SECTION.
           OPEN INPUT COD040 COD002.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA CAD040: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-COD040 TO CXP100-MENSAGEM-ERRO(23: 02)
              GO FIM-BAIXA-BRINDE-OE.

           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA CAD090: "  TO CXP100-MENSAGEM-ERRO
              MOVE ST-COD002 TO CXP100-MENSAGEM-ERRO(23: 02)
              GO FIM-BAIXA-BRINDE-OE.

           IF CXP100-DOCUMENTO = SPACES
              MOVE "0000000000" TO CXP100-DOCUMENTO
           END-IF


           MOVE SPACES TO CXP100-MENSAGEM-ERRO


           MOVE ZEROS TO I
                         TOTAL-SELECAO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CXP100-CONT-INS
             MOVE SEQ-SELECIONADA(I) TO SEQ-CP20
             MOVE CXP100-CONTAPART   TO FORNEC-CP20
             READ CPD020 INVALID KEY
                  CONTINUE
             NOT INVALID KEY
                  MOVE NR-DOCTO-CP20(01: 04) TO NR-CONTRATO-OE20
                                                NR-CONTRATO-CO40

                  MOVE FUNCTION NUMVAL(NR-DOCTO-CP20(06: 03))
                                             TO ITEM-OE20

                  READ COD040 INVALID KEY
                       MOVE SPACES TO CXP100-MENSAGEM-ERRO
                       STRING "Contrato não cadastrado" X"0DA0"
                              "NR-CONTRATO = " NR-CONTRATO-CO40
                         INTO CXP100-MENSAGEM-ERRO
                       EXIT PERFORM
                  END-READ

                  READ OED020 INVALID KEY
                       MOVE SPACES TO CXP100-MENSAGEM-ERRO
                       STRING "Organizacao Inexistente - "
                               NR-DOCTO-CP20 INTO CXP100-MENSAGEM-ERRO
                       EXIT PERFORM
                  END-READ

                  IF CXP100-ALTERACAO <> "S"
                     IF REALIZADO-OE20 = 1
                        MOVE SPACES TO CXP100-MENSAGEM-ERRO
                        STRING "Organizacao já baixado - "
                                NR-DOCTO-CP20 INTO CXP100-MENSAGEM-ERRO
                        EXIT PERFORM
                     END-IF
                  END-IF

                  ADD VALOR-TOT-CP20  TO TOTAL-SELECAO

                  MOVE CODBRINDE-OE20 TO CODIGO-CO02
                  READ COD002 INVALID KEY
                       MOVE ZEROS     TO VALOR-CO02
                       MOVE 0         TO MULT-FORM-CO02
                  END-READ

                  IF MULT-FORM-CO02 = "S" OR "s"
                     COMPUTE QTDE-FORM =
                             QTDE-FORM-OE20 * QTDE-FORM-CO40
                  ELSE
                     MOVE QTDE-FORM-OE20         TO QTDE-FORM
                  END-IF

             END-READ
           END-PERFORM

           IF CXP100-MENSAGEM-ERRO <> SPACES
              GO FIM-BAIXA-BRINDE-OE.

           IF CXP100-TOT-VALOR-SELECIONADO <> TOTAL-SELECAO
              MOVE "Total Selecionado Não Bate com o Valor Informado"
                TO CXP100-MENSAGEM-ERRO
              GO FIM-BAIXA-BRINDE-OE.

           MOVE 1 TO CXP100-ERRO.

      *    repetir a funcao da baixa de brindes acima pois aqui esta
      *    individual
      *
      *    MOVE CXP100-DOCUMENTO(01: 04)  TO NR-CONTRATO-OE20
      *                                      NR-CONTRATO-CO40.
      *
      *    MOVE CXP100-DOCUMENTO(06: 03)  TO ITEM-OE20
      *
      *    READ COD040 INVALID KEY
      *         MOVE "Contrato não cadastrado" TO CXP100-MENSAGEM-ERRO
      *    NOT INVALID KEY
      *         READ OED020 INVALID KEY
      *              MOVE "Brinde Inexistente" TO CXP100-MENSAGEM-ERRO
      *         NOT INVALID KEY
      *              IF REALIZADO-OE20 = 1
      *                 MOVE "Brinde já baixado" TO CXP100-MENSAGEM-ERRO
      *              ELSE
      *                 MOVE CODBRINDE-OE20 TO CODIGO-CO02
      *                 READ COD002 INVALID KEY
      *                      MOVE ZEROS TO VALOR-CO02
      *                      MOVE 0 TO MULT-FORM-CO02
      *                 END-READ
      *                 IF MULT-FORM-CO02 = "S" OR "s"
      *                    COMPUTE QTDE-FORM =
      *                            QTDE-FORM-OE20 * QTDE-FORM-CO40
      *                 ELSE
      *                    MOVE QTDE-FORM-OE20         TO QTDE-FORM
      *                 END-IF
      *                 MOVE 1 TO CXP100-ERRO
      *              END-IF
      *         END-READ
      *    END-READ.
       FIM-BAIXA-BRINDE-OE.
           CLOSE COD040 COD002.

       CALCULA-DIAS-PRAZO SECTION.
           MOVE DATA-PREV-VENDA-CO40 TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO GRDIAS-AAMMDD-FINAL
           MOVE DATA-VENCTO-CO50     TO GRDIAS-AAMMDD-INICIAL

           IF GRDIAS-AAMMDD-INICIAL > GRDIAS-AAMMDD-FINAL
              MOVE ZEROS TO DIAS-PRAZO-CO50
           ELSE
              CALL "GRDIAS1" USING PARAMETROS-GRDIAS
              CANCEL "GRDIAS1"
              MOVE GRDIAS-NUM-DIAS TO DIAS-PRAZO-CO50.

       CALCULA-DIAS-PRAZO-OE SECTION.
           MOVE DATA-PREV-VENDA-CO40 TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO GRDIAS-AAMMDD-FINAL
           MOVE DATA-VENCTO-OE20     TO GRDIAS-AAMMDD-INICIAL
           IF GRDIAS-AAMMDD-INICIAL > GRDIAS-AAMMDD-FINAL
              MOVE ZEROS TO DIAS-PRAZO-CO50
           ELSE
              CALL "GRDIAS1" USING PARAMETROS-GRDIAS
              CANCEL "GRDIAS1"
              MOVE GRDIAS-NUM-DIAS TO DIAS-PRAZO-OE20.

       GRAVA-BRINDE SECTION.
           CLOSE      COD050
           OPEN I-O   COD050

           COMPUTE VALOR-PREVISTO-CO50 = VALOR-TOT-CP20 / QTDE-FORM
           MOVE VALOR-TOT-CP20      TO VALOR-PAGO-CO50

      *    COMPUTE VALOR-PREVISTO-CO50 = CXP100-VALOR / QTDE-FORM.
      *    MOVE CXP100-VALOR        TO VALOR-PAGO-CO50

           MOVE 1                   TO REALIZADO-CO50

           MOVE CXP100-DATA-MOV     TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           CANCEL "GRIDAT2"
           MOVE DATA-INV            TO DATA-PAGTO-CO50

      *    MOVE DATA-DIA-INV        TO DATA-PAGTO-CO50
           PERFORM CALCULA-DIAS-PRAZO
           REWRITE REG-COD050 INVALID KEY
               MOVE "Erro de Regravação...COD050" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           END-REWRITE

           CLOSE      COD050
           OPEN INPUT COD050.

       GRAVA-BRINDE-OE SECTION.
           CLOSE      OED020
           OPEN I-O   OED020

           COMPUTE VALOR-PREVISTO-OE20 = VALOR-TOT-CP20 / QTDE-FORM
           MOVE VALOR-TOT-CP20      TO VALOR-PAGO-OE20

      *    COMPUTE VALOR-PREVISTO-OE20 = CXP100-VALOR / QTDE-FORM
      *    MOVE CXP100-VALOR        TO VALOR-PAGO-OE20

           MOVE 1                   TO REALIZADO-OE20
           MOVE DATA-MOVTO-W        TO DATA-PAGTO-OE20
           PERFORM CALCULA-DIAS-PRAZO-OE
           REWRITE REG-OED020 INVALID KEY
               MOVE "Erro de Regravação...OED020" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           END-REWRITE
           CLOSE      OED020
           OPEN INPUT OED020.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CXP100-MENSAGEM-ERRO
           MOVE ST-CXD100       TO CXP100-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-SEQUENCIA.
           SUBTRACT 1 FROM ULT-SEQUENCIA.
       ERRO-GRAVACAO-CPD020 SECTION.
           MOVE "ERRO GRAVAÇÃO CPD020" TO CXP100-MENSAGEM-ERRO
           MOVE ST-CPD020              TO CXP100-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       INVERTE-DATA-MOVTO SECTION.
           MOVE CXP100-DATA-MOV TO DATA-INV DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           CANCEL "GRIDAT2".
           MOVE DATA-INV TO DATA-MOVTO-I.
           PERFORM ACHAR-SEQUENCIA.
       VERIFICA-3DIAS SECTION.
           MOVE DATA-DIA-INV TO GRDIAS-AAMMDD-FINAL.
           MOVE DATA-MOVTO-I TO GRDIAS-AAMMDD-INICIAL.
           CALL "GRDIAS1" USING PARAMETROS-GRDIAS.
           CANCEL "GRDIAS1".
      *    IF GRDIAS-NUM-DIAS > 3 MOVE 1 TO CXP100-ERRO-3DIAS
      *    ELSE MOVE 0 TO CXP100-ERRO-3DIAS
      *    END-IF.
           IF GRDIAS-NUM-DIAS > 3
              PERFORM GRAVA-ANOTACAO.

       GRAVA-ANOTACAO SECTION.
           OPEN I-O CXD200.
           IF ST-CXD200 = "35"
              CLOSE CXD200 OPEN OUTPUT CXD200
              CLOSE CXD200 OPEN I-O    CXD200.

           MOVE DATA-DIA-INV   TO DATA-OCORRENCIA-CX200.
           MOVE ZEROS          TO SEQ-CX200.
           START CXD200 KEY IS NOT < CHAVE-CX200 INVALID KEY
                 MOVE "10" TO ST-CXD200.
           MOVE ZEROS TO ULT-SEQ.
           PERFORM UNTIL ST-CXD200 = "10"
             READ CXD200 NEXT RECORD AT END
                  MOVE "10" TO ST-CXD200
             NOT AT END
                 IF DATA-OCORRENCIA-CX200 <> DATA-DIA-INV
                    MOVE "10" TO ST-CXD200
                 ELSE
                    MOVE SEQ-CX200 TO ULT-SEQ
                 END-IF
             END-READ
           END-PERFORM.
           ADD 1                       TO ULT-SEQ
           MOVE ULT-SEQ                TO SEQ-CX200
           MOVE DATA-DIA-INV           TO DATA-OCORRENCIA-CX200
           MOVE HORA-REL               TO HORA-OCORRENCIA-CX200
           MOVE USUARIO-W              TO USUARIO-CX200
           MOVE 0                      TO SITUACAO-ANOTACAO-CX200
           MOVE CXP100-DATA-MOV        TO DATA-E
           MOVE CXP100-DESCR-CONTAPART TO DESC-W
           MOVE CXP100-VALOR           TO VALOR-E
           MOVE "LCTO ATRASADO-"       TO DESCRICAO-CX200(1: 14)
           MOVE  DATA-E                TO DESCRICAO-CX200(15: 10)
           MOVE ". SEQ: "              TO DESCRICAO-CX200(25: 7)
           MOVE CXP100-SEQ             TO DESCRICAO-CX200(31: 4)
           MOVE ". FORNEC: "           TO DESCRICAO-CX200(35: 10)
           MOVE DESC-W                 TO DESCRICAO-CX200(45: 21)
           MOVE ". VALOR: "            TO DESCRICAO-CX200(66: 9)
           MOVE VALOR-E                TO DESCRICAO-CX200(75: 13)
           WRITE REG-CXD200 INVALID KEY
               MOVE "Erro de Gravação...CXD200" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           END-WRITE
           CLOSE CXD200.
       PERMISSAO-ALTERACAO SECTION.
           COMPUTE CODIGO-ACESSO = DATA-DIA-INV * 0,0005.
           IF CXP100-SENHA NOT = CODIGO-ACESSO
              MOVE 1                 TO CXP100-ERRO-PERMISSAO
           ELSE
              MOVE ZEROS             TO CXP100-ERRO-PERMISSAO
           END-IF.

       LER-CONTAREDUZ SECTION.
           OPEN INPUT CXD020
           MOVE CXP100-CONTA-REDUZ   TO CODIGO-REDUZ-CX20
           READ CXD020 INVALID KEY
                MOVE SPACES          TO DESCRICAO-CX20
           END-READ
           MOVE DESCRICAO-CX20       TO CXP100-DESCR-CONTAREDUZ
           MOVE TIPO-CONTA-CX20      TO CXP100-TIPO-CONTAW
           CLOSE      CXD020.

       LER-CONTAPART SECTION.
           MOVE CXP100-CONTAPART     TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES          TO NOME-CG01
           END-READ
           MOVE NOME-CG01            TO CXP100-DESCR-CONTAPART.

       LER-HISTORICO SECTION.
           OPEN INPUT CXD030
           MOVE CXP100-COD-HISTORICO TO COD-HISTORICO-CX30
           READ CXD030 INVALID KEY
                MOVE SPACES          TO HISTORICO-CX30
           END-READ
           MOVE HISTORICO-CX30       TO CXP100-HISTORICO
           CLOSE      CXD030.

       LER-TIPO-LCTO SECTION.
           MOVE CXP100-TIPO-LCTO     TO TIPO-LCTO-CX31
           READ CXD031 INVALID KEY
                MOVE SPACES          TO DESCRICAO-CX31
           END-READ

           MOVE DESCRICAO-CX31       TO CXP100-DESCR-TIPO-LCTO
           IF CXP100-TIPO-LCTO < 50
              MOVE "SAÍDA"           TO CXP100-TIPO-VALOR
           ELSE
              MOVE "ENTRADA"         TO CXP100-TIPO-VALOR.

       CARREGA-ULTIMOS SECTION.
           CLOSE      CXD100
           OPEN I-O   CXD100
           MOVE "CLEAR-LIST-BOX"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE DATA-MOVTO-I         TO DATA-MOV-CX100
           MOVE ZEROS                TO SEQ-CX100
           MOVE ZEROS                TO CXP100-CONT
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.

           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END
                   MOVE "10"         TO ST-CXD100
              NOT AT END
                IF DATA-MOV-CX100 NOT NUMERIC
                   DELETE CXD100 NOT INVALID KEY
                       MOVE USUARIO-W TO AUX-USUARIO
                       MOVE "T"       TO AUX-OPERACAO
                       PERFORM GRAVAR-LOGCAIXA
                   END-DELETE
                ELSE
                   IF DATA-MOVTO-I NOT = DATA-MOV-CX100
                      MOVE "10"       TO ST-CXD100
                   ELSE
                      PERFORM MOVER-DADOS-LISTA
                   END-IF
                END-IF
              END-READ
           END-PERFORM
           CLOSE      CXD100
           OPEN INPUT CXD100.
      *    ADD 1 TO ULT-SEQUENCIA.
           MOVE ULT-SEQUENCIA          TO CXP100-SEQ.

       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES            TO CXP100-LINDET
           MOVE SEQ-CX100         TO CXP100-LINDET(01: 06)
      *                              ULT-SEQUENCIA
           MOVE CONTROLE-LOTE-CX100 TO CXP100-LINDET(07:02)
           MOVE TIPO-LCTO-CX100   TO CXP100-LINDET(10: 02)
           MOVE HISTORICO-CX100   TO CXP100-LINDET(14: 30)
           MOVE DOCUMENTO-CX100   TO CXP100-LINDET(45: 10)
           MOVE VALOR-CX100       TO VALOR-E
           MOVE VALOR-E           TO CXP100-LINDET(57: 13)
           IF TIPO-LCTO-CX100 < 50
              MOVE "D"            TO CXP100-LINDET(71: 1)
           ELSE
              MOVE "C"            TO CXP100-LINDET(71: 1)
           END-IF
           MOVE CONTAPART-CX100   TO CXP100-LINDET(74: 06)
                                     CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES TO NOME-CG01
           END-READ

           MOVE NOME-CG01         TO CXP100-LINDET(81: 21)
           MOVE CONTA-REDUZ-CX100 TO MASC-COD
           MOVE MASC-COD          TO CXP100-LINDET(104: 05)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LER-A-PAGAR SECTION.
           MOVE "CLEAR-LIST-BOX1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO ARQUIVO-AUXILIAR
           STRING "\PROGRAMA\KELLO\" USUARIO-W INTO ARQUIVO-AUXILIAR

           OPEN OUTPUT AUXILIAR
           CLOSE       AUXILIAR
           OPEN I-O    AUXILIAR

           INITIALIZE REG-CPD020

           MOVE CXP100-CONTAPART TO FORNEC-CP20
           MOVE ZEROS            TO SITUACAO-CP20
                                    DATA-VENCTO-CP20
                                    ST-CPD020.

           STRING CXP100-VENCTO-INI(5:4)
                  CXP100-VENCTO-INI(3:2)
                  CXP100-VENCTO-INI(1:2) INTO DATA-VENCTO-CP20

           STRING CXP100-VENCTO-FIM(5:4)
                  CXP100-VENCTO-FIM(3:2)
                  CXP100-VENCTO-FIM(1:2) INTO DATA-FIM

           START CPD020 KEY IS NOT < ALT4-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.

           MOVE ZEROS TO CXP100-CONT1.
           PERFORM UNTIL ST-CPD020 = "10"
              READ CPD020 NEXT RECORD AT END
                   MOVE "10" TO ST-CPD020
              NOT AT END
                 IF SITUACAO-CP20    > 0 OR
                    FORNEC-CP20     <> CXP100-CONTAPART OR
                    DATA-VENCTO-CP20 > DATA-FIM
                    MOVE "10" TO ST-CPD020
                 ELSE
                    MOVE DATA-VENCTO-CP20 TO DATA-VENCTO-AUX
                    MOVE DESCRICAO-CP20   TO DESCRICAO-AUX
                    MOVE VALOR-TOT-CP20   TO VALOR-TOT-AUX
                    MOVE PREV-DEF-CP20    TO PREV-DEF-AUX
                    MOVE LIBERADO-CP20    TO LIBERADO-AUX
                    MOVE SEQ-CP20         TO SEQ-AUX
                    MOVE FORNEC-CP20      TO FORNEC-AUX

                    WRITE REG-AUX

                 END-IF
               END-READ
           END-PERFORM

           CLOSE      AUXILIAR
           OPEN INPUT AUXILIAR


           INITIALIZE REG-AUX CXP100-CONT-INS
           EVALUATE CXP100-ORDEM
               WHEN 1 START AUXILIAR KEY IS NOT LESS CHAVE1 INVALID KEY
                            MOVE "10" TO FS-AUXILIAR
                      END-START
               WHEN 2 START AUXILIAR KEY IS NOT LESS CHAVE2 INVALID KEY
                            MOVE "10" TO FS-AUXILIAR
                      END-START
               WHEN 3 START AUXILIAR KEY IS NOT LESS CHAVE3 INVALID KEY
                            MOVE "10" TO FS-AUXILIAR
                      END-START
           END-EVALUATE

           PERFORM UNTIL FS-AUXILIAR = "10"
               READ AUXILIAR NEXT AT END
                    MOVE "10" TO FS-AUXILIAR
               NOT AT END
                    MOVE SPACES           TO CXP100-LINDET1
                    MOVE DATA-VENCTO-AUX  TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    CANCEL "GRIDAT1"
                    MOVE DATA-INV         TO DATA-E
                    MOVE DATA-E           TO CXP100-LINDET1(01: 11)
                    MOVE DESCRICAO-AUX    TO CXP100-LINDET1(12: 30)
                    MOVE VALOR-TOT-AUX    TO VALOR-E
                    MOVE VALOR-E          TO CXP100-LINDET1(44: 13)
                    IF PREV-DEF-AUX  = 0
                       MOVE "D"           TO CXP100-LINDET1(60: 01)
                    ELSE
                       MOVE "P"           TO CXP100-LINDET1(60: 01)
                    END-IF
                    IF LIBERADO-AUX  = 0
                       MOVE "NÃO"         TO CXP100-LINDET1(64: 03)
                    ELSE
                       MOVE "SIM"         TO CXP100-LINDET1(64: 03)
                    END-IF
                    MOVE SEQ-AUX          TO CXP100-LINDET1(75:05)
                    MOVE FORNEC-AUX       TO CXP100-LINDET1(82:6)
                    MOVE "INSERE-LIST1" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM

                    ADD 1 TO CXP100-CONT-INS
               END-READ
           END-PERFORM


           CLOSE AUXILIAR.

       TELA-VALOR-A-PAGAR SECTION.
           MOVE ZEROS TO CXP100-VALOR
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CXP100-CONT-INS
              IF SEQ-SELECIONADA(I) = ZEROS
                 MOVE 200 TO I
              ELSE
                 MOVE CXP100-CONTAPART           TO FORNEC-CP20
                 MOVE SEQ-SELECIONADA(I)         TO SEQ-CP20
                 READ CPD020 NOT INVALID KEY
                      IF CXP100-CONT-INS = 1
                         MOVE DESCRICAO-CP20     TO CXP100-HISTORICO
                         MOVE NR-DOCTO-CP20      TO CXP100-DOCUMENTO
                         MOVE VALOR-TOT-CP20     TO CXP100-VALOR
                         MOVE CODREDUZ-APUR-CP20 TO CXP100-CONTA-REDUZ
                      ELSE
                         MOVE "VARIOS"           TO CXP100-HISTORICO
                                                    CXP100-DOCUMENTO
                        MOVE 999                 TO CXP100-CONTA-REDUZ
                        ADD VALOR-TOT-CP20       TO CXP100-VALOR
                      END-IF
                      MOVE NR-DOCTO-CP20    TO CXP100-DOCTO-GR(I)
                      MOVE VALOR-TOT-CP20   TO CXP100-VALOR-PGTO-GR(I)
                 END-READ
              END-IF
           END-PERFORM
           REFRESH-OBJECT PRINCIPAL.

       TOTALIZA-DESC-ACRES SECTION.
           MOVE ZEROS TO CXP100-TOTAL-GERAL-GRUPO.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 200
             IF CXP100-PARCIAL-GR(I) NOT = ZEROS
                MOVE CXP100-PARCIAL-GR(I)   TO CXP100-VALOR-TOTAL-GR(I)
                ADD CXP100-VALOR-TOTAL-GR(I)TO CXP100-TOTAL-GERAL-GRUPO
             ELSE
                COMPUTE CXP100-VALOR-TOTAL-GR(I) =
                        CXP100-VALOR-PGTO-GR(I) + CXP100-JUROS-GR(I) +
                        CXP100-MULTA-GR(I) - CXP100-DESCONTO-GR(I)
                ADD CXP100-VALOR-TOTAL-GR(I) TO CXP100-TOTAL-GERAL-GRUPO
             END-IF
           END-PERFORM.
           MOVE CXP100-TOTAL-GERAL-GRUPO TO CXP100-VALOR CXP100-VALOR-CH
                                            CXP100-VALOR-HOME.
           MOVE "SAIDA" TO CXP100-TIPO-VALOR.
      *ATUALIZA-VALOR-TOTAL SECTION.
      *    COMPUTE CXP100-VLR-TOTAL = CXP100-VLR-PGTO-TOTAL +
      *       CXP100-VLR-PGTO-PARCIAL + CXP100-VLR-JUROS +
      *       CXP100-VLR-PGTO-MULTA.

      *GERA-CIE SECTION.
      *    MOVE SPACES              TO DESCRICAO-MENS-CI10.
      *    IF CXP100-TIPO-LCTO = 01
      *       PERFORM ACHA-SEQ-CIE
      *       MOVE 05               TO COD-MENS-PADRAO-CI10
      *       MOVE 01               TO FUNCAO-DESTINO-CI10
      *       MOVE NOME-CG01        TO DESCRICAO-MENS-CI10(01: 15)
      *       MOVE HISTORICO-CX100  TO DESCRICAO-MENS-CI10(17: 30)
      *       MOVE VALOR-CX100      TO VALOR-E
      *       MOVE VALOR-E          TO DESCRICAO-MENS-CI10(48: 13)
      *       PERFORM GRAVA-CIE.
      **    PAGTO A VISTA SEM PROGRAMAÇÃO
      **    ENVIADO P/ A DIRETORIA
      *    IF CXP100-TIPO-LCTO = 02 AND LIBERADO-CP20 = 0
      *       PERFORM ACHA-SEQ-CIE
      *       MOVE 01              TO FUNCAO-DESTINO-CI10
      *       MOVE 08              TO COD-MENS-PADRAO-CI10
      *       MOVE NOME-CG01       TO DESCRICAO-MENS-CI10(01: 15)
      *       MOVE HISTORICO-CX100 TO DESCRICAO-MENS-CI10(17: 31)
      *       MOVE VALOR-CX100     TO VALOR-E
      *       MOVE VALOR-E         TO DESCRICAO-MENS-CI10(48: 13)
      *       PERFORM GRAVA-CIE.
      **    Pagto programado mas não liberado pela diretoria.
      **    Enviado p/ a diretoria
      *    IF CXP100-TIPO-LCTO = 03
      *       PERFORM ACHA-SEQ-CIE
      *       MOVE 07               TO COD-MENS-PADRAO-CI10
      *       MOVE 02               TO FUNCAO-DESTINO-CI10
      *       MOVE HISTORICO-CX100  TO DESCRICAO-MENS-CI10(01: 31)
      *       MOVE CXP100-DOCUMENTO TO DESCRICAO-MENS-CI10(32: 11)
      *       MOVE VALOR-CX100      TO VALOR-E
      *       MOVE VALOR-E          TO DESCRICAO-MENS-CI10(43: 14)
      *       PERFORM GRAVA-CIE.
      **    Pagto de brindes - enviado p/ o gerente de contratos
      *ACHA-SEQ-CIE SECTION.
      *    CLOSE CIED010. OPEN INPUT CIED010.
      *    MOVE DATA-DIA-INV    TO DATA-CI10.
      *    MOVE ZEROS           TO SEQ-CI10.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      READ CIED010 NEXT RECORD AT END MOVE "10" TO ST-CIED010
      *       NOT AT END
      *         IF DATA-CI10 NOT = DATA-DIA-INV MOVE "10" TO ST-CIED010
      *            MOVE SEQ-CI10  TO SEQ-ANT-CIE
      *         ELSE CONTINUE
      *      END-READ
      *    END-PERFORM.
      *    CLOSE CIED010.  OPEN I-O CIED010.
      *    INITIALIZE REG-CIED010.
      *    MOVE SEQ-ANT-CIE  TO SEQ-CI10
      *    MOVE DATA-DIA-INV TO DATA-CI10.
      *GRAVA-CIE SECTION.
      *    MOVE DATA-DIA-INV   TO DATA-CI10
      *    ADD 1               TO SEQ-CI10
      *    ACCEPT HORA-W       FROM TIME.
      *    MOVE HORA-W(1: 4)   TO HORA-CI10
      *    MOVE USUARIO-W      TO ORIGEM-CI10
      **    CODIGO DO USUARIO DESTINO (KELLO)
      *    MOVE ZEROS          TO ST-CIED010.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      WRITE REG-CIED010 INVALID KEY
      *             ADD 1 TO SEQ-CI10
      *         NOT INVALID KEY MOVE "10" TO ST-CIED010
      *    END-PERFORM.=
       CLEAR-FLAGS SECTION.
           INITIALIZE CXP100-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       VERIFICA-EMPRESTIMO SECTION.
           MOVE ZEROS TO SITUACAO-CP20 DATA-VENCTO-CP20 FORNEC-CP20.
           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.

           PERFORM UNTIL ST-CPD020 = "10"
             READ CPD020 NEXT RECORD AT END
                  MOVE "10" TO ST-CPD020
             NOT AT END
                  IF SITUACAO-CP20 NOT = 0 OR
                     DATA-VENCTO-CP20 > DATA-DIA-INV
                     MOVE "10" TO ST-CPD020
                  ELSE
                     IF TIPO-FORN-CP20 NOT = 3 OR
                        PREV-DEF-CP20 = 0
                        CONTINUE
                     ELSE
                        PERFORM ATUALIZA-EMPRESTIMO
                        MOVE "10" TO ST-CPD020
                     END-IF
                  END-IF
             END-READ
           END-PERFORM.

       VERIFICA-PRE-DATADO SECTION.
           MOVE ZEROS TO SITUACAO-CP20
                         DATA-VENCTO-CP20
                         FORNEC-CP20

           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.

           PERFORM UNTIL ST-CPD020 = "10"
                 READ CPD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CPD020
                 NOT AT END
                    IF SITUACAO-CP20 NOT = 0 OR
                       DATA-VENCTO-CP20 > DATA-DIA-INV
                       MOVE "10" TO ST-CPD020
                    ELSE
                       IF PORTADOR-CP20 NOT = 12
                          CONTINUE
                       ELSE
                          PERFORM ATUALIZA-CHEQUE-PRE
                          MOVE "10" TO ST-CPD020
                       END-IF
                    END-IF
                 END-READ
           END-PERFORM.
       ATUALIZA-EMPRESTIMO SECTION.
           MOVE USUARIO-W TO PASSAR-USUARIO.
           CALL   "CPP057" USING PARAMETROS-W PASSAR-USUARIO.
           CANCEL "CPP057".
       ATUALIZA-CHEQUE-PRE SECTION.
           CALL   "CPP059" USING PARAMETROS-W.
           CANCEL "CPP059".
       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.

           MOVE DATA-MOVTO-I TO DATA-MOV-CX100
           MOVE ZEROS        TO SEQ-CX100
                                TOTAL-SALDO.

           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.

           MOVE ZEROS TO LIN

           PERFORM CABECALHO

           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD100
              NOT AT END
                IF DATA-MOV-CX100 <> DATA-MOVTO-I
                   MOVE "10" TO ST-CXD100
                ELSE
                   MOVE SPACES            TO LINDET-REL
                   MOVE SEQ-CX100         TO LINDET-REL(01: 06)
                   MOVE CONTROLE-LOTE-CX100 TO LINDET-REL(07:02)
                   MOVE TIPO-LCTO-CX100   TO LINDET-REL(10: 02)
                   MOVE HISTORICO-CX100   TO LINDET-REL(14: 30)
                   MOVE DOCUMENTO-CX100   TO LINDET-REL(45: 10)
                   MOVE VALOR-CX100       TO VALOR-E
                   MOVE VALOR-E           TO LINDET-REL(57: 13)
                   MOVE CONTAPART-CX100   TO LINDET-REL(72: 06)
                   MOVE CONTA-REDUZ-CX100 TO LINDET-REL(80: 05)

                   IF TIPO-LCTO-CX100 < 50
                      COMPUTE TOTAL-SALDO = TOTAL-SALDO - VALOR-CX100
                   ELSE
                      ADD VALOR-CX100 TO TOTAL-SALDO
                   END-IF

                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN

                   IF LIN > 56
                      PERFORM CABECALHO
                   END-IF
                END-IF
             END-READ
           END-PERFORM

           WRITE REG-RELAT FROM CAB03
           MOVE SPACES        TO LINDET-REL
           MOVE "TOTAL . . ." TO LINDET-REL
           MOVE TOTAL-SALDO   TO VALOR-E1
           MOVE VALOR-E1      TO LINDET-REL(57:13)
           WRITE REG-RELAT    FROM LINDET
           WRITE REG-RELAT    FROM CAB03
           MOVE SPACES        TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.
       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL
           MOVE DATA-MOVTO-W           TO DATA-MOV-REL
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE
           END-IF
           WRITE REG-RELAT FROM CAB02 AFTER 2
           WRITE REG-RELAT FROM CAB03
           WRITE REG-RELAT FROM CAB04
           WRITE REG-RELAT FROM CAB03
           MOVE 7 TO LIN.

       ACHAR-SEQUENCIA SECTION.
           MOVE DATA-MOVTO-I TO DATA-MOV-CX100
           MOVE ZEROS        TO SEQ-CX100

           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.

           MOVE 1            TO ULT-SEQUENCIA

           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD100
              NOT AT END
                   IF DATA-MOV-CX100 NOT = DATA-MOVTO-I
                      MOVE "10" TO ST-CXD100
                   ELSE
                      MOVE SEQ-CX100 TO ULT-SEQUENCIA
                   END-IF
              END-READ
           END-PERFORM

           PERFORM INCREMENTA-SEQUENCIA.

       INCREMENTA-SEQUENCIA SECTION.
           ADD 1              TO ULT-SEQUENCIA
           MOVE ULT-SEQUENCIA TO CXP100-SEQ.

       MOSTRA-ULT-SEQUENCIA SECTION.
           MOVE ULT-SEQUENCIA        TO CXP100-SEQ
           MOVE DATA-MOVTO-W         TO CXP100-DATA-MOV
           MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP100-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 CPD020 CPD021 CXD031 CXD100 CCD100 CCD101
                 COD050 CBD100 CPD023 OED020 CXD200 CPD022 CAD004
                 LOGCAIXA LOGCCD CAD018 CGD010 CRD020 CHD010
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

