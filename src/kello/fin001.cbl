       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIN001.
      *DATA: 13/08/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *RELATÓRIO: Posição Financeira
      *FUNÇÃO: Relaciona todos os saldos bancários, contas a pagar(si-
      *        tuação < 1 e não relaciona o tipo de fornecedor 03-em-
      *        préstimos terceiros com valor previsto), contas a rece-
      *        ber, e depósitos programados-cheques(local-10 = 1 e
      *        marca-del diferente de "*").
      *        O 1o.intervalo será data do dia até a sexta-feira(corren-
      *        te) as demais sábado à sexta.
      *        O pagto em atraso será acrescido o vlr multa + juros-mora
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX013.
           COPY CAPX018.
           COPY CGPX001.
           COPY CXPX040.
           COPY CXPX041.
           COPY CXPX100.
           COPY CPPX020.
           COPY CRPX020.
           COPY CHPX010.

           SELECT FIN001 ASSIGN       TO  PATH-FIN001
                         ORGANIZATION         INDEXED
                         ACCESS MODE          DYNAMIC
                         LOCK   MODE  IS    AUTOMATIC
                         WITH   LOCK  ON       RECORD
                         RECORD KEY   IS    FIN001-CH
                         STATUS       IS    ST-FIN001.

           SELECT FERIADOS ASSIGN       TO  PATH-FERIADOS
                           ORGANIZATION           INDEXED
                           ACCESS MODE            DYNAMIC
                           LOCK   MODE  IS      AUTOMATIC
                           WITH   LOCK  ON         RECORD
                           RECORD KEY   IS  FERIADOS-DATA
                           STATUS       IS    ST-FERIADOS.

           SELECT PORTPDOC ASSIGN       TO   PATH-PORTPDOC
                           ORGANIZATION            INDEXED
                           ACCESS MODE             DYNAMIC
                           LOCK   MODE  IS       AUTOMATIC
                           WITH   LOCK  ON          RECORD
                           RECORD KEY   IS  PORTPDOC-CHAVE
                           STATUS       IS     ST-PORTPDOC.


           SELECT AUXCPD   ASSIGN       TO     PATH-AUXCPD
                           ORGANIZATION            INDEXED
                           ACCESS MODE             DYNAMIC
                           LOCK   MODE  IS       AUTOMATIC
                           WITH   LOCK  ON          RECORD
                           RECORD KEY   IS    AUXCPD-CHAVE
                           FILE STATUS  IS       ST-AUXCPD.



           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.
      *                 "teste.txt"
      *                 organization is line sequential
      *                 access mode is sequential.
      *                 PRINTER.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW018.
       COPY CAPW013.
       COPY CGPW001.
       COPY CXPW040.
       COPY CXPW041.
       COPY CXPW100.
       COPY CPPW020.
       COPY CRPW020.
       COPY CHPW010.


      *FIN001-CHAVE = Contas a Receber
      *               Contas a Pagar
      *               Deposito Programado

       FD  FIN001.
       01  REG-FIN001.
           05 FIN001-CH.
              10 FIN001-TP                     PIC x(20).
              10 FIN001-PORTADOR               PIC X(04).

       FD FERIADOS.
       01 REG-FERIADOS.
          05 FERIADOS-DATA                     PIC 9(08).
          05 FILLER REDEFINES FERIADOS-DATA.
             10 FERIADOS-ANO                   PIC 9(04).
             10 FERIADOS-MES                   PIC 9(02).
             10 FERIADOS-DIA                   PIC 9(02).

      *>PORTPDOC-TIPO = 1 (DIA) 2 (VENCIDO) 3 (VENCER)
       FD PORTPDOC.
       01 REG-PORTPDOC.
          05 PORTPDOC-CHAVE.
             10 PORTPDOC-TIPO                  PIC 9(01).
             10 PORTPDOC-TIPODOC               PIC 9(01).
          05 PORTPDOC-VALOR1                   PIC 9(09)V99.
          05 PORTPDOC-VALOR2                   PIC 9(09)V99.
          05 PORTPDOC-VALOR3                   PIC 9(09)V99.
          05 PORTPDOC-VALOR4                   PIC 9(09)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER             PIC X(132).

       FD  AUXCPD.
       01  REG-AUXCPD.
           05 AUXCPD-CHAVE.
              10 AUXCPD-TIPO                    PIC 9(01).
              10 AUXCPD-VENCTO                  PIC 9(08).
              10 FILLER REDEFINES AUXCPD-VENCTO.
                 15 AUXCPD-ANO                  PIC 9(04).
                 15 AUXCPD-MES                  PIC 9(02).
                 15 AUXCPD-DIA                  PIC 9(02).
              10 AUXCPD-FORNECEDOR              PIC 9(06).
              10 AUXCPD-SEQUENCIA               PIC 9(06).
           05 AUXCPD-VALOR                      PIC 9(09)V99.
           05 AUXCPD-EMISSAO                    PIC 9(08).
           05 FILLER REDEFINES AUXCPD-EMISSAO.
              10 AUXCPD-ANO-EMI                 PIC 9(04).
              10 AUXCPD-MES-EMI                 PIC 9(02).
              10 AUXCPD-DIA-EMI                 PIC 9(02).
           05 AUXCPD-DESCRICAO                  PIC X(30).
           05 AUXCPD-PORTADOR                   PIC 9(04).
           05 AUXCPD-TP-FORNEC                  PIC 9(02).
           05 AUXCPD-RESPONSAVEL                PIC X(05).

       WORKING-STORAGE SECTION.
           COPY "FIN001.CPB".
           COPY "FIN001.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CXD040             PIC XX       VALUE SPACES.
           05  ST-CXD041             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CAD013             PIC XX       VALUE SPACES.
           05  ST-FIN001             PIC XX       VALUE SPACES.
           05  ST-FERIADOS           PIC XX       VALUE SPACES.
           05  ST-PORTPDOC           PIC XX       VALUE SPACES.
           05  ST-AUXCPD             PIC XX       VALUE SPACES.
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
           COPY "PARAMETR".
           05  MESANO-TAB OCCURS 3 TIMES PIC 9(6).
           05  MESANOWI.
               10  ANO-WI            PIC 9(4).
               10  MES-WI            PIC 9(2).
           05  MESANO-WI REDEFINES MESANOWI PIC 9(6).
           05  MESANOWW.
               10  MES-WW            PIC 9(2).
               10  ANO-WW            PIC 9(4).
           05  MESANO-WW REDEFINES MESANOWW PIC 9(6).
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZ9,99-.
           05  TOTAL-TIPO            PIC S9(09)V99 VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA              PIC 9(8)     VALUE ZEROS.
           05  DIAMES                PIC 9(4)     VALUE ZEROS.
           05  DIAMES-E              PIC 99/99.
           05  DATA1                 PIC 9(8)     VALUE ZEROS.
           05  DATA2                 PIC 9(8)     VALUE ZEROS.
           05  DATA3                 PIC 9(8)     VALUE ZEROS.
           05  DATA4                 PIC 9(8)     VALUE ZEROS.
           05  DATA5                 PIC 9(8)     VALUE ZEROS.
           05  DATA6                 PIC 9(8)     VALUE ZEROS.
           05  DATA7                 PIC 9(8)     VALUE ZEROS.
           05  DATA8                 PIC 9(8)     VALUE ZEROS.
           05  VENCTO-W              PIC 9(8)     VALUE ZEROS.
      *   VARIAVEL P/ TRANSFORMAR VENCTO DO CTA A RECEBER 8 DIGITOS
           05  MESANO-SALDO-ANT      PIC 9(6)     VALUE ZEROS.
      *  MES/ANO LIMITE PARA CALCULA O SALDO ANTERIOR
           05  MESANO-CORRENTE       PIC 9(6)     VALUE ZEROS.
      *  MES/ANO CORRENTE - LERÁ O SALDO A PARTIR DO ARQUIVO CXD100
      *      ENQUANTO OS DEMAIS DO CXD040(ARQUIVO DE SALDO)
           05  SALDO-CAIXA           PIC S9(8)V99 VALUE ZEROS.
           05  DEPOSITO-DIA          PIC 9(8)V99  VALUE ZEROS.
           05  PAGTO-ATRASO-PREV     PIC S9(8)V99  VALUE ZEROS.
           05  PAGTO-ATRASO-DEF      PIC S9(8)V99  VALUE ZEROS.
           05  PAGTO-DIA-PREV        PIC S9(8)V99  VALUE ZEROS.
           05  PAGTO-DIA-DEF         PIC S9(8)V99  VALUE ZEROS.
           05  CHEQUE-1SEMANA        PIC 9(8)V99  VALUE ZEROS.
           05  PAGAR-PREV-1SEMANA    PIC S9(8)V99  VALUE ZEROS.
           05  PAGAR-DEF-1SEMANA     PIC S9(8)V99  VALUE ZEROS.
           05  RECEBER-1SEMANA       PIC 9(09)V99 VALUE ZEROS.
           05  SALDO-1SEMANA         PIC S9(8)V99 VALUE ZEROS.
           05  CHEQUE-2SEMANA        PIC 9(8)V99  VALUE ZEROS.
           05  PAGAR-PREV-2SEMANA    PIC S9(8)V99  VALUE ZEROS.
           05  PAGAR-DEF-2SEMANA     PIC S9(8)V99  VALUE ZEROS.
           05  RECEBER-2SEMANA       PIC 9(09)V99 VALUE ZEROS.
           05  SALDO-2SEMANA         PIC S9(8)V99 VALUE ZEROS.
           05  CHEQUE-3SEMANA        PIC 9(8)V99  VALUE ZEROS.
           05  PAGAR-PREV-3SEMANA    PIC S9(8)V99  VALUE ZEROS.
           05  PAGAR-DEF-3SEMANA     PIC S9(8)V99  VALUE ZEROS.
           05  RECEBER-3SEMANA       PIC 9(09)V99 VALUE ZEROS.
           05  SALDO-3SEMANA         PIC S9(8)V99 VALUE ZEROS.
           05  CHEQUE-4SEMANA        PIC 9(8)V99  VALUE ZEROS.
           05  PAGAR-PREV-4SEMANA    PIC S9(8)V99  VALUE ZEROS.
           05  PAGAR-DEF-4SEMANA     PIC S9(8)V99  VALUE ZEROS.
           05  RECEBER-4SEMANA       PIC 9(09)V99 VALUE ZEROS.
           05  SALDO-4SEMANA         PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-BANCO           PIC S9(8)V99 VALUE ZEROS.
           05  CODIGO-BANCO          PIC 9(6)     VALUE ZEROS.
           05  VALOR-JUROS           PIC 9(6)     VALUE ZEROS.
           05  SALDO-ACUM            PIC S9(8)V99 VALUE ZEROS.
           05  WSITEM                PIC 9(009)   COMP-5 VALUE ZEROS.
           05  AUX-PORT              PIC 9(04)    VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  BANCOSW OCCURS 40 TIMES.
               10  BANCO-W1          PIC 9(6).
               10  VALOR-W1          PIC S9(8)V99.
           05  UMITEM                OBJECT REFERENCE VALUE NULL.
           05  UMOBJETO              OBJECT REFERENCE VALUE NULL.
           05  AUX-CODIGO            PIC 9(04)    VALUE ZEROS.
           05  ACHEI                 PIC X(01)    VALUE SPACES.
           05  COMP-DATA             PIC X(10)    VALUE SPACES.
           05  AUX-VENCTO            PIC 9(08)    VALUE ZEROS.
           05  AUX-TOTAL             PIC 9(09)V99 VALUE ZEROS.
           05  TOTAL-GERAL           PIC 9(09)V99 VALUE ZEROS.

       01  CAB01.
           05  EMPRESA-REL         PIC X(73)  VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(40)   VALUE
           "POSICAO FINANCEIRA      ".
           05  FILLER              PIC X(04)   VALUE "HR: ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(7)    VALUE SPACES.
           05  FILLER              PIC X(14)   VALUE "DATA EMISSAO: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  LINDET.
           05  FILLER              PIC X(80)   VALUE SPACES.

       01 CAB-LINDET.
          05 FILLER                PIC X(11) VALUE "VENCIMENTO".
          05 FILLER                PIC X(31) VALUE "FORNECEDOR".
          05 FILLER                PIC X(31) VALUE "DESCRIÇÃO".
          05 FILLER                PIC X(14) VALUE "        VALOR".
          05 FILLER                PIC X(05) VALUE "PORT".
          05 FILLER                PIC X(03) VALUE "TP".
          05 FILLER                PIC X(06) VALUE "RESP".
          05 FILLER                PIC X(05) VALUE "SEQUE".

       01 DET-01.
          05 DET-DIA-VEN           PIC 9(02)/ BLANK WHEN ZEROS.
          05 DET-MES-VEN           PIC 9(02)/ BLANK WHEN ZEROS.
          05 DET-ANO-VEN           PIC 9(04)  BLANK WHEN ZEROS.
          05 FILLER                PIC X(01).
          05 DET-FORNECEDOR        PIC X(30).
          05 FILLER                PIC X(01).
          05 DET-DESCRICAO         PIC X(30).
          05 FILLER                PIC X(01).
          05 DET-VALOR             PIC ZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                PIC X(01).
          05 DET-PORTADOR          PIC 9(04) BLANK WHEN ZEROS.
          05 FILLER                PIC X(01).
          05 DET-TIPO              PIC 9(02) BLANK WHEN ZEROS.
          05 FILLER                PIC X(01).
          05 DET-RESPONSAVEL       PIC X(05).
          05 FILLER                PIC X(01).
          05 DET-SEQUENCIA         PIC 9(05) BLANK WHEN ZEROS.

           COPY IMPRESSORA.

       01 indice                   pic 9(02)   value zeros.
       01 wsTexto                  pic x(255)  value spaces.
       01 wssize                   pic 9(09)   comp-5 value zeros.
       01 wsIndice                 pic 9(09)   comp-5 value zeros.

       01 lnktabela.
          02 lnkobjetoscol     object reference occurs 99 times.

       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabela2.
          02 lnkobjetoscol2    object reference occurs 99 times.

       01 lnktabelaCol2.
          02 lnkcolunas2   pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabela3.
          02 lnkobjetoscol3    object reference occurs 99 times.

       01 lnktabelaCol3.
          02 lnkcolunas3   pic 9(09) comp-5 value zeros occurs 99 times.



       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 WS-HORA-SYS         PIC 9(08) VALUE ZEROS.

       01 lnkusu.
          copy usuario.cpy.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL FIN001-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE DATA-INV(5: 4) TO MESANO-CORRENTE(1: 4).
           MOVE DATA-INV(3: 2) TO MESANO-CORRENTE(5: 2).

           MOVE ZEROS TO ERRO-W.
           INITIALIZE FIN001-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE FIN001-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE FIN001-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CXD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD040.
           MOVE "CXD041"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD041.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CAD013"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD013.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CHD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "FIN001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-FIN001.
           MOVE "FERIAD"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-FERIADOS
           MOVE "AUXCPD"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-AUXCPD
           OPEN I-O   FIN001 FERIADOS
           CLOSE      FIN001 FERIADOS
           OPEN INPUT FIN001
           OPEN I-O   FERIADOS

           ACCEPT WS-HORA-SYS FROM TIME

           MOVE WS-HORA-SYS TO PATH-PORTPDOC

           OPEN INPUT CHD010 CXD040 CXD041 CXD100 CRD020 CAD018
                      CPD020 CAD013 CGD001.
           IF ST-FERIADOS <> "00"
              MOVE "ERRO ABERTURA FERIAD: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-FERIADOS TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-CAD018 TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-FIN001 <> "00"
              MOVE "ERRO ABERTURA FIN001: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-FIN001 TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD040 <> "00"
              MOVE "ERRO ABERTURA CXD040: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-CXD040 TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD041 <> "00"
              MOVE "ERRO ABERTURA CXD041: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-CXD041 TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD040 <> "00"
              MOVE "ERRO ABERTURA CXD040: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-CXD040 TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-CXD100 TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-CRD020 TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-CPD020 TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD013 <> "00"
              MOVE "ERRO ABERTURA CAD013: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-CAD013 TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO FIN001-MENSAGEM-ERRO
              MOVE ST-CHD010 TO FIN001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO FIN001-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE DATA-MOVTO-W TO FIN001-ACP-DATA

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN FIN001-CENTRALIZA-TRUE
                    PERFORM CRIAR-LISTVIEW
                    PERFORM CRIAR-LISTVIEW2
                    PERFORM CRIAR-LISTVIEW3
                    PERFORM CENTRALIZAR
               WHEN FIN001-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN FIN001-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN FIN001-LER-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN FIN001-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN FIN001-SALVAR-TRUE
                    PERFORM SALVAR
               WHEN FIN001-SALVAR2-TRUE
                    PERFORM SALVAR2
               WHEN FIN001-CARREGAR-CONFIG-TRUE
                    PERFORM CARREGAR-CONFIG
               WHEN FIN001-PREENCHER-TIPO-TRUE
                    PERFORM PREENCHER-TIPO
               WHEN FIN001-CARREGAR-CONFIG2-TRUE
                    PERFORM CARREGAR-CONFIG2
               WHEN FIN001-EVENTO-FLG-TRUE
                    PERFORM EVENTO-PORTADOR
               WHEN FIN001-EVENTO-FLG2-TRUE
                    PERFORM EVENTO-TIPODOC
               WHEN FIN001-EVENTO-FLG3-TRUE
                    PERFORM EVENTO-FERIADO
               WHEN FIN001-CARREGA-FERIADOS-TRUE
                    PERFORM CARREGAR-FERIADOS
               WHEN FIN001-INSERIR-FERIADO-TRUE
                    PERFORM INSERIR-FERIADO
               WHEN FIN001-CARREGAR-CPD020-TRUE
                    PERFORM CARREGAR-AUXCPD
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGAR-AUXCPD SECTION.
           OPEN INPUT AUXCPD

           MOVE "APAGAR-LB3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB-LINDET             TO FIN001-CAB-LINHA

           INITIALIZE REG-AUXCPD
                      AUX-VENCTO
                      AUX-TOTAL
                      TOTAL-GERAL

           MOVE FIN001-QUAL-TIPO       TO AUXCPD-TIPO
           START AUXCPD KEY IS NOT LESS AUXCPD-CHAVE INVALID KEY
                 MOVE "10" TO ST-AUXCPD.

           PERFORM UNTIL ST-AUXCPD = "10"
                 READ AUXCPD NEXT AT END
                      MOVE "10" TO ST-AUXCPD
                 NOT AT END
                      IF FIN001-QUAL-TIPO <> AUXCPD-TIPO
                         MOVE "10" TO ST-AUXCPD
                      ELSE
                         IF AUX-VENCTO <> AUXCPD-VENCTO
                            IF AUX-TOTAL > 0
                               INITIALIZE DET-01
                               MOVE "TOTAL PARCIAL"  TO DET-DESCRICAO
                               MOVE AUX-TOTAL        TO DET-VALOR
                               MOVE DET-01           TO FIN001-LINDET
                               MOVE "INSERIR-LINHA3" TO DS-PROCEDURE
                               PERFORM CALL-DIALOG-SYSTEM

                               MOVE SPACES           TO FIN001-LINDET
                               MOVE "INSERIR-LINHA3" TO DS-PROCEDURE
                               PERFORM CALL-DIALOG-SYSTEM
                               ADD  AUX-TOTAL        TO TOTAL-GERAL
                               INITIALIZE AUX-TOTAL
                            END-IF
                            MOVE AUXCPD-VENCTO       TO AUX-VENCTO
                         END-IF

                         ADD AUXCPD-VALOR        TO AUX-TOTAL

                         MOVE AUXCPD-DIA         TO DET-DIA-VEN
                         MOVE AUXCPD-MES         TO DET-MES-VEN
                         MOVE AUXCPD-ANO         TO DET-ANO-VEN
                         MOVE AUXCPD-FORNECEDOR  TO CODIGO-CG01
                         READ CGD001 INVALID KEY
                              MOVE "*****"       TO NOME-CG01
                         END-READ
                         MOVE SPACES             TO DET-FORNECEDOR
                         STRING AUXCPD-FORNECEDOR "-" NOME-CG01
                           INTO DET-FORNECEDOR
                         MOVE AUXCPD-DESCRICAO   TO DET-DESCRICAO
                         MOVE AUXCPD-VALOR       TO DET-VALOR
                         MOVE AUXCPD-PORTADOR    TO DET-PORTADOR
                         MOVE AUXCPD-TP-FORNEC   TO DET-TIPO
                         MOVE AUXCPD-RESPONSAVEL TO DET-RESPONSAVEL
                         MOVE AUXCPD-SEQUENCIA   TO DET-SEQUENCIA

                          MOVE DET-01 TO FIN001-LINDET
                          MOVE "INSERIR-LINHA3" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM

           INITIALIZE DET-01
           MOVE "TOTAL PARCIAL"  TO DET-DESCRICAO
           MOVE AUX-TOTAL        TO DET-VALOR
           MOVE AUXCPD-VENCTO    TO AUX-VENCTO
           ADD  AUX-TOTAL        TO TOTAL-GERAL
           MOVE DET-01           TO FIN001-LINDET
           MOVE "INSERIR-LINHA3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES           TO FIN001-LINDET
           MOVE "INSERIR-LINHA3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE DET-01
           MOVE "TOTAL GERAL"    TO DET-DESCRICAO
           MOVE TOTAL-GERAL      TO DET-VALOR
           MOVE DET-01           TO FIN001-LINDET
           MOVE "INSERIR-LINHA3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           CLOSE AUXCPD.

       EVENTO-FERIADO SECTION.
           EVALUATE FIN001-EVENTOS
               WHEN 34027 SET-FOCUS EF-FERIADO
               WHEN 34046 PERFORM EXCLUIR-FERIADO
               WHEN 34013 PERFORM EXCLUIR-FERIADO
               WHEN 34592 PERFORM EXCLUIR-FERIADO
               WHEN 34123 PERFORM CHAMAR-COLUNAS-FAVO3.

       EXCLUIR-FERIADO SECTION.
           INITIALIZE WSITEM
           INVOKE FIN001-LISTVIEW-FERIADO "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM
           IF UMITEM NOT EQUAL NULL
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING LNKCOLUNAS3(1) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE FUNCTION NUMVAL(WSTEXTO(1:2)) TO FERIADOS-DIA
              MOVE FUNCTION NUMVAL(WSTEXTO(4:2)) TO FERIADOS-MES
              MOVE FUNCTION NUMVAL(WSTEXTO(7:4)) TO FERIADOS-ANO

              READ FERIADOS INVALID KEY
                   MOVE "Data Não Encontrada para Exclusão" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   DELETE FERIADOS INVALID KEY
                          MOVE "Erro de Exclusão...FERIADOS" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                   NOT INVALID KEY
                          INVOKE UMITEM "Finalize" RETURNING UMITEM.

       CARREGAR-FERIADOS SECTION.
           INVOKE FIN001-LISTVIEW-FERIADO "DeleteAll"

           INITIALIZE REG-FERIADOS
           START FERIADOS KEY IS NOT LESS FERIADOS-DATA INVALID KEY
                 MOVE "10" TO ST-FERIADOS.

           PERFORM UNTIL ST-FERIADOS = "10"
                 READ FERIADOS NEXT AT END
                      MOVE "10" TO ST-FERIADOS
                 NOT AT END
                      INITIALIZE INDICE
                      INVOKE FIN001-LISTVIEW-FERIADO "ADICIONARITEM"
                                                     RETURNING WSITEM

                      ADD 1 TO INDICE
                      INITIALIZE WSTEXTO
                      STRING FERIADOS-DIA "/"
                             FERIADOS-MES "/"
                             FERIADOS-ANO X"00" INTO WSTEXTO
                      INVOKE FIN001-LISTVIEW-FERIADO "PREENCHERCOLUNAZ"
                       USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO
                 END-READ
           END-PERFORM.

       INSERIR-FERIADO SECTION.
           MOVE FIN001-ACP-FERIADO(1:2) TO FERIADOS-DIA
           MOVE FIN001-ACP-FERIADO(3:2) TO FERIADOS-MES
           MOVE FIN001-ACP-FERIADO(5:4) TO FERIADOS-ANO

           READ FERIADOS INVALID KEY
                WRITE REG-FERIADOS INVALID KEY
                      MOVE "Erro de Gravação...FERIADOS" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                NOT INVALID KEY
                      INITIALIZE INDICE
                      INVOKE FIN001-LISTVIEW-FERIADO "ADICIONARITEM"
                                                    RETURNING WSITEM

                      ADD 1 TO INDICE
                      INITIALIZE WSTEXTO
                      STRING FIN001-ACP-FERIADO(1:2) "/"
                             FIN001-ACP-FERIADO(3:2) "/"
                             FIN001-ACP-FERIADO(5:4) X"00"
                        INTO WSTEXTO
                      INVOKE FIN001-LISTVIEW-FERIADO
                                                  "PREENCHERCOLUNAZ"
                       USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO
                END-WRITE
           NOT INVALID KEY
                MOVE "Data já Informada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.


       EVENTO-PORTADOR SECTION.
           EVALUATE FIN001-ACP-EVENTO
               WHEN 34013 PERFORM EXCLUIR-PORTADOR
               WHEN 34592 PERFORM EXCLUIR-PORTADOR
               WHEN 34046 PERFORM EXCLUIR-PORTADOR
               WHEN 34026 SET-FOCUS SB-TIPO
               WHEN 34123 PERFORM CHAMAR-COLUNAS-FAVO
           END-EVALUATE.

       EXCLUIR-PORTADOR SECTION.
           INITIALIZE WSITEM
           INVOKE FIN001-LISTVIEW-PORTADOR "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM
           IF UMITEM NOT EQUAL NULL
              INVOKE UMITEM "Finalize" RETURNING UMITEM.

       EVENTO-TIPODOC SECTION.
           EVALUATE FIN001-ACP-EVENTO
               WHEN 34013 PERFORM EXCLUIR-TIPODOC
               WHEN 34592 PERFORM EXCLUIR-TIPODOC
               WHEN 34046 PERFORM EXCLUIR-TIPODOC
               WHEN 34026 SET-FOCUS SB-TIPO-DOCTO
               WHEN 34123 PERFORM CHAMAR-COLUNAS-FAVO2
           END-EVALUATE.

       EXCLUIR-TIPODOC SECTION.
           INITIALIZE WSITEM
           INVOKE FIN001-LISTVIEW-TIPODOC "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM
           IF UMITEM NOT EQUAL NULL
              INVOKE UMITEM "Finalize" RETURNING UMITEM.

       PREENCHER-TIPO SECTION.
           PERFORM VERIFICAR-INFORMADO2
           IF ACHEI = "N"
              INITIALIZE INDICE
              INVOKE FIN001-LISTVIEW-TIPODOC "ADICIONARITEM"
                                             RETURNING WSITEM

              ADD 1 TO INDICE
              INITIALIZE WSTEXTO
              STRING FIN001-TIPO-DOCTO(1:1) X"00"
                INTO WSTEXTO
              INVOKE FIN001-LISTVIEW-TIPODOC "PREENCHERCOLUNAZ"
               USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO


              ADD 1 TO INDICE
              INITIALIZE WSTEXTO
              STRING FIN001-TIPO-DOCTO(3:15) X"00"
                INTO WSTEXTO
              INVOKE FIN001-LISTVIEW-TIPODOC "PREENCHERCOLUNAZ"
               USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO
           ELSE
               MOVE "Tipo de Documento já Informado" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           END-IF.

       CRIAR-LISTVIEW2 SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke fin001-listview-tipodoc "adicionarColunaZ"
            using z"Codigo" returning lnkobjetoscol2(indice)
          move indice to lnkcolunas2(indice)

      *>---
      *>---
          add 1 to indice
          invoke fin001-listview-tipodoc "adicionarColunaZ"
            using z"Tipo Documento" returning lnkobjetoscol2(indice)
          move indice to lnkcolunas2(indice)

          perform mostrar-fonte-favo2
          perform mostrar-colunas-favo2

          invoke fin001-listview-tipodoc "gridLines"
          invoke fin001-listview-tipodoc "noBorder".

       mostrar-colunas-favo2 section.
          initialize wsTexto
          move "listview-fin001-b" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  fin001-listview-tipodoc
                                  wsTexto
                                  lnktabela2.
       mostrar-colunas-favo2-fim.
           exit.

       mostrar-fonte-favo2 section.
           move "listview-fin001-b" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu fin001-listview-tipodoc wsTexto.
       mostrar-fonte-favo2-fim.
           exit.

       exportar-para-excel2 section.
           invoke aListview "ExportarParaOExcel"
                    using fin001-listview-tipodoc lnkTabela2.
       exportar-para-excel2-fim.
           EXIT.

       zebrar-itens2 section.
           move "listview-fin001-b" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu fin001-listview-tipodoc wsTexto
           invoke fin001-listview-tipodoc "redrawallitems".
       zebrar-itens2-fim.
           exit.

       chamar-colunas-favo2 section.
           move "listview-fin001-b" to wsTexto
           call "COLFAV" using lnkusu
                               fin001-listview-tipodoc
                               wsTexto
                               lnktabela2

           perform mostrar-colunas-favo2
           perform mostrar-fonte-favo2
           perform zebrar-itens2.
       chamar-colunas-favo2-fim.
           exit.

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke fin001-listview-portador "adicionarColunaZ"
            using z"Codigo" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke fin001-listview-portador "adicionarColunaZ"
            using z"Portador" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke fin001-listview-portador "gridLines"
          invoke fin001-listview-portador "noBorder".

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-fin001" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  fin001-listview-portador
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.

       mostrar-fonte-favo section.
           move "listview-fin001" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu fin001-listview-portador wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       exportar-para-excel section.
           invoke aListview "ExportarParaOExcel"
                    using fin001-listview-portador lnkTabela.
       exportar-para-excel-fim.
           EXIT.

       zebrar-itens section.
           move "listview-fin001" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu fin001-listview-portador wsTexto
           invoke fin001-listview-portador "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-fin001" to wsTexto
           call "COLFAV" using lnkusu
                               fin001-listview-portador
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.

       CRIAR-LISTVIEW3 SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke fin001-listview-feriado "adicionarColunaZ"
            using z"Data" returning lnkobjetoscol3(indice)
          move indice to lnkcolunas3(indice)

          perform mostrar-fonte-favo3
          perform mostrar-colunas-favo3

          invoke fin001-listview-feriado "gridLines"
          invoke fin001-listview-feriado "noBorder".

       mostrar-colunas-favo3 section.
          initialize wsTexto
          move "listview-fin001-c" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  fin001-listview-feriado
                                  wsTexto
                                  lnktabela3.
       mostrar-colunas-favo3-fim.
           exit.

       mostrar-fonte-favo3 section.
           move "listview-fin001-c" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu fin001-listview-feriado wsTexto.
       mostrar-fonte-favo3-fim.
           exit.

       exportar-para-excel3 section.
           invoke aListview "ExportarParaOExcel"
                    using fin001-listview-feriado lnkTabela3.
       exportar-para-excel3-fim.
           EXIT.

       zebrar-itens3 section.
           move "listview-fin001-c" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu fin001-listview-feriado wsTexto
           invoke fin001-listview-feriado "redrawallitems".
       zebrar-itens3-fim.
           exit.

       chamar-colunas-favo3 section.
           move "listview-fin001-c" to wsTexto
           call "COLFAV" using lnkusu
                               fin001-listview-feriado
                               wsTexto
                               lnktabela3

           perform mostrar-colunas-favo3
           perform mostrar-fonte-favo3
           perform zebrar-itens3.
       chamar-colunas-favo3-fim.
           exit.

       CENTRALIZAR SECTION.
          move-object-handle win-feriado handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win5 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


          move-object-handle win3 handle8
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

       LER-PORTADOR SECTION.
           MOVE FIN001-PORTADOR-DEPOSITO TO AUX-PORT
           PERFORM LER-PORT
           MOVE NOME-PORT TO FIN001-DESC-PORTADOR-DEP
           IF NOME-PORT <> "*********"
              PERFORM VERIFICAR-INFORMADO
              IF ACHEI = "N"
                 INITIALIZE INDICE
                 INVOKE FIN001-LISTVIEW-PORTADOR "ADICIONARITEM"
                                                RETURNING WSITEM

                 ADD 1 TO INDICE
                 INITIALIZE WSTEXTO
                 STRING FIN001-PORTADOR-DEPOSITO X"00"
                   INTO WSTEXTO
                 INVOKE FIN001-LISTVIEW-PORTADOR
                                              "PREENCHERCOLUNAZ"
                  USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO


                 ADD 1 TO INDICE
                 INITIALIZE WSTEXTO
                 STRING FIN001-DESC-PORTADOR-DEP X"00"
                   DELIMITED BY "   " INTO WSTEXTO
                 INVOKE FIN001-LISTVIEW-PORTADOR
                                              "PREENCHERCOLUNAZ"
                  USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
              ELSE
                  MOVE "Portador já Informado" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
              END-IF
           ELSE
              MOVE "Portador Inválido" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       VERIFICAR-INFORMADO SECTION.
           MOVE "N"                TO ACHEI

           INITIALIZE WSINDICE
           INVOKE FIN001-LISTVIEW-PORTADOR "SIZE" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
                 ADD 1 TO WSINDICE
                 INVOKE FIN001-LISTVIEW-PORTADOR "ITEMATINDEX"
                        USING WSINDICE RETURNING UMITEM
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(1) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 MOVE FUNCTION NUMVAL(WSTEXTO) TO AUX-CODIGO

                 IF AUX-CODIGO = FIN001-PORTADOR-DEPOSITO
                    MOVE "S" TO ACHEI
                    EXIT PERFORM
                 END-IF
           END-PERFORM.

       VERIFICAR-INFORMADO2 SECTION.
           MOVE "N"                TO ACHEI

           INITIALIZE WSINDICE
           INVOKE FIN001-LISTVIEW-TIPODOC "SIZE" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
                 ADD 1 TO WSINDICE
                 INVOKE FIN001-LISTVIEW-TIPODOC "ITEMATINDEX"
                        USING WSINDICE RETURNING UMITEM
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS2(1) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

                 IF WSTEXTO(1:1) = FIN001-TIPO-DOCTO(1:1)
                    MOVE "S" TO ACHEI
                    EXIT PERFORM
                 END-IF
           END-PERFORM.

       LER-PORT SECTION.
           IF AUX-PORT = 9999
              MOVE "TODOS"            TO NOME-PORT
           ELSE
              MOVE AUX-PORT           TO PORTADOR
              READ CAD018 INVALID KEY
                   MOVE "*********"   TO NOME-PORT.

       POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(33: 4)   TO FIN001-PORTADOR-DEPOSITO

           MOVE FIN001-PORTADOR-DEPOSITO TO AUX-PORT
           PERFORM LER-PORT
           MOVE NOME-PORT TO FIN001-DESC-PORTADOR-DEP.

       SALVAR SECTION.
           CLOSE      FIN001
           OPEN I-O   FIN001

           INITIALIZE REG-FIN001
           MOVE FIN001-TIPO                   TO FIN001-TP
           START FIN001 KEY IS NOT LESS FIN001-CH INVALID KEY
                 MOVE "10" TO ST-FIN001.
           PERFORM UNTIL ST-FIN001 = "10"
                 READ FIN001 NEXT AT END
                      MOVE "10" TO ST-FIN001
                 NOT AT END
                      IF FIN001-TP <> FIN001-TIPO
                         MOVE "10" TO ST-FIN001
                      ELSE
                         DELETE FIN001 INVALID KEY
                             MOVE "Erro de Exclusão...FIN001" TO
                             MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                         END-DELETE
                      END-IF
                 END-READ
           END-PERFORM

           INITIALIZE WSINDICE
           INVOKE FIN001-LISTVIEW-PORTADOR "SIZE" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
                 ADD 1 TO WSINDICE
                 INVOKE FIN001-LISTVIEW-PORTADOR "ITEMATINDEX"
                        USING WSINDICE RETURNING UMITEM
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS(1) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO


                 MOVE FIN001-TIPO              TO FIN001-TP
                 MOVE WSTEXTO                  TO FIN001-PORTADOR
                 WRITE REG-FIN001 INVALID KEY
                       MOVE "Erro de Gravação...FIN001" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                 END-WRITE
           END-PERFORM


           CLOSE      FIN001
           OPEN INPUT FIN001.

       SALVAR2 SECTION.
           CLOSE      FIN001
           OPEN I-O   FIN001

           INITIALIZE REG-FIN001
           MOVE "Tipo Documento"              TO FIN001-TP
           START FIN001 KEY IS NOT LESS FIN001-CH INVALID KEY
                 MOVE "10" TO ST-FIN001.
           PERFORM UNTIL ST-FIN001 = "10"
                 READ FIN001 NEXT AT END
                      MOVE "10" TO ST-FIN001
                 NOT AT END
                      IF FIN001-TP <> "Tipo Documento"
                         MOVE "10" TO ST-FIN001
                      ELSE
                         DELETE FIN001 INVALID KEY
                             MOVE "Erro de Exclusão...FIN001" TO
                             MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                         END-DELETE
                      END-IF
                 END-READ
           END-PERFORM

           INITIALIZE WSINDICE
           INVOKE FIN001-LISTVIEW-TIPODOC "SIZE" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
                 ADD 1 TO WSINDICE
                 INVOKE FIN001-LISTVIEW-TIPODOC "ITEMATINDEX"
                        USING WSINDICE RETURNING UMITEM
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING LNKCOLUNAS2(1) RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO


                 MOVE "Tipo Documento"         TO FIN001-TP
                 MOVE WSTEXTO                  TO FIN001-PORTADOR
                 WRITE REG-FIN001 INVALID KEY
                       MOVE "Erro de Gravação...FIN001" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                 END-WRITE
           END-PERFORM


           CLOSE      FIN001
           OPEN INPUT FIN001.

       CARREGAR-CONFIG SECTION.
           INVOKE FIN001-LISTVIEW-PORTADOR "DeleteAll"

           INITIALIZE REG-FIN001
           MOVE FIN001-TIPO            TO FIN001-TP
           START FIN001 KEY IS NOT LESS FIN001-CH INVALID KEY
                 MOVE "10" TO ST-FIN001.

           PERFORM UNTIL ST-FIN001 = "10"
                 READ FIN001 NEXT AT END
                      MOVE "10" TO ST-FIN001
                 NOT AT END
                      IF FIN001-TIPO <> FIN001-TP
                         MOVE "10" TO ST-FIN001
                      ELSE
                         INITIALIZE INDICE
                         INVOKE FIN001-LISTVIEW-PORTADOR "ADICIONARITEM"
                                                        RETURNING WSITEM

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING FIN001-PORTADOR X"00" INTO WSTEXTO
                         INVOKE FIN001-LISTVIEW-PORTADOR
                                                      "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO


                         IF FIN001-PORTADOR <> 9999
                            MOVE FIN001-PORTADOR  TO PORTADOR
                            READ CAD018 INVALID KEY
                                 MOVE "*********" TO NOME-PORT
                            END-READ
                         ELSE
                            MOVE "TODOS" TO NOME-PORT
                         END-IF

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING NOME-PORT X"00" DELIMITED BY "   "
                           INTO WSTEXTO
                         INVOKE FIN001-LISTVIEW-PORTADOR
                                                      "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
                      END-IF
                 END-READ
           END-PERFORM.

       CARREGAR-CONFIG2 SECTION.
           INVOKE FIN001-LISTVIEW-TIPODOC "DeleteAll"

           INITIALIZE REG-FIN001
           MOVE "Tipo Documento"     TO FIN001-TP
           START FIN001 KEY IS NOT LESS FIN001-CH INVALID KEY
                 MOVE "10" TO ST-FIN001.

           PERFORM UNTIL ST-FIN001 = "10"
                 READ FIN001 NEXT AT END
                      MOVE "10" TO ST-FIN001
                 NOT AT END
                      IF "Tipo Documento" <> FIN001-TP
                         MOVE "10" TO ST-FIN001
                      ELSE
                         INITIALIZE INDICE
                         INVOKE FIN001-LISTVIEW-TIPODOC "ADICIONARITEM"
                                                        RETURNING WSITEM

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING FIN001-PORTADOR(1:1) X"00" INTO WSTEXTO
                         INVOKE FIN001-LISTVIEW-TIPODOC
                                                      "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO


                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         EVALUATE FIN001-PORTADOR
                             WHEN "0" STRING "Boleto" X"00" INTO WSTEXTO
                             WHEN "1" STRING "Dupl/Promis" X"00"
                                      INTO WSTEXTO
                             WHEN "2" STRING "Org.Evento" X"00"
                                      INTO WSTEXTO
                             WHEN "3" STRING "Debito Automatico" X"00"
                                      INTO WSTEXTO
                             WHEN "4" STRING "Cartao de Credito" X"00"
                                      INTO WSTEXTO
                         END-EVALUATE
                         INVOKE FIN001-LISTVIEW-TIPODOC
                                                      "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO
                      END-IF
                 END-READ
           END-PERFORM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE FIN001-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       ENCONTRA-DATA-INTERVALO SECTION.
      *A PEDIDO DO ANDERSON O USUARIO INFORMA A DATA DESEJADA
           MOVE FIN001-ACP-DATA       TO DATA-MOVTO-W
           MOVE FIN001-ACP-DATA(5: 4) TO MESANO-CORRENTE(1: 4).
           MOVE FIN001-ACP-DATA(3: 2) TO MESANO-CORRENTE(5: 2).
      ******************************************************************

      *>Inverte a data que esta DDMMAAAA P/ AAAAMMDD********************
           MOVE DATA-MOVTO-W            TO GRTIME-DATE.
           MOVE 1                       TO GRTIME-TYPE.
           MOVE 4                       TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           MOVE GRTIME-DATE-FINAL       TO DATA-DIA.
      ******************************************************************


      *>Soma 1 dia a data de Hoje***************************************
           MOVE DATA-MOVTO-W            TO GRTIME-DATE
           MOVE 1                       TO GRTIME-TYPE
                                           GRTIME-DAYS
           MOVE 1                       TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           MOVE GRTIME-DATE-FINAL(1: 4) TO DIAMES FIN001-DATA1.
           MOVE GRTIME-DATE-FINAL       TO GRTIME-DATE.
      *>Inverte a data somada anteriormente*****************************
           MOVE 4                       TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           MOVE GRTIME-DATE-FINAL       TO DATA1 GRTIME-DATE.
      ******************************************************************


           MOVE 2                 TO GRTIME-TYPE.
           MOVE 8                 TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
      *    Se a data for sábado, a 1o.semana não será a corrente
           EVALUATE GRTIME-WEEK-NUM
             WHEN 0 MOVE 6 TO GRTIME-DAYS
             WHEN 1 MOVE 5 TO GRTIME-DAYS
             WHEN 2 MOVE 4 TO GRTIME-DAYS
             WHEN 3 MOVE 3 TO GRTIME-DAYS
             WHEN 4 MOVE 2 TO GRTIME-DAYS
             WHEN 5 MOVE 1 TO GRTIME-DAYS
             WHEN 6 MOVE 0 TO GRTIME-DAYS
           END-EVALUATE.
           MOVE 1                  TO GRTIME-FUNCTION.
           PERFORM ADICIONA-INVERTE-DATA.
           MOVE GRTIME-DATE-FINAL  TO DATA2.

           MOVE DIAMES             TO FIN001-DATA2.
           MOVE 1                  TO GRTIME-DAYS.
           PERFORM ADICIONA-INVERTE-DATA.
           MOVE GRTIME-DATE-FINAL  TO DATA3.

           MOVE DIAMES             TO FIN001-DATA3.
           MOVE 6                  TO GRTIME-DAYS.
           PERFORM ADICIONA-INVERTE-DATA.
           MOVE GRTIME-DATE-FINAL  TO DATA4.

           MOVE DIAMES             TO FIN001-DATA4.
           MOVE 1                  TO GRTIME-DAYS.
           PERFORM ADICIONA-INVERTE-DATA.

           MOVE GRTIME-DATE-FINAL  TO DATA5.
           MOVE DIAMES             TO FIN001-DATA5.

           MOVE 6                  TO GRTIME-DAYS.
           PERFORM ADICIONA-INVERTE-DATA.
           MOVE GRTIME-DATE-FINAL  TO DATA6.
           MOVE DIAMES             TO FIN001-DATA6.


           MOVE 1                  TO GRTIME-DAYS.
           PERFORM ADICIONA-INVERTE-DATA.
           MOVE GRTIME-DATE-FINAL  TO DATA7.
           MOVE DIAMES             TO FIN001-DATA7.


           MOVE 6                  TO GRTIME-DAYS.
           PERFORM ADICIONA-INVERTE-DATA.
           MOVE GRTIME-DATE-FINAL  TO DATA8.
           MOVE DIAMES             TO FIN001-DATA8.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       ADICIONA-INVERTE-DATA SECTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           MOVE GRTIME-DATE-FINAL TO GRTIME-DATE.
           MOVE GRTIME-DATE(7: 2) TO DIAMES(1: 2)
           MOVE GRTIME-DATE(5: 2) TO DIAMES(3: 2).
       GRAVA-SALDO-CAIXA SECTION.
           MOVE "Saldo do dia" TO FIN001-MENSAGEM-AGUARDA.
           MOVE MESANO-CORRENTE TO MESANO-WI.
           SUBTRACT 1 FROM MES-WI.
           IF MES-WI = 00
              MOVE 12 TO MES-WI
              SUBTRACT 1 FROM ANO-WI.

           INITIALIZE REG-CXD040
           MOVE ZEROS TO SALDO-CAIXA
                         ANOMES-CX40.
           START CXD040 KEY IS NOT < ANOMES-CX40 INVALID KEY
                 MOVE "10" TO ST-CXD040.

           PERFORM UNTIL ST-CXD040 = "10"
                 READ CXD040 NEXT RECORD AT END
                      MOVE "10" TO ST-CXD040
                 NOT AT END
                      MOVE ANOMES-CX40        TO
                           FIN001-MENSAGEM-AGUARDA(25: 10)
                      MOVE "TELA-AGUARDA"     TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF ANOMES-CX40 > MESANO-WI
                         MOVE "10" TO ST-CXD040
                      ELSE
                         ADD SALDOE-CX40      TO SALDO-CAIXA
                         SUBTRACT SALDOS-CX40 FROM SALDO-CAIXA
                      END-IF
                 END-READ
           END-PERFORM

           INITIALIZE REG-CXD100
           MOVE DATA-DIA TO DATA-MOV-CX100.
           MOVE 01       TO DATA-MOV-CX100(7: 2).
           MOVE ZEROS    TO SEQ-CX100.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.

           PERFORM UNTIL ST-CXD100 = "10"
                 READ CXD100 NEXT RECORD AT END
                      MOVE "10" TO ST-CXD100
                 NOT AT END
                      IF DATA-MOV-CX100 > DATA-DIA
                         MOVE "10" TO ST-CXD100
                      ELSE
                         MOVE DATA-MOV-CX100 TO
                              FIN001-MENSAGEM-AGUARDA(25: 10)
                         MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM

                         IF TIPO-LCTO-CX100 < 50
                            SUBTRACT VALOR-CX100 FROM SALDO-CAIXA
                         ELSE
                            ADD VALOR-CX100 TO SALDO-CAIXA
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           MOVE SALDO-CAIXA             TO SALDO-ACUM VALOR-E
           MOVE SPACES                  TO FIN001-LINDET
           MOVE "SALDO CAIXA DO DIA: "  TO FIN001-LINDET(01: 20)
           MOVE DATA-MOVTO-W            TO DATA-E
           MOVE DATA-E                  TO FIN001-LINDET(21: 10)
           MOVE VALOR-E                 TO FIN001-LINDET(40: 20)
           MOVE "INSERE-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM LINHA-BRANCO
           MOVE "SALDO EM BANCOS"       TO FIN001-LINDET(01: 80)
           MOVE "INSERE-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM LINHA-BRANCO.
       LINHA-BRANCO SECTION.
           MOVE SPACES TO FIN001-LINDET.
           MOVE "INSERE-LISTA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       GRAVA-SALDO-BANCO SECTION.
           MOVE "Saldo em bancos " TO FIN001-MENSAGEM-AGUARDA.
           MOVE MESANO-CORRENTE    TO MESANO-WI.
           SUBTRACT 1 FROM MES-WI.
      *    IF MES-WI = 00 MOVE 12 TO MES-WI
      *                   SUBTRACT 1 FROM ANO-WI.
      *    MOVE MESANO-WI TO ANOMES-CX41.

           INITIALIZE REG-CXD041
           MOVE ZEROS        TO ANOMES-CX41.
           MOVE CODIGO-BANCO TO CONTAPART-CX41.
           MOVE ZEROS        TO SALDO-BANCO.
           START CXD041 KEY IS NOT < ALT-CX41 INVALID KEY
                 MOVE "10" TO ST-CXD041.

           PERFORM UNTIL ST-CXD041 = "10"
                 READ CXD041 NEXT RECORD AT END
                      MOVE "10" TO ST-CXD041
                 NOT AT END
                      MOVE CODIGO-BANCO   TO
                           FIN001-MENSAGEM-AGUARDA(25: 10)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF CONTAPART-CX41 NOT = CODIGO-BANCO
                        OR ANOMES-CX41 NOT < MESANO-CORRENTE
                              MOVE "10" TO ST-CXD041
                      ELSE
                         ADD SALDOS-CX41      TO SALDO-BANCO
                         SUBTRACT SALDOE-CX41 FROM SALDO-BANCO
                      END-IF
                 END-READ
           END-PERFORM.

           INITIALIZE REG-CXD100
           MOVE CODIGO-BANCO TO CONTAPART-CX100.
           MOVE DATA-DIA TO DATA-MOV-CX100.
           MOVE 01       TO DATA-MOV-CX100(7: 2).
           START CXD100 KEY IS NOT < ALT-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
             READ CXD100 NEXT RECORD AT END
                  MOVE "10" TO ST-CXD100
             NOT AT END
                  MOVE CODIGO-BANCO   TO
                       FIN001-MENSAGEM-AGUARDA(25: 10)
                  MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM

                  IF DATA-MOV-CX100 > DATA-DIA OR
                     CONTAPART-CX100 NOT = CODIGO-BANCO
                     MOVE "10" TO ST-CXD100
                  ELSE
                     IF TIPO-LCTO-CX100 NOT < 50
                        SUBTRACT VALOR-CX100 FROM SALDO-BANCO
                     ELSE
                        ADD VALOR-CX100 TO SALDO-BANCO
                     END-IF
                  END-IF
             END-READ
           END-PERFORM
           ADD 1                     TO I
           MOVE CODIGO-BANCO         TO CODIGO-CG01 BANCO-W1(I).
           READ CGD001 INVALID KEY MOVE "*******" TO NOME-CG01.
           MOVE NOME-CG01            TO FIN001-LINDET(01: 30)
           MOVE SALDO-BANCO          TO VALOR-E VALOR-W1(I).
           MOVE VALOR-E              TO FIN001-LINDET(40: 20)
           ADD SALDO-BANCO           TO SALDO-ACUM.
           MOVE "INSERE-LISTA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       LEITURA-BANCO SECTION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 40
               MOVE ZEROS TO BANCO-W1(I) VALOR-W1(I)
           END-PERFORM.
           MOVE ZEROS TO I.
           INITIALIZE REG-CAD013
           MOVE ZEROS TO CODIGO-CA13.
           START CAD013 KEY IS NOT < CODIGO-CA13 INVALID KEY
                 MOVE "10" TO ST-CAD013.
           PERFORM UNTIL ST-CAD013 = "10"
                 READ CAD013 NEXT RECORD AT END
                      MOVE "10" TO ST-CAD013
                 NOT AT END
                      MOVE CODIGO-CA13 TO CODIGO-BANCO
                      PERFORM GRAVA-SALDO-BANCO
                 END-READ
           END-PERFORM.
           PERFORM LINHA-BRANCO.
           MOVE "SALDO"              TO FIN001-LINDET(01: 80)
           MOVE SALDO-ACUM           TO VALOR-E
           MOVE VALOR-E              TO FIN001-LINDET(60: 20).
           MOVE "INSERE-LISTA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM LINHA-BRANCO.
       DEPOSITO-DIAW SECTION.
           MOVE "CHEQUES"            TO FIN001-MENSAGEM-AGUARDA.

           INITIALIZE REG-CHD010
           MOVE DATA-DIA             TO DATA-VENCTO-CH10.
           MOVE ZEROS                TO SITUACAO-CH10 PORTADOR-CH10.
           MOVE ZEROS                TO DEPOSITO-DIA.
           START CHD010 KEY IS NOT < ALT-CH2 INVALID KEY
                 MOVE "10" TO ST-CHD010.
           PERFORM UNTIL ST-CHD010 = "10"
             READ CHD010 NEXT RECORD AT END
                  MOVE "10" TO ST-CHD010
             NOT AT END
                 MOVE DATA-VENCTO-CH10
                                      TO FIN001-MENSAGEM-AGUARDA(25: 10)
                 MOVE "TELA-AGUARDA"  TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM

                 IF DATA-VENCTO-CH10 <> DATA-DIA OR
                    SITUACAO-CH10 <> 0
                    MOVE "10" TO ST-CHD010
                 ELSE
                    MOVE "Deposito Programado" TO FIN001-TP
                    MOVE 9999                  TO FIN001-PORTADOR
                    READ FIN001 INVALID KEY
                         MOVE "Deposito Programado" TO FIN001-TP
                         MOVE PORTADOR-CH10         TO FIN001-PORTADOR
                         READ FIN001 NOT INVALID KEY
                            ADD VALOR-SALDO-CH10    TO DEPOSITO-DIA
                         END-READ
                    NOT INVALID KEY
                         ADD VALOR-SALDO-CH10    TO DEPOSITO-DIA
                    END-READ
                 END-IF
             END-READ
           END-PERFORM.

           ADD DEPOSITO-DIA                TO SALDO-ACUM.
           MOVE "CHEQUES"                  TO FIN001-LINDET(01: 39)
           MOVE DEPOSITO-DIA               TO VALOR-E
           MOVE VALOR-E                    TO FIN001-LINDET(40: 20)
           MOVE SALDO-ACUM                 TO VALOR-E
           MOVE VALOR-E                    TO FIN001-LINDET(60: 20).
           MOVE "INSERE-LISTA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM LINHA-BRANCO.

           IF FIN001-CONSIDERAR-VENCIDO = 1
              PERFORM CONSIDERAR-VENCIDO
           END-IF

           MOVE "Receber do dia"           TO FIN001-MENSAGEM-AGUARDA.
      *****************CONTAS A RECEBER DIA
      *Sem Baixa
           INITIALIZE REG-CRD020
           MOVE ZEROS    TO SITUACAO-CR20 COD-COMPL-CR20.
           MOVE DATA-DIA TO DATA-VENCTO-CR20
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      MOVE DATA-VENCTO-CR20 TO
                           FIN001-MENSAGEM-AGUARDA(25: 10)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF DATA-VENCTO-CR20 <> DATA-DIA OR
                         SITUACAO-CR20 <> 0
                         MOVE "10" TO ST-CRD020
                      ELSE
                         INITIALIZE REG-FIN001
                         EVALUATE TIPO-DOCTO-CR20
                             WHEN 0 MOVE "0-Boleto"             TO
                                          FIN001-TP
                             WHEN 1 MOVE "1-Dupl/Promis"        TO
                                          FIN001-TP
                             WHEN 2 MOVE "2-Org.Evento"         TO
                                          FIN001-TP
                             WHEN 3 MOVE "3-Debito Automatico"  TO
                                          FIN001-TP
                             WHEN 4 MOVE "4-Cartao de Credito"  TO
                                          FIN001-TP
                         END-EVALUATE
                         MOVE 9999               TO FIN001-PORTADOR
                         READ FIN001 INVALID KEY
                              MOVE PORTADOR-CR20      TO FIN001-PORTADOR
                              READ FIN001 NOT INVALID KEY
                                   MOVE 1               TO PORTPDOC-TIPO
                                   MOVE TIPO-DOCTO-CR20 TO
                                        PORTPDOC-TIPODOC
                                   READ PORTPDOC INVALID KEY
                                        MOVE VALOR-SALDO-CR20 TO
                                             PORTPDOC-VALOR1
                                        WRITE REG-PORTPDOC
                                        END-WRITE
                                   NOT INVALID KEY
                                        ADD VALOR-SALDO-CR20 TO
                                            PORTPDOC-VALOR1
                                        REWRITE REG-PORTPDOC
                                        END-REWRITE
                                   END-READ
                              END-READ
                         NOT INVALID KEY
                              MOVE 1               TO PORTPDOC-TIPO
                              MOVE TIPO-DOCTO-CR20 TO PORTPDOC-TIPODOC
                              READ PORTPDOC INVALID KEY
                                   MOVE VALOR-SALDO-CR20 TO
                                        PORTPDOC-VALOR1
                                   WRITE REG-PORTPDOC
                                   END-WRITE
                              NOT INVALID KEY
                                   ADD VALOR-SALDO-CR20 TO
                                       PORTPDOC-VALOR1
                                   REWRITE REG-PORTPDOC
                                   END-REWRITE
                              END-READ
                         END-READ
                      END-IF
                 END-READ
           END-PERFORM

      *Baixa Parcial
           INITIALIZE REG-CRD020
           MOVE 1                   TO SITUACAO-CR20
           MOVE ZEROS               TO COD-COMPL-CR20
           MOVE DATA-DIA            TO DATA-VENCTO-CR20
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      MOVE DATA-VENCTO-CR20 TO
                           FIN001-MENSAGEM-AGUARDA(25: 10)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF DATA-VENCTO-CR20 <> DATA-DIA OR
                         SITUACAO-CR20 <> 1
                         MOVE "10" TO ST-CRD020
                      ELSE
                         INITIALIZE REG-FIN001
                         EVALUATE TIPO-DOCTO-CR20
                             WHEN 0 MOVE "0-Boleto"             TO
                                          FIN001-TP
                             WHEN 1 MOVE "1-Dupl/Promis"        TO
                                          FIN001-TP
                             WHEN 2 MOVE "2-Org.Evento"         TO
                                          FIN001-TP
                             WHEN 3 MOVE "3-Debito Automatico"  TO
                                          FIN001-TP
                             WHEN 4 MOVE "4-Cartao de Credito"  TO
                                          FIN001-TP
                         END-EVALUATE
                         MOVE 9999               TO FIN001-PORTADOR
                         READ FIN001 INVALID KEY
                              MOVE PORTADOR-CR20      TO FIN001-PORTADOR
                              READ FIN001 NOT INVALID KEY
                                   MOVE 1               TO PORTPDOC-TIPO
                                   MOVE TIPO-DOCTO-CR20 TO
                                        PORTPDOC-TIPODOC
                                   READ PORTPDOC INVALID KEY
                                        MOVE VALOR-SALDO-CR20 TO
                                             PORTPDOC-VALOR1
                                        WRITE REG-PORTPDOC
                                        END-WRITE
                                   NOT INVALID KEY
                                        ADD VALOR-SALDO-CR20 TO
                                            PORTPDOC-VALOR1
                                        REWRITE REG-PORTPDOC
                                        END-REWRITE
                                   END-READ
                              END-READ
                         NOT INVALID KEY
                              MOVE 1               TO PORTPDOC-TIPO
                              MOVE TIPO-DOCTO-CR20 TO PORTPDOC-TIPODOC
                              READ PORTPDOC INVALID KEY
                                   MOVE VALOR-SALDO-CR20 TO
                                        PORTPDOC-VALOR1
                                   WRITE REG-PORTPDOC
                                   END-WRITE
                              NOT INVALID KEY
                                   ADD VALOR-SALDO-CR20 TO
                                       PORTPDOC-VALOR1
                                   REWRITE REG-PORTPDOC
                                   END-REWRITE
                              END-READ
                         END-READ
                      END-IF
                 END-READ
           END-PERFORM.

           MOVE "RECEBER DO DIA"           TO FIN001-LINDET
           MOVE "INSERE-LISTA"             TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           PERFORM LINHA-BRANCO.

           INITIALIZE REG-PORTPDOC
                      TOTAL-TIPO
           MOVE 1                          TO PORTPDOC-TIPO
           START PORTPDOC KEY IS NOT LESS PORTPDOC-CHAVE INVALID KEY
                 MOVE "10" TO ST-PORTPDOC
           END-START

           PERFORM UNTIL ST-PORTPDOC = "10"
                 READ PORTPDOC NEXT AT END
                      MOVE "10" TO ST-PORTPDOC
                 NOT AT END
                      IF PORTPDOC-TIPO <> 1
                         MOVE "10" TO ST-PORTPDOC
                      ELSE
                         INITIALIZE FIN001-LINDET
                         EVALUATE PORTPDOC-TIPODOC
                             WHEN 0 MOVE "0-Boleto"             TO
                                    FIN001-LINDET(5:39)
                             WHEN 1 MOVE "1-Dupl/Promis"        TO
                                    FIN001-LINDET(5:39)
                             WHEN 2 MOVE "2-Org.Evento"         TO
                                    FIN001-LINDET(5:39)
                             WHEN 3 MOVE "3-Debito Automatico"  TO
                                    FIN001-LINDET(5:39)
                             WHEN 4 MOVE "4-Cartao de Credito"  TO
                                    FIN001-LINDET(5:39)
                         END-EVALUATE
                         ADD  PORTPDOC-VALOR1  TO TOTAL-TIPO

                         ADD  PORTPDOC-VALOR1  TO SALDO-ACUM
                         MOVE PORTPDOC-VALOR1  TO VALOR-E
                         MOVE VALOR-E          TO FIN001-LINDET(40: 20)
                         MOVE SALDO-ACUM       TO VALOR-E
                         MOVE VALOR-E          TO FIN001-LINDET(60: 20)
                         MOVE "INSERE-LISTA"   TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM

           MOVE SPACES                 TO FIN001-LINDET
           MOVE "TOTAL"                TO FIN001-LINDET(1:39)
           MOVE TOTAL-TIPO             TO VALOR-E
           MOVE VALOR-E                TO FIN001-LINDET(40: 20)
           MOVE "INSERE-LISTA"         TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM LINHA-BRANCO.

       CONSIDERAR-VENCIDO SECTION.
           MOVE "Receber Vencido"    TO FIN001-MENSAGEM-AGUARDA.

      *****************CONTAS A RECEBER VENCIDO
      *Sem Baixa
           INITIALIZE REG-CRD020
           MOVE ZEROS TO SITUACAO-CR20 COD-COMPL-CR20.
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      MOVE DATA-VENCTO-CR20 TO
                           FIN001-MENSAGEM-AGUARDA(25: 10)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF DATA-VENCTO-CR20 NOT < DATA-DIA OR
                         SITUACAO-CR20 <> 0
                         MOVE "10" TO ST-CRD020
                      ELSE
                         INITIALIZE REG-FIN001
                         EVALUATE TIPO-DOCTO-CR20
                             WHEN 0 MOVE "0-Boleto"             TO
                                          FIN001-TP
                             WHEN 1 MOVE "1-Dupl/Promis"        TO
                                          FIN001-TP
                             WHEN 2 MOVE "2-Org.Evento"         TO
                                          FIN001-TP
                             WHEN 3 MOVE "3-Debito Automatico"  TO
                                          FIN001-TP
                             WHEN 4 MOVE "4-Cartao de Credito"  TO
                                          FIN001-TP
                         END-EVALUATE
                         MOVE 9999               TO FIN001-PORTADOR
                         READ FIN001 INVALID KEY
                              MOVE PORTADOR-CR20      TO FIN001-PORTADOR
                              READ FIN001 NOT INVALID KEY
                                   MOVE 2               TO PORTPDOC-TIPO
                                   MOVE TIPO-DOCTO-CR20 TO
                                        PORTPDOC-TIPODOC
                                   READ PORTPDOC INVALID KEY
                                        MOVE VALOR-SALDO-CR20 TO
                                             PORTPDOC-VALOR1
                                        WRITE REG-PORTPDOC
                                        END-WRITE
                                   NOT INVALID KEY
                                        ADD VALOR-SALDO-CR20 TO
                                            PORTPDOC-VALOR1
                                        REWRITE REG-PORTPDOC
                                        END-REWRITE
                                   END-READ
                              END-READ
                         NOT INVALID KEY
                              MOVE 2               TO PORTPDOC-TIPO
                              MOVE TIPO-DOCTO-CR20 TO PORTPDOC-TIPODOC
                              READ PORTPDOC INVALID KEY
                                   MOVE VALOR-SALDO-CR20 TO
                                        PORTPDOC-VALOR1
                                   WRITE REG-PORTPDOC
                                   END-WRITE
                              NOT INVALID KEY
                                   ADD VALOR-SALDO-CR20 TO
                                       PORTPDOC-VALOR1
                                   REWRITE REG-PORTPDOC
                                   END-REWRITE
                              END-READ
                         END-READ
                      END-IF
                 END-READ
           END-PERFORM

      *Baixa Parcial
           INITIALIZE REG-CRD020
           MOVE 1                   TO SITUACAO-CR20
           MOVE ZEROS               TO COD-COMPL-CR20
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      MOVE DATA-VENCTO-CR20 TO
                           FIN001-MENSAGEM-AGUARDA(25: 10)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF DATA-VENCTO-CR20 NOT < DATA-DIA OR
                         SITUACAO-CR20 <> 1
                         MOVE "10" TO ST-CRD020
                      ELSE
                         INITIALIZE REG-FIN001
                         EVALUATE TIPO-DOCTO-CR20
                             WHEN 0 MOVE "0-Boleto"             TO
                                          FIN001-TP
                             WHEN 1 MOVE "1-Dupl/Promis"        TO
                                          FIN001-TP
                             WHEN 2 MOVE "2-Org.Evento"         TO
                                          FIN001-TP
                             WHEN 3 MOVE "3-Debito Automatico"  TO
                                          FIN001-TP
                             WHEN 4 MOVE "4-Cartao de Credito"  TO
                                          FIN001-TP
                         END-EVALUATE
                         MOVE 9999               TO FIN001-PORTADOR
                         READ FIN001 INVALID KEY
                              MOVE PORTADOR-CR20      TO FIN001-PORTADOR
                              READ FIN001 NOT INVALID KEY
                                   MOVE 2               TO PORTPDOC-TIPO
                                   MOVE TIPO-DOCTO-CR20 TO
                                        PORTPDOC-TIPODOC
                                   READ PORTPDOC INVALID KEY
                                        MOVE VALOR-SALDO-CR20 TO
                                             PORTPDOC-VALOR1
                                        WRITE REG-PORTPDOC
                                        END-WRITE
                                   NOT INVALID KEY
                                        ADD VALOR-SALDO-CR20 TO
                                            PORTPDOC-VALOR1
                                        REWRITE REG-PORTPDOC
                                        END-REWRITE
                                   END-READ
                              END-READ
                         NOT INVALID KEY
                              MOVE 2               TO PORTPDOC-TIPO
                              MOVE TIPO-DOCTO-CR20 TO PORTPDOC-TIPODOC
                              READ PORTPDOC INVALID KEY
                                   MOVE VALOR-SALDO-CR20 TO
                                        PORTPDOC-VALOR1
                                   WRITE REG-PORTPDOC
                                   END-WRITE
                              NOT INVALID KEY
                                   ADD VALOR-SALDO-CR20 TO
                                       PORTPDOC-VALOR1
                                   REWRITE REG-PORTPDOC
                                   END-REWRITE
                              END-READ
                         END-READ
                      END-IF
                 END-READ
           END-PERFORM

           MOVE "RECEBER VENCIDO"          TO FIN001-LINDET
           MOVE "INSERE-LISTA"             TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           PERFORM LINHA-BRANCO.

           INITIALIZE REG-PORTPDOC
                      TOTAL-TIPO
           MOVE 2                          TO PORTPDOC-TIPO
           START PORTPDOC KEY IS NOT LESS PORTPDOC-CHAVE INVALID KEY
                 MOVE "10" TO ST-PORTPDOC
           END-START

           PERFORM UNTIL ST-PORTPDOC = "10"
                 READ PORTPDOC NEXT AT END
                      MOVE "10" TO ST-PORTPDOC
                 NOT AT END
                      IF PORTPDOC-TIPO <> 2
                         MOVE "10" TO ST-PORTPDOC
                      ELSE
                         INITIALIZE FIN001-LINDET
                         EVALUATE PORTPDOC-TIPODOC
                             WHEN 0 MOVE "0-Boleto"             TO
                                    FIN001-LINDET(5:39)
                             WHEN 1 MOVE "1-Dupl/Promis"        TO
                                    FIN001-LINDET(5:39)
                             WHEN 2 MOVE "2-Org.Evento"         TO
                                    FIN001-LINDET(5:39)
                             WHEN 3 MOVE "3-Debito Automatico"  TO
                                    FIN001-LINDET(5:39)
                             WHEN 4 MOVE "4-Cartao de Credito"  TO
                                    FIN001-LINDET(5:39)
                         END-EVALUATE
                         ADD  PORTPDOC-VALOR1  TO TOTAL-TIPO

                         ADD  PORTPDOC-VALOR1  TO SALDO-ACUM
                         MOVE PORTPDOC-VALOR1  TO VALOR-E
                         MOVE VALOR-E          TO FIN001-LINDET(40: 20)
                         MOVE SALDO-ACUM       TO VALOR-E
                         MOVE VALOR-E          TO FIN001-LINDET(60: 20)
                         MOVE "INSERE-LISTA"   TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM

           MOVE SPACES                 TO FIN001-LINDET
           MOVE "TOTAL"                TO FIN001-LINDET(1:39)
           MOVE TOTAL-TIPO             TO VALOR-E
           MOVE VALOR-E                TO FIN001-LINDET(40: 20)
           MOVE "INSERE-LISTA"         TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           PERFORM LINHA-BRANCO.


       PAGTO-ATRASOW SECTION.
           MOVE "Pagto em atraso"  TO FIN001-MENSAGEM-AGUARDA.
           INITIALIZE REG-CPD020
           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           MOVE ZEROS TO PAGTO-ATRASO-PREV
                         PAGTO-ATRASO-DEF
           PERFORM UNTIL ST-CPD020 = "10"
                 READ CPD020 NEXT RECORD AT END
                     MOVE "10" TO ST-CPD020
                 NOT AT END
                     MOVE DATA-VENCTO-CP20
                          TO FIN001-MENSAGEM-AGUARDA(25: 10)
                     MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM

                     IF SITUACAO-CP20 > 1
                        MOVE "10" TO ST-CPD020
                     ELSE
                        IF DATA-VENCTO-CP20 NOT < DATA-DIA
                           MOVE "10" TO ST-CPD020
                        ELSE
                           IF PREV-DEF-CP20 IS NOT NUMERIC
                              MOVE 0 TO PREV-DEF-CP20
                           END-IF
                           IF (FIN001-CONSIDERAR-PREVIST = 1 AND
                               PREV-DEF-CP20 = 1) OR
                               PREV-DEF-CP20 = 0
      *                        IF JUROS-MORA-CP20 NOT = ZEROS
      *                           PERFORM CALCULA-JUROS
      *                        ELSE
      *                           MOVE ZEROS TO VALOR-JUROS
      *                        END-IF
                               MOVE "Contas a Pagar" TO FIN001-TP
                               MOVE 9999             TO FIN001-PORTADOR
                               READ FIN001 INVALID KEY
                                    MOVE "Contas a Pagar" TO FIN001-TP
                                    MOVE PORTADOR-CP20    TO
                                         FIN001-PORTADOR
                                    READ FIN001 NOT INVALID KEY
                                         IF PREV-DEF-CP20 = 1
                                            COMPUTE PAGTO-ATRASO-PREV =
                                                    PAGTO-ATRASO-PREV +
      *                                             VALOR-JUROS       +
      *                                             MULTA-ATRASO-CP20 +
                                                    VALOR-TOT-CP20
                                            PERFORM GERAR-PREVISTO
                                         ELSE
                                            COMPUTE PAGTO-ATRASO-DEF =
                                                    PAGTO-ATRASO-DEF  +
      *                                             VALOR-JUROS       +
      *                                             MULTA-ATRASO-CP20 +
                                                    VALOR-TOT-CP20
                                            PERFORM GERAR-DEFINITIVO
                                         END-IF
                                    END-READ
                               NOT INVALID KEY
                                    IF PREV-DEF-CP20 = 1
                                       COMPUTE PAGTO-ATRASO-PREV =
                                               PAGTO-ATRASO-PREV +
      *                                        VALOR-JUROS       +
      *                                        MULTA-ATRASO-CP20 +
                                               VALOR-TOT-CP20
                                       PERFORM GERAR-PREVISTO
                                    ELSE
                                       COMPUTE PAGTO-ATRASO-DEF =
                                               PAGTO-ATRASO-DEF  +
      *                                        VALOR-JUROS       +
      *                                        MULTA-ATRASO-CP20 +
                                               VALOR-TOT-CP20
                                       PERFORM GERAR-DEFINITIVO
                                    END-IF
                               END-READ
                           END-IF
      *                 END-IF
                     END-IF
                 END-READ
           END-PERFORM.

           MOVE "CONTAS A PAGAR VENCIDO"   TO FIN001-LINDET
           MOVE "INSERE-LISTA"             TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO FIN001-LINDET
           MOVE "INSERE-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


           INITIALIZE TOTAL-TIPO

           COMPUTE PAGTO-ATRASO-PREV = PAGTO-ATRASO-PREV * -1.
           COMPUTE PAGTO-ATRASO-DEF  = PAGTO-ATRASO-DEF  * -1.
           MOVE "    PREVISTO"             TO FIN001-LINDET(01: 39)
           MOVE PAGTO-ATRASO-PREV          TO VALOR-E
           MOVE VALOR-E                    TO FIN001-LINDET(40: 20)
           ADD PAGTO-ATRASO-PREV           TO SALDO-ACUM
           MOVE SALDO-ACUM                 TO VALOR-E
           MOVE VALOR-E                    TO FIN001-LINDET(60: 20)
           MOVE "INSERE-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "    DEFINITIVO"           TO FIN001-LINDET(01: 39)
           MOVE PAGTO-ATRASO-DEF           TO VALOR-E
           MOVE VALOR-E                    TO FIN001-LINDET(40: 20)
           ADD PAGTO-ATRASO-DEF            TO SALDO-ACUM
           MOVE SALDO-ACUM                 TO VALOR-E
           MOVE VALOR-E                    TO FIN001-LINDET(60: 20)
           MOVE "INSERE-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           ADD PAGTO-ATRASO-PREV           TO TOTAL-TIPO
           ADD PAGTO-ATRASO-DEF            TO TOTAL-TIPO

           MOVE SPACES                     TO FIN001-LINDET
           MOVE "TOTAL"                    TO FIN001-LINDET(01: 39)
           MOVE TOTAL-TIPO                 TO VALOR-E
           MOVE VALOR-E                    TO FIN001-LINDET(40: 20)
           MOVE VALOR-E                    TO FIN001-LINDET(60: 20)
           MOVE "INSERE-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           PERFORM LINHA-BRANCO.
       CALCULA-JUROS SECTION.
      *    MOVE DATA-VENCTO-CP20 TO GRDIAS-AAMMDD-INICIAL.
      *    MOVE DATA-DIA         TO GRDIAS-AAMMDD-FINAL.
      *    CALL "GRDIAS1" USING PARAMETROS-GRDIAS.
      *    MULTIPLY GRDIAS-NUM-DIAS BY JUROS-MORA-CP20
      *             GIVING VALOR-JUROS.
           MOVE DATA-VENCTO-CP20 TO GRTIME-DATE.
           MOVE DATA-DIA         TO GRTIME-DATE-FINAL.
           MOVE 2                TO GRTIME-TYPE.
           MOVE 3                TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           MULTIPLY GRTIME-DAYS-FINAL BY JUROS-MORA-CP20
                    GIVING VALOR-JUROS.

       GERAR-PREVISTO SECTION.
           MOVE 1                 TO AUXCPD-TIPO
           MOVE FORNEC-CP20       TO AUXCPD-FORNECEDOR
           MOVE SEQ-CP20          TO AUXCPD-SEQUENCIA
           MOVE DATA-VENCTO-CP20  TO AUXCPD-VENCTO
           MOVE VALOR-TOT-CP20    TO AUXCPD-VALOR
           MOVE DATA-EMISSAO-CP20 TO AUXCPD-EMISSAO
           MOVE DESCRICAO-CP20    TO AUXCPD-DESCRICAO
           MOVE PORTADOR-CP20     TO AUXCPD-PORTADOR
           MOVE TIPO-FORN-CP20    TO AUXCPD-TP-FORNEC
           MOVE RESPONSAVEL-CP20  TO AUXCPD-RESPONSAVEL

           WRITE REG-AUXCPD INVALID KEY
                 MOVE "Erro de Gravação...AUXCPD" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM.


       GERAR-DEFINITIVO SECTION.
           MOVE 2                 TO AUXCPD-TIPO
           MOVE FORNEC-CP20       TO AUXCPD-FORNECEDOR
           MOVE SEQ-CP20          TO AUXCPD-SEQUENCIA
           MOVE DATA-VENCTO-CP20  TO AUXCPD-VENCTO
           MOVE VALOR-TOT-CP20    TO AUXCPD-VALOR
           MOVE DATA-EMISSAO-CP20 TO AUXCPD-EMISSAO
           MOVE DESCRICAO-CP20    TO AUXCPD-DESCRICAO
           MOVE PORTADOR-CP20     TO AUXCPD-PORTADOR
           MOVE TIPO-FORN-CP20    TO AUXCPD-TP-FORNEC
           MOVE RESPONSAVEL-CP20  TO AUXCPD-RESPONSAVEL

           WRITE REG-AUXCPD INVALID KEY
                 MOVE "Erro de Gravação...AUXCPD" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM.

       PAGTO-DIAW SECTION.
           MOVE "Pagamento do dia" TO FIN001-MENSAGEM-AGUARDA.
           INITIALIZE REG-CPD020
           MOVE DATA-DIA  TO DATA-VENCTO-CP20.
           MOVE ZEROS     TO SITUACAO-CP20.
           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           MOVE ZEROS TO PAGTO-DIA-PREV
                         PAGTO-DIA-DEF
           PERFORM UNTIL ST-CPD020 = "10"
                 READ CPD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CPD020
                 NOT AT END
                     MOVE DATA-VENCTO-CP20
                           TO FIN001-MENSAGEM-AGUARDA(25: 10)
                     MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM

                     IF SITUACAO-CP20 > 1
                        MOVE "10" TO ST-CPD020
                     ELSE
                        IF DATA-VENCTO-CP20 > DATA-DIA
                           MOVE "10" TO ST-CPD020
                        ELSE
                           IF PREV-DEF-CP20 IS NOT NUMERIC
                              MOVE 0 TO PREV-DEF-CP20
                           END-IF
                           IF (FIN001-CONSIDERAR-PREVIST = 1 AND
                               PREV-DEF-CP20 = 1) OR
                               PREV-DEF-CP20 = 0
                               MOVE "Contas a Pagar" TO FIN001-TP
                               MOVE 9999             TO FIN001-PORTADOR
                               READ FIN001 INVALID KEY
                                   MOVE "Contas a Pagar" TO FIN001-TP
                                   MOVE PORTADOR-CP20 TO FIN001-PORTADOR
                                   READ FIN001 NOT INVALID KEY
                                        IF PREV-DEF-CP20 = 1
                                           ADD VALOR-TOT-CP20 TO
                                               PAGTO-DIA-PREV
                                        ELSE
                                           ADD VALOR-TOT-CP20 TO
                                               PAGTO-DIA-DEF
                                        END-IF
                                   END-READ
                               NOT INVALID KEY
                                   IF PREV-DEF-CP20 = 1
                                      ADD VALOR-TOT-CP20 TO
                                          PAGTO-DIA-PREV
                                   ELSE
                                      ADD VALOR-TOT-CP20 TO
                                          PAGTO-DIA-DEF
                                   END-IF
                               END-READ
                           END-IF
                        END-IF
                     END-IF
                 END-READ
           END-PERFORM.

           MOVE "CONTAS A PAGAR DO DIA"   TO FIN001-LINDET
           MOVE "INSERE-LISTA"            TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES                    TO FIN001-LINDET
           MOVE "INSERE-LISTA"            TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           INITIALIZE TOTAL-TIPO


           COMPUTE PAGTO-DIA-PREV = PAGTO-DIA-PREV * -1.
           MOVE "    PREVISTO"            TO FIN001-LINDET(01: 39)
           MOVE PAGTO-DIA-PREV            TO VALOR-E
           MOVE VALOR-E                   TO FIN001-LINDET(40: 20)
           ADD PAGTO-DIA-PREV             TO SALDO-ACUM
           MOVE SALDO-ACUM                TO VALOR-E
           MOVE VALOR-E                   TO FIN001-LINDET(60: 20)
           MOVE "INSERE-LISTA"            TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           COMPUTE PAGTO-DIA-DEF = PAGTO-DIA-DEF * -1.
           MOVE "    DEFINITIVO"          TO FIN001-LINDET(01: 39)
           MOVE PAGTO-DIA-DEF             TO VALOR-E
           MOVE VALOR-E                   TO FIN001-LINDET(40: 20)
           ADD PAGTO-DIA-DEF              TO SALDO-ACUM
           MOVE SALDO-ACUM                TO VALOR-E
           MOVE VALOR-E                   TO FIN001-LINDET(60: 20)
           MOVE "INSERE-LISTA"            TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           ADD PAGTO-DIA-PREV  TO TOTAL-TIPO
           ADD PAGTO-DIA-DEF   TO TOTAL-TIPO

           MOVE SPACES                    TO FIN001-LINDET
           MOVE "TOTAL"                   TO FIN001-LINDET(1:39)
           MOVE TOTAL-TIPO                TO VALOR-E
           MOVE VALOR-E                   TO FIN001-LINDET(40: 20)
           MOVE "INSERE-LISTA"            TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       DEPOSITO-PROGRAMADO SECTION.
           MOVE "Deposito programado" TO FIN001-MENSAGEM-AGUARDA.
           INITIALIZE REG-CHD010
           MOVE DATA1                TO DATA-VENCTO-CH10.
           MOVE ZEROS                TO SITUACAO-CH10 PORTADOR-CH10.
           MOVE ZEROS                TO CHEQUE-1SEMANA CHEQUE-2SEMANA
                                        CHEQUE-3SEMANA CHEQUE-4SEMANA.
           START CHD010 KEY IS NOT < ALT-CH2 INVALID KEY
                 MOVE "10" TO ST-CHD010.
           PERFORM UNTIL ST-CHD010 = "10"
                 READ CHD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CHD010
                 NOT AT END
                     MOVE DATA-VENCTO-CH10
                          TO FIN001-MENSAGEM-AGUARDA(25: 10)
                     MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM

                     IF DATA-VENCTO-CH10 > DATA8 OR
                        SITUACAO-CH10 <> 0
                        MOVE "10" TO ST-CHD010
                     ELSE
                        MOVE "Deposito Programado" TO FIN001-TP
                        MOVE 9999                  TO FIN001-PORTADOR
                        READ FIN001 INVALID KEY
                             MOVE PORTADOR-CH10  TO FIN001-PORTADOR
                             READ FIN001 NOT INVALID KEY
                                  PERFORM INTERVALO-CHEQUE
                             END-READ
                        NOT INVALID KEY
                             PERFORM INTERVALO-CHEQUE
                        END-READ
                 END-READ
           END-PERFORM.
       INTERVALO-CHEQUE SECTION.
           IF DATA-VENCTO-CH10 NOT >
              DATA2 ADD VALOR-SALDO-CH10 TO CHEQUE-1SEMANA
           ELSE
            IF DATA-VENCTO-CH10 NOT >
               DATA4 ADD VALOR-SALDO-CH10 TO CHEQUE-2SEMANA
            ELSE
              IF DATA-VENCTO-CH10  NOT > DATA6
                 ADD VALOR-SALDO-CH10 TO CHEQUE-3SEMANA
              ELSE
                ADD VALOR-SALDO-CH10 TO CHEQUE-4SEMANA.
       CONTAS-A-RECEBER SECTION.
      *>Inicializa todos os tipos de documentos
           MOVE 3 TO PORTPDOC-TIPO
           MOVE 0 TO PORTPDOC-TIPODOC
           MOVE 0 TO PORTPDOC-VALOR1
           MOVE 0 TO PORTPDOC-VALOR2
           MOVE 0 TO PORTPDOC-VALOR3
           MOVE 0 TO PORTPDOC-VALOR4
           WRITE REG-PORTPDOC
           MOVE 3 TO PORTPDOC-TIPO
           MOVE 1 TO PORTPDOC-TIPODOC
           MOVE 0 TO PORTPDOC-VALOR1
           MOVE 0 TO PORTPDOC-VALOR2
           MOVE 0 TO PORTPDOC-VALOR3
           MOVE 0 TO PORTPDOC-VALOR4
           WRITE REG-PORTPDOC
           MOVE 3 TO PORTPDOC-TIPO
           MOVE 2 TO PORTPDOC-TIPODOC
           MOVE 0 TO PORTPDOC-VALOR1
           MOVE 0 TO PORTPDOC-VALOR2
           MOVE 0 TO PORTPDOC-VALOR3
           MOVE 0 TO PORTPDOC-VALOR4
           WRITE REG-PORTPDOC
           MOVE 3 TO PORTPDOC-TIPO
           MOVE 3 TO PORTPDOC-TIPODOC
           MOVE 0 TO PORTPDOC-VALOR1
           MOVE 0 TO PORTPDOC-VALOR2
           MOVE 0 TO PORTPDOC-VALOR3
           MOVE 0 TO PORTPDOC-VALOR4
           WRITE REG-PORTPDOC
           MOVE 3 TO PORTPDOC-TIPO
           MOVE 4 TO PORTPDOC-TIPODOC
           MOVE 0 TO PORTPDOC-VALOR1
           MOVE 0 TO PORTPDOC-VALOR2
           MOVE 0 TO PORTPDOC-VALOR3
           MOVE 0 TO PORTPDOC-VALOR4
           WRITE REG-PORTPDOC
      ******************************************************************

           MOVE "Contas a receber"  TO FIN001-MENSAGEM-AGUARDA.
      *Sem Baixa
           INITIALIZE REG-CRD020
           MOVE DATA1               TO DATA-VENCTO-CR20.
           MOVE ZEROS TO SITUACAO-CR20 COD-COMPL-CR20.
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      MOVE DATA-VENCTO-CR20 TO
                           FIN001-MENSAGEM-AGUARDA(25: 10)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF DATA-VENCTO-CR20 > DATA8 OR
                         SITUACAO-CR20 <> 0
                         MOVE "10" TO ST-CRD020
                      ELSE
                         INITIALIZE REG-FIN001
                         EVALUATE TIPO-DOCTO-CR20
                             WHEN 0 MOVE "0-Boleto"             TO
                                          FIN001-TP
                             WHEN 1 MOVE "1-Dupl/Promis"        TO
                                          FIN001-TP
                             WHEN 2 MOVE "2-Org.Evento"         TO
                                          FIN001-TP
                             WHEN 3 MOVE "3-Debito Automatico"  TO
                                          FIN001-TP
                             WHEN 4 MOVE "4-Cartao de Credito"  TO
                                          FIN001-TP
                         END-EVALUATE
                         MOVE 9999               TO FIN001-PORTADOR
                         READ FIN001 INVALID KEY
                              MOVE PORTADOR-CR20      TO FIN001-PORTADOR
                              READ FIN001 NOT INVALID KEY
                                   MOVE 3               TO PORTPDOC-TIPO
                                   MOVE TIPO-DOCTO-CR20 TO
                                        PORTPDOC-TIPODOC
                                   READ PORTPDOC INVALID KEY
                                        INITIALIZE PORTPDOC-VALOR1
                                                   PORTPDOC-VALOR2
                                                   PORTPDOC-VALOR3
                                                   PORTPDOC-VALOR4
                                        PERFORM INTERVALO-RECEBER
                                        WRITE REG-PORTPDOC
                                        END-WRITE
                                   NOT INVALID KEY
                                        PERFORM INTERVALO-RECEBER
                                        REWRITE REG-PORTPDOC
                                        END-REWRITE
                                   END-READ
                              END-READ
                         NOT INVALID KEY
                              MOVE 3               TO PORTPDOC-TIPO
                              MOVE TIPO-DOCTO-CR20 TO PORTPDOC-TIPODOC
                              READ PORTPDOC INVALID KEY
                                   INITIALIZE PORTPDOC-VALOR1
                                              PORTPDOC-VALOR2
                                              PORTPDOC-VALOR3
                                              PORTPDOC-VALOR4
                                   PERFORM INTERVALO-RECEBER
                                   WRITE REG-PORTPDOC
                                   END-WRITE
                              NOT INVALID KEY
                                   PERFORM INTERVALO-RECEBER
                                   REWRITE REG-PORTPDOC
                                   END-REWRITE
                              END-READ
                         END-READ
                      END-IF
                 END-READ
           END-PERFORM

      *Baixa Parcial
           INITIALIZE REG-CRD020
           MOVE DATA1               TO DATA-VENCTO-CR20
           MOVE 1                   TO SITUACAO-CR20
           MOVE ZEROS               TO COD-COMPL-CR20
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      MOVE DATA-VENCTO-CR20 TO
                           FIN001-MENSAGEM-AGUARDA(25: 10)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF DATA-VENCTO-CR20 > DATA8 OR
                         SITUACAO-CR20   <> 1
                         MOVE "10" TO ST-CRD020
                      ELSE
                         INITIALIZE REG-FIN001
                         EVALUATE TIPO-DOCTO-CR20
                             WHEN 0 MOVE "0-Boleto"             TO
                                          FIN001-TP
                             WHEN 1 MOVE "1-Dupl/Promis"        TO
                                          FIN001-TP
                             WHEN 2 MOVE "2-Org.Evento"         TO
                                          FIN001-TP
                             WHEN 3 MOVE "3-Debito Automatico"  TO
                                          FIN001-TP
                             WHEN 4 MOVE "4-Cartao de Credito"  TO
                                          FIN001-TP
                         END-EVALUATE
                         MOVE 9999               TO FIN001-PORTADOR
                         READ FIN001 INVALID KEY
                              MOVE PORTADOR-CR20      TO FIN001-PORTADOR
                              READ FIN001 NOT INVALID KEY
                                   MOVE 3               TO PORTPDOC-TIPO
                                   MOVE TIPO-DOCTO-CR20 TO
                                        PORTPDOC-TIPODOC
                                   READ PORTPDOC INVALID KEY
                                        INITIALIZE PORTPDOC-VALOR1
                                                   PORTPDOC-VALOR2
                                                   PORTPDOC-VALOR3
                                                   PORTPDOC-VALOR4
                                        PERFORM INTERVALO-RECEBER
                                        WRITE REG-PORTPDOC
                                        END-WRITE
                                   NOT INVALID KEY
                                        PERFORM INTERVALO-RECEBER
                                        REWRITE REG-PORTPDOC
                                        END-REWRITE
                                   END-READ
                              END-READ
                         NOT INVALID KEY
                              MOVE 3               TO PORTPDOC-TIPO
                              MOVE TIPO-DOCTO-CR20 TO PORTPDOC-TIPODOC
                              READ PORTPDOC INVALID KEY
                                   INITIALIZE PORTPDOC-VALOR1
                                              PORTPDOC-VALOR2
                                              PORTPDOC-VALOR3
                                              PORTPDOC-VALOR4
                                   PERFORM INTERVALO-RECEBER
                                   WRITE REG-PORTPDOC
                                   END-WRITE
                              NOT INVALID KEY
                                   PERFORM INTERVALO-RECEBER
                                   REWRITE REG-PORTPDOC
                                   END-REWRITE
                              END-READ
                         END-READ
                      END-IF
                 END-READ
           END-PERFORM.
           .
       INTERVALO-RECEBER SECTION.
           IF DATA-VENCTO-CR20 NOT > DATA2
              ADD VALOR-SALDO-CR20 TO PORTPDOC-VALOR1
           ELSE
              IF DATA-VENCTO-CR20 NOT > DATA4
                 ADD VALOR-SALDO-CR20 TO PORTPDOC-VALOR2
              ELSE
                 IF DATA-VENCTO-CR20 NOT > DATA6
                    ADD VALOR-SALDO-CR20 TO PORTPDOC-VALOR3
                 ELSE
                    ADD VALOR-SALDO-CR20 TO PORTPDOC-VALOR4.
       CONTAS-A-PAGAR SECTION.
           MOVE "Contas a pagar" TO FIN001-MENSAGEM-AGUARDA.
           MOVE ZEROS TO PAGAR-PREV-1SEMANA PAGAR-DEF-1SEMANA
                         PAGAR-PREV-2SEMANA PAGAR-DEF-2SEMANA
                         PAGAR-PREV-3SEMANA PAGAR-DEF-3SEMANA
                         PAGAR-PREV-4SEMANA PAGAR-DEF-4SEMANA

           INITIALIZE REG-CPD020
           MOVE DATA1     TO DATA-VENCTO-CP20.
           MOVE ZEROS     TO SITUACAO-CP20.
           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
                 READ CPD020 NEXT RECORD AT END
                     MOVE "10" TO ST-CPD020
                 NOT AT END
                     MOVE DATA-VENCTO-CP20
                       TO FIN001-MENSAGEM-AGUARDA(25: 10)
                     MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM

                     IF SITUACAO-CP20 > 1
                        MOVE "10" TO ST-CPD020
                     ELSE
                        IF DATA-VENCTO-CP20 > DATA8
                           MOVE "10" TO ST-CPD020
                        ELSE
                           IF PREV-DEF-CP20 IS NOT NUMERIC
                              MOVE 0 TO PREV-DEF-CP20
                           END-IF
                           IF (FIN001-CONSIDERAR-PREVIST = 1 AND
                               PREV-DEF-CP20 = 1) OR
                               PREV-DEF-CP20 = 0
                               MOVE "Contas a Pagar" TO FIN001-TP
                               MOVE 9999             TO FIN001-PORTADOR
                               READ FIN001 INVALID KEY
                                    MOVE "Contas a Pagar" TO FIN001-TP
                                    MOVE PORTADOR-CP20 TO
                                         FIN001-PORTADOR
                                    READ FIN001 NOT INVALID KEY
                                         PERFORM INTERVALO-PAGAR
                                    END-READ
                               NOT INVALID KEY
                                    MOVE "Contas a Pagar" TO FIN001-TP
                                    MOVE PORTADOR-CP20 TO
                                         FIN001-PORTADOR
                                    READ FIN001 NOT INVALID KEY
                                         PERFORM INTERVALO-PAGAR
                                    END-READ
                               END-READ
                           END-IF
                        END-IF
                     END-IF
                 END-READ
           END-PERFORM.
       INTERVALO-PAGAR SECTION.
           IF DATA-VENCTO-CP20 NOT > DATA2
              IF PREV-DEF-CP20 = 1
                 ADD VALOR-TOT-CP20 TO PAGAR-PREV-1SEMANA
              ELSE
                 ADD VALOR-TOT-CP20 TO PAGAR-DEF-1SEMANA
              END-IF
           ELSE
            IF DATA-VENCTO-CP20 NOT > DATA4
               IF PREV-DEF-CP20 = 1
                  ADD VALOR-TOT-CP20 TO PAGAR-PREV-2SEMANA
               ELSE
                  ADD VALOR-TOT-CP20 TO PAGAR-DEF-2SEMANA
               END-IF
            ELSE
              IF DATA-VENCTO-CP20 NOT > DATA6
                 IF PREV-DEF-CP20 = 1
                    ADD VALOR-TOT-CP20 TO PAGAR-PREV-3SEMANA
                 ELSE
                    ADD VALOR-TOT-CP20 TO PAGAR-DEF-3SEMANA
                 END-IF
              ELSE
                 IF PREV-DEF-CP20 = 1
                    ADD VALOR-TOT-CP20 TO PAGAR-PREV-4SEMANA
                 ELSE
                    ADD VALOR-TOT-CP20 TO PAGAR-DEF-4SEMANA.
       SALDO-PREVISTO SECTION.
           MOVE "UNSHOW-TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           INITIALIZE REG-PORTPDOC
                      RECEBER-1SEMANA
                      RECEBER-2SEMANA
                      RECEBER-3SEMANA
                      RECEBER-4SEMANA

           MOVE 3 TO PORTPDOC-TIPO
           START PORTPDOC KEY IS NOT LESS PORTPDOC-CHAVE INVALID KEY
                 MOVE "10" TO ST-PORTPDOC.

           PERFORM UNTIL ST-PORTPDOC = "10"
                 READ PORTPDOC NEXT AT END
                      MOVE "10" TO ST-PORTPDOC
                 NOT AT END
                      IF PORTPDOC-TIPO <> 3
                         MOVE "10" TO ST-PORTPDOC
                      ELSE
                         ADD PORTPDOC-VALOR1 TO RECEBER-1SEMANA
                         ADD PORTPDOC-VALOR2 TO RECEBER-2SEMANA
                         ADD PORTPDOC-VALOR3 TO RECEBER-3SEMANA
                         ADD PORTPDOC-VALOR4 TO RECEBER-4SEMANA
                      END-IF
                 END-READ
           END-PERFORM


           COMPUTE SALDO-1SEMANA = (SALDO-ACUM + CHEQUE-1SEMANA +
                                    RECEBER-1SEMANA) -
                               (PAGAR-PREV-1SEMANA + PAGAR-DEF-1SEMANA)
           COMPUTE SALDO-2SEMANA = (SALDO-1SEMANA + CHEQUE-2SEMANA +
                                    RECEBER-2SEMANA) -
                               (PAGAR-PREV-2SEMANA + PAGAR-DEF-2SEMANA)
           COMPUTE SALDO-3SEMANA = (SALDO-2SEMANA + CHEQUE-3SEMANA +
                                    RECEBER-3SEMANA) -
                               (PAGAR-PREV-3SEMANA + PAGAR-DEF-3SEMANA)
           COMPUTE SALDO-4SEMANA = (SALDO-3SEMANA + CHEQUE-4SEMANA +
                                    RECEBER-4SEMANA) -
                               (PAGAR-PREV-4SEMANA + PAGAR-DEF-4SEMANA)

           MOVE CHEQUE-1SEMANA      TO VALOR-E.
           MOVE VALOR-E             TO FIN001-LINDET1(01: 17).
           MOVE CHEQUE-2SEMANA      TO VALOR-E
           MOVE VALOR-E             TO FIN001-LINDET1(18: 14).
           MOVE CHEQUE-3SEMANA      TO VALOR-E.
           MOVE VALOR-E             TO FIN001-LINDET1(32: 14).
           MOVE CHEQUE-4SEMANA      TO VALOR-E.
           MOVE VALOR-E             TO FIN001-LINDET1(46: 14).
           MOVE "INSERE-LISTA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           INITIALIZE REG-PORTPDOC
           MOVE 3                   TO PORTPDOC-TIPO
           START PORTPDOC KEY IS NOT LESS PORTPDOC-CHAVE INVALID KEY
                 MOVE "10" TO ST-PORTPDOC
           END-START
           PERFORM UNTIL ST-PORTPDOC = "10"
                 READ PORTPDOC NEXT AT END
                      MOVE "10" TO ST-PORTPDOC
                 NOT AT END
                      IF PORTPDOC-TIPO <> 3
                         MOVE "10" TO ST-PORTPDOC
                      ELSE
                         MOVE PORTPDOC-VALOR1 TO VALOR-E
                         MOVE VALOR-E         TO FIN001-LINDET1(01:17)
                         MOVE PORTPDOC-VALOR2 TO VALOR-E
                         MOVE VALOR-E         TO FIN001-LINDET1(18:14)
                         MOVE PORTPDOC-VALOR3 TO VALOR-E
                         MOVE VALOR-E         TO FIN001-LINDET1(32:14)
                         MOVE PORTPDOC-VALOR4 TO VALOR-E
                         MOVE VALOR-E         TO FIN001-LINDET1(46:14)
                         MOVE "INSERE-LISTA1" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM

           MULTIPLY PAGAR-PREV-1SEMANA BY -1 GIVING PAGAR-PREV-1SEMANA.
           MULTIPLY PAGAR-PREV-2SEMANA BY -1 GIVING PAGAR-PREV-2SEMANA.
           MULTIPLY PAGAR-PREV-3SEMANA BY -1 GIVING PAGAR-PREV-3SEMANA.
           MULTIPLY PAGAR-PREV-4SEMANA BY -1 GIVING PAGAR-PREV-4SEMANA.
           MOVE PAGAR-PREV-1SEMANA   TO VALOR-E.
           MOVE VALOR-E              TO FIN001-LINDET1(01: 17).
           MOVE PAGAR-PREV-2SEMANA   TO VALOR-E
           MOVE VALOR-E              TO FIN001-LINDET1(18: 14).
           MOVE PAGAR-PREV-3SEMANA   TO VALOR-E.
           MOVE VALOR-E              TO FIN001-LINDET1(32: 14).
           MOVE PAGAR-PREV-4SEMANA   TO VALOR-E.
           MOVE VALOR-E              TO FIN001-LINDET1(46: 14).
           MOVE "INSERE-LISTA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MULTIPLY PAGAR-DEF-1SEMANA BY -1 GIVING PAGAR-DEF-1SEMANA.
           MULTIPLY PAGAR-DEF-2SEMANA BY -1 GIVING PAGAR-DEF-2SEMANA.
           MULTIPLY PAGAR-DEF-3SEMANA BY -1 GIVING PAGAR-DEF-3SEMANA.
           MULTIPLY PAGAR-DEF-4SEMANA BY -1 GIVING PAGAR-DEF-4SEMANA.
           MOVE PAGAR-DEF-1SEMANA    TO VALOR-E.
           MOVE VALOR-E              TO FIN001-LINDET1(01: 17).
           MOVE PAGAR-DEF-2SEMANA    TO VALOR-E
           MOVE VALOR-E              TO FIN001-LINDET1(18: 14).
           MOVE PAGAR-DEF-3SEMANA    TO VALOR-E.
           MOVE VALOR-E              TO FIN001-LINDET1(32: 14).
           MOVE PAGAR-DEF-4SEMANA    TO VALOR-E.
           MOVE VALOR-E              TO FIN001-LINDET1(46: 14).
           MOVE "INSERE-LISTA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


           MOVE SALDO-1SEMANA       TO VALOR-E.
           MOVE VALOR-E             TO FIN001-LINDET1(01: 17).
           MOVE SALDO-2SEMANA       TO VALOR-E
           MOVE VALOR-E             TO FIN001-LINDET1(18: 14).
           MOVE SALDO-3SEMANA       TO VALOR-E.
           MOVE VALOR-E             TO FIN001-LINDET1(32: 14).
           MOVE SALDO-4SEMANA       TO VALOR-E.
           MOVE VALOR-E             TO FIN001-LINDET1(46: 14).
           MOVE "INSERE-LISTA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-LISTA SECTION.
           OPEN OUTPUT PORTPDOC AUXCPD
           CLOSE       PORTPDOC AUXCPD
           OPEN I-O    PORTPDOC AUXCPD
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM ENCONTRA-DATA-INTERVALO
           PERFORM GRAVA-SALDO-CAIXA
           PERFORM LEITURA-BANCO
           PERFORM DEPOSITO-DIAW
           PERFORM PAGTO-ATRASOW
           PERFORM PAGTO-DIAW.

           MOVE SPACES         TO FIN001-LINDET
           MOVE "INSERE-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "SALDO FINAL DO DIA"  TO FIN001-LINDET
           MOVE SALDO-ACUM            TO VALOR-E
           MOVE VALOR-E               TO FIN001-LINDET(60: 20)

           MOVE "INSERE-LISTA"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SALDO-ACUM TO SALDO-1SEMANA.
           PERFORM DEPOSITO-PROGRAMADO.
           PERFORM CONTAS-A-RECEBER.
           PERFORM CONTAS-A-PAGAR.
           PERFORM SALDO-PREVISTO.
           CLOSE       PORTPDOC AUXCPD.
       CLEAR-FLAGS SECTION.
           INITIALIZE FIN001-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "FIN001"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN INPUT PORTPDOC
           MOVE ZEROS TO PAG-W SALDO-ACUM.

           COPY CONDENSA.

           PERFORM CABECALHO.
           MOVE SPACES  TO LINDET.
           MOVE "     VALOR R$" TO LINDET(40: 20).
           MOVE "     SALDO R$" TO LINDET(60: 15).
           PERFORM IMPRIME-LINHA.
           MOVE SPACES                 TO LINDET.
           MOVE "SALDO CAIXA DO DIA: " TO LINDET(01: 20).
           MOVE DATA-MOVTO-W           TO DATA-E.
           MOVE DATA-E                 TO LINDET(21: 10)
           MOVE SALDO-CAIXA            TO VALOR-E SALDO-ACUM.
           MOVE VALOR-E                TO LINDET(40: 20)
           PERFORM IMPRIME-LINHA.
           MOVE "SALDO EM BANCOS"      TO LINDET.
           WRITE REG-RELAT FROM LINDET AFTER 2.
           ADD 2 TO LIN.
           MOVE SPACES                 TO LINDET.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 40
              IF BANCO-W1(I) = ZEROS
                 MOVE 41 TO I
              ELSE
                 MOVE BANCO-W1(I) TO CODIGO-CG01
                 READ CGD001 INVALID KEY
                      MOVE SPACES TO NOME-CG01
                 END-READ
                 MOVE NOME-CG01   TO LINDET(01: 30)
                 MOVE VALOR-W1(I) TO VALOR-E
                 ADD VALOR-W1(I)  TO SALDO-ACUM
                 MOVE VALOR-E     TO LINDET(40: 20)
                 PERFORM IMPRIME-LINHA
              END-IF
           END-PERFORM.
           MOVE "SALDO"              TO LINDET(01: 80)
           MOVE SALDO-ACUM           TO VALOR-E
           MOVE VALOR-E              TO LINDET(40: 20).
           PERFORM IMPRIME-LINHA.
           MOVE "CHEQUES"            TO LINDET(01: 30)
           MOVE DEPOSITO-DIA         TO VALOR-E
           MOVE VALOR-E              TO LINDET(40: 20)
           ADD DEPOSITO-DIA          TO SALDO-ACUM
           MOVE SALDO-ACUM           TO VALOR-E
           MOVE VALOR-E              TO LINDET(60: 20)
           WRITE REG-RELAT FROM LINDET AFTER 2.
           ADD 2 TO LIN.

           IF FIN001-CONSIDERAR-VENCIDO = 1
              MOVE SPACES               TO LINDET
              MOVE "RECEBER VENCIDO"    TO LINDET
              WRITE REG-RELAT FROM LINDET
              ADD 1 TO LIN

              MOVE SPACES               TO LINDET
              WRITE REG-RELAT FROM LINDET
              ADD 1 TO LIN

              INITIALIZE REG-PORTPDOC
                         TOTAL-TIPO
              MOVE 2                    TO PORTPDOC-TIPO
              START PORTPDOC KEY IS NOT LESS PORTPDOC-CHAVE INVALID KEY
                    MOVE "10" TO ST-PORTPDOC
              END-START
              PERFORM UNTIL ST-PORTPDOC = "10"
                    READ PORTPDOC NEXT AT END
                         MOVE "10" TO ST-PORTPDOC
                    NOT AT END
                         IF PORTPDOC-TIPO <> 2
                            MOVE "10" TO ST-PORTPDOC
                         ELSE
                            INITIALIZE LINDET
                            EVALUATE PORTPDOC-TIPODOC
                                WHEN 0 MOVE "0-Boleto"             TO
                                       LINDET(01: 30)
                                WHEN 1 MOVE "1-Dupl/Promis"        TO
                                       LINDET(01: 30)
                                WHEN 2 MOVE "2-Org.Evento"         TO
                                       LINDET(01: 30)
                                WHEN 3 MOVE "3-Debito Automatico"  TO
                                       LINDET(01: 30)
                                WHEN 4 MOVE "4-Cartao de Credito"  TO
                                       LINDET(01: 30)
                            END-EVALUATE
                            MOVE PORTPDOC-VALOR1  TO VALOR-E
                            MOVE VALOR-E          TO LINDET(40: 20)
                            ADD PORTPDOC-VALOR1   TO SALDO-ACUM
                            MOVE SALDO-ACUM       TO VALOR-E
                            MOVE VALOR-E          TO LINDET(60: 20)
                            PERFORM IMPRIME-LINHA

                            ADD PORTPDOC-VALOR1   TO TOTAL-TIPO
                         END-IF
                    END-READ
              END-PERFORM

              MOVE SPACES               TO LINDET
              MOVE "TOTAL"              TO LINDET(1:30)
              MOVE TOTAL-TIPO           TO VALOR-E
              MOVE VALOR-E              TO LINDET(40:20)
              PERFORM IMPRIME-LINHA

              MOVE SPACES               TO LINDET
              PERFORM IMPRIME-LINHA
           END-IF

           MOVE "RECEBER DO DIA"           TO LINDET
           PERFORM IMPRIME-LINHA
           ADD 1 TO LIN

           MOVE SPACES               TO LINDET
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN

           INITIALIZE REG-PORTPDOC
                      TOTAL-TIPO
           MOVE 1                    TO PORTPDOC-TIPO
           START PORTPDOC KEY IS NOT LESS PORTPDOC-CHAVE INVALID KEY
                 MOVE "10" TO ST-PORTPDOC
           END-START
           PERFORM UNTIL ST-PORTPDOC = "10"
                 READ PORTPDOC NEXT AT END
                      MOVE "10" TO ST-PORTPDOC
                 NOT AT END
                      IF PORTPDOC-TIPO <> 1
                         MOVE "10" TO ST-PORTPDOC
                      ELSE
                         INITIALIZE LINDET
                         EVALUATE PORTPDOC-TIPODOC
                             WHEN 0 MOVE "0-Boleto"             TO
                                    LINDET(01: 30)
                             WHEN 1 MOVE "1-Dupl/Promis"        TO
                                    LINDET(01: 30)
                             WHEN 2 MOVE "2-Org.Evento"         TO
                                    LINDET(01: 30)
                             WHEN 3 MOVE "3-Debito Automatico"  TO
                                    LINDET(01: 30)
                             WHEN 4 MOVE "4-Cartao de Credito"  TO
                                    LINDET(01: 30)
                         END-EVALUATE
                         MOVE PORTPDOC-VALOR1  TO VALOR-E
                         MOVE VALOR-E          TO LINDET(40: 20)
                         ADD PORTPDOC-VALOR1   TO SALDO-ACUM
                         MOVE SALDO-ACUM       TO VALOR-E
                         MOVE VALOR-E          TO LINDET(60: 20)
                         PERFORM IMPRIME-LINHA

                         ADD PORTPDOC-VALOR1   TO TOTAL-TIPO

                      END-IF
                 END-READ
           END-PERFORM

           MOVE SPACES               TO LINDET
           MOVE "TOTAL"              TO LINDET(1:30)
           MOVE TOTAL-TIPO           TO VALOR-E
           MOVE VALOR-E              TO LINDET(40:20)
           PERFORM IMPRIME-LINHA

           MOVE SPACES               TO LINDET
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN

           MOVE "CONTAS A PAGAR VENCIDOS"  TO LINDET
           PERFORM IMPRIME-LINHA.

           INITIALIZE TOTAL-TIPO

           MOVE SPACES                     TO LINDET
           MOVE "    PREVISTO"             TO LINDET(01: 30)
           MOVE PAGTO-ATRASO-PREV          TO VALOR-E
           MOVE VALOR-E                    TO LINDET(40: 20)
           ADD PAGTO-ATRASO-PREV           TO SALDO-ACUM
           MOVE SALDO-ACUM                 TO VALOR-E
           MOVE VALOR-E                    TO LINDET(60: 20)
           PERFORM IMPRIME-LINHA

           MOVE SPACES                     TO LINDET
           MOVE "    DEFINITIVO"           TO LINDET(01: 30)
           MOVE PAGTO-ATRASO-DEF           TO VALOR-E
           MOVE VALOR-E                    TO LINDET(40: 20)
           ADD PAGTO-ATRASO-DEF            TO SALDO-ACUM
           MOVE SALDO-ACUM                 TO VALOR-E
           MOVE VALOR-E                    TO LINDET(60: 20)
           PERFORM IMPRIME-LINHA

           ADD PAGTO-ATRASO-PREV TO TOTAL-TIPO
           ADD PAGTO-ATRASO-DEF  TO TOTAL-TIPO

           MOVE SPACES                     TO LINDET
           MOVE "TOTAL"                    TO LINDET(1:30)
           MOVE TOTAL-TIPO                 TO VALOR-E
           MOVE VALOR-E                    TO LINDET(40:20)
           PERFORM IMPRIME-LINHA


           MOVE SPACES               TO LINDET
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN

           MOVE "CONTAS A PAGAR DO DIA"    TO LINDET
           PERFORM IMPRIME-LINHA.

           INITIALIZE TOTAL-TIPO

           MOVE SPACES                     TO LINDET
           MOVE "    PREVISTO"             TO LINDET(01: 30)
           MOVE PAGTO-DIA-PREV             TO VALOR-E
           MOVE VALOR-E                    TO LINDET(40: 20)
           ADD PAGTO-DIA-PREV              TO SALDO-ACUM
           MOVE SALDO-ACUM                 TO VALOR-E
           MOVE VALOR-E                    TO LINDET(60: 20)
           PERFORM IMPRIME-LINHA.

           MOVE SPACES                     TO LINDET
           MOVE "    DEFINITIVO"           TO LINDET(01: 30)
           MOVE PAGTO-DIA-DEF              TO VALOR-E
           MOVE VALOR-E                    TO LINDET(40: 20)
           ADD PAGTO-DIA-DEF               TO SALDO-ACUM
           MOVE SALDO-ACUM                 TO VALOR-E
           MOVE VALOR-E                    TO LINDET(60: 20)
           PERFORM IMPRIME-LINHA.

           ADD PAGTO-DIA-PREV TO TOTAL-TIPO
           ADD PAGTO-DIA-DEF  TO TOTAL-TIPO

           MOVE SPACES                     TO LINDET
           MOVE "TOTAL"                    TO LINDET(1:30)
           MOVE TOTAL-TIPO                 TO VALOR-E
           MOVE VALOR-E                    TO LINDET(40:20)
           PERFORM IMPRIME-LINHA


           MOVE SPACES                     TO LINDET.
           PERFORM IMPRIME-LINHA.

           MOVE "SALDO FINAL DO DIA"       TO LINDET(01:30)
           MOVE SALDO-ACUM                 TO VALOR-E
           MOVE VALOR-E                    TO LINDET(60:20)
           PERFORM IMPRIME-LINHA.

           MOVE SPACES                     TO LINDET.
           PERFORM IMPRIME-LINHA.
           MOVE "    1o.SEMANA  "          TO LINDET(22: 15)
           MOVE "    2o.SEMANA  "          TO LINDET(37: 15)
           MOVE "    3o.SEMANA  "          TO LINDET(52: 15)
           MOVE "    4o.SEMANA  "          TO LINDET(67: 14)
           PERFORM IMPRIME-LINHA.
           MOVE SPACES                     TO LINDET
           MOVE FIN001-DATA1               TO DIAMES-E
           MOVE DIAMES-E                   TO LINDET(22: 06)
           MOVE "a"                        TO LINDET(28: 02)
           MOVE FIN001-DATA2               TO DIAMES-E
           MOVE DIAMES-E                   TO LINDET(30: 06)
           MOVE FIN001-DATA3               TO DIAMES-E
           MOVE DIAMES-E                   TO LINDET(37: 06)
           MOVE "a"                        TO LINDET(43: 02)
           MOVE FIN001-DATA4               TO DIAMES-E
           MOVE DIAMES-E                   TO LINDET(45: 06)
           MOVE FIN001-DATA5               TO DIAMES-E
           MOVE DIAMES-E                   TO LINDET(52: 06)
           MOVE "a"                        TO LINDET(58: 02)
           MOVE FIN001-DATA6               TO DIAMES-E
           MOVE DIAMES-E                   TO LINDET(60: 06)
           MOVE FIN001-DATA7               TO DIAMES-E
           MOVE DIAMES-E                   TO LINDET(67: 06)
           MOVE "a"                        TO LINDET(73: 02)
           MOVE FIN001-DATA8               TO DIAMES-E
           MOVE DIAMES-E                   TO LINDET(75: 06)
           PERFORM IMPRIME-LINHA.
           MOVE SPACES                     TO LINDET.
           MOVE "CHEQUES"                  TO LINDET(01: 21)
           MOVE CHEQUE-1SEMANA             TO VALOR-E
           MOVE VALOR-E                    TO LINDET(22: 14)
           MOVE CHEQUE-2SEMANA             TO VALOR-E
           MOVE VALOR-E                    TO LINDET(36: 15)
           MOVE CHEQUE-3SEMANA             TO VALOR-E
           MOVE VALOR-E                    TO LINDET(51: 15)
           MOVE CHEQUE-4SEMANA             TO VALOR-E
           MOVE VALOR-E                    TO LINDET(66: 14)
           PERFORM IMPRIME-LINHA.

           INITIALIZE REG-PORTPDOC
           MOVE 3                          TO PORTPDOC-TIPO
           START PORTPDOC KEY IS NOT LESS PORTPDOC-CHAVE INVALID KEY
                 MOVE "10" TO ST-PORTPDOC.

           PERFORM UNTIL ST-PORTPDOC = "10"
                 READ PORTPDOC NEXT AT END
                      MOVE "10" TO ST-PORTPDOC
                 NOT AT END
                      IF PORTPDOC-TIPO <> 3
                         MOVE "10" TO ST-PORTPDOC
                      ELSE
                         EVALUATE PORTPDOC-TIPODOC
                             WHEN 0 MOVE "BOLETO"             TO
                                    LINDET(01: 21)
                             WHEN 1 MOVE "DUPL/PROMIS"        TO
                                    LINDET(01: 21)
                             WHEN 2 MOVE "ORG.EVENTO"         TO
                                    LINDET(01: 21)
                             WHEN 3 MOVE "DEBITO AUTOMATICO"  TO
                                    LINDET(01: 21)
                             WHEN 4 MOVE "CARTAO DE CREDITO"  TO
                                    LINDET(01: 21)
                         END-EVALUATE
                         MOVE PORTPDOC-VALOR1 TO VALOR-E
                         MOVE VALOR-E         TO LINDET(22:14)
                         MOVE PORTPDOC-VALOR2 TO VALOR-E
                         MOVE VALOR-E         TO LINDET(36: 15)
                         MOVE PORTPDOC-VALOR3 TO VALOR-E
                         MOVE VALOR-E         TO LINDET(51: 15)
                         MOVE PORTPDOC-VALOR4 TO VALOR-E
                         MOVE VALOR-E         TO LINDET(66: 14)
                         PERFORM IMPRIME-LINHA
                      END-IF
                 END-READ
           END-PERFORM


           MOVE "PAGAR PREVISTO     "      TO LINDET(01: 21)
           MOVE PAGAR-PREV-1SEMANA         TO VALOR-E
           MOVE VALOR-E                    TO LINDET(22: 14)
           MOVE PAGAR-PREV-2SEMANA         TO VALOR-E
           MOVE VALOR-E                    TO LINDET(36: 15)
           MOVE PAGAR-PREV-3SEMANA         TO VALOR-E
           MOVE VALOR-E                    TO LINDET(51: 14)
           MOVE PAGAR-PREV-4SEMANA         TO VALOR-E
           MOVE VALOR-E                    TO LINDET(66: 15)
           PERFORM IMPRIME-LINHA.


           MOVE "PAGAR REALIZADO    "      TO LINDET(01: 21)
           MOVE PAGAR-DEF-1SEMANA          TO VALOR-E
           MOVE VALOR-E                    TO LINDET(22: 14)
           MOVE PAGAR-DEF-2SEMANA          TO VALOR-E
           MOVE VALOR-E                    TO LINDET(36: 15)
           MOVE PAGAR-DEF-3SEMANA          TO VALOR-E
           MOVE VALOR-E                    TO LINDET(51: 14)
           MOVE PAGAR-DEF-4SEMANA          TO VALOR-E
           MOVE VALOR-E                    TO LINDET(66: 15)
           PERFORM IMPRIME-LINHA.


           MOVE "SALDO PREVISTO     "      TO LINDET(01: 21)
           MOVE SALDO-1SEMANA              TO VALOR-E
           MOVE VALOR-E                    TO LINDET(22: 14)
           MOVE SALDO-2SEMANA              TO VALOR-E
           MOVE VALOR-E                    TO LINDET(36: 15)
           MOVE SALDO-3SEMANA              TO VALOR-E
           MOVE VALOR-E                    TO LINDET(51: 14)
           MOVE SALDO-4SEMANA              TO VALOR-E
           MOVE VALOR-E                    TO LINDET(66: 15)
           PERFORM IMPRIME-LINHA.

           COPY DESCONDENSA.


           CLOSE        PORTPDOC.

       IMPRIME-LINHA SECTION.
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, FIN001-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD040 CXD041 CXD100 CGD001 CAD013 CPD020 CRD020
                 CHD010 CAD018 FIN001 FERIADOS.
           EXIT PROGRAM.
