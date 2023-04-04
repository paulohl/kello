       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP201.
      *DATA: 14/01/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: EXTRATO DE CONTAS CORRENTES
      *FUNÇÃO: Listar todos os lançamentos dentro do intervalo de
      *        vencto selecionado e do fornecedor solicitado,
      *        informando saldo anterior, vencidos e a vencer.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass"
           AListview          is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CCPX001.
           COPY CCPX010.
           COPY CCPX100.
           COPY CCPX101.
           COPY CPPX020.
           COPY CPPX021.
           COPY CXPX020.
           COPY CAPX004.
           COPY LOGCCD.SEL.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK = TIPO-WK VENCTO-WK
                                WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.



       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CCPW001.
       COPY CCPW010.
       COPY CCPW100.
       COPY CCPW101.
       COPY CPPW020.
       COPY CPPW021.
       COPY CXPW020.
       COPY CAPW004.
       COPY LOGCCD.FD.

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(6).
           05  TIPO-WK             PIC 9.
      *    TIPO = 1 - VENCIDOS     2- A VENCER
           05  DATA-MOVTO-WK       PIC 9(8).
           05  RESPONSAVEL-WK      PIC X(5).
           05  HISTORICO-WK        PIC X(30).
           05  VENCTO-WK           PIC 9(8).
           05  VLR-CRED-INI-WK     PIC 9(8)V99.
           05  VLR-DEB-INI-WK      PIC S9(8)V99.
           05  DIAMES-LIQ-WK       PIC 9(4).
           05  VLR-CRED-TX-WK      PIC 9(08)V99.
           05  VLR-DEB-TX-WK       PIC S9(8)V99.
           05  FORNEC-WK           PIC 9(06).
           05  SEQUEN-WK           PIC 9(05).
           05  DOCTO-WK            PIC X(10).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(155).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP201.CPB".
           COPY "CCP201.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CCD001             PIC XX       VALUE SPACES.
           05  ST-CCD010             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD101             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD021             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-LOGCCD             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
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
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9999.
               10  MES-II            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  MESANOINI.
               10 ANOINI             PIC 9(04).
               10 MESINI             PIC 9(02).
           05  MESANOFIN.
               10 ANOFIN             PIC 9(04).
               10 MESFIN             PIC 9(02).
           05  DIAINI                PIC 9(02).
           05  DIAFIN                PIC 9(02).
           05  NUM-DIAS              PIC 9(02).
           05  QTDE-TAXA             PIC 9(04).
           05  IND                   PIC 9(04).
           05  TOTAL-TAXA            PIC 9(04)V999.
           05  AUX-VALOR             PIC S9(09)V99.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  GRAVA1-REGRAVA2       PIC 9        VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  DATA-BASE             PIC 9(8)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(6)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  VALOR1                PIC 9(08)V99.
           05  VALOR2                PIC s9(08)V99.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  DIA-MES-E             PIC ZZ/ZZ.
           05  SALDO-WI              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-WT              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-GI              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-GT              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-ANT             PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-ANT-T           PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-E               PIC ZZ.ZZZ.ZZ9,99-.
           05  TIPO-ANT              PIC 9        VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           05  VALOR-AUX             PIC 9(09)V99 VALUE ZEROS.
           05  DS-DIAS               pic 9(05)    value zeros.
           05  CONTADOR              pic 9(05)    value zeros.
           05  VALORE-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE ENTRADA
           05  VALORS-W              PIC 9(8)V99  VALUE ZEROS.
           05  AUX-SITUACAO          PIC 9(01)    VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(55)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(57)   VALUE
           "EXTRATO DE CONTAS CORRENTES ".
           05  FILLER              PIC X(28)   VALUE SPACES.
           05  FILLER              PIC X(06)   VALUE "TIPO: ".
           05  TIPO-REL            PIC X(19)   VALUE SPACES.
       01  CAB02A.
           05  FILLER              PIC X(12)    VALUE "FORNECEDOR: ".
           05  NOME-REL            PIC X(50)   VALUE SPACES.
           05  FILLER              PIC X(01).
           05  FILLER              PIC X(11)   VALUE "INT.VECTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(05) VALUE " ATE ".
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB02B.
           05  FILLER              PIC X(6)    VALUE "TAXA: ".
           05  TAXA-REL            PIC Z9,99.
           05  FILLER              PIC X(03)   VALUE "%".
           05  FILLER              PIC X(11)   VALUE "DATA BASE: ".
           05  BASE-REL            PIC 99/99/9999.

       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB031.
           05  FILLER              PIC X(151)  VALUE ALL "=".
       01  CAB03A.
           05  FILLER              PIC X(110)  VALUE ALL "-".
       01  CAB03A1.
           05  FILLER              PIC X(151)  VALUE ALL "-".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-MOVTO RESP. HISTORICO          NR-DOCTO   VENCIMENTO VA
      -    "LOR-CREDITO  VALOR-DEBITO   SALDO-ATUAL  LIQUI".

       01  det-04.
           05 det04-dia-mov        pic 99/.
           05 det04-mes-mov        pic 99/.
           05 det04-ano-mov        pic 9999.
           05 filler               pic x(01).
           05 det04-resp           pic x(05).
           05 filler               pic x(01).
           05 det04-historico      pic x(17).
           05 filler               pic x(01).
           05 det04-nr-docto       pic x(10).
           05 filler               pic x(01).
           05 det04-dia-venc       pic 99/.
           05 det04-mes-venc       pic 99/.
           05 det04-ano-venc       pic 9999.
           05 filler               pic x(01).
           05 det04-vlrcred        pic zz.zzz.zz9,99 blank when zeros.
           05 filler               pic x(01).
           05 det04-vlrdeb         pic zz.zzz.zz9,99- blank when zeros.
           05 filler               pic x(01).
           05 det04-vlrsld         pic zz.zzz.zz9,99-.

       01  CAB041.
           05  FILLER              PIC X(152)  VALUE
           "DATA-MOVTO RESP. HISTORICO          NR-DOCTO   VENCIMENTO VA
      -    "LOR-CREDITO   VALOR-DEBITO   SALDO-ATUAL  VALOR-CREDITO  VAL
      -    "OR-DEBITO   SALDO-ATUAL    LIQUI".

       01  det-041.
           05 det041-dia-mov        pic 99/.
           05 det041-mes-mov        pic 99/.
           05 det041-ano-mov        pic 9999.
           05 filler                pic x(01).
           05 det041-resp           pic x(05).
           05 filler                pic x(01).
           05 det041-historico      pic x(17).
           05 filler                pic x(01).
           05 det041-nr-docto       pic x(10).
           05 filler                pic x(01).
           05 det041-dia-venc       pic 99/.
           05 det041-mes-venc       pic 99/.
           05 det041-ano-venc       pic 9999.
           05 filler                pic x(01).
           05 det041-vlrcred        pic zz.zzz.zz9,99 blank when zeros.
           05 filler                pic x(01).
           05 det041-vlrdeb         pic zz.zzz.zz9,99- blank when zeros.
           05 filler                pic x(01).
           05 det041-vlrsld         pic zz.zzz.zz9,99-.


       01  DET-LOG.
           05 DET-USUARIO          PIC X(05).
           05 FILLER               PIC X(01).
           05 DET-DIA              PIC 99/.
           05 DET-MES              PIC 99/.
           05 DET-ANO              PIC 9999.
           05 FILLER               PIC X(01).
           05 DET-HORA             PIC 99.
           05 FILLER               PIC X(01) VALUE ":".
           05 DET-MINU             PIC 99.
           05 FILLER               PIC X(01).
           05 DET-PROGRAMA         PIC X(07).

       01  LINDET.
           05  LINDET-REL          PIC X(150)  VALUE SPACES.

       01 indice                   pic 9(02).
       01 wssize                   pic 9(09) comp-5 value zeros.
       01 wsIndice                 pic 9(09) comp-5 value zeros.
       77 wsTexto                  pic x(255) value spaces.
       77 wsItem                   pic 9(009) comp-5 value zeros.

       01 lnktabela.
          02 lnkobjetoscol             object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnkusu.
          copy usuario.cpy.

           copy   "ldifdias".

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
           MOVE GS-VERSION-NO       TO DS-VERSION-NO
           MOVE EMPRESA-W           TO EMP-REC
           MOVE NOME-EMPRESA-W      TO EMPRESA-REL
           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CCD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD010.
           MOVE "CCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "CCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD101.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CPD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD021.
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "LOGCCD"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOGCCD.

           OPEN I-O   LOGCCD
           CLOSE      LOGCCD
           OPEN INPUT LOGCCD

           OPEN INPUT CGD001 CCD100 CCD010 CCD001 CXD020 CAD004.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD001 <> "00"
              MOVE "ERRO ABERTURA CCD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD010 <> "00"
              MOVE "ERRO ABERTURA CCD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOGCCD <> "00"
              MOVE "ERRO ABERTURA LOGCCD: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOGCCD TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           MOVE "SENHA32"     TO PROGRAMA-CA004

           READ CAD004 INVALID KEY
                MOVE "N" TO GS-LIBERA
           NOT INVALID KEY
                MOVE "S" TO GS-LIBERA
           END-READ

           CLOSE CAD004

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
                    PERFORM CRIAR-LISTVIEW
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GERAR-RELATORIO-TRUE
                    PERFORM GERAR-RELATORIO
                    PERFORM CARREGA-LISTA
                    PERFORM CARREGA-LISTVIEW
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
                    PERFORM CARREGA-LISTVIEW
               WHEN GS-LER-FORNEC-TRUE
                    PERFORM LER-FORNECEDOR
               WHEN GS-POPUP-FORNEC-TRUE
                    PERFORM POPUP-FORNECEDOR
               WHEN GS-LER-CONTA-REDUZ-TRUE
                    PERFORM LER-CONTA-REDUZIDA
               WHEN GS-POPUP-CONTAREDUZ-TRUE
                    PERFORM POPUP-CONTA-REDUZ
               WHEN GS-VERIFICA-ACESSO-TRUE
                    PERFORM VERIFICA-ACESSO-USUARIO
               WHEN GS-PROGRAMA-APAGAR-TRUE
                    PERFORM PROGRAMACAO-APAGAR
               WHEN GS-TRATAR-EVENTO-TRUE
                    PERFORM TRATAR-EVENTO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Data Movto" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Resp" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Histórico" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Nr-Docto" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Vencto" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Valor Crédito" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Valor Débito" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Saldo Atual" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Valor Crédito" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Valor Débito" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Saldo Atual" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Liqui" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke gs-acp-listview "gridLines"
          invoke gs-acp-listview "noBorder".

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-ccp201" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.

       mostrar-fonte-favo section.
           move "listview-ccp201" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       EXPORTAR-PARA-EXCEL section.
           invoke aListview "ExportarParaOExcel"
                    using gs-acp-listview lnkTabela.
       EXPORTAR-PARA-EXCEL-fim.
           EXIT.

       zebrar-itens section.
           move "listview-ccp201" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-acp-listview wsTexto
           invoke gs-acp-listview "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-ccp201" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.

       TRATAR-EVENTO SECTION.
           evaluate gs-acp-evento
               when 34123  perform chamar-colunas-favo
               when 34027  set-focus ef2
           end-evaluate.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       VERIFICA-ACESSO-USUARIO SECTION.
           MOVE 0 TO GS-LIBERA-ACESSO
           MOVE 0 TO TIPO-ACESSO-CC01.
           MOVE GS-CODIGO TO CODIGO-CG01.
           READ CGD001 INVALID KEY INITIALIZE REG-CGD001.

           IF T-PESFIS-CG01 = 1
              MOVE 1 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-PESJUR-CG01 = 1
              MOVE 2 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-FUNC-CG01 = 1
              MOVE 3 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-REPRES-CG01 = 1
              MOVE 4 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-FOTOG-CG01 = 1
              MOVE 5 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-CINEG-CG01 = 1
              MOVE 6 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-VEND-CG01 = 1
              MOVE 7 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-IMPOSTO-CG01 = 1
              MOVE 8 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-INVESTIDOR-CG01 = 1
              MOVE 9 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF OUTRO3-CG01= 1
              MOVE 0 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-TERCEIRIZADO-CG01 = 1
              MOVE 10 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-FRANQUIA-CG01 = 1
              MOVE 11 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

       LER-USUARIO SECTION.
           MOVE USUARIO-W          TO NOME-REDUZ-CC01.
           IF GS-LIBERA-ACESSO = 0
             READ CCD001 INVALID KEY
                  MOVE 0  TO GS-LIBERA-ACESSO
               NOT INVALID KEY
                  MOVE 1 TO GS-LIBERA-ACESSO.
       POPUP-FORNECEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-CODIGO.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME.
       POPUP-CONTA-REDUZ SECTION.
           CALL   "CXP020T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CXP020T".
           MOVE PASSAR-STRING-1(52: 5) TO GS-CTA-APUR-PROG
           PERFORM LER-CONTA-REDUZIDA.
       LER-FORNECEDOR SECTION.
           MOVE GS-CODIGO TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "*********" TO NOME-CG01.
           MOVE NOME-CG01 TO GS-NOME.
       LER-CONTA-REDUZIDA SECTION.
           MOVE GS-CTA-APUR-PROG TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY MOVE "*********" TO DESCRICAO-CX20.
           MOVE DESCRICAO-CX20 TO GS-DESCR-CTA-APUR.
       PROGRAMACAO-APAGAR SECTION.
      *    GRAVA CONTA CORRENTE
           CLOSE CCD100 CCD101. OPEN I-O CCD100 CCD101.
           MOVE GS-CODIGO     TO FORNEC-CC100
           MOVE DATA-DIA-I    TO DATA-MOVTO-CC100
           MOVE 00            TO TIPO-LCTO-CC100
      *    16 = CONTA CORRENTE
           MOVE 16            TO TIPO-FORN-CC100
           MOVE "PROGRAMAC"   TO NR-DOCTO-CC100
           MOVE DATA-DIA-I    TO DATA-EMISSAO-CC100
           MOVE GS-VENCTO-PROG TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV      TO DATA-VENCTO-CC100
           MOVE "SOMENTE PROGRAMADO - CTA.PAGAR"
                              TO DESCRICAO-CC100
           MOVE 0             TO SITUACAO-CC100
           MOVE 1             TO LIBERADO-CC100
           MOVE 0             TO TIPO-MOEDA-CC100
           MOVE 0101          TO NR-PARCELA-CC100.
           MOVE GS-CTA-APUR-PROG   TO CODREDUZ-APUR-CC100
           MOVE GS-VALOR-PROG TO VALOR-CC100
           MOVE 1             TO CRED-DEB-CC100
           MOVE ZEROS         TO DATA-PGTO-CC100
           MOVE ZEROS         TO JUROS-PAGO-CC100
           MOVE ZEROS         TO MULTA-PAGA-CC100
           MOVE ZEROS         TO DESCONTO-CC100
           MOVE ZEROS         TO VALOR-PAGO-CC100
           MOVE USUARIO-W     TO RESPONSAVEL-CC100
           MOVE USUARIO-W     TO DIGITADOR-CC100
           MOVE ZEROS         TO SEQ-CAIXA-CC100
           PERFORM ACHA-SEQ-CCD101
           MOVE SEQ-CC101     TO SEQ-CC100
           WRITE REG-CCD100 INVALID KEY
                 PERFORM ACHA-SEQ-CCD101
                 MOVE SEQ-CC101 TO SEQ-CC100
                 WRITE REG-CCD100
                 END-WRITE
           END-WRITE.
           CLOSE CCD100 CCD101. OPEN INPUT CCD100.

      *    GRAVA CONTAS A PAGAR.
           OPEN I-O CPD020 CPD021.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD021 <> "00"
              MOVE "ERRO ABERTURA CPD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           MOVE GS-VALOR-PROG    TO VALOR-TOT-CP20
           MOVE GS-VENCTO-PROG   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV         TO DATA-VENCTO-CP20
           MOVE GS-DESCRICAO-PROG TO DESCRICAO-CP20
           MOVE "49"             TO PORTADOR-CP20
      *    PORTADOR = 49 - CONTA CORRENTE
           MOVE DATA-DIA-I       TO DATA-MOVTO-CP20
           MOVE GS-CODIGO        TO FORNEC-CP20
           MOVE SEQ-CC100        TO NR-DOCTO-CP20
           MOVE 16               TO TIPO-FORN-CP20
      *    TIPO-FORN-CP20 = 16 (CONTA CORRENTE)
           MOVE DATA-DIA-I       TO DATA-EMISSAO-CP20
           MOVE GS-VENCTO-PROG   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV         TO DATA-VENCTO-CP20
           MOVE 0                TO PREV-DEF-CP20
           MOVE 0                TO SITUACAO-CP20
           MOVE 0                TO LIBERADO-CP20
           MOVE 0                TO TIPO-MOEDA-CP20
           MOVE 0101             TO NR-PARCELA-CP20.
           MOVE GS-CTA-APUR-PROG TO CODREDUZ-APUR-CP20
           MOVE GS-VALOR-PROG    TO VALOR-TOT-CP20
           MOVE ZEROS            TO JUROS-MORA-CP20 MULTA-ATRASO-CP20
                                    TAXA-APLIC-CP20 JURO-PAGO-CP20
                                    MULTA-PAGA-CP20 DESCONTO-CP20
                                    DATA-PGTO-CP20 VALOR-LIQ-CP20
                                    CONTABILIZADO-CP20
           MOVE USUARIO-W        TO RESPONSAVEL-CP20
           MOVE USUARIO-W        TO DIGITADOR-CP20
           MOVE ZEROS            TO SEQ-CAIXA-CP20
           MOVE 0                TO TIPO-CONTA-CP20
           PERFORM ACHA-SEQ-CPD021
           MOVE SEQ-CP21         TO SEQ-CP20
           MOVE ZEROS TO ST-CPD020
           PERFORM UNTIL ST-CPD020 = "10"
             WRITE REG-CPD020 INVALID KEY
               PERFORM ACHA-SEQ-CPD021
               MOVE SEQ-CP21 TO SEQ-CP20
               CONTINUE
             NOT INVALID KEY
               MOVE "10" TO ST-CPD020
           END-PERFORM.
           CLOSE CPD020 CPD021.

       ACHA-SEQ-CPD021 SECTION.
           MOVE GS-CODIGO TO FORNEC-CP21.
           READ CPD021 INVALID KEY
                  MOVE 1 TO SEQ-CP21
                  WRITE REG-CPD021
                  END-WRITE
                NOT INVALID KEY
                  ADD 1 TO SEQ-CP21
                  REWRITE REG-CPD021
                  END-REWRITE
           END-READ.
       ACHA-SEQ-CCD101 SECTION.
           MOVE GS-CODIGO TO FORNEC-CC101.
           READ CCD101 INVALID KEY
                MOVE 1 TO SEQ-CC101
                WRITE REG-CCD101
                END-WRITE
              NOT INVALID KEY
                ADD 1 TO SEQ-CC101
                REWRITE REG-CCD101
                END-REWRITE
           END-READ.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       GERAR-RELATORIO SECTION.
           CLOSE    CCD010
           OPEN I-O CCD010
           INITIALIZE REG-CCD010
           MOVE GS-CODIGO TO FORNEC-CC10
           START CCD010 KEY IS NOT LESS ALT-CC10 INVALID KEY
               MOVE "10" TO ST-CCD010.
           PERFORM UNTIL ST-CCD010 = "10"
               READ CCD010 NEXT AT END
                   MOVE "10" TO ST-CCD010
               NOT AT END
                   IF GS-CODIGO <> FORNEC-CC10
                      MOVE "10" TO ST-CCD010
                   ELSE
                      DELETE CCD010
                      END-DELETE
                      IF ST-CCD010 <> "00"
                         DISPLAY "ERRO DE EXCLUSÃO" STOP " "
                      END-IF
                   END-IF
               END-READ
           END-PERFORM
           CLOSE    CCD010
           OPEN I-O CCD010

           MOVE FUNCTION NUMVAL(GS-ACP-SITUACAO(1:1)) TO AUX-SITUACAO

           INITIALIZE REG-CCD100
           MOVE GS-CODIGO TO FORNEC-CC100
           START CCD100 KEY IS NOT LESS CHAVE-CC100 INVALID KEY
               MOVE "10" TO ST-CCD100.
           PERFORM UNTIL ST-CCD100 = "10"
               READ CCD100 NEXT AT END
                   MOVE "10" TO ST-CCD100
               NOT AT END
                   IF GS-CODIGO <> FORNEC-CC100
                      MOVE "10" TO ST-CCD100
                   ELSE
                      IF AUX-SITUACAO = SITUACAO-CC100
      *               IF SITUACAO-CC100 = 0
                         MOVE DATA-VENCTO-CC100(1: 6) TO
                              ANOMES-VCTO-CC10
                         MOVE FORNEC-CC100            TO
                              FORNEC-CC10
                         MOVE ZEROS TO VALORE-W VALORS-W
                         IF CRED-DEB-CC100 = 0
                            MOVE VALOR-CC100 TO VALORS-W
                         ELSE
                            MOVE VALOR-CC100 TO VALORE-W
                         END-IF
                         READ CCD010 INVALID KEY
                              MOVE VALORE-W TO SALDOE-CC10
                              MOVE VALORS-W TO SALDOS-CC10
                              WRITE REG-CCD010
                              END-WRITE
                         NOT INVALID KEY
                             ADD VALORE-W TO SALDOE-CC10
                             ADD VALORS-W TO SALDOS-CC10
                             REWRITE REG-CCD010
                             END-REWRITE
                         END-READ
                      END-IF
                   END-IF
               END-READ
           END-PERFORM

           CLOSE      CCD010
           OPEN INPUT CCD010

           MOVE GS-DATA-BASE TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO DATA-BASE

           CLOSE WORK.  OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE GS-VENCTO-INI TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV TO VENCTO-INI
           MOVE GS-VENCTO-FIM TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV TO VENCTO-FIM
           MOVE ZEROS TO SEQ-W
           PERFORM LER-SALDO-ANT.

           MOVE VENCTO-INI   TO DATA-VENCTO-CC100
      *    MOVE ZEROS        TO SITUACAO-CC100
           MOVE AUX-SITUACAO TO SITUACAO-CC100
           MOVE GS-CODIGO    TO FORNEC-CC100.
           MOVE ZEROS TO SEQ-W
           START CCD100 KEY IS NOT < ALT1-CC100 INVALID KEY
                 MOVE "10" TO ST-CCD100.
           INITIALIZE REG-WORK.
           PERFORM UNTIL ST-CCD100 = "10"
             READ CCD100 NEXT RECORD AT END
                  MOVE "10" TO ST-CCD100
              NOT AT END
      *        IF SITUACAO-CC100    >     0           OR
               IF SITUACAO-CC100    <>    AUX-SITUACAO OR
                  FORNEC-CC100      <>    GS-CODIGO     OR
                  DATA-VENCTO-CC100 >     VENCTO-FIM
                  MOVE "10" TO ST-CCD100
               ELSE
                  ADD 1                TO SEQ-W
                  MOVE SEQ-W           TO GS-EXIBE-CODIGO SEQ-WK
                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  IF DATA-VENCTO-CC100 < DATA-DIA-I
                     MOVE 1 TO TIPO-WK
                  ELSE
                     MOVE 2 TO TIPO-WK
                  END-IF

                  MOVE DATA-MOVTO-CC100     TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV             TO DATA-MOVTO-WK
                  MOVE RESPONSAVEL-CC100    TO RESPONSAVEL-WK
                  MOVE NR-DOCTO-CC100       TO DOCTO-WK
                  MOVE DESCRICAO-CC100      TO HISTORICO-WK
                  MOVE DATA-VENCTO-CC100    TO VENCTO-WK

                  MOVE FORNEC-CC100         TO FORNEC-WK
                  MOVE SEQ-CC100            TO SEQUEN-WK

                  MOVE VALOR-CC100          TO AUX-VALOR

                  IF DATA-BASE > 0
                     IF DATA-VENCTO-CC100 = DATA-BASE
                        IF CRED-DEB-CC100 = 0
                           MOVE VALOR-CC100      TO VLR-CRED-TX-WK
                           MOVE ZEROS            TO VLR-DEB-TX-WK
                        ELSE
                           MOVE ZEROS            TO VLR-CRED-TX-WK
                                                 VLR-DEB-TX-WK
                           SUBTRACT VALOR-CC100  FROM VLR-DEB-TX-WK
                        END-IF
                     ELSE
                        IF DATA-VENCTO-CC100 > DATA-BASE
                           CALL   "UTIDIAS" USING DATA-BASE
                                              DATA-VENCTO-CC100 DS-DIAS
                           CANCEL "UTIDIAS"

                           MOVE VALOR-CC100 TO AUX-VALOR

                           PERFORM UNTIL DS-DIAS < 30
                                COMPUTE DS-DIAS = DS-DIAS - 30
                                IF CRED-DEB-CC100 = 0
                                   COMPUTE AUX-VALOR =
                                                AUX-VALOR +
                                     (AUX-VALOR * GS-TAXA / 100)
                                ELSE
                                   COMPUTE AUX-VALOR = AUX-VALOR +
                                         (AUX-VALOR * GS-TAXA / 100)
                                END-IF
                           END-PERFORM

                           COMPUTE VALOR-AUX = AUX-VALOR * GS-TAXA / 100

                           COMPUTE VALOR-AUX = VALOR-AUX / 30

                           COMPUTE VALOR-AUX = VALOR-AUX * DS-DIAS

                           ADD VALOR-AUX     TO AUX-VALOR

                           IF CRED-DEB-CC100 = 1
                              SUBTRACT AUX-VALOR  FROM VLR-DEB-TX-WK
                              MOVE ZEROS          TO VLR-CRED-TX-WK
                           ELSE
                              MOVE AUX-VALOR      TO VLR-CRED-TX-WK
                              MOVE ZEROS          TO VLR-DEB-TX-WK
                           END-IF

                        ELSE
                           CALL   "UTIDIAS" USING DATA-VENCTO-CC100
                                                  DATA-BASE DS-DIAS
                           CANCEL "UTIDIAS"

                           MOVE VALOR-CC100 TO AUX-VALOR

                           PERFORM UNTIL DS-DIAS < 30
                                COMPUTE DS-DIAS = DS-DIAS - 30
                                IF CRED-DEB-CC100 = 0
                                   COMPUTE AUX-VALOR =
                                                AUX-VALOR +
                                     (AUX-VALOR * GS-TAXA / 100)
                                ELSE
                                   COMPUTE AUX-VALOR = AUX-VALOR +
                                         (AUX-VALOR * GS-TAXA / 100)
                                END-IF
                           END-PERFORM

                           COMPUTE VALOR-AUX = AUX-VALOR * GS-TAXA / 100

                           COMPUTE VALOR-AUX = VALOR-AUX / 30

                           COMPUTE VALOR-AUX = VALOR-AUX * DS-DIAS

                           ADD VALOR-AUX     TO AUX-VALOR

                           IF CRED-DEB-CC100 = 1
                              COMPUTE AUX-VALOR = AUX-VALOR * (-1)
                              MOVE AUX-VALOR      TO VLR-DEB-TX-WK
                              MOVE ZEROS          TO VLR-CRED-TX-WK
                           ELSE
                              MOVE AUX-VALOR      TO VLR-CRED-TX-WK
                              MOVE ZEROS          TO VLR-DEB-TX-WK
                           END-IF
                        END-IF
                     END-IF
                  ELSE
                    MOVE ZEROS             TO VLR-CRED-TX-WK
                                              VLR-DEB-TX-WK
                  END-IF

                  IF CRED-DEB-CC100 = 0
                     MOVE VALOR-CC100      TO VLR-CRED-INI-WK
                     MOVE ZEROS            TO VLR-DEB-INI-WK
                  ELSE
                     MOVE ZEROS            TO VLR-CRED-INI-WK
                                              VLR-DEB-INI-WK
                     SUBTRACT VALOR-CC100  FROM VLR-DEB-INI-WK
                  END-IF

                  MOVE DATA-PGTO-CC100(5: 2) TO DIAMES-LIQ-WK(3: 2)
                  MOVE DATA-PGTO-CC100(7: 2) TO DIAMES-LIQ-WK(1: 2)
                  WRITE REG-WORK
                  END-WRITE
               END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


       LER-SALDO-ANT SECTION.
           MOVE VENCTO-INI(1: 6) TO MESANO-I.
           IF MES-II = 01
              MOVE 12 TO MES-II
              SUBTRACT 1 FROM ANO-II
           ELSE
              SUBTRACT 1 FROM MES-II.

           MOVE ZEROS     TO ANOMES-VCTO-CC10.
           MOVE GS-CODIGO TO FORNEC-CC10.
           MOVE ZEROS TO SALDO-ANT SALDO-ANT-T.
      *    LEITURA DO ARQUIVO DE SALDOS O TOTAL DE TODOS OS MESES
      *    ANTERIORES
           START CCD010 KEY IS NOT < ALT-CC10 INVALID KEY
                 MOVE "10" TO ST-CCD010.
           PERFORM UNTIL ST-CCD010 = "10"
             READ CCD010 NEXT RECORD AT END
                  MOVE "10" TO ST-CCD010
             NOT AT END
               IF ANOMES-VCTO-CC10 > MESANO-I OR
                  FORNEC-CC10 <> GS-CODIGO MOVE "10" TO ST-CCD010
               ELSE
                  ADD 1                TO SEQ-W
                  MOVE SEQ-W           TO GS-EXIBE-CODIGO
                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  ADD SALDOS-CC10 TO SALDO-ANT
                  SUBTRACT SALDOE-CC10 FROM SALDO-ANT
               END-IF
             END-READ
           END-PERFORM.

           MOVE VENCTO-INI(1: 6) TO DATA-VENCTO-CC100(1: 6)
           MOVE 01               TO DATA-VENCTO-CC100(7: 2)
           MOVE AUX-SITUACAO     TO SITUACAO-CC100
      *    MOVE ZEROS            TO SITUACAO-CC100
           MOVE GS-CODIGO        TO FORNEC-CC100.
           START CCD100 KEY IS NOT < ALT1-CC100 INVALID KEY
                 MOVE "10" TO ST-CCD100.
           PERFORM UNTIL ST-CCD100 = "10"
             READ CCD100 NEXT RECORD AT END MOVE "10" TO ST-CCD100
               NOT AT END
                 IF FORNEC-CC100 NOT = GS-CODIGO OR
                    DATA-VENCTO-CC100 NOT < VENCTO-INI OR
                    SITUACAO-CC100 > AUX-SITUACAO
      *             SITUACAO-CC100 > 0
                    MOVE "10" TO ST-CCD100
                 ELSE
                   ADD 1                TO SEQ-W
                   MOVE SEQ-W           TO GS-EXIBE-CODIGO
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   IF CRED-DEB-CC100 = 0
                      ADD      VALOR-CC100 TO   SALDO-ANT
                   ELSE
                      SUBTRACT VALOR-CC100 FROM SALDO-ANT
                   END-IF

                 END-IF
             END-READ
           END-PERFORM

           IF DATA-BASE > 0
               MOVE ZEROS            TO DATA-VENCTO-CC100
               MOVE AUX-SITUACAO     TO SITUACAO-CC100
      *        MOVE ZEROS            TO SITUACAO-CC100
               MOVE GS-CODIGO        TO FORNEC-CC100.
               START CCD100 KEY IS NOT < ALT1-CC100 INVALID KEY
                     MOVE "10" TO ST-CCD100.
               PERFORM UNTIL ST-CCD100 = "10"
                 READ CCD100 NEXT RECORD AT END
                      MOVE "10" TO ST-CCD100
                 NOT AT END
                      IF FORNEC-CC100 NOT = GS-CODIGO OR
                         DATA-VENCTO-CC100 > VENCTO-INI OR
                         DATA-VENCTO-CC100 = VENCTO-INI OR
                         SITUACAO-CC100 > AUX-SITUACAO
      *                  SITUACAO-CC100 > 0
                         MOVE "10" TO ST-CCD100
                      ELSE
                         ADD 1                TO SEQ-W
                         MOVE SEQ-W           TO GS-EXIBE-CODIGO
                         MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM

                         CALL   "UTIDIAS" USING DATA-VENCTO-CC100
                                                DATA-BASE DS-DIAS
                         CANCEL "UTIDIAS"

                          MOVE VALOR-CC100 TO AUX-VALOR

                          PERFORM UNTIL DS-DIAS < 30
                               COMPUTE DS-DIAS = DS-DIAS - 30
                               IF CRED-DEB-CC100 = 0
                                  COMPUTE AUX-VALOR =
                                               AUX-VALOR +
                                    (AUX-VALOR * GS-TAXA / 100)
                               ELSE
                                  COMPUTE AUX-VALOR = AUX-VALOR +
                                        (AUX-VALOR * GS-TAXA / 100)
                               END-IF
                          END-PERFORM

                          COMPUTE VALOR-AUX = AUX-VALOR * GS-TAXA / 100

                          COMPUTE VALOR-AUX = VALOR-AUX / 30

                          COMPUTE VALOR-AUX = VALOR-AUX * DS-DIAS

                          ADD VALOR-AUX TO AUX-VALOR

                          IF CRED-DEB-CC100 = 1
                             COMPUTE AUX-VALOR = AUX-VALOR * (-1)
                          END-IF

                          ADD  AUX-VALOR      TO SALDO-ANT-T

                      END-IF
                 END-READ
               END-PERFORM.

       CARREGA-LISTVIEW SECTION.
      *    display "aqui " stop " "
           invoke gs-acp-listview "deleteall"

           MOVE ZEROS               TO SALDO-WT
                                       SALDO-GT

           MOVE ZEROS               TO TIPO-WK
                                       VENCTO-WK
                                       VALOR1
                                       VALOR2

           START WORK KEY IS NOT < ALT-WK INVALID KEY
                 MOVE "10" TO ST-WORK
           END-START

           MOVE 3                   TO TIPO-ANT

           initialize indice
           invoke gs-acp-listview "adicionarItem" returning wsItem

           move 3                   to indice
           initialize wsTexto
           string "SALDO ANTERIOR . . ." X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           move saldo-ant           to saldo-gi
                                       saldo-e


           move 8                   to indice
           initialize wsTexto
           string saldo-e x"00"   into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           move saldo-ant-t         to saldo-gt
                                       saldo-e

           move 11                  to indice
           initialize wsTexto
           string saldo-e x"00"   into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           move zeros               to saldo-wi
                                       saldo-wt

           move saldo-ant           to saldo-wi
           move saldo-ant-t         to saldo-wt


           perform until st-work = "10"
              read work next record at end
                   move "10" to st-work
              not at end
      *       display "reg-wok = " reg-work stop " "
                   if tipo-wk not = tipo-ant
                      initialize indice
                      invoke gs-acp-listview "adicionarItem"
                                   returning wsItem

                      evaluate tipo-wk
      *                  WHEN 0 MOVE "LIQUIDADOS"  TO GS-LINDET
                         when 1 move "VENCIDOS  "  to gs-lindet
                         when 2 move "À VENCER  "  to gs-lindet
                      end-evaluate

                      initialize indice
                      invoke gs-acp-listview "adicionarItem"
                                   returning wsItem

                      move 1                   to indice
                      initialize wsTexto
                      string gs-lindet X"00" delimited by "  "
                        into wsTexto
                      invoke gs-acp-listview "preencherColunaZ"
                       using wsItem lnkcolunas(indice) wsTexto

                      initialize indice
                      invoke gs-acp-listview "adicionarItem"
                                   returning wsItem

                      move tipo-wk     to tipo-ant
                   end-if

                   initialize indice
                   invoke gs-acp-listview "adicionarItem"
                                returning wsItem

      *>Data de movimento
                   move 1                   to indice
                   initialize wsTexto
                   move data-movto-wk       to data-e
                   string data-e X"00" delimited by "  " into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto

      *>Reponsável
                   add  1                   to indice
                   initialize wsTexto
                   string responsavel-wk x"00" delimited by "   "
                                          into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto
      *>Histórico
                   add  1                   to indice
                   initialize wsTexto
                   string historico-wk x"00" delimited by "   "
                                          into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto
      *>Nr Documento
                   add  1                   to indice
                   initialize wsTexto
                   string docto-wk x"00" delimited by "   "
                                          into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto
      *>Vencimento
                   move vencto-wk           to data-inv
                   call "GRIDAT1" using data-inv
                   move data-inv            to data-e

                   add  1                   to indice
                   initialize wsTexto
                   string data-e x"00" delimited by "   "
                                          into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto
      *>Valor Crédito
                   add  1                   to indice
                   initialize wsTexto
                   add  vlr-cred-ini-wk     to valor1
                   move vlr-cred-ini-wk     to valor-e
                   string valor-e x"00" into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto
      *>Valor Débito
                   add  1                   to indice
                   initialize wsTexto
                   move vlr-deb-ini-wk      to valor-e1
                   add  vlr-deb-ini-wk      to valor2
                   string valor-e1 x"00"  into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto

                   add vlr-cred-ini-wk to saldo-wi saldo-gi
                   add vlr-deb-ini-wk  to saldo-wi saldo-gi
      *>Saldo
                   add  1                   to indice
                   initialize wsTexto
                   move saldo-wi            to saldo-e
                   string saldo-e x"00" into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto
      *>Valor Crédito Taxa
                   add  1                   to indice
                   initialize wsTexto
                   move vlr-cred-tx-wk      to valor-e
                   string valor-e x"00" into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto
      *>Valor Débito Taxa
                   add  1                   to indice
                   initialize wsTexto
                   move vlr-deb-tx-wk       to valor-e1
                   string valor-e1 x"00"  into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto

                   add vlr-cred-tx-wk       to saldo-wt saldo-gt
                   add vlr-deb-tx-wk        to saldo-wt saldo-gt
      *>Saldo
                   add  1                   to indice
                   initialize wsTexto
                   move saldo-wt            to saldo-e
                   string saldo-e x"00" into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto
      *>Liquidação
                   add  1                   to indice
                   initialize wsTexto
                   move diames-liq-wk       to dia-mes-e
                   string dia-mes-e x"00" into wsTexto
                   invoke gs-acp-listview "preencherColunaZ"
                    using wsItem lnkcolunas(indice) wsTexto
              END-READ
           END-PERFORM.

           initialize indice
           invoke gs-acp-listview "adicionarItem" returning wsItem

           initialize indice
           invoke gs-acp-listview "adicionarItem" returning wsItem

           move 3                   to indice
           initialize wsTexto
           string "SOMA . . ." X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           move 6                   to indice
           initialize wsTexto
           move valor1              to valor-e
           string valor-e X"00"   into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           move 7                   to indice
           initialize wsTexto
           move valor2              to valor-e1
           string valor-e1 X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           initialize indice
           invoke gs-acp-listview "adicionarItem" returning wsItem

           initialize indice
           invoke gs-acp-listview "adicionarItem" returning wsItem

           move 3                   to indice
           initialize wsTexto
           string "SALDO . . ." X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           move 8                   to indice
           initialize wsTexto
           move saldo-gi            to saldo-e
           string saldo-e X"00"   into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           move 11                  to indice
           initialize wsTexto
           move saldo-gt            to saldo-e
           string saldo-e X"00"   into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.


       CARREGA-LISTA SECTION.
           MOVE ZEROS               TO SALDO-WT
                                       SALDO-GT

           MOVE "CLEAR-LIST-BOX"    TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES              TO GS-LINDET.
           MOVE "REFRESH-DATA"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS               TO TIPO-WK VENCTO-WK VALOR1 VALOR2.
           START WORK KEY IS NOT < ALT-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           MOVE 3                   TO TIPO-ANT

           MOVE SALDO-ANT           TO SALDO-GI SALDO-E
           MOVE "SALDO ANTERIOR..." TO GS-LINDET(1: 25)
           MOVE SALDO-E             TO GS-LINDET(96: 14)
           MOVE SALDO-ANT-T         TO SALDO-GT SALDO-E
           MOVE SALDO-E             TO GS-LINDET(141: 14)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           move zeros to saldo-wi saldo-wt

           MOVE SALDO-ANT   TO SALDO-WI
           MOVE SALDO-ANT-T TO SALDO-WT


           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   IF TIPO-WK NOT = TIPO-ANT
                      PERFORM LINHA-BRANCO
                      EVALUATE TIPO-WK
      *                  WHEN 0 MOVE "LIQUIDADOS"  TO GS-LINDET
                         WHEN 1 MOVE "VENCIDOS  "  TO GS-LINDET
                         WHEN 2 MOVE "À VENCER  "  TO GS-LINDET
                      END-EVALUATE
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      PERFORM LINHA-BRANCO
      *               IF TIPO-WK <> 2
      *                  IF SALDO-WI = 0
      *                     MOVE SALDO-ANT   TO SALDO-WI
      *                  END-IF
      *                  IF SALDO-WT = 0
      *                     MOVE SALDO-ANT-T TO SALDO-WT
      *                  END-IF
      *               END-IF
                      MOVE TIPO-WK     TO TIPO-ANT

                      IF DATA-BASE > 0
                         MOVE ALL "-" TO GS-LINDET(1:166)
                      ELSE
                         MOVE ALL "-" TO GS-LINDET(1:112)
                      END-IF

                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      MOVE SPACES TO GS-LINDET


                      MOVE "DATA-MOVTO RESP. HISTORICO" TO GS-LINDET
                      MOVE "NR-DOCTO   VENCIMENTO  VLR-CREDITO     VLR-D
      -                    "EBITO   SALDO ATUAL" TO GS-LINDET(49:64)
                      IF DATA-BASE > 0
                         MOVE "VLR-CREDITO     VLR-DEBITO  SALDO ATUAL"
      *                  TO GS-LINDET(105:38)
                         TO GS-LINDET(118:43)
                         MOVE "LIQUID" TO GS-LINDET(161:5)
                      ELSE
                         MOVE "LIQUID" TO GS-LINDET(107:5)
                      END-IF

                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF DATA-BASE > 0
                         MOVE ALL "-" TO GS-LINDET(1:166)
                      ELSE
                         MOVE ALL "-" TO GS-LINDET(1:112)
                      END-IF
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      MOVE SPACES TO GS-LINDET
                   END-IF
                   MOVE SPACES            TO GS-LINDET
                   MOVE DATA-MOVTO-WK     TO DATA-E
                   MOVE DATA-E            TO GS-LINDET(1: 11)
                   MOVE RESPONSAVEL-WK    TO GS-LINDET(12: 6)
                   MOVE HISTORICO-WK      TO GS-LINDET(18: 31)
                   MOVE DOCTO-WK          TO GS-LINDET(49:11)
                   MOVE VENCTO-WK         TO DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV          TO DATA-E
                   MOVE DATA-E            TO GS-LINDET(60: 11)
                   MOVE VLR-CRED-INI-WK   TO VALOR-E
                   ADD  VLR-CRED-INI-WK   TO VALOR1
                   MOVE VALOR-E           TO GS-LINDET(70: 14)
                   MOVE VLR-DEB-INI-WK    TO VALOR-E1
      *            COMPUTE VALOR2 = VALOR2 - VLR-DEB-INI-WK
                   ADD  VLR-DEB-INI-WK    TO VALOR2
                   MOVE VALOR-E1          TO GS-LINDET(84: 15)
      *            IF DIAMES-LIQ-WK = ZEROS
                      ADD VLR-CRED-INI-WK TO SALDO-WI SALDO-GI
                      ADD VLR-DEB-INI-WK  TO SALDO-WI SALDO-GI
      *            END-IF
                   MOVE SALDO-WI          TO SALDO-E
                   MOVE SALDO-E           TO GS-LINDET(99: 14)

                   MOVE VLR-CRED-TX-WK    TO VALOR-E
                   MOVE VALOR-E           TO GS-LINDET(116:14)
                   MOVE VLR-DEB-TX-WK     TO VALOR-E1
                   MOVE VALOR-E1          TO GS-LINDET(130:14)
      *            IF DIAMES-LIQ-WK = ZEROS
                      ADD VLR-CRED-TX-WK  TO SALDO-WT SALDO-GT
                      ADD VLR-DEB-TX-WK   TO SALDO-WT SALDO-GT
      *            END-IF
                   MOVE SALDO-WT           TO SALDO-E
                   MOVE SALDO-E            TO GS-LINDET(144: 14)

                   MOVE DIAMES-LIQ-WK     TO DIA-MES-E
                   MOVE DIA-MES-E         TO GS-LINDET(158: 5)

                   MOVE SEQUEN-WK         TO GS-LINDET(174:5)

                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM

                   MOVE FORNEC-WK             TO LOGCCD-FORNEC
                   MOVE SEQUEN-WK             TO LOGCCD-SEQ
                   READ LOGCCD NOT INVALID KEY
                        MOVE LOGCCD-USUARIO   TO DET-USUARIO
                        MOVE LOGCCD-DIA       TO DET-DIA
                        MOVE LOGCCD-MES       TO DET-MES
                        MOVE LOGCCD-ANO       TO DET-ANO
                        MOVE LOGCCD-HORA(1:2) TO DET-HORA
                        MOVE LOGCCD-HORA(3:2) TO DET-MINU
                        MOVE LOGCCD-PROGRAMA  TO DET-PROGRAMA
                        MOVE DET-LOG          TO GS-LINDET
                        MOVE "INSERE-LIST" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                        PERFORM LINHA-BRANCO
                   END-READ
              END-READ
           END-PERFORM.
           PERFORM LINHA-BRANCO
           MOVE "SOMA...." TO GS-LINDET(1:20)
           MOVE VALOR1     TO VALOR-E
           MOVE VALOR-E    TO GS-LINDET(70:14)
           MOVE VALOR2     TO VALOR-E1
           MOVE VALOR-E1   TO GS-LINDET(84:14)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM LINHA-BRANCO

           MOVE "SALDO..." TO GS-LINDET(1: 20)
           MOVE SALDO-GI   TO SALDO-E
           MOVE SALDO-E    TO GS-LINDET(99: 14)
           MOVE SALDO-GT   TO SALDO-E
           MOVE SALDO-E    TO GS-LINDET(144: 14)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       LINHA-BRANCO SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP201" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO SALDO-WT SALDO-GT
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.

           MOVE ZEROS TO VENCTO-WK TIPO-WK VALOR1 VALOR2
                         SALDO-WI SALDO-WT
           MOVE SALDO-ANT TO SALDO-GI SALDO-E
           MOVE SPACES TO LINDET-REL.
           MOVE "SALDO ANTERIOR..." TO LINDET-REL(1: 20)
      *    MOVE SALDO-E    TO LINDET-REL(75: 14)
           MOVE SALDO-E    TO LINDET-REL(87: 14)

           IF GS-TAXA = 0
              MOVE ZEROS      TO SALDO-GT SALDO-E
              MOVE SALDO-E    TO LINDET-REL(118: 14)
           ELSE
              MOVE SALDO-ANT-T  TO SALDO-GT SALDO-E
              MOVE SALDO-E      TO LINDET-REL(118: 14).

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.


           MOVE SALDO-ANT   TO SALDO-WI
           MOVE SALDO-ANT-T TO SALDO-WT

           MOVE 3     TO TIPO-ANT
           START WORK KEY IS NOT < ALT-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF TIPO-WK NOT = TIPO-ANT
                   MOVE SPACES TO GS-LINDET
                   EVALUATE TIPO-WK
      *              WHEN 0 MOVE "LIQUIDADOS"  TO LINDET-REL
                     WHEN 1 MOVE "VENCIDOS  "  TO LINDET-REL
                     WHEN 2 MOVE "À VENCER  "  TO LINDET-REL
                   END-EVALUATE
                   WRITE REG-RELAT FROM LINDET AFTER 2
                   END-WRITE
                   ADD 2 TO LIN
                   IF LIN > 56
                      PERFORM CABECALHO
                   END-IF
                   MOVE TIPO-WK TO TIPO-ANT

      *            IF TIPO-WK <> 2
      *               IF SALDO-WI = 0
      *                  MOVE SALDO-ANT   TO SALDO-WI
      *               END-IF
      *               IF SALDO-WT = 0
      *                  MOVE SALDO-ANT-T TO SALDO-WT
      *               END-IF
      *            END-IF

                END-IF
                MOVE SPACES            TO LINDET-REL
                MOVE DATA-MOVTO-WK     TO DATA-E
                MOVE DATA-E            TO LINDET-REL(1: 11)
                MOVE RESPONSAVEL-WK    TO LINDET-REL(12: 5)
                MOVE HISTORICO-WK      TO LINDET-REL(18: 18)
                MOVE DOCTO-WK          TO LINDET-REL(37:11)
                MOVE VENCTO-WK         TO DATA-INV
                CALL "GRIDAT1" USING DATA-INV
                MOVE DATA-INV          TO DATA-E
                MOVE DATA-E            TO LINDET-REL(48: 11)
                MOVE VLR-CRED-INI-WK   TO VALOR-E
                ADD  VLR-CRED-INI-WK   TO VALOR1
                MOVE VALOR-E           TO LINDET-REL(59: 14)
                MOVE VLR-DEB-INI-WK    TO VALOR-E1
                ADD  VLR-DEB-INI-WK    TO VALOR2
                MOVE VALOR-E1          TO LINDET-REL(73: 14)
      *         IF DIAMES-LIQ-WK = ZEROS
                   ADD VLR-CRED-INI-WK TO SALDO-WI SALDO-GI
                   ADD VLR-DEB-INI-WK  TO SALDO-WI SALDO-GI
      *         END-IF
                MOVE SALDO-WI          TO SALDO-E
                MOVE SALDO-E           TO LINDET-REL(87: 14)

                IF DATA-BASE > 0
                   MOVE VLR-CRED-TX-WK    TO VALOR-E
                   MOVE VALOR-E           TO LINDET-REL(101:14)
                   MOVE VLR-DEB-TX-WK     TO VALOR-E1
                   MOVE VALOR-E1          TO LINDET-REL(115:14)
      *            IF DIAMES-LIQ-WK = ZEROS
                      ADD VLR-CRED-TX-WK  TO SALDO-WT SALDO-GT
                      ADD VLR-DEB-TX-WK   TO SALDO-WT SALDO-GT
      *            END-IF
                   MOVE SALDO-WT           TO SALDO-E
                   MOVE SALDO-E            TO LINDET-REL(129: 14)

                   MOVE DIAMES-LIQ-WK     TO DIA-MES-E
                   MOVE DIA-MES-E         TO LINDET-REL(143: 5)
                ELSE
                   MOVE DIAMES-LIQ-WK     TO DIA-MES-E
                   MOVE DIA-MES-E         TO LINDET-REL(102: 5)
                END-IF

                WRITE REG-RELAT FROM LINDET-REL
                END-WRITE

                ADD 1 TO LIN
                IF LIN > 56
                   PERFORM CABECALHO
                END-IF

                MOVE FORNEC-WK             TO LOGCCD-FORNEC
                MOVE SEQUEN-WK             TO LOGCCD-SEQ
                READ LOGCCD NOT INVALID KEY
                     MOVE LOGCCD-USUARIO   TO DET-USUARIO
                     MOVE LOGCCD-DIA       TO DET-DIA
                     MOVE LOGCCD-MES       TO DET-MES
                     MOVE LOGCCD-ANO       TO DET-ANO
                     MOVE LOGCCD-HORA(1:2) TO DET-HORA
                     MOVE LOGCCD-HORA(3:2) TO DET-MINU
                     MOVE LOGCCD-PROGRAMA  TO DET-PROGRAMA
                     MOVE DET-LOG          TO LINDET-REL
                     WRITE REG-RELAT FROM LINDET-REL
                     ADD 1 TO LIN
                     IF LIN > 56
                        PERFORM CABECALHO
                     END-IF
                END-READ
              END-READ
           END-PERFORM.

           MOVE SPACES TO LINDET-REL
           WRITE REG-RELAT FROM LINDET-REL

           MOVE "SOMA...." TO LINDET-REL(1:20)
           MOVE VALOR1     TO VALOR-E
           MOVE VALOR-E    TO LINDET-REL(59:14)
           MOVE VALOR2     TO VALOR-E1
           MOVE VALOR-E1   TO LINDET-REL(73:14)

           WRITE REG-RELAT FROM LINDET-REL



           IF DATA-BASE > 0
              WRITE REG-RELAT FROM CAB03A1
              MOVE SPACES TO LINDET-REL
              MOVE "SALDO..." TO LINDET-REL(1: 20)
              MOVE SALDO-GI   TO SALDO-E
              MOVE SALDO-E    TO LINDET-REL(87: 14)
              MOVE SALDO-GT   TO SALDO-E
              MOVE SALDO-E    TO LINDET-REL(129: 14)
              WRITE REG-RELAT FROM LINDET
           ELSE
              WRITE REG-RELAT FROM CAB03A
              MOVE SPACES TO LINDET-REL
              MOVE "SALDO..." TO LINDET-REL(1: 20)
              MOVE SALDO-GI   TO SALDO-E
              MOVE SALDO-E    TO LINDET-REL(87: 14)
              MOVE ZEROS      TO SALDO-E
              MOVE SALDO-E    TO LINDET-REL(129: 14)
              WRITE REG-RELAT FROM LINDET.


           copy descondensa.

       CABECALHO SECTION.
           MOVE GS-TIPO-LCTO  TO TIPO-REL.
           MOVE SPACES TO NOME-REL
           STRING GS-CODIGO "-" GS-NOME INTO NOME-REL
           MOVE GS-VENCTO-INI TO VENCTO-INI-REL
           MOVE GS-VENCTO-FIM TO VENCTO-FIM-REL
           MOVE GS-TAXA       TO TAXA-REL
           MOVE GS-DATA-BASE  TO BASE-REL
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB02A.
           WRITE REG-RELAT FROM CAB02B.
           IF DATA-BASE > 0
              WRITE REG-RELAT FROM CAB031
              WRITE REG-RELAT FROM CAB041
              WRITE REG-RELAT FROM CAB031
           ELSE
              WRITE REG-RELAT FROM CAB03
              WRITE REG-RELAT FROM CAB04
              WRITE REG-RELAT FROM CAB03
           END-IF
           MOVE 8 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CCD010 CCD100 CGD001 WORK CCD001 CXD020 LOGCCD
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
