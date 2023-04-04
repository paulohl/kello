       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP204.
      *DATA: 14/04/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELATORIO DE REPORTAGEM ANALÍTICO
      *        Listar todos as reportagens que estiverem
      *        dentro do intervalo de reportagem solicitado.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX004.
           COPY CAPX012.
           COPY COPX001.
           COPY COPX003.
           COPY COPX040.
           COPY REPX100.
           COPY REPX101.
           COPY REPX204.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS DATA-REPORT-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DOCUMENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK = EVENTO-WK CIDADE-WK
                                   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CONTRATO-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW004.
       COPY CAPW012.
       COPY COPW001.
       COPY COPW003.
       COPY COPW040.
       COPY REPW100.
       COPY REPW101.
       COPY REPW204.

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(5).
           05  DOCUMENTO-WK        PIC 9(6).
           05  CONTRATO-WK         PIC 9(4).
           05  DATA-REPORT-WK      PIC 9(8).
           05  DATA-VENDA-WK       PIC 9(8).
           05  FORMANDO-WK         PIC 9(4).
           05  PARTICIPANTE-WK     PIC 9(4).
           05  QT-EQUIPE-WK        PIC 99V9.
           05  EVENTO-WK           PIC X(10).
           05  CIDADE-WK           PIC X(11).
           05  PADRAO-WK           PIC X.
           05  QT-FILME-WK         PIC 9(3)V99.
           05  QT-FITA-WK          PIC 9(3)V99.
           05  VLR-REPORT-WK       PIC 9(8)V99.
           05  VLR-DESPESA-WK      PIC 9(8)V99.
           05  DIAS-WK             PIC 9(6).
           05  TAXA-WK             PIC 9(6)V9(8).
           05  PM-WK               PIC 9(4)V99.
           05  JUROS-WK            PIC 9(8)V99.
           05  CUSTO-CORRIG-WK     PIC 9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(140).
       WORKING-STORAGE SECTION.
           COPY "REP204.CPB".
           COPY "REP204.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-RED204             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  DESPESA-W             PIC 9(8)V99  VALUE ZEROS.
      *    GRAVA-W CONTROLA SE GRAVA P/ WORK - ORDEM CIDADE / REGIAO
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZ,ZZ.
           05  QTDE-E1               PIC Z.ZZZ,ZZ.
           05  QTDE-E2               PIC ZZ.ZZZ,ZZ.
           05  QTDE-E3               PIC ZZZ.ZZZ.
           05  QTDE-E4               PIC ZZ,Z.
           05  TOT-PARTICIPANTE      PIC 9(6)     VALUE ZEROS.
      *    CALCULO PM E JUROS
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(6)V9(8) VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  PM-E                  PIC ZZZ,ZZ   BLANK WHEN ZEROS.
           05  DIAS-E                PIC ZZZ.ZZ9  BLANK WHEN ZEROS.
           05  TAXA-E                PIC ZZZ.ZZ9,99999999
                                                  BLANK WHEN ZEROS.
           05  TOT-GER-FITA          PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-FILME         PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-EQUIPE        PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-FORMANDO      PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-PARTICIPANTE  PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-VLR-REPORT    PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-VLR-JUROS     PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-VLR-CORRIG    PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-VLR-DESPESA   PIC 9(8)V99  VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
           05  ACHEI                 PIC X(01)    VALUE SPACES.

           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(95)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RELATORIO DE REPORTAGEM     ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(45)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB02A.
           05  FILLER              PIC X(08)   VALUE "CIDADE: ".
           05  CIDADE-REL          PIC ZZZZ    BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-CID-REL        PIC X(30)   VALUE ZEROS.
       01  CAB02B.
           05  FILLER              PIC X(08)   VALUE "REGIAO: ".
           05  REGIAO-REL          PIC ZZZZ    BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-REG-REL        PIC X(30)   VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(140)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(140)  VALUE
           "DATA-REPOR  DOCTO CONT CIDADE      P FORM EVENTO     PART EQ
      -    "U. Q-FITA Q-FILM VLR-REPORT VLR-DESPES  VLR-TOTAL      JUROS
      -    "   P.M. VLR-CORRIG".
       01  LINDET.
           05  LINDET-REL          PIC X(140)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(122)  VALUE
           "QT.FORM   QT.PART   QT.EQUIPE   QT.FITA  QT-FILME   VLR-TOT-
      -    "REPORT  VLR-TOT-DESPESA   TOTAL-GERAL      JUROS TOTAL-CORRI
      -    "G.".
       01  LINTOT.
           05  LINTOT-REL          PIC X(122)  VALUE SPACES.

           copy impressora.


       01 lnktabela.
          02 lnkobjetoscol object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas pic 9(09) comp-5 value zeros occurs 99 times.

       01 indice                       pic 9(02).

       01 wssize                       pic 9(09) comp-5 value zeros.
       01 wsIndice                     pic 9(09) comp-5 value zeros.
       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 umitem                       object reference.
       77 umobjeto                     object reference.

       01 lnkusu.
          copy usuario.cpy.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           MOVE "RED204"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED204.

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CAD010 CAD012 COD003 COD040 RED100 RED101
                      COD001

           OPEN I-O   RED204
           CLOSE      RED204
           OPEN INPUT RED204.

           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED101 <> "00"
              MOVE "ERRO ABERTURA RED101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED204 <> "00"
              MOVE "ERRO ABERTURA RED204: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED204 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
                    PERFORM CRIAR-LISTVIEW
                    PERFORM VERIFICAR-SENHA-STATUS
               WHEN GS-TRATAR-EVENTO-TRUE
                    PERFORM TRATAR-EVENTO
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-CIDADE-TRUE
                    PERFORM CHAMAR-POPUP-CIDADE
               WHEN GS-LE-CIDADE-TRUE
                   PERFORM LE-CIDADE
               WHEN GS-POPUP-REGIAO-TRUE
                    PERFORM CHAMAR-POPUP-REGIAO
               WHEN GS-LE-REGIAO-TRUE
                   PERFORM LE-REGIAO
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
               DISABLE-OBJECT PB12
           NOT INVALID KEY
               ENABLE-OBJECT PB12.

           CLOSE CAD004.

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
            using z"DATA REPOR" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"DOCTO" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CONT" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CIDADE" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"P" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"FORM" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"EVENTO" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"PART" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"EQU." returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Q-FITA" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Q-FILM" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"VLR-REPORT" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"VLR-DESPES" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"VLR-TOTAL" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"DIAS" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"PM" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"TX ACUMULADA" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"VLR-JUROS" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"VLR-CORRIGIDO" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke gs-acp-listview "gridLines"
          invoke gs-acp-listview "noBorder".


       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-rep204" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.

       mostrar-fonte-favo section.
           move "listview-rep204" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       zebrar-itens section.
           move "listview-rep204" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-acp-listview wsTexto
           invoke gs-acp-listview "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-rep204" to wsTexto
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
           EVALUATE GS-ACP-EVENTO
               WHEN 34026 SET-FOCUS EF1
               WHEN 34123 PERFORM CHAMAR-COLUNAS-FAVO.
       TRATAR-EVENTO-FIM.
           EXIT.

       GRAVA-STATUS SECTION.
           CLOSE    RED204
           OPEN I-O RED204

           INITIALIZE REG-RED204
           START RED204 KEY IS NOT LESS CODIGO-RED204 INVALID KEY
                MOVE "10" TO ST-RED204.
           PERFORM UNTIL ST-RED204 = "10"
                READ RED204 NEXT AT END
                     MOVE "10" TO ST-RED204
                NOT AT END
                     DELETE RED204 INVALID KEY
                         MOVE "Erro de Exclusão...RED204" TO MENSAGEM
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
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-RED204
               WRITE REG-RED204
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      RED204
           OPEN INPUT RED204.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-RED204
           START RED204 KEY IS NOT LESS CODIGO-RED204 INVALID KEY
               MOVE "10" TO ST-RED204.

           PERFORM UNTIL ST-RED204 = "10"
               READ RED204 NEXT AT END
                    MOVE "10" TO ST-RED204
               NOT AT END
                    MOVE CODIGO-RED204 TO CODIGO-CO01
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
              CLOSE      RED204
              OPEN I-O   RED204
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO CODIGO-RED204
                        WRITE REG-RED204

                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM
              CLOSE      RED204
              OPEN INPUT RED204.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.

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
           MOVE STATUS-CO01            TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       CHAMAR-POPUP-STATUS SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP001T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       VERIFICAR-IGUAL SECTION.
           MOVE 0   TO GS-FLAG-CRITICA
           MOVE "N" TO ACHEI
           MOVE 1   TO GS-CONT
           MOVE SPACES TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               IF GS-LINHA-STATUS(1:2) = GS-STATUS
                  MOVE "S" TO ACHEI
                  EXIT PERFORM
               ELSE
                  ADD 1 TO GS-CONT
                  MOVE SPACES TO GS-LINHA-STATUS
                  MOVE "LER-LINHA" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               END-IF
           END-PERFORM

           IF ACHEI = "S"
              MOVE "Status já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CHAMAR-POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-CID.
       LE-CIDADE SECTION.
           MOVE GS-CIDADE  TO CIDADE.
           READ CAD010 INVALID KEY MOVE "****" TO NOME-CID.
           MOVE NOME-CID           TO GS-NOME-CID.
       CHAMAR-POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-REGIAO.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-REG.
       LE-REGIAO SECTION.
           MOVE GS-REGIAO  TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE "****" TO NOME-REG.
           MOVE NOME-REG           TO GS-NOME-REG.
       GRAVA-WORK SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK.

           INITIALIZE TOT-GER-FILME
                      TOT-GER-FORMANDO
                      TOT-GER-VLR-DESPESA
                      TOT-GER-EQUIPE
                      TOT-GER-PARTICIPANTE
                      TOT-GER-VLR-REPORT
                      TOT-GER-FITA
                      TOT-GER-VLR-JUROS
                      TOT-GER-VLR-CORRIG.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-VECTO-INI   TO DATA-INV
                                  VECTO-INI-ANT
                                  VECTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO VECTO-INI
           MOVE GS-VECTO-FIM   TO DATA-INV
                                  VECTO-FIM-ANT
                                  VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO VECTO-FIM

           MOVE VECTO-INI      TO DATA-MOV-R100.
           START RED100 KEY IS NOT < DATA-MOV-R100 INVALID KEY
                  MOVE "10" TO ST-RED100.
           MOVE ZEROS TO ULT-SEQ.
           PERFORM UNTIL ST-RED100 = "10"
                  READ RED100 NEXT RECORD AT END
                       MOVE "10" TO ST-RED100
                  NOT AT END
                       IF DATA-MOV-R100 > VECTO-FIM
                          MOVE "10" TO ST-RED100
                       ELSE
                          PERFORM VERIFICA-CONT-EVENTO
                       END-IF
                  END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       VERIFICA-CONT-EVENTO SECTION.
           PERFORM VERIFICA-TOTAL-PARTICIPANTE
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE ZEROS               TO CONTRATO-R101.
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END
                  MOVE "10" TO ST-RED101
             NOT AT END
                IF DOCTO-R101 <> DOCTO-R100
                   MOVE "10" TO ST-RED101
                ELSE
                   MOVE CONTRATO-R101         TO CONTRATO-WK


                   MOVE CONTRATO-R101 TO NR-CONTRATO-CO40
                   READ COD040 INVALID KEY
                        INITIALIZE REG-COD040
                   END-READ

                   MOVE DATA-PREV-VENDA-CO40  TO DATA-INV
                   CALL "GRIDAT2" USING DATA-INV
                   MOVE DATA-INV              TO GRTIME-DATE-FINAL
                   MOVE DATA-MOV-R100         TO GRTIME-DATE
                   IF GRTIME-DATE > GRTIME-DATE-FINAL
                      MOVE GRTIME-DATE-FINAL  TO GRTIME-DATE
                   END-IF


                   PERFORM PESQUISAR-STATUS
                   IF ACHEI = "S"
                      MOVE CIDADE-CO40           TO CIDADE
                      MOVE PADRAO-CO40           TO PADRAO-WK
                      READ CAD010 INVALID KEY
                           MOVE SPACES           TO NOME-CID
                      END-READ
                      MOVE NOME-CID              TO CIDADE-WK
                      PERFORM VERIFICA-OPCAO-RELATORIO

                      IF GRAVA-W = 0
                         CONTINUE
                      ELSE
                         ADD 1                   TO ULT-SEQ
                         MOVE ULT-SEQ            TO SEQ-WK
                         MOVE DOCTO-R100         TO DOCUMENTO-WK
                         MOVE DATA-MOV-R100      TO DATA-REPORT-WK
                                                    GS-EXIBE-MOVTO
                         MOVE "TELA-AGUARDA1"    TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM

                         MOVE QT-PARTIC-R101     TO PARTICIPANTE-WK
                         MOVE QTDE-FORM-CO40     TO FORMANDO-WK
                         MOVE EVENTO-R101        TO CODIGO-CO03
                         READ COD003 INVALID KEY
                              MOVE SPACES        TO NOME-CO03
                         END-READ
                         MOVE NOME-CO03          TO EVENTO-WK
                         COMPUTE QT-FILME-WK ROUNDED =
                                (TOT-FILME-REPORT-R100
                               * QT-PARTIC-R101) / TOT-PARTICIPANTE
                         COMPUTE QT-FITA-WK ROUNDED =
                                (TOT-FITA-REPORT-R100
                               * QT-PARTIC-R101) / TOT-PARTICIPANTE
                         COMPUTE QT-EQUIPE-WK ROUNDED =
                                (QTDE-PESSOAS-R100 *
                                 QT-PARTIC-R101) / TOT-PARTICIPANTE
                         COMPUTE VLR-REPORT-WK ROUNDED =
                                (VLR-TOT-REPORT-R100 *
                                 QT-PARTIC-R101) / TOT-PARTICIPANTE
                         COMPUTE DESPESA-W = VLR-COMB-R100           +
                                             VLR-HOSP-R100           +
                                             VLR-REFEICAO-R100       +
                                             VLR-PASSAGEM-R100       +
                                             VLR-MAT-R100            +
                                             VLR-DESPESA-REPORT-R100 +
                                             VLR-ALUGUEL-R100        +
                                             VLR-OUTROS-R100

                         COMPUTE VLR-DESPESA-WK ROUNDED =
                                (DESPESA-W * QT-PARTIC-R101) /
                                 TOT-PARTICIPANTE

                         ADD QT-FILME-WK        TO TOT-GER-FILME
                         ADD QT-FITA-WK         TO TOT-GER-FITA
                         ADD QT-EQUIPE-WK       TO TOT-GER-EQUIPE
                         ADD FORMANDO-WK        TO TOT-GER-FORMANDO
                         ADD PARTICIPANTE-WK    TO TOT-GER-PARTICIPANTE
                         ADD VLR-REPORT-WK      TO TOT-GER-VLR-REPORT
                         ADD VLR-DESPESA-WK     TO TOT-GER-VLR-DESPESA


                         MOVE 2 TO GRTIME-TYPE
                         MOVE 3 TO GRTIME-FUNCTION
                         CALL "GRTIME" USING PARAMETROS-GRTIME
                         MOVE GRTIME-DAYS-FINAL TO DIAS-WK
                         COMPUTE PM-WK = GRTIME-DAYS-FINAL / 30
                         MOVE PM-WK             TO PRAZO-MEDIO
                         MOVE 1                 TO TAXA-ACUMULADA
                         PERFORM VARYING CONT FROM 1 BY 1 UNTIL
                                    CONT > PRAZO-MEDIO
                             COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                                      ((GS-TAXA / 100) + 1)
                         END-PERFORM

                         MOVE TAXA-ACUMULADA    TO TAXA-WK

                         COMPUTE JUROS-WK =
                                (VLR-REPORT-WK + VLR-DESPESA-WK) *
                                                (TAXA-ACUMULADA - 1)
                         COMPUTE CUSTO-CORRIG-WK = VLR-REPORT-WK +
                                 VLR-DESPESA-WK + JUROS-WK

                         ADD JUROS-WK           TO TOT-GER-VLR-JUROS
                         ADD CUSTO-CORRIG-WK    TO TOT-GER-VLR-CORRIG
                         WRITE REG-WORK
                         END-WRITE
                      END-IF
                   END-IF
                END-IF
             END-READ
           END-PERFORM.
       VERIFICA-OPCAO-RELATORIO SECTION.
           MOVE 0 TO GRAVA-W.
           EVALUATE GS-TIPO-REL
             WHEN 1 MOVE 1 TO GRAVA-W
             WHEN 2 IF CIDADE = GS-CIDADE MOVE 1 TO GRAVA-W
             WHEN 3 IF REGIAO-CID = GS-REGIAO MOVE 1 TO GRAVA-W
           END-EVALUATE.

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

       VERIFICA-TOTAL-PARTICIPANTE SECTION.
           MOVE ZEROS               TO TOT-PARTICIPANTE.
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE ZEROS               TO CONTRATO-R101.
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END
                  MOVE "10" TO ST-RED101
             NOT AT END
                  IF DOCTO-R101 <> DOCTO-R100
                     MOVE "10" TO ST-RED101
                  ELSE
                     ADD QT-PARTIC-R101   TO TOT-PARTICIPANTE
                  END-IF
             END-READ
           END-PERFORM.

       CARREGA-LISTA SECTION.
           INVOKE GS-ACP-LISTVIEW "Deleteall"

           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
      *               PERFORM MOVER-DADOS-LINDET
                      PERFORM MOVER-LISTVIEW
      *               MOVE "INSERE-LIST" TO DS-PROCEDURE
      *               PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM

           PERFORM MOSTRAR-FONTE-FAVO
           PERFORM MOSTRAR-COLUNAS-FAVO

           PERFORM TOTALIZA.

       MOVER-LISTVIEW SECTION.
           INITIALIZE INDICE
           INVOKE GS-ACP-LISTVIEW "ADICIONARITEM"
                                  RETURNING WSITEM

      *>Data reportagem
           MOVE DATA-REPORT-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING DATA-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Documento
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING DOCUMENTO-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Contrato
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING CONTRATO-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Cidade
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING CIDADE-WK X"00" DELIMITED BY "   " INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Padrao
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING PADRAO-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Formando
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING FORMANDO-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Evento
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING EVENTO-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Participante
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING PARTICIPANTE-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Qtde Equipe
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE QT-EQUIPE-WK TO QTDE-E4
           STRING QTDE-E4 X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Qtde Fita
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE QT-FITA-WK TO QTDE-E
           STRING QTDE-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Qtde Filme
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE QT-FILME-WK TO QTDE-E
           STRING QTDE-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor Reportagem
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE VLR-REPORT-WK TO VALOR-E
           STRING VALOR-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor Despesa
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE VLR-DESPESA-WK TO VALOR-E
           STRING VALOR-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor Total
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           ADD VLR-REPORT-WK VLR-DESPESA-WK GIVING VALOR-E
           STRING VALOR-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Dias
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE DIAS-WK        TO DIAS-E
           STRING DIAS-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>PM
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE PM-WK          TO PM-E
           STRING PM-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Taxa Acumulada
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE TAXA-WK        TO TAXA-E
           STRING TAXA-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor Juros
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE JUROS-WK TO VALOR-E1
           STRING VALOR-E1 X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Custo Corrigido
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE CUSTO-CORRIG-WK TO VALOR-E1
           STRING VALOR-E1 X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO.



       MOVER-DADOS-LINDET SECTION.
           MOVE DATA-REPORT-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(01: 11)
           MOVE DOCUMENTO-WK      TO GS-LINDET(12: 7)
           MOVE CONTRATO-WK       TO GS-LINDET(19: 5)
           MOVE CIDADE-WK         TO GS-LINDET(24: 12)
           MOVE PADRAO-WK         TO GS-LINDET(36: 2)
           MOVE FORMANDO-WK       TO GS-LINDET(38: 05)
           MOVE EVENTO-WK         TO GS-LINDET(43: 11)
           MOVE PARTICIPANTE-WK   TO GS-LINDET(54: 5)
           MOVE QT-EQUIPE-WK      TO QTDE-E4
           MOVE QTDE-E4           TO GS-LINDET(59: 5)
           MOVE QT-FITA-WK        TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(64: 7)
           MOVE QT-FILME-WK       TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(71: 7)
           MOVE VLR-REPORT-WK     TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(78: 11)
           MOVE VLR-DESPESA-WK    TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(89: 11).
           ADD VLR-REPORT-WK VLR-DESPESA-WK GIVING VALOR-E
           MOVE VALOR-E           TO GS-LINDET(100: 10).


           MOVE JUROS-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(115: 12)
           MOVE PM-WK             TO PM-E
           MOVE PM-E              TO GS-LINDET(127: 7)
           MOVE CUSTO-CORRIG-WK     TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(134: 13).
       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "DTA-REPORT" TO GS-DESCR-ORDEM ORDEM-REL
                START WORK KEY IS NOT < DATA-REPORT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "DOCUMENTO" TO GS-DESCR-ORDEM ORDEM-REL
                START WORK KEY IS NOT < DOCUMENTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CONTRATO" TO GS-DESCR-ORDEM ORDEM-REL
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "CIDADE" TO GS-DESCR-ORDEM ORDEM-REL
                START WORK KEY IS NOT < CIDADE-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "EVENTO" TO GS-DESCR-ORDEM ORDEM-REL
                START WORK KEY IS NOT < ALT1-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE TOT-GER-FORMANDO     TO QTDE-E3
           MOVE QTDE-E3              TO GS-LINTOT(1: 8)
           MOVE TOT-GER-PARTICIPANTE TO QTDE-E3
           MOVE QTDE-E3              TO GS-LINTOT(9: 8)
           MOVE TOT-GER-EQUIPE       TO QTDE-E2
           MOVE QTDE-E2              TO GS-LINTOT(17: 10)
           MOVE TOT-GER-FITA         TO QTDE-E2
           MOVE QTDE-E2              TO GS-LINTOT(27: 10)
           MOVE TOT-GER-FILME        TO QTDE-E2
           MOVE QTDE-E2              TO GS-LINTOT(37: 10)
           MOVE TOT-GER-VLR-REPORT   TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINTOT(47: 14)
           MOVE TOT-GER-VLR-DESPESA  TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(61: 11)
           ADD TOT-GER-VLR-REPORT TO TOT-GER-VLR-DESPESA GIVING VALOR-E1
           MOVE VALOR-E1             TO GS-LINTOT(72: 14).
           MOVE TOT-GER-VLR-JUROS    TO VALOR-E1
           MOVE VALOR-E1              TO GS-LINTOT(86: 14)
           MOVE TOT-GER-VLR-CORRIG   TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINTOT(100: 13).

           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP204" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           copy descondensa.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE DATA-REPORT-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01: 11)
           MOVE DOCUMENTO-WK      TO LINDET-REL(12: 7)
           MOVE CONTRATO-WK       TO LINDET-REL(19: 5)
           MOVE CIDADE-WK         TO LINDET-REL(24: 12)
           MOVE PADRAO-WK         TO LINDET-REL(36: 2)
           MOVE FORMANDO-WK       TO LINDET-REL(38: 05)
           MOVE EVENTO-WK         TO LINDET-REL(43: 11)
           MOVE PARTICIPANTE-WK   TO LINDET-REL(54: 5)
           MOVE QT-EQUIPE-WK      TO QTDE-E4
           MOVE QTDE-E4           TO LINDET-REL(59: 5)
           MOVE QT-FITA-WK        TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(64: 7)
           MOVE QT-FILME-WK       TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(71: 7)
           MOVE VLR-REPORT-WK     TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(78: 11)
           MOVE VLR-DESPESA-WK    TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(89: 11).
           ADD VLR-REPORT-WK TO VLR-DESPESA-WK GIVING VALOR-E
           MOVE VALOR-E           TO LINDET-REL(100: 10).
           MOVE JUROS-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(111: 11)
           MOVE PM-WK             TO PM-E
           MOVE PM-E              TO LINDET-REL(122: 7)
           MOVE CUSTO-CORRIG-WK   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(129: 10)

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.


       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE TOT-GER-FORMANDO     TO QTDE-E3
           MOVE QTDE-E3              TO LINTOT-REL(1: 10)
           MOVE TOT-GER-PARTICIPANTE TO QTDE-E3
           MOVE QTDE-E3              TO LINTOT-REL(11: 10)
           MOVE TOT-GER-EQUIPE       TO QTDE-E2
           MOVE QTDE-E2              TO LINTOT-REL(21: 10)
           MOVE TOT-GER-FITA         TO QTDE-E2
           MOVE QTDE-E2              TO LINTOT-REL(31: 10)
           MOVE TOT-GER-FILME        TO QTDE-E2
           MOVE QTDE-E2              TO LINTOT-REL(41: 13)
           MOVE TOT-GER-VLR-REPORT   TO VALOR-E1
           MOVE VALOR-E1             TO LINTOT-REL(54: 17)
           MOVE TOT-GER-VLR-DESPESA  TO VALOR-E1
           MOVE VALOR-E1             TO LINTOT-REL(71: 14)
           ADD TOT-GER-VLR-REPORT TO TOT-GER-VLR-DESPESA GIVING VALOR-E1
           MOVE VALOR-E1             TO LINTOT-REL(85: 13).

           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           MOVE 2 TO LIN.

           EVALUATE GS-TIPO-REL
             WHEN 1 CONTINUE
             WHEN 2 MOVE GS-CIDADE      TO CIDADE-REL
                    MOVE GS-NOME-CID    TO NOME-CID-REL
                    WRITE REG-RELAT FROM CAB02A AFTER 2
                    ADD 2 TO LIN
             WHEN 3 MOVE GS-REGIAO      TO REGIAO-REL
                    MOVE GS-NOME-REG    TO NOME-REG-REL
                    WRITE REG-RELAT FROM CAB02B AFTER 2
                    ADD 2 TO LIN
           END-EVALUATE
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           ADD 3 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 CAD012 COD003 COD040 RED100 RED101 WORK
                 COD001 RED204.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
