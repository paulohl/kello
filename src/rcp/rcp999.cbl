       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP999.
      *DATA: 01/12/2016
      *AUTOR: ALFREDO SAVIOLLI NETO
      *FUNÇÃO: RELATORIO DE VENDAS X ESCALA DE VENDAS
       ENVIRONMENT DIVISION.
       class-control.
           Window              is class "wclass"
           Color               is class "color"
           AListview           is class "alistview".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX010.
           COPY CGPX001.
           COPY MTPX020.
           COPY MTPX020E.
           COPY RCPX100.
           COPY RCPX100C.
           COPY RCPX100E.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = ALBUM-WK.


           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
           COPY CGPW010.
           COPY CGPW001.
           COPY MTPW020.
           COPY MTPW020E.
           COPY RCPW100.
           COPY RCPW100C.
           COPY RCPW100E.

       FD  WORK.
       01  REG-WORK.
           05  COD-VENDEDOR-WK          PIC 9(6).
           05  VENDEDOR-WK              PIC X(20).
           05  ALBUM-WK                 PIC 9(8).
           05  CLIENTE-WK               PIC X(20).
           05  DATA-VENDA-WK            PIC 9(8).
           05  PRECO-ESCALA-BRUTO-WK    PIC 9(9)v99.
           05  PRECO-ESCALA-WK          PIC 9(9)v99.
           05  COMISSAO-WK              PIC 9(1).
           05  VLR-VENDA-DEF-WK         PIC 9(8)V99.
           05  VLR-VENDA-WK             PIC 9(9)V99.
           05  VLR-VARIACAO-WK          PIC 9(9)V99.
           05  PERC-VARIACAO-WK         PIC S9(06)V99.
           05  PM-WK                    PIC 9(03)V99.
           05  DESC-FORMANDO-WK         PIC 9(9)V99.
           05  DESC-COMISSAO-WK         PIC 9(9)V99.
           05  DESC-AVISTA-WK           PIC 9(9)V99.
           05  VLR-LIQUIDO-WK           PIC 9(9)V99.
           05  JUROS-WK                 PIC 9(9)V99.
           05  VLR-LIQUIDO-MAIS-JR-WK   PIC 9(9)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(140).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP999.CPB".
           COPY "RCP999.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD100C            PIC XX       VALUE SPACES.
           05  ST-RCD100E            PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD020E            PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02).
           05  ACHEI                 PIC X(01)    VALUE SPACES.
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
           05  MOVTO-INI             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATAI.
               10  ANO-I             PIC 9(4).
               10  MES-I             PIC 9(2).
               10  DIA-I             PIC 9(2).
           05  DATA-I REDEFINES DATAI PIC 9(8).
           05  MESANOW.
               10  MES-WW            PIC 9(2).
               10  ANO-WW            PIC 9(4).
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9(4).
               10  MES-II            PIC 9(2).
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  MESANO-E              PIC ZZ/ZZZZ       BLANK WHEN ZEROS.
           05  DATA-E                PIC 99/99/9999    BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ    BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  VENDEDOR-ANT          PIC 9(6)     VALUE ZEROS.
           05  VLR-DEFLACAO          PIC 9(8)V99  VALUE ZEROS.
           05  VALOR-COMISSAO        PIC 9(8)V99  VALUE ZEROS.
           05  VALOR-COMISSAO-DEF    PIC 9(8)V99  VALUE ZEROS.
           05  PM-W                  PIC 9(3)V9.
           05  PM-E                  PIC ZZZ,Z.
           05  ALBUMW.
               10  CONTRATO-W        PIC 9(4).
               10  SEQ-W             PIC 9(4).
           05  ALBUM-W REDEFINES ALBUMW PIC 9(8).
           05  TOT-VENDA             PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VENDA-DEF         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ESCALA-BRUTO      PIC 9(9)V99  VALUE ZEROS.
           05  TOT-DESC-ESCALA       PIC 9(9)V99  VALUE ZEROS.
           05  TOT-ESCALA            PIC 9(9)V99  VALUE ZEROS.
           05  TOT-VARIACAO          PIC 9(9)V99  VALUE ZEROS.
           05  TOT-PRAZO-MED         PIC 9(9)V99  VALUE ZEROS.
           05  TOT-BONIF             PIC 9(9)V99  VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
           05  MASC-VALOR            PIC ZZZ.ZZZ.ZZ9,99-
               BLANK WHEN ZEROS.
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.
           05  AUX-DATA2             PIC 9(08).
           05  AUX-DATA              PIC 9(08).
           05  AUX-DATA-R REDEFINES AUX-DATA.
               10 AUX-MESANO         PIC 9(06).
               10 AUX-DIA            PIC 9(02).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.
       01 perc-desc                    pic 9(03)v9(02).
       01 aux-valor                    pic S9(09)v99 value zeros.
       01 qtde-meses                   pic 9(04)    value zeros.
       01 ind                          pic 9(04)    value zeros.
       01 qtde-dias                    pic 9(02)    value zeros.
       01 taxa-w                       pic 9(04)v99999   value zeros.

       01 detRGBFundo.
          03 lsRedFundo    pic 9(003) comp-5 value zeros.
          03 lsGreenFundo  pic 9(003) comp-5 value zeros.
          03 lsBlueFundo   pic 9(003) comp-5 value zeros.

       01 lnkusu.
          copy usuario.cpy.

       01 lnktabela.
          02 lnkobjetoscol object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.

       01 indice                       pic 9(02).
       01 wssize                       pic 9(09) comp-5 value zeros.
       01 wsIndice                     pic 9(09) comp-5 value zeros.
       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 umitem                       object reference.
       77 aColorFundo                  object reference.
       77 umobjeto                     object reference.

       01 mensagem                 pic x(200).
       01 tipo-msg                 pic x(01).
       01 resp-msg                 pic x(01).

       01  CAB01.
           05  EMPRESA-REL         PIC X(35)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RELATORIO DE COMISSAO DE VENDEDOR -".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(03)   VALUE SPACES.
           05  ORDEM-REL2          PIC X(16)   VALUE SPACES.
       01  CAB02A.
           05  DET-DESCRICAO       PIC X(14).
           05  DATA-MOVTO-INI-REL  PIC ZZ/ZZ/ZZZZ.
           05  FILLER              PIC XXX     VALUE " a ".
           05  DATA-MOVTO-FIM-REL  PIC ZZ/ZZ/ZZZZ.
       01  CAB02B.
           05  FILLER              PIC X(10) VALUE "CONTRATO: ".
           05  CONTRATO-REL        PIC 9999.

       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "CODIGO NOME-VENDEDOR                  DATA-VECTO  VLR-COMISS
      -    "AO".
       01  CAB05.
           05  FILLER              PIC X(80)  VALUE
           "CODIGO NOME-VEND  P.M. DT-VCTO     VLR-VENDA     VENDA-DEF
      -    "VLR-COMIS COMIS-DEFL".
       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.
       01  LINDET1.
           05  LINDET1-REL         PIC X(80)  VALUE SPACES.
       01  CAB06.
           05  FILLER               PIC X(80)    VALUE
           "__________________   __________________   __________________
      -    "  __________________".
       01  CAB07.
           05  FILLER               PIC X(80)    VALUE
           "    EMITENTE              DIRETOR                CPD
      -     "     FINANCEIRO".
       01  CAB08.
           05  FILLER               PIC X(80)    VALUE
           "_____/_____/______   _____/_____/______   _____/_____/______
      -     "  _____/_____/______".



       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE DATA-MOVTO-W TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO DATA-MOVTO-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD020E" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020E
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD100C" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100C.
           MOVE "RCD100E" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100E.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario

           OPEN INPUT CGD001  MTD020 MTD020E RCD100 RCD100E CGD010
                      RCD100C
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100C <> "00"
              MOVE "ERRO ABERTURA RCD100C: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100C TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100E <> "00"
              MOVE "ERRO ABERTURA RCD100E: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100E TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020E <> "00"
              MOVE "ERRO ABERTURA MTD020E: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020E TO GS-MENSAGEM-ERRO(23: 02)
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
      *        WHEN GS-PRINTER-FLG-TRUE
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM CHAMAR-POPUP-VENDEDOR
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LE-VENDEDOR
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

       TRATAR-EVENTO SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34123 perform chamar-colunas-favo
           END-EVALUATE.
       TRATAR-EVENTO-FIM.
           EXIT.

       chamar-colunas-favo section.
           move "listview-rcp999" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo.
       chamar-colunas-favo-fim.
           exit.


       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Cont/Álbum" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Cliente" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Vendedor" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Dt Venda" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Escala À Vista" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Desc Escala" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Bônus Comissão" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Escala Líquido" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Desc. À Vista" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Líquido" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Juros" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Líquido + Jr" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Valor venda" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Valor Defla" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Variação R$" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Variação %" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"PM" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke gs-acp-listview "gridLines"
          invoke gs-acp-listview "noBorder".
       CRIAR-LISTVIEW-PRODUTOS-FIM.
           EXIT.

       mostrar-fonte-favo section.
           move "listview-rcp999" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-rcp999" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR  TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "****" TO NOME-CG01.
           MOVE NOME-CG01    TO GS-DESC-VENDEDOR.
       CHAMAR-POPUP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.
           PERFORM LE-VENDEDOR.
      *----------------------------------------------------------
       INVERTE-DATA SECTION.
           MOVE DATA-INV     TO VECTO-FIM.
           MOVE GS-MOVTO-INI TO DATA-INV DATA-MOVTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO MOVTO-INI.
           MOVE GS-MOVTO-FIM TO DATA-INV DATA-MOVTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO MOVTO-FIM
           MOVE GS-DESCRICAO TO DET-DESCRICAO

           MOVE GS-CONTRATO  TO CONTRATO-REL.

       GRAVA-WORK SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM INVERTE-DATA.

           INITIALIZE REG-RCD100

           IF GS-CONTRATO > 0
              PERFORM POR-CONTRATO
           ELSE
              EVALUATE GS-OPCAO
                 WHEN 1 PERFORM POR-DTMOVTO
                 WHEN 2 PERFORM POR-DTVECTO
              END-EVALUATE
           END-IF

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       POR-CONTRATO SECTION.
           STRING GS-CONTRATO "0000"       INTO ALBUM-REC
           START RCD100 KEY IS NOT LESS ALBUM-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   IF GS-CONTRATO <> ALBUM-REC(1:4)
                      MOVE "10" TO ST-RCD100
                   ELSE
                      IF GS-OPCAO = 1
                         IF DATA-MOVTO-REC NOT < MOVTO-INI AND
                            DATA-MOVTO-REC NOT > MOVTO-FIM OR
                            MOVTO-INI = 0
                            MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
                            MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                            IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                               VENDEDOR-REC
                               PERFORM MOVER-DADOS-WORK
                            END-IF
                         END-IF
                      ELSE
                         IF DATAVEN-REC NOT < MOVTO-INI AND
                            DATAVEN-REC NOT > MOVTO-FIM OR
                            MOVTO-INI = 0
                            MOVE DATAVEN-REC     TO GS-EXIBE-MOVTO
                            MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                            IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                               VENDEDOR-REC
                               PERFORM MOVER-DADOS-WORK
                            END-IF
                         END-IF
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       POR-DTMOVTO SECTION.
           INITIALIZE REG-RCD100
           MOVE MOVTO-INI      TO DATA-MOVTO-REC
           START RCD100 KEY IS NOT LESS ALT-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   IF DATA-MOVTO-REC > MOVTO-FIM
                      MOVE "10" TO ST-RCD100
                   ELSE
                      MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
                      MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                         VENDEDOR-REC
                         PERFORM MOVER-DADOS-WORK
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       POR-DTVECTO SECTION.
           INITIALIZE REG-RCD100
           MOVE MOVTO-INI      TO DATAVEN-REC
           START RCD100 KEY IS NOT LESS DATAVEN-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   IF DATAVEN-REC > MOVTO-FIM
                      MOVE "10" TO ST-RCD100
                   ELSE
                      MOVE DATAVEN-REC  TO GS-EXIBE-MOVTO
                      MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                         VENDEDOR-REC
                         PERFORM MOVER-DADOS-WORK
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.


       MOVER-DADOS-WORK SECTION.
           INITIALIZE REG-WORK

           MOVE VENDEDOR-REC         TO COD-VENDEDOR-WK
           MOVE COD-VENDEDOR-WK      TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES          TO NOME-CG01
           END-READ
           MOVE NOME-CG01            TO VENDEDOR-WK
           MOVE ALBUM-REC            TO ALBUM-WK
           MOVE 0                    TO CLASSIF-CG10
           MOVE ALBUM-WK             TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES          TO COMPRADOR-CG10
           END-READ
           MOVE COMPRADOR-CG10       TO CLIENTE-WK
           MOVE DATAVEN-REC          TO DATA-VENDA-WK
           IF MEMBRO-COMISSAO-REC IS NOT NUMERIC
              MOVE 0 TO MEMBRO-COMISSAO-REC
           END-IF
           MOVE MEMBRO-COMISSAO-REC  TO COMISSAO-WK
           MOVE TOTAL-REC            TO VLR-VENDA-WK
           MOVE TOTAL-DEF-REC        TO VLR-VENDA-DEF-WK
           MOVE PM-REC               TO PM-WK

           INITIALIZE REG-RCD100E
           MOVE "N"                  TO ACHEI
           MOVE ALBUM-REC            TO ALBUM-REC-E
           START RCD100E KEY IS NOT LESS CHAVE-ALBUM-REC-E INVALID KEY
                 MOVE "10" TO ST-RCD100E.

           PERFORM UNTIL ST-RCD100E = "10"
                 READ RCD100E NEXT AT END
                      MOVE "10" TO ST-RCD100E
                 NOT AT END
                      IF ALBUM-REC <> ALBUM-REC-E
                         MOVE "10" TO ST-RCD100E
                      ELSE
                         ADD TOTAL-ESCALA-RCD-E TO PRECO-ESCALA-WK

                         COMPUTE PRECO-ESCALA-BRUTO-WK =
                                 PRECO-ESCALA-BRUTO-WK +
                                (RECIBO-REC-E * PRECO-ESCALA-RCD-E)

                         COMPUTE DESC-FORMANDO-WK ROUNDED =
                                 DESC-FORMANDO-WK +
                                (RECIBO-REC-E * PRECO-ESCALA-RCD-E *
                                 PERC-DESC-FORM-RCD-E / 100)

                         IF PERC-DESC-COMISSAO-RCD-E > 0 AND
                            MEMBRO-COMISSAO-REC = 1
                            COMPUTE DESC-COMISSAO-WK ROUNDED =
                                    DESC-COMISSAO-WK +
                                   (RECIBO-REC-E * PRECO-ESCALA-RCD-E *
                                    PERC-DESC-COMISSAO-RCD-E / 100)
                         END-IF

                         MOVE "S"               TO ACHEI
                      END-IF
                 END-READ
           END-PERFORM

           IF MEMBRO-COMISSAO-REC IS NOT NUMERIC
              MOVE 0 TO MEMBRO-COMISSAO-REC
           END-IF

           MOVE ALBUM-REC                TO ALBUM-REC-C
           READ RCD100C INVALID KEY
                INITIALIZE REG-RCD100C
           END-READ

           MOVE ALBUM-REC(1:4)     TO CONTRATO-MTG-E
           READ MTD020E INVALID KEY
                INITIALIZE REG-MTD020E
           END-READ

           IF PM-AVISTA-MTG-E IS NOT NUMERIC
              MOVE 0 TO PM-AVISTA-MTG-E
           END-IF

           COMPUTE AUX-VALOR = PRECO-ESCALA-BRUTO-WK -
                       (DESC-FORMANDO-WK + DESC-COMISSAO-WK)

           IF PM-WK NOT > PM-AVISTA-MTG-E
              COMPUTE DESC-AVISTA-WK =
              - (AUX-VALOR * TAXA-DESCONTO-MTG-E / 100)
           ELSE
              MOVE ZEROS TO DESC-AVISTA-WK
           END-IF

           COMPUTE VLR-LIQUIDO-WK = AUX-VALOR - DESC-AVISTA-WK

           IF PM-WK > CARENCIA-JUROS-MTG-E
              MOVE VLR-LIQUIDO-WK TO AUX-VALOR

              COMPUTE QTDE-MESES = PM-WK / 30

              MOVE ZEROS TO IND
              PERFORM QTDE-MESES TIMES
                  ADD 1 TO IND


                  COMPUTE AUX-VALOR ROUNDED = AUX-VALOR +
                         (AUX-VALOR * TAXA-JUROS-MTG-E / 100)
              END-PERFORM

              COMPUTE QTDE-DIAS = PM-WK - (QTDE-MESES * 30)

              IF QTDE-DIAS > 0
                 COMPUTE TAXA-W = (((TAXA-JUROS-MTG-E / 30) / 100)
                                           * QTDE-DIAS)
                 COMPUTE AUX-VALOR ROUNDED = AUX-VALOR +
                                  (AUX-VALOR * TAXA-W / 100)

              END-IF

              COMPUTE JUROS-WK = AUX-VALOR - VLR-LIQUIDO-WK

           ELSE
              MOVE ZEROS TO JUROS-WK
           END-IF

           COMPUTE VLR-LIQUIDO-MAIS-JR-WK = VLR-LIQUIDO-WK + JUROS-WK


      *    IF ACHEI = "N"
      *       INITIALIZE REG-MTD020E
      *       MOVE ALBUM-REC(1:4)        TO CONTRATO-MTG-E
      *       START MTD020E KEY IS NOT LESS CONTRATO-MTG-E INVALID KEY
      *             MOVE "10" TO ST-MTD020E
      *       END-START
      *       PERFORM UNTIL ST-MTD020E = "10"
      *             READ MTD020E NEXT AT END
      *                  MOVE "10" TO ST-MTD020E
      *             NOT AT END
      *                  IF ALBUM-REC(1:4) <> CONTRATO-MTG-E
      *                     MOVE "10" TO ST-MTD020E
      *                  ELSE
      *>Encadernação
      *                     IF QENCADER-REC > QTDE-ACIMA-DE-ENCADER
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-ENCADER
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-ENCADER
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *
      *                     COMPUTE DESC-FORMANDO-WK ROUNDED =
      *                             DESC-FORMANDO-WK +
      *                            (QENCADER-REC *
      *                             PRECO-ENCADER-MTG-E *
      *                             PERC-DESC-FORM-ENCADER / 100)
      *
      *                     COMPUTE VALOR-FORMANDO ROUNDED =
      *                           (QENCADER-REC * PRECO-ENCADER-MTG-E) -
      *                           (QENCADER-REC *
      *                            PRECO-ENCADER-MTG-E *
      *                            PERC-DESC-FORM-ENCADER / 100)
      *
      *                     IF PERC-DESC-COM-ENCADER > 0
      *                         COMPUTE DESC-COMISSAO-WK ROUNDED =
      *                                 DESC-COMISSAO-WK +
      *                                (VALOR-FORMANDO *
      *                                 PERC-DESC-COM-ENCADER / 100)
      *                     END-IF
      *
      *
      *
      *
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-ENCADER-MTG-E * QENCADER-REC) -
      *                      ((PRECO-ENCADER-MTG-E * QENCADER-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Estojo
      *                     IF QESTOJO-REC > QTDE-ACIMA-DE-ESTOJO
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-ESTOJO
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-ESTOJO
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-ESTOJO-MTG-E * QESTOJO-REC) -
      *                      ((PRECO-ESTOJO-MTG-E * QESTOJO-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Folha
      *                     IF QFOLHAS-REC > QTDE-ACIMA-DE-FOLHA
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-FOLHA
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-FOLHA
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-FOLHA-MTG-E * QFOLHAS-REC) -
      *                      ((PRECO-FOLHA-MTG-E * QFOLHAS-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Foto
      *                     IF QFOTOS-REC > QTDE-ACIMA-DE-FOTO
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-FOTO
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-FOTO
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-FOTO-MTG-E * QFOTOS-REC) -
      *                      ((PRECO-FOTO-MTG-E * QFOTOS-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Fita
      *                     IF QFITAS-REC > QTDE-ACIMA-DE-FITA
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-FITA
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-FITA
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-FITA-MTG-E * QFITAS-REC) -
      *                      ((PRECO-FITA-MTG-E * QFITAS-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Porta Fita
      *                     IF QPFITA-REC > QTDE-ACIMA-DE-PT-FITA
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-PT-FITA
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-PT-FITA
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-PORTA-FITA-MTG-E * QPFITA-REC) -
      *                      ((PRECO-PORTA-FITA-MTG-E * QPFITA-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Poster
      *                     IF QPOSTER-REC > QTDE-ACIMA-DE-POSTER
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-POSTER
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-POSTER
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-POSTER-MTG-E * QPOSTER-REC) -
      *                      ((PRECO-POSTER-MTG-E * QPOSTER-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>DVD
      *                     IF QDVD-REC > QTDE-ACIMA-DE-DVD
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-DVD
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-DVD
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-DVD-MTG-E * QDVD-REC) -
      *                      ((PRECO-DVD-MTG-E * QDVD-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Porta DVD
      *                     IF QPORTA-DVD-REC > QTDE-ACIMA-DE-PT-DVD
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-PT-DVD
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-PT-DVD
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-PORTA-DVD-MTG-E * QPORTA-DVD-REC) -
      *                      ((PRECO-PORTA-DVD-MTG-E * QPORTA-DVD-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Foto CD
      *                     IF QFOTO-CD-REC > QTDE-ACIMA-DE-FT-CD
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-FT-CD
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-FT-CD
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-FOTO-CD-MTG-E * QFOTO-CD-REC) -
      *                      ((PRECO-FOTO-CD-MTG-E * QFOTO-CD-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Moldura
      *                     IF QMOLDURA-REC > QTDE-ACIMA-DE-MOLDURA
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-MOLDURA
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-MOLDURA
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-MOLDURA-MTG-E * QMOLDURA-REC) -
      *                      ((PRECO-MOLDURA-MTG-E * QMOLDURA-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Book
      *                     IF QBOOK-REC > QTDE-ACIMA-DE-BOOK
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-BOOK
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-BOOK
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-BOOK-MTG-E * QBOOK-REC) -
      *                      ((PRECO-BOOK-MTG-E * QBOOK-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Porta retrato
      *                     IF QPORTA-RETRATO-REC-C >
      *                        QTDE-ACIMA-DE-PT-RET
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-PT-RET
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-PT-RET
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                     ((PRECO-PORTA-RETRATO-MTG-E * QPOSTER-REC) -
      *                     ((PRECO-PORTA-RETRATO-MTG-E * QPOSTER-REC) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Pendrive
      *                     IF QPENDRIVE-REC-C > QTDE-ACIMA-DE-PENDRIVE
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-PENDRIVE
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-PENDRIVE
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-PENDRIVE-MTG-E * QPENDRIVE-REC-C) -
      *                      ((PRECO-PENDRIVE-MTG-E * QPENDRIVE-REC-C) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Video HD
      *                     IF QVIDEO-HD-REC-C > QTDE-ACIMA-DE-VIDEO-HD
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-VIDEO-HD
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-VIDEO-HD
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-VIDEO-HD-MTG-E * QVIDEO-HD-REC-C) -
      *                      ((PRECO-VIDEO-HD-MTG-E * QVIDEO-HD-REC-C) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Revista
      *                     IF QREVISTA-REC-C > QTDE-ACIMA-DE-REVISTA
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-REVISTA
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-REVISTA
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-REVISTA-MTG-E * QREVISTA-REC-C) -
      *                      ((PRECO-REVISTA-MTG-E * QREVISTA-REC-C) *
      *                                PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *>Calendário
      *                     IF QCALENDARIO-REC-C > QTDE-ACIMA-DE-CALEND
      *                        IF MEMBRO-COMISSAO-REC = 1
      *                           MOVE PERC-DESC-COM-CALEND
      *                             TO PERC-DESC
      *                        ELSE
      *                           MOVE PERC-DESC-FORM-CALEND
      *                             TO PERC-DESC
      *                        END-IF
      *                     ELSE
      *                        MOVE ZEROS TO PERC-DESC
      *                     END-IF
      *
      *                     COMPUTE TOT-ESCALA ROUNDED =
      *                      ((PRECO-CALENDARIO-MTG-E *
      *                        QCALENDARIO-REC-C) -
      *                      ((PRECO-CALENDARIO-MTG-E *
      *                        QCALENDARIO-REC-C) * PERC-DESC / 100))
      *                     ADD TOT-ESCALA TO PRECO-ESCALA-WK
      *                  END-IF
      *             END-READ
      *       END-PERFORM
      *    END-IF
      *
           IF ACHEI = "S"
              WRITE REG-WORK INVALID KEY
                    MOVE "Erro de Gravação...WORK" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
              END-WRITE.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-VENDA TOT-VENDA-DEF TOT-ESCALA TOT-BONIF
                         TOT-PRAZO-MED TOT-ESCALA-BRUTO TOT-DESC-ESCALA

           INVOKE GS-ACP-LISTVIEW "DeleteAll"

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK.
           MOVE ZEROS TO REG-WORK
           START WORK KEY IS GREATER THAN CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      ADD VLR-VENDA-WK     TO TOT-VENDA
                      ADD VLR-VENDA-DEF-WK TO TOT-VENDA-DEF
                      ADD PRECO-ESCALA-WK  TO TOT-ESCALA
                      ADD PRECO-ESCALA-BRUTO-WK TO TOT-ESCALA-BRUTO
                      ADD VLR-VARIACAO-WK  TO TOT-VARIACAO
                      ADD DESC-COMISSAO-WK TO TOT-DESC-ESCALA

                      IF COMISSAO-WK = 1
                         IF PRECO-ESCALA-WK > 0
                            ADD DESC-COMISSAO-WK TO TOT-BONIF
                         ELSE
                            ADD VLR-VENDA-WK TO TOT-BONIF
                         END-IF
                      END-IF

                      MOVE ALBUM-WK        TO GS-EXIBE-MOVTO
                      MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM


                      PERFORM PREENCHER-LISTVIEW

                 END-READ
           END-PERFORM

           perform totalizar-listview

           perform mostrar-fonte-favo
           perform mostrar-colunas-favo.

       PREENCHER-LISTVIEW SECTION.
           INITIALIZE INDICE
           INVOKE GS-ACP-LISTVIEW "ADICIONARITEM" RETURNING WSITEM

      *>Álbum
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING ALBUM-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Cliente
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING CLIENTE-WK X"00" DELIMITED BY "   " INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Vendedor
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING VENDEDOR-WK X"00" DELIMITED BY "   " INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Data Venda
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING DATA-VENDA-WK(7:2) "/" DATA-VENDA-WK(5:2) "/"
                  DATA-VENDA-WK(1:4) X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor Escala
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE PRECO-ESCALA-BRUTO-WK TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Desconto Escala
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
      *    COMPUTE MASC-VALOR = PRECO-ESCALA-BRUTO-WK - PRECO-ESCALA-WK
           MOVE DESC-FORMANDO-WK TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Bonus Comissao
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           IF COMISSAO-WK = 1
              MOVE DESC-COMISSAO-WK TO MASC-VALOR
           ELSE
              MOVE ZEROS TO MASC-VALOR
           END-IF
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Líquido
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE PRECO-ESCALA-WK      TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Desconto À Vista
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE DESC-AVISTA-WK      TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Líquido
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE VLR-LIQUIDO-WK       TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Juros
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE JUROS-WK TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Líquido + Juros
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE VLR-LIQUIDO-MAIS-JR-WK TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor Venda
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE VLR-VENDA-WK TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor Deflacionado
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE VLR-VENDA-DEF-WK TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Variação em R$
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           COMPUTE MASC-VALOR = VLR-VENDA-WK - VLR-LIQUIDO-MAIS-JR-WK
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Variação em %
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           COMPUTE MASC-VALOR ROUNDED =
              ((VLR-VENDA-WK - VLR-LIQUIDO-MAIS-JR-WK) /
                VLR-LIQUIDO-MAIS-JR-WK) * 100
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>PM
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE PM-WK TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

           COMPUTE AUX-VALOR = VLR-VENDA-WK - VLR-LIQUIDO-MAIS-JR-WK

           IF AUX-VALOR < 0
              move  255 to lsRedFundo
              move  128 to lsGreenFundo
              move  128 to lsBlueFundo

              invoke Color "RGB" using lsRedFundo
                                       lsGreenFundo
                                       lsBlueFundo
                                       returning aColorFundo

              invoke gs-acp-listview "itemAtIndex"
                     using wsitem returning umitem
              invoke umitem "SetarCorDeFundoObjeto"
                     using aColorFundo.

       TOTALIZAR-LISTVIEW SECTION.
           INITIALIZE INDICE
           INVOKE GS-ACP-LISTVIEW "ADICIONARITEM" RETURNING WSITEM

      *>Álbum
           ADD 1 TO INDICE
      *>Cliente
           ADD 1 TO INDICE
      *>Vendedor
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING "TOTALIZADOR ..." X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Data Venda
           ADD 1 TO INDICE
      *>Valor Escala
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE TOT-ESCALA-BRUTO TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Desconto Escala
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
      *    COMPUTE MASC-VALOR = TOT-ESCALA-BRUTO - TOT-ESCALA
           MOVE TOT-DESC-ESCALA TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Bonus Comissao
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE TOT-BONIF TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Líquido
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE TOT-ESCALA TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor Venda
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE TOT-VENDA TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Valor Venda Deflacionada
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE TOT-VENDA-DEF TO MASC-VALOR
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Variação em R$
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           COMPUTE MASC-VALOR = TOT-VENDA - TOT-ESCALA
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO
      *>Variação em %
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           COMPUTE MASC-VALOR ROUNDED = ((TOT-VENDA - TOT-ESCALA)
                                           / TOT-ESCALA) * 100
           STRING MASC-VALOR X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO.
      *>PM
      *    ADD 1 TO INDICE.
      *    INITIALIZE WSTEXTO
      *    COMPUTE MASC-VALOR = TOT-PRAZO-MED / TOT-VENDA
      *    STRING MASC-VALOR X"00" INTO WSTEXTO
      *    INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
      *      USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP999" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 MTD020 MTD020E RCD100 RCD100E WORK CGD010
                 RCD100C
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
