       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP208.
      *DATA: 03/04/2004.
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELATORIO DE ACOMPANHAMENTO DE VENDAS POR CONTRATO
      *        POR CONTRATO OU POR INTERVALO DE DATA(MOVTO OU VENDA)
      *        QTDE DE FOTOS VENDEDIDAS = QFOTOS-REC + QABERTURA-REC
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY MTPX019.
           COPY CGPX001.
           COPY COPX040.
           COPY MTPX020.
           COPY RCPX100.
           COPY CEAPX010.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS ALBUM-WK
                  ALTERNATE RECORD KEY IS VENDEDOR-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NOME-CLIENTE-WK
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-VENDA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TOTVEN-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TOTFITA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TOTPORTA-FITA-WK
                                           WITH DUPLICATES.

      *    SELECT WORK2 ASSIGN TO VARIA-W2
      *           ORGANIZATION IS INDEXED
      *           ACCESS MODE IS DYNAMIC
      *           STATUS IS ST-WORK2
      *           RECORD KEY IS CONTRATO-WK2.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY MTPW019.
       COPY CGPW001.
       COPY COPW040.
       COPY MTPW020.
       COPY RCPW100.
       COPY CEAPW010.

       FD  WORK.
       01  REG-WORK.
           05  DTAMOV-WK             PIC 9(08).
           05  ALBUM-WK              PIC 9(8).
           05  NOME-CLIENTE-WK       PIC X(17).
           05  FONE-WK               PIC 9(11).
           05  TOTALB-WK             PIC 9.
           05  TOTFOT-WK             PIC 999.
           05  TOTFITA-WK            PIC 99.
           05  TOTDVD-WK             PIC 99.
           05  TOTFOL-WK             PIC 999.
           05  TOTPOSTER-WK          PIC 99.
           05  TOTPORTA-FITA-WK      PIC 99.
           05  TOTAVUL-WK            PIC 999.
           05  TOTCOMIS-WK           PIC 999.
           05  TOTVEN-WK             PIC 9(10)V99.
           05  PM-WK                 PIC 9(14)V99.
           05  TOTVENIDX-WK          PIC 9(09)V99.
           05  DATA-VENDA-WK         PIC 9(08).
           05  COD-VENDEDOR-WK       PIC 9(6).
           05  VENDEDOR-WK           PIC X(09).
           05  FITDEV-WK             PIC S9(3).
           05  ALBDEV-WK             PIC S9(3).
           05  CODFOT-WK             PIC 9(05).
           05  CODALB-WK             PIC 9(05).
           05  DEVOL-WK              PIC S9(05).
           05  DEVFITA-WK            PIC S9(05).
           05  DEVDVD-WK             PIC S9(05).

      *FD  WORK2.
      *01  REG-WORK2.
      *    05  CONTRATO-WK2          PIC 9(4).
      *    05  PRODUZIDA-WK2         PIC 9(6).
      *    05  MONTADA-WK2           PIC 9(6).
      *    05  AVULSA-WK2            PIC 9(6).
      *    05  PERDIDA-WK2           PIC 9(6).
      *    05  FORM-PROD-WK2         PIC 9(6).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(140).
       WORKING-STORAGE SECTION.
           COPY "RCP208.CPB".
           COPY "RCP208.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
           COPY IMPRESSORA.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-MTD001             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-CEAD010            PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
      *    05  ST-WORK2              PIC XX       VALUE SPACES.
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
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999    BLANK WHEN ZEROS.
           05  DATA-E1               PIC 99/99/99      BLANK WHEN ZEROS.
           05  WS-DATA6              PIC 9(06)    VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC Z.ZZZ,ZZ      BLANK WHEN ZEROS.
           05  VALOR-E2A             PIC ZZZZ,ZZ       BLANK WHEN ZEROS.
           05  VALOR-E2              PIC ZZZ,ZZ        BLANK WHEN ZEROS.
           05  VALOR-E3              PIC ZZZ.ZZZ,ZZ    BLANK WHEN ZEROS.
           05  VALOR-E4              PIC ZZ,ZZ         BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  DESPESA-W             PIC 9(8)V99  VALUE ZEROS.

           05  SALALB-W        PIC 9(05)         VALUE ZEROS.
           05  PRAZO-W         PIC 9(05)V999     VALUE ZEROS.
           05  SALDO-W         PIC S9(05)        VALUE ZEROS.
           05  VALOR-FOTOW     PIC 9(8)V99       VALUE ZEROS.
           05  VALOR-FITAW     PIC 9(8)V99       VALUE ZEROS.
           05  VALOR-ALBUMW    PIC 9(8)V99       VALUE ZEROS.
           05  SOMA-AUX        PIC 9(07)         VALUE ZEROS.

           05  VALORU-W        PIC 9(10)V99      VALUE ZEROS.
           05  COMISSAO-W      PIC 9(10)V99      VALUE ZEROS.
           05  TOTALB-W        PIC 9(05)         VALUE ZEROS.
           05  TOTFOT-W        PIC 9(05)         VALUE ZEROS.
           05  TOTFITA-W       PIC 9(05)         VALUE ZEROS.
           05  TOTDVD-W       PIC 9(05)         VALUE ZEROS.
           05  TOTFOL-W        PIC 9(05)         VALUE ZEROS.
           05  TOTVEN-W        PIC 9(15)V99      VALUE ZEROS.
           05  TOTPORTA-W      PIC 9(05)         VALUE ZEROS.
           05  TOTPOSTER-W     PIC 9(5)          VALUE ZEROS.
           05  PERC-W          PIC 9(3)V99       VALUE ZEROS.
           05  PERC-E          PIC ZZZ,ZZ.
           05  PRAZO-W1        PIC 9(15)V99      VALUE ZEROS.
           05  PRAZO-W2        PIC 9(15)V99      VALUE ZEROS.
           05  CONTRATO-W      PIC 9(4)          VALUE ZEROS.
           05  VALOR-FOTO-W    PIC 9(3)V99       VALUE ZEROS.

           05  WS-ALBUM.
               10  WS-ALB1     PIC 9(04)         VALUE ZEROS.
               10  WS-ALB2     PIC 9(04)         VALUE ZEROS.

           05  QTDE-E                PIC ZZZ.
           05  QTDE-E1               PIC ZZ.
           05  QTDE-E2               PIC Z.
           05  QTDE-E3               PIC ZZZZ.ZZZ.
           05  QTDE-E4               PIC ZZZZ.
           05  QTDE-E5               PIC ZZ.ZZZ.
           05  QTDE-E6               PIC ZZZZZ.
           05  TOT-PARTICIPANTE      PIC 9(6)     VALUE ZEROS.
      *    CALCULO PM E JUROS
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  PM-E                  PIC ZZZ,ZZ   BLANK WHEN ZEROS.

           05  TOT1-ALBUM         PIC 9(4)     VALUE ZEROS.
           05  TOT1-FOTO          PIC 9(6)     VALUE ZEROS.
           05  TOT1-FITA          PIC 9(4)     VALUE ZEROS.
           05  TOT1-DVD           PIC 9(4)     VALUE ZEROS.
           05  TOT1-FOLHA         PIC 9(6)     VALUE ZEROS.
           05  TOT1-POSTER        PIC 9(3)     VALUE ZEROS.
           05  TOT1-PFITA         PIC 9(3)     VALUE ZEROS.
           05  TOT1-VENDA-BR      PIC 9(8)V99  VALUE ZEROS.
           05  TOT1-VENDA-IDX     PIC 9(8)V99  VALUE ZEROS.
           05  TOT1-VLR-UNIT-FOTO PIC 9(8)V99  VALUE ZEROS.
           05  TOT1-VLR-FOTO      PIC 9(8)V99  VALUE ZEROS.
           05  TOT1-VLR-ALBUM     PIC 9(8)V99  VALUE ZEROS.
           05  TOT1-VLR-FITA      PIC 9(8)V99  VALUE ZEROS.
           05  TOT1-COMISSAO      PIC 9(8)V99  VALUE ZEROS.
           05  TOT1-DEV-FOTO      PIC 9(5)     VALUE ZEROS.
           05  TOT1-DEV-FITA      PIC 9(3)     VALUE ZEROS.
           05  TOT1-DEV-DVD       PIC 9(3)     VALUE ZEROS.
           05  TOT1-DEV-ALBUM     PIC 9(3)     VALUE ZEROS.

      *    05  TOT2-FORM-PROD     PIC 9(4)     VALUE ZEROS.
      *    05  TOT2-FORM-VEND     PIC 9(4)     VALUE ZEROS.
      *    05  TOT2-FORM-SALD     PIC 9(4)     VALUE ZEROS.
      *    05  TOT2-FOTO-DISP     PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-FOTO-COMI     PIC 9(3)     VALUE ZEROS.
      *    05  TOT2-FOTO-VEND     PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-FOTO-DEVL     PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-FOTO-SALD     PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-FITA-VEND     PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-PERC-VEND     PIC 9(3)V99  VALUE ZEROS.
      *    05  TOT2-PERC-DEVL     PIC 9(3)V99  VALUE ZEROS.
      *    05  TOT2-PERC-SALD     PIC 9(3)V99  VALUE ZEROS.
      *    05  TOT2-VENDA-BR      PIC 9(8)V99  VALUE ZEROS.
      *    05  TOT2-VENDA-LIQ     PIC 9(8)V99  VALUE ZEROS.
      *    05  TOT2-PRECO-FOT     PIC 9(3)V99  VALUE ZEROS.
      *    05  TOT2-VENDA-CLI     PIC 9(4)V99  VALUE ZEROS.
      *    05  TOT2-FOGO          PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-AVULSA        PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-MONTADA       PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-PRODUZIDA     PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-FOTOS-MTG     PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-PERDIDA       PIC 9(6)     VALUE ZEROS.
      *    05  TOT2-PM-ACUM       PIC 9(11)V99 VALUE ZEROS.
      *    05  TOT2-AVULSA-VEND   PIC 9(6)     VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(41)   VALUE
           "RELAT.ACOMPANHAMENTO VENDAS-ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  DESC-ORDEM-REL      PIC X(38)   VALUE SPACES.
       01  CAB02A.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(41)   VALUE
           "VENDAS POR CONTRATO         ORDEM: ".
           05  ORDEM1-REL          PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  DESC1-ORDEM-REL     PIC X(38)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "
      -    "                       --FOTOS- VENDIDAS FOTOS".

       01  CAB05.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "ALBUM    NOME-CLIENTE      DDD-FONE    DATA-VDA VENDEDOR  VD
      -    "A-BRUTA  P.M. VLR-INDEX VEN(DEV) AL FI PF %VEND".

       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  CAB07.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "------------------------------ VENDAS ----------------------
      -    "--  DEVOL".

       01  CAB08.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "CLI.  FOTOS FITA PFT   VENDA-BRUTA  P.M. VENDA-INDEXAD %FOTO
      -    "S  FOTOS".

       01  CAB09.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "-- FORMANDOS-- ---------- FOTOGRAFIAS--------- VEND --RESULT
      -    " FOTOS %- FATURAMENT                  P-MED V.MED.".
       01  CAB10.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "PROD VEND SALD DISPON COM  VENDA DEVOLU  SALDO FITA VENDA DE
      -    "VOL SALDO VENDA-BRUT  P.M. VLIQ-INDEX R$FOT CLIENT".
       01  LINTOT.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINTOT-REL          PIC X(110)  VALUE SPACES.

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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
      *    MOVE "MTD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD001.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.
           ACCEPT VARIA-W2 FROM TIME.
           ADD 1 TO VARIA-W2
      *    OPEN OUTPUT WORK2  CLOSE WORK2  OPEN I-O WORK2.

           OPEN INPUT CEAD010 CGD001 COD040 RCD100 MTD020
                      CAD010 MTD019.
      *               MTD001.
           IF ST-CEAD010 <> "00"
              MOVE "ERRO ABERTURA CEAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CEAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF ST-MTD001 <> "00"
      *       MOVE "ERRO ABERTURA MTD001: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-MTD001 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM CHAMAR-POPUP-CONTRATO
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LE-CONTRATO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE "****" TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO.

       CHAMAR-POPUP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP040T".
           MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO.
           PERFORM LE-CONTRATO.

      *----------------------------------------------------------

       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK  CLOSE WORK   OPEN I-O WORK.
      *    CLOSE WORK2 OPEN OUTPUT WORK2 CLOSE WORK2  OPEN I-O WORK2.
           MOVE ZEROS TO PRAZO-W1 PRAZO-W2.
      *    MOVE ZEROS TO TOT2-FORM-PROD TOT2-FORM-VEND TOT2-FORM-SALD
      *                  TOT2-FOTO-DISP TOT2-FOTO-COMI TOT2-FOTO-VEND
      *                  TOT2-FOTO-SALD TOT2-FITA-VEND TOT2-PERC-VEND
      *                  TOT2-PERC-DEVL TOT2-PERC-SALD TOT2-VENDA-BR
      *                  TOT2-VENDA-LIQ TOT2-PRECO-FOT TOT2-VENDA-CLI
      *                  TOT2-MONTADA TOT2-AVULSA TOT2-FOGO TOT2-PERDIDA
      *                  TOT2-PRODUZIDA TOT2-FOTOS-MTG TOT2-PM-ACUM
      *                  TOT2-AVULSA-VEND.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE GS-CONTRATO    TO ALBUM-REC(1: 4)
           MOVE ZEROS          TO ALBUM-REC(5: 4)
           START RCD100 KEY IS NOT < ALBUM-REC INVALID KEY
                 MOVE "10" TO ST-RCD100
           END-START

           PERFORM UNTIL ST-RCD100 = "10"
             READ RCD100 NEXT RECORD AT END MOVE "10" TO ST-RCD100
              NOT AT END
               MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
               MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
               MOVE ALBUM-REC(1: 4)    TO CONTRATO-W

               IF CONTRATO-W <> GS-CONTRATO
                  MOVE "10" TO ST-RCD100
               ELSE PERFORM MOVER-DADOS-WORK
               END-IF

             END-READ
           END-PERFORM.

      *    ALBUM NAO VENDIDOS
           MOVE GS-CONTRATO    TO CONTRATO-MTG
           MOVE ZEROS          TO NRALBUM-MTG
           START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                 MOVE "10" TO ST-MTD020
           END-START

           PERFORM UNTIL ST-MTD020 = "10"
             READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
              NOT AT END
               IF CONTRATO-MTG <> GS-CONTRATO
                  MOVE "10" TO ST-MTD020
               ELSE
                  MOVE DATAMOV-MTG  TO GS-EXIBE-MOVTO
                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM

                  MOVE ALBUM-MTG          TO ALBUM-REC
                  READ RCD100 INVALID KEY
                       PERFORM MOVER-DADOS-WORK-MTG
                    NOT INVALID KEY
                       CONTINUE
                  END-READ
               END-IF
             END-READ
           END-PERFORM.

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

      *    MOVE ZEROS TO CONTRATO-WK2.
      *    START WORK2 KEY IS NOT < CONTRATO-WK2 INVALID KEY
      *          MOVE "10" TO ST-WORK2.
      *    PERFORM UNTIL ST-WORK2 = "10"
      *      READ WORK2 NEXT RECORD AT END MOVE "10" TO ST-WORK2
      *        NOT AT END
      *           ADD MONTADA-WK2   TO TOT2-MONTADA
      *           ADD PRODUZIDA-WK2 TO TOT2-PRODUZIDA
      *           ADD AVULSA-WK2    TO TOT2-AVULSA
      *           ADD PERDIDA-WK2   TO TOT2-PERDIDA
      *           ADD FORM-PROD-WK2 TO TOT2-FORM-PROD
      *      END-READ
      *    END-PERFORM.

       MOVER-DADOS-WORK SECTION.
      *    ADD 1                   TO TOT2-FORM-VEND
           MOVE DATA-MOVTO-REC     TO DTAMOV-WK.
           MOVE ALBUM-REC          TO ALBUM-WK ALBUMMT19

           READ MTD019 INVALID KEY
                MOVE SPACES                  TO NOME-CLIENTE-WK
                MOVE ZEROS                   TO FONE-WK
            NOT INVALID KEY
                MOVE NOME-FORM-MT19          TO NOME-CLIENTE-WK
                MOVE FONE-MT19               TO FONE-WK(4: 8)
                MOVE CIDADE-MT19             TO CIDADE

                READ CAD010 INVALID KEY
                     MOVE ZEROS              TO FONE-WK(1: 3)
                 NOT INVALID KEY
                     MOVE DDD-CID(2: 3)      TO FONE-WK(1: 3)
                END-READ

           END-READ

           IF QENCADER-REC IS NOT NUMERIC
              MOVE ZEROS           TO QENCADER-REC.
           MOVE QENCADER-REC       TO TOTALB-WK.

           MOVE DATAVEN-REC        TO DATA-VENDA-WK

           COMPUTE TOTFOT-WK = QFOTOS-REC + QABERTURA-REC
      *    ADD QFOTOS-REC          TO TOT2-FOTO-VEND
      *    ADD QABERTURA-REC       TO TOT2-FOTO-VEND
           MOVE QAVULSAS-REC       TO TOTAVUL-WK.
      *    ADD QAVULSAS-REC        TO TOT2-AVULSA-VEND.
           MOVE QCOMISSAO-REC      TO TOTCOMIS-WK.
      *    ADD QCOMISSAO-REC       TO TOT2-FOTO-COMI
           MOVE QFITAS-REC         TO TOTFITA-WK.
           MOVE QDVD-REC           TO TOTDVD-WK.
      *    ADD QFITAS-REC          TO TOT2-FITA-VEND.
           MOVE QPOSTER-REC        TO TOTPOSTER-WK.
           MOVE QPFITA-REC         TO TOTPORTA-FITA-WK.
           MOVE QFOLHAS-REC        TO TOTFOL-WK.
           MOVE TOTAL-REC          TO TOTVEN-WK.
      *    ADD TOTAL-REC           TO TOT2-VENDA-BR.
           MOVE PM-REC             TO PM-WK.
           MOVE TOTAL-DEF-REC      TO TOTVENIDX-WK.
      *    ADD TOTAL-DEF-REC       TO TOT2-VENDA-LIQ.
      *    COMPUTE TOT2-PM-ACUM = TOT2-PM-ACUM + (TOTAL-REC * PM-REC).

           MOVE VENDEDOR-REC       TO CODIGO-CG01
                                      COD-VENDEDOR-WK
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO VENDEDOR-WK.
           PERFORM GRAVA-DEVOLUCAO.
           WRITE REG-WORK.

       MOVER-DADOS-WORK-MTG SECTION.
           INITIALIZE REG-WORK

           MOVE QT-FOTOS-MTG       TO DEVOL-WK.
           MOVE QT-FITAS-MTG       TO DEVFITA-WK
           MOVE QT-DVD-MTG         TO DEVDVD-WK
           MOVE ALBUM-MTG          TO ALBUM-WK ALBUMMT19

           READ MTD019 INVALID KEY
                MOVE SPACES                  TO NOME-CLIENTE-WK
                MOVE ZEROS                   TO FONE-WK
            NOT INVALID KEY
                MOVE NOME-FORM-MT19          TO NOME-CLIENTE-WK
                MOVE FONE-MT19               TO FONE-WK(4: 8)
                MOVE CIDADE-MT19             TO CIDADE

                READ CAD010 INVALID KEY
                     MOVE ZEROS              TO FONE-WK(1: 3)
                 NOT INVALID KEY
                     MOVE DDD-CID(2: 3)      TO FONE-WK(1: 3)
                END-READ

           END-READ

           WRITE REG-WORK.

       GRAVA-DEVOLUCAO SECTION.
           MOVE ALBUM-REC      TO ALBUM-MTG.
           READ MTD020 INVALID KEY INITIALIZE REG-MTD020.
      *    MOVE ALBUM-REC(1: 4) TO CONTRATO-MT01.
      *    IF FOGO-MTG = 9 ADD QT-FOTOS-MTG   TO TOT2-FOGO
      *    ELSE ADD QT-FOTOS-MTG              TO TOT2-FOTOS-MTG
      *    READ MTD001.
      *
      *    MOVE CODALBUM-MT01   TO CODALB-WK.
      *    MOVE CODFOTO-MT01    TO CODFOT-WK.
      *    MOVE CONTRATO-MT01   TO CONTRATO-WK2.
      *    READ WORK2 INVALID KEY
      *         MOVE AVULSA-MT01      TO AVULSA-WK2
      *         MOVE PRODUZIDA-MT01   TO PRODUZIDA-WK2
      *         MOVE MONTADA-MT01     TO MONTADA-WK2
      *         MOVE PERDIDA-MT01     TO PERDIDA-WK2
      *         MOVE CLIEN-ALBUM-MT01 TO FORM-PROD-WK2
      *         WRITE REG-WORK2
      *         END-WRITE
      *    END-READ.
      *    COMPUTE SALDO-W = QT-FOTOS-MTG -
      *              (TOTFOT-WK + TOTCOMIS-WK + TOTAVUL-WK).
           COMPUTE SALDO-W = QT-FOTOS-MTG - (TOTFOT-WK + TOTCOMIS-WK).
           MOVE SALDO-W        TO DEVOL-WK.
           COMPUTE SALDO-W = QT-FITAS-MTG - TOTFITA-WK.
           MOVE SALDO-W        TO DEVFITA-WK.
           COMPUTE SALDO-W = QT-DVD-MTG - TOTDVD-WK.
           MOVE SALDO-W        TO DEVDVD-WK.
           COMPUTE SALDO-W = QT-ENCADER-MTG - TOTALB-WK.
           MOVE SALDO-W        TO ALBDEV-WK.
      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT1-ALBUM TOT1-FOTO TOT1-FITA TOT1-FOLHA
                         TOT1-PFITA TOT1-VENDA-BR TOT1-VENDA-IDX
                         TOT1-VLR-UNIT-FOTO TOT1-DEV-FOTO TOT1-DEV-FITA
                         TOT1-DEV-ALBUM TOT1-VLR-FOTO TOT1-VLR-ALBUM
                         TOT1-VLR-FITA TOT1-COMISSAO  TOT1-FITA
                         TOT1-DEV-FITA
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA.

       MOVER-DADOS-LINDET SECTION.
           MOVE ALBUM-WK          TO GS-LINDET(1: 9) WS-ALBUM

           MOVE NOME-CLIENTE-WK   TO GS-LINDET(10: 18)

           MOVE FONE-WK           TO GS-LINDET(28: 12)

           MOVE DATA-VENDA-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV(1: 4)    TO WS-DATA6(1: 4)
           MOVE DATA-INV(7: 2)    TO WS-DATA6(5: 2)
           MOVE WS-DATA6          TO DATA-E1
           MOVE DATA-E1           TO GS-LINDET(40: 09)

           MOVE VENDEDOR-WK       TO GS-LINDET(49: 10)

           MOVE TOTVEN-WK         TO VALOR-E1
           ADD TOTVEN-WK          TO TOT1-VENDA-BR
           MOVE VALOR-E1          TO GS-LINDET(59: 09)

           MOVE PM-WK             TO PM-E
           MOVE PM-E              TO GS-LINDET(68: 7)

           MOVE TOTVENIDX-WK      TO VALOR-E1
           ADD TOTVENIDX-WK       TO TOT1-VENDA-IDX
           MOVE VALOR-E1          TO GS-LINDET(75: 09)

           MOVE TOTFOT-WK         TO QTDE-E
           ADD TOTFOT-WK          TO TOT1-FOTO
           MOVE QTDE-E            TO GS-LINDET(84: 3)

           MOVE "("               TO GS-LINDET(87: 1)
           MOVE DEVOL-WK          TO QTDE-E
           ADD DEVOL-WK           TO TOT1-DEV-FOTO
           ADD DEVFITA-WK         TO TOT1-DEV-FITA
           ADD DEVDVD-WK          TO TOT1-DEV-DVD
           MOVE QTDE-E            TO GS-LINDET(88: 3)
           MOVE ")"               TO GS-LINDET(91: 2)

           MOVE TOTALB-WK         TO QTDE-E1
           IF WS-ALB2 NOT EQUAL ZEROS
              ADD 1               TO TOT1-ALBUM
           END-IF
           MOVE QTDE-E1           TO GS-LINDET(93: 3)

           MOVE TOTFITA-WK        TO QTDE-E1
           ADD TOTFITA-WK         TO TOT1-FITA
           MOVE QTDE-E1           TO GS-LINDET(96: 3)

           MOVE TOTPORTA-FITA-WK  TO QTDE-E1
           ADD TOTPORTA-FITA-WK   TO TOT1-PFITA
           MOVE QTDE-E1           TO GS-LINDET(99: 3)

           MOVE TOTDVD-WK         TO QTDE-E1
           ADD TOTDVD-WK          TO TOT1-DVD
           MOVE QTDE-E1           TO GS-LINDET(102: 3)


      *     PERCENTAGEM DE FOTOS VENDIDAS
           COMPUTE PERC-W = (TOTFOT-WK * 100) / (TOTFOT-WK + DEVOL-WK)
           MOVE PERC-W            TO PERC-E
           MOVE PERC-E            TO GS-LINDET(106: 6).



       ORDEM SECTION.

           INITIALIZE REG-WORK

           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "ALBUM"              TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALBUM-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "NOME CLIENTE"       TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < NOME-CLIENTE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "DATA VENDA  "       TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DATA-VENDA-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "VENDEDOR    "       TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < VENDEDOR-WK INVALID KEY
                      MOVE "10" TO ST-WORK

             WHEN 5
                MOVE "VALOR TOTAL "       TO GS-DESCR-ORDEM

                START WORK KEY IS NOT < TOTVEN-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "FITA        "       TO GS-DESCR-ORDEM

                START WORK KEY IS NOT < TOTFITA-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 7
                MOVE "PORTA FITA  "       TO GS-DESCR-ORDEM

                START WORK KEY IS NOT < TOTPORTA-FITA-WK INVALID KEY
                      MOVE "10" TO ST-WORK

           END-EVALUATE.

       TOTALIZA SECTION.
           MOVE SPACES               TO GS-LINTOT.
           MOVE TOT1-ALBUM           TO QTDE-E4
           MOVE QTDE-E4              TO GS-LINTOT(1: 5)
           MOVE TOT1-FOTO            TO QTDE-E5
           MOVE QTDE-E5              TO GS-LINTOT(6: 7)
           MOVE TOT1-FITA            TO QTDE-E4
           MOVE QTDE-E4              TO GS-LINTOT(13: 5)
           MOVE TOT1-PFITA           TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(20: 4)
           MOVE TOT1-DVD             TO QTDE-E4
           MOVE QTDE-E4              TO GS-LINTOT(24: 5)
           MOVE TOT1-VENDA-BR        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(29: 14)
           COMPUTE PRAZO-W = PRAZO-W1 / TOT1-VENDA-BR.
           MOVE PRAZO-W              TO PM-E
           MOVE PM-E                 TO GS-LINTOT(43: 6)
           MOVE TOT1-VENDA-IDX       TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(49: 14)
           COMPUTE PERC-W = (TOT1-FOTO * 100) / (TOT1-DEV-FOTO +
                                  TOT1-FOTO)

           MOVE PERC-W               TO PERC-E
           MOVE PERC-E               TO GS-LINTOT(64: 7)
           MOVE TOT1-DEV-FOTO        TO QTDE-E6
           MOVE QTDE-E6              TO GS-LINTOT(71: 5).
           MOVE TOT1-DEV-FITA        TO QTDE-E6
           MOVE QTDE-E6              TO GS-LINTOT(77: 5).
           MOVE TOT1-DEV-DVD         TO QTDE-E6
           MOVE QTDE-E6              TO GS-LINTOT(83: 5).

           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP208" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.


           IF GS-IMPR-RELATORIO-TRUE
              PERFORM MOVER-WORK-REL.

           COPY DESCONDENSA.

       MOVER-WORK-REL SECTION.
           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE ALBUM-WK          TO LINDET-REL(1: 9) WS-ALBUM

           MOVE NOME-CLIENTE-WK   TO LINDET-REL(10: 18)

           MOVE FONE-WK           TO LINDET-REL(28: 12)

           MOVE DATA-VENDA-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV(1: 4)    TO WS-DATA6(1: 4)
           MOVE DATA-INV(7: 2)    TO WS-DATA6(5: 2)
           MOVE WS-DATA6          TO DATA-E1
           MOVE DATA-E1           TO LINDET-REL(40: 09)

           MOVE VENDEDOR-WK       TO LINDET-REL(49: 10)

           MOVE TOTVEN-WK         TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(59: 09)

           MOVE PM-WK             TO PM-E
           MOVE PM-E              TO LINDET-REL(68: 7)

           MOVE TOTVENIDX-WK      TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(75: 09)

           MOVE TOTFOT-WK         TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(84: 3)

           MOVE "("               TO LINDET-REL(87: 1)
           MOVE DEVOL-WK          TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(88: 3)
           MOVE ")"               TO LINDET-REL(91: 2)

           MOVE TOTALB-WK         TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(93: 3)

           MOVE TOTFITA-WK        TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(96: 3)

           MOVE TOTPORTA-FITA-WK  TO QTDE-E1
           ADD TOTPORTA-FITA-WK   TO TOT1-PFITA
           MOVE QTDE-E1           TO LINDET-REL(99: 3)

           MOVE TOTDVD-WK         TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(102: 3)


      *     PERCENTAGEM DE FOTOS VENDIDAS
           COMPUTE PERC-W = (TOTFOT-WK * 100) / (TOTFOT-WK + DEVOL-WK)
           MOVE PERC-W            TO PERC-E
           MOVE PERC-E            TO LINDET-REL(105: 6).

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.


       TOTALIZA-REL SECTION.
           MOVE GS-LINTOT  TO LINTOT-REL.
           WRITE REG-RELAT FROM CAB07 AFTER 2.
           WRITE REG-RELAT FROM CAB08.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           MOVE "CONTRATO  "    TO ORDEM-REL
           MOVE "CONTRATO..: "  TO DESC-ORDEM-REL(1: 12)
           MOVE GS-CONTRATO     TO DESC-ORDEM-REL(13: 4)

           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           MOVE 2 TO LIN.

           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB05.
           WRITE REG-RELAT FROM CAB03.
           ADD 6 TO LIN.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CEAD010 COD040 CGD001 MTD020 RCD100
                 CAD010 MTD019 WORK.
      *          WORK2 MTD001.
           DELETE FILE WORK.
      *    DELETE FILE WORK2.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
