       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP061.
      *DATA: 22/09/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELATORIO/RESUMO GERAL DE VENDAS
      *        POR CONTRATO OU POR INTERVALO DE DATA(MOVTO OU VENDA)
      *        QTDE DE FOTOS VENDEDIDAS = QFOTOS-REC + QABERTURA-REC
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CAPX018.

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

           SELECT CHD041i ASSIGN TO PATH-CHD041i
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS CHAVE-41i = NOME-41i
                                            PARCELA-41i
                  WITH DUPLICATES
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CHD041i.


           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS NOME-ARQUIVO-WK
                  ALTERNATE RECORD KEY IS
                  DATA-BASE-WK WITH DUPLICATES.

           SELECT WORK1 ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK1
                  RECORD KEY IS CHAVE-WK1 = TIPO-WK1
                                            DATA-BASE-WK1
                                            NOME-ARQUIVO-WK1.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.
      *                 "TESTE.TXT"
      *                 ORGANIZATION IS LINE SEQUENTIAL
      *                 ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW018.

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

       FD  CHD041i.
       01  REG-CHD041i.
           05  NOME-41i              PIC X(16).
           05  PARCELA-41i           PIC 9(03).
           05  DIAS-41i              PIC 9(3)V99.
           05  TAXA-41i              PIC 9V999999.
           05  JURO-41i              PIC 9(10)V99.
           05  SALDO-41i             PIC 9(10)V99.

       FD  WORK.
       01  REG-WORK.
           05 DATA-BASE-WK           PIC 9(08).
           05 PORTADOR-WK            PIC X(20).
           05 PM-WK                  PIC 9(14)V99.
           05 VALOR-BRUTO-WK         PIC 9(12)V99.
           05 VALOR-LIQUIDO-WK       PIC 9(12)V99.
           05 VALOR-JUROS-WK         PIC 9(12)V99.
           05 NOME-ARQUIVO-WK        PIC X(16).

       FD  WORK1.
       01  REG-WORK1.
           05 TIPO-WK1               PIC X(04).
           05 DATA-BASE-WK1          PIC 9(08).
           05 PORTADOR-WK1           PIC X(20).
           05 VALOR-BRUTO-WK1        PIC 9(12)V99.
           05 VALOR-LIQUIDO-WK1      PIC 9(12)V99.
           05 VALOR-JUROS-WK1        PIC 9(12)V99.
           05 NOME-ARQUIVO-WK1       PIC X(16).


       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(150).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CHP061.CPB".
           COPY "CHP061.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CHD040             PIC XX       VALUE SPACES.
           05  ST-CHD041             PIC XX       VALUE SPACES.
           05  ST-CHD041i            PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  ST-WORK3              PIC XX       VALUE SPACES.
           05  ST-WORK5              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  AUX-TIPO              PIC X(04)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W3              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W5              PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  VALOR-E               PIC ZZ.ZZZ.ZZ9,99.
           05  VALOR-E2              PIC ZZ.ZZ9,99.
           05  QTDE-E                PIC ZZZ.ZZ9  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  VALOR-LIQUIDO         PIC 9(12)V99 VALUE ZEROS.
           05  VALOR-JUROS           PIC 9(12)V99 VALUE ZEROS.
           05  TOT-BRUTO             PIC 9(12)V99 VALUE ZEROS.
           05  TOT-LIQUIDO           PIC 9(12)V99 VALUE ZEROS.
           05  TOT-JUROS             PIC 9(12)V99 VALUE ZEROS.
           05  TOT-PM                PIC 9(12)V99 VALUE ZEROS.
           05  TOTAL-NEGOCIACAO      PIC 9(06)    VALUE ZEROS.
           05  TOTAL-NEGOCIACAO-A    PIC 9(06)    VALUE ZEROS.
           05  TOT-BRUTO-A           PIC 9(12)V99 VALUE ZEROS.
           05  TOT-JUROS-A           PIC 9(12)V99 VALUE ZEROS.
           05  TOT-LIQUIDO-A         PIC 9(12)V99 VALUE ZEROS.
           05  MASC-DIAS             PIC ZZ99.
           05  WS-ALBUM.
               10  WS-ALB1     PIC 9(04)         VALUE ZEROS.
               10  WS-ALB2     PIC 9(04)         VALUE ZEROS.

           05  TOT-PARTICIPANTE      PIC 9(6)     VALUE ZEROS.
      *    CALCULO PM E JUROS
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  PM-E                  PIC ZZ9,99.
           05  DIAS                  PIC 9(04) VALUE ZEROS.

           05  LIN                PIC 9(02)    VALUE ZEROS.
           05  PASSAR-STRING-1    PIC X(65).
           05  contador           pic 9(10).

       01 AUX-ALBUM.
          05 AUX-CONTRATO          PIC 9(04).
          05 AUX-CLIENTE           PIC 9(04).

           COPY "PARAMETR".

       01 lnkusu.
          copy usuario.cpy.

       01 WSSIZE                       pic 9(009) comp-5 value zeros.
       01 WSINDICE                     pic 9(009) comp-5 value zeros.
       01 UMITEM                       object reference value null.
       77 wsCheckEnable                pic 9(02) comp-5 value zeros.
       01 UMOBJETO                     object reference value null.
       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       01 indice              pic 9(02).
       01 lnktabela.
          02 lnkobjetoscol  object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01  CAB01.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(49)   VALUE
           "RELATORIO DOS CHEQUES DEFLACIONADOS - DATA BASE: ".
           05  DESC-ORDEM-REL      PIC X(38)   VALUE SPACES.
           05  FILLER              PIC X(03)   VALUE " - ".
           05  DESC-TIPO-REL       PIC X(15)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(21) VALUE
               "1º PORTADOR DESTINO: ".
           05 PORTADOR1-REL        PIC X(20).
       01  CAB04.
           05  FILLER              PIC X(21) VALUE
               "2º PORTADOR DESTINO: ".
           05 PORTADOR2-REL        PIC X(20).
       01  CAB05.
           05  FILLER              PIC X(21) VALUE
               "3º PORTADOR DESTINO: ".
           05 PORTADOR3-REL        PIC X(20).
       01  CAB06.
           05 DET-DESCRICAO        PIC X(100) VALUE SPACES.
       01  CAB-DET.
           05 FILLER               PIC X(130) VALUE ALL "-".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01  DET-01.
           05 FILLER               PIC X(4).
           05 DET-TIPO             PIC X(04).

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
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CHD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD040.
           MOVE "CHD041"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD041.
           MOVE "CHD041i" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD041i.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.
           ACCEPT VARIA-W1 FROM TIME.
           OPEN OUTPUT WORK1  CLOSE WORK1  OPEN I-O WORK1.

           OPEN I-O   CHD041I
           CLOSE      CHD041I
           OPEN INPUT CAD018 CHD040 CHD041 CAD004 CHD041i

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario

           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD040 <> "00"
              MOVE "ERRO ABERTURA CHD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD041i <> "00"
              MOVE "ERRO ABERTURA CHD041i:"  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD041i TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD041 <> "00"
              MOVE "ERRO ABERTURA CHD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD041 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM CHAMAR-POPUP-PORTADOR
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LE-PORTADOR
               WHEN GS-VERIFICA-PERMISSAO-TRUE
                    PERFORM VERIFICAR-PERMISSAO
               WHEN GS-TROCA-PORTADOR-TRUE
                    PERFORM TROCA-PORTADOR
               WHEN GS-CHAMAR-DEFLACAO-TRUE
                    PERFORM CHAMAR-DEFLACAO
               WHEN GS-CARREGAR-DETALHAMENTO-TRUE
                    PERFORM CARREGAR-DETALHAMENTO

           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Impressão" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)

          invoke gs-acp-listview "Checkboxes".

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke gs-acp-listview "gridLines"
          invoke gs-acp-listview "noBorder".


       CARREGAR-DETALHAMENTO SECTION.
           MOVE "LIMPAR-LB-DET" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-LINDET-ATE      TO DET-01
           INITIALIZE REG-WORK1
                      GS-CONT-DETALHAMENTO
                      TOTAL-NEGOCIACAO-A
                      TOT-BRUTO-A
                      TOT-JUROS-A
                      TOT-LIQUIDO-A

           MOVE DET-TIPO           TO TIPO-WK1
           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
                 MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
                 READ WORK1 NEXT AT END
                      MOVE "10" TO ST-WORK1
                 NOT AT END
                      IF DET-TIPO <> TIPO-WK1
                         MOVE "10" TO ST-WORK1
                      ELSE
                         MOVE SPACES TO GS-LINDET-DETALHAMENTO
                         ADD  1                 TO TOTAL-NEGOCIACAO-A
                         MOVE DATA-BASE-WK1     TO DATA-INV
                         CALL "GRIDAT1" USING DATA-INV
                         MOVE DATA-INV          TO DATA-E
                         MOVE DATA-E            TO
                              GS-LINDET-DETALHAMENTO(01:10)
                         MOVE PORTADOR-WK1      TO
                              GS-LINDET-DETALHAMENTO(12:20)
                         MOVE VALOR-BRUTO-WK1   TO VALOR-E
                         MOVE VALOR-E           TO
                              GS-LINDET-DETALHAMENTO(34:13)
                         MOVE VALOR-JUROS-WK1   TO VALOR-E
                         MOVE VALOR-E           TO
                              GS-LINDET-DETALHAMENTO(49:13)
                         MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
                         MOVE VALOR-E           TO
                              GS-LINDET-DETALHAMENTO(65:13)

                         MOVE "INSERE-LB-DETALHAMENTO" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM

                         ADD VALOR-BRUTO-WK1    TO TOT-BRUTO-A
                         ADD VALOR-JUROS-WK1    TO TOT-JUROS-A
                         ADD VALOR-LIQUIDO-WK1  TO TOT-LIQUIDO-A
                      END-IF
                 END-READ
           END-PERFORM.

           IF TOTAL-NEGOCIACAO-A > 0
              PERFORM TOTALIZA-A.

       TOTALIZA-A SECTION.
           MOVE ALL "-"              TO GS-LINDET-DETALHAMENTO
           MOVE "INSERE-LB-DETALHAMENTO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-DETALHAMENTO
           MOVE "TOTAL NEGOCIACAO "  TO GS-LINDET-DETALHAMENTO(1:17)
           MOVE TOTAL-NEGOCIACAO-A   TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET-DETALHAMENTO(18:7)
           MOVE TOT-BRUTO-A          TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-DETALHAMENTO(34:13)
           MOVE TOT-JUROS-A          TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-DETALHAMENTO(49:13)
           MOVE TOT-LIQUIDO-A        TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-DETALHAMENTO(65:13).
           MOVE "INSERE-LB-DETALHAMENTO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-DETALHAMENTO
           MOVE "MEDIA NEGOCIACAO "  TO GS-LINDET-DETALHAMENTO(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-A / TOTAL-NEGOCIACAO-A
           MOVE VALOR-E           TO GS-LINDET-DETALHAMENTO(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-A / TOTAL-NEGOCIACAO-A
           MOVE VALOR-E           TO GS-LINDET-DETALHAMENTO(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-A /
                                     TOTAL-NEGOCIACAO-A
           MOVE VALOR-E           TO GS-LINDET-DETALHAMENTO(65:13).
           MOVE "INSERE-LB-DETALHAMENTO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ALL "-"           TO GS-LINDET-DETALHAMENTO
           MOVE "INSERE-LB-DETALHAMENTO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win3 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VERIFICAR-PERMISSAO SECTION.
           MOVE "SENHA21"     TO PROGRAMA-CA004.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
                MOVE "Usuário Sem Permissão" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.

       TROCA-PORTADOR SECTION.
           CLOSE      CHD041
           OPEN I-O   CHD041

           MOVE GS-LINDET(89:16)       TO NOME-41
           READ CHD041 INVALID KEY
               MOVE "Deflação Não Encontrada" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE GS-PORTADOR-DESTINO TO PORTADOR-DESTINO-41
               REWRITE REG-CHD041 INVALID KEY
                   MOVE "Erro de Regravação...CHD041" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

           CLOSE      CHD041
           OPEN INPUT CHD041.

       CHAMAR-DEFLACAO SECTION.
           CALL   "CHP057T" USING PARAMETROS-W GS-LINDET(89:16)
           CANCEL "CHP057T".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-PORTADOR SECTION.
           EVALUATE GS-PORTADOR
               WHEN 1 MOVE GS-PORTADOR1  TO PORTADOR
               WHEN 2 MOVE GS-PORTADOR2  TO PORTADOR
               WHEN 3 MOVE GS-PORTADOR3  TO PORTADOR
               WHEN 4 MOVE GS-PORTADOR-DESTINO TO PORTADOR
           END-EVALUATE

           READ CAD018 INVALID KEY
               MOVE "****" TO NOME-PORT.

           EVALUATE GS-PORTADOR
               WHEN 1 MOVE NOME-PORT  TO GS-DESC-PORT1
               WHEN 2 MOVE NOME-PORT  TO GS-DESC-PORT2
               WHEN 3 MOVE NOME-PORT  TO GS-DESC-PORT3
               WHEN 4 MOVE NOME-PORT  TO GS-DESC-PORT-DESTINO
           END-EVALUATE.

       CHAMAR-POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP018T".
           EVALUATE GS-PORTADOR
               WHEN 1 MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR1
               WHEN 2 MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR2
               WHEN 3 MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR3
               WHEN 4 MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR-DESTINO
           END-EVALUATE
           PERFORM LE-PORTADOR.

      *----------------------------------------------------------
       INVERTE-DATA SECTION.
           MOVE GS-VECTO-INI TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-FIM.

       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK  CLOSE WORK   OPEN I-O WORK.
           CLOSE WORK1 OPEN OUTPUT WORK1 CLOSE WORK1  OPEN I-O WORK1.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           INITIALIZE REG-CHD041

           PERFORM INVERTE-DATA
           MOVE VECTO-INI               TO DATA-BASE-41
           START CHD041 KEY IS NOT LESS DATA-BASE-41 INVALID KEY
               MOVE "10" TO ST-CHD041.
           PERFORM UNTIL ST-CHD041 = "10"
               READ CHD041 NEXT AT END
                   MOVE "10" TO ST-CHD041
               NOT AT END
                   IF DATA-BASE-41 > VECTO-FIM
                      MOVE "10" TO ST-CHD041
                   ELSE
                      IF GS-PORTADOR1 = 0 OR (GS-PORTADOR1 =
                         PORTADOR-DESTINO-41 OR GS-PORTADOR2 =
                         PORTADOR-DESTINO-41 OR GS-PORTADOR3 =
                         PORTADOR-DESTINO-41)
                          ADD 1 TO CONTADOR
                          MOVE CONTADOR TO GS-EXIBE-MOVTO2
                          MOVE DATA-BASE-41    TO GS-EXIBE-MOVTO
                          MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                          PERFORM MOVER-DADOS-WORK
                      END-IF
                   END-IF
               END-READ
           END-PERFORM

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-WORK SECTION.
           MOVE ZEROS                   TO VALOR-JUROS
           MOVE DATA-BASE-41            TO DATA-BASE-WK
                                           DATA-BASE-WK1
           MOVE PORTADOR-DESTINO-41     TO PORTADOR
           READ CAD018 INVALID KEY
               MOVE "****"              TO NOME-PORT
           END-READ
           MOVE NOME-PORT               TO PORTADOR-WK
                                           PORTADOR-WK1
           MOVE VLR-BRUTO-41            TO VALOR-BRUTO-WK
                                           VALOR-BRUTO-WK1
           MOVE PM-41                   TO PM-WK
           MOVE NOME-41                 TO NOME-ARQUIVO-WK
                                           NOME-ARQUIVO-WK1

           INITIALIZE REG-CHD041I
           MOVE NOME-41                 TO NOME-41I
           START CHD041I KEY IS NOT LESS CHAVE-41i INVALID KEY
                 MOVE "10" TO ST-CHD041I.

           PERFORM UNTIL ST-CHD041I = "10"
                 READ CHD041I NEXT AT END
                      MOVE "10" TO ST-CHD041I
                 NOT AT END
                      IF NOME-41 <> NOME-41I
                         MOVE "10" TO ST-CHD041I
                      ELSE
                         COMPUTE DIAS = PARCELA-41i * 30

                         MOVE DIAS TO MASC-DIAS

                         IF DIAS > 999
                            MOVE DIAS         TO TIPO-WK1
                         ELSE
                            IF DIAS > 99
                               MOVE DIAS(2:3) TO TIPO-WK1
                            ELSE
                               MOVE DIAS(3:2) TO TIPO-WK1
                            END-IF
                         END-IF

                         MOVE MASC-DIAS      TO TIPO-WK1
                         MOVE SALDO-41I      TO VALOR-LIQUIDO
                                                VALOR-LIQUIDO-WK1
                         ADD  JURO-41I       TO VALOR-JUROS
                         MOVE JURO-41I       TO VALOR-JUROS-WK1
                         WRITE REG-WORK1 INVALID KEY
                              MOVE "Erro de Gravação...WORK1"
                                       TO MENSAGEM
                              MOVE "C" TO TIPO-MSG
                              PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                      END-IF
                 END-READ
           END-PERFORM

           MOVE VALOR-LIQUIDO                TO VALOR-LIQUIDO-WK
           MOVE VALOR-JUROS                  TO VALOR-JUROS-WK

           WRITE REG-WORK INVALID KEY
               MOVE "Erro de Gravação...WORK" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM.

      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-BRUTO TOT-LIQUIDO TOT-JUROS TOT-PM
                         TOT-BRUTO-A  TOT-LIQUIDO-A  TOT-JUROS-A
                         TOTAL-NEGOCIACAO
                         TOTAL-NEGOCIACAO-A
                         GS-CONT-ATE
                         GS-CONT-DETALHAMENTO

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK

           START WORK KEY IS NOT LESS DATA-BASE-WK INVALID KEY
               MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

           PERFORM TOTALIZA.


           INVOKE GS-ACP-LISTVIEW "DeleteAll"

           INITIALIZE INDICE
           INVOKE GS-ACP-LISTVIEW "ADICIONARITEM"
                                   RETURNING WSITEM

      *>Descrição
           ADD 1 TO INDICE
           INITIALIZE WSTEXTO

           STRING "Resumo " X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
             USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

           INITIALIZE REG-WORK1
                      AUX-TIPO
           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
               MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   IF TIPO-WK1 <> AUX-TIPO
                      INITIALIZE INDICE
                      INVOKE GS-ACP-LISTVIEW "ADICIONARITEM"
                                              RETURNING WSITEM

      *>Descrição
                      ADD 1 TO INDICE
                      INITIALIZE WSTEXTO

                      STRING "Até " TIPO-WK1 X"00" INTO WSTEXTO
                      INVOKE GS-ACP-LISTVIEW "PREENCHERCOLUNAZ"
                        USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO


                      MOVE SPACES   TO GS-LINDET-ATE
                      MOVE TIPO-WK1 TO AUX-TIPO
                                       DET-TIPO
                      MOVE DET-01   TO GS-LINDET-ATE
                      MOVE "INSERE-LB-ATE" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
              END-READ
           END-PERFORM

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-chp061" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.

       mostrar-fonte-favo section.
           move "listview-chp061" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       zebrar-itens section.
           move "listview-chp061" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-acp-listview wsTexto
           invoke gs-acp-listview "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-chp061" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.

       MOVER-DADOS-LINDET SECTION.
           MOVE SPACES TO GS-LINDET
           ADD  1                 TO TOTAL-NEGOCIACAO
           MOVE DATA-BASE-WK      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(01:10)
           MOVE PORTADOR-WK       TO GS-LINDET(12:20)
           MOVE VALOR-BRUTO-WK    TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(34:13)
           MOVE PM-WK             TO VALOR-E2
           MOVE VALOR-E2          TO GS-LINDET(49:9)
           MOVE VALOR-JUROS-WK    TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(59:13)
           MOVE VALOR-LIQUIDO-WK  TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(74:13)
           MOVE NOME-ARQUIVO-WK   TO GS-LINDET(89:16)

           ADD VALOR-BRUTO-WK     TO TOT-BRUTO
           ADD PM-WK              TO TOT-PM
           ADD VALOR-JUROS-WK     TO TOT-JUROS
           ADD VALOR-LIQUIDO-WK   TO TOT-LIQUIDO.

       TOTALIZA SECTION.
           MOVE ALL "-"           TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES            TO GS-LINDET
           MOVE "TOTAL NEGOCIACAO " TO GS-LINDET(1:17)
           MOVE TOTAL-NEGOCIACAO  TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(18:7)
           MOVE TOT-BRUTO         TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(34:13)
           MOVE TOT-PM            TO VALOR-E2
           MOVE VALOR-E2          TO GS-LINDET(49:9)
           MOVE TOT-JUROS         TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(59:13)
           MOVE TOT-LIQUIDO       TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(74:13).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES            TO GS-LINDET
           MOVE "MEDIA NEGOCIACAO " TO GS-LINDET(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO / TOTAL-NEGOCIACAO
           MOVE VALOR-E           TO GS-LINDET(34:13)
           COMPUTE VALOR-E2 ROUNDED = TOT-PM / TOTAL-NEGOCIACAO
           MOVE VALOR-E2          TO GS-LINDET(49:9)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS / TOTAL-NEGOCIACAO
           MOVE VALOR-E           TO GS-LINDET(59:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO / TOTAL-NEGOCIACAO
           MOVE VALOR-E           TO GS-LINDET(74:13).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ALL "-"           TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CHP061" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           INVOKE GS-ACP-LISTVIEW "Size" RETURNING WSSIZE

           move 1 to wsIndice
           PERFORM WSSIZE TIMES
              add 1 to wsIndice
              invoke gs-acp-listview "itemAtIndex"
                     using wsIndice returning umItem

              if umItem not equal null
                 initialize wsCheckEnable
                 invoke umItem "getCheckBoxValue"
                        returning wsCheckEnable

                 if wsCheckEnable equal 1
                    invoke umItem "getColumnValue"
                                  using lnkColunas(1)
                              returning umObjeto
                    initialize wsTexto
                    invoke umObjeto "getValue" returning wsTexto

                    if wsTexto(1:6) = "Resumo"
                       MOVE "   DATA    PORTADOR                VALOR BR
      -                     "UTO         PM   VALOR JUROS  VALOR LIQUIDO
      -                     "  NOME ARQUIVO"
                         TO DET-DESCRICAO
                       MOVE "RESUMO" TO DESC-TIPO-REL
                       PERFORM MOVER-WORK-RESUMO-REL
                    else
                       MOVE "   DATA    PORTADOR                VALOR BR
      -                     "UTO    VALOR JUROS   VALOR LIQUIDO"
                         TO DET-DESCRICAO

                       MOVE WSTEXTO TO DESC-TIPO-REL
                       PERFORM CABECALHO
                       PERFORM IMPRIMIR-DADOS
                    end-if
                 end-if
           END-PERFORM

           COPY DESCONDENSA.


       IMPRIMIR-DADOS SECTION.
           INITIALIZE REG-WORK1
                      TOT-BRUTO-A  TOT-LIQUIDO-A  TOT-JUROS-A
                      TOTAL-NEGOCIACAO-A


           MOVE WSTEXTO(5:4) TO AUX-TIPO

           MOVE AUX-TIPO TO TIPO-WK1

           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
               MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   IF TIPO-WK1 <> AUX-TIPO
                      MOVE "10" TO ST-WORK1
                   ELSE
                      ADD 1                  TO TOTAL-NEGOCIACAO-A
                      ADD VALOR-BRUTO-WK1    TO TOT-BRUTO-A
                      ADD VALOR-JUROS-WK1    TO TOT-JUROS-A
                      ADD VALOR-LIQUIDO-WK1  TO TOT-LIQUIDO-A

                      MOVE SPACES TO LINDET-REL
                      MOVE DATA-BASE-WK1     TO DATA-INV
                      CALL "GRIDAT1" USING DATA-INV
                      MOVE DATA-INV          TO DATA-E
                      MOVE DATA-E            TO LINDET-REL(01:10)
                      MOVE PORTADOR-WK1      TO LINDET-REL(12:20)
                      MOVE VALOR-BRUTO-WK1   TO VALOR-E
                      MOVE VALOR-E           TO LINDET-REL(34:13)
                      MOVE VALOR-JUROS-WK1   TO VALOR-E
                      MOVE VALOR-E           TO LINDET-REL(49:13)
                      MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
                      MOVE VALOR-E           TO LINDET-REL(65:13)
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                   END-IF
              END-READ
           END-PERFORM

           MOVE ALL "-"              TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.

           MOVE SPACES               TO LINDET-REL
           MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
           MOVE TOTAL-NEGOCIACAO-A   TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(18:7)
           MOVE TOT-BRUTO-A          TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(34:13)
           MOVE TOT-JUROS-A          TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(49:13)
           MOVE TOT-LIQUIDO-A        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(65:13).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL
           MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-A / TOTAL-NEGOCIACAO-A
           MOVE VALOR-E           TO LINDET-REL(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-A  / TOTAL-NEGOCIACAO-A
           MOVE VALOR-E           TO LINDET-REL(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-A /
                                     TOTAL-NEGOCIACAO-A
           MOVE VALOR-E           TO LINDET-REL(65:13).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE ALL "-"           TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.


       MOVER-WORK-RESUMO-REL SECTION.
            MOVE ZEROS TO LIN.
            PERFORM CABECALHO.
            INITIALIZE REG-WORK
            START WORK KEY IS NOT LESS DATA-BASE-WK INVALID KEY
                  MOVE "10" TO ST-WORK.

            PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    PERFORM MOVER-DADOS-RELATORIO
               END-READ
            END-PERFORM.
            PERFORM TOTALIZA-REL.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE DATA-BASE-WK      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01:10)
           MOVE PORTADOR-WK       TO LINDET-REL(12:20)
           MOVE VALOR-BRUTO-WK    TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(34:13)
           MOVE PM-WK             TO VALOR-E2
           MOVE VALOR-E2          TO LINDET-REL(49:9)
           MOVE VALOR-JUROS-WK    TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(59:13)
           MOVE VALOR-LIQUIDO-WK  TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(74:13)
           MOVE NOME-ARQUIVO-WK   TO LINDET-REL(89:16)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.

       TOTALIZA-REL SECTION.
           MOVE ALL "-"           TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE SPACES            TO LINDET-REL
           MOVE "TOTAL NEGOCIACAO " TO LINDET-REL(1:17)
           MOVE TOTAL-NEGOCIACAO  TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(18:7)
           MOVE TOT-BRUTO         TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(34:13)
           MOVE TOT-PM            TO VALOR-E2
           MOVE VALOR-E2          TO LINDET-REL(49:9)
           MOVE TOT-JUROS         TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(59:13)
           MOVE TOT-LIQUIDO       TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(74:13).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE SPACES            TO LINDET-REL
           MOVE "MEDIA NEGOCIACAO " TO LINDET-REL(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO / TOTAL-NEGOCIACAO
           MOVE VALOR-E           TO LINDET-REL(34:13)
           COMPUTE VALOR-E2 ROUNDED = TOT-PM / TOTAL-NEGOCIACAO
           MOVE VALOR-E2          TO LINDET-REL(49:9)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS / TOTAL-NEGOCIACAO
           MOVE VALOR-E           TO LINDET-REL(59:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO / TOTAL-NEGOCIACAO
           MOVE VALOR-E           TO LINDET-REL(74:13).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE ALL "-"           TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.

       IMPRIME-ATE-30 SECTION.
           INITIALIZE REG-WORK1
           MOVE "30" TO TIPO-WK1

           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
               MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   IF TIPO-WK1 <> "30"
                      MOVE "10" TO ST-WORK1
                   ELSE
                      MOVE SPACES TO LINDET-REL
                      MOVE DATA-BASE-WK1     TO DATA-INV
                      CALL "GRIDAT1" USING DATA-INV
                      MOVE DATA-INV          TO DATA-E
                      MOVE DATA-E            TO LINDET-REL(01:10)
                      MOVE PORTADOR-WK1      TO LINDET-REL(12:20)
                      MOVE VALOR-BRUTO-WK1   TO VALOR-E
                      MOVE VALOR-E           TO LINDET-REL(34:13)
                      MOVE VALOR-JUROS-WK1   TO VALOR-E
                      MOVE VALOR-E           TO LINDET-REL(49:13)
                      MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
                      MOVE VALOR-E           TO LINDET-REL(65:13)
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                   END-IF
              END-READ
           END-PERFORM
           MOVE ALL "-"              TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
      *    MOVE TOTAL-NEGOCIACAO-30  TO QTDE-E
      *    MOVE QTDE-E               TO LINDET-REL(18:7)
      *    MOVE TOT-BRUTO-30         TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(34:13)
      *    MOVE TOT-JUROS-30         TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(49:13)
      *    MOVE TOT-LIQUIDO-30       TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
      *    COMPUTE VALOR-E ROUNDED = TOT-BRUTO-30 / TOTAL-NEGOCIACAO-30
      *    MOVE VALOR-E           TO LINDET-REL(34:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-JUROS-30 / TOTAL-NEGOCIACAO-30
      *    MOVE VALOR-E           TO LINDET-REL(49:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-30 /
      *                              TOTAL-NEGOCIACAO-30
      *    MOVE VALOR-E           TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE ALL "-"           TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *
      *IMPRIME-ATE-60 SECTION.
      *    INITIALIZE REG-WORK1
      *    MOVE "60" TO TIPO-WK1
      *
      *    START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
      *        MOVE "10" TO ST-WORK1.
      *
      *    PERFORM UNTIL ST-WORK1 = "10"
      *       READ WORK1 NEXT RECORD AT END
      *            MOVE "10" TO ST-WORK1
      *       NOT AT END
      *            IF TIPO-WK1 <> "60"
      *               MOVE "10" TO ST-WORK1
      *            ELSE
      *               MOVE SPACES TO LINDET-REL
      *               MOVE DATA-BASE-WK1     TO DATA-INV
      *               CALL "GRIDAT1" USING DATA-INV
      *               MOVE DATA-INV          TO DATA-E
      *               MOVE DATA-E            TO LINDET-REL(01:10)
      *               MOVE PORTADOR-WK1      TO LINDET-REL(12:20)
      *               MOVE VALOR-BRUTO-WK1   TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(34:13)
      *               MOVE VALOR-JUROS-WK1   TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(49:13)
      *               MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(65:13)
      *               WRITE REG-RELAT FROM LINDET
      *               ADD 1 TO LIN
      *               IF LIN > 56
      *                  PERFORM CABECALHO
      *               END-IF
      *            END-IF
      *       END-READ
      *    END-PERFORM
      *    MOVE ALL "-"              TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
      *    MOVE TOTAL-NEGOCIACAO-60  TO QTDE-E
      *    MOVE QTDE-E               TO LINDET-REL(18:7)
      *    MOVE TOT-BRUTO-60         TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(34:13)
      *    MOVE TOT-JUROS-60         TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(49:13)
      *    MOVE TOT-LIQUIDO-60       TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
      *    COMPUTE VALOR-E ROUNDED = TOT-BRUTO-60 / TOTAL-NEGOCIACAO-60
      *    MOVE VALOR-E           TO LINDET-REL(34:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-JUROS-60 / TOTAL-NEGOCIACAO-60
      *    MOVE VALOR-E           TO LINDET-REL(49:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-60 /
      *                              TOTAL-NEGOCIACAO-60
      *    MOVE VALOR-E           TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE ALL "-"           TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *
      *IMPRIME-ATE-90 SECTION.
      *    INITIALIZE REG-WORK1
      *    MOVE "90" TO TIPO-WK1
      *
      *    START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
      *        MOVE "10" TO ST-WORK1.
      *
      *    PERFORM UNTIL ST-WORK1 = "10"
      *       READ WORK1 NEXT RECORD AT END
      *            MOVE "10" TO ST-WORK1
      *       NOT AT END
      *            IF TIPO-WK1 <> "90"
      *               MOVE "10" TO ST-WORK1
      *            ELSE
      *               MOVE SPACES TO LINDET-REL
      *               MOVE DATA-BASE-WK1     TO DATA-INV
      *               CALL "GRIDAT1" USING DATA-INV
      *               MOVE DATA-INV          TO DATA-E
      *               MOVE DATA-E            TO LINDET-REL(01:10)
      *               MOVE PORTADOR-WK1      TO LINDET-REL(12:20)
      *               MOVE VALOR-BRUTO-WK1   TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(34:13)
      *               MOVE VALOR-JUROS-WK1   TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(49:13)
      *               MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(65:13)
      *               WRITE REG-RELAT FROM LINDET
      *               ADD 1 TO LIN
      *               IF LIN > 56
      *                  PERFORM CABECALHO
      *               END-IF
      *            END-IF
      *       END-READ
      *    END-PERFORM
      *    MOVE ALL "-"              TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
      *    MOVE TOTAL-NEGOCIACAO-90  TO QTDE-E
      *    MOVE QTDE-E               TO LINDET-REL(18:7)
      *    MOVE TOT-BRUTO-90         TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(34:13)
      *    MOVE TOT-JUROS-90         TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(49:13)
      *    MOVE TOT-LIQUIDO-90       TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
      *    COMPUTE VALOR-E ROUNDED = TOT-BRUTO-90 / TOTAL-NEGOCIACAO-90
      *    MOVE VALOR-E           TO LINDET-REL(34:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-JUROS-90 / TOTAL-NEGOCIACAO-90
      *    MOVE VALOR-E           TO LINDET-REL(49:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-90 /
      *                              TOTAL-NEGOCIACAO-90
      *    MOVE VALOR-E           TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE ALL "-"           TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *
      *IMPRIME-ATE-120 SECTION.
      *    INITIALIZE REG-WORK1
      *    MOVE "120" TO TIPO-WK1
      *
      *    START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
      *        MOVE "10" TO ST-WORK1.
      *
      *    PERFORM UNTIL ST-WORK1 = "10"
      *       READ WORK1 NEXT RECORD AT END
      *            MOVE "10" TO ST-WORK1
      *       NOT AT END
      *            IF TIPO-WK1 <> "120"
      *               MOVE "10" TO ST-WORK1
      *            ELSE
      *               MOVE SPACES TO LINDET-REL
      *               MOVE DATA-BASE-WK1     TO DATA-INV
      *               CALL "GRIDAT1" USING DATA-INV
      *               MOVE DATA-INV          TO DATA-E
      *               MOVE DATA-E            TO LINDET-REL(01:10)
      *               MOVE PORTADOR-WK1      TO LINDET-REL(12:20)
      *               MOVE VALOR-BRUTO-WK1   TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(34:13)
      *               MOVE VALOR-JUROS-WK1   TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(49:13)
      *               MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(65:13)
      *               WRITE REG-RELAT FROM LINDET
      *               ADD 1 TO LIN
      *               IF LIN > 56
      *                  PERFORM CABECALHO
      *               END-IF
      *            END-IF
      *       END-READ
      *    END-PERFORM
      *    MOVE ALL "-"              TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
      *    MOVE TOTAL-NEGOCIACAO-120 TO QTDE-E
      *    MOVE QTDE-E               TO LINDET-REL(18:7)
      *    MOVE TOT-BRUTO-120        TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(34:13)
      *    MOVE TOT-JUROS-120        TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(49:13)
      *    MOVE TOT-LIQUIDO-120      TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
      *    COMPUTE VALOR-E ROUNDED = TOT-BRUTO-120/ TOTAL-NEGOCIACAO-120
      *    MOVE VALOR-E           TO LINDET-REL(34:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-JUROS-120/ TOTAL-NEGOCIACAO-120
      *    MOVE VALOR-E           TO LINDET-REL(49:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-120/
      *                              TOTAL-NEGOCIACAO-120
      *    MOVE VALOR-E           TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE ALL "-"           TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *
      *IMPRIME-ATE-150 SECTION.
      *    INITIALIZE REG-WORK1
      *    MOVE "150" TO TIPO-WK1
      *
      *    START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
      *        MOVE "10" TO ST-WORK1.
      *
      *    PERFORM UNTIL ST-WORK1 = "10"
      *       READ WORK1 NEXT RECORD AT END
      *            MOVE "10" TO ST-WORK1
      *       NOT AT END
      *            IF TIPO-WK1 <> "150"
      *               MOVE "10" TO ST-WORK1
      *            ELSE
      *               MOVE SPACES TO LINDET-REL
      *               MOVE DATA-BASE-WK1     TO DATA-INV
      *               CALL "GRIDAT1" USING DATA-INV
      *               MOVE DATA-INV          TO DATA-E
      *               MOVE DATA-E            TO LINDET-REL(01:10)
      *               MOVE PORTADOR-WK1      TO LINDET-REL(12:20)
      *               MOVE VALOR-BRUTO-WK1   TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(34:13)
      *               MOVE VALOR-JUROS-WK1   TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(49:13)
      *               MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(65:13)
      *               WRITE REG-RELAT FROM LINDET
      *               ADD 1 TO LIN
      *               IF LIN > 56
      *                  PERFORM CABECALHO
      *               END-IF
      *            END-IF
      *       END-READ
      *    END-PERFORM
      *    MOVE ALL "-"              TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
      *    MOVE TOTAL-NEGOCIACAO-150 TO QTDE-E
      *    MOVE QTDE-E               TO LINDET-REL(18:7)
      *    MOVE TOT-BRUTO-150        TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(34:13)
      *    MOVE TOT-JUROS-150        TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(49:13)
      *    MOVE TOT-LIQUIDO-150      TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
      *    COMPUTE VALOR-E ROUNDED = TOT-BRUTO-150/ TOTAL-NEGOCIACAO-150
      *    MOVE VALOR-E           TO LINDET-REL(34:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-JUROS-150/ TOTAL-NEGOCIACAO-150
      *    MOVE VALOR-E           TO LINDET-REL(49:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-150/
      *                              TOTAL-NEGOCIACAO-150
      *    MOVE VALOR-E           TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE ALL "-"           TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *
      *IMPRIME-ATE-180 SECTION.
      *    INITIALIZE REG-WORK1
      *    MOVE "180" TO TIPO-WK1
      *
      *    START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
      *        MOVE "10" TO ST-WORK1.
      *
      *    PERFORM UNTIL ST-WORK1 = "10"
      *       READ WORK1 NEXT RECORD AT END
      *            MOVE "10" TO ST-WORK1
      *       NOT AT END
      *            IF TIPO-WK1 <> "180"
      *               MOVE "10" TO ST-WORK1
      *            ELSE
      *               MOVE SPACES TO LINDET-REL
      *               MOVE DATA-BASE-WK1     TO DATA-INV
      *               CALL "GRIDAT1" USING DATA-INV
      *               MOVE DATA-INV          TO DATA-E
      *               MOVE DATA-E            TO LINDET-REL(01:10)
      *               MOVE PORTADOR-WK1      TO LINDET-REL(12:20)
      *               MOVE VALOR-BRUTO-WK1   TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(34:13)
      *               MOVE VALOR-JUROS-WK1   TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(49:13)
      *               MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
      *               MOVE VALOR-E           TO LINDET-REL(65:13)
      *               WRITE REG-RELAT FROM LINDET
      *               ADD 1 TO LIN
      *               IF LIN > 56
      *                  PERFORM CABECALHO
      *               END-IF
      *            END-IF
      *       END-READ
      *    END-PERFORM
      *    MOVE ALL "-"              TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
      *    MOVE TOTAL-NEGOCIACAO-180 TO QTDE-E
      *    MOVE QTDE-E               TO LINDET-REL(18:7)
      *    MOVE TOT-BRUTO-180        TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(34:13)
      *    MOVE TOT-JUROS-180        TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(49:13)
      *    MOVE TOT-LIQUIDO-180      TO VALOR-E
      *    MOVE VALOR-E              TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE SPACES               TO LINDET-REL
      *    MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
      *    COMPUTE VALOR-E ROUNDED = TOT-BRUTO-180/ TOTAL-NEGOCIACAO-180
      *    MOVE VALOR-E           TO LINDET-REL(34:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-JUROS-180/ TOTAL-NEGOCIACAO-180
      *    MOVE VALOR-E           TO LINDET-REL(49:13)
      *    COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-180/
      *                              TOTAL-NEGOCIACAO-180
      *    MOVE VALOR-E           TO LINDET-REL(65:13).
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.
      *    MOVE ALL "-"           TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET
      *    ADD 1 TO LIN
      *    IF LIN > 56
      *       PERFORM CABECALHO.

       CABECALHO SECTION.
           MOVE GS-DESC-PORT1   TO PORTADOR1-REL
           MOVE GS-DESC-PORT2   TO PORTADOR2-REL
           MOVE GS-DESC-PORT3   TO PORTADOR3-REL

           MOVE "DATA BASE: "   TO DESC-ORDEM-REL(1: 12)
           PERFORM MOVER-DATA-REL

           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02.
           MOVE 2 TO LIN.

           WRITE REG-RELAT FROM CAB-DET
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB05.
           WRITE REG-RELAT FROM CAB06.
           WRITE REG-RELAT FROM CAB-DET.
           ADD 7 TO LIN.

       MOVER-DATA-REL SECTION.
           MOVE GS-VECTO-INI    TO DATA-E
           MOVE DATA-E          TO DESC-ORDEM-REL(13: 11)
           MOVE "a"             TO DESC-ORDEM-REL(24: 2)
           MOVE GS-VECTO-FIM    TO DATA-E
           MOVE DATA-E          TO DESC-ORDEM-REL(26: 10).

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to gs-flag-critica
           move spaces to mensagem.


       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD018 CHD040 CHD041 CAD004 CHD041I
                 WORK WORK1
           DELETE FILE WORK.
           DELETE FILE WORK1.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
           STOP RUN.

