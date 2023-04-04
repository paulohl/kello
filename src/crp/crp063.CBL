       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP063.
      *DATA: 13/09/2006
      *AUTOR: ALFREDO SAVIOLLI NETO
      *FUNÇÃO: RELATORIO DOS TITULOS DEFLACIONADOS

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
           COPY CRPX040.
           COPY CRPX041.

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

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW018.
       COPY CRPW040.
       COPY CRPW041.

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
           05 TIPO-WK1               PIC X(03).
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
           COPY "CRP063.CPB".
           COPY "CRP063.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD040             PIC XX       VALUE SPACES.
           05  ST-CRD041             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  ST-WORK3              PIC XX       VALUE SPACES.
           05  ST-WORK5              PIC XX       VALUE SPACES.
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
           05  TOT-BRUTO-30          PIC 9(12)V99 VALUE ZEROS.
           05  TOT-BRUTO-60          PIC 9(12)V99 VALUE ZEROS.
           05  TOT-BRUTO-90          PIC 9(12)V99 VALUE ZEROS.
           05  TOT-BRUTO-120         PIC 9(12)V99 VALUE ZEROS.
           05  TOT-BRUTO-150         PIC 9(12)V99 VALUE ZEROS.
           05  TOT-BRUTO-180         PIC 9(12)V99 VALUE ZEROS.
           05  TOT-LIQUIDO           PIC 9(12)V99 VALUE ZEROS.
           05  TOT-LIQUIDO-30        PIC 9(12)V99 VALUE ZEROS.
           05  TOT-LIQUIDO-60        PIC 9(12)V99 VALUE ZEROS.
           05  TOT-LIQUIDO-90        PIC 9(12)V99 VALUE ZEROS.
           05  TOT-LIQUIDO-120       PIC 9(12)V99 VALUE ZEROS.
           05  TOT-LIQUIDO-150       PIC 9(12)V99 VALUE ZEROS.
           05  TOT-LIQUIDO-180       PIC 9(12)V99 VALUE ZEROS.
           05  TOT-JUROS             PIC 9(12)V99 VALUE ZEROS.
           05  TOT-JUROS-30          PIC 9(12)V99 VALUE ZEROS.
           05  TOT-JUROS-60          PIC 9(12)V99 VALUE ZEROS.
           05  TOT-JUROS-90          PIC 9(12)V99 VALUE ZEROS.
           05  TOT-JUROS-120         PIC 9(12)V99 VALUE ZEROS.
           05  TOT-JUROS-150         PIC 9(12)V99 VALUE ZEROS.
           05  TOT-JUROS-180         PIC 9(12)V99 VALUE ZEROS.
           05  TOT-PM                PIC 9(12)V99 VALUE ZEROS.
           05  TOTAL-NEGOCIACAO      PIC 9(06)    VALUE ZEROS.
           05  TOTAL-NEGOCIACAO-30   PIC 9(06)    VALUE ZEROS.
           05  TOTAL-NEGOCIACAO-60   PIC 9(06)    VALUE ZEROS.
           05  TOTAL-NEGOCIACAO-90   PIC 9(06)    VALUE ZEROS.
           05  TOTAL-NEGOCIACAO-120  PIC 9(06)    VALUE ZEROS.
           05  TOTAL-NEGOCIACAO-150  PIC 9(06)    VALUE ZEROS.
           05  TOTAL-NEGOCIACAO-180  PIC 9(06)    VALUE ZEROS.
           05  WS-ALBUM.
               10  WS-ALB1     PIC 9(04)         VALUE ZEROS.
               10  WS-ALB2     PIC 9(04)         VALUE ZEROS.

           05  TOT-PARTICIPANTE      PIC 9(6)     VALUE ZEROS.
      *    CALCULO PM E JUROS
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  PM-E                  PIC ZZ9,99.

           05  LIN                PIC 9(02)    VALUE ZEROS.
           05  PASSAR-STRING-1    PIC X(65).
           05  contador           pic 9(10).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 AUX-ALBUM.
          05 AUX-CONTRATO          PIC 9(04).
          05 AUX-CLIENTE           PIC 9(04).

           COPY "PARAMETR".

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
           MOVE "CRD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD040.
           MOVE "CRD041"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD041.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.
           ACCEPT VARIA-W1 FROM TIME.
           OPEN OUTPUT WORK1  CLOSE WORK1  OPEN I-O WORK1.

           OPEN INPUT CAD018 CRD040 CRD041 CAD004
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD040 <> "00"
              MOVE "ERRO ABERTURA CRD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD041 <> "00"
              MOVE "ERRO ABERTURA CRD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD041 TO GS-MENSAGEM-ERRO(23: 02)
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
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
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
           CLOSE      CRD041
           OPEN I-O   CRD041

           MOVE GS-LINDET(89:16)       TO NOME-41
           READ CRD041 INVALID KEY
               MOVE "Deflação Não Encontrada" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE GS-PORTADOR-DESTINO TO PORTADOR-DESTINO-41
               REWRITE REG-CRD041 INVALID KEY
                   MOVE "Erro de Regravação...CRD041" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

           CLOSE      CRD041
           OPEN INPUT CRD041.

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

           INITIALIZE REG-CRD041

           PERFORM INVERTE-DATA
           MOVE VECTO-INI               TO DATA-BASE-41
           START CRD041 KEY IS NOT LESS DATA-BASE-41 INVALID KEY
               MOVE "10" TO ST-CRD041.
           PERFORM UNTIL ST-CRD041 = "10"
               READ CRD041 NEXT AT END
                   MOVE "10" TO ST-CRD041
               NOT AT END
                   IF DATA-BASE-41 > VECTO-FIM
                      MOVE "10" TO ST-CRD041
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
           IF JURO-30-41 > 0
              MOVE "30"                      TO TIPO-WK1
              MOVE SALDO-30-41               TO VALOR-LIQUIDO
                                                VALOR-LIQUIDO-WK1
              ADD  JURO-30-41                TO VALOR-JUROS
              MOVE JURO-30-41                TO VALOR-JUROS-WK1
              WRITE REG-WORK1 INVALID KEY
                   MOVE "Erro de Gravação" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              END-WRITE
           END-IF
           IF JURO-60-41 > 0
              MOVE "60"                      TO TIPO-WK1
              MOVE SALDO-60-41               TO VALOR-LIQUIDO
                                                VALOR-LIQUIDO-WK1
              ADD  JURO-60-41                TO VALOR-JUROS
              MOVE JURO-60-41                TO VALOR-JUROS-WK1
              WRITE REG-WORK1 INVALID KEY
                   MOVE "Erro de Gravação" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              END-WRITE
           END-IF
           IF JURO-90-41 > 0
              MOVE "90"                      TO TIPO-WK1
              MOVE SALDO-90-41               TO VALOR-LIQUIDO
                                                VALOR-LIQUIDO-WK1
              ADD  JURO-90-41                TO VALOR-JUROS
              MOVE JURO-90-41                TO VALOR-JUROS-WK1
              WRITE REG-WORK1 INVALID KEY
                   MOVE "Erro de Gravação" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              END-WRITE
           END-IF
           IF JURO-120-41 > 0
              MOVE "120"                     TO TIPO-WK1
              MOVE SALDO-120-41               TO VALOR-LIQUIDO
                                                VALOR-LIQUIDO-WK1
              ADD  JURO-120-41                TO VALOR-JUROS
              MOVE JURO-120-41                TO VALOR-JUROS-WK1
              WRITE REG-WORK1 INVALID KEY
                   MOVE "Erro de Gravação" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              END-WRITE
           END-IF
           IF JURO-150-41 > 0
              MOVE "150"                      TO TIPO-WK1
              MOVE SALDO-150-41               TO VALOR-LIQUIDO
                                                VALOR-LIQUIDO-WK1
              ADD  JURO-150-41                TO VALOR-JUROS
              MOVE JURO-150-41                TO VALOR-JUROS-WK1
              WRITE REG-WORK1 INVALID KEY
                   MOVE "Erro de Gravação" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              END-WRITE
           END-IF
           IF JURO-180-41 > 0
              MOVE "180"                      TO TIPO-WK1
              MOVE SALDO-180-41               TO VALOR-LIQUIDO
                                                 VALOR-LIQUIDO-WK1
              ADD  JURO-180-41                TO VALOR-JUROS
              MOVE JURO-180-41                TO VALOR-JUROS-WK1
              WRITE REG-WORK1 INVALID KEY
                   MOVE "Erro de Gravação" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              END-WRITE
           END-IF
           MOVE VALOR-LIQUIDO                TO VALOR-LIQUIDO-WK
           MOVE VALOR-JUROS                  TO VALOR-JUROS-WK

           WRITE REG-WORK.

      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-BRUTO TOT-LIQUIDO TOT-JUROS TOT-PM
                         TOT-BRUTO-30  TOT-LIQUIDO-30  TOT-JUROS-30
                         TOT-BRUTO-60  TOT-LIQUIDO-60  TOT-JUROS-60
                         TOT-BRUTO-90  TOT-LIQUIDO-90  TOT-JUROS-90
                         TOT-BRUTO-120 TOT-LIQUIDO-120 TOT-JUROS-120
                         TOT-BRUTO-150 TOT-LIQUIDO-150 TOT-JUROS-150
                         TOT-BRUTO-180 TOT-LIQUIDO-180 TOT-JUROS-180
                         TOTAL-NEGOCIACAO
                         TOTAL-NEGOCIACAO-30
                         TOTAL-NEGOCIACAO-60
                         TOTAL-NEGOCIACAO-90
                         TOTAL-NEGOCIACAO-120
                         TOTAL-NEGOCIACAO-150
                         TOTAL-NEGOCIACAO-180
                         GS-CONT-30
                         GS-CONT-60
                         GS-CONT-90
                         GS-CONT-120
                         GS-CONT-150
                         GS-CONT-180

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

           INITIALIZE REG-WORK1

           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
               MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   EVALUATE TIPO-WK1
                       WHEN "30"  MOVE SPACES TO GS-LINDET-30
                                  PERFORM MOVER-DADOS-LB-30
                                  MOVE "INSERE-LB-30" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                       WHEN "60"  MOVE SPACES TO GS-LINDET-60
                                  PERFORM MOVER-DADOS-LB-60
                                  MOVE "INSERE-LB-60" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                       WHEN "90"  MOVE SPACES TO GS-LINDET-90
                                  PERFORM MOVER-DADOS-LB-90
                                  MOVE "INSERE-LB-90" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                       WHEN "120" MOVE SPACES TO GS-LINDET-120
                                  PERFORM MOVER-DADOS-LB-120
                                  MOVE "INSERE-LB-120" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                       WHEN "150" MOVE SPACES TO GS-LINDET-150
                                  PERFORM MOVER-DADOS-LB-150
                                  MOVE "INSERE-LB-150" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                       WHEN "180" MOVE SPACES TO GS-LINDET-180
                                  PERFORM MOVER-DADOS-LB-180
                                  MOVE "INSERE-LB-180" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                   END-EVALUATE
              END-READ
           END-PERFORM.

           IF TOTAL-NEGOCIACAO-30 > 0
              PERFORM TOTALIZA-30.

           IF TOTAL-NEGOCIACAO-60 > 0
              PERFORM TOTALIZA-60.

           IF TOTAL-NEGOCIACAO-90 > 0
              PERFORM TOTALIZA-90.

           IF TOTAL-NEGOCIACAO-120 > 0
              PERFORM TOTALIZA-120.

           IF TOTAL-NEGOCIACAO-150 > 0
              PERFORM TOTALIZA-150.

           IF TOTAL-NEGOCIACAO-180 > 0
              PERFORM TOTALIZA-180.

       MOVER-DADOS-LB-30 SECTION.
           ADD  1                 TO TOTAL-NEGOCIACAO-30
           MOVE DATA-BASE-WK1     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET-30(01:10)
           MOVE PORTADOR-WK1      TO GS-LINDET-30(12:20)
           MOVE VALOR-BRUTO-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-30(34:13)
           MOVE VALOR-JUROS-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-30(49:13)
           MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-30(65:13)

           ADD VALOR-BRUTO-WK1    TO TOT-BRUTO-30
           ADD VALOR-JUROS-WK1    TO TOT-JUROS-30
           ADD VALOR-LIQUIDO-WK1  TO TOT-LIQUIDO-30.

       MOVER-DADOS-LB-60 SECTION.
           ADD  1                 TO TOTAL-NEGOCIACAO-60
           MOVE DATA-BASE-WK1     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET-60(01:10)
           MOVE PORTADOR-WK1      TO GS-LINDET-60(12:20)
           MOVE VALOR-BRUTO-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-60(34:13)
           MOVE VALOR-JUROS-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-60(49:13)
           MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-60(65:13)

           ADD VALOR-BRUTO-WK1    TO TOT-BRUTO-60
           ADD VALOR-JUROS-WK1    TO TOT-JUROS-60
           ADD VALOR-LIQUIDO-WK1  TO TOT-LIQUIDO-60.

       MOVER-DADOS-LB-90 SECTION.
           ADD  1                 TO TOTAL-NEGOCIACAO-90
           MOVE DATA-BASE-WK1     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET-90(01:10)
           MOVE PORTADOR-WK1      TO GS-LINDET-90(12:20)
           MOVE VALOR-BRUTO-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-90(34:13)
           MOVE VALOR-JUROS-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-90(49:13)
           MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-90(65:13)

           ADD VALOR-BRUTO-WK1    TO TOT-BRUTO-90
           ADD VALOR-JUROS-WK1    TO TOT-JUROS-90
           ADD VALOR-LIQUIDO-WK1  TO TOT-LIQUIDO-90.

       MOVER-DADOS-LB-120 SECTION.
           ADD  1                 TO TOTAL-NEGOCIACAO-120
           MOVE DATA-BASE-WK1     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET-120(01:10)
           MOVE PORTADOR-WK1      TO GS-LINDET-120(12:20)
           MOVE VALOR-BRUTO-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-120(34:13)
           MOVE VALOR-JUROS-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-120(49:13)
           MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-120(65:13)

           ADD VALOR-BRUTO-WK1    TO TOT-BRUTO-120
           ADD VALOR-JUROS-WK1    TO TOT-JUROS-120
           ADD VALOR-LIQUIDO-WK1  TO TOT-LIQUIDO-120.

       MOVER-DADOS-LB-150 SECTION.
           ADD  1                 TO TOTAL-NEGOCIACAO-150
           MOVE DATA-BASE-WK1     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET-150(01:10)
           MOVE PORTADOR-WK1      TO GS-LINDET-150(12:20)
           MOVE VALOR-BRUTO-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-150(34:13)
           MOVE VALOR-JUROS-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-150(49:13)
           MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-150(65:13)

           ADD VALOR-BRUTO-WK1    TO TOT-BRUTO-150
           ADD VALOR-JUROS-WK1    TO TOT-JUROS-150
           ADD VALOR-LIQUIDO-WK1  TO TOT-LIQUIDO-150.

       MOVER-DADOS-LB-180 SECTION.
           ADD  1                 TO TOTAL-NEGOCIACAO-180
           MOVE DATA-BASE-WK1     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET-180(01:10)
           MOVE PORTADOR-WK1      TO GS-LINDET-180(12:20)
           MOVE VALOR-BRUTO-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-180(34:13)
           MOVE VALOR-JUROS-WK1   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-180(49:13)
           MOVE VALOR-LIQUIDO-WK1 TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET-180(65:13)

           ADD VALOR-BRUTO-WK1    TO TOT-BRUTO-180
           ADD VALOR-JUROS-WK1    TO TOT-JUROS-180
           ADD VALOR-LIQUIDO-WK1  TO TOT-LIQUIDO-180.

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

       TOTALIZA-30 SECTION.
           MOVE ALL "-"              TO GS-LINDET-30
           MOVE "INSERE-LB-30"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-30
           MOVE "TOTAL NEGOCIACAO "  TO GS-LINDET-30(1:17)
           MOVE TOTAL-NEGOCIACAO-30  TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET-30(18:7)
           MOVE TOT-BRUTO-30         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-30(34:13)
           MOVE TOT-JUROS-30         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-30(49:13)
           MOVE TOT-LIQUIDO-30       TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-30(65:13).
           MOVE "INSERE-LB-30" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-30
           MOVE "MEDIA NEGOCIACAO "  TO GS-LINDET-30(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-30 / TOTAL-NEGOCIACAO-30
           MOVE VALOR-E           TO GS-LINDET-30(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-30 / TOTAL-NEGOCIACAO-30
           MOVE VALOR-E           TO GS-LINDET-30(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-30 /
                                     TOTAL-NEGOCIACAO-30
           MOVE VALOR-E           TO GS-LINDET-30(65:13).
           MOVE "INSERE-LB-30" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ALL "-"           TO GS-LINDET-30
           MOVE "INSERE-LB-30" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA-60 SECTION.
           MOVE ALL "-"              TO GS-LINDET-60
           MOVE "INSERE-LB-60"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-60
           MOVE "TOTAL NEGOCIACAO "  TO GS-LINDET-60(1:17)
           MOVE TOTAL-NEGOCIACAO-60  TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET-60(18:7)
           MOVE TOT-BRUTO-60         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-60(34:13)
           MOVE TOT-JUROS-60         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-60(49:13)
           MOVE TOT-LIQUIDO-60       TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-60(65:13).
           MOVE "INSERE-LB-60" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-60
           MOVE "MEDIA NEGOCIACAO "  TO GS-LINDET-60(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-60 / TOTAL-NEGOCIACAO-60
           MOVE VALOR-E           TO GS-LINDET-60(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-60 / TOTAL-NEGOCIACAO-60
           MOVE VALOR-E           TO GS-LINDET-60(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-60 /
                                     TOTAL-NEGOCIACAO-60
           MOVE VALOR-E           TO GS-LINDET-60(65:13).
           MOVE "INSERE-LB-60" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ALL "-"           TO GS-LINDET-60
           MOVE "INSERE-LB-60" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA-90 SECTION.
           MOVE ALL "-"              TO GS-LINDET-90
           MOVE "INSERE-LB-90"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-90
           MOVE "TOTAL NEGOCIACAO "  TO GS-LINDET-90(1:17)
           MOVE TOTAL-NEGOCIACAO-90  TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET-90(18:7)
           MOVE TOT-BRUTO-90         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-90(34:13)
           MOVE TOT-JUROS-90         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-90(49:13)
           MOVE TOT-LIQUIDO-90       TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-90(65:13).
           MOVE "INSERE-LB-90" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-90
           MOVE "MEDIA NEGOCIACAO "  TO GS-LINDET-90(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-90 / TOTAL-NEGOCIACAO-90
           MOVE VALOR-E           TO GS-LINDET-90(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-90 / TOTAL-NEGOCIACAO-90
           MOVE VALOR-E           TO GS-LINDET-90(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-90 /
                                     TOTAL-NEGOCIACAO-90
           MOVE VALOR-E           TO GS-LINDET-90(65:13).
           MOVE "INSERE-LB-90" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ALL "-"           TO GS-LINDET-90
           MOVE "INSERE-LB-90" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA-120 SECTION.
           MOVE ALL "-"              TO GS-LINDET-120
           MOVE "INSERE-LB-120"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-120
           MOVE "TOTAL NEGOCIACAO "  TO GS-LINDET-120(1:17)
           MOVE TOTAL-NEGOCIACAO-120  TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET-120(18:7)
           MOVE TOT-BRUTO-120         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-120(34:13)
           MOVE TOT-JUROS-120         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-120(49:13)
           MOVE TOT-LIQUIDO-120       TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-120(65:13).
           MOVE "INSERE-LB-120" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-120
           MOVE "MEDIA NEGOCIACAO "  TO GS-LINDET-120(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-120 /
                                     TOTAL-NEGOCIACAO-120
           MOVE VALOR-E           TO GS-LINDET-120(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-120 /
                                     TOTAL-NEGOCIACAO-120
           MOVE VALOR-E           TO GS-LINDET-120(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-120 /
                                     TOTAL-NEGOCIACAO-120
           MOVE VALOR-E           TO GS-LINDET-120(65:13).
           MOVE "INSERE-LB-120" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ALL "-"           TO GS-LINDET-120
           MOVE "INSERE-LB-120" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA-150 SECTION.
           MOVE ALL "-"              TO GS-LINDET-150
           MOVE "INSERE-LB-150"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-150
           MOVE "TOTAL NEGOCIACAO "  TO GS-LINDET-150(1:17)
           MOVE TOTAL-NEGOCIACAO-150  TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET-150(18:7)
           MOVE TOT-BRUTO-150         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-150(34:13)
           MOVE TOT-JUROS-150         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-150(49:13)
           MOVE TOT-LIQUIDO-150       TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-150(65:13).
           MOVE "INSERE-LB-150" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-150
           MOVE "MEDIA NEGOCIACAO "  TO GS-LINDET-150(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-150 /
                                     TOTAL-NEGOCIACAO-150
           MOVE VALOR-E           TO GS-LINDET-150(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-150 /
                                     TOTAL-NEGOCIACAO-150
           MOVE VALOR-E           TO GS-LINDET-150(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-150 /
                                     TOTAL-NEGOCIACAO-150
           MOVE VALOR-E           TO GS-LINDET-150(65:13).
           MOVE "INSERE-LB-150" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ALL "-"           TO GS-LINDET-150
           MOVE "INSERE-LB-150" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA-180 SECTION.
           MOVE ALL "-"              TO GS-LINDET-180
           MOVE "INSERE-LB-180"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-180
           MOVE "TOTAL NEGOCIACAO "  TO GS-LINDET-180(1:17)
           MOVE TOTAL-NEGOCIACAO-180  TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET-180(18:7)
           MOVE TOT-BRUTO-180         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-180(34:13)
           MOVE TOT-JUROS-180         TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-180(49:13)
           MOVE TOT-LIQUIDO-180       TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET-180(65:13).
           MOVE "INSERE-LB-180" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINDET-180
           MOVE "MEDIA NEGOCIACAO "  TO GS-LINDET-180(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-180 /
                                     TOTAL-NEGOCIACAO-180
           MOVE VALOR-E           TO GS-LINDET-180(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-180 /
                                     TOTAL-NEGOCIACAO-180
           MOVE VALOR-E           TO GS-LINDET-180(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-180 /
                                     TOTAL-NEGOCIACAO-180
           MOVE VALOR-E           TO GS-LINDET-180(65:13).
           MOVE "INSERE-LB-180" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ALL "-"           TO GS-LINDET-180
           MOVE "INSERE-LB-180" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP063" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           IF GS-RESUMO-TRUE
              MOVE
              "   DATA    PORTADOR                VALOR BRUTO         PM
      -   "   VALOR JUROS  VALOR LIQUIDO  NOME ARQUIVO" TO DET-DESCRICAO
              MOVE "RESUMO" TO DESC-TIPO-REL
              PERFORM MOVER-WORK-RESUMO-REL.

           MOVE "   DATA    PORTADOR                VALOR BRUTO    VALOR
      -    " JUROS   VALOR LIQUIDO" TO DET-DESCRICAO

           IF GS-ATE-30-TRUE
              MOVE "ATE 30" TO DESC-TIPO-REL
              PERFORM CABECALHO
              PERFORM IMPRIME-ATE-30.

           IF GS-ATE-60-TRUE
              MOVE "ATE 60" TO DESC-TIPO-REL
              PERFORM CABECALHO
              PERFORM IMPRIME-ATE-60.

           IF GS-ATE-90-TRUE
              MOVE "ATE 90" TO DESC-TIPO-REL
              PERFORM CABECALHO
              PERFORM IMPRIME-ATE-90.

           IF GS-ATE-120-TRUE
              MOVE "ATE 120" TO DESC-TIPO-REL
              PERFORM CABECALHO
              PERFORM IMPRIME-ATE-120.

           IF GS-ATE-150-TRUE
              MOVE "ATE 150" TO DESC-TIPO-REL
              PERFORM CABECALHO
              PERFORM IMPRIME-ATE-150.

           IF GS-ATE-180-TRUE
              MOVE "ATE 180" TO DESC-TIPO-REL
              PERFORM CABECALHO
              PERFORM IMPRIME-ATE-180.

           COPY DESCONDENSA.

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
           MOVE SPACES               TO LINDET-REL
           MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
           MOVE TOTAL-NEGOCIACAO-30  TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(18:7)
           MOVE TOT-BRUTO-30         TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(34:13)
           MOVE TOT-JUROS-30         TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(49:13)
           MOVE TOT-LIQUIDO-30       TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(65:13).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL
           MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-30 / TOTAL-NEGOCIACAO-30
           MOVE VALOR-E           TO LINDET-REL(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-30 / TOTAL-NEGOCIACAO-30
           MOVE VALOR-E           TO LINDET-REL(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-30 /
                                     TOTAL-NEGOCIACAO-30
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

       IMPRIME-ATE-60 SECTION.
           INITIALIZE REG-WORK1
           MOVE "60" TO TIPO-WK1

           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
               MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   IF TIPO-WK1 <> "60"
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
           MOVE SPACES               TO LINDET-REL
           MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
           MOVE TOTAL-NEGOCIACAO-60  TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(18:7)
           MOVE TOT-BRUTO-60         TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(34:13)
           MOVE TOT-JUROS-60         TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(49:13)
           MOVE TOT-LIQUIDO-60       TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(65:13).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL
           MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-60 / TOTAL-NEGOCIACAO-60
           MOVE VALOR-E           TO LINDET-REL(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-60 / TOTAL-NEGOCIACAO-60
           MOVE VALOR-E           TO LINDET-REL(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-60 /
                                     TOTAL-NEGOCIACAO-60
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

       IMPRIME-ATE-90 SECTION.
           INITIALIZE REG-WORK1
           MOVE "90" TO TIPO-WK1

           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
               MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   IF TIPO-WK1 <> "90"
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
           MOVE SPACES               TO LINDET-REL
           MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
           MOVE TOTAL-NEGOCIACAO-90  TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(18:7)
           MOVE TOT-BRUTO-90         TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(34:13)
           MOVE TOT-JUROS-90         TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(49:13)
           MOVE TOT-LIQUIDO-90       TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(65:13).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL
           MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-90 / TOTAL-NEGOCIACAO-90
           MOVE VALOR-E           TO LINDET-REL(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-90 / TOTAL-NEGOCIACAO-90
           MOVE VALOR-E           TO LINDET-REL(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-90 /
                                     TOTAL-NEGOCIACAO-90
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

       IMPRIME-ATE-120 SECTION.
           INITIALIZE REG-WORK1
           MOVE "120" TO TIPO-WK1

           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
               MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   IF TIPO-WK1 <> "120"
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
           MOVE SPACES               TO LINDET-REL
           MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
           MOVE TOTAL-NEGOCIACAO-120 TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(18:7)
           MOVE TOT-BRUTO-120        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(34:13)
           MOVE TOT-JUROS-120        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(49:13)
           MOVE TOT-LIQUIDO-120      TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(65:13).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL
           MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-120/ TOTAL-NEGOCIACAO-120
           MOVE VALOR-E           TO LINDET-REL(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-120/ TOTAL-NEGOCIACAO-120
           MOVE VALOR-E           TO LINDET-REL(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-120/
                                     TOTAL-NEGOCIACAO-120
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

       IMPRIME-ATE-150 SECTION.
           INITIALIZE REG-WORK1
           MOVE "150" TO TIPO-WK1

           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
               MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   IF TIPO-WK1 <> "150"
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
           MOVE SPACES               TO LINDET-REL
           MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
           MOVE TOTAL-NEGOCIACAO-150 TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(18:7)
           MOVE TOT-BRUTO-150        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(34:13)
           MOVE TOT-JUROS-150        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(49:13)
           MOVE TOT-LIQUIDO-150      TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(65:13).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL
           MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-150/ TOTAL-NEGOCIACAO-150
           MOVE VALOR-E           TO LINDET-REL(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-150/ TOTAL-NEGOCIACAO-150
           MOVE VALOR-E           TO LINDET-REL(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-150/
                                     TOTAL-NEGOCIACAO-150
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

       IMPRIME-ATE-180 SECTION.
           INITIALIZE REG-WORK1
           MOVE "180" TO TIPO-WK1

           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
               MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   IF TIPO-WK1 <> "180"
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
           MOVE SPACES               TO LINDET-REL
           MOVE "TOTAL NEGOCIACAO "  TO LINDET-REL(1:17)
           MOVE TOTAL-NEGOCIACAO-180 TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(18:7)
           MOVE TOT-BRUTO-180        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(34:13)
           MOVE TOT-JUROS-180        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(49:13)
           MOVE TOT-LIQUIDO-180      TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(65:13).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL
           MOVE "MEDIA NEGOCIACAO "  TO LINDET-REL(1:17)
           COMPUTE VALOR-E ROUNDED = TOT-BRUTO-180/ TOTAL-NEGOCIACAO-180
           MOVE VALOR-E           TO LINDET-REL(34:13)
           COMPUTE VALOR-E ROUNDED = TOT-JUROS-180/ TOTAL-NEGOCIACAO-180
           MOVE VALOR-E           TO LINDET-REL(49:13)
           COMPUTE VALOR-E ROUNDED = TOT-LIQUIDO-180/
                                     TOTAL-NEGOCIACAO-180
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
           CLOSE CAD018 CRD040 CRD041 CAD004
                 WORK WORK1
           DELETE FILE WORK.
           DELETE FILE WORK1.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
           STOP RUN.

