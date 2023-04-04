       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPP063.
      *DATA: 30-04-2006
      *AUTOR: ALFREDO SAVIOLLI NETO
      *FUNÇÃO: Extrato de Fornecedor individual
      *        Listar todos os títulos com situação = 0(pagar) e 2(pago)
      *        dentro do intervalo de movimento solicitado.

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CGPX001.
           COPY CPPX020.
           COPY CPPX021.
           COPY CPPX024.
           COPY LOGX001.
           COPY LOGX003.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = FORNEC-WK SEQ-WK
                  ALTERNATE RECORD KEY IS PADRAO-WK =
                                          DATA-MOVTO-WK
                                          VENCTO-WK
                                          VALOR-WK
                                          DATA-PAGTO-WK
                                          FORNECEDOR-WK
                                          FORMA-PAGTO-WK
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CH-FORNECEDOR-WK =
                                          FORNECEDOR-WK
                                          DATA-MOVTO-WK
                                          VENCTO-WK
                                          VALOR-WK
                                          DATA-PAGTO-WK
                                          FORMA-PAGTO-WK
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CH-VENCTO-WK =
                                          VENCTO-WK
                                          VALOR-WK
                                          DATA-MOVTO-WK
                                          FORNECEDOR-WK
                                          DATA-PAGTO-WK
                                          FORMA-PAGTO-WK
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CH-VALOR-WK =
                                          VALOR-WK
                                          VENCTO-WK
                                          DATA-MOVTO-WK
                                          FORNECEDOR-WK
                                          DATA-PAGTO-WK
                                          FORMA-PAGTO-WK
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CH-FORMA-PAGTO-WK =
                                          FORMA-PAGTO-WK
                                          VENCTO-WK
                                          VALOR-WK
                                          VALOR-WK
                                          DATA-MOVTO-WK
                                          FORNECEDOR-WK
                                          DATA-PAGTO-WK
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CH-DATA-PAGTO-WK =
                                          DATA-PAGTO-WK
                                          VENCTO-WK
                                          VALOR-WK
                                          DATA-MOVTO-WK
                                          FORNECEDOR-WK
                                          FORMA-PAGTO-WK
                  WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CGPW001.
       COPY CPPW020.
       COPY CPPW021.
       COPY CPPW024.
       COPY LOGW001.
       COPY LOGW003.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  FORNEC-WK           PIC 9(6).
           05  SEQ-WK              PIC 9(5).
           05  DOCUMENTO-WK        PIC X(10).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  VENCTO-WK           PIC 9(8).
           05  DATA-PAGTO-WK       PIC 9(8).
           05  DIAS-ATRAS-WK       PIC 9(3).
           05  DESCRICAO-WK        PIC X(30).
           05  VALOR-WK            PIC 9(8)V99.
           05  VALOR-PAGO-WK       PIC 9(8)V99.
           05  SITUACAO-WK         PIC 9.
           05  FORNECEDOR-WK       PIC X(30).
           05  FORMA-PAGTO-WK      PIC X(15).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CPP063.CPB".
           COPY "CPP063.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD021             PIC XX       VALUE SPACES.
           05  ST-CPD024             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-LOG003             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIBERADO              PIC X(01)    VALUE SPACES.
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
           05  MOVTO-INI             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  PAGTO-INI             PIC 9(8)     VALUE ZEROS.
           05  PAGTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  PAGTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  PAGTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  MASC-VALOR            PIC Z.ZZZ.ZZ9,99.
      *    BLANK WHEN ZEROS.
           05  DATA-E                PIC 99/99/9999.
      *    BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  VALOR-ACUM            PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-ACUM            PIC 9(8)V99  VALUE ZEROS.
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  TOT-TITULO            PIC 9(3)     VALUE ZEROS.
           05  TOT-VALOR             PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VALOR-PAGO        PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ATRAS-MEDIO       PIC 9(3)V99  VALUE ZEROS.
           05  TOT-VALOR-A-PAGAR     PIC 9(8)V99  VALUE ZEROS.
           05  ATRASO-MEDIO-E        PIC ZZZ,ZZ.
           05  SEQ-CAIXA             PIC 9(3)     VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           COPY "PARAMETR".
       01  CAB01.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RELACAO DE CONTAS A PAGAR - ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(14)   VALUE "INTERV.MOVTO: ".
           05  MOVTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MOVTO-FIM-REL       PIC 99/99/9999.
       01  CAB02A.
           05  FILLER              PIC X(12)   VALUE "FORNECEDOR: ".
           05  FORNECEDOR-REL      PIC ZZZ.ZZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-FORNEC-REL     PIC X(30)   VALUE ZEROS.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(14)   VALUE "INTERV.PAGTO: ".
           05  PAGTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  PAGTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-MOVTO  N.DOCTO    DESCRICAO                      PO
      -    "   VALOR DATA-VECTO DATA-PAGTO  VALOR-PAGO DIAS".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(100)  VALUE
           "QTDE TITULOS     VALOR-TOTAL        VALOR PAGO  ATRAS.MED
      -    "  VALOR A PAGAR".
       01  LINTOT.
           05  LINTOT-REL          PIC X(100)  VALUE SPACES.

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

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       01  TIPO-MSG                        PIC X(01).
       01  RESP-MSG                        PIC X(01).
       01  MENSAGEM                        PIC X(200).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CPP052-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CPP052-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CPP052-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CPP052-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CPD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD021.
           MOVE "CPD024"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD024.
           MOVE "LOG001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOG003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOG003.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CGD001

           OPEN I-O CPD020 CPD021 LOG001 LOG003 CPD024.
           OPEN INPUT CAD004

           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CPP052-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CPP052-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO CPP052-MENSAGEM-ERRO
              MOVE ST-CPD020 TO CPP052-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO CPP052-MENSAGEM-ERRO
              MOVE ST-CAD004 TO CPP052-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD021 <> "00"
              MOVE "ERRO ABERTURA CPD021: "  TO CPP052-MENSAGEM-ERRO
              MOVE ST-CPD021 TO CPP052-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD024 <> "00"
              MOVE "ERRO ABERTURA CPD024: "  TO CPP052-MENSAGEM-ERRO
              MOVE ST-CPD024 TO CPP052-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO CPP052-MENSAGEM-ERRO
              MOVE ST-LOG001 TO CPP052-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG003 <> "00"
              MOVE "ERRO ABERTURA LOG003: "  TO CPP052-MENSAGEM-ERRO
              MOVE ST-LOG003 TO CPP052-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.

           MOVE "N"           TO LIBERADO
           MOVE "SENHA34"     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 NOT INVALID KEY
                MOVE "S"      TO LIBERADO
           END-READ

           CLOSE LOG001 LOG003 CPD020 CPD021 CPD024

           OPEN INPUT CPD020 CPD021 CPD024

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CPP063"            to logacess-programa
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
              MOVE "Executar pelo MENU" TO CPP052-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CPP052-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CPP052-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CPP052-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-DATA-MOVTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN CPP052-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN CPP052-ITEM-SELECIONADO-TRUE
                    PERFORM RETIRADA
      *             PERFORM CHAMA-ALTERACAO
               WHEN CPP052-CALCULA-QTDIAS-TRUE
                    PERFORM CALCULA-DIAS
               WHEN CPP052-CALCULA-JUROS-TRUE
                    PERFORM CALCULA-JUROS
               WHEN CPP052-CALCULA-REAPLICAR-TRUE
                    PERFORM CALCULA-REAPLICAR
               WHEN CPP052-ATUALIZA-RETIRADA-TRUE
                    PERFORM ATUALIZA-RETIRADA
      *        WHEN CPP052-REGRAVA-DADOS-TRUE
      *             PERFORM REGRAVA-DADOS
               WHEN CPP052-POPUP-FORNEC-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN CPP052-LE-FORNEC-TRUE
                   PERFORM LE-FORNEC
      *        WHEN CPP052-LE-PORTADOR-TRUE
      *            PERFORM LE-PORTADOR
      *        WHEN CPP052-LE-TIPO-FORN-TRUE
      *            PERFORM LE-TIPO-FORNEC
      *        WHEN CPP052-LE-COD-APUR-TRUE
      *            PERFORM LE-COD-APURACAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POPUP SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO CPP052-FORNECEDOR.
           MOVE PASSAR-STRING-1(1: 30) TO CPP052-NOME-FORNECEDOR.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE CPP052-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-FORNEC SECTION.
           MOVE CPP052-FORNECEDOR  TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "****" TO NOME-CG01.
           MOVE NOME-CG01          TO CPP052-NOME-FORNECEDOR.
      *                               CPP052-DESCR-FORN.
      *LE-PORTADOR SECTION.
      *    MOVE CPP052-PORTADOR    TO PORTADOR.
      *    READ CAD018 INVALID KEY MOVE "******" TO NOME-PORT.
      *    MOVE NOME-PORT          TO CPP052-DESCR-PORTADOR.
      *LE-TIPO-FORNEC SECTION.
      *    MOVE CPP052-TIPO-FORN   TO CODIGO-TIPO.
      *    READ CAD019 INVALID KEY MOVE "****" TO NOME-TIPO.
      *    MOVE NOME-TIPO          TO CPP052-DESCR-TIPO-FORN.
      *LE-COD-APURACAO SECTION.
      *    MOVE CPP052-COD-APURACAO TO CODIGO-REDUZ-CX20.
      *    READ CXD020 INVALID KEY MOVE "*****" TO DESCRICAO-CX20.
      *    MOVE DESCRICAO-CX20     TO CPP052-DESCR-APURACAO.
       VERIFICA-DATA-MOVTO-ANT SECTION.
      *    IF CPP052-VECTO-INI NOT = VECTO-INI-ANT
      *       OR CPP052-VECTO-FIM NOT = VECTO-FIM-ANT
      *          PERFORM GRAVA-WORK.

       INVERTER-DATAS SECTION.

      *---------- DATA DE VENCIMENTO
           MOVE CPP052-VECTO-INI TO DATA-INV
                                    VECTO-INI-ANT
                                    VECTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV  TO    VECTO-INI.

           MOVE CPP052-VECTO-FIM TO DATA-INV
                                    VECTO-FIM-ANT
                                    VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV  TO    VECTO-FIM.


      *---------- DATA DE MOVIMENTO
           MOVE CPP052-MOVTO-INI TO DATA-INV
                                    MOVTO-INI-ANT
                                    MOVTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV  TO    MOVTO-INI

           MOVE CPP052-MOVTO-FIM TO DATA-INV
                                    MOVTO-FIM-ANT
                                    MOVTO-FIM-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV  TO    MOVTO-FIM

      *---------- DATA DE PAGAMENTO
           MOVE CPP052-PAGTO-INI TO DATA-INV
                                    PAGTO-INI-ANT
                                    PAGTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV  TO    PAGTO-INI

           MOVE CPP052-PAGTO-FIM TO DATA-INV
                                    PAGTO-FIM-ANT
                                    PAGTO-FIM-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV  TO    PAGTO-FIM.


       GRAVA-WORK SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK.

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS

           MOVE ZEROS TO TOT-VALOR TOT-VALOR-PAGO TOT-VALOR-A-PAGAR
                         TOTAL-ACUM TOT-TITULO.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           INITIALIZE REG-CPD020

           PERFORM INVERTER-DATAS


           IF CPP052-FORNECEDOR > 0
              MOVE CPP052-FORNECEDOR TO FORNEC-CP20
              IF VECTO-INI > 0
                 MOVE VECTO-INI      TO DATA-VENCTO-CP20
              END-IF
              START CPD020 KEY IS NOT LESS ALT1-CP20 INVALID KEY
                   MOVE "10" TO ST-CPD020
              END-START
           ELSE
              IF CPP052-MOVTO-INI > 0
                 MOVE MOVTO-INI      TO DATA-MOVTO-CP20
                 START CPD020 KEY IS NOT LESS DATA-MOVTO-CP20 INVALID
                 KEY
                      MOVE "10" TO ST-CPD020
                 END-START
              ELSE
                 IF CPP052-PAGTO-INI > 0
                    MOVE PAGTO-INI   TO DATA-PGTO-CP20
                    START CPD020 KEY IS NOT LESS ALT6-CP20 INVALID KEY
                         MOVE "10" TO ST-CPD020
                    END-START
                 ELSE
                    IF CPP052-VECTO-INI > 0
                       START CPD020 KEY IS NOT LESS DATA-MOVTO-CP20
                       INVALID KEY
                           MOVE "10" TO ST-CPD020.

           PERFORM UNTIL ST-CPD020 = "10"
               READ CPD020 NEXT AT END
                   MOVE "10" TO ST-CPD020
               NOT AT END
                   IF (CPP052-FORNECEDOR > 0 AND
                       CPP052-FORNECEDOR <> FORNEC-CP20)
                      MOVE "10" TO ST-CPD020
                   ELSE
                      IF CPP052-FORNECEDOR > 0 AND VECTO-FIM > 0 AND
                         DATA-VENCTO-CP20 > VECTO-FIM
                         MOVE "10" TO ST-CPD020
                      ELSE
                         IF CPP052-FORNECEDOR = 0 AND CPP052-MOVTO-INI
                            > 0 AND DATA-MOVTO-CP20 > MOVTO-FIM
                            MOVE "10" TO ST-CPD020
                         ELSE
                            IF CPP052-FORNECEDOR = 0 AND
                               CPP052-MOVTO-INI = 0  AND
                               CPP052-PAGTO-INI > 0  AND
                               DATA-PGTO-CP20 > PAGTO-FIM
                               MOVE "10" TO ST-CPD020
                            ELSE
                               IF CPP052-EM-ABERTO = 0 OR
                                 (CPP052-EM-ABERTO = 1)
      *                           AND TIPO-FORN-CP20 = 17)
                                  PERFORM COMPARAR-FILTROS
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                   END-IF
               END-READ
           END-PERFORM
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       COMPARAR-FILTROS SECTION.
           IF CPP052-FORNECEDOR = 0 OR CPP052-FORNECEDOR = FORNEC-CP20
              IF (MOVTO-INI = 0 AND MOVTO-FIM = 0) OR (DATA-MOVTO-CP20
                  NOT < MOVTO-INI AND DATA-MOVTO-CP20 NOT > MOVTO-FIM)
              IF (VECTO-INI = 0 AND VECTO-FIM = 0) OR (DATA-VENCTO-CP20
                  NOT < VECTO-INI AND DATA-VENCTO-CP20 NOT > VECTO-FIM)
              IF (PAGTO-INI = 0 AND PAGTO-FIM = 0) OR (DATA-PGTO-CP20
                  NOT < PAGTO-INI AND DATA-PGTO-CP20 NOT > PAGTO-FIM)
                  PERFORM MOVER-WORK.

       MOVER-WORK SECTION.
           MOVE DATA-MOVTO-CP20     TO DATA-MOVTO-WK
                                       CPP052-EXIBE-MOVTO
           MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF RESPONSAVEL-CP20 = USUARIO-W OR LIBERADO = "S"
              IF SITUACAO-CP20 = 0 OR = 1 OR = 2
                 MOVE FORNEC-CP20         TO FORNEC-CP24
                 MOVE SEQ-CP20            TO SEQ-CP24
                 READ CPD024 NOT INVALID KEY
                     EVALUATE CPP052-OPCAO
                         WHEN 1 IF SITUACAO-CP20 = 2
                                   PERFORM MOVER-DADOS
                                END-IF
                         WHEN 2 IF SITUACAO-CP20 = 0 OR 1
                                   PERFORM MOVER-DADOS
                                END-IF
                         WHEN 3 IF WS-DATA-CPU > DATA-VENCTO-CP20
                                   PERFORM MOVER-DADOS
                                END-IF
                         WHEN 4 PERFORM MOVER-DADOS
                     END-EVALUATE
                 END-READ
              END-IF
           END-IF.

       MOVER-DADOS SECTION.
           MOVE FORNEC-CP20         TO FORNEC-WK CODIGO-CG01
           MOVE SEQ-CP20            TO SEQ-WK
           READ CGD001 INVALID KEY
               MOVE "----------"    TO FORNECEDOR-WK
           NOT INVALID KEY
               MOVE NOME-CG01       TO FORNECEDOR-WK
           END-READ
           MOVE DESCRICAO-CP20      TO DESCRICAO-WK
           MOVE DATA-MOVTO-CP20     TO DATA-MOVTO-WK
           MOVE DATA-VENCTO-CP20    TO VENCTO-WK
           MOVE VALOR-LIQ-CP20      TO VALOR-PAGO-WK
           MOVE VALOR-TOT-CP20      TO VALOR-WK
           MOVE DATA-PGTO-CP20      TO DATA-PAGTO-WK
           EVALUATE TIPO-CP24
               WHEN 1      MOVE "EM MAOS    " TO FORMA-PAGTO-WK
               WHEN 2      MOVE "DUPLICATA  " TO FORMA-PAGTO-WK
               WHEN 3      MOVE "DEP. BANCO " TO FORMA-PAGTO-WK
               WHEN 4      MOVE "GUIA/BOLETO" TO FORMA-PAGTO-WK
               WHEN OTHER  MOVE "-----------" TO FORMA-PAGTO-WK
           END-EVALUATE

           MOVE SITUACAO-CP20       TO SITUACAO-WK
           MOVE NR-DOCTO-CP20       TO DOCUMENTO-WK
           PERFORM DIAS-ATRASO
           ADD 1                    TO TOT-TITULO
           ADD VALOR-LIQ-CP20       TO TOT-VALOR-PAGO
           ADD VALOR-TOT-CP20       TO TOT-VALOR
           IF SITUACAO-CP20 = 0
              ADD VALOR-TOT-CP20 TO TOT-VALOR-A-PAGAR
           END-IF
           WRITE REG-WORK.

       DIAS-ATRASO SECTION.
           IF DATA-PGTO-CP20 NOT = ZEROS
              MOVE DATA-VENCTO-CP20      TO GRDIAS-AAMMDD-INICIAL
              MOVE DATA-PGTO-CP20        TO GRDIAS-AAMMDD-FINAL
              CALL "GRDIAS1" USING PARAMETROS-GRDIAS
              MOVE GRDIAS-NUM-DIAS       TO DIAS-ATRAS-WK
              COMPUTE VALOR-ACUM = DIAS-ATRAS-WK * VALOR-LIQ-CP20
              ADD VALOR-ACUM             TO TOTAL-ACUM
           ELSE MOVE ZEROS TO DIAS-ATRAS-WK TOTAL-ACUM.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO CPP052-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "----------------------------------" TO CPP052-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "DATA-MOVTO FORNECEDOR                      DESCRICAO
      -    "                    DATA-VECTO         VALOR FORMA-PGTO   DA
      -    "TA-PAGTO     VLR-PAGTO   ATR SEQ" TO CPP052-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "----------------------------------" TO CPP052-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO CPP052-LINDET

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

       MOVER-DADOS-LINDET SECTION.
           MOVE DATA-MOVTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO CPP052-LINDET(01:11)
           MOVE FORNECEDOR-WK     TO CPP052-LINDET(12:30)
           MOVE DESCRICAO-WK      TO CPP052-LINDET(44:30)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO CPP052-LINDET(76:10)
           MOVE VALOR-WK          TO MASC-VALOR
           MOVE MASC-VALOR        TO CPP052-LINDET(88:13)
           MOVE FORMA-PAGTO-WK    TO CPP052-LINDET(101:13)
           MOVE DATA-PAGTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO CPP052-LINDET(114:10)
           IF SITUACAO-WK = 1
              MOVE "SUSPENSO"     TO CPP052-LINDET(126:13)
           ELSE
              MOVE VALOR-PAGO-WK  TO MASC-VALOR
              MOVE MASC-VALOR     TO CPP052-LINDET(126:13)
           END-IF.
           MOVE DIAS-ATRAS-WK     TO CPP052-LINDET(141:03)
           MOVE SEQ-WK            TO CPP052-LINDET(145:05)
           MOVE FORNEC-WK         TO CPP052-LINDET(151:06).
       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE CPP052-ORDEM
             WHEN 1
                MOVE "MOVTO" TO CPP052-DESCR-ORDEM
                START WORK KEY IS NOT < PADRAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "FORNECEDOR" TO CPP052-DESCR-ORDEM
                START WORK KEY IS NOT < CH-FORNECEDOR-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "VENCTO" TO CPP052-DESCR-ORDEM
                START WORK KEY IS NOT < CH-VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "VALOR" TO CPP052-DESCR-ORDEM
                START WORK KEY IS NOT < CH-VALOR-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "FORMA PAGTO" TO CPP052-DESCR-ORDEM
                START WORK KEY IS NOT < CH-FORMA-PAGTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "DATA PAGTO" TO CPP052-DESCR-ORDEM
                START WORK KEY IS NOT < CH-DATA-PAGTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       TOTALIZA SECTION.
           MOVE SPACES TO CPP052-LINTOT.
           MOVE TOT-TITULO        TO CPP052-LINTOT(01: 14)
           MOVE TOT-VALOR         TO VALOR-E
           MOVE VALOR-E           TO CPP052-LINTOT(18: 13)
           MOVE TOT-VALOR-PAGO    TO VALOR-E
           MOVE VALOR-E           TO CPP052-LINTOT(36: 13)
           DIVIDE TOTAL-ACUM BY TOT-VALOR GIVING TOT-ATRAS-MEDIO
           MOVE TOT-ATRAS-MEDIO   TO ATRASO-MEDIO-E
           MOVE ATRASO-MEDIO-E    TO CPP052-LINTOT(54: 10)
           MOVE TOT-VALOR-A-PAGAR TO VALOR-E
           MOVE VALOR-E           TO CPP052-LINTOT(66: 13)
           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       RETIRADA SECTION.
           MOVE CPP052-LINDET(151:06) TO FORNEC-CP20
      *    MOVE CPP052-FORNECEDOR     TO FORNEC-CP20
           MOVE CPP052-LINDET(145:05) TO SEQ-CP20
           START CPD020 KEY IS = CHAVE-CP20 INVALID KEY
                 CONTINUE
           NOT INVALID KEY
               READ CPD020
               END-READ
               IF RESPONSAVEL-CP20 = USUARIO-W OR LIBERADO = "S"
      *           IF TIPO-FORN-CP20 <> 17
      *              MOVE "Programação já Confirmada pelo Financeiro, Nã
      *                   "o podendo ser alterada" TO MENSAGEM
      *              MOVE "C" TO TIPO-MSG
      *              PERFORM EXIBIR-MENSAGEM
      *           ELSE
                     CALL "CPP022A" USING PARAMETROS-W
                                          COD-USUARIO-W FORNEC-CP20
                                          SEQ-CP20
                     CANCEL "CPP022A"
                     READ CPD020
                     END-READ
      *           END-IF
               ELSE
                  MOVE "Esse Usuário Não é o Responsável pelo Lançamento
      -                " do Mesmo" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
           END-START.

      *    START CPD020 KEY IS = CHAVE-CP20 INVALID KEY CONTINUE.
      *    READ CPD020 INVALID KEY INITIALIZE REG-CPD020.
      *    MOVE TIPO-FORN-CP20      TO CPP052-TIPO-FORN.
      *    MOVE DATA-VENCTO-CP20    TO DATA-INV.
      *    CALL "GRIDAT1" USING DATA-INV.
      *    MOVE DATA-INV            TO CPP052-VENCTO1 GRTIME-DATE.
      *    MOVE 1                   TO GRTIME-TYPE.
      *    MOVE 30                  TO GRTIME-DAYS.
      *    MOVE 5                   TO GRTIME-FUNCTION.
      *    CALL "GRTIME" USING PARAMETROS-GRTIME.
      *    MOVE GRTIME-DATE-FINAL   TO CPP052-EMISSAO1.
      *    MOVE TAXA-APLIC-CP20     TO CPP052-TAXA1 CPP052-TAXA2
      *    MOVE VALOR-TOT-CP20      TO CPP052-VALOR1-PROG.
      *    COMPUTE CPP052-VALOR1 = CPP052-VALOR1-PROG / ((CPP052-TAXA1/
      *           100) + 1)
      *    COMPUTE CPP052-JUROS1 = CPP052-VALOR1-PROG - CPP052-VALOR1.
       CALCULA-DIAS SECTION.
           MOVE CPP052-EMISSAO1     TO GRTIME-DATE.
           MOVE CPP052-VENCTO2      TO GRTIME-DATE-FINAL.
           MOVE 3                   TO GRTIME-FUNCTION.
           MOVE 1                   TO GRTIME-TYPE.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           MOVE GRTIME-DAYS-FINAL   TO CPP052-DIAS2.
       CALCULA-JUROS SECTION.
           MOVE CPP052-TAXA2        TO CPP052-TAXA3.
           COMPUTE CPP052-VLR-JUROS2 = (CPP052-VLR-RETIR2 *
                 (CPP052-TAXA2 / 100) /30) * CPP052-DIAS2.
       CALCULA-REAPLICAR SECTION.
           MOVE CPP052-VLR-JUROS2 TO CPP052-JUROS-RESG3.
           MOVE CPP052-EMISSAO1  TO CPP052-EMISSAO3.
           MOVE CPP052-VENCTO1   TO CPP052-VENCTO3.
           COMPUTE CPP052-VLR-INVESTIDO3 = CPP052-VALOR1 -
                CPP052-VLR-RETIR2.
           COMPUTE CPP052-JUROS3 = CPP052-VLR-INVESTIDO3 *
               (CPP052-TAXA3 / 100).
           COMPUTE CPP052-NOVO-VALOR3 = CPP052-VLR-INVESTIDO3 +
               CPP052-JUROS3 + CPP052-JUROS-RESG3.
      *    MOVE CPP052-VENCTO2   TO CPP052-EMISSAO3 GRTIME-DATE.
      *    MOVE 1                TO GRTIME-TYPE GRTIME-FUNCTION.
      *    MOVE 30               TO GRTIME-DAYS.
      *    CALL "GRTIME" USING PARAMETROS-GRTIME.
      *    MOVE GRTIME-DATE-FINAL TO CPP052-VENCTO3.
       ATUALIZA-RETIRADA SECTION.
           CLOSE    CPD020 CPD021
           OPEN I-O CPD020 CPD021 LOG003 LOG001

           MOVE DATA-MOVTO-W  TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-MOVTO-I.

      *    Grava a pagar
           MOVE FORNEC-CP20       TO FORNEC-WK
           MOVE SEQ-CP20          TO SEQ-WK.
           READ WORK.
           MOVE DATA-MOVTO-I      TO DATA-MOVTO-CP20 DATA-MOVTO-WK.
           MOVE CPP052-EMISSAO3   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO DATA-EMISSAO-CP20.
           MOVE CPP052-VENCTO3    TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO DATA-VENCTO-CP20 VENCTO-WK.
           MOVE CPP052-TAXA3      TO TAXA-APLIC-CP20.
           MOVE CPP052-NOVO-VALOR3 TO VALOR-TOT-CP20 VALOR-WK.
           REWRITE REG-CPD020 NOT INVALID KEY
               MOVE USUARIO-W   TO LOG3-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG3-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG3-HORAS
               MOVE "A"         TO LOG3-OPERACAO
               MOVE "CPD020"    TO LOG3-ARQUIVO
               MOVE "CPP063"    TO LOG3-PROGRAMA
               MOVE REG-CPD020  TO LOG3-REGISTRO
               WRITE REG-LOG003
               END-WRITE.

           REWRITE REG-WORK.
      *    MOVE "ATUALIZA-ITEM" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.

           MOVE 0                 TO PREV-DEF-CP20.
           MOVE CPP052-VENCTO2    TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO DATA-VENCTO-CP20 VENCTO-WK.
           MOVE CPP052-EMISSAO1   TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO DATA-EMISSAO-CP20.
           MOVE CPP052-TAXA2      TO TAXA-APLIC-CP20.
           MOVE CPP052-VLR-RETIR2 TO VALOR-TOT-CP20 VALOR-WK.
           MOVE FORNEC-CP20 TO FORNEC-CP21 FORNEC-WK.
           MOVE "PAGTO EMPRESTIMO" TO DESCRICAO-CP20.
           MOVE ZEROS TO SEQ-CP21.
           MOVE ZEROS TO ST-CPD021.
           PERFORM UNTIL ST-CPD021 = "10"
            READ CPD021 INVALID KEY
                MOVE 1 TO SEQ-CP21
                WRITE REG-CPD021 INVALID KEY
                      CONTINUE
                NOT INVALID KEY
                    MOVE USUARIO-W   TO LOG1-USUARIO
                    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                    MOVE WS-DATA-CPU TO LOG1-DATA
                    ACCEPT WS-HORA-SYS FROM TIME
                    MOVE WS-HORA-SYS TO LOG1-HORAS
                    MOVE "I"         TO LOG1-OPERACAO
                    MOVE "CPD021"    TO LOG1-ARQUIVO
                    MOVE "CPP063"    TO LOG1-PROGRAMA
                    MOVE REG-CPD021  TO LOG1-REGISTRO
                    WRITE REG-LOG001
                    END-WRITE
                    MOVE "10" TO ST-CPD021
                END-WRITE
            NOT INVALID KEY
                  ADD 1 TO SEQ-CP21
                  REWRITE REG-CPD021 INVALID KEY
                       CONTINUE
                  NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG1-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG1-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG1-HORAS
                       MOVE "A"         TO LOG1-OPERACAO
                       MOVE "CPD021"    TO LOG1-ARQUIVO
                       MOVE "CPP063"    TO LOG1-PROGRAMA
                       MOVE REG-CPD021  TO LOG1-REGISTRO
                       WRITE REG-LOG001
                       END-WRITE
                       MOVE "10" TO ST-CPD021
                  END-REWRITE
            END-READ
           END-PERFORM.
           MOVE SEQ-CP21          TO SEQ-CP20 SEQ-WK.
           WRITE REG-WORK.
           WRITE REG-CPD020 NOT INVALID KEY
               MOVE USUARIO-W   TO LOG3-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG3-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG3-HORAS
               MOVE "I"         TO LOG3-OPERACAO
               MOVE "CPD020"    TO LOG3-ARQUIVO
               MOVE "CPP063"    TO LOG3-PROGRAMA
               MOVE REG-CPD020  TO LOG3-REGISTRO
               WRITE REG-LOG003
               END-WRITE.

           CLOSE CPD020 CPD021 LOG003 LOG001
           OPEN INPUT CPD020 CPD021

           perform carrega-lista.
      *    PERFORM MOVER-DADOS-LINDET.
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *CHAMA-ALTERACAO SECTION.
      *    MOVE CPP052-FORNECEDOR   TO FORNEC-CP20.
      *    MOVE CPP052-LINDET(108:5) TO SEQ-CP20.
      *    START CPD020 KEY IS = CHAVE-CP20 INVALID KEY CONTINUE.
      *    READ CPD020 INVALID KEY INITIALIZE REG-CPD020.
      *    MOVE DATA-MOVTO-W        TO CPP052-DATA-MOVTO.
      *    MOVE FORNEC-CP20         TO CPP052-COD-FORN.
      *    MOVE "********"          TO CPP052-DESCR-FORN.
      *    MOVE TIPO-FORN-CP20      TO CPP052-TIPO-FORN CODIGO-TIPO.
      *    READ CAD019 INVALID KEY  MOVE "****" TO NOME-TIPO.
      *    MOVE NOME-TIPO           TO CPP052-DESCR-TIPO-FORN.
      *    MOVE PORTADOR-CP20       TO CPP052-PORTADOR PORTADOR.
      *    READ CAD018 INVALID KEY MOVE "******" TO NOME-PORT.
      *    MOVE NOME-PORT           TO CPP052-DESCR-PORTADOR.
      *    MOVE NR-DOCTO-CP20       TO CPP052-NR-DOCTO.
      *    MOVE DATA-EMISSAO-CP20   TO DATA-INV.
      *    CALL "GRIDAT1" USING DATA-INV.
      *    MOVE DATA-INV            TO CPP052-DATA-EMISSAO.
      *    MOVE DATA-VENCTO-CP20    TO DATA-INV.
      *    CALL "GRIDAT1" USING DATA-INV.
      *    MOVE DATA-INV            TO CPP052-DATA-VENCTO.
      *    MOVE DESCRICAO-CP20      TO CPP052-DESCRICAO.
      *    MOVE TIPO-MOEDA-CP20     TO CPP052-TIPO-MOEDA.
      *    EVALUATE TIPO-MOEDA-CP20
      *      WHEN 0 MOVE "-Real"    TO CPP052-TIPO-MOEDA(2: 6)
      *      WHEN 1 MOVE "-Dolar"   TO CPP052-TIPO-MOEDA(2: 5)
      *    END-EVALUATE
      *    MOVE CODREDUZ-APUR-CP20  TO CPP052-COD-APURACAO.
      *    MOVE "*************"     TO CPP052-DESCR-APURACAO.
      *    MOVE PREV-DEF-CP20       TO CPP052-PREV-DEF
      *    EVALUATE PREV-DEF-CP20
      *      WHEN 0 MOVE "-Definitivo" TO CPP052-PREV-DEF(2: 11)
      *      WHEN 0 MOVE "-Previsto  " TO CPP052-PREV-DEF(2: 11)
      *    END-EVALUATE
      *    MOVE JUROS-MORA-CP20     TO CPP052-JUROS-MORA.
      *    MOVE MULTA-ATRASO-CP20   TO CPP052-MULTA-ATRASO.
      *    MOVE TAXA-APLIC-CP20     TO CPP052-TAXA
      *    MOVE VALOR-TOT-CP20      TO CPP052-VALOR-TOTAL.
      *    MOVE RESPONSAVEL-CP20    TO CPP052-RESPONSAVEL.
      *    MOVE SITUACAO-CP20       TO CPP052-SITUACAO.
      *    MOVE TIPO-CONTA-CP20     TO CPP052-TIPO-CONTA.
      *    EVALUATE TIPO-CONTA-CP20
      *      WHEN 0 MOVE "-Temporária" TO CPP052-TIPO-CONTA(2: 11)
      *      WHEN 0 MOVE "-Permanente" TO CPP052-TIPO-CONTA(2: 11)
      *    END-EVALUATE.
      *    MOVE "CHAMA-ALTERACAO" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *REGRAVA-DADOS SECTION.
      *    MOVE DATA-MOVTO-W          TO DATA-INV
      *    CALL "GRIDAT2" USING DATA-INV
      *    MOVE DATA-INV              TO DATA-MOVTO-CP20.
      *    MOVE CPP052-TIPO-FORN      TO TIPO-FORN-CP20
      *    MOVE CPP052-PORTADOR       TO PORTADOR-CP20
      *    MOVE CPP052-NR-DOCTO       TO NR-DOCTO-CP20.
      *    MOVE CPP052-DATA-EMISSAO   TO DATA-INV.
      *    CALL "GRIDAT2" USING DATA-INV.
      *    MOVE DATA-INV              TO DATA-EMISSAO-CP20.
      *    MOVE CPP052-DATA-VENCTO    TO DATA-INV
      *    CALL "GRIDAT2" USING DATA-INV.
      *    MOVE DATA-INV              TO DATA-VENCTO-CP20.
      *    MOVE CPP052-DESCRICAO      TO DESCRICAO-CP20
      *    IF CPP052-TIPO-MOEDA = SPACES MOVE "0" TO TIPO-MOEDA-CP20
      *    ELSE MOVE CPP052-TIPO-MOEDA(1: 1) TO TIPO-MOEDA-CP20.
      *    MOVE CPP052-COD-APURACAO   TO CODREDUZ-APUR-CP20
      *    MOVE CPP052-JUROS-MORA     TO JUROS-MORA-CP20
      *    MOVE CPP052-MULTA-ATRASO   TO MULTA-ATRASO-CP20
      *    IF CPP052-PREV-DEF = SPACES MOVE "0" TO PREV-DEF-CP20
      *    ELSE MOVE CPP052-PREV-DEF(1: 1) TO PREV-DEF-CP20.
      *    MOVE CPP052-TAXA           TO TAXA-APLIC-CP20
      *    MOVE CPP052-RESPONSAVEL    TO RESPONSAVEL-CP20
      *    MOVE "********"            TO DIGITADOR-CP20.
      *    IF CPP052-TIPO-CONTA = SPACES MOVE "0" TO TIPO-CONTA-CP20
      *    ELSE MOVE CPP052-TIPO-CONTA(1: 1) TO TIPO-CONTA-CP20.
      *    MOVE CPP052-VALOR-TOTAL    TO VALOR-TOT-CP20
      *    MOVE CPP052-SITUACAO       TO SITUACAO-CP20.
      *    REWRITE REG-CPD020.
      *    PERFORM GRAVA-WORK.
      *    PERFORM CARREGA-LISTA.
      *
      **    Deverá ser regravado o arquivo work, pois pode ter havido
      *    alguma alteração no arquivo em que diferencie a classificação
      *    por exemplo, mudar a data de vencto fora do intervalo soli-
      *    citado pelo usuário.
       CLEAR-FLAGS SECTION.
           INITIALIZE CPP052-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CPP063" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE DATA-MOVTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01: 11)
           MOVE DOCUMENTO-WK      TO LINDET-REL(12: 11)
           MOVE DESCRICAO-WK      TO LINDET-REL(23: 31)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(58: 12)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(70: 11)
           MOVE DATA-PAGTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(81: 11)
           MOVE VALOR-PAGO-WK     TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(92: 12)
           MOVE DIAS-ATRAS-WK     TO LINDET-REL(104: 03)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.


       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE TOT-TITULO        TO LINTOT-REL(01: 14)
           MOVE TOT-VALOR         TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(15: 13)
           MOVE TOT-VALOR-PAGO    TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(33: 13)
           DIVIDE TOTAL-ACUM BY TOT-VALOR GIVING TOT-ATRAS-MEDIO
           MOVE TOT-ATRAS-MEDIO   TO ATRASO-MEDIO-E
           MOVE ATRASO-MEDIO-E    TO LINTOT-REL(51: 10)
           MOVE TOT-VALOR-A-PAGAR TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(63: 13)
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           MOVE CPP052-FORNECEDOR  TO FORNECEDOR-REL.
           MOVE CPP052-NOME-FORNECEDOR TO NOME-FORNEC-REL.
           MOVE CPP052-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB02A AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 7 TO LIN.

       EXIBIR-MENSAGEM SECTION.
           MOVE SPACES TO RESP-MSG
           CALL   "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL "MENSAGEM".
       EXIBIR-MENSAGEM-FIM.
           EXIT.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CPP052-DATA-BLOCK.
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
           move "CPP063"            to logacess-programa
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

           CLOSE CGD001 CPD020 CPD021 CPD024 WORK CAD004

           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

