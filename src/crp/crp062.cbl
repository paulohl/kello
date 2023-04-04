       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP062.
       DATE-WRITTEN. 12/07/2005.
       AUTHOR. ALFREDO SAVIOLLI NETO.
      *FUNÇÃO: RELATORIO DUPLICATAS PARA TRANSFERENCIA.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CAPX002.
           COPY CAPX010.
           COPY CGPX010.
           COPY CGPX020.
           COPY CGPX011.
           COPY CAPX018.
           COPY CRPX020.
           COPY CRPX020B.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGACESS.SEL.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = DATA-MOVTO-WK
                                           COD-COMPL-WK
                                           SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK1 = CLIENTE-WK VENC-WK
                               NR-DUPLICATA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-WK2 = CIDADE-WK VENC-WK
                               NR-DUPLICATA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-WK3 = CONTRATO-WK VENC-WK
                               NR-DUPLICATA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-WK4 = PORTADOR-WK VENC-WK
                               NR-DUPLICATA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-WK5 = VENC-WK VALOR-WK
                               NR-DUPLICATA-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW002.
       COPY CAPW010.
       COPY CGPW010.
       COPY CGPW020.
       COPY CGPW011.
       COPY CAPW018.
       COPY CRPW020.
       COPY CRPW020B.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGACESS.FD.

       FD  WORK.
       01  REG-WORK.
           05  DATA-MOVTO-WK      PIC 9(08).
           05  SEQ-WK             PIC 9(04).
           05  CLIENTE-WK         PIC X(20).
           05  CIDADE-WK          PIC X(13).
           05  NR-DUPLICATA-WK    PIC x(07).
           05  CONTRATO-WK        PIC 9(04).
           05  PORTADOR-WK        PIC X(05).
           05  CARTEIRA-WK        PIC X(04).
           05  VENC-WK            PIC 9(08).
           05  VALOR-WK           PIC 9(08)V99.
           05  COD-COMPL-WK       PIC 9(09).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP062.CPB".
           COPY "CRP062.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPDIAS1.CPY".
      *    COPY "CPWEEK1.CPY".
           COPY "CPTIME.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  AUX-SEQUENCIA         PIC 9(05)    VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W          PIC 9(8)          VALUE ZEROS.
           05  DATA-E           PIC 99/99/9999    BLANK WHEN ZEROS.
           05  VENC-W           PIC 9(8)          VALUE ZEROS.
           05  VALOR-E          PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1         PIC ZZZ.ZZZ,ZZ    BLANK WHEN ZEROS.
           05  DATA-DIA-W       PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA         PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI       PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM       PIC 9(8)     VALUE ZEROS.
           05  MOVTO-INI        PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM        PIC 9(8)     VALUE ZEROS.
           05  DATA-BASE-I      PIC 9(8)     VALUE ZEROS.
           05  NUMERO-CHEQUE    PIC X(07)    VALUE SPACES.
           05  VALOR-ACUMULADO  PIC 9(10)V99 VALUE ZEROS.
           05  VALOR-TOTAL      PIC 9(10)V99 VALUE ZEROS.
           05  DIAS-INI         PIC 9(3)     VALUE ZEROS.
           05  DIAS-FIM         PIC 9(3)     VALUE ZEROS.
           05  NOME-PORT-W      PIC X(10)    VALUE SPACES.
           05  AAAAMMDD         PIC 9(8)     VALUE ZEROS.
           05  DDMMAAAA         PIC 9(8)     VALUE ZEROS.
           05  I                PIC 99       VALUE ZEROS.
           05  SENHA-W1         PIC 9(4)     COMP-3.
           05  LIN              PIC 9(02)    VALUE ZEROS.
           05  AUX-SENHA        PIC 9(4)     VALUE ZEROS.
           05  TOTAL-ACUMULADO  PIC 9(09)V99.
           05  AUX-DATA         PIC 9(08).
           05  AUX-VALOR        PIC 9(09)V99.
           05  MASC-CHEQUE      PIC 999999 BLANK WHEN ZEROS.
           05  AUX-TIPO         PIC 9(01).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem                 pic x(200).
       01 tipo-msg                 pic x(01).
       01 resp-msg                 pic x(01).

       01  LINDET1.
           05  LINDET1-REL         PIC X(110)   VALUE SPACES.

       01  WS-DATA-SYS.
           05 WS-DATA-CPU.
              10 WS-ANO-CPU        PIC 9(04).
              10 WS-MES-CPU        PIC 9(02).
              10 WS-DIA-CPU        PIC 9(02).
           05 FILLER               PIC X(13).

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
           05  FILLER              PIC X(38)   VALUE
           "RELACAO DUPLICATA PARA TRANSFERENCIA: ".
           05  ORDEM-REL           PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(03)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.MOVTO: ".
           05  MOVTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MOVTO-FIM-REL       PIC 99/99/9999.
           05  FILLER              PIC X(03)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "CLIENTE              CIDADE                    DUPLIC    CON
      -    "T PORT  CART   DT.VENCTO      VALOR      TOTAL".
       01  LINTOT.
           05  FILLER              PIC X(12)   VALUE "QT-TITULOS: ".
           05  QTDE-TITULO-REL     PIC ZZZZ.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "TOT-PERIODO: ".
           05  TOTAL-PERIODO-REL   PIC ZZ.ZZZ.ZZZ,ZZ.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "VENCIDAS...: ".
           05  TOTAL-VENCIDAS      PIC ZZ.ZZZ.ZZZ,ZZ.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "A VENCER...: ".
           05  TOTAL-AVENCER       PIC ZZ.ZZZ.ZZZ,ZZ.

      *-------------------------------------------------------------

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           ACCEPT DATA6-W FROM DATE.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.

           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).

           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-W.

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS

           ACCEPT HORA-BRA FROM TIME.


      *    COPY "CBDATA1.CPY".
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
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD020B" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020B.
           MOVE "CRD200"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CAD004 CAD018 CRD020 CRD200 CRD201 CAD010 CGD010
                      CGD011 CGD020 CAD002 CRD020B

           ACCEPT VARIA-W FROM TIME.

           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: " TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: " TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: " TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020B <> "00"
              MOVE "ERRO ABERTURA CRD020B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP062"            to logacess-programa
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

           MOVE 0,01 TO GS-VALOR-INI
           MOVE ALL "9" TO GS-VALOR-FIM


           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-TRANSF-PORTADOR-TRUE
                    PERFORM TRANSFERE-PORTADOR
               WHEN GS-VERIF-SENHA-TRUE
                    PERFORM VERIFICA-SENHA-PORTADOR
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LE-PORTADOR
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA-CHEQUE
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVAR-WORK
                    PERFORM CARREGA-LISTA-CHEQUE
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIMIR-RELATORIO
                    END-IF
               WHEN GS-LE-CARTAO-TRUE
                    PERFORM LER-CARTAO
               WHEN GS-POPUP-CARTAO-TRUE
                    PERFORM POPUP-CARTAO
               WHEN GS-ALTERA-TAXA-TRUE
                    PERFORM ALTERA-TAXA
               WHEN GS-BAIXAR-O-LOTE-TRUE
                    PERFORM BAIXAR-LOTE
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop"

          MOVE "SENHA65"     TO PROGRAMA-CA004
          MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
          READ CAD004 INVALID KEY
               DISABLE-OBJECT PB14
          NOT INVALID KEY
               ENABLE-OBJECT PB14.
          .

       BAIXAR-LOTE SECTION.
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS

           MOVE "Deseja Realmente Baixar os Lançamentos ?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM 140-EXIBIR-MENSAGEM

           IF RESP-MSG = "S"
              OPEN I-O   WORK

              CLOSE      CRD020 CRD020B
              OPEN I-O   CRD020 CRD020B

              INITIALIZE REG-WORK
              START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
                   MOVE "10" TO ST-WORK
              END-START

              PERFORM UNTIL ST-WORK = "10"
                   READ WORK NEXT AT END
                        MOVE "10" TO ST-WORK
                   NOT AT END
                        MOVE COD-COMPL-WK        TO COD-COMPL-CR20
                        MOVE SEQ-WK              TO SEQ-CR20
                        READ CRD020 INVALID KEY
                             MOVE "Contas a Receber Não Encontrado" TO
                                                    MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM 140-EXIBIR-MENSAGEM
                        NOT INVALID KEY
                             IF SITUACAO-CR20 = 0 AND
                                VALOR-SALDO-CR20 = 0
                                MOVE VALOR-TOT-CR20 TO VALOR-SALDO-CR20
                             END-IF
                             IF VALOR-SALDO-CR20 > 0
                                INITIALIZE REG-CRD020B
                                MOVE CLASS-CLIENTE-CR20
                                  TO CLASS-CLIENTE-CR20B
                                MOVE CLIENTE-CR20
                                  TO CLIENTE-CR20B
                                MOVE SEQ-CR20
                                  TO SEQ-CR20B
                                MOVE WS-DATA-CPU
                                  TO DATA-RCTO-CR20B
                                MOVE VALOR-SALDO-CR20
                                  TO VALOR-BAIXA-CR20B
                                MOVE VALOR-SALDO-CR20
                                  TO VALOR-TOT-CR20
                                MOVE VALOR-SALDO-CR20
                                  TO VALOR-LIQ-CR20B
                                MOVE SEQ-CAIXA-CR20
                                  TO SEQ-CAIXA-CR20B
                                MOVE FORMA-PAGTO-CR20
                                  TO FORMA-PAGTO-CR20B
                                MOVE USUARIO-W TO USUARIO-CR20B
                                WRITE REG-CRD020B INVALID KEY
                                   REWRITE REG-CRD020B INVALID KEY
                                     MOVE "Erro de Gravação..CRD020B" TO
                                     MENSAGEM
                                     MOVE "C" TO TIPO-MSG
                                     PERFORM 140-EXIBIR-MENSAGEM
                                   NOT INVALID KEY
                                     MOVE VALOR-LIQ-CR20B TO
                                          VALOR-LIQ-CR20
                                     MOVE DATA-RCTO-CR20B TO
                                          DATA-RCTO-CR20
                                     MOVE 0          TO VALOR-SALDO-CR20
                                     MOVE 2          TO SITUACAO-CR20
                                     REWRITE REG-CRD020 INVALID KEY
                                         MOVE "Erro de Regravação...CRD0
      -                                       "20" TO MENSAGEM
                                         MOVE "C" TO TIPO-MSG
                                         PERFORM 140-EXIBIR-MENSAGEM
                                     END-REWRITE
                                   END-REWRITE
                                NOT INVALID KEY
                                     MOVE VALOR-LIQ-CR20B TO
                                          VALOR-LIQ-CR20
                                     MOVE DATA-RCTO-CR20B TO
                                          DATA-RCTO-CR20
                                     MOVE 0          TO VALOR-SALDO-CR20
                                     MOVE 2          TO SITUACAO-CR20
                                     REWRITE REG-CRD020 INVALID KEY
                                         MOVE "Erro de Regravação...CRD0
      -                                       "20" TO MENSAGEM
                                         MOVE "C" TO TIPO-MSG
                                         PERFORM 140-EXIBIR-MENSAGEM
                                     END-REWRITE
                                END-WRITE
                             END-IF
                        END-READ
                   END-READ
              END-PERFORM

              CLOSE      CRD020 CRD020B WORK
              OPEN INPUT CRD020 CRD020B.

       ALTERA-TAXA SECTION.
           MOVE "Deseja Realmente Atualizar as Taxas ?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM 140-EXIBIR-MENSAGEM

           IF RESP-MSG = "S"
              OPEN I-O   WORK

              CLOSE      CRD020
              OPEN I-O   CRD020

              INITIALIZE REG-WORK
              START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
                   MOVE "10" TO ST-WORK
              END-START

              PERFORM UNTIL ST-WORK = "10"
                   READ WORK NEXT AT END
                        MOVE "10" TO ST-WORK
                   NOT AT END
                        MOVE COD-COMPL-WK        TO COD-COMPL-CR20
                        MOVE SEQ-WK              TO SEQ-CR20
                        READ CRD020 INVALID KEY
                             MOVE "Contas a Receber Não Encontrado" TO
                             MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM 140-EXIBIR-MENSAGEM
                        NOT INVALID KEY
                             MOVE CARTAO-CRED-CR20 TO CODIGO-CG20
                             READ CGD020 INVALID KEY
                                 MOVE "Cartão de Crédito Não Encontrado"
                                   TO MENSAGEM
                                 MOVE "C" TO TIPO-MSG
                                 PERFORM 140-EXIBIR-MENSAGEM
                             NOT INVALID KEY
                                 MOVE TAXA-CREDITO-CG20       TO
                                      TAXA-ADMINIST-CREDITO-CR20
                                 MOVE TAXA-PARCELA-CG20       TO
                                      TAXA-ADMINIST-PARCELA-CR20
                                 REWRITE REG-CRD020 INVALID KEY
                                     MOVE "Erro de Regravação...CRD020"
                                       TO MENSAGEM
                                     MOVE "C" TO TIPO-MSG
                                     PERFORM 140-EXIBIR-MENSAGEM
                                 END-REWRITE
                             END-READ
                        END-READ
                   END-READ
              END-PERFORM

              CLOSE      CRD020 WORK
              OPEN INPUT CRD020.

       140-EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       LER-CARTAO SECTION.
           MOVE GS-ACP-CARTAO TO CODIGO-CG20
           READ CGD020 INVALID KEY
                MOVE SPACES TO NOME-CG20.
           MOVE NOME-CG20 TO GS-DESC-CARTAO.

       POPUP-CARTAO SECTION.
           CALL   "CGP020T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CGP020T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-CARTAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-ACP-CARTAO.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       LE-PORTADOR SECTION.
           MOVE GS-PORTADOR  TO PORTADOR.
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT TO GS-DESC-PORTADOR.

       POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-PORTADOR.
           MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR.


       GRAVAR-WORK SECTION.

           MOVE GS-AUX-PORTADOR2 TO GS-PORTADOR
           MOVE GS-NUMERO-CHEQUE TO MASC-CHEQUE
           MOVE MASC-CHEQUE      TO NUMERO-CHEQUE

           CLOSE      CRD020 CRD200 CRD201
           OPEN INPUT CRD020 CRD200 CRD201

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


           MOVE GS-TIPO(1:1) TO AUX-TIPO

           IF GS-MOVTO-INI > 0
              PERFORM PELA-DATA-MOVTO
           ELSE
              PERFORM PELA-DATA-VECTO.

           CLOSE       WORK
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       PELA-DATA-MOVTO SECTION.
           INITIALIZE REG-CRD020

           MOVE GS-MOVTO-INI TO DATA-INV
                                MOVTO-INI-REL.

           CALL "GRIDAT2"   USING DATA-INV.
           MOVE DATA-INV    TO DATA-MOVTO-CR20
                               MOVTO-INI.

           MOVE GS-MOVTO-FIM TO DATA-INV
                                MOVTO-FIM-REL.

           CALL "GRIDAT2"   USING DATA-INV
           MOVE DATA-INV    TO MOVTO-FIM


           IF GS-VENCTO-INI > 0
              MOVE GS-VENCTO-INI TO DATA-INV
                                     VENCTO-INI-REL
              CALL "GRIDAT2"   USING DATA-INV
              MOVE DATA-INV      TO  VENCTO-INI
              MOVE GS-VENCTO-FIM TO  DATA-INV
                                     VENCTO-FIM-REL
              CALL "GRIDAT2"   USING DATA-INV
              MOVE DATA-INV    TO VENCTO-FIM
           ELSE
              MOVE ZEROS TO VENCTO-INI VENCTO-FIM.



           START CRD020 KEY IS NOT LESS DATA-MOVTO-CR20 INVALID KEY
               MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
               READ CRD020 NEXT AT END
                   MOVE "10" TO ST-CRD020
               NOT AT END
                   IF DATA-MOVTO-CR20 > MOVTO-FIM
                      MOVE "10" TO ST-CRD020
                   ELSE
                      IF VENCTO-INI = 0 AND VENCTO-FIM = 0
                         IF SITUACAO-CR20 = 0
                            IF AUX-TIPO = 9 OR
                               AUX-TIPO = TIPO-DOCTO-CR20
                               IF GS-ACP-CARTAO = 0 OR
                                  GS-ACP-CARTAO = CARTAO-CRED-CR20
                                  PERFORM CONTINUA-COMPARACAO
                               END-IF
                            END-IF
                         END-IF
                      ELSE
                         IF DATA-VENCTO-CR20 NOT < VENCTO-INI AND
                            DATA-VENCTO-CR20 NOT > VENCTO-FIM
                            IF SITUACAO-CR20 = 0
                               IF AUX-TIPO = 9 OR
                                  AUX-TIPO = TIPO-DOCTO-CR20
                                  IF GS-ACP-CARTAO = 0 OR
                                     GS-ACP-CARTAO = CARTAO-CRED-CR20
                                     PERFORM CONTINUA-COMPARACAO
                                  END-IF
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       CONTINUA-COMPARACAO SECTION.
           IF GS-PORTADOR = 0 OR   PORTADOR-CR20
              IF VALOR-TOT-CR20 NOT < GS-VALOR-INI  AND
                 VALOR-TOT-CR20 NOT > GS-VALOR-FIM
                 IF NUMERO-CHEQUE = SPACES OR
                    NUMERO-CHEQUE = NR-DOCTO-CR20
                    IF GS-LOTE = 0 OR LOTE-CR20
                       MOVE COD-COMPL-CR20   TO COD-COMPL-CG10
                                                COD-COMPL-CG11

                       READ CGD010 INVALID KEY
                            MOVE SPACES      TO COMPRADOR-CG10
                       END-READ
                       READ CGD011 INVALID KEY
                            MOVE ZEROS       TO CIDADE1-CG11
                       END-READ
                       MOVE CIDADE1-CG11     TO CIDADE
                       READ CAD010 INVALID KEY
                            MOVE SPACES      TO NOME-CID
                       END-READ
                       MOVE NOME-CID         TO CIDADE-WK
                       MOVE COMPRADOR-CG10   TO CLIENTE-WK

                       MOVE NR-DOCTO-CR20    TO NR-DUPLICATA-WK

                       MOVE CLIENTE-CR20(1:4) TO CONTRATO-WK

                       MOVE PORTADOR-CR20     TO PORTADOR
                       READ CAD018 INVALID KEY
                            MOVE "*****"      TO PORTADOR-WK
                       NOT INVALID KEY
                            MOVE NOME-PORT    TO PORTADOR-WK
                       END-READ

                       EVALUATE CARTEIRA-CR20
                         WHEN 1 MOVE "SIMPLES" TO CARTEIRA-WK
                         WHEN 2 MOVE "CAUCAO" TO CARTEIRA-WK
                         WHEN 3 MOVE "DESCONTO" TO CARTEIRA-WK
                       END-EVALUATE

                       MOVE DATA-VENCTO-CR20 TO VENC-WK
                       MOVE VALOR-TOT-CR20   TO VALOR-WK

                       ADD VALOR-TOT-CR20    TO GS-TOTAL-PERIODO
                       ADD 1                 TO GS-QTDE-TITULOS

                       MOVE DATA-MOVTO-CR20  TO DATA-MOVTO-WK
                       MOVE SEQ-CR20         TO SEQ-WK
                       MOVE COD-COMPL-CR20   TO COD-COMPL-WK

                       MOVE "Gravando "  TO GS-EXIBE-VENCTO(1: 9)
                       MOVE DATA-VENCTO-CR20 TO DATA-INV
                       CALL "GRIDAT1" USING DATA-INV
                       MOVE DATA-INV     TO DATA-E
                       MOVE DATA-E       TO GS-EXIBE-VENCTO
                       MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM

                       WRITE REG-WORK
                       END-WRITE.



       PELA-DATA-VECTO SECTION.
           INITIALIZE REG-CRD020

           MOVE GS-VENCTO-INI TO DATA-INV
                                 VENCTO-INI-REL.

           CALL "GRIDAT2"   USING DATA-INV.
           MOVE DATA-INV    TO DATA-VENCTO-CR20
                               VENCTO-INI.

           MOVE GS-VENCTO-FIM TO DATA-INV
                                 VENCTO-FIM-REL.

           CALL "GRIDAT2"   USING DATA-INV
           MOVE DATA-INV    TO VENCTO-FIM

           MOVE ZEROS       TO SITUACAO-CR20.
           MOVE GS-PORTADOR TO PORTADOR-CR20.

           START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID KEY
               MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
               READ CRD020 NEXT AT END
                   MOVE "10" TO ST-CRD020
               NOT AT END
                   IF DATA-VENCTO-CR20 > VENCTO-FIM OR SITUACAO-CR20 <>0
                      MOVE "10" TO ST-CRD020
                   ELSE
                      IF GS-PORTADOR = 0 or PORTADOR-CR20 AND
                         VALOR-TOT-CR20 NOT < GS-VALOR-INI AND
                         VALOR-TOT-CR20 NOT > GS-VALOR-FIM
                         IF NUMERO-CHEQUE = SPACES OR
                            NUMERO-CHEQUE = NR-DOCTO-CR20
                            IF AUX-TIPO = 9 OR
                               AUX-TIPO = TIPO-DOCTO-CR20
                               IF GS-ACP-CARTAO = 0 OR
                                  GS-ACP-CARTAO = CARTAO-CRED-CR20
                                  IF GS-LOTE = 0 OR LOTE-CR20

                               MOVE COD-COMPL-CR20   TO COD-COMPL-CG10
                                                        COD-COMPL-CG11
                               READ CGD010 INVALID KEY
                                    MOVE SPACES      TO COMPRADOR-CG10
                               END-READ
                               READ CGD011 INVALID KEY
                                    MOVE ZEROS       TO CIDADE1-CG11
                               END-READ
                               MOVE CIDADE1-CG11     TO CIDADE
                               READ CAD010 INVALID KEY
                                    MOVE SPACES      TO NOME-CID
                               END-READ
                               MOVE NOME-CID         TO CIDADE-WK
                               MOVE COMPRADOR-CG10   TO CLIENTE-WK

                               MOVE NR-DOCTO-CR20   TO NR-DUPLICATA-WK

                               MOVE CLIENTE-CR20(1:4) TO CONTRATO-WK

                               MOVE PORTADOR-CR20     TO PORTADOR
                               READ CAD018 INVALID KEY
                                    MOVE "*****"      TO PORTADOR-WK
                               NOT INVALID KEY
                                    MOVE NOME-PORT    TO PORTADOR-WK
                               END-READ

                               EVALUATE CARTEIRA-CR20
                                  WHEN 1 MOVE "SIMPLES" TO CARTEIRA-WK
                                  WHEN 2 MOVE "CAUCAO" TO CARTEIRA-WK
                                  WHEN 3 MOVE "DESCONTO" TO CARTEIRA-WK
                               END-EVALUATE

                               MOVE DATA-VENCTO-CR20 TO VENC-WK
                               MOVE VALOR-TOT-CR20   TO VALOR-WK

                               ADD VALOR-TOT-CR20    TO GS-TOTAL-PERIODO
                               ADD 1                 TO GS-QTDE-TITULOS

                               MOVE DATA-MOVTO-CR20  TO DATA-MOVTO-WK
                               MOVE SEQ-CR20         TO SEQ-WK
                               MOVE COD-COMPL-CR20   TO COD-COMPL-WK

                               MOVE "Gravando " TO GS-EXIBE-VENCTO(1: 9)
                               MOVE DATA-VENCTO-CR20 TO DATA-INV
                               CALL "GRIDAT1" USING DATA-INV
                               MOVE DATA-INV     TO DATA-E
                               MOVE DATA-E       TO GS-EXIBE-VENCTO
                               MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                               PERFORM CALL-DIALOG-SYSTEM

                               WRITE REG-WORK
                               END-WRITE
                               END-IF
                               END-IF
                            END-IF
                         END-IF
                   END-IF
               END-READ
           END-PERFORM.


       CARREGA-LISTA-CHEQUE SECTION.
           OPEN I-O WORK


           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       START-WORK.
           MOVE ZEROS TO GS-TOTAL-PERIODO GS-QTDE-TITULOS GS-AVENCER
                         GS-VENCIDAS TOTAL-ACUMULADO
                         AUX-DATA GS-CONTADOR AUX-VALOR

           INITIALIZE REG-WORK
           MOVE ZEROS TO VENC-WK NR-DUPLICATA-WK.

           EVALUATE GS-ORDEM
               WHEN 1  MOVE SPACES TO CLIENTE-WK
                       START WORK KEY IS NOT < ALT-WK1 INVALID KEY
                             MOVE "10" TO ST-WORK
                       END-START
               WHEN 2  MOVE SPACES TO CIDADE-WK
                       START WORK KEY IS NOT < ALT-WK2 INVALID KEY
                             MOVE "10" TO ST-WORK
                       END-START
               WHEN 3  MOVE ZEROS TO CONTRATO-WK
                       START WORK KEY IS NOT < ALT-WK3 INVALID KEY
                             MOVE "10" TO ST-WORK
                       END-START
               WHEN 4  MOVE SPACES TO PORTADOR-WK
                       START WORK KEY IS NOT < ALT-WK4 INVALID KEY
                             MOVE "10" TO ST-WORK
                       END-START
               WHEN 5  START WORK KEY IS NOT < ALT-WK5 INVALID KEY
                             MOVE "10" TO ST-WORK
                       END-START
               WHEN 6  MOVE ZEROS TO VALOR-WK
                       START WORK KEY IS NOT < ALT-WK5 INVALID KEY
                             MOVE "10" TO ST-WORK
                       END-START
           END-EVALUATE

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END

                   IF AUX-DATA = 0
                      MOVE VENC-WK TO AUX-DATA
                   END-IF

                   IF AUX-DATA <> VENC-WK
                      MOVE SPACES TO GS-LINDET
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      MOVE VENC-WK TO AUX-DATA
                      MOVE ZEROS TO TOTAL-ACUMULADO
                   END-IF

                   MOVE CLIENTE-WK      TO GS-LINDET(1:20)
                   MOVE CIDADE-WK       TO GS-LINDET(22:13)
                   MOVE NR-DUPLICATA-WK TO GS-LINDET(48:6)
                   MOVE CONTRATO-WK     TO GS-LINDET(58:4)
                   MOVE PORTADOR-WK     TO GS-LINDET(63:5)
                   MOVE CARTEIRA-WK     TO GS-LINDET(69:4)

                   MOVE VENC-WK         TO DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV        TO DATA-E
                   MOVE DATA-E          TO GS-LINDET(76: 10)

                   MOVE DATA-MOVTO-WK  TO DATA-MOVTO-CR20
                   MOVE SEQ-WK         TO SEQ-CR20
                   MOVE COD-COMPL-WK   TO COD-COMPL-CR20
                   READ CRD020 NOT INVALID KEY
                       MOVE COD-COMPL-CR20 TO COD-COMPL-CG10
                                              COD-COMPL-CG11
                       READ CGD010 INVALID KEY
                           MOVE SPACES TO COMPRADOR-CG10
                       END-READ
                       READ CGD011 INVALID KEY
                           MOVE ZEROS  TO CIDADE1-CG11
                       END-READ
                       MOVE CIDADE1-CG11 TO CIDADE
                       READ CAD010 INVALID KEY
                           MOVE SPACES TO NOME-CID
                       END-READ
                       MOVE NOME-CID       TO GS-LINDET(22: 13)
                       MOVE COMPRADOR-CG10 TO GS-LINDET(1: 20)
                       MOVE VALOR-TOT-CR20 TO VALOR-E1
                       MOVE VALOR-E1       TO GS-LINDET(86: 10)

                       ADD VALOR-TOT-CR20    TO GS-TOTAL-PERIODO
                       ADD 1                 TO GS-QTDE-TITULOS

                       IF DATA-VENCTO-CR20 > WS-DATA-CPU
                          COMPUTE GS-AVENCER = GS-AVENCER +
                          VALOR-TOT-CR20
                       ELSE
                          COMPUTE GS-VENCIDAS = GS-VENCIDAS +
                          VALOR-TOT-CR20
                       END-IF

                       COMPUTE TOTAL-ACUMULADO = TOTAL-ACUMULADO +
                                                 VALOR-TOT-CR20

                       MOVE TOTAL-ACUMULADO TO VALOR-E1
                       MOVE VALOR-E1        TO GS-LINDET(97:10)

                       MOVE "INSERE-LIST" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-READ
           END-PERFORM.

           CLOSE WORK.


       VERIFICA-SENHA-PORTADOR SECTION.
           MOVE COD-USUARIO-W TO CODIGO-CA002
           READ CAD002 INVALID KEY
                MOVE ZEROS    TO SENHA-W
           NOT INVALID KEY
                MOVE SENHA-CA002 TO SENHA-W
           END-READ

           MOVE 0             TO GS-SENHA-AUTORIZADA.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "SENHA02"     TO PROGRAMA-CA004.

           READ CAD004 INVALID KEY
                MOVE 2 TO GS-SENHA-AUTORIZADA
           NOT INVALID KEY
               MOVE SENHA-W  TO AUX-SENHA
               IF GS-SENHA <> SENHA-W
                  MOVE 1 TO GS-SENHA-AUTORIZADA
               ELSE
                  CONTINUE
           END-READ.
      *---------------------------------------------------------------
       TRANSFERE-PORTADOR SECTION.
           CLOSE    CRD020 CRD200 CRD201.
           OPEN I-O CRD020 CRD200 CRD201 WORK.

           MOVE "Transferindo "  TO GS-EXIBE-VENCTO(1: 13)
           MOVE DATA-VENCTO-CR20 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV     TO DATA-E
           MOVE DATA-E       TO GS-EXIBE-VENCTO(14: 10)
           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


       START-CHD010-TRANSF.
           MOVE ZEROS TO DATA-MOVTO-WK SEQ-WK.

           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                GO TO SAIR2.

           IF ST-WORK <> "00" AND "02"
              GO TO START-CHD010-TRANSF.

       READ-CHD010-TRANSF.
           READ WORK NEXT AT END
               GO TO SAIR2.

           IF ST-WORK <> "00" AND "02"
              GO TO READ-CHD010-TRANSF.

           MOVE DATA-MOVTO-WK  TO DATA-MOVTO-CR20
           MOVE SEQ-WK         TO SEQ-CR20
           MOVE COD-COMPL-WK   TO COD-COMPL-CR20
           READ CRD020 INVALID KEY
               CONTINUE
           NOT INVALID KEY

                PERFORM GRAVAR-ANOTACOES

                MOVE GS-PORTADOR-T  TO PORTADOR-CR20
                REWRITE REG-CRD020
                END-REWRITE


                MOVE DATA-MOVTO-CR20 TO GS-EXIBE-VENCTO
                MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-READ

           GO TO READ-CHD010-TRANSF.

       SAIR2.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           CLOSE      CRD020 CRD200 CRD201 WORK.
           OPEN INPUT CRD020 CRD200 CRD201.

       GRAVAR-ANOTACOES SECTION.
           INITIALIZE REG-CRD200
           MOVE COD-COMPL-CR20     TO COD-COMPL-CR200
           MOVE 1                  TO SEQ-CR200
           MOVE 0                  TO AUX-SEQUENCIA

           START CRD200 KEY IS NOT LESS CHAVE-CR200 INVALID KEY
               PERFORM GRAVAR-CRD200
               GO TO SAIR-CRD200.

           IF ST-CRD200 <> "00" AND "02"
              GO TO GRAVAR-ANOTACOES.

       READ-CRD200.
           READ CRD200 NEXT AT END
               PERFORM GRAVAR-CRD200
               GO TO SAIR-CRD200.

           IF ST-CRD200 <> "00" AND "02"
              GO TO READ-CRD200.

           IF COD-COMPL-CR20 = COD-COMPL-CR200
              MOVE SEQ-CR200 TO AUX-SEQUENCIA
           ELSE
              PERFORM GRAVAR-CRD200
              GO TO SAIR-CRD200.

           GO TO READ-CRD200.

       SAIR-CRD200.
           EXIT.


       GRAVAR-CRD200 SECTION.
           ADD 1 TO AUX-SEQUENCIA
           INITIALIZE REG-CRD200

           MOVE COD-COMPL-CR20     TO COD-COMPL-CR200
           MOVE AUX-SEQUENCIA      TO SEQ-CR200
      *    MOVE DATA-DIA-W         TO DATA-MOVTO-CR200
           MOVE WS-DATA-CPU        TO DATA-MOVTO-CR200

           MOVE 0                  TO DATA-RETORNO-CR200
           MOVE USUARIO-W          TO USUARIO-CR200
           MOVE 0                  TO SITUACAO-ANOTACAO-CR200

           MOVE HORA-BRA(1: 4)     TO HORA-MOVTO-CR200

           WRITE REG-CRD200 NOT INVALID KEY
               PERFORM GRAVAR-CRD201
           END-WRITE.

       GRAVAR-CRD201 SECTION.
           INITIALIZE REG-CRD201
           MOVE COD-COMPL-CR20     TO COD-COMPL-CR201
           MOVE AUX-SEQUENCIA      TO SEQ-CR201
           MOVE 1                  TO SUBSEQ-CR201
           STRING "TRANSF.PORTADOR-DUPLIC: " NR-DOCTO-CR20 "    - "
           GS-AUX-PORTADOR "-" GS-NOME-PORTADOR " P/ " GS-PORTADOR-T "-"
                      GS-DESC-PORTADOR-T INTO ANOTACAO-CR201

           WRITE REG-CRD201.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       IMPRIMIR-RELATORIO SECTION.
           OPEN I-O WORK
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO GS-LINDET.

       START-WORK10.
           INITIALIZE REG-WORK
           MOVE ZEROS TO GS-TOTAL-PERIODO GS-QTDE-TITULOS GS-AVENCER
                         GS-VENCIDAS

           MOVE ZEROS TO VENC-WK NR-DUPLICATA-WK AUX-DATA.

           EVALUATE GS-ORDEM
               WHEN 1  MOVE SPACES TO CLIENTE-WK
                       START WORK KEY IS NOT < ALT-WK1 INVALID KEY
                             GO TO SAIR10
                       END-START
               WHEN 2  MOVE SPACES TO CIDADE-WK
                       START WORK KEY IS NOT < ALT-WK2 INVALID KEY
                             GO TO SAIR10
                       END-START
               WHEN 3  MOVE ZEROS TO CONTRATO-WK
                       START WORK KEY IS NOT < ALT-WK3 INVALID KEY
                             GO TO SAIR10
                       END-START
               WHEN 4  MOVE SPACES TO PORTADOR-WK
                       START WORK KEY IS NOT < ALT-WK4 INVALID KEY
                             GO TO SAIR10
                       END-START
               WHEN 5  START WORK KEY IS NOT < ALT-WK5 INVALID KEY
                             GO TO SAIR10
                       END-START
               WHEN 6  MOVE ZEROS TO VALOR-WK
                       START WORK KEY IS NOT < ALT-WK5 INVALID KEY
                             GO TO SAIR10
                       END-START
           END-EVALUATE

           IF ST-WORK <> "00" AND "02"
              GO TO IMPRIMIR-RELATORIO.

       READ-WORK10.
           READ WORK NEXT AT END
               GO TO SAIR10.

           IF ST-WORK <> "00" AND "02"
              GO TO READ-WORK10.

           IF AUX-DATA = 0
              MOVE VENC-WK TO AUX-DATA.

           IF AUX-DATA <> VENC-WK
              MOVE SPACES TO GS-LINDET
              WRITE REG-RELAT FROM GS-LINDET
              ADD 1 TO LIN
              IF LIN > 56
                 PERFORM CABECALHO
              END-IF
              MOVE VENC-WK TO AUX-DATA
              MOVE ZEROS TO TOTAL-ACUMULADO.

           MOVE SPACES         TO GS-LINDET


           MOVE CLIENTE-WK      TO GS-LINDET(1:20)
           MOVE CIDADE-WK       TO GS-LINDET(22:13)
           MOVE NR-DUPLICATA-WK TO GS-LINDET(48:6)
           MOVE CONTRATO-WK     TO GS-LINDET(58:4)
           MOVE PORTADOR-WK     TO GS-LINDET(63:5)
           MOVE CARTEIRA-WK     TO GS-LINDET(69:4)
           MOVE VENC-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV        TO DATA-E
           MOVE DATA-E          TO GS-LINDET(76: 10)

           MOVE DATA-MOVTO-WK  TO DATA-MOVTO-CR20
           MOVE SEQ-WK         TO SEQ-CR20
           MOVE COD-COMPL-WK   TO COD-COMPL-CR20
           READ CRD020 NOT INVALID KEY
                IF SITUACAO-CR20 = 0
                   MOVE COD-COMPL-CR20 TO COD-COMPL-CG10
                                          COD-COMPL-CG11
                   READ CGD010 INVALID KEY
                       MOVE SPACES     TO COMPRADOR-CG10
                   END-READ
                   READ CGD011 INVALID KEY
                       MOVE ZEROS      TO CIDADE1-CG11
                   END-READ
                   MOVE CIDADE1-CG11   TO CIDADE
                   READ CAD010 INVALID KEY
                       MOVE SPACES     TO NOME-CID
                   END-READ
                   MOVE NOME-CID       TO GS-LINDET(22: 13)
                   MOVE COMPRADOR-CG10 TO GS-LINDET(1: 20)
                   MOVE VALOR-TOT-CR20 TO VALOR-E1
                   MOVE VALOR-E1       TO GS-LINDET(86: 10)

                   ADD VALOR-TOT-CR20  TO GS-TOTAL-PERIODO
                   ADD 1               TO GS-QTDE-TITULOS

                   IF DATA-VENCTO-CR20 > WS-DATA-CPU
                      COMPUTE GS-AVENCER = GS-AVENCER + VALOR-TOT-CR20
                   ELSE
                      COMPUTE GS-VENCIDAS = GS-VENCIDAS + VALOR-TOT-CR20
                   END-IF

                   COMPUTE TOTAL-ACUMULADO = TOTAL-ACUMULADO +
                                             VALOR-TOT-CR20

                   MOVE TOTAL-ACUMULADO TO VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET(97:10)


                   WRITE REG-RELAT FROM GS-LINDET
                   ADD 1 TO LIN
                   IF LIN > 56
                      PERFORM CABECALHO
                   END-IF
                END-IF
           END-READ

           GO TO READ-WORK10.

       SAIR10.
           CLOSE WORK
           MOVE GS-QTDE-TITULOS   TO QTDE-TITULO-REL
           MOVE GS-TOTAL-PERIODO  TO TOTAL-PERIODO-REL
           MOVE GS-AVENCER        TO TOTAL-AVENCER
           MOVE GS-VENCIDAS       TO TOTAL-VENCIDAS
           WRITE REG-RELAT FROM LINTOT AFTER 2.

           COPY DESCONDENSA.

           EXIT.

       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.



       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP062" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
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
           move "CRP062"            to logacess-programa
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

           CLOSE CRD020 CAD004 CAD018 CRD200 CRD201 CAD010 CGD010 CGD011
                 CGD020 CAD002 CRD020B
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
