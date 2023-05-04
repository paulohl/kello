       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP056.
       DATE-WRITTEN. 09/06/1999.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: Relatório de cheques pré-datados - PROB/DEVOLV/BAIXADOS
      *FUNÇÃO: Listar todos os cheques que estiverem dentro do intervalo
      *        de vencimento. Situaçao = 2-baixado  5-devolvido
      *                                  6-problemático
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CGPX010.
           COPY CHPX010.
           COPY CHPX013.
           COPY CHPX010B.
           COPY CAPX018.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = DATA-MOVTO-WK SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK = VENCTO-WK VALOR-WK
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK =
                     PORTADOR-WK CARTEIRA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NOME-CLIEN-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK = SITUACAO-WK
                     VENCTO-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO "C:\TESTE.TXT"
                        ORGANIZATION IS LINE SEQUENTIAL.
      *                 PRINTER.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CGPW010.
       COPY CAPW018.
       COPY CHPW010.
       COPY CHPW013.
       COPY CHPW010B.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  CLASSIF-WK          PIC 9.
           05  CLIENTE-WK          PIC 9(8).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  SEQ-WK              PIC 9(4).
           05  NOME-CLIEN-WK       PIC X(20).
           05  NR-CHEQUE-WK        PIC X(7).
           05  PORTADOR-WK         PIC X(10).
           05  VENCTO-WK           PIC 9(8).
           05  VALOR-WK            PIC 9(8)V99.
           05  SITUACAO-WK         PIC X(4).
           05  CARTEIRA-WK         PIC X(4).
      *    05  ALINEA-WK           PIC 99.
           05  DATA-BAIXA-WK       PIC 9(8).
           05  JUROS-WK            PIC S9(6)V99.
           05  DESCONTO-WK         PIC S9(6)V99.
      *    ALINEA-DATA BAIXA-JUROS - CAMPOS A SEREM PREENCHIDOS EM
      *    CASO DE CHEQUES DEVOLVIDOS
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "CHP056.CPB".
           COPY "CHP056.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD010b            PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
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
           05  GRAVA-CHD013          PIC 9        VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  JUROS-E               PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DESC-E                PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  NOME-CLIEN-ANT        PIC X(20)    VALUE SPACES.
           05  PORTADOR-ANT          PIC X(10)    VALUE ZEROS.
           05  VENCTO-ANT            PIC 9(8)     VALUE ZEROS.
           05  SITUACAO-ANT          PIC X(4)     VALUE SPACES.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  SENHA-W1              PIC 9(4)     COMP-3.
           05  PASSAR-STRING         PIC X(65)    VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.

           COPY "PARAMETR".

       01 mensagem                   pic x(200).
       01 tipo-msg                   pic x(01).
       01 resp-msg                   pic x(01).
       01 DOIS-BRANCO                PIC 9(01) VALUE ZEROS.


       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

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
           05  FILLER              PIC X(41)   VALUE
           "RELACAO CHEQUE-PROB/BAIX/DEVOL-ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(119)  VALUE
           "DATA-VECTO NOME-CLIENTE         NR-CHEQ PORTADOR   SIT.
      -   "    VALOR DATA-BAIXA      JUROS   VLR-DESC            TOTAL".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(12)   VALUE "QT-TITULOS: ".
           05  QTDE-TITULO-REL     PIC ZZZZ.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "TOT-PERIODO: ".
           05  TOTAL-PERIODO-REL   PIC ZZ.ZZZ.ZZZ,ZZ.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU         PIC 9(04).
             10 WS-MES-CPU         PIC 9(02).
             10 WS-DIA-CPU         PIC 9(02).
          05 FILLER                PIC X(13).

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
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CHD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD013"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "CHD010B" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010B.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD200"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CAD018 CGD010 CAD004 CHD010B.
           OPEN I-O   CHD010 CHD013
           CLOSE      CHD010 CHD013
           OPEN INPUT CHD010 CHD013

           OPEN I-O   CRD200 CRD201
           CLOSE      CRD200 CRD201
           OPEN INPUT CRD200 CRD201

           IF ST-CHD010 = "35"  CLOSE CHD010  OPEN OUTPUT CHD010
                                CLOSE CHD010  OPEN I-O CHD010.
           IF ST-CHD013 = "35"  CLOSE CHD013  OPEN OUTPUT CHD013
                                CLOSE CHD013  OPEN I-O CHD013.
           IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
                                CLOSE CRD200  OPEN I-O CRD200.
           IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
                                CLOSE CRD201  OPEN I-O CRD201.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010B <> "00"
              MOVE "ERRO ABERTURA CHD010B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CHP056"            to logacess-programa
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
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-VENCTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM CHAMA-ALTERACAO
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM POPUP-PORTADOR
      *        WHEN GS-TRANSF-PORTADOR-TRUE
      *             PERFORM TRANSFERE-PORTADOR
               WHEN GS-VERIF-SENHA-TRUE
                    PERFORM VERIFICA-SENHA
               WHEN GS-VERIF-RCTO-DEVOLV-TRUE
                    PERFORM VERIFICA-RCTO-CH-DEVOLVIDO
               WHEN GS-RECTO-CH-DEVOLV-TRUE
                    PERFORM EFETUA-RCTO-CH-DEVOLV
               WHEN GS-REVERTE-TRUE
                    PERFORM REVERTER-SITUACAO
               WHEN GS-VERIFICA-SITUACAO-TRUE
                    PERFORM VERIFICA-SITUACAO
               WHEN GS-REVERTE-TODOS-TRUE
                    MOVE "Deseja Reverter Todos os Cheques?" TO MENSAGEM
                    MOVE "Q" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                    IF RESP-MSG = "S"
                       PERFORM REVERTER-TODAS-SITUACOES
                    END-IF
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       REVERTER-TODAS-SITUACOES SECTION.
           CLOSE    CHD010 CHD013
           OPEN I-O CHD010 CHD013

           INITIALIZE TOTAL-W
                      GS-TOTAL-PERIODO
                      GS-QTDE-TITULOS
                      DOIS-BRANCO

           MOVE 1          TO GS-LINHA
           MOVE SPACES     TO GS-LINDET
           MOVE "LER-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL DOIS-BRANCO = 2
               IF GS-LINDET = SPACES
                  ADD 1  TO DOIS-BRANCO
               ELSE
                  MOVE 0 TO DOIS-BRANCO
               END-IF
               MOVE GS-LINDET(110: 8)
                 TO PASSAR-STRING(1: 8) DATA-MOVTO-CH10
               MOVE GS-LINDET(119: 4)
                 TO PASSAR-STRING(10: 4) SEQ-CH10

               READ CHD010 NOT INVALID KEY
                    MOVE 0 TO SITUACAO-CH10
                    REWRITE REG-CHD010 NOT INVALID KEY
                       MOVE DATA-MOVTO-CH10 TO DATA-MOVTO-WK
                                               DATA-MOVTO-CH13
                       MOVE SEQ-CH10        TO SEQ-WK SEQ-CH13
                       READ CHD013 INVALID KEY
                            CONTINUE
                       NOT INVALID KEY
                            DELETE CHD013
                       END-READ
                       READ WORK INVALID KEY
                            CONTINUE
                       NOT INVALID KEY
                             MOVE "OK  " TO SITUACAO-WK
                             MOVE ZEROS  TO DATA-BAIXA-WK
                                            JUROS-WK
                             REWRITE REG-WORK
                             END-REWRITE
                             PERFORM MOVER-DADOS
                             MOVE "ATUALIZA-LISTA2" TO DS-PROCEDURE
                             PERFORM CALL-DIALOG-SYSTEM
                       END-READ
                    END-REWRITE
               END-READ

               ADD 1           TO GS-LINHA
               MOVE SPACES     TO GS-LINDET
               MOVE "LER-LIST" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM

           END-PERFORM

           CLOSE      CHD010 CHD013
           OPEN INPUT CHD010 CHD013

           REFRESH-OBJECT PRINCIPAL.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

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
       EFETUA-RCTO-CH-DEVOLV SECTION.
           CLOSE      CHD010 CHD013
           OPEN I-O   CHD010 CHD013

           MOVE DATA-MOVTO-CH10 TO DATA-MOVTO-WK DATA-MOVTO-CH13.
           MOVE SEQ-CH10        TO SEQ-WK SEQ-CH13.
           READ CHD013 INVALID KEY
                MOVE 0 TO GRAVA-CHD013
                INITIALIZE REG-CHD013
           NOT INVALID KEY
                MOVE 1 TO GRAVA-CHD013.

           READ WORK INVALID KEY
                CONTINUE
           NOT INVALID KEY
                MOVE GS-FORMA-PAGTO-D     TO FORMA-PAGTO-CH13
                MOVE GS-DATA-COMPRA-D     TO DATA-COMPRA-CH13
                MOVE GS-DATA-APRES-D      TO DATA-INV
                CALL "GRIDAT2" USING DATA-INV
                MOVE DATA-INV             TO DATA-APRES-CH13
                MOVE GS-DATA-REPRES-D     TO DATA-REAPRES-CH13
                MOVE GS-DATA-RECTO-D      TO DATA-INV
                                             DATA-BAIXA-WK
                CALL "GRIDAT2" USING DATA-INV
                MOVE DATA-INV             TO DATA-RECTO-CH13
                MOVE GS-VLR-JUROS-D       TO VLR-JUROS-CH13
                MOVE GS-VLR-DESCONTO-D    TO VLR-DESCONTO-CH13
                MOVE GS-VLR-MULTA-D       TO VLR-MULTA-CH13
                MOVE GS-ALINEA-D          TO ALINEA-CH13
      *                                      ALINEA-WK
                MOVE DATA-MOVTO-CH10      TO DATA-MOVTO-CH13
                MOVE GS-DATA-ENTRADA-D    TO DATA-ENTRADA-CH13
                MOVE GS-DATA-COMPRA-D     TO DATA-COMPRA-CH13
                MOVE GS-DATA-RECTO-D      TO DATA-RECTO-CH13
                MOVE GS-DCR-MEM-D         TO DCR-MEM-CH13

                MOVE SEQ-CH10             TO SEQ-CH13
                COMPUTE JUROS-WK = VLR-JUROS-CH13 +
                                   VLR-MULTA-CH13 - VLR-DESCONTO-CH13
                MOVE 2 TO SITUACAO-CH10

                MOVE "BAIX" TO SITUACAO-WK
                REWRITE REG-WORK
                END-REWRITE
                REWRITE REG-CHD010
                END-REWRITE
                IF GRAVA-CHD013 = 0
                   WRITE REG-CHD013
                   END-WRITE
                ELSE REWRITE REG-CHD013
                     END-REWRITE
                END-IF
           END-READ.

           CLOSE      CHD010 CHD013
           OPEN INPUT CHD010 CHD013

           PERFORM MOVER-DADOS.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM GRAVA-ANOTACAO-BAIXA-DEVOL.
       GRAVA-ANOTACAO-BAIXA-DEVOL SECTION.
           CLOSE      CRD200 CRD201
           OPEN I-O   CRD200 CRD201
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
                 READ CRD200 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD200
                 NOT AT END
                     IF COD-COMPL-CR200 <> COD-COMPL-CH10
                        MOVE "10" TO ST-CRD200
                     ELSE
                        MOVE SEQ-CR200 TO ULT-SEQ
                        CONTINUE
                 END-READ
           END-PERFORM.
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ      TO SEQ-CR200
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200.
           MOVE ZEROS        TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE DATA-DIA-I   TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200       TO SEQ-CR201.
           MOVE COD-COMPL-CR200 TO COD-COMPL-CR201.
           MOVE ZEROS           TO SUBSEQ-CR201.
           MOVE "RECTO EFETUADO NO CH.DEVOLV.            - MOTIVO: "
                  TO ANOTACAO-CR201(1: 80)
           MOVE NR-CHEQUE-CH10  TO ANOTACAO-CR201(30: 10).
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.

           PERFORM VARYING COL1 FROM 1 BY 80 UNTIL COL1 > 640
              MOVE GS-ANOTACAO(COL1: 80) TO ANOTACAO-W
              MOVE ANOTACAO-W TO ANOTACAO-CR201
              IF ANOTACAO-W <> SPACES
                 ADD 1 TO SUBSEQ-CR201
                 WRITE REG-CRD201 INVALID KEY
                       ADD 1 TO SUBSEQ-CR201
                       WRITE REG-CRD201
                       END-WRITE
                 END-WRITE
              END-IF
           END-PERFORM

           CLOSE      CRD200 CRD201
           OPEN INPUT CRD200 CRD201.
       VERIFICA-RCTO-CH-DEVOLVIDO SECTION.
      *  Verifica a possibilidade de se fazer a baixa do ch.devolvido
           MOVE 0             TO GS-ERRO-RCTO-DEVOLVIDO.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "SENHA01"     TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                MOVE 2 TO GS-ERRO-RCTO-DEVOLVIDO
           NOT INVALID KEY
           MOVE GS-LINDET(110: 8) TO DATA-MOVTO-CH10
           MOVE GS-LINDET(119: 4) TO SEQ-CH10
           READ CHD010 INVALID KEY
                MOVE 1 TO GS-ERRO-RCTO-DEVOLVIDO
           NOT INVALID KEY
                MOVE VALOR-CH10              TO GS-VALOR-TOTAL
                MOVE DATA-MOVTO-CH10         TO DATA-MOVTO-CH13
                MOVE SEQ-CH10                TO SEQ-CH13
                READ CHD013 INVALID KEY
                     INITIALIZE REG-CHD013
                END-READ
                MOVE VLR-JUROS-CH13          TO GS-VLR-JUROS-D
                MOVE VLR-MULTA-CH13          TO GS-VLR-MULTA-D
                MOVE VLR-DESCONTO-CH13       TO GS-VLR-DESCONTO-D
                MOVE ALINEA-CH13             TO GS-ALINEA-D
                MOVE DATA-RECTO-CH13         TO DATA-INV
                CALL "GRIDAT1" USING DATA-INV
                MOVE DATA-INV                TO GS-DATA-RECTO-D
                MOVE DATA-APRES-CH13         TO DATA-INV
                CALL "GRIDAT1" USING DATA-INV
                MOVE DATA-INV                TO GS-DATA-APRES-D
                MOVE DATA-REAPRES-CH13       TO GS-DATA-REPRES-D
                MOVE FORMA-PAGTO-CH13        TO GS-FORMA-PAGTO-D
                MOVE DATA-ENTRADA-CH13       TO GS-DATA-ENTRADA-D
                MOVE DATA-COMPRA-CH13        TO GS-DATA-COMPRA-D
                MOVE DATA-RECTO-CH13         TO GS-DATA-RECTO-D
                MOVE DCR-MEM-CH13            TO GS-DCR-MEM-D

                IF SITUACAO-CH10 <> 5
                     MOVE 1 TO GS-ERRO-RCTO-DEVOLVIDO.

      *    GS-ERRO-RCTO-DEVOLVIDO = 0(PROCESSO LIBERADO)
      *       = 1(CHEQUE NÃO PODE SER BAIXADO. PORQUE O CHEQUE NÃO
      *         POSSUI SITUACAO = 5(DEVOLVIDO))
      *       = 2(USUARIO NÃO LIBERADO P/ ESSE PROCEDIMENTO)
       VERIFICA-SENHA SECTION.
           MOVE GS-SENHA TO SENHA-W1
           IF SENHA-W1 <> SENHA-W
              MOVE 1 TO GS-SENHA-AUTORIZADA
           ELSE
              MOVE 0 TO GS-SENHA-AUTORIZADA.
       REVERTER-SITUACAO SECTION.
           CLOSE      CHD010 CHD013
           OPEN I-O   CHD010 CHD013
           MOVE 0 TO SITUACAO-CH10
           REWRITE REG-CHD010.

           MOVE DATA-MOVTO-CH10 TO DATA-MOVTO-WK
                                   DATA-MOVTO-CH13
           MOVE SEQ-CH10        TO SEQ-WK SEQ-CH13
           READ CHD013 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE CHD013.

           READ WORK INVALID KEY
                CONTINUE
           NOT INVALID KEY
                 MOVE "OK  " TO SITUACAO-WK
                 MOVE ZEROS TO DATA-BAIXA-WK JUROS-WK
                 REWRITE REG-WORK
                 END-REWRITE
                 PERFORM MOVER-DADOS
                 MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           END-READ

           CLOSE      CHD010 CHD013
           OPEN INPUT CHD010 CHD013.

       VERIFICA-SITUACAO SECTION.
      *  Verifica a possibilidade de reverter a baixa do ch.
           MOVE 0             TO GS-CH-RECEBIDO

           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "SENHA03"     TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                MOVE 2 TO GS-CH-RECEBIDO
             NOT INVALID KEY
               IF GS-LINDET = SPACES
                  MOVE ZEROS  TO GS-LINDET
               END-IF
               MOVE GS-LINDET(110: 8) TO DATA-MOVTO-CH10
               MOVE GS-LINDET(119: 4) TO SEQ-CH10
               READ CHD010 INVALID KEY
                    CONTINUE
               NOT INVALID KEY
                   IF SITUACAO-CH10 = 2
                      MOVE 1 TO GS-CH-RECEBIDO
                   END-IF
               END-READ.

       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR TO PORTADOR.
           READ CAD018 INVALID KEY
                MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT   TO GS-DESC-PORTADOR.
       POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP018T".
           MOVE PASSAR-PARAMETROS(1: 30) TO GS-DESC-PORTADOR
           MOVE PASSAR-PARAMETROS(1: 30) TO GS-DESC-PORTADOR-T
           MOVE PASSAR-PARAMETROS(33: 2) TO GS-PORTADOR
           MOVE PASSAR-PARAMETROS(33: 2) TO GS-PORTADOR-T.
      *TRANSFERE-PORTADOR SECTION.
      *    MOVE ZEROS TO CLASSIF-WK CLIENTE-WK SEQ-WK.
      *    START WORK KEY IS NOT < CHAVE-WK INVALID KEY
      *          MOVE "10" TO ST-WORK.
      *
      *    PERFORM UNTIL ST-WORK = "10"
      *      READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
      *        NOT AT END
      *           MOVE CLIENTE-WK   TO GS-EXIBE-TRANSF
      *           MOVE "REFRESH-WIN5" TO DS-PROCEDURE
      *           PERFORM CALL-DIALOG-SYSTEM
      *           MOVE GS-DESC-PORTADOR-T TO PORTADOR-WK
      *           REWRITE REG-WORK
      *           END-REWRITE
      *           MOVE DATA-MOVTO-WK TO DATA-MOVTO-CH10
      *           MOVE SEQ-WK        TO SEQ-CH10
      *           READ CHD010 INVALID KEY CONTINUE
      *             NOT INVALID KEY
      *               PERFORM GRAVA-ANOTACAO
      *               MOVE GS-PORTADOR-T TO PORTADOR-CH10
      *               REWRITE REG-CHD010
      *               END-REWRITE
      *           END-READ
      *      END-READ
      *    END-PERFORM.
      *    MOVE "UNSHOW-WIN5" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.
      *GRAVA-ANOTACAO SECTION.
      *    MOVE COD-COMPL-CH10 TO COD-COMPL-CR200
      *    MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
      *    START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
      *          MOVE "10" TO ST-CRD200.
      *    PERFORM UNTIL ST-CRD200 = "10"
      *      READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
      *        NOT AT END
      *          IF COD-COMPL-CR200 <> COD-COMPL-CH10
      *                       MOVE "10" TO ST-CRD200
      *          ELSE MOVE SEQ-CR200 TO ULT-SEQ
      *               CONTINUE
      *      END-READ
      *    END-PERFORM.
      *    ADD 1 TO ULT-SEQ.
      *    MOVE ULT-SEQ      TO SEQ-CR200.
      *    MOVE COD-COMPL-CH10 TO COD-COMPL-CR200.
      *    MOVE ZEROS        TO DATA-RETORNO-CR200
      *    MOVE USUARIO-W    TO USUARIO-CR200
      *    MOVE DATA-DIA-I   TO DATA-MOVTO-CR200
      *    MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200
      *
      *    MOVE ZEROS TO ST-CRD200.
      *    PERFORM UNTIL ST-CRD200 = "10"
      *       WRITE REG-CRD200 INVALID KEY
      *          ADD 1 TO SEQ-CR200
      *          CONTINUE
      *        NOT INVALID KEY MOVE "10" TO ST-CRD200
      *    END-PERFORM.
      *
      *    MOVE SEQ-CR200      TO SEQ-CR201.
      *    MOVE COD-COMPL-CH10 TO COD-COMPL-CR201.
      *    MOVE "TRANSF.PORTADOR-CHEQUE: XXXXXXXXXX - 01-XXXXXXXXXXXXXXX
      *-    "X P/ 99-XXXXXXXXXXXXXXXX" TO ANOTACAO-CR201.
      *    MOVE NR-CHEQUE-CH10      TO ANOTACAO-CR201(25: 11)
      *    MOVE PORTADOR-CH10       TO ANOTACAO-CR201(38: 2) PORTADOR
      *    READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
      *    MOVE NOME-PORT           TO ANOTACAO-CR201(41: 16)
      *    MOVE GS-PORTADOR-T       TO ANOTACAO-CR201(61: 2) PORTADOR
      *    READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
      *    MOVE NOME-PORT           TO ANOTACAO-CR201(64: 16)
      *    MOVE ZEROS TO ST-CRD201.
      *    MOVE 1              TO SUBSEQ-CR201.
      *    PERFORM UNTIL ST-CRD201 = "10"
      *      WRITE REG-CRD201 INVALID KEY
      *        ADD 1 TO SUBSEQ-CR201
      *        CONTINUE
      *       NOT INVALID KEY
      *         MOVE "10" TO ST-CRD201
      *      END-WRITE
      *    END-PERFORM.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.
           CLOSE       WORK.
           OPEN I-O    WORK.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-VENCTO-INI TO DATA-INV VENCTO-INI-ANT
                                 VENCTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV      TO VENCTO-INI.
           MOVE GS-VENCTO-FIM TO DATA-INV VENCTO-FIM-ANT
                                 VENCTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV      TO VENCTO-FIM.
           MOVE VENCTO-INI    TO DATA-VENCTO-CH10
           MOVE ZEROS         TO PORTADOR-CH10.
           MOVE 1             TO SITUACAO-CH10.
           START CHD010 KEY IS NOT < ALT-CH2 INVALID KEY
                  MOVE "10" TO ST-CHD010.

           PERFORM UNTIL ST-CHD010 = "10"
                  READ CHD010 NEXT RECORD AT END
                       MOVE "10" TO ST-CHD010
                  NOT AT END
                       MOVE DATA-VENCTO-CH10 TO GS-EXIBE-VENCTO
                       MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       IF SITUACAO-CH10 = 3 OR 4
                          MOVE 5 TO SITUACAO-CH10
                          PERFORM REPOSICIONA-CHAVE
                       ELSE
                        IF GS-PORTADOR <> 00
                           IF GS-PORTADOR <> PORTADOR-CH10
                              CONTINUE
                           ELSE
                              PERFORM CONT-GRAVA-WORK
                           END-IF
                        ELSE
                           PERFORM CONT-GRAVA-WORK
                        END-IF
                       END-IF
                  END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CONT-GRAVA-WORK SECTION.
           IF DATA-VENCTO-CH10 < VENCTO-INI
              PERFORM REPOSICIONA-CHAVE
           ELSE
            IF DATA-VENCTO-CH10 > VENCTO-FIM
               ADD 1 TO SITUACAO-CH10
               PERFORM REPOSICIONA-CHAVE
            ELSE
             EVALUATE SITUACAO-CH10
               WHEN 1 IF GS-BAIXADO = 1
                         PERFORM MOVER-DADOS-WORK
                      ELSE
                         IF GS-DEVOLVIDO = 1
                            PERFORM MOVER-DADOS-WORK
                         END-IF
                      END-IF
               WHEN 2 IF GS-BAIXADO = 1
                         PERFORM MOVER-DADOS-WORK
                      END-IF
               WHEN 5 IF GS-DEVOLVIDO = 1
                         PERFORM MOVER-DADOS-WORK
               WHEN 6 IF GS-PROBLEMATICO = 1
                         PERFORM MOVER-DADOS-WORK
             END-EVALUATE
             WRITE REG-WORK.
       REPOSICIONA-CHAVE SECTION.
           MOVE VENCTO-INI TO DATA-VENCTO-CH10.
           MOVE ZEROS TO PORTADOR-CH10.
           START CHD010 KEY IS NOT < ALT-CH2 INVALID KEY
                 MOVE "10" TO ST-CHD010.
       MOVER-DADOS-WORK SECTION.
           MOVE DATA-VENCTO-CH10    TO VENCTO-WK
           MOVE CLASS-CLIENTE-CH10  TO CLASSIF-WK
                                       CLASSIF-CG10
           MOVE CLIENTE-CH10        TO CLIENTE-WK.
           MOVE NOME-CH10           TO NOME-CLIEN-WK.
           MOVE NR-CHEQUE-CH10      TO NR-CHEQUE-WK
           MOVE DATA-MOVTO-CH10     TO DATA-MOVTO-WK
           MOVE SEQ-CH10            TO SEQ-WK
           MOVE PORTADOR-CH10       TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "******"       TO NOME-PORT.
           MOVE NOME-PORT           TO PORTADOR-WK
           MOVE VALOR-SALDO-CH10    TO VALOR-WK
           EVALUATE CARTEIRA-CH10
             WHEN 1 MOVE "SIMP"     TO CARTEIRA-WK
             WHEN 2 MOVE "CAUC"     TO CARTEIRA-WK
             WHEN 3 MOVE "DESC"     TO CARTEIRA-WK
           END-EVALUATE.
           MOVE DATA-MOVTO-CH10   TO DATA-MOVTO-CH13
           MOVE SEQ-CH10          TO SEQ-CH13.
           READ CHD013 INVALID KEY
                INITIALIZE REG-CHD013.
           MOVE DATA-RECTO-CH13           TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                  TO DATA-BAIXA-WK.
      *    MOVE ALINEA-CH13               TO ALINEA-WK.
           COMPUTE JUROS-WK = VLR-JUROS-CH13 +
                              VLR-MULTA-CH13
           MOVE VLR-DESCONTO-CH13         TO DESCONTO-WK
           MOVE VALOR-SALDO-CH10          TO VALOR-WK

           IF SITUACAO-CH10 = 2
              MOVE ZEROS            TO VALOR-WK
              INITIALIZE REG-CHD010B
              MOVE DATA-MOVTO-CH10     TO DATA-MOVTO-CH10B
              MOVE SEQ-CH10            TO SEQ-CH10B
              START CHD010B KEY IS NOT LESS CHAVE-CH10B INVALID KEY
                   MOVE "10" TO ST-CHD010B
              END-START
              PERFORM UNTIL ST-CHD010B = "10"
                   READ CHD010B NEXT AT END
                        MOVE "10" TO ST-CHD010B
                   NOT AT END
                        IF DATA-MOVTO-CH10 <> DATA-MOVTO-CH10B OR
                           SEQ-CH10        <> SEQ-CH10B
                           MOVE "10" TO ST-CHD010B
                        ELSE
                           COMPUTE VALOR-WK = VALOR-WK          +
                                              VALOR-BAIXA-CH10B +
                                              JURO-RCTO-CH10B   +
                                              MULTA-RCTO-CH10B  -
                                              DESCONTO-CH10B
                        END-IF
                   END-READ
              END-PERFORM
           END-IF
           IF SITUACAO-CH10 = 1
              IF GS-BAIXADO = 1
                 MOVE ZEROS            TO VALOR-WK
              END-IF
              INITIALIZE REG-CHD010B
              MOVE DATA-MOVTO-CH10     TO DATA-MOVTO-CH10B
              MOVE SEQ-CH10            TO SEQ-CH10B
              START CHD010B KEY IS NOT LESS CHAVE-CH10B INVALID KEY
                   MOVE "10" TO ST-CHD010B
              END-START
              PERFORM UNTIL ST-CHD010B = "10"
                   READ CHD010B NEXT AT END
                        MOVE "10" TO ST-CHD010B
                   NOT AT END
                        IF DATA-MOVTO-CH10 <> DATA-MOVTO-CH10B OR
                           SEQ-CH10        <> SEQ-CH10B
                           MOVE "10" TO ST-CHD010B
                        ELSE
                           IF GS-BAIXADO = 1
                              COMPUTE VALOR-WK = VALOR-WK          +
                                                 VALOR-BAIXA-CH10B +
                                                 JURO-RCTO-CH10B   +
                                                 MULTA-RCTO-CH10B  -
                                                 DESCONTO-CH10B
                           END-IF
                           MOVE DATA-RCTO-CH10B    TO DATA-INV
                           CALL "GRIDAT1" USING DATA-INV
                           MOVE DATA-INV           TO DATA-BAIXA-WK
                        END-IF
                   END-READ
              END-PERFORM
           END-IF
           EVALUATE SITUACAO-CH10
             WHEN 0 MOVE "OK"   TO SITUACAO-WK
             WHEN 1 MOVE "PARC" TO SITUACAO-WK
             WHEN 2 MOVE "BAIX" TO SITUACAO-WK
             WHEN 5 MOVE "DEVO" TO SITUACAO-WK
             WHEN 6 MOVE "PROB" TO SITUACAO-WK
           END-EVALUATE.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO GS-TOTAL-PERIODO GS-QTDE-TITULOS.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "VENCTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK
                START WORK KEY IS NOT < VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CLIENTE" TO GS-DESCR-ORDEM
                MOVE SPACES TO NOME-CLIEN-WK
                START WORK KEY IS NOT < NOME-CLIEN-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "PORTADOR/CART" TO GS-DESCR-ORDEM
                MOVE SPACES TO PORTADOR-WK CARTEIRA-WK
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "VCTO/VALOR" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK VALOR-WK
                START WORK KEY IS NOT < ALT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "SITUAÇÃO/VCTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO SITUACAO-WK VENCTO-WK
                START WORK KEY IS NOT < ALT2-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF NOME-CLIEN-ANT  NOT = ZEROS
                 IF NOME-CLIEN-ANT NOT = NOME-CLIEN-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF PORTADOR-ANT NOT = SPACES
                 IF PORTADOR-ANT NOT = PORTADOR-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
             WHEN 5
              IF SITUACAO-ANT NOT = SPACES
                 IF SITUACAO-ANT NOT = SITUACAO-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE NOME-CLIEN-WK     TO GS-LINDET(01: 21)
           MOVE NR-CHEQUE-WK      TO GS-LINDET(22: 8)
           MOVE PORTADOR-WK       TO GS-LINDET(30: 11)
           MOVE SITUACAO-WK       TO GS-LINDET(41: 5)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(46: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(57: 14)
           ADD VALOR-WK           TO TOTAL-W GS-TOTAL-PERIODO.
           ADD 1                  TO GS-QTDE-TITULOS
           MOVE DATA-BAIXA-WK     TO DATA-E
           MOVE DATA-E            TO GS-LINDET(71: 11)
           MOVE JUROS-WK          TO JUROS-E
           MOVE JUROS-E           TO GS-LINDET(82: 12)
           MOVE DESCONTO-WK       TO DESC-E
           MOVE DESC-E            TO GS-LINDET(93:10)
      *    MOVE ALINEA-WK         TO GS-LINDET(93: 3)
           MOVE TOTAL-W           TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(104: 13)
           MOVE DATA-MOVTO-WK     TO GS-LINDET(118: 8)
           MOVE SEQ-WK            TO GS-LINDET(127: 4).
       ZERA-VARIAVEIS SECTION.
           MOVE SPACES TO PORTADOR-ANT NOME-CLIEN-ANT SITUACAO-ANT.
           MOVE ZEROS TO VENCTO-ANT TOTAL-W.
       MOVER-CHAVE-ANT SECTION.
           MOVE NOME-CLIEN-WK     TO NOME-CLIEN-ANT.
           MOVE VENCTO-WK         TO VENCTO-ANT.
           MOVE PORTADOR-WK       TO PORTADOR-ANT.
           MOVE SITUACAO-WK       TO SITUACAO-ANT.
       TOTALIZA SECTION.
           MOVE ZEROS TO TOTAL-W.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CHAMA-ALTERACAO SECTION.
           IF GS-LINDET = SPACES MOVE ZEROS TO GS-LINDET.
           MOVE GS-LINDET(118: 8) TO PASSAR-STRING(1: 8) DATA-MOVTO-CH10
           MOVE GS-LINDET(127: 4) TO PASSAR-STRING(10: 4) SEQ-CH10.
           MOVE USUARIO-W         TO PASSAR-STRING(20: 5)

           MOVE COD-USUARIO-W     TO COD-USUARIO-CA004
                                     PASSAR-STRING(26: 3)
           MOVE "CHP010A"         TO PROGRAMA-CA004.

           CALL   "CHP010A" USING PARAMETROS-W PASSAR-STRING
           CANCEL "CHP010A".
      *    READ CAD004 INVALID KEY
      *        CALL "CHP010B" USING PASSAR-STRING
      *        CANCEL "CHP010B"
      *    NOT INVALID KEY
      *        CALL "CHP010A" USING PASSAR-STRING
      *        CANCEL "CHP010A".

           READ CHD010.
           MOVE DATA-MOVTO-CH10    TO DATA-MOVTO-WK.
           MOVE SEQ-CH10           TO SEQ-WK.
           READ WORK.
           PERFORM MOVER-DADOS-WORK.
           REWRITE REG-WORK.
           PERFORM MOVER-DADOS.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    Deveria ser regravado o arquivo work, pois pode ter havido
      *    alguma alteração no arquivo em que diferencie a classificação
      *    por exemplo, mudar a data de vencto fora do intervalo soli-
      *    citado pelo usuário. Mas foi solicitado que não regrave.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CHP056" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
                READ WORK NEXT RECORD AT END
                     MOVE "10" TO ST-WORK
                NOT AT END
                     PERFORM MOVER-DADOS-RELATORIO
                END-READ
           END-PERFORM.
           MOVE GS-QTDE-TITULOS   TO QTDE-TITULO-REL.
           MOVE GS-TOTAL-PERIODO  TO TOTAL-PERIODO-REL.
           WRITE REG-RELAT FROM LINTOT AFTER 2.

           MOVE SPACES TO REG-RELAT.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF NOME-CLIEN-ANT NOT = ZEROS
                 IF NOME-CLIEN-ANT NOT = NOME-CLIEN-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF PORTADOR-ANT NOT = SPACES
                 IF PORTADOR-ANT NOT = PORTADOR-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF SITUACAO-ANT NOT = SPACES
                 IF SITUACAO-ANT NOT = SITUACAO-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.

           MOVE NOME-CLIEN-WK     TO LINDET-REL(01: 21)
           MOVE NR-CHEQUE-WK      TO LINDET-REL(22: 8)
           MOVE PORTADOR-WK       TO LINDET-REL(30: 11)
           MOVE SITUACAO-WK       TO LINDET-REL(41: 5)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(46: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(57: 14)
           ADD VALOR-WK           TO TOTAL-W.
           MOVE DATA-BAIXA-WK     TO DATA-E
           MOVE DATA-E            TO LINDET-REL(71: 11)
           MOVE JUROS-WK          TO JUROS-E
           MOVE JUROS-E           TO LINDET-REL(82: 12)
           MOVE DESCONTO-WK       TO DESC-E
           MOVE DESC-E            TO LINDET-REL(93:10)
      *    MOVE ALINEA-WK         TO LINDET-REL(93: 3)
           MOVE TOTAL-W           TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(104: 13)

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE ZEROS TO TOTAL-W.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
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
           move "CHP056"            to logacess-programa
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

           CLOSE CHD010 CHD013 CAD018 CGD010 CRD200 CRD201 CHD010B.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
