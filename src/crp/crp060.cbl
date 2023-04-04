       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP060.
      *DATA: 06/09/2001
      *AUTORA: LUCIA
      *PROGRAMA: RESUMO DE ARRECADAÇÃO DE ARECEBER/CHEQUES
      *FUNÇÃO: Listar todos os títulos que pertencerem ao contrato sele-
      *        cionado, e tipo-docto-cr20 <> 2(org.evento) E CHEQUES.
      *        Quando o cheque possuir recebimento de juros (chd013),
      *        ele considera o valor-liquido-ch13 para somar
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CGPX010.
           COPY CRPX020.
           COPY CHPX010.
           COPY CHPX013.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CLIENTE-WK
                  ALTERNATE RECORD KEY IS NOME-CLIEN-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CGPW010.
       COPY CRPW020.
       COPY CHPW010.
       COPY CHPW013.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  CLASSIF-WK          PIC 9.
           05  CLIENTE-WK          PIC 9(8).
           05  NOME-CLIEN-WK       PIC X(30).
           05  VLR-TOTAL-WK        PIC 9(8)V99.
           05  PARC-TOTAL-WK       PIC 9(2).
           05  VLR-PAGO-WK         PIC 9(8)V99.
           05  PARC-PAGO-WK        PIC 9(2).
           05  VLR-ATRASADO-WK     PIC 9(8)V99.
           05  PARC-ATRASADO-WK    PIC 9(2).
           05  VLR-RESTANTE-WK     PIC 9(8)V99.
           05  PARC-RESTANTE-WK    PIC 9(2).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP060.CPB".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VLR-GERAL-TOT         PIC 9(8)V99  VALUE ZEROS.
           05  PARC-GERAL-TOT        PIC 9(4)     VALUE ZEROS.
           05  VLR-PAGO-TOT          PIC 9(8)V99  VALUE ZEROS.
           05  PARC-PAGO-TOT         PIC 9(4)     VALUE ZEROS.
           05  VLR-ATRASADO-TOT      PIC 9(8)V99  VALUE ZEROS.
           05  PARC-ATRASADO-TOT     PIC 9(4)     VALUE ZEROS.
           05  VLR-RESTANTE-TOT      PIC 9(8)V99  VALUE ZEROS.
           05  PARC-RESTANTE-TOT     PIC 9(4)     VALUE ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  CLIENTE-W             PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(30)    VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           COPY "PARAMETR".
       01  CAB01.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(60)   VALUE
           "RESUMO DE ARRECADACAO DE CONTAS A RECEBER/CHEQUE- ORDEM: ".
           05  ORDEM-REL           PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(6)    VALUE "TIPO: ".
           05  TIPO-REL            PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE "CONTRATO: ".
           05  CONTRATO-REL        PIC 9(4)    VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "COD. NOME                                   TOTAL
      -    "   PAGO          ATRASADO          RESTANTE ".
       01  LINDET.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Geral...:".
           05  TOTAL-GERAL-REL     PIC x(20)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Atrasado:".
           05  TOTAL-ATRASADO-REL  PIC X(20)   VALUE SPACES.
       01  LINTOT1.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Recebido:".
           05  TOTAL-PAGO-REL      PIC x(20)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Restante:".
           05  TOTAL-RESTANTE-REL  PIC X(20)   VALUE SPACES.

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
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE NOME-EMP           TO EMPRESA-REL
           MOVE EMPRESA            TO EMP-REC
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CHD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD013"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CGD010 CRD020 CHD010 CHD013.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
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
           move "CRP060"            to logacess-programa
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
               WHEN GS-PRINTER-FLG-TRUE
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-VENCTO-ANT
                    PERFORM CRIA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CRIA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

      *    GRAVA CONTAS A RECEBER
           MOVE 0             TO COD-COMPL-CR20(1: 1)
           MOVE GS-CONTRATO   TO COD-COMPL-CR20(2: 4)
           MOVE ZEROS         TO COD-COMPL-CR20(6: 4)
                                 SEQ-CR20

           START CRD020 KEY IS NOT < CHAVE-CR20 INVALID KEY
                  MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                  READ CRD020 NEXT RECORD AT END
                       MOVE "10" TO ST-CRD020
                  NOT AT END
                       MOVE COD-COMPL-CR20(2: 4) TO CONTRATO-W
                       IF GS-CONTRATO <> CONTRATO-W
                          MOVE "10" TO ST-CRD020
                       ELSE
                          IF SITUACAO-CR20 > 2
                             CONTINUE
                          ELSE
                             PERFORM CONT-GRAVA-WORK
                          END-IF
                       END-IF
                  END-READ
           END-PERFORM.

      *    GRAVACAO DE CHEQUE
           MOVE 0             TO COD-COMPL-CH10(1: 1)
           MOVE GS-CONTRATO   TO COD-COMPL-CH10(2: 4)
           MOVE ZEROS         TO COD-COMPL-CH10(6: 4)
           MOVE ZEROS         TO DATA-VENCTO-CH10
           START CHD010 KEY IS NOT < ALT-CH4 INVALID KEY
                  MOVE "10" TO ST-CHD010.

           PERFORM UNTIL ST-CHD010 = "10"
                  READ CHD010 NEXT RECORD AT END
                       MOVE "10" TO ST-CHD010
                  NOT AT END
                       MOVE COD-COMPL-CH10(2: 4) TO CONTRATO-W
                       IF GS-CONTRATO <> CONTRATO-W
                          MOVE "10" TO ST-CHD010
                       ELSE
                          IF SITUACAO-CH10 <> 0 AND <> 2 AND <> 5
                             CONTINUE
                          ELSE
                             PERFORM CONT-GRAVA-WORK-CHEQUE
                          END-IF
                       END-IF
                  END-READ
           END-PERFORM.


           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CONT-GRAVA-WORK SECTION.
           IF TIPO-DOCTO-CR20 = 2
              CONTINUE
           ELSE
              MOVE CLIENTE-CR20 TO CLIENTE-WK
                                   GS-EXIBE-VENCTO
              READ WORK INVALID KEY
                   INITIALIZE REG-WORK
                   PERFORM GRAVA-WORK
              NOT INVALID KEY
                   PERFORM REGRAVA-WORK
              END-READ
              MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           END-IF.
       CONT-GRAVA-WORK-CHEQUE SECTION.
           MOVE CLIENTE-CH10 TO CLIENTE-WK
                                GS-EXIBE-VENCTO
           READ WORK INVALID KEY
                INITIALIZE REG-WORK
                PERFORM GRAVA-WORK-CHEQUE
           NOT INVALID KEY
                PERFORM REGRAVA-WORK-CHEQUE
           END-READ
           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       GRAVA-WORK SECTION.
           MOVE CLASS-CLIENTE-CR20     TO CLASSIF-WK
                                          CLASSIF-CG10
           MOVE CLIENTE-CR20           TO CLIENTE-WK
                                          CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE "*******"         TO COMPRADOR-CG10.

           MOVE COMPRADOR-CG10         TO NOME-CLIEN-WK.
           MOVE 1                      TO PARC-TOTAL-WK.
           MOVE VALOR-TOT-CR20         TO VLR-TOTAL-WK
           IF DATA-RCTO-CR20 <> ZEROS
              MOVE VALOR-LIQ-CR20      TO VLR-PAGO-WK
              MOVE 1                   TO PARC-PAGO-WK
              IF VALOR-SALDO-CR20 > 0
                 MOVE VALOR-SALDO-CR20 TO VLR-ATRASADO-WK
                 MOVE 1                TO PARC-ATRASADO-WK
                 MOVE ZEROS            TO VLR-RESTANTE-WK
                                          PARC-RESTANTE-WK
              ELSE
                 MOVE ZEROS            TO VLR-ATRASADO-WK
                                          PARC-ATRASADO-WK
                                          VLR-RESTANTE-WK
                                          PARC-RESTANTE-WK
              END-IF
           ELSE
              IF DATA-VENCTO-CR20 < DATA-DIA-I
                 MOVE VALOR-TOT-CR20    TO VLR-ATRASADO-WK
                 MOVE 1                 TO PARC-ATRASADO-WK
                 MOVE ZEROS             TO VLR-PAGO-WK
                                           PARC-PAGO-WK
                                           VLR-RESTANTE-WK
                                           PARC-RESTANTE-WK
              ELSE
                 MOVE VALOR-TOT-CR20    TO VLR-RESTANTE-WK
                 MOVE 1                 TO PARC-RESTANTE-WK
                 MOVE ZEROS             TO VLR-PAGO-WK
                                           PARC-PAGO-WK
                                           VLR-ATRASADO-WK
                                           PARC-ATRASADO-WK
              END-IF
           END-IF.
           WRITE REG-WORK.
       REGRAVA-WORK SECTION.
           ADD VALOR-TOT-CR20          TO VLR-TOTAL-WK.
           ADD 1                       TO PARC-TOTAL-WK.
           IF DATA-RCTO-CR20 <> ZEROS
              ADD VALOR-LIQ-CR20       TO VLR-PAGO-WK
              ADD 1                    TO PARC-PAGO-WK
              IF VALOR-SALDO-CR20 > 0
                 ADD VALOR-SALDO-CR20  TO VLR-ATRASADO-WK
                 ADD 1                 TO PARC-ATRASADO-WK
              END-IF
           ELSE
              IF DATA-VENCTO-CR20 < DATA-DIA-I
                 ADD VALOR-TOT-CR20    TO VLR-ATRASADO-WK
                 ADD 1                 TO PARC-ATRASADO-WK
              ELSE
                 ADD VALOR-TOT-CR20    TO VLR-RESTANTE-WK
                 ADD 1                 TO PARC-RESTANTE-WK
              END-IF
           END-IF.
           REWRITE REG-WORK.
       GRAVA-WORK-CHEQUE SECTION.
           MOVE CLASS-CLIENTE-CH10   TO CLASSIF-WK
                                        CLASSIF-CG10
           MOVE CLIENTE-CH10         TO CLIENTE-WK
                                        CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE "*******"       TO COMPRADOR-CG10.

           MOVE COMPRADOR-CG10       TO NOME-CLIEN-WK
           MOVE VALOR-CH10           TO VLR-TOTAL-WK
           MOVE 1                    TO PARC-TOTAL-WK

           EVALUATE SITUACAO-CH10
               WHEN 2
      *             VALORES PAGOS
                    MOVE DATA-MOVTO-CH10   TO DATA-MOVTO-CH13
                    MOVE SEQ-CH10          TO SEQ-CH13
                    READ CHD013 INVALID KEY
                         MOVE VALOR-CH10    TO VLR-PAGO-WK
                    NOT INVALID KEY
                         COMPUTE VLR-PAGO-WK =
                                (VALOR-CH10 + VLR-JUROS-CH13 +
                                 VLR-MULTA-CH13) - VLR-DESCONTO-CH13
                    END-READ
                    MOVE 1                 TO PARC-PAGO-WK
                    MOVE ZEROS             TO VLR-ATRASADO-WK
                                              PARC-ATRASADO-WK
                                              VLR-RESTANTE-WK
                                              PARC-RESTANTE-WK
               WHEN 5
                    MOVE VALOR-CH10    TO VLR-ATRASADO-WK
                    MOVE 1             TO PARC-ATRASADO-WK
                    MOVE ZEROS         TO VLR-PAGO-WK
                                          PARC-PAGO-WK
                                          VLR-RESTANTE-WK
                                          PARC-RESTANTE-WK
               WHEN 0
                    MOVE VALOR-CH10    TO VLR-RESTANTE-WK
                    MOVE 1             TO PARC-RESTANTE-WK
                    MOVE ZEROS         TO VLR-PAGO-WK
                                          PARC-PAGO-WK
                                          VLR-ATRASADO-WK
                                          PARC-ATRASADO-WK
               WHEN OTHER
                    CONTINUE
           END-EVALUATE.
           WRITE REG-WORK.
       REGRAVA-WORK-CHEQUE SECTION.
           ADD VALOR-CH10       TO VLR-TOTAL-WK
           ADD 1                TO PARC-TOTAL-WK

           EVALUATE SITUACAO-CH10
               WHEN 2
      *          VALORES PAGOS
                 MOVE DATA-MOVTO-CH10   TO DATA-MOVTO-CH13
                 MOVE SEQ-CH10          TO SEQ-CH13
                 READ CHD013 INVALID KEY
                      ADD VALOR-CH10      TO VLR-PAGO-WK
                 NOT INVALID KEY
                      COMPUTE VLR-PAGO-WK =
                             (VALOR-CH10 + VLR-JUROS-CH13 +
                              VLR-MULTA-CH13) - VLR-DESCONTO-CH13
                 END-READ
                 ADD 1                 TO PARC-PAGO-WK
               WHEN 5
                 ADD VALOR-CH10        TO VLR-ATRASADO-WK
                 ADD 1                 TO PARC-ATRASADO-WK
               WHEN 0
                 ADD VALOR-CH10        TO VLR-RESTANTE-WK
                 ADD 1                 TO PARC-RESTANTE-WK
               WHEN OTHER CONTINUE
           END-EVALUATE.

           REWRITE REG-WORK.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO VLR-GERAL-TOT
                         PARC-GERAL-TOT
                         VLR-PAGO-TOT
                         PARC-PAGO-TOT
                         VLR-ATRASADO-TOT
                         PARC-ATRASADO-TOT
                         VLR-RESTANTE-TOT
                         PARC-RESTANTE-TOT

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    IF GS-ORDEM-RADIO  = 2 AND
                       VLR-ATRASADO-WK = 0
                       CONTINUE
                    ELSE
                       PERFORM MOVER-DADOS-LINDET
                    END-IF
               END-READ
           END-PERFORM.
           PERFORM TOTALIZA.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "CÓDIGO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO CLIENTE-WK
                START WORK KEY IS NOT < CLIENTE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CLIENTE" TO GS-DESCR-ORDEM
                MOVE SPACES TO NOME-CLIEN-WK
                START WORK KEY IS NOT < NOME-CLIEN-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           MOVE CLIENTE-WK(5: 4)  TO GS-LINDET(1: 5)
           MOVE NOME-CLIEN-WK     TO GS-LINDET(06: 31)
           MOVE VLR-TOTAL-WK      TO VALOR-E
      *    ADD VLR-TOTAL-WK       TO VLR-GERAL-TOT
           MOVE VALOR-E           TO GS-LINDET(37: 13)
           MOVE "("               TO GS-LINDET(50: 1)
           MOVE PARC-TOTAL-WK     TO GS-LINDET(51: 2)
           ADD PARC-TOTAL-WK      TO PARC-GERAL-TOT
           MOVE ")"               TO GS-LINDET(53: 2)
           MOVE VLR-PAGO-WK       TO VALOR-E
           ADD VLR-PAGO-WK        TO VLR-PAGO-TOT
           MOVE VALOR-E           TO GS-LINDET(55: 13)
           MOVE "("               TO GS-LINDET(68: 1)
           MOVE PARC-PAGO-WK      TO GS-LINDET(69: 2)
           ADD PARC-PAGO-WK       TO PARC-PAGO-TOT
           MOVE ")"               TO GS-LINDET(71: 2)
           MOVE VLR-ATRASADO-WK   TO VALOR-E
           ADD VLR-ATRASADO-WK    TO VLR-ATRASADO-TOT
           MOVE VALOR-E           TO GS-LINDET(73: 13)
           MOVE "("               TO GS-LINDET(86: 1)
           MOVE PARC-ATRASADO-WK  TO GS-LINDET(87: 2)
           ADD PARC-ATRASADO-WK   TO PARC-ATRASADO-TOT
           MOVE ")"               TO GS-LINDET(89: 2)
           MOVE VLR-RESTANTE-WK   TO VALOR-E
           ADD VLR-RESTANTE-WK    TO VLR-RESTANTE-TOT
           MOVE VALOR-E           TO GS-LINDET(91: 13)
           MOVE "("               TO GS-LINDET(104: 1)
           MOVE PARC-RESTANTE-WK  TO GS-LINDET(105: 2)
           ADD PARC-RESTANTE-WK   TO PARC-RESTANTE-TOT
           MOVE ")"               TO GS-LINDET(107: 2).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA SECTION.
           COMPUTE VLR-GERAL-TOT = VLR-PAGO-TOT + VLR-ATRASADO-TOT +
                                   VLR-RESTANTE-TOT.
           MOVE VLR-GERAL-TOT     TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-GERAL(1: 13)
           MOVE "("               TO GS-TOTAL-GERAL(14: 1)
           MOVE PARC-GERAL-TOT    TO GS-TOTAL-GERAL(15: 4)
           MOVE ")"               TO GS-TOTAL-GERAL(19: 1)
           MOVE VLR-PAGO-TOT      TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-PAGO(1: 13)
           MOVE "("               TO GS-TOTAL-PAGO(14: 1)
           MOVE PARC-PAGO-TOT     TO GS-TOTAL-PAGO(15: 4)
           MOVE ")"               TO GS-TOTAL-PAGO(19: 1)
           MOVE VLR-ATRASADO-TOT  TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-ATRASADO(1: 13)
           MOVE "("               TO GS-TOTAL-ATRASADO(14: 1)
           MOVE PARC-ATRASADO-TOT TO GS-TOTAL-ATRASADO(15: 4)
           MOVE ")"               TO GS-TOTAL-ATRASADO(19: 1)
           MOVE VLR-RESTANTE-TOT  TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-RESTANTE(1: 13)
           MOVE "("               TO GS-TOTAL-RESTANTE(14: 1)
           MOVE PARC-RESTANTE-TOT TO GS-TOTAL-RESTANTE(15: 4)
           MOVE ")"               TO GS-TOTAL-RESTANTE(19: 1).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP060" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           PERFORM ORDEM
           MOVE ZEROS TO LIN
           PERFORM CABECALHO
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    IF GS-ORDEM-RADIO  = 2 AND
                       VLR-ATRASADO-WK = 0
                       CONTINUE
                    ELSE
                       PERFORM MOVER-DADOS-RELATORIO
                    END-IF
               END-READ
           END-PERFORM.
           MOVE GS-TOTAL-GERAL    TO TOTAL-GERAL-REL.
           MOVE GS-TOTAL-PAGO     TO TOTAL-PAGO-REL.
           MOVE GS-TOTAL-ATRASADO TO TOTAL-ATRASADO-REL.
           MOVE GS-TOTAL-RESTANTE TO TOTAL-RESTANTE-REL.
           WRITE REG-RELAT FROM LINTOT AFTER 2
           WRITE REG-RELAT FROM LINTOT1

           MOVE SPACES TO REG-RELAT.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL.
           MOVE CLIENTE-WK(5: 4)  TO LINDET-REL(1: 5)
           MOVE NOME-CLIEN-WK     TO LINDET-REL(06: 31)
           MOVE VLR-TOTAL-WK      TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(37: 13)
           MOVE "("               TO LINDET-REL(50: 1)
           MOVE PARC-TOTAL-WK     TO LINDET-REL(51: 2)
           MOVE ")"               TO LINDET-REL(53: 2)
           MOVE VLR-PAGO-WK       TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(55: 13)
           MOVE "("               TO LINDET-REL(68: 1)
           MOVE PARC-PAGO-WK      TO LINDET-REL(69: 2)
           MOVE ")"               TO LINDET-REL(71: 2)
           MOVE VLR-ATRASADO-WK   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(73: 13)
           MOVE "("               TO LINDET-REL(86: 1)
           MOVE PARC-ATRASADO-WK  TO LINDET-REL(87: 2)
           MOVE ")"               TO LINDET-REL(89: 2)
           MOVE VLR-RESTANTE-WK   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(91: 13)
           MOVE "("               TO LINDET-REL(104: 1)
           MOVE PARC-RESTANTE-WK  TO LINDET-REL(105: 2)
           MOVE ")"               TO LINDET-REL(107: 2).

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE ZEROS TO TOTAL-W.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           MOVE GS-CONTRATO TO CONTRATO-REL.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           IF GS-ORDEM-RADIO = 1 MOVE "GERAL" TO TIPO-REL
           ELSE MOVE "ATRASADO"  TO TIPO-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
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
           move "CRP060"            to logacess-programa
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

           CLOSE CRD020 CGD010 CHD010 CHD013 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
