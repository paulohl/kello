       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP055.
      *DATA: 08/06/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Relatório de cheques pre-datados por ordem de vendedor
      *        dentro do intervalo de data de movimento solicitado.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CHPX010.
           COPY CAPX018.
           COPY CHPX012.
           COPY RCPX100.
           COPY RCPX101.
           COPY LOGACESS.SEL.


           SELECT AUXILIAR ASSIGN TO PATH-AUXILIAR
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-AUX = DATA-MOVTO-AUX SEQ-AUX
                  ALTERNATE RECORD KEY ALT-AUX1 =
                     NOME-AUX PORTADOR-AUX WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-AUX2 = SITUACAO-AUX
                     DATA-VENCTO-AUX PORTADOR-AUX WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-AUX3 = DATA-MOVTO-AUX
                     VENDEDOR-AUX SEQ-AUX WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-AUX4 = COD-COMPL-AUX
                     DATA-VENCTO-AUX WITH DUPLICATES
                  STATUS IS ST-AUXILIAR.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CHPW010.
       COPY CGPW001.
       COPY CAPW018.
       COPY CHPW012.
       COPY RCPW100.
       COPY RCPW101.
       COPY LOGACESS.FD.

       FD  AUXILIAR.
       01  REG-AUX.
           05  DATA-MOVTO-AUX           PIC 9(8).
           05  SEQ-AUX                  PIC 9(4).
           05  COD-COMPL-AUX.
               10  CLASS-CLIENTE-AUX    PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-AUX          PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-AUX                 PIC X(30).
           05  CIDADE-AUX               PIC X(19).
           05  LOTE-AUX                 PIC X(01).
           05  CONTA-CORR-AUX           PIC 9(8)   COMP-3.
           05  DV-CONTA-AUX             PIC X.
           05  BANCO-AUX                PIC 9(4)   COMP-3.
           05  AGENCIA-AUX              PIC 9(5)   COMP-3.
           05  DV-AGENCIA-AUX           PIC X.
           05  COMPENSACAO-AUX          PIC 9(3).
           05  CPF-AUX                  PIC 9(11)  COMP-3.
           05  NR-CHEQUE-AUX            PIC X(7).
           05  OUTRO-DOCTO-AUX          PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-AUX          PIC 9(8).
           05  PORTADOR-AUX             PIC 9999.
           05  CARTEIRA-AUX             PIC 9.
      *    CARTEIRA-CH10  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-AUX         PIC 99.
           05  SITUACAO-AUX             PIC 9.
      *    SITUACAO = 0-OK 1-PARCIAL 2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-AUX        PIC 9(5).
           05  VALOR-AUX                PIC 9(8)V99.
           05  VENDEDOR-AUX             PIC 9(6)   COMP-3.
           05  DIGITADOR-AUX            PIC X(5).
           05  SEQ-CAIXA-AUX            PIC 9(3).
           05  NR-NOTA-FISCAL-AUX       PIC X(10).
           05  DATA-NTA-FISCAL-AUX      PIC 9(8)   COMP-3.
           05  ORIGEM-AUX               PIC 99.
           05  VALOR-SALDO-AUX          PIC 9(08)V99.
           05  FILLER                   PIC X(100).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CHP055.CPB".
           COPY "CHP055.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CHD012             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-AUXILIAR           PIC XX       VALUE SPACES.
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
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO            PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-FIM        PIC 9(8)     VALUE ZEROS.
           05  VENDEDOR-ANT          PIC 9(6)     VALUE ZEROS.
           05  TOTAL-VENDEDOR        PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-GERAL           PIC 9(8)V99  VALUE ZEROS.
           05  MES-W                 PIC 9(6)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(55)   VALUE SPACES.
           05  FILLER              PIC X(50)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(55)    VALUE
           "RELATORIO DE CHEQUES PRE-DATADOS POR VENDEDOR".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999.
           05  FILLER              PIC X(18)    VALUE SPACES.
           05  HORA-REL            PIC X(5)     VALUE "  :  ".
           05  FILLER              PIC XX       VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(112)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(112)   VALUE
           "DATA-VECTO BANC NR-CHEQ   NOME                           ALB
      -    "BUM   PORTADOR        ORIGEM                VALOR".

       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(112)   VALUE SPACES.

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
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CHD010"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010
           MOVE "CGD001"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001
           MOVE "CAD018"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018
           MOVE "CHD012"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD012
           MOVE "RCD100"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100
           MOVE "RCD101"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS


           ACCEPT VARIA-W FROM TIME

           MOVE VARIA-W    TO PATH-AUXILIAR

           OPEN OUTPUT AUXILIAR
           CLOSE       AUXILIAR


           OPEN INPUT CHD010 CGD001 CHD012 CAD018 RCD100 RCD101.

           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD012 <> "00"
              MOVE "ERRO ABERTURA CHD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CHP055"            to logacess-programa
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
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-LISTA-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LE-VENDEDOR
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM POPUP-VENDEDOR
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
       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01   TO GS-DESCR-VENDEDOR.
       POPUP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-VENDEDOR
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-MOVTO-INI TO DATA-INV DATA-MOVTO-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-MOVTO.


           MOVE GS-MOVTO-FIM TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-MOVTO-FIM.

           INITIALIZE REG-CHD010

           MOVE ZEROS    TO VENDEDOR-ANT TOTAL-VENDEDOR TOTAL-GERAL.

           EVALUATE GS-OPCAO
               WHEN 1 PERFORM POR-MOVIMENTO
               WHEN 2 PERFORM POR-VENDA
           END-EVALUATE

           MOVE SPACES        TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "TOTAL GERAL . . ." TO GS-LINDET
           MOVE TOTAL-GERAL      TO VALOR-E
           MOVE VALOR-E          TO GS-LINDET(95: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       POR-MOVIMENTO SECTION.
           IF GS-OPCAO-VENDEDOR = 2
               MOVE GS-VENDEDOR TO VENDEDOR-CH10
           ELSE
               MOVE ZEROS       TO VENDEDOR-CH10.

           MOVE DATA-MOVTO TO DATA-MOVTO-CH10.
           MOVE ZEROS TO SEQ-CH10.
           START CHD010 KEY IS NOT < ALT-CH3 INVALID KEY
                 MOVE "10" TO ST-CHD010.

           PERFORM UNTIL ST-CHD010 = "10"
                 READ CHD010 NEXT RECORD AT END
                      PERFORM TOTALIZA-VENDEDOR
                      MOVE "10" TO ST-CHD010
                 NOT AT END
                      IF DATA-MOVTO-CH10 > DATA-MOVTO-FIM
                         PERFORM TOTALIZA-VENDEDOR
                         MOVE "10" TO ST-CHD010
                      ELSE
                         IF GS-OPCAO-VENDEDOR = 2 AND
                            GS-VENDEDOR = VENDEDOR-CH10 OR
                            GS-OPCAO-VENDEDOR = 1 AND
                            DATA-MOVTO-CH10 NOT > DATA-MOVTO-FIM
                            IF GS-LOTE = LOTE-CH10 OR GS-LOTE = SPACES
                               IF SITUACAO-CH10 = 0
                                  PERFORM MOVER-DADOS-LISTA
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

       POR-VENDA SECTION.
           OPEN I-O AUXILIAR

           INITIALIZE REG-RCD100

           MOVE DATA-MOVTO         TO DATAVEN-REC
           START RCD100 KEY IS NOT LESS DATAVEN-REC INVALID KEY
                MOVE "10" TO ST-RCD100.

           PERFORM UNTIL ST-RCD100 = "10"
                READ RCD100 NEXT AT END
                     MOVE "10" TO ST-RCD100
                NOT AT END
                     IF DATAVEN-REC > DATA-MOVTO-FIM
                        MOVE "10" TO ST-RCD100
                     ELSE
                        INITIALIZE REG-RCD101
                        MOVE ALBUM-REC TO ALBUM-REC1
                        START RCD101 KEY IS NOT LESS ALBUM-REC1
                                                             INVALID KEY
                             MOVE "10" TO ST-RCD101
                        END-START
                        PERFORM UNTIL ST-RCD101 = "10"
                             READ RCD101 NEXT AT END
                                  MOVE "10" TO ST-RCD101
                             NOT AT END
                                  IF ALBUM-REC <> ALBUM-REC1
                                     MOVE "10" TO ST-RCD101
                                  ELSE
                                     IF TIPO-REC1 = 1
                                        PERFORM PROCURAR-CHEQUE
                                     END-IF
                                  END-IF
                             END-READ
                        END-PERFORM
                     END-IF
                END-READ
           END-PERFORM

           CLOSE      AUXILIAR
           OPEN INPUT AUXILIAR

           INITIALIZE REG-AUX
           START AUXILIAR KEY IS NOT LESS ALT-AUX3 INVALID KEY
                MOVE "10" TO ST-AUXILIAR.

           PERFORM UNTIL ST-AUXILIAR = "10"
                READ AUXILIAR NEXT AT END
                     PERFORM TOTALIZA-VENDEDOR
                     MOVE "10" TO ST-AUXILIAR
                NOT AT END
                     MOVE REG-AUX TO REG-CHD010
                     PERFORM MOVER-DADOS-LISTA
                END-READ
           END-PERFORM

           CLOSE AUXILIAR.

       PROCURAR-CHEQUE SECTION.
           INITIALIZE REG-CHD010
           MOVE ALBUM-REC            TO CLIENTE-CH10

           START CHD010 KEY IS NOT LESS ALT-CH4 INVALID KEY
                MOVE "10" TO ST-CHD010.

           PERFORM UNTIL ST-CHD010 = "10"
                READ CHD010 NEXT AT END
                     MOVE "10" TO ST-CHD010
                NOT AT END
                     IF ALBUM-REC <> CLIENTE-CH10
                        MOVE "10" TO ST-CHD010
                     ELSE
                        IF VENCTO-REC1 = DATA-VENCTO-CH10
                           IF GS-OPCAO-VENDEDOR = 2 AND
                              GS-VENDEDOR = VENDEDOR-CH10 OR
                              GS-OPCAO-VENDEDOR = 1
                              IF GS-LOTE = LOTE-CH10 OR
                                 GS-LOTE = SPACES
                                 IF SITUACAO-CH10 = 0
                                    PERFORM MOVER-DADOS
                                 END-IF
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                END-READ
           END-PERFORM.

       MOVER-DADOS SECTION.
           MOVE REG-CHD010         TO REG-AUX
           WRITE REG-AUX.

       MOVER-DADOS-LISTA SECTION.
           IF VENDEDOR-CH10 <> VENDEDOR-ANT
              PERFORM CABECALHO-VENDEDOR.

           MOVE SPACES            TO GS-LINDET.
           MOVE DATA-VENCTO-CH10  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(1: 11)
           MOVE BANCO-CH10        TO GS-LINDET(12: 5)
           MOVE NR-CHEQUE-CH10    TO GS-LINDET(17: 8)
           MOVE NOME-CH10         TO GS-LINDET(25: 31)
           MOVE CLIENTE-CH10      TO GS-LINDET(56: 9)
           MOVE PORTADOR-CH10     TO GS-LINDET(65: 2) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES       TO NOME-PORT.
           MOVE "-"               TO GS-LINDET(67: 1)
           MOVE NOME-PORT         TO GS-LINDET(68: 12)
           MOVE ORIGEM-CH10       TO GS-LINDET(81: 2) CODIGO-CH12
           READ CHD012 INVALID KEY
                MOVE SPACES       TO DESCR-ORIGEM-CH12.
           MOVE "-"               TO GS-LINDET(83: 1)
           MOVE DESCR-ORIGEM-CH12 TO GS-LINDET(84: 11)
           MOVE VALOR-CH10        TO VALOR-E
           ADD VALOR-CH10         TO TOTAL-VENDEDOR TOTAL-GERAL.
           MOVE VALOR-E           TO GS-LINDET(95: 13)
           MOVE "INSERE-LIST"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CABECALHO-VENDEDOR SECTION.
           PERFORM TOTALIZA-VENDEDOR.
           MOVE SPACES            TO GS-LINDET.
           MOVE "INSERE-LIST"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE VENDEDOR-CH10     TO VENDEDOR-ANT CODIGO-CG01
                                     GS-LINDET(1: 6)
           MOVE "-"               TO GS-LINDET(7: 1)
           READ CGD001 INVALID KEY
                MOVE SPACES       TO NOME-CG01.
           MOVE NOME-CG01         TO GS-LINDET(8: 30).
           MOVE "INSERE-LIST"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES            TO GS-LINDET.
           MOVE "INSERE-LIST"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       TOTALIZA-VENDEDOR SECTION.
           MOVE SPACES            TO GS-LINDET.
           IF VENDEDOR-ANT <> ZEROS
              MOVE "TOTAL VENDEDOR . . . " TO GS-LINDET(1: 17)
              MOVE TOTAL-VENDEDOR TO VALOR-E
              MOVE VALOR-E        TO GS-LINDET(95: 13)
              MOVE "INSERE-LIST"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              MOVE ZEROS          TO TOTAL-VENDEDOR.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CHP055" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           COPY CONDENSA.


           MOVE ZEROS TO VENDEDOR-ANT TOTAL-VENDEDOR TOTAL-GERAL.
           MOVE ZEROS TO LIN PAG-W.

           PERFORM CABECALHO.

           EVALUATE GS-OPCAO
               WHEN 1 PERFORM POR-MOVIMENTO2
               WHEN 2 PERFORM POR-VENDA2
           END-EVALUATE

           PERFORM TOTALIZA-VENDEDOR-IMPR.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           MOVE "TOTAL GERAL . .  ." TO LINDET-REL.
           MOVE TOTAL-GERAL          TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(95: 13)
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 2 TO LIN.

           COPY DESCONDENSA.


       POR-MOVIMENTO2 SECTION.
           INITIALIZE REG-CHD010

           IF GS-OPCAO-VENDEDOR = 2
               MOVE GS-VENDEDOR TO VENDEDOR-CH10
           ELSE
               MOVE ZEROS       TO VENDEDOR-CH10.

           MOVE DATA-MOVTO TO DATA-MOVTO-CH10.

           MOVE ZEROS TO SEQ-CH10.
           START CHD010 KEY IS NOT < ALT-CH3  INVALID KEY
                 MOVE "10" TO ST-CHD010.
           PERFORM UNTIL ST-CHD010 = "10"
                 READ CHD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CHD010
                 NOT AT END
                    IF DATA-MOVTO-CH10 > DATA-MOVTO-FIM
                       MOVE "10" TO ST-CHD010
                    ELSE
                       IF GS-OPCAO-VENDEDOR = 2 AND
                          GS-VENDEDOR = VENDEDOR-CH10 OR
                          GS-OPCAO-VENDEDOR = 1 AND
                          DATA-MOVTO-CH10 NOT > DATA-MOVTO-FIM
                          IF GS-LOTE = LOTE-CH10 OR GS-LOTE = SPACES
                             IF SITUACAO-CH10 = 0
                                PERFORM MOVER-DADOS-REL
                             END-IF
                          END-IF
                      END-IF
                    END-IF
                 END-READ
           END-PERFORM.
       POR-MOVIMENTO2-FIM.
           EXIT.

       POR-VENDA2 SECTION.
           OPEN I-O AUXILIAR

           INITIALIZE REG-RCD100

           MOVE DATA-MOVTO         TO DATAVEN-REC
           START RCD100 KEY IS NOT LESS DATAVEN-REC INVALID KEY
                MOVE "10" TO ST-RCD100.

           PERFORM UNTIL ST-RCD100 = "10"
                READ RCD100 NEXT AT END
                     MOVE "10" TO ST-RCD100
                NOT AT END
                     IF DATAVEN-REC > DATA-MOVTO-FIM
                        MOVE "10" TO ST-RCD100
                     ELSE
                        INITIALIZE REG-RCD101
                        MOVE ALBUM-REC TO ALBUM-REC1
                        START RCD101 KEY IS NOT LESS ALBUM-REC1
                                                             INVALID KEY
                             MOVE "10" TO ST-RCD101
                        END-START
                        PERFORM UNTIL ST-RCD101 = "10"
                             READ RCD101 NEXT AT END
                                  MOVE "10" TO ST-RCD101
                             NOT AT END
                                  IF ALBUM-REC <> ALBUM-REC1
                                     MOVE "10" TO ST-RCD101
                                  ELSE
                                     IF TIPO-REC1 = 1
                                        PERFORM PROCURAR-CHEQUE
                                     END-IF
                                  END-IF
                             END-READ
                        END-PERFORM
                     END-IF
                END-READ
           END-PERFORM

           CLOSE      AUXILIAR
           OPEN INPUT AUXILIAR

           INITIALIZE REG-AUX
           START AUXILIAR KEY IS NOT LESS ALT-AUX3 INVALID KEY
                MOVE "10" TO ST-AUXILIAR.

           PERFORM UNTIL ST-AUXILIAR = "10"
                READ AUXILIAR NEXT AT END
                     MOVE "10" TO ST-AUXILIAR
                NOT AT END
                     MOVE REG-AUX TO REG-CHD010
                     PERFORM MOVER-DADOS-REL
                END-READ
           END-PERFORM

           CLOSE AUXILIAR.
       POR-VENDA2-FIM.
           EXIT.


       MOVER-DADOS-REL SECTION.
           IF VENDEDOR-CH10 <> VENDEDOR-ANT
              PERFORM CABECALHO-VENDEDOR-REL.
           MOVE SPACES            TO LINDET-REL.
           MOVE DATA-VENCTO-CH10  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(1: 11)
           MOVE BANCO-CH10        TO LINDET-REL(12: 5)
           MOVE NR-CHEQUE-CH10    TO LINDET-REL(17: 10)
           MOVE NOME-CH10         TO LINDET-REL(27: 31)
           MOVE CLIENTE-CH10      TO LINDET-REL(58: 9)
           MOVE PORTADOR-CH10     TO LINDET-REL(67: 2) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES       TO NOME-PORT.
           MOVE "-"               TO LINDET-REL(69: 1)
           MOVE NOME-PORT         TO LINDET-REL(70: 12)
           MOVE ORIGEM-CH10       TO LINDET-REL(83: 2) CODIGO-CH12
           READ CHD012 INVALID KEY
                MOVE SPACES       TO DESCR-ORIGEM-CH12.
           MOVE "-"               TO LINDET-REL(85: 1)
           MOVE DESCR-ORIGEM-CH12 TO LINDET-REL(86: 11)
           MOVE VALOR-CH10        TO VALOR-E
           ADD VALOR-CH10         TO TOTAL-VENDEDOR TOTAL-GERAL.
           MOVE VALOR-E           TO LINDET-REL(97: 13)
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.

       CABECALHO-VENDEDOR-REL SECTION.
           PERFORM TOTALIZA-VENDEDOR-IMPR.
           MOVE SPACES        TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE VENDEDOR-CH10 TO VENDEDOR-ANT CODIGO-CG01
                                 LINDET-REL(1: 6)
           MOVE "-"           TO LINDET-REL(7: 1)
           READ CGD001 INVALID KEY
                MOVE SPACES   TO NOME-CG01.
           MOVE NOME-CG01     TO LINDET-REL(8: 30).
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 58
              PERFORM CABECALHO.
           MOVE SPACES TO LINDET.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 58
              PERFORM CABECALHO.
       TOTALIZA-VENDEDOR-IMPR SECTION.
           MOVE SPACES TO LINDET-REL.
           IF VENDEDOR-ANT <> ZEROS
              MOVE "TOTAL VENDEDOR . . . " TO LINDET-REL(1: 17)
              MOVE TOTAL-VENDEDOR     TO VALOR-E
              MOVE VALOR-E            TO LINDET-REL(95: 13)
              WRITE REG-RELAT FROM LINDET
              ADD 1 TO LIN
              IF LIN > 56 PERFORM CABECALHO
              END-IF
              MOVE ZEROS              TO TOTAL-VENDEDOR
              MOVE SPACES             TO LINDET-REL.

       CABECALHO SECTION.
           ADD 1         TO LIN PAG-W.
           MOVE PAG-W    TO PAG-REL.
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
           move "CHP055"            to logacess-programa
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

           CLOSE CHD010 CGD001 CAD018 CHD012 RCD100 RCD101.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
