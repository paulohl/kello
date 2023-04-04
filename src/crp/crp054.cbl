       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP054.
      *DATA: 09/04/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Fluxo de recebimento. Totaliza diário, mensal, acumulado,
      *        os títulos a receber dentro do vencto e tipo de carteira
      *        solicitada.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CRPX020.
           COPY CRPX020B.
           COPY CBPX001.
           COPY LOGACESS.SEL.

           SELECT WORK ASSIGN TO VARIA-W
                  ACCESS MODE IS DYNAMIC
                  ORGANIZATION IS INDEXED
                  STATUS IS ST-WORK
                  RECORD KEY IS DATA-WK.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CRPW020.
       COPY CRPW020B.
       COPY CBPW001.
       COPY LOGACESS.FD.

       FD  WORK.
       01  REG-WORK.
           05  DATA-WK             PIC 9(8).
           05  VALOR-WK            PIC 9(8)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP054.CPB".
           COPY "CRP054.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  AUX-TIPO              PIC 9(01)    VALUE ZEROS.
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
           05  TOTAL-MES             PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-ACUM            PIC 9(8)V99  VALUE ZEROS.
           05  VENCTO-ANT            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MES-ANT               PIC 9(6)     VALUE ZEROS.
           05  MES-W                 PIC 9(6)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(05)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(55)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(05)    VALUE SPACES.
           05  FILLER              PIC X(25)    VALUE
           "FLUXO DE RECEBIMENTOS".
           05  TIPO-DOCUMENTO-REL  PIC X(33).
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB02B.
           05  FILLER              PIC X(16)
               VALUE "FORMA DE PAGTO: ".
           05  FORMA-PGTO-REL      PIC X(20).
           05  FILLER              PIC X(16)
               VALUE "CONTA CORRENTE: ".
           05  CONTA-REL           PIC 9(06).
           05  FILLER              PIC X(01).
           05  NOME-CONTA-REL      PIC X(30).
       01  CAB03.
           05  FILLER              PIC X(05)    VALUE SPACES.
           05  FILLER              PIC X(75)    VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(05)    VALUE SPACES.
           05  DET-DESCRICAO       PIC X(25).
           05  FILLER              PIC X(18)    VALUE " TOTAL DO DIA".
           05  FILLER              PIC X(19)    VALUE " TOTAL DO MES".
           05  FILLER              PIC X(13)    VALUE " TOTAL ACUMUL".
       01  LINDET.
           05  FILLER              PIC X(5)     VALUE SPACES.
           05  DATA-REL            PIC X(25)    VALUE SPACES.
           05  TOTAL-DIA-REL       PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(05)    VALUE SPACES.
           05  TOTAL-MES-REL       PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(06)    VALUE SPACES.
           05  TOTAL-ACUM-REL      PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.

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

       01 PASSAR-STRING.
          05 PASSAR-STRING1            PIC X(65).

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
           MOVE "CRD020"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CBD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CBD001
           MOVE "CRD020B"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020B.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CRD020 CRD020B CBD001.

           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: " TO GS-MENSAGEM-ERRO
              MOVE ST-CBD001 TO GS-MENSAGEM-ERRO(23:2)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020B <> "00"
              MOVE "ERRO ABERTURA CRD020B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP054"            to logacess-programa
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
                    PERFORM ZERA-VARIAVEIS
                    PERFORM INICIO-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-CONTA-TRUE
                    PERFORM LER-CONTA
               WHEN GS-POPUP-CONTA-TRUE
                    PERFORM POPUP-CONTA
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       LER-CONTA SECTION.
           IF GS-ACP-CONTA > 0
              MOVE GS-ACP-CONTA TO CODIGO-FORN-CB01
              READ CBD001 INVALID KEY
                   MOVE "Conta corrente inválida" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM 140-EXIBIR-MENSAGEM
              NOT INVALID KEY
                   MOVE TITULAR-CB01 TO GS-DESC-CONTA
                   REFRESH-OBJECT PRINCIPAL
              END-READ
           ELSE
              MOVE SPACES TO GS-DESC-CONTA
              REFRESH-OBJECT PRINCIPAL.

       POPUP-CONTA SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING
           CANCEL "CBP001T"
           MOVE PASSAR-STRING(49:6) TO GS-ACP-CONTA
           PERFORM LER-CONTA.

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

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO TOTAL-MES TOTAL-ACUM VENCTO-ANT MES-ANT.
       INICIO-WORK SECTION.
           IF ST-WORK = "35"
              ACCEPT VARIA-W FROM TIME
              OPEN OUTPUT WORK
              CLOSE       WORK
              OPEN I-O    WORK
           ELSE
              CLOSE       WORK
              OPEN OUTPUT WORK
              CLOSE       WORK
              OPEN I-O    WORK.

           INITIALIZE REG-CRD020

           IF GS-OPCAO = 1
              MOVE GS-VENCTO-INI TO DATA-INV
              CALL "GRIDAT2" USING  DATA-INV
              MOVE DATA-INV      TO VENCTO-INI
              MOVE GS-VENCTO-FIM TO DATA-INV
              CALL "GRIDAT2" USING  DATA-INV
              MOVE DATA-INV      TO VENCTO-FIM
              MOVE ZEROS TO DATA-VENCTO-CR20
                            SITUACAO-CR20
                            CLIENTE-CR20
                            CLASS-CLIENTE-CR20
              START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                    MOVE "10" TO ST-CRD020
              END-START
              PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                     MOVE "10" TO ST-CRD020
                 NOT AT END
                     MOVE DATA-VENCTO-CR20 TO GS-EXIBE-FEEDBACK
                     MOVE "REFRESH-FEEDBACK" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     IF SITUACAO-CR20 > 1
                        CONTINUE
                     ELSE
                        MOVE GS-TIPO-DOCUMENTO(1:1) TO AUX-TIPO
                        IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                           EVALUATE CARTEIRA-CR20
                              WHEN 1 IF GS-SIMPLES = 1
                                        PERFORM VERIFICA-DATAS
                              WHEN 2 IF GS-CAUCAO = 1
                                        PERFORM VERIFICA-DATAS
                              WHEN 3 IF GS-DESCONTO = 1
                                        PERFORM VERIFICA-DATAS
                          END-EVALUATE
                        END-IF
                     END-IF
                 END-READ
              END-PERFORM
           ELSE
              MOVE ZEROS TO DATA-RCTO-CR20B
                            SEQ-CAIXA-CR20B
              START CRD020B KEY IS NOT < ALT6-CR20B INVALID KEY
                    MOVE "10" TO ST-CRD020B
              END-START
              PERFORM UNTIL ST-CRD020B = "10"
                 READ CRD020B NEXT RECORD AT END
                     MOVE "10" TO ST-CRD020B
                 NOT AT END
                     IF GS-FORMA-PAGTO-D(1:1) = "9" OR
                        GS-FORMA-PAGTO-D = FORMA-PAGTO-CR20B
                        IF GS-ACP-CONTA = 0 OR GS-ACP-CONTA =
                           CONTA-CORRENTE-CR20B
                           MOVE DATA-RCTO-CR20B    TO GS-EXIBE-FEEDBACK
                           MOVE "REFRESH-FEEDBACK" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                           MOVE CLASS-CLIENTE-CR20B  TO
                                CLASS-CLIENTE-CR20
                           MOVE CLIENTE-CR20B        TO CLIENTE-CR20
                           MOVE SEQ-CR20B            TO SEQ-CR20
                           READ CRD020 NOT INVALID KEY
                              MOVE GS-TIPO-DOCUMENTO(1:1) TO AUX-TIPO
                              IF AUX-TIPO = 9 OR AUX-TIPO =
                                 TIPO-DOCTO-CR20
                                 EVALUATE CARTEIRA-CR20
                                    WHEN 1 IF GS-SIMPLES = 1
                                              PERFORM VERIFICA-DATAS
                                    WHEN 2 IF GS-CAUCAO = 1
                                              PERFORM VERIFICA-DATAS
                                    WHEN 3 IF GS-DESCONTO = 1
                                              PERFORM VERIFICA-DATAS
                                END-EVALUATE
                              END-IF
                           END-READ
                        END-IF
                     END-IF
                 END-READ
              END-PERFORM.
       VERIFICA-DATAS SECTION.
           IF GS-OPCAO = 1
              IF DATA-VENCTO-CR20 < VENCTO-INI
                 MOVE 11111111 TO DATA-WK
              ELSE
                 IF DATA-VENCTO-CR20 > VENCTO-FIM
                    MOVE 99999999 TO DATA-WK
                 ELSE
                    MOVE DATA-VENCTO-CR20 TO DATA-WK
                 END-IF
              END-IF
            ELSE
              IF DATA-RCTO-CR20B < VENCTO-INI
                 MOVE 11111111 TO DATA-WK
              ELSE
                 IF DATA-RCTO-CR20B > VENCTO-FIM
                    MOVE 99999999 TO DATA-WK
                 ELSE
                    MOVE DATA-RCTO-CR20B TO DATA-WK
                 END-IF
              END-IF
            END-IF

            READ WORK INVALID KEY
                 PERFORM GRAVA-WORK
            NOT INVALID KEY
                 PERFORM REGRAVA-WORK.
       GRAVA-WORK SECTION.
            MOVE VALOR-SALDO-CR20   TO VALOR-WK.
            WRITE REG-WORK.
       REGRAVA-WORK SECTION.
            ADD VALOR-SALDO-CR20    TO VALOR-WK.
            REWRITE REG-WORK.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO DATA-WK.
           START WORK KEY IS NOT < DATA-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                   PERFORM MOVER-DADOS
              END-READ
           END-PERFORM.
       MOVER-DADOS SECTION.
           MOVE DATA-WK(01: 06) TO MES-W.
           IF MES-ANT NOT = ZEROS
              IF MES-W NOT = MES-ANT
                 MOVE ZEROS TO TOTAL-MES
                 MOVE SPACES TO GS-LINDET
                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
           MOVE MES-W TO MES-ANT.
           IF DATA-WK = 11111111
              MOVE "ANTES DO PERÍODO"    TO GS-LINDET(01: 26)
           ELSE IF DATA-WK = 99999999
                   MOVE "APÓS O PERÍODO" TO GS-LINDET(01: 26)
                ELSE MOVE DATA-WK    TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV TO DATA-E
                     MOVE DATA-E         TO GS-LINDET(01: 26).
           ADD VALOR-WK                  TO TOTAL-MES TOTAL-ACUM
           MOVE VALOR-WK                 TO VALOR-E.
           MOVE VALOR-E                  TO GS-LINDET(27: 13).
           MOVE TOTAL-MES                TO VALOR-E.
           MOVE VALOR-E                  TO GS-LINDET(46: 13).
           MOVE TOTAL-ACUM               TO VALOR-E.
           MOVE VALOR-E                  TO GS-LINDET(65: 13).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP054" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           IF GS-OPCAO = 1
              MOVE "VENCIMENTO"   TO DET-DESCRICAO
           ELSE
              MOVE "BAIXA"        TO DET-DESCRICAO
           END-IF.
           MOVE GS-TIPO-DOCUMENTO TO TIPO-DOCUMENTO-REL
           MOVE GS-FORMA-PAGTO-D  TO FORMA-PGTO-REL
           MOVE GS-ACP-CONTA      TO CONTA-REL
           MOVE GS-DESC-CONTA     TO NOME-CONTA-REL
           PERFORM ZERA-VARIAVEIS.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO LIN PAG-W. PERFORM CABECALHO.
           MOVE ZEROS TO DATA-WK.
           START WORK KEY IS NOT < DATA-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                   PERFORM MOVER-DADOS-REL
              END-READ
           END-PERFORM.

           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.
       MOVER-DADOS-REL SECTION.
           MOVE DATA-WK(01: 06) TO MES-W.
           IF MES-ANT NOT = ZEROS
              IF MES-W NOT = MES-ANT
                 MOVE ZEROS TO TOTAL-MES
                 MOVE SPACES TO REG-RELAT
                 WRITE REG-RELAT
                 ADD 1 TO LIN.
           MOVE MES-W TO MES-ANT.
           IF DATA-WK = 11111111
              MOVE "ANTES DO PERIODO"    TO DATA-REL
           ELSE IF DATA-WK = 99999999
                   MOVE "APOS O PERIODO" TO DATA-REL
                ELSE MOVE DATA-WK    TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV TO DATA-E
                     MOVE DATA-E         TO DATA-REL.
           ADD VALOR-WK                  TO TOTAL-MES TOTAL-ACUM
           MOVE VALOR-WK                 TO TOTAL-DIA-REL.
           MOVE TOTAL-MES                TO TOTAL-MES-REL.
           MOVE TOTAL-ACUM               TO TOTAL-ACUM-REL.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PAG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB02B.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 7 TO LIN.

       140-EXIBIR-MENSAGEM SECTION.
           MOVE SPACES TO RESP-MSG
           CALL   "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL "MENSAGEM"
           MOVE SPACES TO MENSAGEM.

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
           move "CRP054"            to logacess-programa
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

           CLOSE CRD020 CRD020B WORK CBD001.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
