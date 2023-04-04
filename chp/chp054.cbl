       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP054.
      *DATA: 04/06/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Fluxo de recebimento. Totaliza diário, mensal, acumulado,
      *        os CHEQUES a receber dentro do vencto e tipo de carteira
      *        solicitada.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX018.
           COPY CHPX010.
           COPY LOGACESS.SEL.

           SELECT WORK ASSIGN TO VARIA-W
                  ACCESS MODE IS DYNAMIC
                  ORGANIZATION IS INDEXED
                  STATUS IS ST-WORK
                  RECORD KEY IS DATA-WK.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW018.
       COPY CHPW010.
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
           COPY "CHP054.CPB".
           COPY "CHP054.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  TOTAL-MES             PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-ACUM            PIC 9(8)V99  VALUE ZEROS.
           05  VENCTO-ANT            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MES-ANT               PIC 9(6)     VALUE ZEROS.
           05  MES-W                 PIC 9(6)     VALUE ZEROS.
           05  ACHEI                 PIC X(1)     VALUE SPACES.
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
           05  FILLER              PIC X(58)    VALUE
           "FLUXO DE RECEBIMENTOS DE CHEQUES".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(05)    VALUE SPACES.
           05  FILLER              PIC X(75)    VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(05)    VALUE SPACES.
           05  FILLER              PIC X(25)    VALUE "VENCIMENTO".
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

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

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
           MOVE "CHD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CHD010 CAD018.

           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CHP054"            to logacess-programa
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
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LE-PORTADOR
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       INCLUIR SECTION.
           MOVE "Você Deseja Incluir o Portador?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE GS-PORTADOR TO GS-LINHA-PORTADOR(1:4)
              MOVE GS-DESCR-PORTADOR TO GS-LINHA-PORTADOR(6:30)
              MOVE "INSERIR-LINHA" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           MOVE SPACES TO MENSAGEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-PORTADOR SECTION.
           MOVE GS-PORTADOR TO PORTADOR.
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT TO GS-DESCR-PORTADOR.
       POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-PORTADOR
           MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR.
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

           MOVE GS-VENCTO-INI TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO VENCTO-INI.
           MOVE GS-VENCTO-FIM TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO VENCTO-FIM.
           IF GS-OPCAO-PORTADOR = 2
              MOVE GS-PORTADOR TO PORTADOR-CH10
           ELSE
              MOVE ZEROS TO PORTADOR-CH10.

           EVALUATE GS-TIPO-CH
               WHEN 1 MOVE ZEROS TO SITUACAO-CH10
               WHEN 2 MOVE 5     TO SITUACAO-CH10
               WHEN 3 MOVE 6     TO SITUACAO-CH10
           END-EVALUATE

           MOVE VENCTO-INI       TO DATA-VENCTO-CH10.
           START CHD010 KEY IS NOT < ALT-CH2 INVALID KEY
                 MOVE "10" TO ST-CHD010.
           PERFORM UNTIL ST-CHD010 = "10"
                 READ CHD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CHD010
                 NOT AT END
                      MOVE DATA-VENCTO-CH10 TO GS-EXIBE-FEEDBACK
                      MOVE "REFRESH-FEEDBACK" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF DATA-VENCTO-CH10 > VENCTO-FIM
                         MOVE "10" TO ST-CHD010
                      ELSE
                         EVALUATE GS-TIPO-CH
                             WHEN 1 IF SITUACAO-CH10 <> 0
                                       MOVE "10" TO ST-CHD010
                                    ELSE
                                       PERFORM CONTINUAR
                                    END-IF
                             WHEN 2 IF SITUACAO-CH10 <> 5
                                       MOVE "10" TO ST-CHD010
                                    ELSE
                                       PERFORM CONTINUAR
                                    END-IF
                             WHEN 3 IF SITUACAO-CH10 <> 6
                                       MOVE "10" TO ST-CHD010
                                    ELSE
                                       PERFORM CONTINUAR
                                    END-IF
                         END-EVALUATE
                      END-IF
                 END-READ
           END-PERFORM.

       CONTINUAR SECTION.
           IF GS-OPCAO-PORTADOR = 2
              PERFORM PROCURAR-PORTADOR
              IF ACHEI = "S"
                 EVALUATE CARTEIRA-CH10
                    WHEN 1 IF GS-SIMPLES = 1
                              PERFORM VERIFICA-DATAS
                    WHEN 2 IF GS-CAUCAO = 1
                              PERFORM VERIFICA-DATAS
                    WHEN 3 IF GS-DESCONTO = 1
                              PERFORM VERIFICA-DATAS
                 END-EVALUATE
              END-IF
           ELSE
              EVALUATE CARTEIRA-CH10
                 WHEN 1 IF GS-SIMPLES = 1
                           PERFORM VERIFICA-DATAS
                 WHEN 2 IF GS-CAUCAO = 1
                           PERFORM VERIFICA-DATAS
                 WHEN 3 IF GS-DESCONTO = 1
                           PERFORM VERIFICA-DATAS
              END-EVALUATE.

       PROCURAR-PORTADOR SECTION.
           MOVE 1 TO GS-CONT
           MOVE SPACES TO GS-LINHA-PORTADOR
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE "N" TO ACHEI
           PERFORM UNTIL GS-LINHA-PORTADOR = SPACES OR ACHEI = "S"
               IF GS-LINHA-PORTADOR(1:4) = PORTADOR-CH10
                  MOVE "S" TO ACHEI
               END-IF
               ADD 1 TO GS-CONT
               MOVE SPACES TO GS-LINHA-PORTADOR
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.
       VERIFICA-DATAS SECTION.
            MOVE DATA-VENCTO-CH10 TO DATA-WK.
            READ WORK INVALID KEY PERFORM GRAVA-WORK
                NOT INVALID KEY
                   PERFORM REGRAVA-WORK.
       GRAVA-WORK SECTION.
            MOVE VALOR-CH10   TO VALOR-WK.
            WRITE REG-WORK.
       REGRAVA-WORK SECTION.
            ADD VALOR-CH10    TO VALOR-WK.
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
           MOVE DATA-WK                  TO DATA-INV
           CALL "GRIDAT1"                USING DATA-INV
           MOVE DATA-INV                 TO DATA-E
           MOVE DATA-E                   TO GS-LINDET(01: 26).
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
           MOVE "CHP054" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           PERFORM ZERA-VARIAVEIS.

           COPY CONDENSA.

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

           COPY DESCONDENSA.


       MOVER-DADOS-REL SECTION.
           MOVE DATA-WK(01: 06) TO MES-W.
           IF MES-ANT NOT = ZEROS
              IF MES-W NOT = MES-ANT
                 MOVE ZEROS TO TOTAL-MES
                 MOVE SPACES TO REG-RELAT
                 WRITE REG-RELAT
                 ADD 1 TO LIN.
           MOVE MES-W TO MES-ANT.
           MOVE DATA-WK    TO DATA-INV
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
           move "CHP054"            to logacess-programa
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

           CLOSE CHD010 CAD018 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

