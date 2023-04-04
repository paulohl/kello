       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPP054.
      *DATA: 28/07/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Fluxo de pagamentos. Totaliza diário, mensal, acumulado,
      *        os títulos a pagar ou suspensos conforme solicitação.
      *        Tem a opção previsto/definitivo.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CPPX020.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ACCESS MODE IS DYNAMIC
                  ORGANIZATION IS INDEXED
                  STATUS IS ST-WORK
                  RECORD KEY IS DATA-WK.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CPPW020.
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
           COPY "CPP054.CPB".
           COPY "CPP054.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CPD020             PIC XX       VALUE SPACES.
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
           COPY "PARAMETR".
           05  PREV-DEF              PIC 9        VALUE ZEROS.
      * prev-def -> verifica a opcao do usuario. Caso = 2 a opção foi
      * para os títulos previsto e definivo.
           05  PAGAR-SUSP            PIC 9        VALUE ZEROS.
      * pagar-susp -> verifica a opção do usuário. Caso = 2 a opção foi
      * para os títulos a pagar e suspensos.

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
           "FLUXO DE PAGAMENTOS ".
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


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CPP054-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CPP054-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CPP054-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CPP054-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CPD020.

           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO CPP054-MENSAGEM-ERRO
              MOVE ST-CPD020 TO CPP054-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CPP054-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CPP054"            to logacess-programa
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


           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CPP054-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CPP054-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CPP054-CARREGA-LISTA-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM INICIO-WORK
                    PERFORM CARREGA-LISTA
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
           INITIALIZE CPP054-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO TOTAL-MES TOTAL-ACUM VENCTO-ANT
                         MES-ANT PREV-DEF PAGAR-SUSP.
       INICIO-WORK SECTION.
           IF ST-WORK = "35"
              ACCEPT VARIA-W FROM TIME
              OPEN OUTPUT WORK  CLOSE WORK   OPEN I-O WORK
           ELSE CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
           MOVE CPP054-VENCTO-INI TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO VENCTO-INI.
           MOVE CPP054-VENCTO-FIM TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO VENCTO-FIM.
           MOVE ZEROS TO DATA-VENCTO-CP20 SITUACAO-CP20 FORNEC-CP20.
           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           IF CPP054-A-PAGAR = 1 AND CPP054-SUSPENSA = 1
              MOVE 2 TO PAGAR-SUSP.
           IF CPP054-PREVISTO = 1 AND CPP054-DEFINITIVO = 1
              MOVE 2 TO PREV-DEF.
           IF CPP054-PREVISTO = 0 AND CPP054-DEFINITIVO = 0
              MOVE "10" TO ST-CPD020.
           IF CPP054-A-PAGAR = 0 AND CPP054-SUSPENSA = 0
              MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
              READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
               NOT AT END
                   IF PAGAR-SUSP = 2
                      IF PREV-DEF = 2 PERFORM VERIFICA-DATAS
                      ELSE IF CPP054-PREVISTO = 1
                              IF PREV-DEF-CP20 = 1
                                 PERFORM VERIFICA-DATAS
                              ELSE CONTINUE
                           ELSE
                             IF PREV-DEF-CP20 = 1 PERFORM VERIFICA-DATAS
                             ELSE CONTINUE
                   ELSE IF CPP054-A-PAGAR = 1
                           IF SITUACAO-CP20 = 0
                              IF PREV-DEF = 2 PERFORM VERIFICA-DATAS
                              ELSE
                               IF CPP054-PREVISTO = 1
                                  IF PREV-DEF-CP20 = 1
                                     PERFORM VERIFICA-DATAS
                                  ELSE CONTINUE
                               ELSE IF PREV-DEF-CP20 = 0
                                       PERFORM VERIFICA-DATAS
                                    ELSE CONTINUE
                           ELSE CONTINUE
                        ELSE
                           IF SITUACAO-CP20 = 1
                              IF PREV-DEF = 2 PERFORM VERIFICA-DATAS
                              ELSE
                               IF CPP054-PREVISTO = 1
                                  IF PREV-DEF-CP20 = 1
                                     PERFORM VERIFICA-DATAS
                                  ELSE CONTINUE
                               ELSE IF PREV-DEF-CP20 = 0
                                       PERFORM VERIFICA-DATAS
                                    ELSE CONTINUE
                           ELSE CONTINUE
              END-READ
           END-PERFORM.
       VERIFICA-DATAS SECTION.
           IF DATA-VENCTO-CP20 < VENCTO-INI
              MOVE 11111111 TO DATA-WK
           ELSE
            IF DATA-VENCTO-CP20 > VENCTO-FIM
               MOVE 99999999 TO DATA-WK
            ELSE
              MOVE DATA-VENCTO-CP20 TO DATA-WK.
            READ WORK INVALID KEY PERFORM GRAVA-WORK
                NOT INVALID KEY
                   PERFORM REGRAVA-WORK.
       GRAVA-WORK SECTION.
            MOVE VALOR-TOT-CP20   TO VALOR-WK.
            WRITE REG-WORK.
       REGRAVA-WORK SECTION.
            ADD VALOR-TOT-CP20    TO VALOR-WK.
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
                 MOVE SPACES TO CPP054-LINDET
                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
           MOVE MES-W TO MES-ANT.
           IF DATA-WK = 11111111
              MOVE "ANTES DO PERÍODO"    TO CPP054-LINDET(01: 26)
           ELSE IF DATA-WK = 99999999
                   MOVE "APÓS O PERÍODO" TO CPP054-LINDET(01: 26)
                ELSE MOVE DATA-WK    TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV TO DATA-E
                     MOVE DATA-E         TO CPP054-LINDET(01: 26).
           ADD VALOR-WK                  TO TOTAL-MES TOTAL-ACUM
           MOVE VALOR-WK                 TO VALOR-E.
           MOVE VALOR-E                  TO CPP054-LINDET(27: 13).
           MOVE TOTAL-MES                TO VALOR-E.
           MOVE VALOR-E                  TO CPP054-LINDET(46: 13).
           MOVE TOTAL-ACUM               TO VALOR-E.
           MOVE VALOR-E                  TO CPP054-LINDET(65: 13).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CPP054-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CPP054" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           PERFORM ZERA-VARIAVEIS.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO LIN PAG-W. PERFORM CABECALHO.
           MOVE SPACES TO LINDET.
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
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CPP054-DATA-BLOCK.
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
           move "CPP054"            to logacess-programa
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
           CLOSE CPD020 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

