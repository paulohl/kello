       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPP053.
      *DATA: 23/07/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *RELATÓRIO: Boletim de Pagamento
      *FUNÇÃO: Listar todos os títulos que foram pagos no intervalo
      *        de movimento solicitado. Lista apenas os títulos com
      *        situação = 2(pago)
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CPPX020.
           COPY CXPX100.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS DATA-MOVTO-WK
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FORNEC-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-PAGTO-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CPPW020.
       COPY CXPW100.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(5).
           05  FORNEC-WK           PIC X(30).
           05  PORTADOR-WK         PIC 9(4).
           05  TIPO-FORNEC-WK      PIC 9(2).
           05  DATA-VENCTO-WK      PIC 9(8).
           05  DATA-PAGTO-WK       PIC 9(8).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  VALOR-WK            PIC 9(8)V99.
           05  VALOR-PAGTO-WK      PIC 9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CPP053.CPB".
           COPY "CPP053.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
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
           05  MOVTO-INI             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  TOTAL-VALOR           PIC 9(12)V99 VALUE ZEROS.
           05  TOTAL-PAGO            PIC 9(12)V99 VALUE ZEROS.
           COPY "PARAMETR".

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
           "BOLETIM DE PAGAMENTOS     - ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.PAGTO.: ".
           05  MOVTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MOVTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-MOVTO N.NR. NOME-FORNECEDOR                TP PO
      -    " VALOR         DATA-VECTO DATA-PAGTO    VALOR-PAGO".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

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

       01  linha-total.
           05 filler                   pic x(48)
              value "Totalizador . . .".
           05 det-valor                pic zz.zzz.zzz,zz
              blank when zeros.
           05 filler                   pic x(31).
           05 det-vlrpago              pic zz.zzz.zzz,zz
              blank when zeros.

       01  linha-total2.
           05 filler                   pic x(53)
              value "Totalizador . . .".
           05 det-valor2               pic zz.zzz.zzz,zz
              blank when zeros.
           05 filler                   pic x(31).
           05 det-vlrpago2             pic zz.zzz.zzz,zz
              blank when zeros.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CPP053-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CPP053-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CPP053-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CPP053-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           MOVE "LOGACESS"  TO ARQ-REC. MOVE EMPRESA-REF TO
           ARQUIVO-LOGACESS

           OPEN INPUT CPD020 CGD001 CXD100.

           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CPP053-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CPP053-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO CPP053-MENSAGEM-ERRO
              MOVE ST-CPD020 TO CPP053-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO CPP053-MENSAGEM-ERRO
              MOVE ST-CXD100 TO CPP053-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CPP053-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CPP053"            to logacess-programa
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
               WHEN CPP053-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CPP053-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CPP053-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN CPP053-GRAVA-WORK-TRUE
                    PERFORM GRAVA-WORK
      *             PERFORM ZERA-VARIAVEIS
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
           INITIALIZE CPP053-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INVERTE-DATA SECTION.
           MOVE CPP053-MOVTO-INI TO DATA-INV MOVTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO MOVTO-INI.
           MOVE CPP053-MOVTO-FIM TO DATA-INV MOVTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO MOVTO-FIM.
       GRAVA-WORK SECTION.
           PERFORM INVERTE-DATA.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME.

           OPEN OUTPUT WORK.
           CLOSE       WORK.
           OPEN I-O    WORK.

           INITIALIZE TOTAL-VALOR
                      TOTAL-PAGO

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 0              TO SEQ-WK.
      *    MOVE 2              TO SITUACAO-CP20.
      *    MOVE MOVTO-INI      TO DATA-MOVTO-CP20.
      *    START CPD020 KEY IS NOT < ALT3-CP20 INVALID KEY
      *          MOVE "10" TO ST-CPD020.
      *    PERFORM UNTIL ST-CPD020 = "10"
      *       READ CPD020 NEXT RECORD AT END
      *            MOVE "10" TO ST-CPD020
      *       NOT AT END
      *            IF DATA-MOVTO-CP20 > MOVTO-FIM OR
      *               SITUACAO-CP20 NOT = 2
      *               MOVE "10" TO ST-CPD020
      *            ELSE
      *               PERFORM MOVER-DADOS-WORK
      *               MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
      *               PERFORM CALL-DIALOG-SYSTEM
      *               WRITE REG-WORK
      *            END-IF
      *      END-READ
      *    END-PERFORM.


           INITIALIZE REG-CPD020
           MOVE MOVTO-INI      TO DATA-PGTO-CP20
           START CPD020 KEY IS NOT < ALT6-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
              READ CPD020 NEXT RECORD AT END
                   MOVE "10" TO ST-CPD020
              NOT AT END
                   IF DATA-PGTO-CP20 > MOVTO-FIM
                      MOVE "10" TO ST-CPD020
                   ELSE
                      PERFORM MOVER-DADOS-WORK
                      MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      WRITE REG-WORK
                   END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           ADD 1 TO SEQ-WK.
           MOVE DATA-MOVTO-CP20     TO DATA-VENCTO-WK
           MOVE DATA-MOVTO-CP20  TO DATA-MOVTO-WK
           MOVE FORNEC-CP20       TO CODIGO-CG01
           READ CGD001 INVALID KEY MOVE "****" TO NOME-CG01
           END-READ
           MOVE NOME-CG01         TO FORNEC-WK.
           MOVE PORTADOR-CP20     TO PORTADOR-WK.
           MOVE TIPO-FORN-CP20    TO TIPO-FORNEC-WK.
           MOVE VALOR-TOT-CP20    TO VALOR-WK.
           MOVE DATA-VENCTO-CP20  TO DATA-VENCTO-WK.
           MOVE DATA-PGTO-CP20    TO DATA-PAGTO-WK
                                     CPP053-EXIBE-VENCTO
           MOVE VALOR-LIQ-CP20    TO VALOR-PAGTO-WK

           ADD VALOR-TOT-CP20     TO TOTAL-VALOR
           ADD VALOR-LIQ-CP20     TO TOTAL-PAGO.
       ORDEM SECTION.
           EVALUATE CPP053-ORDEM
             WHEN 1
                MOVE "DATA-MOVTO" TO CPP053-DESCR-ORDEM
                MOVE ZEROS TO DATA-MOVTO-WK
                START WORK KEY IS NOT < DATA-MOVTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "VENCTO" TO CPP053-DESCR-ORDEM
                MOVE ZEROS TO DATA-VENCTO-WK
                START WORK KEY IS NOT < DATA-VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "FORNECEDOR" TO CPP053-DESCR-ORDEM
                MOVE SPACES TO FORNEC-WK
                START WORK KEY IS NOT < FORNEC-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "DATA-PAGTO" TO CPP053-DESCR-ORDEM
                MOVE ZEROS TO DATA-PAGTO-WK
                START WORK KEY IS NOT < DATA-PAGTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM INVERTE-DATA.
           MOVE SPACES TO CPP053-LINDET.
           PERFORM ORDEM.
           MOVE CPP053-DESCR-ORDEM TO ORDEM-REL.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  MOVE DATA-MOVTO-WK    TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV          TO DATA-E
                  MOVE DATA-E            TO CPP053-LINDET(01: 11)
                  MOVE FORNEC-WK         TO CPP053-LINDET(12: 29)
                  MOVE PORTADOR-WK       TO CPP053-LINDET(41: 05)
                  MOVE TIPO-FORNEC-WK    TO CPP053-LINDET(46: 03)
                  MOVE VALOR-WK          TO VALOR-E
                  MOVE VALOR-E           TO CPP053-LINDET(49: 14)
                  MOVE DATA-VENCTO-WK    TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV          TO DATA-E
                  MOVE DATA-E            TO CPP053-LINDET(71: 11)
                  MOVE DATA-PAGTO-WK     TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV          TO DATA-E
                  MOVE DATA-E            TO CPP053-LINDET(82: 11)
                  MOVE VALOR-PAGTO-WK    TO VALOR-E
                  MOVE VALOR-E           TO CPP053-LINDET(93: 13)
                  MOVE "INSERE-LIST" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM

           MOVE TOTAL-VALOR    TO DET-VALOR    DET-VALOR2
           MOVE TOTAL-PAGO     TO DET-VLRPAGO  DET-VLRPAGO2
           MOVE LINHA-TOTAL    TO CPP053-LINHA-TOTAL
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CLEAR-FLAGS SECTION.
           INITIALIZE CPP053-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CPP053" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM ORDEM.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  MOVE DATA-MOVTO-WK     TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV          TO DATA-E
                  MOVE DATA-E            TO LINDET-REL(01: 11)
                  MOVE FORNEC-WK         TO LINDET-REL(12: 29)
                  MOVE PORTADOR-WK       TO LINDET-REL(47: 05)
                  MOVE TIPO-FORNEC-WK    TO LINDET-REL(52: 03)
                  MOVE VALOR-WK          TO VALOR-E
                  MOVE VALOR-E           TO LINDET-REL(54: 14)
                  MOVE DATA-VENCTO-WK    TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV          TO DATA-E
                  MOVE DATA-E            TO LINDET-REL(76: 11)
                  MOVE DATA-PAGTO-WK      TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV          TO DATA-E
                  MOVE DATA-E            TO LINDET-REL(87: 11)
                  MOVE VALOR-PAGTO-WK    TO VALOR-E
                  MOVE VALOR-E           TO LINDET-REL(98: 13)
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
              END-READ
           END-PERFORM.
           WRITE REG-RELAT FROM LINHA-TOTAL2
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           COPY DESCONDENSA.


       CABECALHO SECTION.
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
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CPP053-DATA-BLOCK.
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
           move "CPP053"            to logacess-programa
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

           CLOSE CPD020 CGD001 WORK CXD100.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

