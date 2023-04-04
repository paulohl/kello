       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPP058.
      *DATA: 03/11/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Liberações de pagamentos-Contas a pagar
      *FUNÇÃO: Relatório de pagamentos liberados pela diretoria
      *        Serão listados apenas lançamentos a pagar e
      *        liberado = 1(sim)
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
           COPY CAPX018.
           COPY CAPX019.
           COPY LOGACESS.SEL.
      *    COPY CIEPX010.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = FORNEC-WK SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK = VENCTO-WK VALOR-WK
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-FORNEC-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PORTADOR-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NOME-FORN-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CAPW018.
       COPY CAPW019.
       COPY CPPW020.
       COPY LOGACESS.FD.
      *COPY CIEPW010.
       FD  WORK.
       01  REG-WORK.
           05  FORNEC-WK           PIC 9(6).
           05  SEQ-WK              PIC 9(5).
           05  NOME-FORN-WK        PIC X(20).
           05  PORTADOR-WK         PIC 9(4).
           05  TIPO-FORNEC-WK      PIC 9(2).
           05  VENCTO-WK           PIC 9(8).
           05  DESCRICAO-WK        PIC X(30).
           05  VALOR-WK            PIC 9(8)V99.
           05  MULTA-WK            PIC 9(6)V99.
           05  JUROS-WK            PIC 9(6)V99.
           05  PREV-DEF-WK         PIC 9.
           05  LIBERADO-WK         PIC 9.
           05  SUSPENSO-WK         PIC 9.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CPP058.CPB".
           COPY "CPP058.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD019             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
      *    05  ST-CIED010            PIC XX       VALUE SPACES.
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
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  NOME-FORN-ANT         PIC X(20)    VALUE SPACES.
           05  PORTADOR-ANT          PIC 9(4)     VALUE ZEROS.
           05  TIPO-FORN-ANT         PIC 9(2)     VALUE ZEROS.
           05  VENCTO-ANT            PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
           COPY "PARAMETR".
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
           05  FILLER              PIC X(35)   VALUE
           "RELACAO DE CONTAS A PAGAR - ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(21)   VALUE SPACES.
      *    05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
      *    05  VENCTO-INI-REL      PIC 99/99/9999.
      *    05  FILLER              PIC X(3)    VALUE ' a '.
      *    05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-MOVTO NOME-FORNECEDOR      DESCRICAO
      -    "   TP PO         VALOR      MULTA      JUROS T".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

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
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-DIA-INV.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CAD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD019.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CAD018 CAD019 CPD020 CGD001.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD019 <> "00"
              MOVE "ERRO ABERTURA CAD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CPP058"            to logacess-programa
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
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-VENCTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
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
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
      *VERIFICA-VENCTO-ANT SECTION.
      *    IF GS-VENCTO-INI NOT = VENCTO-INI-ANT
      *       OR GS-VENCTO-FIM NOT = VENCTO-FIM-ANT
      *          PERFORM GRAVA-WORK.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE GS-VENCTO-INI TO DATA-INV VENCTO-INI-ANT
      *                              VENCTO-INI-REL.
      *    CALL "GRIDAT2" USING DATA-INV.
      *    MOVE DATA-INV    TO VENCTO-INI.
      *    MOVE GS-VENCTO-FIM TO DATA-INV VENCTO-FIM-ANT
      *                              VENCTO-FIM-REL.
      *    CALL "GRIDAT2" USING DATA-INV.
      *    MOVE DATA-INV    TO VENCTO-FIM.
      *    MOVE VENCTO-INI TO DATA-VENCTO-CP20

           MOVE ZEROS       TO DATA-VENCTO-CP20.
           MOVE ZEROS TO SITUACAO-CP20 FORNEC-CP20
           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                  MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
             READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
              NOT AT END
               IF SITUACAO-CP20 > 0 MOVE "10" TO ST-CPD020
               ELSE
      *         IF DATA-VENCTO-CP20 < VENCTO-INI OR
      *            DATA-VENCTO-CP20 > VENCTO-FIM CONTINUE
      *         ELSE
                 IF LIBERADO-CP20 = 0 CONTINUE
                 ELSE
                   MOVE DATA-VENCTO-CP20    TO VENCTO-WK
                                               GS-EXIBE-VENCTO
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   MOVE FORNEC-CP20         TO FORNEC-WK CODIGO-CG01
                   READ CGD001 INVALID KEY MOVE "****" TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01           TO NOME-FORN-WK
                   MOVE SEQ-CP20            TO SEQ-WK
                   MOVE TIPO-FORN-CP20      TO TIPO-FORNEC-WK
                   MOVE PORTADOR-CP20       TO PORTADOR-WK
                   MOVE DESCRICAO-CP20      TO DESCRICAO-WK
                   MOVE VALOR-TOT-CP20      TO VALOR-WK
                   MOVE PREV-DEF-CP20       TO PREV-DEF-WK
                   MOVE LIBERADO-CP20       TO LIBERADO-WK
                   MOVE MULTA-ATRASO-CP20   TO MULTA-WK
                   MOVE JUROS-MORA-CP20     TO JUROS-WK
                   IF SITUACAO-CP20 = 1 MOVE 1 TO SUSPENSO-WK
                   ELSE MOVE 0 TO SUSPENSO-WK
                   END-IF
                   WRITE REG-WORK
               END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-TOTAL-A-PAGAR.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
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
                MOVE "FORNECEDOR" TO GS-DESCR-ORDEM
                MOVE SPACES TO NOME-FORN-WK
                START WORK KEY IS NOT < NOME-FORN-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "PORTADOR" TO GS-DESCR-ORDEM
                MOVE ZEROS TO PORTADOR-WK
                START WORK KEY IS NOT < PORTADOR-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "TIPO FORNEC" TO GS-DESCR-ORDEM
                MOVE ZEROS TO TIPO-FORNEC-WK
                START WORK KEY IS NOT < TIPO-FORNEC-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "VCTO/VALOR" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK VALOR-WK
                START WORK KEY IS NOT < ALT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF NOME-FORN-ANT NOT = ZEROS
                 IF NOME-FORN-ANT NOT = NOME-FORN-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF PORTADOR-ANT NOT = ZEROS
                 IF PORTADOR-ANT NOT = PORTADOR-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF TIPO-FORN-ANT NOT = ZEROS
                 IF TIPO-FORN-ANT NOT = TIPO-FORNEC-WK
                    PERFORM TOTALIZA
             WHEN 5
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(01: 11)
           MOVE NOME-FORN-WK      TO GS-LINDET(12: 21)
           MOVE DESCRICAO-WK      TO GS-LINDET(33: 31)
           MOVE TIPO-FORNEC-WK    TO GS-LINDET(62: 03)
           MOVE PORTADOR-WK       TO GS-LINDET(65: 04)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(70: 14)
           IF PREV-DEF-WK = ZEROS MOVE "D" TO GS-LINDET(83: 1)
           ELSE MOVE "P" TO GS-LINDET(83: 1).
           ADD VALOR-WK           TO GS-TOTAL-A-PAGAR
           MOVE MULTA-WK          TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(84: 11)
           MOVE JUROS-WK          TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(95: 11)
           MOVE LIBERADO-WK TO GS-LIBERACAO.
           MOVE FORNEC-WK         TO GS-LINDET(110: 06).
           MOVE SEQ-WK            TO GS-LINDET(117: 05).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ZERA-VARIAVEIS SECTION.
           MOVE SPACES TO NOME-FORN-ANT.
           MOVE ZEROS TO VENCTO-ANT PORTADOR-ANT
                         TIPO-FORN-ANT.
       MOVER-CHAVE-ANT SECTION.
           MOVE NOME-FORN-WK      TO NOME-FORN-ANT.
           MOVE TIPO-FORNEC-WK    TO TIPO-FORN-ANT.
           MOVE VENCTO-WK         TO VENCTO-ANT.
           MOVE PORTADOR-WK       TO PORTADOR-ANT.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-LINDET(110: 21).
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CPP058" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

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

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF NOME-FORN-ANT NOT = ZEROS
                 IF NOME-FORN-ANT NOT = NOME-FORN-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF PORTADOR-ANT NOT = ZEROS
                 IF PORTADOR-ANT NOT = PORTADOR-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF TIPO-FORN-ANT NOT = ZEROS
                 IF TIPO-FORN-ANT NOT = TIPO-FORNEC-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01: 11)
           MOVE NOME-FORN-WK      TO LINDET-REL(12: 21)
           MOVE DESCRICAO-WK      TO LINDET-REL(33: 31)
           MOVE TIPO-FORNEC-WK    TO LINDET-REL(62: 03)
           MOVE PORTADOR-WK       TO LINDET-REL(65: 04)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(70: 14)
           IF PREV-DEF-WK = ZEROS MOVE "D" TO LINDET-REL(83: 1)
           ELSE MOVE "P" TO LINDET-REL(83: 1).
           MOVE MULTA-WK          TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(84: 11)
           MOVE JUROS-WK          TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(95: 11)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
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
           move "CPP058"            to logacess-programa
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


           CLOSE CPD020 CAD018 CAD019 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
