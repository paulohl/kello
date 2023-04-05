       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPP055.
      *DATA: 18/08/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Relatório de Aprovação - Contas a pagar
      *FUNÇÃO: Listar todos os títulos que estiverem dentro do intervalo
      *        de vencimento. As ordens serão: Vencto, Portador, Forne-
      *        cedor e tipo-fornecedor, valor.
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
                  ALTERNATE RECORD KEY IS NOME-FORN-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK = LIBERADO-WK
                            VENCTO-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CAPW018.
       COPY CAPW019.
       COPY CPPW020.
      *COPY CIEPW010.
       FD  WORK.
       01  REG-WORK.
           05  FORNEC-WK           PIC 9(6).
           05  SEQ-WK              PIC 9(5).
           05  NOME-FORN-WK        PIC X(20).
           05  PORTADOR-WK         PIC 9(2).
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
           COPY "CPP055.CPB".
           COPY "CPP055.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD019             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
      *    05  ST-CIED010            PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  NOME-FORN-ANT         PIC X(20)    VALUE SPACES.
           05  PORTADOR-ANT          PIC 9(2)     VALUE ZEROS.
           05  TIPO-FORN-ANT         PIC 9(2)     VALUE ZEROS.
           05  VENCTO-ANT            PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4).
           05  LIN                   PIC 9(2)     VALUE ZEROS.
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
           05  FILLER              PIC X(35)   VALUE
           "RELACAO DE CONTAS A PAGAR - ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(21)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-MOVTO NOME-FORNECEDOR      DESCRICAO
      -    "   TP PO         VALOR      MULTA      JUROS T".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CPP055-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-DIA-INV.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CPP055-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CPP055-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CPP055-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CAD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD019.
      *    MOVE "CIED010" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CIED010.
           OPEN INPUT CAD018 CAD019 CPD020 CGD001.
      *    OPEN I-O CIED010.
      *    IF ST-CIED010 = "35"
      *       CLOSE CIED010   OPEN OUTPUT CIED010   CLOSE CIED010
      *       OPEN I-O CIED010.
      *    IF ST-CIED010 <> "00"
      *       MOVE "ERRO ABERTURA CIED010: "  TO CPP055-MENSAGEM-ERRO
      *       MOVE ST-CIED010 TO CPP055-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CPP055-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CPP055-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO CPP055-MENSAGEM-ERRO
              MOVE ST-CAD018 TO CPP055-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO CPP055-MENSAGEM-ERRO
              MOVE ST-CPD020 TO CPP055-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD019 <> "00"
              MOVE "ERRO ABERTURA CAD019: "  TO CPP055-MENSAGEM-ERRO
              MOVE ST-CAD019 TO CPP055-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CPP055-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CPP055-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CPP055-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CPP055-GRAVA-WORK-FLG-TRUE
                    PERFORM VERIFICA-VENCTO-ANT
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN CPP055-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN CPP055-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN CPP055-LIBERACAO-TRUE
                    PERFORM ATUALIZA-LIBERACAO
               WHEN CPP055-ACHAR-VENCTO-TRUE
                    PERFORM ACHAR-VENCTO
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
       ACHAR-VENCTO SECTION.
           MOVE DATA-DIA-INV   TO GRTIME-DATE.
           MOVE 2              TO GRTIME-TYPE.
           MOVE 1              TO GRTIME-FUNCTION.
           MOVE 7              TO GRTIME-DAYS.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           MOVE 01010001       TO CPP055-VENCTO-INI.
           CALL "GRIDAT1" USING GRTIME-DATE-FINAL.
           MOVE GRTIME-DATE-FINAL TO CPP055-VENCTO-FIM.


       LIMPAR-DADOS SECTION.
           INITIALIZE CPP055-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       ATUALIZA-LIBERACAO SECTION.
      * ****************
           MOVE CPP055-SENHA TO SENHA-WW.
           IF SENHA-WW = SENHA-W
              IF USUARIO-W = "VANDE"
                 PERFORM ATUALIZA-LIBERACAO-CPD020
              ELSE MOVE "TELA-ATUALIZA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           ELSE MOVE "TELA-ATUALIZA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
      *ACHA-SEQ-CIE SECTION.
      *    MOVE DATA-DIA-INV    TO DATA-CI10.
      *    MOVE ZEROS           TO SEQ-CI10.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      READ CIED010 NEXT RECORD AT END MOVE "10" TO ST-CIED010
      *       NOT AT END
      *         IF DATA-CI10 NOT = DATA-DIA-INV MOVE "10" TO ST-CIED010
      *         ELSE CONTINUE
      *      END-READ
      *    END-PERFORM.
      *GRAVA-CIE SECTION.
      *    PERFORM ACHA-SEQ-CIE.
      *    MOVE DATA-DIA-INV   TO DATA-CI10
      *    ADD 1               TO SEQ-CI10
      *    ACCEPT HORA-W       FROM TIME.
      *    MOVE HORA-W(1: 4)   TO HORA-CI10
      *    MOVE USUARIO-W      TO ORIGEM-CI10
      *                        TO COD-FUNCAO-CI10
      ** Função que exerce o destinatario

      *    MOVE 7              TO FUNCAO-DESTINO-CI10.
      *    MOVE 24             TO COD-MENS-PADRAO-CI10.
      *    MOVE "EMITIR RELATORIO DE APROVACAO DO CTAS A PAGAR"
      *                        TO DESCRICAO-MENS-CI10.
      *    MOVE ZEROS          TO ST-CIED010.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      WRITE REG-CIED010 INVALID KEY
      *             ADD 1 TO SEQ-CI10
      *         NOT INVALID KEY MOVE "10" TO ST-CIED010
      *    END-PERFORM.
       ATUALIZA-LIBERACAO-CPD020 SECTION.
      *    PERFORM GRAVA-CIE.

           CLOSE CPD020.  OPEN I-O CPD020
           MOVE ZEROS TO FORNEC-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                 MOVE FORNEC-WK  TO FORNEC-CP20
                 MOVE SEQ-WK     TO SEQ-CP20
                 READ CPD020 INVALID KEY CONTINUE
                   NOT INVALID KEY
                     MOVE LIBERADO-WK  TO LIBERADO-CP20
                     REWRITE REG-CPD020
                     END-REWRITE
                 END-READ
             END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           MOVE CPP055-LINDET(110: 6) TO FORNEC-WK.
           MOVE CPP055-LINDET(117: 5) TO SEQ-WK.
           READ WORK INVALID KEY CONTINUE
            NOT INVALID KEY
             IF LIBERADO-WK = 0
                MOVE 1       TO LIBERADO-WK
                MOVE "SIM"   TO CPP055-LINDET(106: 03)
                ADD VALOR-WK TO CPP055-TOTAL-LIBERADO
                REWRITE REG-WORK
             ELSE MOVE 0          TO LIBERADO-WK
                MOVE "NÃO"        TO CPP055-LINDET(106: 03)
                SUBTRACT VALOR-WK FROM CPP055-TOTAL-LIBERADO
                REWRITE REG-WORK.
       VERIFICA-VENCTO-ANT SECTION.
           IF CPP055-VENCTO-INI NOT = VENCTO-INI-ANT
              OR CPP055-VENCTO-FIM NOT = VENCTO-FIM-ANT
                 PERFORM GRAVA-WORK.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE CPP055-VENCTO-INI TO DATA-INV VENCTO-INI-ANT
                                     VENCTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VENCTO-INI.
           MOVE CPP055-VENCTO-FIM TO DATA-INV VENCTO-FIM-ANT
                                     VENCTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VENCTO-FIM.
           MOVE VENCTO-INI TO DATA-VENCTO-CP20
           MOVE ZEROS TO SITUACAO-CP20 FORNEC-CP20
           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                  MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
             READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
              NOT AT END

               IF SITUACAO-CP20 > 1 MOVE "10" TO ST-CPD020
               ELSE
                IF SITUACAO-CP20 = 1 AND DATA-VENCTO-CP20 > VENCTO-FIM
                      MOVE "10" TO ST-CPD020
                ELSE
                IF DATA-VENCTO-CP20 < VENCTO-INI OR
                   DATA-VENCTO-CP20 > VENCTO-FIM CONTINUE
                ELSE
                 IF TIPO-FORN-CP20 = 3 AND PREV-DEF-CP20 = 1 CONTINUE
                 ELSE
                   MOVE DATA-VENCTO-CP20    TO VENCTO-WK
                                               CPP055-EXIBE-VENCTO
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

       VERIFICA-PREV-DEF SECTION.
           IF CPP055-PREVISTO-TRUE
              IF CPP055-DEFINITIVO-TRUE
                 MOVE 2 TO PREV-DEF-W
              ELSE MOVE 1 TO PREV-DEF-W
           ELSE IF CPP055-DEFINITIVO-TRUE
                   MOVE 0 TO PREV-DEF-W
                ELSE MOVE 3 TO PREV-DEF-W.
      * PREV-DEF = 2 OS 2 TIPOS ESTAO SELECIONADOS
      * PREV-DEF = 3 NENHUM DOS TIPOS ESTAO SELECIONADOS
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO CPP055-LINDET.
           MOVE ZEROS TO CPP055-TOTAL-A-PAGAR CPP055-TOTAL-LIBERADO.
           PERFORM ORDEM.
           PERFORM VERIFICA-PREV-DEF.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF CPP055-SUSPENSO-TRUE AND SUSPENSO-WK = 1
                   IF PREV-DEF-W = 3 OR 2 PERFORM MOVER-DADOS-LINDET
                   ELSE IF PREV-DEF-W = 0 AND PREV-DEF-WK = 0
                           PERFORM MOVER-DADOS-LINDET
                        ELSE
                          IF PREV-DEF-W = 1 AND PREV-DEF-WK = 1
                             PERFORM MOVER-DADOS-LINDET
                          END-IF
                        END-IF
                   END-IF
                ELSE
                   IF SUSPENSO-WK = 1 OR PREV-DEF-W = 3 CONTINUE
                   ELSE
                    IF PREV-DEF-W = 2 PERFORM MOVER-DADOS-LINDET
                    ELSE
                        IF PREV-DEF-W = 0 AND PREV-DEF-WK = 0
                           PERFORM MOVER-DADOS-LINDET
                        ELSE
                          IF PREV-DEF-W = 1 AND PREV-DEF-WK = 1
                             PERFORM MOVER-DADOS-LINDET
                          END-IF
                        END-IF
                    END-IF
                   END-IF
                END-IF
              END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE CPP055-ORDEM
             WHEN 1
                MOVE "VENCTO" TO CPP055-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK
                START WORK KEY IS NOT < VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "FORNECEDOR" TO CPP055-DESCR-ORDEM
                MOVE SPACES TO NOME-FORN-WK
                START WORK KEY IS NOT < NOME-FORN-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "PORTADOR" TO CPP055-DESCR-ORDEM
                MOVE ZEROS TO PORTADOR-WK
                START WORK KEY IS NOT < PORTADOR-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "TIPO FORNEC" TO CPP055-DESCR-ORDEM
                MOVE ZEROS TO TIPO-FORNEC-WK
                START WORK KEY IS NOT < TIPO-FORNEC-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "VCTO/VALOR" TO CPP055-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK VALOR-WK
                START WORK KEY IS NOT < ALT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "LIB/VCTO" TO CPP055-DESCR-ORDEM
                MOVE ZEROS TO LIBERADO-WK VENCTO-WK
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE CPP055-ORDEM
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
             WHEN 6
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO CPP055-LINDET(01: 11)
           MOVE NOME-FORN-WK      TO CPP055-LINDET(12: 21)
           MOVE DESCRICAO-WK      TO CPP055-LINDET(33: 31)
           MOVE TIPO-FORNEC-WK    TO CPP055-LINDET(64: 03)
           MOVE PORTADOR-WK       TO CPP055-LINDET(67: 03)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO CPP055-LINDET(70: 14)
           IF PREV-DEF-WK = ZEROS MOVE "D" TO CPP055-LINDET(83: 1)
           ELSE MOVE "P" TO CPP055-LINDET(83: 1).
           ADD VALOR-WK           TO CPP055-TOTAL-A-PAGAR
           MOVE MULTA-WK          TO VALOR-E1
           MOVE VALOR-E1          TO CPP055-LINDET(84: 11)
           MOVE JUROS-WK          TO VALOR-E1
           MOVE VALOR-E1          TO CPP055-LINDET(95: 11)
           IF LIBERADO-WK = 0
              MOVE "NÃO"          TO CPP055-LINDET(106: 03)
           ELSE MOVE "SIM"        TO CPP055-LINDET(106: 03)
                ADD VALOR-WK      TO CPP055-TOTAL-LIBERADO.
           MOVE LIBERADO-WK TO CPP055-LIBERACAO.
           MOVE FORNEC-WK         TO CPP055-LINDET(110: 06).
           MOVE SEQ-WK            TO CPP055-LINDET(117: 05).
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
           MOVE SPACES TO CPP055-LINDET.
           MOVE ZEROS TO CPP055-LINDET(110: 21).
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CPP055-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CPP055" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF CPP055-SUSPENSO-TRUE AND SUSPENSO-WK = 1
                   IF PREV-DEF-W = 3 OR 2 PERFORM MOVER-DADOS-RELATORIO
                   ELSE IF PREV-DEF-W = 0 AND PREV-DEF-WK = 0
                           PERFORM MOVER-DADOS-RELATORIO
                        ELSE
                          IF PREV-DEF-W = 1 AND PREV-DEF-WK = 1
                             PERFORM MOVER-DADOS-LINDET
                          END-IF
                        END-IF
                   END-IF
                ELSE
                   IF SUSPENSO-WK = 1 OR PREV-DEF-W = 3 CONTINUE
                   ELSE
                    IF PREV-DEF-W = 2 PERFORM MOVER-DADOS-RELATORIO
                    ELSE
                        IF PREV-DEF-W = 0 AND PREV-DEF-WK = 0
                           PERFORM MOVER-DADOS-RELATORIO
                        ELSE
                          IF PREV-DEF-W = 1 AND PREV-DEF-WK = 1
                             PERFORM MOVER-DADOS-RELATORIO
                          END-IF
                        END-IF
                    END-IF
                   END-IF
                END-IF
              END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE CPP055-ORDEM
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
             WHEN 6
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
           MOVE TIPO-FORNEC-WK    TO LINDET-REL(64: 03)
           MOVE PORTADOR-WK       TO LINDET-REL(67: 03)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(70: 14)
           IF PREV-DEF-WK = ZEROS MOVE "D" TO LINDET-REL(83: 1)
           ELSE MOVE "P" TO LINDET-REL(83: 1).
           MOVE MULTA-WK          TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(84: 11)
           MOVE JUROS-WK          TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(95: 11)
           IF LIBERADO-WK = 0
              MOVE "NAO"          TO LINDET-REL(106: 3)
           ELSE MOVE "SIM"        TO LINDET-REL(106: 3).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           MOVE CPP055-DESCR-ORDEM TO ORDEM-REL.
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
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CPP055-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CPD020 CAD018 CAD019 WORK.
      *    CLOSE CIED010.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

