       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP210.
      *DATA: 08/06/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RESUMO DE PRODUÇAO DE FOTOGRAFIA
      *FUNÇÃO: Listar todos as análises que estiverem dentro de interva-
      *        lo de data solicitado
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY LBPX103.
           COPY LBPX104.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS COD-FUNCIONARIO-WK
                  ALTERNATE RECORD KEY IS
                      FUNCIONARIO-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY LBPW103.
       COPY LBPW104.
       FD  WORK.
       01  REG-WORK.
           05  COD-FUNCIONARIO-WK   PIC 9(6).
           05  FUNCIONARIO-WK       PIC X(30).
           05  PRODUZIDAS-WK        PIC 9(8).
           05  PERDIDAS-WK          PIC 9(8).
           05  MIN-TEMPO-DECOR-WK   PIC 9(8).
           05  MIN-TEMPO-INTERV-WK  PIC 9(8).
           05  MIN-TEMPO-INTERR-WK  PIC 9(8).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP210.CPB".
           COPY "LBP210.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-LBD103             PIC XX       VALUE SPACES.
           05  ST-LBD104             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
      *    TOTALIZA VARIAVEIS
           05  TOTAL-PRODUZIDO       PIC 9(8)     VALUE ZEROS.
           05  TOTAL-PERDIDO         PIC 9(8)     VALUE ZEROS.
           05  TOTAL-TEMPO-DECOR     PIC 9(8)     VALUE ZEROS.
           05  TOTAL-INTERVALO       PIC 9(8)     VALUE ZEROS.
           05  TOTAL-INTERRUPCAO     PIC 9(8)     VALUE ZEROS.

           05  QTDE-E                PIC ZZ.ZZZ.ZZZ.
           05  PERC-E                PIC ZZZ,ZZ.

      *    VARIAVEIS P/ CALCULO DE HORAS
           05  MINUTOS-TEMPO-DECOR   PIC 9(6)     VALUE ZEROS.
           05  MINUTOS-INTERVALO     PIC 9(6)     VALUE ZEROS.
           05  MINUTOS-INTERRUPCAO   PIC 9(6)     VALUE ZEROS.
           05  TOT-HORA              PIC 9(6)     VALUE ZEROS.
           05  TOT-MIN               PIC 9(6)     VALUE ZEROS.
           05  TEMPOE.
               10  HOR-E             PIC ZZZZZZ.
               10  PONT-E            PIC X VALUE ":".
               10  MIN-E             PIC 99.
           05  TEMPO-E REDEFINES TEMPOE PIC X(9).

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.



       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(45)   VALUE
           "RESUMO DE PRODUCAO DE FUNCIONARIO".
           05  FILLER              PIC X(12)   VALUE "DIAS UTEIS: ".
           05  DIAS-UTEIS-REL      PIC ZZZZ    BLANK WHEN ZEROS.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "INT.DATA MOVTO: ".
           05  DATA-INI-REL        PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  DATA-FIM-REL        PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "TIPO FOTOS                      PRODUZIDO   %-PRO    PERDAS
      -    "  %-PER      TEMPO   %-TEM     INTERRUP   %-INT".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "LBD103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD103.
           MOVE "LBD104"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD104.
           OPEN INPUT CGD001 LBD103 LBD104.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD103 <> "00"
              MOVE "ERRO ABERTURA LBD103: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD103 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD104 <> "00"
              MOVE "ERRO ABERTURA LBD104: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD104 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
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
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-DATA-INI    TO DATA-INV DATA-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-INI.
           MOVE GS-DATA-FIM    TO DATA-INV DATA-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-FIM.
           MOVE ZEROS TO TOTAL-PRODUZIDO TOTAL-PERDIDO
                         MINUTOS-TEMPO-DECOR MINUTOS-INTERVALO
                         MINUTOS-INTERRUPCAO TOTAL-TEMPO-DECOR
                         TOTAL-INTERVALO TOTAL-INTERRUPCAO.
           PERFORM GRAVA-DIAS-UTEIS.
           PERFORM GRAVA-PRODUZIDAS
           PERFORM GRAVA-PERDIDAS.

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       GRAVA-DIAS-UTEIS SECTION.
           MOVE DATA-INI        TO GRTIME-DATE
           MOVE DATA-FIM        TO GRTIME-DATE-FINAL
           MOVE 3               TO GRTIME-FUNCTION.
           MOVE 2               TO GRTIME-TYPE
           CALL "GRTIME" USING PARAMETROS-GRTIME
           COMPUTE GS-DIAS-UTEIS ROUNDED = (GRTIME-DAYS-FINAL / 7) * 5.

       GRAVA-PRODUZIDAS SECTION.
           MOVE ZEROS          TO SEQ-L103.
           MOVE DATA-INI       TO DATA-MOVTO-L103.
           START LBD103 KEY IS NOT < CHAVE-L103 INVALID KEY
                  MOVE "10" TO ST-LBD103.
           PERFORM UNTIL ST-LBD103 = "10"
             READ LBD103 NEXT RECORD AT END MOVE "10" TO ST-LBD103
              NOT AT END
               IF DATA-MOVTO-L103 > DATA-FIM
                  MOVE "10" TO ST-LBD103
               ELSE
                MOVE DATA-MOVTO-L103    TO GS-EXIBE-VENCTO
                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                MOVE FUNCIONARIO-L103    TO COD-FUNCIONARIO-WK
                PERFORM CALCULA-HORAS
                READ WORK INVALID KEY
                     INITIALIZE REG-WORK
                     MOVE FUNCIONARIO-L103  TO COD-FUNCIONARIO-WK
                                               CODIGO-CG01
                     PERFORM LER-FUNCIONARIO
                     MOVE NOME-CG01       TO FUNCIONARIO-WK
                     MOVE QTDE-FOTOS-L103 TO PRODUZIDAS-WK
                     MOVE MINUTOS-TEMPO-DECOR TO MIN-TEMPO-DECOR-WK
                     MOVE MINUTOS-INTERVALO   TO MIN-TEMPO-INTERV-WK
                     MOVE MINUTOS-INTERRUPCAO TO MIN-TEMPO-INTERR-WK
                     ADD QTDE-FOTOS-L103      TO TOTAL-PRODUZIDO
                     ADD MINUTOS-TEMPO-DECOR  TO TOTAL-TEMPO-DECOR
                     ADD MINUTOS-INTERVALO    TO TOTAL-INTERVALO
                     ADD MINUTOS-INTERRUPCAO  TO TOTAL-INTERRUPCAO
                     WRITE REG-WORK
                     END-WRITE
                   NOT INVALID KEY
                     ADD QTDE-FOTOS-L103  TO PRODUZIDAS-WK
                     ADD QTDE-FOTOS-L103  TO TOTAL-PRODUZIDO
                     ADD MINUTOS-TEMPO-DECOR TO TOTAL-TEMPO-DECOR
                                                MIN-TEMPO-DECOR-WK
                     ADD MINUTOS-INTERVALO   TO TOTAL-INTERVALO
                                                MIN-TEMPO-INTERV-WK
                     ADD MINUTOS-INTERRUPCAO TO TOTAL-INTERRUPCAO
                                                MIN-TEMPO-INTERR-WK
                     REWRITE REG-WORK
                     END-REWRITE
                END-READ
             END-READ
           END-PERFORM.

       CALCULA-HORAS SECTION.

           MOVE ZEROS TO PARAMETROS-GRTIME
           MOVE HORA-INIC-L103     TO GRTIME-TIME(1: 4)
           IF HORA-INIC-L103 > HORA-FIM-L103
              ADD 2400 TO HORA-FIM-L103
           END-IF
           MOVE HORA-FIM-L103      TO GRTIME-TIME-FINAL(1: 4)
           MOVE 3 TO GRTIME-FUNCTION
           CALL "GRTIME" USING PARAMETROS-GRTIME
           COMPUTE MINUTOS-TEMPO-DECOR =
              (GRTIME-TOTAL-HOURS * 60) + GRTIME-TOTAL-MINUTES
           MOVE TEMPO-INTERVALO-L103(1: 2) TO TOT-HORA
           MOVE TEMPO-INTERVALO-L103(3: 2) TO TOT-MIN
           COMPUTE MINUTOS-INTERVALO = (TOT-HORA * 60) + TOT-MIN

           MOVE TEMPO-INTERRUPC-L103(1: 2) TO TOT-HORA
           MOVE TEMPO-INTERRUPC-L103(3: 2) TO TOT-MIN
           COMPUTE MINUTOS-INTERRUPCAO = (TOT-HORA * 60) + TOT-MIN.


       GRAVA-PERDIDAS SECTION.
           MOVE ZEROS          TO SEQ-L104.
           MOVE DATA-INI       TO DATA-MOVTO-L104.
           START LBD104 KEY IS NOT < CHAVE-L104 INVALID KEY
                  MOVE "10" TO ST-LBD104.
           PERFORM UNTIL ST-LBD104 = "10"
             READ LBD104 NEXT RECORD AT END MOVE "10" TO ST-LBD104
              NOT AT END
               IF DATA-MOVTO-L104 > DATA-FIM
                  MOVE "10" TO ST-LBD104
               ELSE
                MOVE DATA-MOVTO-L104    TO GS-EXIBE-VENCTO
                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                MOVE FUNCIONARIO-L104    TO COD-FUNCIONARIO-WK
                READ WORK INVALID KEY
                     INITIALIZE REG-WORK
                     MOVE FUNCIONARIO-L104  TO COD-FUNCIONARIO-WK
                                              CODIGO-CG01
                     PERFORM LER-FUNCIONARIO
                     MOVE NOME-CG01       TO FUNCIONARIO-WK
                     MOVE FOTOS-PROB-L104 TO PERDIDAS-WK
                     ADD FOTOS-PROB-L104  TO TOTAL-PERDIDO
                     WRITE REG-WORK
                     END-WRITE
                   NOT INVALID KEY
                     ADD FOTOS-PROB-L104  TO PERDIDAS-WK
                     ADD FOTOS-PROB-L104  TO TOTAL-PERDIDO
                     REWRITE REG-WORK
                     END-REWRITE
                END-READ
             END-READ
           END-PERFORM.
       LER-FUNCIONARIO SECTION.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE SPACES TO FUNCIONARIO-WK.
           START WORK KEY IS NOT < FUNCIONARIO-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           MOVE SPACES            TO GS-LINDET
           MOVE "INSERE-LIST"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "TOTAL GERAL: "  TO GS-LINDET(1: 30)

           MOVE TOTAL-PRODUZIDO  TO QTDE-E
           MOVE QTDE-E           TO GS-LINDET(32: 12)
           COMPUTE PERC-E = (TOTAL-PRODUZIDO / TOTAL-PRODUZIDO) * 100
           MOVE PERC-E           TO GS-LINDET(44: 6)
           MOVE TOTAL-PERDIDO    TO QTDE-E
           MOVE QTDE-E           TO GS-LINDET(50: 11)
           COMPUTE PERC-E = (TOTAL-PERDIDO / TOTAL-PRODUZIDO) * 100
           MOVE PERC-E           TO GS-LINDET(63: 7)
           COMPUTE TOT-HORA = (TOTAL-TEMPO-DECOR - TOTAL-INTERVALO)
                               / 60
           COMPUTE TOT-MIN = (TOTAL-TEMPO-DECOR - TOTAL-INTERVALO)
                             - (TOT-HORA * 60)
           MOVE TOT-HORA TO HOR-E
           MOVE TOT-MIN TO MIN-E
           MOVE TEMPO-E          TO GS-LINDET(70: 10)
           COMPUTE PERC-E = (TOTAL-TEMPO-DECOR / TOTAL-TEMPO-DECOR)
                            * 100
           MOVE PERC-E           TO GS-LINDET(81: 7)

           COMPUTE TOT-HORA = TOTAL-INTERRUPCAO / 60
           COMPUTE TOT-MIN = TOTAL-INTERRUPCAO - (TOT-HORA * 60)
           MOVE TOT-HORA         TO HOR-E
           MOVE TOT-MIN          TO MIN-E
           MOVE TEMPO-E          TO GS-LINDET(91: 10)
           COMPUTE PERC-E = (TOTAL-INTERRUPCAO / TOTAL-INTERRUPCAO)
                            * 100
           MOVE PERC-E           TO GS-LINDET(102: 7).

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-LINDET SECTION.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE FUNCIONARIO-WK   TO GS-LINDET(1: 31)
           MOVE PRODUZIDAS-WK    TO QTDE-E
           MOVE QTDE-E           TO GS-LINDET(32: 12)
           COMPUTE PERC-E = (PRODUZIDAS-WK / TOTAL-PRODUZIDO) * 100
           MOVE PERC-E           TO GS-LINDET(44: 6)
           MOVE PERDIDAS-WK      TO QTDE-E
           MOVE QTDE-E           TO GS-LINDET(50: 11)
           COMPUTE PERC-E = (PERDIDAS-WK / TOTAL-PRODUZIDO) * 100
           MOVE PERC-E           TO GS-LINDET(63: 7)
           COMPUTE TOT-HORA = (MIN-TEMPO-DECOR-WK - MIN-TEMPO-INTERV-WK)
                               / 60
           COMPUTE TOT-MIN = (MIN-TEMPO-DECOR-WK - MIN-TEMPO-INTERV-WK)
                             - (TOT-HORA * 60)
           MOVE TOT-HORA TO HOR-E
           MOVE TOT-MIN TO MIN-E
           MOVE TEMPO-E          TO GS-LINDET(70: 10)
           COMPUTE PERC-E = (MIN-TEMPO-DECOR-WK / TOTAL-TEMPO-DECOR)
                            * 100
           MOVE PERC-E           TO GS-LINDET(81: 7)

           COMPUTE TOT-HORA = MIN-TEMPO-INTERR-WK / 60
           COMPUTE TOT-MIN = MIN-TEMPO-INTERR-WK - (TOT-HORA * 60)
           MOVE TOT-HORA         TO HOR-E
           MOVE TOT-MIN          TO MIN-E
           MOVE TEMPO-E          TO GS-LINDET(91: 10)
           COMPUTE PERC-E = (MIN-TEMPO-INTERR-WK / TOTAL-INTERRUPCAO)
                            * 100
           MOVE PERC-E           TO GS-LINDET(102: 7).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP210" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE SPACES TO FUNCIONARIO-WK
           START WORK KEY IS NOT < FUNCIONARIO-WK INVALID KEY
                 MOVE "10"  TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                      PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           MOVE SPACES            TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           MOVE "TOTAL GERAL: "   TO LINDET-REL(1: 30)

           MOVE TOTAL-PRODUZIDO  TO QTDE-E
           MOVE QTDE-E           TO LINDET-REL(32: 12)
           COMPUTE PERC-E = (TOTAL-PRODUZIDO / TOTAL-PRODUZIDO) * 100
           MOVE PERC-E           TO LINDET-REL(44: 6)
           MOVE TOTAL-PERDIDO    TO QTDE-E
           MOVE QTDE-E           TO LINDET-REL(50: 11)
           COMPUTE PERC-E = (TOTAL-PERDIDO / TOTAL-PRODUZIDO) * 100
           MOVE PERC-E           TO LINDET-REL(63: 7)
           COMPUTE TOT-HORA = (TOTAL-TEMPO-DECOR - TOTAL-INTERVALO)
                               / 60
           COMPUTE TOT-MIN = (TOTAL-TEMPO-DECOR - TOTAL-INTERVALO)
                             - (TOT-HORA * 60)
           MOVE TOT-HORA TO HOR-E
           MOVE TOT-MIN TO MIN-E
           MOVE TEMPO-E          TO LINDET-REL(70: 10)
           COMPUTE PERC-E = (TOTAL-TEMPO-DECOR / TOTAL-TEMPO-DECOR)
                            * 100
           MOVE PERC-E           TO LINDET-REL(81: 7)

           COMPUTE TOT-HORA = TOTAL-INTERRUPCAO / 60
           COMPUTE TOT-MIN = TOTAL-INTERRUPCAO - (TOT-HORA * 60)
           MOVE TOT-HORA         TO HOR-E
           MOVE TOT-MIN          TO MIN-E
           MOVE TEMPO-E          TO LINDET-REL(91: 10)
           COMPUTE PERC-E = (TOTAL-INTERRUPCAO / TOTAL-INTERRUPCAO)
                            * 100
           MOVE PERC-E           TO LINDET-REL(102: 7).

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           PERFORM MOVER-DADOS
           MOVE GS-LINDET TO LINDET-REL

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
           MOVE GS-DIAS-UTEIS   TO DIAS-UTEIS-REL
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
           CLOSE CGD001 LBD103 LBD104.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
