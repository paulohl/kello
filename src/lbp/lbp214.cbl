       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP214.
      *DATA: 31/05/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RESUMO DE ATIVIDADES GERAIS - POR DATA E TIPO OPERAÇÃO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY LBPX028.
           COPY LBPX105.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS COD-FUNCIONARIO-WK
                  ALTERNATE RECORD KEY IS FUNCIONARIO-WK
                                   WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY LBPW028.
       COPY LBPW105.
       FD  WORK.
       01  REG-WORK.
           05  COD-FUNCIONARIO-WK  PIC 9(6).
           05  FUNCIONARIO-WK      PIC X(30).
           05  QT-OPERACAO-WK      PIC 9(8).
           05  QT-ALBUM-WK         PIC 9(8).
           05  MIN-TEMPO-DECOR-WK  PIC 9(8).
      *    TEMPO DECORRIDO = (HORA-FIM - HORA-INI) - TEMPO-INTERVALO
           05  MIN-TEMPO-INTERRUP-WK  PIC 9(8).
      *    TEMPO INTERRUPÇÃO = HORAS E MINUTOS DE INTERRUPÇÃO
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP214.CPB".
           COPY "LBP214.CPY".
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
           05  ST-LBD028             PIC XX       VALUE SPACES.
           05  ST-LBD105             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-INI-ANT          PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM-ANT          PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
      *    OPCAO INDIVIDUAL
           05  GRAVAR-OPCAO          PIC 9        VALUE ZEROS.
           05  CODIGO-W              PIC 9(3)     VALUE ZEROS.

      *    TOTALIZA VARIAVEIS
           05  TOTAL-OPERACAO-G      PIC 9(8)     VALUE ZEROS.
           05  TOTAL-ALBUM-G         PIC 9(8)     VALUE ZEROS.
           05  TOTAL-MIN-TEMPO-G     PIC 9(8)     VALUE ZEROS.
           05  TOTAL-MIN-INTERRUP-G  PIC 9(8)     VALUE ZEROS.
           05  QTDE-E                PIC ZZ.ZZZ.ZZZ.
           05  MEDIA-E               PIC ZZZ.ZZZ,ZZ.
      *    VARIAVEIS P/ CALCULO DE HORAS
           05  TOT-HORA              PIC 9(6)     VALUE ZEROS.
           05  TOT-MIN               PIC 9(2)     VALUE ZEROS.
           05  HORA-INTERV           PIC 9(2)     VALUE ZEROS.
           05  MIN-INTERV            PIC 9(2)     VALUE ZEROS.

           05  TEMPOE.
               10  HOR-E             PIC ZZZZZZ.
               10  DOIS-PONTOS-E     PIC X VALUE ":".
               10  MIN-E             PIC 99.
           05  TEMPO-E REDEFINES TEMPOE PIC X(9).
           05  TEMPOW.
               10  HOR-W             PIC 99.
               10  MIN-W             PIC 99.
           05  TEMPO-W REDEFINES TEMPOW PIC 9(4).
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
           05  EMPRESA-REL         PIC X(70)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(43)   VALUE
           "RESUMO DE ATIVIDADES GERAIS       - ORDEM: ".
           05  ORDEM-REL           PIC X(18)   VALUE SPACES.
           05  FILLER              PIC X(10)    VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "INT.DATA MOVTO: ".
           05  DATA-INI-REL        PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  DATA-FIM-REL        PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "CODIGO FUNCIONARIO                    QT.OPERACAO    QT.ALBU
      -    "M     TEMPO    INTERP  MEDIA/HORA".
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
           MOVE "LBD028"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD028.
           MOVE "LBD105"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD105.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           OPEN INPUT CGD001 LBD028 LBD105.
           IF ST-LBD028 <> "00"
              MOVE "ERRO ABERTURA LBD028: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD028 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD105 <> "00"
              MOVE "ERRO ABERTURA LBD105: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD105 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-LER-INDIVIDUAL-TRUE
                    PERFORM LER-CODIGOS
               WHEN GS-OPCAO-POP-UP-TRUE
                    PERFORM LER-POPUP
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
       LER-CODIGOS SECTION.
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 1 MOVE GS-CODIGO TO CODIGO-LB28
                 READ LBD028 INVALID KEY MOVE SPACES TO DESCRICAO-LB28
                 END-READ
                 MOVE DESCRICAO-LB28 TO GS-DESCRICAO
           END-EVALUATE.

       LER-POPUP SECTION.
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 1 CALL   "LBP028T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP028T"
                    MOVE PASSAR-STRING-1(1: 20)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(33: 3)  TO GS-CODIGO
           END-EVALUATE.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-DATA-INI    TO DATA-INV DATA-INI-ANT DATA-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-INI.
           MOVE GS-DATA-FIM    TO DATA-INV DATA-FIM-ANT DATA-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-FIM.
      *    VERIFICA OPCAO DO RELATORIO - INDIV OU GERAL
           MOVE DATA-INI       TO DATA-MOVTO-L105
           MOVE GS-CODIGO      TO CODIGO-W
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 MOVE ZEROS TO SEQ-L105
                    START LBD105 KEY IS NOT < CHAVE-L105 INVALID KEY
                          MOVE "10" TO ST-LBD105
             WHEN 1 MOVE GS-CODIGO TO OPERACAO-L105
                    START LBD105 KEY IS NOT < ALT2-L105 INVALID KEY
                          MOVE "10" TO ST-LBD105
           END-EVALUATE.

           PERFORM UNTIL ST-LBD105 = "10"
             READ LBD105 NEXT RECORD AT END MOVE "10" TO ST-LBD105
              NOT AT END
               MOVE FUNCIONARIO-L105  TO GS-EXIBE-VENCTO
               MOVE "TELA-AGUARDA1"    TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM

               IF DATA-MOVTO-L105 > DATA-FIM
                  MOVE "10" TO ST-LBD105
               ELSE
                PERFORM VERIFICA-OPCAO
                IF GRAVAR-OPCAO = ZEROS MOVE "10" TO ST-LBD105
                                       CONTINUE
                ELSE
                 MOVE FUNCIONARIO-L105   TO COD-FUNCIONARIO-WK
                 READ WORK INVALID KEY INITIALIZE REG-WORK
                    MOVE FUNCIONARIO-L105  TO COD-FUNCIONARIO-WK
                                              CODIGO-CG01
                    READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                    END-READ
                    MOVE NOME-CG01         TO FUNCIONARIO-WK
                    MOVE QTDE-OPER-L105    TO QT-OPERACAO-WK
                    MOVE QT-ALBUM-L105     TO QT-ALBUM-WK

                    INITIALIZE PARAMETROS-GRTIME
                    MOVE HORA-INIC-L105     TO GRTIME-TIME(1: 4)
                    IF HORA-INIC-L105 > HORA-FIM-L105
                       ADD 2400 TO HORA-FIM-L105
                    END-IF
                    MOVE HORA-FIM-L105      TO GRTIME-TIME-FINAL(1: 4)
                    MOVE 3 TO GRTIME-FUNCTION
                    CALL "GRTIME" USING PARAMETROS-GRTIME
                    MOVE TEMPO-INTERVALO-L105(1: 2) TO HORA-INTERV
                    MOVE TEMPO-INTERVALO-L105(3: 2) TO MIN-INTERV
                    COMPUTE MIN-TEMPO-DECOR-WK =
                      ((GRTIME-TOTAL-HOURS * 60) + GRTIME-TOTAL-MINUTES)
                      - ((HORA-INTERV * 60) + MIN-INTERV)
                    MOVE TEMPO-INTERRUPC-L105(1: 2) TO TOT-HORA
                    MOVE TEMPO-INTERRUPC-L105(3: 2) TO TOT-MIN
                    COMPUTE MIN-TEMPO-INTERRUP-WK =
                       (TOT-HORA * 60) + TOT-MIN
                    WRITE REG-WORK
                    END-WRITE
                  NOT INVALID KEY
                    ADD QTDE-OPER-L105    TO QT-OPERACAO-WK
                    ADD QT-ALBUM-L105     TO QT-ALBUM-WK

                    INITIALIZE PARAMETROS-GRTIME
                    MOVE HORA-INIC-L105     TO GRTIME-TIME(1: 4)
                    IF HORA-INIC-L105 > HORA-FIM-L105
                       ADD 2400 TO HORA-FIM-L105
                    END-IF
                    MOVE HORA-FIM-L105      TO GRTIME-TIME-FINAL(1: 4)
                    MOVE 3 TO GRTIME-FUNCTION
                    CALL "GRTIME" USING PARAMETROS-GRTIME
                    MOVE TEMPO-INTERVALO-L105(1: 2) TO HORA-INTERV
                    MOVE TEMPO-INTERVALO-L105(3: 2) TO MIN-INTERV
                    COMPUTE MIN-TEMPO-DECOR-WK =
                     (((GRTIME-TOTAL-HOURS * 60) + GRTIME-TOTAL-MINUTES)
                      - ((HORA-INTERV * 60) + MIN-INTERV)) +
                      MIN-TEMPO-DECOR-WK
                    MOVE TEMPO-INTERRUPC-L105(1: 2) TO TOT-HORA
                    MOVE TEMPO-INTERRUPC-L105(3: 2) TO TOT-MIN
                    COMPUTE MIN-TEMPO-INTERRUP-WK = ((TOT-HORA * 60)
                        + TOT-MIN) + MIN-TEMPO-INTERRUP-WK
                    REWRITE REG-WORK
                    END-REWRITE
                END-IF
               END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       VERIFICA-OPCAO SECTION.
           MOVE GS-CODIGO TO CODIGO-W.
           MOVE 0 TO GRAVAR-OPCAO
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 MOVE 1 TO GRAVAR-OPCAO
             WHEN 1 IF CODIGO-W = OPERACAO-L105
                       MOVE 1 TO GRAVAR-OPCAO
           END-EVALUATE.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           INITIALIZE REG-WORK.
           MOVE "FUNCIONARIO" TO GS-DESCR-ORDEM
           START WORK KEY IS NOT < FUNCIONARIO-WK INVALID KEY
                 MOVE "10" TO ST-WORK

           PERFORM ZERA-VARIAVEIS.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "TOTAL GERAL: "        TO GS-LINDET(1: 30)
           MOVE TOTAL-OPERACAO-G       TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(39: 12)
           MOVE TOTAL-ALBUM-G          TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(51: 12)
           COMPUTE TOT-HORA = TOTAL-MIN-TEMPO-G / 60
           COMPUTE TOT-MIN = TOTAL-MIN-TEMPO-G - (TOT-HORA * 60)
           MOVE TOT-HORA               TO HOR-E
           MOVE TOT-MIN                TO MIN-E
           MOVE TEMPO-E                TO GS-LINDET(63: 10)
           COMPUTE TOT-HORA = TOTAL-MIN-INTERRUP-G / 60
           COMPUTE TOT-MIN = TOTAL-MIN-INTERRUP-G - (TOT-HORA * 60)
           MOVE TOT-HORA               TO HOR-E
           MOVE TOT-MIN                TO MIN-E
           MOVE TEMPO-E                TO GS-LINDET(73: 10).
           COMPUTE MEDIA-E = (TOTAL-OPERACAO-G / TOTAL-MIN-TEMPO-G)
                              * 60.
           MOVE MEDIA-E                TO GS-LINDET(83: 11).

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-LINDET SECTION.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE COD-FUNCIONARIO-WK     TO GS-LINDET(1: 7)
           MOVE FUNCIONARIO-WK         TO GS-LINDET(8: 31)
           MOVE QT-OPERACAO-WK         TO QTDE-E
           ADD QT-OPERACAO-WK          TO TOTAL-OPERACAO-G
           MOVE QTDE-E                 TO GS-LINDET(39: 12)
           MOVE QT-ALBUM-WK            TO QTDE-E
           ADD QT-ALBUM-WK             TO TOTAL-ALBUM-G
           MOVE QTDE-E                 TO GS-LINDET(51: 12)
           ADD MIN-TEMPO-DECOR-WK      TO TOTAL-MIN-TEMPO-G
           ADD MIN-TEMPO-INTERRUP-WK   TO TOTAL-MIN-INTERRUP-G
           COMPUTE TOT-HORA = MIN-TEMPO-DECOR-WK / 60
           COMPUTE TOT-MIN = MIN-TEMPO-DECOR-WK - (TOT-HORA * 60)
           MOVE TOT-HORA               TO HOR-E
           MOVE TOT-MIN                TO MIN-E
           MOVE TEMPO-E                TO GS-LINDET(63: 10)
           COMPUTE TOT-HORA = MIN-TEMPO-INTERRUP-WK / 60
           COMPUTE TOT-MIN = MIN-TEMPO-INTERRUP-WK - (TOT-HORA * 60)
           MOVE TOT-HORA               TO HOR-E
           MOVE TOT-MIN                TO MIN-E
           MOVE TEMPO-E                TO GS-LINDET(73: 10).
           COMPUTE MEDIA-E = (QT-OPERACAO-WK / MIN-TEMPO-DECOR-WK)
                              * 60.
           MOVE MEDIA-E                TO GS-LINDET(83: 11).

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO TOTAL-OPERACAO-G TOTAL-ALBUM-G
                         TOTAL-MIN-TEMPO-G TOTAL-MIN-INTERRUP-G.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP214" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE SPACES TO FUNCIONARIO-WK
           START WORK KEY IS NOT < FUNCIONARIO-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM ZERA-VARIAVEIS.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                      PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           MOVE SPACES          TO LINDET-REL

           MOVE "TOTAL GERAL: "        TO LINDET-REL(1: 30)
           MOVE TOTAL-OPERACAO-G       TO QTDE-E
           MOVE QTDE-E                 TO LINDET-REL(39: 12)
           MOVE TOTAL-ALBUM-G          TO QTDE-E
           MOVE QTDE-E                 TO LINDET-REL(51: 12)
           COMPUTE TOT-HORA = TOTAL-MIN-TEMPO-G / 60
           COMPUTE TOT-MIN = TOTAL-MIN-TEMPO-G - (TOT-HORA * 60)
           MOVE TOT-HORA               TO HOR-E
           MOVE TOT-MIN                TO MIN-E
           MOVE TEMPO-E                TO LINDET-REL(63: 10)
           COMPUTE TOT-HORA = TOTAL-MIN-INTERRUP-G / 60
           COMPUTE TOT-MIN = TOTAL-MIN-INTERRUP-G - (TOT-HORA * 60)
           MOVE TOT-HORA               TO HOR-E
           MOVE TOT-MIN                TO MIN-E
           MOVE TEMPO-E                TO LINDET-REL(73: 10).
           COMPUTE MEDIA-E = (TOTAL-OPERACAO-G / TOTAL-MIN-TEMPO-G)
                              * 60.
           MOVE MEDIA-E                TO LINDET-REL(83: 11).

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
           CLOSE CGD001 LBD028 LBD105.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
