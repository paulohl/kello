       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP203.
      *DATA: 01/06/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RELATORIO DE ATIVIDADES GERAIS DE PRODUÇÃO
      *FUNÇÃO: Listar todos os trabalhos que estiverem dentro do
      *        intervalo de data solicitado. Poderá ser geral ou indiv.
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY LBPX023.
           COPY LBPX028.
           COPY LBPX029.
           COPY LBPX105.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS DATA-MOVTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FUNCIONARIO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-INTER-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TURNO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS OPERACAO-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY LBPW023.
       COPY LBPW028.
       COPY LBPW029.
       COPY LBPW105.
       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(4).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  FUNCIONARIO-WK      PIC X(20).
           05  TURNO-WK            PIC X(05).
           05  QT-FOTOS-WK         PIC 9(4).
           05  QT-ALBUM-WK         PIC 9(4).
           05  OPERACAO-WK         PIC X(15).
           05  TEMPO-DECOR-WK      PIC 9(4).
      *    TEMPO DECORRIDO = (HORA-FIM - HORA-INI)
           05  TEMPO-INTERV-WK     PIC 9(4).
      *    TEMPO INTERRUPÇÃO = HORAS E MINUTOS DE INTERVALO
           05  TEMPO-INTERRUP-WK   PIC 9(4).
      *    TEMPO INTERRUPÇÃO = HORAS E MINUTOS DE INTERRUPÇÃO
           05  TIPO-INTER-WK       PIC X(15).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP203.CPB".
           COPY "LBP203.CPY".
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
           05  ST-LBD023             PIC XX       VALUE SPACES.
           05  ST-LBD028             PIC XX       VALUE SPACES.
           05  ST-LBD029             PIC XX       VALUE SPACES.
           05  ST-LBD105             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
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
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-INI-ANT          PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM-ANT          PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
      *    OPCAO INDIVIDUAL
           05  GRAVAR-OPCAO          PIC 9        VALUE ZEROS.
           05  CODIGO-W              PIC 9(3)     VALUE ZEROS.

      *    CONTROLE DE QUEBRA
           05  DATA-MOVTO-ANT        PIC 9(8)     VALUE ZEROS.
           05  FUNCIONARIO-ANT       PIC X(20)    VALUE SPACES.
           05  TURNO-ANT             PIC X(05)    VALUE SPACES.
           05  OPERACAO-ANT          PIC X(15)    VALUE SPACES.
           05  TIPO-INTERRUPCAO-ANT  PIC X(15)    VALUE SPACES.
      *    TOTALIZA VARIAVEIS
           05  TOTAL-FOTO            PIC 9(7)     VALUE ZEROS.
           05  TOTAL-ALBUM           PIC 9(5)     VALUE ZEROS.
           05  TOTAL-TEMPO-HOR       PIC 9(4)     VALUE ZEROS.
           05  TOTAL-TEMPO-MIN       PIC 9(5)     VALUE ZEROS.
           05  TOTAL-INT-HOR         PIC 9(4)     VALUE ZEROS.
           05  TOTAL-INT-MIN         PIC 9(5)     VALUE ZEROS.
           05  TOTAL-INTERV-HOR      PIC 9(4)     VALUE ZEROS.
           05  TOTAL-INTERV-MIN      PIC 9(5)     VALUE ZEROS.
           05  TOTAL-FOTO-G          PIC 9(7)     VALUE ZEROS.
           05  TOTAL-ALBUM-G         PIC 9(5)     VALUE ZEROS.
           05  TOTAL-TEMPO-HOR-G     PIC 9(4)     VALUE ZEROS.
           05  TOTAL-TEMPO-MIN-G     PIC 9(5)     VALUE ZEROS.
           05  TOTAL-INT-HOR-G       PIC 9(4)     VALUE ZEROS.
           05  TOTAL-INT-MIN-G       PIC 9(5)     VALUE ZEROS.
           05  TOTAL-INTERV-HOR-G    PIC 9(4)     VALUE ZEROS.
           05  TOTAL-INTERV-MIN-G    PIC 9(5)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZ.ZZZ.
           05  FOTO-E                PIC ZZZ.ZZZ.
           05  ALBUM-E               PIC ZZZZZ.
      *    VARIAVEIS P/ CALCULO DE HORAS
           05  TOT-HORA              PIC 9(2)     VALUE ZEROS.
           05  TOT-MIN               PIC 9(2)     VALUE ZEROS.
           05  HORA-INTERV           PIC 9(2)     VALUE ZEROS.
           05  MIN-INTERV            PIC 9(2)     VALUE ZEROS.

           05  TEMPOE.
               10  HOR-E             PIC ZZZZ.
               10  DOIS-PONTOS-E     PIC X VALUE ":".
               10  MIN-E             PIC 99.
           05  TEMPO-E REDEFINES TEMPOE PIC X(7).
           05  TEMPOW.
               10  HOR-W             PIC 99.
               10  MIN-W             PIC 99.
           05  TEMPO-W REDEFINES TEMPOW PIC 9(4).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
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
           05  FILLER              PIC X(38)   VALUE
           "ATIVIDADES GERAIS DE PRODUCAO- ORDEM: ".
           05  ORDEM-REL           PIC X(23)   VALUE SPACES.
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
           "DATA-MOVTO FUNCIONARIO          TURNO OPERACAO          FOTO
      -    "S ALBUM   TEMPO INTERV INTERP TIPO-INTERRUP".
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
           MOVE "LBD023"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD023.
           MOVE "LBD028"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD028.
           MOVE "LBD029"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD029.
           MOVE "LBD105"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD105.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           OPEN INPUT CGD001 LBD023 LBD028 LBD029 LBD105.
           IF ST-LBD023 <> "00"
              MOVE "ERRO ABERTURA LBD023: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD023 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD028 <> "00"
              MOVE "ERRO ABERTURA LBD028: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD028 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD029 <> "00"
              MOVE "ERRO ABERTURA LBD029: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD029 TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM IMPRIME-RELATORIO
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
             WHEN 1 MOVE GS-CODIGO TO CODIGO-CG01
                 READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                 END-READ
                 MOVE NOME-CG01    TO GS-DESCRICAO
             WHEN 2 MOVE GS-CODIGO TO CODIGO-LB28
                 READ LBD028 INVALID KEY MOVE SPACES TO DESCRICAO-LB28
                 END-READ
                 MOVE DESCRICAO-LB28 TO GS-DESCRICAO
             WHEN 3 MOVE GS-CODIGO TO CODIGO-LB29
                 READ LBD029 INVALID KEY MOVE SPACES TO DESCRICAO-LB29
                 END-READ
                 MOVE DESCRICAO-LB29 TO GS-DESCRICAO
           END-EVALUATE.

       LER-POPUP SECTION.
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 1 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(1: 30)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(33: 6)  TO GS-CODIGO
             WHEN 2 CALL   "LBP028T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP028T"
                    MOVE PASSAR-STRING-1(1: 20)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(33: 3)  TO GS-CODIGO
             WHEN 3 CALL   "LBP029T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP029T"
                    MOVE PASSAR-STRING-1(1: 20)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(33: 2)  TO GS-CODIGO
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
             WHEN 1 MOVE GS-CODIGO TO FUNCIONARIO-L105
                    START LBD105 KEY IS NOT < ALT1-L105 INVALID KEY
                          MOVE "10" TO ST-LBD105
             WHEN 2 MOVE CODIGO-W TO OPERACAO-L105
                    START LBD105 KEY IS NOT < ALT2-L105 INVALID KEY
                          MOVE "10" TO ST-LBD105
             WHEN 3 MOVE CODIGO-W TO TIPO-INTERR-L105
                    START LBD105 KEY IS NOT < ALT3-L105 INVALID KEY
                          MOVE "10" TO ST-LBD105
           END-EVALUATE.

           MOVE ZEROS          TO SEQ-WK.
           PERFORM UNTIL ST-LBD105 = "10"
             READ LBD105 NEXT RECORD AT END MOVE "10" TO ST-LBD105
              NOT AT END
               IF DATA-MOVTO-L105 > DATA-FIM
                  MOVE "10" TO ST-LBD105
               ELSE
                PERFORM VERIFICA-OPCAO
                IF GRAVAR-OPCAO = ZEROS MOVE "10" TO ST-LBD105
                                       CONTINUE
                ELSE
                 ADD 1                   TO SEQ-WK
                 MOVE DATA-MOVTO-L105    TO DATA-MOVTO-WK
                                            GS-EXIBE-VENCTO
                 MOVE "TELA-AGUARDA1"    TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 MOVE FUNCIONARIO-L105   TO CODIGO-CG01
                 READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                 END-READ
                 MOVE NOME-CG01           TO FUNCIONARIO-WK
                 MOVE QTDE-OPER-L105     TO QT-FOTOS-WK
                 MOVE QT-ALBUM-L105    TO QT-ALBUM-WK

                 MOVE OPERACAO-L105       TO CODIGO-LB28
                 READ LBD028 INVALID KEY MOVE SPACES TO DESCRICAO-LB28
                 END-READ
                 MOVE DESCRICAO-LB28     TO OPERACAO-WK
                 MOVE TIPO-INTERR-L105   TO CODIGO-LB29
                 READ LBD029 INVALID KEY MOVE SPACES TO DESCRICAO-LB29
                 END-READ
                 MOVE DESCRICAO-LB29     TO TIPO-INTER-WK
                 MOVE TURNO-L105         TO CODIGO-LB23
                 READ LBD023 INVALID KEY MOVE SPACES TO DESCRICAO-LB23
                 END-READ
                 MOVE DESCRICAO-LB23     TO TURNO-WK
                 MOVE ZEROS TO GRTIME-TIME GRTIME-TIME-FINAL
                 MOVE HORA-INIC-L105     TO GRTIME-TIME(1: 4)
                 MOVE HORA-FIM-L105      TO GRTIME-TIME-FINAL(1: 4)
                 MOVE 3 TO GRTIME-FUNCTION
                 CALL "GRTIME" USING PARAMETROS-GRTIME
      *          MOVE TEMPO-INTERVALO-L105(1: 2) TO HORA-INTERV
      *
      *          MOVE TEMPO-INTERVALO-L105(3: 2) TO MIN-INTERV
      *          COMPUTE TOT-HORA = GRTIME-TOTAL-HOURS - HORA-INTERV
      *          COMPUTE TOT-MIN = GRTIME-TOTAL-MINUTES - MIN-INTERV
      *          MOVE TOT-HORA TO TEMPO-DECOR-WK(1: 2)
      *          MOVE TOT-MIN  TO TEMPO-DECOR-WK(3: 2)
                 MOVE GRTIME-TOTAL-HOURS    TO TOT-HORA
                 MOVE GRTIME-TOTAL-MINUTES  TO TOT-MIN
                 MOVE TOT-HORA              TO TEMPO-DECOR-WK(1: 2)
                 MOVE TOT-MIN               TO TEMPO-DECOR-WK(3: 2)
                 MOVE TEMPO-INTERVALO-L105  TO TEMPO-INTERV-WK
                 MOVE TEMPO-INTERRUPC-L105  TO TEMPO-INTERRUP-WK

                 WRITE REG-WORK
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
             WHEN 1 IF GS-CODIGO = FUNCIONARIO-L105
                       MOVE 1 TO GRAVAR-OPCAO
             WHEN 2 IF CODIGO-W = OPERACAO-L105
                       MOVE 1 TO GRAVAR-OPCAO
             WHEN 3 IF CODIGO-W = TIPO-INTERR-L105
                       MOVE 1 TO GRAVAR-OPCAO
           END-EVALUATE.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           PERFORM ZERA-VARIAVEIS.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                  PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA
           MOVE "TOTAL GERAL: " TO GS-LINDET(1: 30)
           MOVE TOTAL-FOTO-G           TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(55: 8)
           MOVE TOTAL-ALBUM-G          TO ALBUM-E
           MOVE ALBUM-E                TO GS-LINDET(63: 6)

      *    calcula total de tempo decorrido
           COMPUTE HOR-W = (TOTAL-TEMPO-MIN-G / 60)
           COMPUTE TOTAL-TEMPO-MIN-G = TOTAL-TEMPO-MIN-G - (HOR-W * 60)
           MOVE TOTAL-TEMPO-MIN-G      TO MIN-E
           COMPUTE TOTAL-TEMPO-HOR-G = HOR-W + TOTAL-TEMPO-HOR-G
           MOVE TOTAL-TEMPO-HOR-G      TO HOR-E
           MOVE TEMPO-E                TO GS-LINDET(69: 8)
      *    calcula total de tempo de intervalo
           COMPUTE HOR-W = (TOTAL-INTERV-MIN-G / 60)
           COMPUTE TOTAL-INTERV-MIN-G = TOTAL-INTERV-MIN-G
                            - (HOR-W * 60)
           MOVE TOTAL-INTERV-MIN-G      TO MIN-E
           COMPUTE TOTAL-INTERV-HOR-G = HOR-W + TOTAL-INTERV-HOR-G
           MOVE TOTAL-INTERV-HOR-G      TO HOR-E
           MOVE TEMPO-E                 TO GS-LINDET(77: 7)
      *    calcula total de tempo de interrupção
           COMPUTE HOR-W = (TOTAL-INT-MIN-G / 60)
           COMPUTE TOTAL-INT-MIN-G = TOTAL-INT-MIN-G - (HOR-W * 60)
           MOVE TOTAL-INT-MIN-G      TO MIN-E
           COMPUTE TOTAL-INT-HOR-G = HOR-W + TOTAL-INT-HOR-G
           MOVE TOTAL-INT-HOR-G      TO HOR-E
           MOVE TEMPO-E              TO GS-LINDET(84: 7)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "DATA-MOVTO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DATA-MOVTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "FUNCIONARIO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < FUNCIONARIO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "TURNO     " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < TURNO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "OPERAÇÃO " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < OPERACAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "TIPO-INTERRUPÇ" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < TIPO-INTER-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF DATA-MOVTO-ANT NOT = ZEROS
                 IF DATA-MOVTO-ANT NOT = DATA-MOVTO-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF FUNCIONARIO-ANT  NOT = SPACES
                 IF FUNCIONARIO-ANT NOT = FUNCIONARIO-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF TURNO-ANT NOT = SPACES
                 IF TURNO-ANT NOT = TURNO-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF OPERACAO-ANT NOT = SPACES
                 IF OPERACAO-ANT NOT = OPERACAO-WK
                    PERFORM TOTALIZA
             WHEN 5
              IF TIPO-INTERRUPCAO-ANT NOT = SPACES
                 IF TIPO-INTERRUPCAO-ANT NOT = TIPO-INTER-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE DATA-MOVTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV               TO DATA-E
           MOVE DATA-E                 TO GS-LINDET(1: 11)
           MOVE FUNCIONARIO-WK         TO GS-LINDET(12: 21)
           MOVE TURNO-WK               TO GS-LINDET(33: 6)
           MOVE OPERACAO-WK            TO GS-LINDET(39: 16)
           MOVE QT-FOTOS-WK            TO FOTO-E
           ADD QT-FOTOS-WK             TO TOTAL-FOTO.
           MOVE FOTO-E                 TO GS-LINDET(55: 8)
           MOVE QT-ALBUM-WK            TO ALBUM-E
           MOVE ALBUM-E                TO GS-LINDET(63: 6)
           ADD QT-ALBUM-WK             TO TOTAL-ALBUM
           MOVE TEMPO-DECOR-WK(1: 2)   TO HOR-E
           MOVE TEMPO-DECOR-WK(3: 2)   TO MIN-E
           MOVE TEMPO-DECOR-WK         TO TEMPO-W
           ADD HOR-W                   TO TOTAL-TEMPO-HOR
           ADD MIN-W                   TO TOTAL-TEMPO-MIN
           MOVE TEMPO-E                TO GS-LINDET(69: 8)

           MOVE TEMPO-INTERV-WK(1: 2)  TO HOR-E
           MOVE TEMPO-INTERV-WK(3: 2)  TO MIN-E
           MOVE TEMPO-INTERV-WK        TO TEMPO-W
           ADD HOR-W                   TO TOTAL-INTERV-HOR
           ADD MIN-W                   TO TOTAL-INTERV-MIN
           MOVE TEMPO-E                TO GS-LINDET(77: 7)

           MOVE TEMPO-INTERRUP-WK(1: 2) TO HOR-E
           MOVE TEMPO-INTERRUP-WK(3: 2) TO MIN-E
           MOVE TEMPO-INTERRUP-WK       TO TEMPO-W
           ADD HOR-W                    TO TOTAL-INT-HOR
           ADD MIN-W                    TO TOTAL-INT-MIN
           MOVE TEMPO-E                 TO GS-LINDET(84: 7)
           MOVE TIPO-INTER-WK           TO GS-LINDET(92: 15).

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO DATA-MOVTO-ANT.
           MOVE SPACES TO FUNCIONARIO-ANT TURNO-ANT OPERACAO-ANT
                          TIPO-INTERRUPCAO-ANT.
           MOVE ZEROS TO TOTAL-FOTO-G TOTAL-ALBUM-G TOTAL-TEMPO-HOR-G
                         TOTAL-TEMPO-MIN-G TOTAL-INT-HOR-G
                         TOTAL-INT-MIN-G TOTAL-ALBUM-G
                         TOTAL-INTERV-MIN-G TOTAL-INTERV-HOR-G
           PERFORM ZERA-SUBTOTAL.

       ZERA-SUBTOTAL SECTION.
           MOVE ZEROS TO TOTAL-FOTO TOTAL-ALBUM TOTAL-TEMPO-HOR
                         TOTAL-TEMPO-MIN TOTAL-INT-HOR TOTAL-ALBUM
                         TOTAL-INTERV-MIN TOTAL-INTERV-HOR
                         TOTAL-INT-MIN.

       MOVER-CHAVE-ANT SECTION.
           MOVE DATA-MOVTO-WK          TO DATA-MOVTO-ANT.
           MOVE FUNCIONARIO-WK         TO FUNCIONARIO-ANT.
           MOVE TURNO-WK               TO TURNO-ANT
           MOVE OPERACAO-WK            TO OPERACAO-ANT
           MOVE TIPO-INTER-WK          TO TIPO-INTERRUPCAO-ANT.
       TOTALIZA SECTION.
           MOVE SPACES                 TO GS-LINDET
           MOVE TOTAL-FOTO             TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(55: 8)
           MOVE TOTAL-ALBUM            TO ALBUM-E
           MOVE ALBUM-E                TO GS-LINDET(63: 6)

      *    calcula total de tempo decorrido
           COMPUTE HOR-W = (TOTAL-TEMPO-MIN / 60)
           COMPUTE TOTAL-TEMPO-MIN = TOTAL-TEMPO-MIN - (HOR-W * 60)
           MOVE TOTAL-TEMPO-MIN        TO MIN-E
           COMPUTE TOTAL-TEMPO-HOR = HOR-W + TOTAL-TEMPO-HOR
           MOVE TOTAL-TEMPO-HOR        TO HOR-E
           MOVE TEMPO-E                TO GS-LINDET(69: 8)

      *    calcula total de tempo de interrupção
           COMPUTE HOR-W = (TOTAL-INTERV-MIN / 60)
           COMPUTE TOTAL-INTERV-MIN = TOTAL-INTERV-MIN - (HOR-W * 60)
           MOVE TOTAL-INTERV-MIN          TO MIN-E
           COMPUTE TOTAL-INTERV-HOR = HOR-W + TOTAL-INTERV-HOR
           MOVE TOTAL-INTERV-HOR          TO HOR-E
           MOVE TEMPO-E                   TO GS-LINDET(77: 7)

      *    calcula total de tempo de interrupção
           COMPUTE HOR-W = (TOTAL-INT-MIN / 60)
           COMPUTE TOTAL-INT-MIN = TOTAL-INT-MIN - (HOR-W * 60)
           MOVE TOTAL-INT-MIN          TO MIN-E
           COMPUTE TOTAL-INT-HOR = HOR-W + TOTAL-INT-HOR
           MOVE TOTAL-INT-HOR          TO HOR-E
           MOVE TEMPO-E                TO GS-LINDET(84: 7)

           ADD TOTAL-FOTO              TO TOTAL-FOTO-G
           ADD TOTAL-ALBUM             TO TOTAL-ALBUM-G
           ADD TOTAL-TEMPO-HOR         TO TOTAL-TEMPO-HOR-G
           ADD TOTAL-TEMPO-MIN         TO TOTAL-TEMPO-MIN-G
           ADD TOTAL-INTERV-HOR        TO TOTAL-INTERV-HOR-G
           ADD TOTAL-INTERV-MIN        TO TOTAL-INTERV-MIN-G.
           ADD TOTAL-INT-HOR           TO TOTAL-INT-HOR-G
           ADD TOTAL-INT-MIN           TO TOTAL-INT-MIN-G.

           PERFORM ZERA-SUBTOTAL.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP203" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           PERFORM ZERA-VARIAVEIS.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                      PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           MOVE SPACES          TO LINDET-REL

           MOVE "TOTAL GERAL: "        TO LINDET-REL(1: 30)
           MOVE TOTAL-FOTO-G           TO QTDE-E
           MOVE QTDE-E                 TO LINDET-REL(55: 8)
           MOVE TOTAL-ALBUM-G          TO ALBUM-E
           MOVE ALBUM-E                TO LINDET-REL(63: 6)

      *    calcula total de tempo decorrido
           COMPUTE HOR-W = (TOTAL-TEMPO-MIN-G / 60)
           COMPUTE TOTAL-TEMPO-MIN-G = TOTAL-TEMPO-MIN-G - (HOR-W * 60)
           MOVE TOTAL-TEMPO-MIN-G      TO MIN-E
           COMPUTE TOTAL-TEMPO-HOR-G = HOR-W + TOTAL-TEMPO-HOR-G
           MOVE TOTAL-TEMPO-HOR-G      TO HOR-E
           MOVE TEMPO-E                TO LINDET-REL(69: 8)
      *    calcula total de tempo de INTERVALO
           COMPUTE HOR-W = (TOTAL-INTERV-MIN-G / 60)
           COMPUTE TOTAL-INTERV-MIN-G =
                            TOTAL-INTERV-MIN-G - (HOR-W * 60)
           MOVE TOTAL-INTERV-MIN-G      TO MIN-E
           COMPUTE TOTAL-INTERV-HOR-G = HOR-W + TOTAL-INTERV-HOR-G
           MOVE TOTAL-INTERV-HOR-G      TO HOR-E
           MOVE TEMPO-E                 TO LINDET-REL(77: 7)
      *    calcula total de tempo de interrupção
           COMPUTE HOR-W = (TOTAL-INT-MIN-G / 60)
           COMPUTE TOTAL-INT-MIN-G = TOTAL-INT-MIN-G - (HOR-W * 60)
           MOVE TOTAL-INT-MIN-G      TO MIN-E
           COMPUTE TOTAL-INT-HOR-G = HOR-W + TOTAL-INT-HOR-G
           MOVE TOTAL-INT-HOR-G      TO HOR-E
           MOVE TEMPO-E                TO LINDET-REL(84: 7)

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF DATA-MOVTO-ANT NOT = ZEROS
                 IF DATA-MOVTO-ANT NOT = DATA-MOVTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF FUNCIONARIO-ANT  NOT = SPACES
                 IF FUNCIONARIO-ANT NOT = FUNCIONARIO-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF TURNO-ANT NOT = SPACES
                 IF TURNO-ANT NOT = TURNO-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF OPERACAO-ANT NOT = SPACES
                 IF OPERACAO-ANT NOT = OPERACAO-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF TIPO-INTERRUPCAO-ANT NOT = SPACES
                 IF TIPO-INTERRUPCAO-ANT NOT = TIPO-INTER-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS
           MOVE GS-LINDET TO LINDET-REL

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES                 TO LINDET-REL

           MOVE TOTAL-FOTO             TO QTDE-E
           MOVE QTDE-E                 TO LINDET-REL(60: 8)
           MOVE TOTAL-ALBUM            TO ALBUM-E
           MOVE ALBUM-E                TO LINDET-REL(68: 6)

      *    calcula total de tempo decorrido
           COMPUTE HOR-W = (TOTAL-TEMPO-MIN / 60)
           COMPUTE TOTAL-TEMPO-MIN = TOTAL-TEMPO-MIN - (HOR-W * 60)
           MOVE TOTAL-TEMPO-MIN        TO MIN-E
           COMPUTE TOTAL-TEMPO-HOR = HOR-W + TOTAL-TEMPO-HOR
           MOVE TOTAL-TEMPO-HOR        TO HOR-E
           MOVE TEMPO-E                TO LINDET-REL(74: 8)
      *    calcula total de tempo de INTERVALO
           COMPUTE HOR-W = (TOTAL-INTERV-MIN / 60)
           COMPUTE TOTAL-INTERV-MIN = TOTAL-INTERV-MIN - (HOR-W * 60)
           MOVE TOTAL-INTERV-MIN        TO MIN-E
           COMPUTE TOTAL-INTERV-HOR = HOR-W + TOTAL-INT-HOR
           MOVE TOTAL-INTERV-HOR        TO HOR-E
           MOVE TEMPO-E                 TO LINDET-REL(82: 7)
      *    calcula total de tempo de interrupção
           COMPUTE HOR-W = (TOTAL-INT-MIN / 60)
           COMPUTE TOTAL-INT-MIN = TOTAL-INT-MIN - (HOR-W * 60)
           MOVE TOTAL-INT-MIN        TO MIN-E
           COMPUTE TOTAL-INT-HOR = HOR-W + TOTAL-INT-HOR
           MOVE TOTAL-INT-HOR        TO HOR-E
           MOVE TEMPO-E              TO LINDET-REL(89: 7)

           ADD TOTAL-FOTO         TO TOTAL-FOTO-G
           ADD TOTAL-ALBUM        TO TOTAL-ALBUM-G
           ADD TOTAL-ALBUM        TO TOTAL-ALBUM-G
           ADD TOTAL-TEMPO-HOR    TO TOTAL-TEMPO-HOR-G
           ADD TOTAL-TEMPO-MIN    TO TOTAL-TEMPO-MIN-G
           ADD TOTAL-INTERV-HOR   TO TOTAL-INT-HOR-G
           ADD TOTAL-INTERV-MIN   TO TOTAL-INT-MIN-G.
           ADD TOTAL-INT-HOR      TO TOTAL-INT-HOR-G
           ADD TOTAL-INT-MIN      TO TOTAL-INT-MIN-G.

           PERFORM ZERA-SUBTOTAL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.

           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
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
           CLOSE LBD023 LBD028 LBD029 LBD105 CGD001.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
