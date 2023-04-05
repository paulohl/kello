       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP202.
      *DATA: 31/05/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RELATORIO DE TRABALHO DE AMPLIAÇÃO
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
           COPY LBPX026.
           COPY LBPX027.
           COPY LBPX029.
           COPY LBPX103.
           COPY LBPX104.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS DATA-MOVTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FUNCIONARIO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-INTER-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-FOTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS IMPRESSORA-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY LBPW026.
       COPY LBPW027.
       COPY LBPW029.
       COPY LBPW103.
       COPY LBPW104.
       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(4).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  FUNCIONARIO-WK      PIC X(15).
           05  IMPRESSORA-WK       PIC X(15).
           05  QT-FOTOS-WK         PIC 9(4).
           05  TIPO-FOTO-WK        PIC X(10).
           05  QT-ROLOS-WK         PIC 9(3)V9(3).
           05  QT-PERDAS-WK        PIC 9(4).
           05  TEMPO-DECOR-WK      PIC 9(4).
      *    TEMPO DECORRIDO = (HORA-FIM - HORA-INI) - TEMPO-INTERVALO
           05  TEMPO-INTERRUP-WK   PIC 9(4).
      *    TEMPO INTERRUPÇÃO = HORAS E MINUTOS DE INTERRUPÇÃO
           05  TIPO-INTER-WK       PIC X(15).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP202.CPB".
           COPY "LBP202.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-LBD026             PIC XX       VALUE SPACES.
           05  ST-LBD027             PIC XX       VALUE SPACES.
           05  ST-LBD029             PIC XX       VALUE SPACES.
           05  ST-LBD103             PIC XX       VALUE SPACES.
           05  ST-LBD104             PIC XX       VALUE SPACES.
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
           05  CODIGO-W              PIC 9(2)     VALUE ZEROS.

      *    CONTROLE DE QUEBRA
           05  DATA-MOVTO-ANT        PIC 9(8)     VALUE ZEROS.
           05  FUNCIONARIO-ANT       PIC X(15)    VALUE SPACES.
           05  IMPRESSORA-ANT        PIC X(15)    VALUE SPACES.
           05  TIPO-FOTO-ANT         PIC X(10)    VALUE SPACES.
           05  TIPO-INTERRUPCAO-ANT  PIC X(15)    VALUE SPACES.
      *    TOTALIZA VARIAVEIS
           05  TOTAL-FOTO            PIC 9(7)     VALUE ZEROS.
           05  TOTAL-ROLO            PIC 9(4)V999 VALUE ZEROS.
           05  TOTAL-PERDA           PIC 9(5)     VALUE ZEROS.
           05  TOTAL-TEMPO-HOR       PIC 9(4)     VALUE ZEROS.
           05  TOTAL-TEMPO-MIN       PIC 9(5)     VALUE ZEROS.
           05  TOTAL-INT-HOR         PIC 9(4)     VALUE ZEROS.
           05  TOTAL-INT-MIN         PIC 9(5)     VALUE ZEROS.
           05  TOTAL-FOTO-G          PIC 9(7)     VALUE ZEROS.
           05  TOTAL-ROLO-G          PIC 9(4)V999 VALUE ZEROS.
           05  TOTAL-PERDA-G         PIC 9(5)     VALUE ZEROS.
           05  TOTAL-TEMPO-HOR-G     PIC 9(4)     VALUE ZEROS.
           05  TOTAL-TEMPO-MIN-G     PIC 9(5)     VALUE ZEROS.
           05  TOTAL-INT-HOR-G       PIC 9(4)     VALUE ZEROS.
           05  TOTAL-INT-MIN-G       PIC 9(5)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZZZZZ.
           05  FOTO-E                PIC ZZZ.ZZZ.
           05  ROLO-E                PIC ZZZZ,ZZZ.
           05  PERDA-E               PIC ZZZZZ.
      *    VARIAVEIS P/ CALCULO DE HORAS
           05  TOT-HORA              PIC 9(2)     VALUE ZEROS.
           05  TOT-MIN               PIC 9(2)     VALUE ZEROS.
           05  HORA-INTERV           PIC 9(2)     VALUE ZEROS.
           05  MIN-INTERV            PIC 9(2)     VALUE ZEROS.
           05  TOTAL-MINUTO          PIC 9(5)     VALUE ZEROS.

           05  TEMPOE.
               10  HOR-E             PIC ZZZZ.
               10  DOIS-PONTOS-E     PIC X VALUE ":".
               10  MIN-E             PIC ZZ.
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
           05  FILLER              PIC X(43)   VALUE
           "RELATORIO DE TRABALHO DE AMPLIACAO- ORDEM: ".
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
           "DATA-MOVTO FUNCIONÁRIO     IMPRESSORA      TIPO-FOTO     ROL
      -    "OS   FOTOS PERDA   TEMPO INTERP TIPO     ".
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
           MOVE "LBD026"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD026.
           MOVE "LBD027"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD027.
           MOVE "LBD029"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD029.
           MOVE "LBD103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD103.
           MOVE "LBD104"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD104.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           OPEN INPUT CGD001 LBD026 LBD027 LBD029 LBD103 LBD104.
           IF ST-LBD026 <> "00"
              MOVE "ERRO ABERTURA LBD026: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD026 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD027 <> "00"
              MOVE "ERRO ABERTURA LBD027: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD027 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD029 <> "00"
              MOVE "ERRO ABERTURA LBD029: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD029 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD103 <> "00"
              MOVE "ERRO ABERTURA LBD103: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD103 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD104 <> "00"
              MOVE "ERRO ABERTURA LBD104: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD104 TO GS-MENSAGEM-ERRO(23: 02)
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
             WHEN 1 MOVE GS-CODIGO TO CODIGO-CG01
                 READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                 END-READ
                 MOVE NOME-CG01    TO GS-DESCRICAO
             WHEN 2 MOVE GS-CODIGO TO CODIGO-LB26
                 READ LBD026 INVALID KEY MOVE SPACES TO DESCRICAO-LB26
                 END-READ
                 MOVE DESCRICAO-LB26 TO GS-DESCRICAO
             WHEN 3 MOVE GS-CODIGO TO CODIGO-LB27
                 READ LBD027 INVALID KEY MOVE SPACES TO DESCRICAO-LB27
                 END-READ
                 MOVE DESCRICAO-LB27 TO GS-DESCRICAO
             WHEN 4 MOVE GS-CODIGO TO CODIGO-LB29
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
             WHEN 2 CALL   "LBP026T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP026T"
                    MOVE PASSAR-STRING-1(1: 20)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(33: 2)  TO GS-CODIGO
             WHEN 3 CALL   "LBP027T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP027T"
                    MOVE PASSAR-STRING-1(1: 20)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(33: 2)  TO GS-CODIGO
             WHEN 4 CALL   "LBP029T" USING PARAMETROS-W PASSAR-STRING-1
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
           MOVE DATA-INI       TO DATA-MOVTO-L103
           MOVE GS-CODIGO      TO CODIGO-W
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 MOVE ZEROS TO SEQ-L103
                    START LBD103 KEY IS NOT < CHAVE-L103 INVALID KEY
                          MOVE "10" TO ST-LBD103
             WHEN 1 MOVE GS-CODIGO TO FUNCIONARIO-L103
                    START LBD103 KEY IS NOT < ALT1-L103 INVALID KEY
                          MOVE "10" TO ST-LBD103
             WHEN 2 MOVE CODIGO-W TO IMPRESSORA-L103
                    START LBD103 KEY IS NOT < ALT2-L103 INVALID KEY
                          MOVE "10" TO ST-LBD103
             WHEN 3 MOVE CODIGO-W TO TIPO-FOTO-L103
                    START LBD103 KEY IS NOT < ALT3-L103 INVALID KEY
                          MOVE "10" TO ST-LBD103
             WHEN 4 MOVE CODIGO-W TO TIPO-INTERR-L103
                    START LBD103 KEY IS NOT < ALT4-L103 INVALID KEY
                          MOVE "10" TO ST-LBD103
           END-EVALUATE.

           MOVE ZEROS          TO SEQ-WK.
           PERFORM UNTIL ST-LBD103 = "10"
             READ LBD103 NEXT RECORD AT END MOVE "10" TO ST-LBD103
              NOT AT END
               IF DATA-MOVTO-L103 > DATA-FIM
                  MOVE "10" TO ST-LBD103
               ELSE
                PERFORM VERIFICA-OPCAO
                IF GRAVAR-OPCAO = ZEROS MOVE "10" TO ST-LBD103
                                       CONTINUE
                ELSE
                 ADD 1                   TO SEQ-WK
                 MOVE DATA-MOVTO-L103    TO DATA-MOVTO-WK
                                            GS-EXIBE-VENCTO
                 MOVE "TELA-AGUARDA1"    TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 MOVE FUNCIONARIO-L103   TO CODIGO-CG01
                 READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                 END-READ
                 MOVE NOME-CG01          TO FUNCIONARIO-WK
                 MOVE QTDE-FOTOS-L103    TO QT-FOTOS-WK
                 MOVE QTDE-ROLOS-L103    TO QT-ROLOS-WK
                 MOVE ZEROS              TO QT-PERDAS-WK
                 PERFORM VERIFICA-QTDE-PERDAS

                 MOVE TIPO-FOTO-L103    TO CODIGO-LB27
                 READ LBD027 INVALID KEY MOVE SPACES TO DESCRICAO-LB27
                 END-READ
                 MOVE DESCRICAO-LB27     TO TIPO-FOTO-WK
                 MOVE TIPO-INTERR-L103   TO CODIGO-LB29
                 READ LBD029 INVALID KEY MOVE SPACES TO DESCRICAO-LB29
                 END-READ
                 MOVE DESCRICAO-LB29     TO TIPO-INTER-WK
                 MOVE IMPRESSORA-L103    TO CODIGO-LB26
                 READ LBD026 INVALID KEY MOVE SPACES TO DESCRICAO-LB26
                 END-READ
                 MOVE DESCRICAO-LB26     TO IMPRESSORA-WK
                 MOVE ZEROS TO GRTIME-TIME GRTIME-TIME-FINAL
                 MOVE HORA-INIC-L103     TO GRTIME-TIME(1: 4)
                 IF HORA-INIC-L103 > HORA-FIM-L103
                    ADD 2400 TO HORA-FIM-L103
                 END-IF
                 MOVE HORA-FIM-L103      TO GRTIME-TIME-FINAL(1: 4)
                 MOVE 3 TO GRTIME-FUNCTION
                 CALL "GRTIME" USING PARAMETROS-GRTIME
                 MOVE TEMPO-INTERVALO-L103(1: 2) TO HORA-INTERV
                 MOVE TEMPO-INTERVALO-L103(3: 2) TO MIN-INTERV
                 COMPUTE TOTAL-MINUTO =
                  ((GRTIME-TOTAL-HOURS * 60) + GRTIME-TOTAL-MINUTES) -
                  ((HORA-INTERV * 60) + MIN-INTERV)
                 COMPUTE TOT-HORA = TOTAL-MINUTO / 60
                 COMPUTE TOT-MIN = TOTAL-MINUTO - (TOT-HORA * 60)
                 MOVE TOT-HORA TO TEMPO-DECOR-WK(1: 2)
                 MOVE TOT-MIN  TO TEMPO-DECOR-WK(3: 2)

                 MOVE TEMPO-INTERRUPC-L103  TO TEMPO-INTERRUP-WK

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
             WHEN 1 IF GS-CODIGO = FUNCIONARIO-L103
                       MOVE 1 TO GRAVAR-OPCAO
             WHEN 2 IF CODIGO-W = IMPRESSORA-L103
                       MOVE 1 TO GRAVAR-OPCAO
             WHEN 3 IF CODIGO-W = TIPO-FOTO-L103
                       MOVE 1 TO GRAVAR-OPCAO
             WHEN 4 IF CODIGO-W = TIPO-INTERR-L103
                       MOVE 1 TO GRAVAR-OPCAO
           END-EVALUATE.
       VERIFICA-QTDE-PERDAS SECTION.
      *    VERIFICA NO ARQUIVO DE AVALIAÇÃO DE AMPLIAÇÃO(LBD104)
      *    AS FOTOS-PROB-L104 QUE PERTENCEM AO MESMO FUNCIONÁRIO,
      *    TIPO-FOTO E DATA-MOVTO
           MOVE DATA-MOVTO-L103    TO DATA-MOVTO-L104.
           MOVE FUNCIONARIO-L103   TO FUNCIONARIO-L104.
           MOVE TIPO-FOTO-L103     TO TIPO-FOTO-L104
           START LBD104 KEY IS NOT < ALT1-L104 INVALID KEY
                 MOVE "10"  TO ST-LBD104.
           PERFORM UNTIL ST-LBD104 = "10"
             READ LBD104 NEXT RECORD AT END MOVE "10" TO ST-LBD104
               NOT AT END
                 IF DATA-MOVTO-L104 <> DATA-MOVTO-L103 OR
                    FUNCIONARIO-L104 <> FUNCIONARIO-L103 OR
                    TIPO-FOTO-L104 <> TIPO-FOTO-L103
                    MOVE "10" TO ST-LBD104
                 ELSE
                   ADD FOTOS-PROB-L104   TO QT-PERDAS-WK
                 END-IF
             END-READ
           END-PERFORM.

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
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA
           MOVE "TOTAL GERAL: " TO GS-LINDET(1: 30)
           MOVE TOTAL-ROLO-G           TO ROLO-E
           MOVE ROLO-E                 TO GS-LINDET(55: 9)
           MOVE TOTAL-FOTO-G           TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(64: 8)
           MOVE TOTAL-PERDA-G          TO PERDA-E
           MOVE PERDA-E                TO GS-LINDET(72: 6)

      *    calcula total de tempo decorrido
           COMPUTE HOR-W = (TOTAL-TEMPO-MIN-G / 60)
           COMPUTE TOTAL-TEMPO-MIN-G = TOTAL-TEMPO-MIN-G - (HOR-W * 60)
           MOVE TOTAL-TEMPO-MIN-G      TO MIN-E
           COMPUTE TOTAL-TEMPO-HOR-G = HOR-W + TOTAL-TEMPO-HOR-G
           MOVE TOTAL-TEMPO-HOR-G      TO HOR-E
           MOVE TEMPO-E                TO GS-LINDET(78: 8)
      *    calcula total de tempo de interrupção
           COMPUTE HOR-W = (TOTAL-INT-MIN-G / 60)
           COMPUTE TOTAL-INT-MIN-G = TOTAL-INT-MIN-G - (HOR-W * 60)
           MOVE TOTAL-INT-MIN-G      TO MIN-E
           COMPUTE TOTAL-INT-HOR-G = HOR-W + TOTAL-INT-HOR-G
           MOVE TOTAL-INT-HOR-G      TO HOR-E
           MOVE TEMPO-E                TO GS-LINDET(86: 7)

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
                MOVE "IMPRESSORA" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < IMPRESSORA-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "TIPO-FOTO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < TIPO-FOTO-WK INVALID KEY
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
              IF IMPRESSORA-ANT NOT = SPACES
                 IF IMPRESSORA-ANT NOT = IMPRESSORA-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF TIPO-FOTO-ANT NOT = SPACES
                 IF TIPO-FOTO-ANT NOT = TIPO-FOTO-WK
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
           MOVE FUNCIONARIO-WK         TO GS-LINDET(12: 16)
           MOVE IMPRESSORA-WK          TO GS-LINDET(28: 16)
           MOVE TIPO-FOTO-WK           TO GS-LINDET(44: 11)
           MOVE QT-ROLOS-WK            TO ROLO-E
           MOVE ROLO-E                 TO GS-LINDET(55: 9)
           ADD QT-ROLOS-WK             TO TOTAL-ROLO.
           MOVE QT-FOTOS-WK            TO FOTO-E
           ADD QT-FOTOS-WK             TO TOTAL-FOTO.
           MOVE FOTO-E                 TO GS-LINDET(64: 8)
           MOVE QT-PERDAS-WK           TO PERDA-E
           MOVE PERDA-E                TO GS-LINDET(72: 6)
           ADD QT-PERDAS-WK            TO TOTAL-PERDA
           MOVE TEMPO-DECOR-WK(1: 2)   TO HOR-E
           MOVE TEMPO-DECOR-WK(3: 2)   TO MIN-E
           MOVE TEMPO-DECOR-WK         TO TEMPO-W
           ADD HOR-W                   TO TOTAL-TEMPO-HOR
           ADD MIN-W                   TO TOTAL-TEMPO-MIN
           MOVE TEMPO-E                TO GS-LINDET(78: 8)

           MOVE TEMPO-INTERRUP-WK(1: 2) TO HOR-E
           MOVE TEMPO-INTERRUP-WK(3: 2) TO MIN-E
           MOVE TEMPO-INTERRUP-WK       TO TEMPO-W
           ADD HOR-W                    TO TOTAL-INT-HOR
           ADD MIN-W                    TO TOTAL-INT-MIN
           MOVE TEMPO-E                 TO GS-LINDET(86: 7)
           MOVE TIPO-INTER-WK           TO GS-LINDET(94: 15).

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO DATA-MOVTO-ANT.
           MOVE SPACES TO FUNCIONARIO-ANT IMPRESSORA-ANT TIPO-FOTO-ANT
                          TIPO-INTERRUPCAO-ANT.
           MOVE ZEROS TO TOTAL-FOTO-G TOTAL-ROLO-G TOTAL-TEMPO-HOR-G
                         TOTAL-TEMPO-MIN-G TOTAL-INT-HOR-G
                         TOTAL-INT-MIN-G TOTAL-PERDA-G.
           PERFORM ZERA-SUBTOTAL.

       ZERA-SUBTOTAL SECTION.
           MOVE ZEROS TO TOTAL-FOTO TOTAL-ROLO TOTAL-TEMPO-HOR
                         TOTAL-TEMPO-MIN TOTAL-INT-HOR TOTAL-PERDA
                         TOTAL-INT-MIN.

       MOVER-CHAVE-ANT SECTION.
           MOVE DATA-MOVTO-WK          TO DATA-MOVTO-ANT.
           MOVE FUNCIONARIO-WK         TO FUNCIONARIO-ANT.
           MOVE IMPRESSORA-WK          TO IMPRESSORA-ANT
           MOVE TIPO-FOTO-WK           TO TIPO-FOTO-ANT
           MOVE TIPO-INTER-WK          TO TIPO-INTERRUPCAO-ANT.
       TOTALIZA SECTION.
           MOVE SPACES                 TO GS-LINDET
           MOVE TOTAL-ROLO             TO ROLO-E
           MOVE ROLO-E                 TO GS-LINDET(55: 9)
           MOVE TOTAL-FOTO             TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(64: 8)
           MOVE TOTAL-PERDA            TO PERDA-E
           MOVE PERDA-E                TO GS-LINDET(72: 6)

      *    calcula total de tempo decorrido
           COMPUTE HOR-W = (TOTAL-TEMPO-MIN / 60)
           COMPUTE TOTAL-TEMPO-MIN = TOTAL-TEMPO-MIN - (HOR-W * 60)
           MOVE TOTAL-TEMPO-MIN        TO MIN-E
           COMPUTE TOTAL-TEMPO-HOR = HOR-W + TOTAL-TEMPO-HOR
           MOVE TOTAL-TEMPO-HOR        TO HOR-E
           MOVE TEMPO-E                TO GS-LINDET(78: 8)
      *    calcula total de tempo de interrupção
           COMPUTE HOR-W = (TOTAL-INT-MIN / 60)
           COMPUTE TOTAL-INT-MIN = TOTAL-INT-MIN - (HOR-W * 60)
           MOVE TOTAL-INT-MIN          TO MIN-E
           COMPUTE TOTAL-INT-HOR = HOR-W + TOTAL-INT-HOR
           MOVE TOTAL-INT-HOR          TO HOR-E
           MOVE TEMPO-E                TO GS-LINDET(86: 7)

           ADD TOTAL-FOTO      TO TOTAL-FOTO-G
           ADD TOTAL-ROLO      TO TOTAL-ROLO-G
           ADD TOTAL-PERDA     TO TOTAL-PERDA-G
           ADD TOTAL-TEMPO-HOR TO TOTAL-TEMPO-HOR-G
           ADD TOTAL-TEMPO-MIN TO TOTAL-TEMPO-MIN-G
           ADD TOTAL-INT-HOR   TO TOTAL-INT-HOR-G
           ADD TOTAL-INT-MIN   TO TOTAL-INT-MIN-G.

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
           MOVE "LBP202" TO DS-SET-NAME
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
           MOVE TOTAL-ROLO-G           TO ROLO-E
           MOVE ROLO-E                 TO LINDET-REL(55: 9)
           MOVE TOTAL-FOTO-G           TO QTDE-E
           MOVE QTDE-E                 TO LINDET-REL(64: 8)
           MOVE TOTAL-PERDA-G          TO PERDA-E
           MOVE PERDA-E                TO LINDET-REL(72: 6)

      *    calcula total de tempo decorrido
           COMPUTE HOR-W = (TOTAL-TEMPO-MIN-G / 60)
           COMPUTE TOTAL-TEMPO-MIN-G = TOTAL-TEMPO-MIN-G - (HOR-W * 60)
           MOVE TOTAL-TEMPO-MIN-G      TO MIN-E
           COMPUTE TOTAL-TEMPO-HOR-G = HOR-W + TOTAL-TEMPO-HOR-G
           MOVE TOTAL-TEMPO-HOR-G      TO HOR-E
           MOVE TEMPO-E                TO LINDET-REL(78: 8)
      *    calcula total de tempo de interrupção
           COMPUTE HOR-W = (TOTAL-INT-MIN-G / 60)
           COMPUTE TOTAL-INT-MIN-G = TOTAL-INT-MIN-G - (HOR-W * 60)
           MOVE TOTAL-INT-MIN-G      TO MIN-E
           COMPUTE TOTAL-INT-HOR-G = HOR-W + TOTAL-INT-HOR-G
           MOVE TOTAL-INT-HOR-G      TO HOR-E
           MOVE TEMPO-E                TO LINDET-REL(86: 7)

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
              IF IMPRESSORA-ANT NOT = SPACES
                 IF IMPRESSORA-ANT NOT = IMPRESSORA-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF TIPO-FOTO-ANT NOT = SPACES
                 IF TIPO-FOTO-ANT NOT = TIPO-FOTO-WK
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
           MOVE TOTAL-ROLO             TO ROLO-E
           MOVE ROLO-E                 TO LINDET-REL(55: 9)
           MOVE TOTAL-FOTO             TO QTDE-E
           MOVE QTDE-E                 TO LINDET-REL(64: 8)
           MOVE TOTAL-PERDA            TO PERDA-E
           MOVE PERDA-E                TO LINDET-REL(72: 6)

      *    calcula total de tempo decorrido
           COMPUTE HOR-W = (TOTAL-TEMPO-MIN / 60)
           COMPUTE TOTAL-TEMPO-MIN = TOTAL-TEMPO-MIN - (HOR-W * 60)
           MOVE TOTAL-TEMPO-MIN        TO MIN-E
           COMPUTE TOTAL-TEMPO-HOR = HOR-W + TOTAL-TEMPO-HOR
           MOVE TOTAL-TEMPO-HOR        TO HOR-E
           MOVE TEMPO-E                TO LINDET-REL(78: 8)
      *    calcula total de tempo de interrupção
           COMPUTE HOR-W = (TOTAL-INT-MIN / 60)
           COMPUTE TOTAL-INT-MIN = TOTAL-INT-MIN - (HOR-W * 60)
           MOVE TOTAL-INT-MIN        TO MIN-E
           COMPUTE TOTAL-INT-HOR = HOR-W + TOTAL-INT-HOR
           MOVE TOTAL-INT-HOR        TO HOR-E
           MOVE TEMPO-E              TO LINDET-REL(86: 7)

           ADD TOTAL-FOTO      TO TOTAL-FOTO-G
           ADD TOTAL-ROLO      TO TOTAL-ROLO-G
           ADD TOTAL-PERDA     TO TOTAL-PERDA-G
           ADD TOTAL-TEMPO-HOR TO TOTAL-TEMPO-HOR-G
           ADD TOTAL-TEMPO-MIN TO TOTAL-TEMPO-MIN-G
           ADD TOTAL-INT-HOR   TO TOTAL-INT-HOR-G
           ADD TOTAL-INT-MIN   TO TOTAL-INT-MIN-G.

           PERFORM ZERA-SUBTOTAL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.

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
           CLOSE LBD026 LBD027 LBD029 LBD103 LBD104 CGD001.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
