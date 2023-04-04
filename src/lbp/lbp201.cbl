       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP201.
      *DATA: 31/05/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: ANÁLISE DE FILMES REVELADOS
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
           COPY LBPX021.
           COPY LBPX023.
           COPY LBPX029.
           COPY LBPX101.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS DATA-MOVTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FUNCIONARIO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-INTER-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-FILME-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TURNO-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY LBPW021.
       COPY LBPW023.
       COPY LBPW029.
       COPY LBPW101.
       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(4).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  FUNCIONARIO-WK      PIC X(20).
           05  TURNO-WK            PIC X(10).
           05  QT-FILME-WK         PIC 9(4).
           05  TIPO-FILME-WK       PIC X(15).
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
           COPY "LBP201.CPB".
           COPY "LBP201.CPY".
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
           05  ST-LBD021             PIC XX       VALUE SPACES.
           05  ST-LBD023             PIC XX       VALUE SPACES.
           05  ST-LBD029             PIC XX       VALUE SPACES.
           05  ST-LBD101             PIC XX       VALUE SPACES.
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
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
      *    CONTROLE DE QUEBRA
           05  DATA-MOVTO-ANT        PIC 9(8)     VALUE ZEROS.
           05  FUNCIONARIO-ANT       PIC X(20)    VALUE SPACES.
           05  TURNO-ANT             PIC X(10)    VALUE SPACES.
           05  TIPO-FILME-ANT        PIC X(15)    VALUE SPACES.
           05  TIPO-INTERRUPCAO-ANT  PIC X(15)    VALUE SPACES.
      *    TOTALIZA VARIAVEIS
           05  TOTAL-FILME           PIC 9(8)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZZ.ZZZ.
           05  TOTAL-FILME-G         PIC 9(8)     VALUE ZEROS.
      *    VARIAVEIS P/ CALCULO DE HORAS
           05  TOT-HORA              PIC 9(2)     VALUE ZEROS.
           05  TOT-MIN               PIC 9(2)     VALUE ZEROS.
           05  HORA-INTERV           PIC 9(2)     VALUE ZEROS.
           05  MIN-INTERV            PIC 9(2)     VALUE ZEROS.
           05  TOTAL-MINUTO          PIC 9(5)     VALUE ZEROS.

           05  TEMPOE.
               10  HORA-E            PIC 9(2).
               10  DOIS-PONTOS-E     PIC X VALUE ":".
               10  MIN-E             PIC 9(2).
           05  TEMPO-E REDEFINES TEMPOE PIC X(5).

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(55)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(33)   VALUE
           "ANALISE FILMES REVELADOS- ORDEM: ".
           05  ORDEM-REL           PIC X(18)   VALUE SPACES.
           05  FILLER              PIC X(10)    VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "INT.DATA MOVTO: ".
           05  DATA-INI-REL        PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  DATA-FIM-REL        PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(100) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(100)  VALUE
           "DATA-MOVTO FUNCIONARIO          TURNO      QT-FILME TIPO-FIL
      -    "ME      TEMPO H.INTERR TIPO-INTERRUPC".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(100)  VALUE SPACES.

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
           MOVE "LBD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD021.
           MOVE "LBD023"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD023.
           MOVE "LBD029"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD029.
           MOVE "LBD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD101.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           OPEN INPUT CGD001 LBD021 LBD023 LBD029 LBD101.
           IF ST-LBD021 <> "00"
              MOVE "ERRO ABERTURA LBD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD023 <> "00"
              MOVE "ERRO ABERTURA LBD023: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD023 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD029 <> "00"
              MOVE "ERRO ABERTURA LBD029: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD029 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD101 <> "00"
              MOVE "ERRO ABERTURA LBD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD101 TO GS-MENSAGEM-ERRO(23: 02)
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
           MOVE ZEROS          TO SEQ-L101.
           MOVE DATA-INI       TO DATA-MOVTO-L101.
           MOVE ZEROS          TO SEQ-WK.
           START LBD101 KEY IS NOT < CHAVE-L101 INVALID KEY
                  MOVE "10" TO ST-LBD101.
           PERFORM UNTIL ST-LBD101 = "10"
             READ LBD101 NEXT RECORD AT END MOVE "10" TO ST-LBD101
              NOT AT END
               IF DATA-MOVTO-L101 > DATA-FIM
                  MOVE "10" TO ST-LBD101
               ELSE
                ADD 1                   TO SEQ-WK
                MOVE DATA-MOVTO-L101    TO DATA-MOVTO-WK GS-EXIBE-VENCTO
                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                MOVE FUNCIONARIO-L101   TO CODIGO-CG01
                READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                END-READ
                MOVE NOME-CG01          TO FUNCIONARIO-WK
                MOVE QTDE-FILMES-L101   TO QT-FILME-WK
                MOVE TIPO-FILME-L101    TO CODIGO-LB21
                READ LBD021 INVALID KEY MOVE SPACES TO DESCRICAO-LB21
                END-READ
                MOVE DESCRICAO-LB21     TO TIPO-FILME-WK
                MOVE TIPO-INTERR-L101   TO CODIGO-LB29
                READ LBD029 INVALID KEY MOVE SPACES TO DESCRICAO-LB29
                END-READ
                MOVE DESCRICAO-LB29     TO TIPO-INTER-WK
                MOVE TURNO-L101         TO CODIGO-LB23
                READ LBD023 INVALID KEY MOVE SPACES TO DESCRICAO-LB23
                END-READ
                MOVE DESCRICAO-LB23     TO TURNO-WK
                MOVE ZEROS TO GRTIME-TIME GRTIME-TIME-FINAL
                MOVE HORA-INIC-L101     TO GRTIME-TIME(1: 4)
                IF HORA-INIC-L101 > HORA-FIM-L101
                   ADD 2400 TO HORA-FIM-L101
                END-IF
                MOVE HORA-FIM-L101      TO GRTIME-TIME-FINAL(1: 4)
                MOVE 3 TO GRTIME-FUNCTION
                CALL "GRTIME" USING PARAMETROS-GRTIME
                MOVE TEMPO-INTERVALO-L101(1: 2) TO HORA-INTERV
                MOVE TEMPO-INTERVALO-L101(3: 2) TO MIN-INTERV
                COMPUTE TOTAL-MINUTO =
                  ((GRTIME-TOTAL-HOURS * 60) + GRTIME-TOTAL-MINUTES) -
                  ((HORA-INTERV * 60) + MIN-INTERV)
                COMPUTE TOT-HORA = TOTAL-MINUTO / 60
                COMPUTE TOT-MIN = TOTAL-MINUTO - (TOT-HORA * 60)
                MOVE TOT-HORA TO TEMPO-DECOR-WK(1: 2)
                MOVE TOT-MIN  TO TEMPO-DECOR-WK(3: 2)
                MOVE TEMPO-INTERRUPC-L101  TO TEMPO-INTERRUP-WK

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
           PERFORM ORDEM.
           MOVE ZEROS TO TOTAL-FILME-G TOTAL-FILME.
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
           MOVE TOTAL-FILME-G   TO QTDE-E
           MOVE QTDE-E          TO GS-LINDET(44: 9)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE "REFRESH-DATA" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           INITIALIZE REG-WORK
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
                MOVE "TURNO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < TURNO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "TIPO-FILME" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < TIPO-FILME-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "TIPO-INTERRUP" TO GS-DESCR-ORDEM
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
              IF TIPO-FILME-ANT NOT = SPACES
                 IF TIPO-FILME-ANT NOT = TIPO-FILME-WK
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
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(1: 11)
           MOVE FUNCIONARIO-WK    TO GS-LINDET(12: 21)
           MOVE TURNO-WK          TO GS-LINDET(33: 11)
           MOVE QT-FILME-WK       TO QTDE-E
           ADD QT-FILME-WK        TO TOTAL-FILME.
           MOVE QTDE-E            TO GS-LINDET(44: 9)
           MOVE TIPO-FILME-WK     TO GS-LINDET(53: 16)
           MOVE TEMPO-DECOR-WK(1: 2)    TO HORA-E
           MOVE TEMPO-DECOR-WK(3: 2)    TO MIN-E
           MOVE TEMPO-E                 TO GS-LINDET(69: 6)

           MOVE TEMPO-INTERRUP-WK(1: 2)   TO HORA-E
           MOVE TEMPO-INTERRUP-WK(3: 2)   TO MIN-E
           MOVE TEMPO-E                   TO GS-LINDET(75: 6)
           MOVE TIPO-INTER-WK             TO GS-LINDET(85: 15).

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO DATA-MOVTO-ANT.
           MOVE SPACES TO FUNCIONARIO-ANT TURNO-ANT TIPO-FILME-ANT
                          TIPO-INTERRUPCAO-ANT.
       MOVER-CHAVE-ANT SECTION.
           MOVE DATA-MOVTO-WK     TO DATA-MOVTO-ANT.
           MOVE FUNCIONARIO-WK    TO FUNCIONARIO-ANT.
           MOVE TURNO-WK          TO TURNO-ANT
           MOVE TIPO-FILME-WK     TO TIPO-FILME-ANT
           MOVE TIPO-INTER-WK     TO TIPO-INTERRUPCAO-ANT.
       TOTALIZA SECTION.
           MOVE SPACES        TO GS-LINDET
           MOVE TOTAL-FILME   TO QTDE-E
           MOVE QTDE-E        TO GS-LINDET(44: 9)
           ADD TOTAL-FILME    TO TOTAL-FILME-G
           MOVE ZEROS TO TOTAL-FILME.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP201" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO TOTAL-FILME TOTAL-FILME-G
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
           MOVE "TOTAL GERAL: " TO LINDET-REL(1: 30)
           MOVE TOTAL-FILME-G   TO QTDE-E
           MOVE QTDE-E          TO LINDET-REL(44: 9)
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
              IF TIPO-FILME-ANT NOT = SPACES
                 IF TIPO-FILME-ANT NOT = TIPO-FILME-WK
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
           MOVE SPACES          TO LINDET-REL
           MOVE "TOTAL......: " TO LINDET-REL(1: 30)
           MOVE TOTAL-FILME     TO QTDE-E
           MOVE QTDE-E          TO LINDET-REL(44: 9)
           ADD TOTAL-FILME TO TOTAL-FILME-G
           MOVE ZEROS TO TOTAL-FILME
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
           CLOSE LBD021 LBD023 LBD029 LBD101 CGD001.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
