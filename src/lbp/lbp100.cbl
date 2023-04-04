       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP100.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 25/05/2000.
      *FUNÇÃO: Movimento de RECEBIMENTO DE FILMES


       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX003.
           COPY COPX040.
           COPY CGPX001.
           COPY LBPX021.
           COPY LBPX100.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = DATA-MOVTO-WK SEQ-WK
                  ALTERNATE RECORD KEY IS CONTRATO-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW003.
       COPY COPW040.
       COPY CGPW001.
       COPY LBPW021.
       COPY LBPW100.
       FD  WORK.
       01  REG-WORK.
           05  DATA-MOVTO-WK       PIC 9(8).
           05  SEQ-WK              PIC 9(3).
           05  CONTRATO-WK         PIC 9(4).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP100.CPB".
           COPY "LBP100.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-LBD021             PIC XX       VALUE SPACES.
           05  ST-LBD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  MASC-IDENTIFICADOR    PIC ZZZZZZZZ9 BLANK WHEN ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(39)   VALUE
           "CONFERENCIA-MOVTO RECTO DE FILMES".
           05  FILLER              PIC X(07)   VALUE "MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(07)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(87)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(87)   VALUE
           "SEQ CONT     NR-HD EVE-DESCRICAO  DATA-EVENT FOTOGR-NOME
      -    "   QT-FILME TP-DESCRIAO    ".

       01  LINDET.
           05  LINDET-REL          PIC X(87)   VALUE SPACES.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "LBD021" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD021.
           MOVE "LBD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD100.
           OPEN I-O LBD100.
           OPEN INPUT CGD001 COD003 COD040 LBD021.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.

           IF ST-LBD100 = "35"
              CLOSE LBD100      OPEN OUTPUT LBD100
              CLOSE LBD100      OPEN I-O LBD100
           END-IF.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD021 <> "00"
              MOVE "ERRO ABERTURA LBD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD100 <> "00"
              MOVE "ERRO ABERTURA LBD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      LBD100
           OPEN INPUT LBD100
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1 PERFORM REGRAVA-DADOS
                   ELSE PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                   COPY IMPRESSORA.CHAMA.
                   IF LNK-MAPEAMENTO <> SPACES
                      PERFORM IMPRIME-RELATORIO
                   END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE DATA-MOVTO-I     TO DATA-MOVTO-L100
                   MOVE GS-LINDET(1: 3)  TO SEQ-L100
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
               WHEN GS-LE-CONTRATO-IMP-TRUE
                   PERFORM LE-CONTRATO-IMP
               WHEN GS-LE-EVENTO-TRUE
                   PERFORM LE-EVENTO
               WHEN GS-LE-TIPO-FILME-TRUE
                   PERFORM LE-TIPO-FILME
               WHEN GS-LE-FOTOGRAFO-TRUE
                   PERFORM LE-FOTOGRAFO
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(22: 10) TO GS-DESC-CONTRATO
                    MOVE "-"         TO GS-DESC-CONTRATO(11: 1)
                    MOVE PASSAR-STRING-1(33: 10)
                         TO GS-DESC-CONTRATO(12: 10)
                    MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO
             WHEN 2 CALL   "COP003T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP003T"
                    MOVE PASSAR-STRING-1(1: 20) TO GS-NOME-EVENTO
                    MOVE PASSAR-STRING-1(33: 5) TO GS-EVENTO
             WHEN 3 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-FOTOGRAFO
                    MOVE PASSAR-STRING-1(33: 6) TO GS-FOTOGRAFO
             WHEN 4 CALL   "LBP021T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP021T"
                    MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-TP-FILME
                    MOVE PASSAR-STRING-1(33: 2) TO GS-TIPO-FILME
             WHEN 5 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(22: 10) TO GS-DESC-CONTRATO-IMP
                    MOVE "-"         TO GS-DESC-CONTRATO-IMP(11: 1)
                    MOVE PASSAR-STRING-1(33: 10)
                         TO GS-DESC-CONTRATO-IMP(12: 10)
                    MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO-IMP
           END-EVALUATE.
      *----------------------------------------------------------------
       LE-FOTOGRAFO SECTION.
           MOVE GS-FOTOGRAFO       TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-FOTOGRAFO.
       LE-EVENTO SECTION.
           MOVE GS-EVENTO          TO CODIGO-CO03.
           READ COD003 INVALID KEY MOVE "********" TO NOME-CO03.
           MOVE NOME-CO03          TO GS-NOME-EVENTO.
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE "******" TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO.
       LE-CONTRATO-IMP SECTION.
           MOVE GS-CONTRATO-IMP    TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE "******" TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO-IMP.

       LE-TIPO-FILME SECTION.
           MOVE GS-TIPO-FILME      TO CODIGO-LB21.
           READ LBD021 INVALID KEY MOVE SPACES TO DESCRICAO-LB21.
           MOVE DESCRICAO-LB21     TO GS-DESC-TP-FILME.
      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           START LBD100 KEY IS = CHAVE-L100 INVALID KEY CONTINUE.
           READ LBD100 INVALID KEY INITIALIZE REG-LBD100.
           MOVE DATA-MOVTO-W         TO  GS-DATA-MOVTO
           MOVE CONTRATO-L100        TO  GS-CONTRATO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40   TO GS-DESC-CONTRATO.
           MOVE SEQ-L100             TO  GS-SEQ
           MOVE EVENTO-L100          TO  GS-EVENTO CODIGO-CO03.
           READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03.
           MOVE NOME-CO03            TO  GS-NOME-EVENTO
           MOVE DATA-EVENTO-L100     TO  GS-DATA-EVENTO
           MOVE FOTOGRAFO-L100       TO  GS-FOTOGRAFO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01            TO  GS-NOME-FOTOGRAFO.
           MOVE QTDE-FILMES-L100     TO  GS-QTDE-FILME
           MOVE TIPO-FILME-L100      TO  GS-TIPO-FILME CODIGO-LB21
           READ LBD021 INVALID KEY MOVE SPACES TO DESCRICAO-LB21.
           MOVE DESCRICAO-LB21       TO  GS-DESC-TP-FILME.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-LBD100
           INITIALIZE GS-DATA-BLOCK
           MOVE ULT-SEQ TO GS-SEQ
           MOVE DATA-MOVTO-W TO GS-DATA-MOVTO.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

       EXCLUI SECTION.
           CLOSE      LBD100
           OPEN I-O   LBD100
           DELETE LBD100.
           CLOSE      LBD100
           OPEN INPUT LBD100
           PERFORM LIMPAR-DADOS.
      *    PERFORM CARREGA-ULTIMOS.

       SALVAR-DADOS SECTION.
           MOVE DATA-MOVTO-I           TO DATA-MOVTO-L100
           MOVE GS-SEQ                 TO SEQ-L100
           MOVE GS-CONTRATO            TO CONTRATO-L100
           MOVE GS-EVENTO              TO EVENTO-L100
           MOVE GS-DATA-EVENTO         TO DATA-EVENTO-L100
           MOVE GS-FOTOGRAFO           TO FOTOGRAFO-L100
           MOVE GS-QTDE-FILME          TO QTDE-FILMES-L100
           MOVE GS-TIPO-FILME          TO TIPO-FILME-L100.
           MOVE USUARIO-W              TO DIGITADOR-L100
           MOVE GS-IDENTIFICADOR       TO IDENTIFICADOR-L100.
       GRAVA-DADOS SECTION.
           CLOSE    LBD100
           OPEN I-O LBD100
           MOVE ZEROS TO ST-LBD100.
           PERFORM UNTIL ST-LBD100 = "10"
             WRITE REG-LBD100 INVALID KEY
                 ADD 1 TO SEQ-L100
               NOT INVALID KEY
                 MOVE "10" TO ST-LBD100.
           PERFORM MOVER-DADOS-LISTA.
           CLOSE      LBD100
           OPEN INPUT LBD100
           ADD 1 TO ULT-SEQ.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           CLOSE    LBD100
           OPEN I-O LBD100
           REWRITE REG-LBD100 INVALID KEY
                 MOVE "Erro Regravacao LBD100" TO GS-MENSAGEM-ERRO
                 MOVE ST-LBD100 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
           CLOSE      LBD100
           OPEN INPUT LBD100
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-DATA-MOVTO   TO DATA-MOVTO-W DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV        TO DATA-MOVTO-I DATA-MOVTO-L100.
           MOVE ZEROS           TO SEQ-L100 GS-SEQ.
           MOVE SPACES TO GS-LINDET
           START LBD100 KEY IS NOT < CHAVE-L100
                    INVALID KEY MOVE "10" TO ST-LBD100.
           PERFORM UNTIL ST-LBD100 = "10"
              READ LBD100 NEXT RECORD AT END MOVE "10" TO ST-LBD100
              NOT AT END
                IF DATA-MOVTO-L100 <> DATA-MOVTO-I
                   MOVE "10" TO ST-LBD100
                ELSE
                  MOVE SEQ-L100         TO GS-SEQ
                  PERFORM MOVER-DADOS-LISTA
                  MOVE "INSERE-LIST" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
           ADD 1 TO GS-SEQ.
           MOVE GS-SEQ TO ULT-SEQ.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE SEQ-L100           TO GS-LINDET(1: 4)
           MOVE CONTRATO-L100      TO GS-LINDET(5: 5)
           MOVE IDENTIFICADOR-L100 TO MASC-IDENTIFICADOR
           MOVE MASC-IDENTIFICADOR TO GS-LINDET(10:9)
           MOVE EVENTO-L100        TO GS-LINDET(20: 4) CODIGO-CO03
           READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03.
           MOVE NOME-CO03          TO GS-LINDET(24: 10)
           MOVE DATA-EVENTO-L100   TO DATA-E
           MOVE DATA-E             TO GS-LINDET(35: 11)
           MOVE FOTOGRAFO-L100     TO GS-LINDET(46: 07) CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO GS-LINDET(53: 10)
           MOVE QTDE-FILMES-L100   TO GS-LINDET(64: 9)
           MOVE TIPO-FILME-L100    TO GS-LINDET(73: 3) CODIGO-LB21
           READ LBD021 INVALID KEY MOVE SPACES TO DESCRICAO-LB21.
           MOVE DESCRICAO-LB21     TO GS-LINDET(76: 10).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------
       GRAVA-WORK SECTION.
           CLOSE WORK.   OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE GS-DATA-MOVTO   TO DATA-MOVTO-W DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV        TO DATA-MOVTO-I DATA-MOVTO-L100.
           MOVE ZEROS           TO SEQ-L100.
           START LBD100 KEY IS NOT < CHAVE-L100
                    INVALID KEY MOVE "10" TO ST-LBD100.
           PERFORM UNTIL ST-LBD100 = "10"
              READ LBD100 NEXT RECORD AT END MOVE "10" TO ST-LBD100
              NOT AT END
                IF DATA-MOVTO-L100 <> DATA-MOVTO-I
                   MOVE "10" TO ST-LBD100
                ELSE
                  IF GS-CONTRATO-IMP <> ZEROS
                     IF CONTRATO-L100 <> GS-CONTRATO-IMP
                        CONTINUE
                     ELSE
                      MOVE CONTRATO-L100    TO CONTRATO-WK
                      MOVE DATA-MOVTO-L100  TO DATA-MOVTO-WK
                      MOVE SEQ-L100         TO SEQ-WK
                      WRITE REG-WORK
                      END-WRITE
                     END-IF
                  ELSE
                     MOVE CONTRATO-L100    TO CONTRATO-WK
                     MOVE DATA-MOVTO-L100  TO DATA-MOVTO-WK
                     MOVE SEQ-L100         TO SEQ-WK
                     WRITE REG-WORK
                     END-WRITE
                END-IF
              END-READ
           END-PERFORM.

       IMPRIME-RELATORIO SECTION.
           PERFORM GRAVA-WORK.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT.
           MOVE ZEROS              TO CONTRATO-ANT CONTRATO-WK.
           START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  IF CONTRATO-WK <> CONTRATO-ANT AND
                     CONTRATO-ANT <> ZEROS
                     PERFORM CABECALHO
                  END-IF
                  MOVE CONTRATO-WK   TO CONTRATO-ANT
                  MOVE DATA-MOVTO-WK TO DATA-MOVTO-L100
                  MOVE SEQ-WK        TO SEQ-L100
                  READ LBD100 INVALID KEY INITIALIZE REG-LBD100
                  END-READ
                  PERFORM MOVER-DADOS-REL
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
             END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       MOVER-DADOS-REL SECTION.

           MOVE SPACES             TO LINDET-REL
           MOVE SEQ-L100           TO LINDET-REL(1: 4)
           MOVE CONTRATO-L100      TO LINDET-REL(5: 5)
           MOVE IDENTIFICADOR-L100 TO LINDET-REL(10:9)
           MOVE EVENTO-L100        TO LINDET-REL(20: 4) CODIGO-CO03
           READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03.
           MOVE NOME-CO03          TO LINDET-REL(24: 10)
           MOVE DATA-EVENTO-L100   TO DATA-E
           MOVE DATA-E             TO LINDET-REL(35: 11)
           MOVE FOTOGRAFO-L100     TO LINDET-REL(46: 07) CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO LINDET-REL(53: 10)
           MOVE QTDE-FILMES-L100   TO LINDET-REL(64: 9)
           MOVE TIPO-FILME-L100    TO LINDET-REL(73: 3) CODIGO-LB21
           READ LBD021 INVALID KEY MOVE SPACES TO DESCRICAO-LB21.
           MOVE DESCRICAO-LB21     TO LINDET-REL(76: 15).
       CABECALHO SECTION.
           MOVE DATA-MOVTO-W    TO DATA-MOVTO-REL.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD003 COD040 LBD100 LBD021 CGD001 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
