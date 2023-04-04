       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP105.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 30/05/2000.
      *FUNÇÃO: Movimento de ATIVIDADES GERAIS DE PRODUÇÃO

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

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY LBPW023.
       COPY LBPW028.
       COPY LBPW029.
       COPY LBPW105.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP105.CPB".
           COPY "LBP105.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-LBD023             PIC XX       VALUE SPACES.
           05  ST-LBD028             PIC XX       VALUE SPACES.
           05  ST-LBD029             PIC XX       VALUE SPACES.
           05  ST-LBD105             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  ULT-FUNC              PIC 9(6)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
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
           05  QTDE-E                PIC ZZZ,ZZZ  BLANK WHEN ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(90)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(69)   VALUE
           "CONFERENCIA-MOVTO DE ATIVIDADES GERAIS DE PRODUCAO".
           05  FILLER              PIC X(07)   VALUE "MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(07)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "SEQ FUNCIO-NOME         TURNO        OPERACAO      QT-OPER
      -    "QT-ALB HR-INI HR-FIM INTERRUP TP-INTERRUPCAO INTER".

       01  LINDET.
           05  LINDET-REL          PIC X(110)   VALUE SPACES.
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
           MOVE "LBD023" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD023.
           MOVE "LBD028" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD028.
           MOVE "LBD029" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD029.
           MOVE "LBD105" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD105.
           OPEN I-O LBD105.
           OPEN INPUT CGD001 LBD023 LBD028 LBD029.
           IF ST-LBD105 = "35"
              CLOSE LBD105      OPEN OUTPUT LBD105
              CLOSE LBD105      OPEN I-O LBD105
           END-IF.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
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
                   MOVE DATA-MOVTO-I     TO DATA-MOVTO-L105
                   MOVE GS-LINDET(1: 3)  TO SEQ-L105
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-TURNO-TRUE
                   PERFORM LE-TURNO
               WHEN GS-LE-INTERRUPCAO-TRUE
                   PERFORM LE-INTERRUPCAO
               WHEN GS-LE-FUNCIONARIO-TRUE
                   PERFORM LE-FUNCIONARIO
               WHEN GS-LE-OPERACAO-TRUE
                   PERFORM LE-OPERACAO
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
             WHEN 1 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-FUNCIONARIO
                    MOVE PASSAR-STRING-1(33: 6) TO GS-FUNCIONARIO
             WHEN 2 CALL   "LBP023T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP023T"
                    MOVE PASSAR-STRING-1(1: 15) TO GS-DESC-TURNO
                    MOVE PASSAR-STRING-1(33: 1) TO GS-TURNO
             WHEN 3 CALL   "LBP028T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP028T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-OPERACAO
                    MOVE PASSAR-STRING-1(33: 3) TO GS-OPERACAO
                    IF GS-OPERACAO = 15 OR 16 OR 19 OR 47 OR 62
                       MOVE "HABILITA-QT-ALBUM" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                    END-IF
             WHEN 4 CALL   "LBP029T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP029T"
                    MOVE PASSAR-STRING-1(1: 20) TO GS-NOME-INTERRUPCAO
                    MOVE PASSAR-STRING-1(33: 2) TO GS-TIPO-INTERRUPCAO
           END-EVALUATE.
      *----------------------------------------------------------------
       LE-FUNCIONARIO SECTION.
           MOVE GS-FUNCIONARIO       TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-FUNCIONARIO.
       LE-TURNO SECTION.
           MOVE GS-TURNO           TO CODIGO-LB23.
           READ LBD023 INVALID KEY MOVE SPACES TO DESCRICAO-LB23.
           MOVE DESCRICAO-LB23     TO GS-DESC-TURNO.
       LE-INTERRUPCAO SECTION.
           MOVE GS-TIPO-INTERRUPCAO TO CODIGO-LB29.
           READ LBD029 INVALID KEY  MOVE SPACES TO DESCRICAO-LB29.
           MOVE DESCRICAO-LB29      TO GS-NOME-INTERRUPCAO.
       LE-OPERACAO SECTION.
           MOVE GS-OPERACAO        TO CODIGO-LB28.
           READ LBD028 INVALID KEY MOVE SPACES TO DESCRICAO-LB28.
           MOVE DESCRICAO-LB28     TO GS-DESC-OPERACAO.
           IF GS-OPERACAO = 15 OR 16 OR 19 OR 47 OR 62
              MOVE "HABILITA-QT-ALBUM" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.
      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           START LBD105 KEY IS = CHAVE-L105 INVALID KEY CONTINUE.
           READ LBD105 INVALID KEY INITIALIZE REG-LBD105.
           MOVE DATA-MOVTO-W         TO  GS-DATA-MOVTO
           MOVE SEQ-L105             TO  GS-SEQ
           MOVE TURNO-L105           TO  GS-TURNO CODIGO-LB23.
           READ LBD023 INVALID KEY MOVE SPACES TO DESCRICAO-LB23.
           MOVE DESCRICAO-LB23       TO  GS-DESC-TURNO.
           MOVE TIPO-INTERR-L105     TO  GS-TIPO-INTERRUPCAO
                                         CODIGO-LB29.
           READ LBD029 INVALID KEY MOVE SPACES TO DESCRICAO-LB29.
           MOVE DESCRICAO-LB29       TO  GS-NOME-INTERRUPCAO.
           MOVE FUNCIONARIO-L105     TO  GS-FUNCIONARIO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01            TO  GS-NOME-FUNCIONARIO.
           MOVE OPERACAO-L105        TO  GS-OPERACAO CODIGO-LB28
           READ LBD028 INVALID KEY MOVE SPACES TO DESCRICAO-LB28.
           MOVE DESCRICAO-LB28       TO  GS-DESC-OPERACAO.
           MOVE QTDE-OPER-L105       TO  GS-QTDE-OPERACAO
           MOVE QT-ALBUM-L105        TO  GS-QT-ALBUM
           MOVE HORA-INIC-L105       TO  GS-HORA-INI
           MOVE HORA-FIM-L105        TO  GS-HORA-FIM
           MOVE TEMPO-INTERRUPC-L105 TO  GS-TEMPO-INTERRUP
           MOVE TEMPO-INTERVALO-L105 TO  GS-TEMPO-INTERVALO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           MOVE GS-FUNCIONARIO TO ULT-FUNC.
           INITIALIZE REG-LBD105
           INITIALIZE GS-DATA-BLOCK
           MOVE ULT-SEQ TO GS-SEQ
           MOVE ULT-FUNC TO GS-FUNCIONARIO
           MOVE DATA-MOVTO-W TO GS-DATA-MOVTO.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

       EXCLUI SECTION.
           DELETE LBD105.
           PERFORM LIMPAR-DADOS.

       SALVAR-DADOS SECTION.
           MOVE DATA-MOVTO-I           TO DATA-MOVTO-L105
           MOVE GS-SEQ                 TO SEQ-L105
           MOVE GS-TURNO               TO TURNO-L105
           MOVE GS-FUNCIONARIO         TO FUNCIONARIO-L105.
           MOVE GS-QTDE-OPERACAO       TO QTDE-OPER-L105.
           MOVE GS-OPERACAO            TO OPERACAO-L105.
           MOVE GS-QT-ALBUM            TO QT-ALBUM-L105
           MOVE GS-HORA-INI            TO HORA-INIC-L105
           MOVE GS-HORA-FIM            TO HORA-FIM-L105.
           MOVE GS-TIPO-INTERRUPCAO    TO TIPO-INTERR-L105.
           MOVE GS-TEMPO-INTERRUP      TO TEMPO-INTERRUPC-L105
           MOVE GS-TEMPO-INTERVALO     TO TEMPO-INTERVALO-L105.
           MOVE USUARIO-W              TO DIGITADOR-L105.
       GRAVA-DADOS SECTION.
           MOVE ZEROS TO ST-LBD105.
           PERFORM UNTIL ST-LBD105 = "10"
             WRITE REG-LBD105 INVALID KEY
                 ADD 1 TO SEQ-L105
               NOT INVALID KEY
                 MOVE "10" TO ST-LBD105.
           PERFORM MOVER-DADOS-LISTA.
           ADD 1 TO ULT-SEQ.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           REWRITE REG-LBD105 INVALID KEY
                 MOVE "Erro Regravacao LBD105" TO GS-MENSAGEM-ERRO
                 MOVE ST-LBD105 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
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
           MOVE DATA-INV        TO DATA-MOVTO-I DATA-MOVTO-L105.
           MOVE ZEROS           TO SEQ-L105 GS-SEQ.
           START LBD105 KEY IS NOT < CHAVE-L105
                    INVALID KEY MOVE "10" TO ST-LBD105.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-LBD105 = "10"
              READ LBD105 NEXT RECORD AT END MOVE "10" TO ST-LBD105
              NOT AT END
                IF DATA-MOVTO-L105 <> DATA-MOVTO-I
                   MOVE "10" TO ST-LBD105
                ELSE
                   PERFORM MOVER-DADOS-LISTA
                   MOVE SEQ-L105      TO GS-SEQ
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
           ADD 1 TO GS-SEQ.
           MOVE GS-SEQ TO ULT-SEQ.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE SEQ-L105           TO GS-LINDET(1: 4)
           MOVE FUNCIONARIO-L105   TO GS-LINDET(5: 07) CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO GS-LINDET(12: 12)
           MOVE TURNO-L105         TO GS-LINDET(25: 2) CODIGO-LB23
           READ LBD023 INVALID KEY MOVE SPACES TO DESCRICAO-LB23.
           MOVE DESCRICAO-LB23     TO GS-LINDET(27: 10)
           MOVE OPERACAO-L105      TO GS-LINDET(38: 4) CODIGO-LB28
           READ LBD028 INVALID KEY MOVE SPACES TO DESCRICAO-LB28.
           MOVE DESCRICAO-LB28     TO GS-LINDET(42: 09)
           MOVE QTDE-OPER-L105     TO GS-LINDET(52: 8)
           MOVE QT-ALBUM-L105      TO GS-LINDET(61: 7)
           MOVE HORA-INIC-L105     TO GS-LINDET(68: 7)
           MOVE HORA-FIM-L105      TO GS-LINDET(75: 7)
           MOVE TEMPO-INTERRUPC-L105  TO GS-LINDET(82: 9)
           MOVE TIPO-INTERR-L105   TO GS-LINDET(91: 3) CODIGO-LB29.
           READ LBD029 INVALID KEY MOVE SPACES TO DESCRICAO-LB29.
           MOVE DESCRICAO-LB29     TO GS-LINDET(94: 11)
           MOVE TEMPO-INTERVALO-L105 TO GS-LINDET(106: 4).
      *-------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP105" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE DATA-MOVTO-I   TO DATA-MOVTO-L105.
           MOVE ZEROS          TO SEQ-L105.
           START LBD105 KEY IS NOT < CHAVE-L105 INVALID KEY
                 MOVE "10" TO ST-LBD105.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-LBD105 = "10"
             READ LBD105 NEXT RECORD AT END MOVE "10" TO ST-LBD105
              NOT AT END
                IF DATA-MOVTO-L105 <> DATA-MOVTO-I
                         MOVE "10" TO ST-LBD105
                ELSE
                  PERFORM MOVER-DADOS-REL
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
                END-IF
             END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       MOVER-DADOS-REL SECTION.
           MOVE SPACES             TO LINDET-REL
           MOVE SEQ-L105           TO LINDET-REL(1: 4)
           MOVE FUNCIONARIO-L105   TO LINDET-REL(5: 07) CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO LINDET-REL(12: 12)
           MOVE TURNO-L105         TO LINDET-REL(25: 2) CODIGO-LB23
           READ LBD023 INVALID KEY MOVE SPACES TO DESCRICAO-LB23.
           MOVE DESCRICAO-LB23     TO LINDET-REL(27: 10)
           MOVE OPERACAO-L105      TO LINDET-REL(38: 4) CODIGO-LB28
           READ LBD028 INVALID KEY MOVE SPACES TO DESCRICAO-LB28.
           MOVE DESCRICAO-LB28     TO LINDET-REL(42: 9)
           MOVE QTDE-OPER-L105     TO LINDET-REL(52: 8)
           MOVE QT-ALBUM-L105      TO LINDET-REL(61: 7)
           MOVE HORA-INIC-L105     TO LINDET-REL(68: 7)
           MOVE HORA-FIM-L105      TO LINDET-REL(75: 7)
           MOVE TEMPO-INTERRUPC-L105  TO LINDET-REL(82: 9)
           MOVE TIPO-INTERR-L105   TO LINDET-REL(91: 3) CODIGO-LB29.
           READ LBD029 INVALID KEY MOVE SPACES TO DESCRICAO-LB29.
           MOVE DESCRICAO-LB29     TO LINDET-REL(94: 11)
           MOVE TEMPO-INTERVALO-L105 TO LINDET-REL(106: 4).

       CABECALHO SECTION.
           MOVE DATA-MOVTO-W    TO DATA-MOVTO-REL.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
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
           CLOSE CGD001 LBD023 LBD028 LBD029 LBD105.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
