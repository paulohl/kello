       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP080.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 10/09/2001.
      *FUNÇÃO: Movimento de DE outras DESPESAS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX040.
           COPY COPX080.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW040.
       COPY COPW080.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP080.CPB".
           COPY "COP080.CPY".
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
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD080             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
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
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  ITEM-W                PIC 9(2)     VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  QTDE-E                PIC ZZZ.ZZZ.

           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01  CAB01.
           05  EMPRESA-REL         PIC X(85)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(76)   VALUE
           "CONFERENCIA DO MOVIMENTO DE DESPESAS FIXAS".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(105)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(105)  VALUE
           "CONT IT DESCRICAO                      DATA-PREV.    QTDE
      -    " VALOR-PREV DATA-REAL.    QTDE     VALOR-REAL".

       01  LINDET.
           05  LINDET-REL          PIC X(105)  VALUE SPACES.
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
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD080" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD080.
           OPEN I-O COD080.
           OPEN INPUT COD040.
           IF ST-COD080 = "35"
              CLOSE COD080      OPEN OUTPUT COD080
              CLOSE COD080      OPEN I-O COD080
           END-IF.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD080 <> "00"
              MOVE "ERRO ABERTURA COD080: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD080 TO GS-MENSAGEM-ERRO(23: 02)
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
                   IF GS-TIPO-GRAVACAO = 1
                      PERFORM REGRAVA-DADOS
                   ELSE
                      PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
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
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4)  TO NR-CONTRATO-CO80
                   MOVE GS-LINDET(6: 2)  TO ITEM-CO80
                   PERFORM CARREGAR-DADOS
               WHEN GS-VERIFICAR-CONTRATO-TRUE
                    PERFORM VERIFICA-CONTRATO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.
       VERIFICA-CONTRATO SECTION.
           MOVE GS-CONTRATO            TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE "Número do Contrato Não Cadastrado no (COP040)"
                  TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

      *----------------------------------------------------------------
       EXCLUI SECTION.
           DELETE COD080.
           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
       CARREGAR-DADOS SECTION.
           START COD080 KEY IS = CHAVE-CO80 INVALID KEY CONTINUE.
           READ COD080 INVALID KEY INITIALIZE REG-COD080.
           MOVE NR-CONTRATO-CO80     TO  GS-CONTRATO
           MOVE ITEM-CO80            TO  GS-NR-ITEM
           MOVE DESCRICAO-CO80       TO  GS-DESCRICAO
           MOVE DATA-PREV-CO80       TO  DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO  GS-DATA-PREV
           MOVE QTDE-PREV-CO80       TO  GS-QTDE-PREV
           MOVE VALOR-PREV-CO80      TO  GS-VALOR-PREV
           MOVE DATA-REAL-CO80       TO  DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO  GS-DATA-REAL
           MOVE QTDE-REAL-CO80       TO  GS-QTDE-REAL
           MOVE VALOR-REAL-CO80      TO  GS-VALOR-REAL.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-COD080
           MOVE GS-CONTRATO  TO CONTRATO-W
           MOVE GS-NR-ITEM   TO ITEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE CONTRATO-W  TO GS-CONTRATO
           MOVE ITEM-W         TO GS-NR-ITEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO           TO NR-CONTRATO-CO80
           MOVE GS-NR-ITEM            TO ITEM-CO80
           MOVE GS-DESCRICAO          TO DESCRICAO-CO80
           MOVE GS-DATA-PREV          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATA-PREV-CO80
           MOVE GS-QTDE-PREV          TO QTDE-PREV-CO80
           MOVE GS-VALOR-PREV         TO VALOR-PREV-CO80
           MOVE GS-DATA-REAL          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATA-REAL-CO80
           MOVE GS-QTDE-REAL          TO QTDE-REAL-CO80
           MOVE GS-VALOR-REAL         TO VALOR-REAL-CO80.

       GRAVA-DADOS SECTION.
           MOVE ZEROS TO ST-COD080.
           PERFORM UNTIL ST-COD080 = "10"
             WRITE REG-COD080 INVALID KEY
                 ADD 1 TO ITEM-CO80 GS-NR-ITEM
               NOT INVALID KEY
                 MOVE "10" TO ST-COD080.
           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           ADD 1 TO GS-NR-ITEM.

       REGRAVA-DADOS SECTION.
           REWRITE REG-COD080 INVALID KEY
                 MOVE "Erro Regravacao COD080" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD080 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
      *-----------------------------------------------------------
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-CONTRATO     TO NR-CONTRATO-CO80.
           MOVE ZEROS           TO ITEM-CO80 GS-NR-ITEM.
           START COD080 KEY IS NOT < CHAVE-CO80
                    INVALID KEY MOVE "10" TO ST-COD080.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-COD080 = "10"
              READ COD080 NEXT RECORD AT END MOVE "10" TO ST-COD080
              NOT AT END
                IF NR-CONTRATO-CO80 <> GS-CONTRATO
                   MOVE "10" TO ST-COD080
                ELSE
                   PERFORM MOVER-DADOS-LISTA
                   MOVE ITEM-CO80     TO GS-NR-ITEM
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
           ADD 1 TO GS-NR-ITEM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE NR-CONTRATO-CO80   TO GS-LINDET(1: 5)
           MOVE ITEM-CO80          TO GS-LINDET(6: 3)
           MOVE DESCRICAO-CO80     TO GS-LINDET(9: 31)
           MOVE DATA-PREV-CO80     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(40: 11)
           MOVE QTDE-PREV-CO80     TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(51: 8)
           MOVE VALOR-PREV-CO80    TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(59: 14)
           MOVE DATA-REAL-CO80     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(73: 11)
           MOVE QTDE-REAL-CO80     TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(84: 8)
           MOVE VALOR-REAL-CO80    TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(92: 14).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP080" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-CONTRATO    TO NR-CONTRATO-CO80.
           MOVE ZEROS          TO ITEM-CO80.
           START COD080 KEY IS = CHAVE-CO80 INVALID KEY
                 MOVE "10" TO ST-COD080.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-COD080 = "10"
             READ COD080 NEXT RECORD AT END MOVE "10" TO ST-COD080
              NOT AT END
                IF NR-CONTRATO-CO80 <> GS-CONTRATO
                         MOVE "10" TO ST-COD080
                ELSE
                  MOVE NR-CONTRATO-CO80   TO LINDET-REL(1: 5)
                  MOVE ITEM-CO80          TO LINDET-REL(6: 3)
                  MOVE DESCRICAO-CO80     TO LINDET(9: 31)
                  MOVE DATA-PREV-CO80     TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV           TO DATA-E
                  MOVE DATA-E             TO LINDET-REL(40: 11)
                  MOVE QTDE-PREV-CO80     TO QTDE-E
                  MOVE QTDE-E             TO LINDET-REL(51: 8)
                  MOVE VALOR-PREV-CO80    TO VALOR-E
                  MOVE VALOR-E            TO LINDET-REL(59: 14)
                  MOVE DATA-REAL-CO80     TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV           TO DATA-E
                  MOVE DATA-E             TO LINDET-REL(73: 11)
                  MOVE QTDE-REAL-CO80     TO QTDE-E
                  MOVE QTDE-E             TO LINDET-REL(84: 8)
                  MOVE VALOR-REAL-CO80    TO VALOR-E
                  MOVE VALOR-E            TO LINDET-REL(92: 14)
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
                END-IF
             END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       exibir-mensagem section.
           move 1 to gs-flag-critica
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 COD080.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
