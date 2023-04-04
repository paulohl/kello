       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP106.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 19/06/2000.
      *FUNÇÃO: Movimento de AVALIAÇÃO DE FOTOS(MONTAGEM)

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX040.
           COPY CGPX001.
           COPY LBPX030.
           COPY LBPX106.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW040.
       COPY CGPW001.
       COPY LBPW030.
       COPY LBPW106.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP106.CPB".
           COPY "LBP106.CPY".
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
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-LBD030             PIC XX       VALUE SPACES.
           05  ST-LBD106             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  ULT-CONT              PIC 9(4)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
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
           "CONF.MOVTO AVALIACAO FOTOS MONTAGEM".
           05  FILLER              PIC X(07)   VALUE "MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(07)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "SEQ CONT FOTOGR-NOME            FOT-PRO TIP-PROBLEMA
      -    "ALBUM".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.
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
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "LBD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD030.
           MOVE "LBD106" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD106.
           OPEN I-O LBD106.
           OPEN INPUT CGD001 LBD030 COD040.
           IF ST-LBD106 = "35"
              CLOSE LBD106      OPEN OUTPUT LBD106
              CLOSE LBD106      OPEN I-O LBD106
           END-IF.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD030 <> "00"
              MOVE "ERRO ABERTURA LBD030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD106 <> "00"
              MOVE "ERRO ABERTURA LBD106: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD106 TO GS-MENSAGEM-ERRO(23: 02)
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
                   MOVE DATA-MOVTO-I     TO DATA-MOVTO-L106
                   MOVE GS-LINDET(1: 3)  TO SEQ-L106
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
               WHEN GS-LE-PROBLEMA-TRUE
                   PERFORM LE-PROBLEMA
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
             WHEN 2 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-FOTOGRAFO
                    MOVE PASSAR-STRING-1(33: 6) TO GS-FOTOGRAFO
             WHEN 3 CALL   "LBP030T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP030T"
                    MOVE PASSAR-STRING-1(1: 20) TO GS-NOME-PROBLEMA
                    MOVE PASSAR-STRING-1(33: 3) TO GS-TIPO-PROBLEMA
           END-EVALUATE.
      *----------------------------------------------------------------
       LE-FOTOGRAFO SECTION.
           MOVE GS-FOTOGRAFO       TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-FOTOGRAFO.
       LE-PROBLEMA SECTION.
           MOVE GS-TIPO-PROBLEMA   TO CODIGO-LB30.
           READ LBD030 INVALID KEY MOVE SPACES TO DESCRICAO-LB30.
           MOVE DESCRICAO-LB30     TO GS-NOME-PROBLEMA.
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE "******" TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO.
      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           START LBD106 KEY IS = CHAVE-L106 INVALID KEY CONTINUE.
           READ LBD106 INVALID KEY INITIALIZE REG-LBD106.
           MOVE DATA-MOVTO-W         TO  GS-DATA-MOVTO
           MOVE CONTRATO-L106        TO  GS-CONTRATO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40   TO GS-DESC-CONTRATO.
           MOVE SEQ-L106             TO  GS-SEQ
           MOVE TIPO-PROBLEMA-L106   TO GS-TIPO-PROBLEMA CODIGO-LB30.
           READ LBD030 INVALID KEY MOVE SPACES TO DESCRICAO-LB30.
           MOVE DESCRICAO-LB30       TO  GS-NOME-PROBLEMA.
           MOVE FOTOGRAFO-L106       TO  GS-FOTOGRAFO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01            TO  GS-NOME-FOTOGRAFO.
           MOVE QT-FOTOG-PROB-L106   TO  GS-QT-FOTOG-PROB
           EVALUATE ALBUM-L106
             WHEN 0 MOVE "0-Não" TO GS-ALBUM
             WHEN 1 MOVE "1-Sim" TO GS-ALBUM
           END-EVALUATE.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           MOVE GS-CONTRATO  TO ULT-CONT
           INITIALIZE REG-LBD106
           INITIALIZE GS-DATA-BLOCK
           MOVE ULT-SEQ TO GS-SEQ
           MOVE ULT-CONT TO GS-CONTRATO
           MOVE DATA-MOVTO-W TO GS-DATA-MOVTO.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

       EXCLUI SECTION.
           DELETE LBD106.
           PERFORM LIMPAR-DADOS.
      *    PERFORM CARREGA-ULTIMOS.

       SALVAR-DADOS SECTION.
           MOVE DATA-MOVTO-I          TO DATA-MOVTO-L106
           MOVE GS-SEQ                TO SEQ-L106
           MOVE GS-CONTRATO           TO CONTRATO-L106
           MOVE GS-TIPO-PROBLEMA      TO TIPO-PROBLEMA-L106.
           MOVE GS-FOTOGRAFO          TO FOTOGRAFO-L106
           MOVE GS-QT-FOTOG-PROB      TO QT-FOTOG-PROB-L106
           MOVE GS-ALBUM(1: 1)         TO ALBUM-L106
           MOVE USUARIO-W              TO DIGITADOR-L106.
       GRAVA-DADOS SECTION.
           MOVE ZEROS TO ST-LBD106.
           PERFORM UNTIL ST-LBD106 = "10"
             WRITE REG-LBD106 INVALID KEY
                 ADD 1 TO SEQ-L106
               NOT INVALID KEY
                 MOVE "10" TO ST-LBD106.
           PERFORM MOVER-DADOS-LISTA.
           ADD 1 TO ULT-SEQ.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           REWRITE REG-LBD106 INVALID KEY
                 MOVE "Erro Regravacao LBD106" TO GS-MENSAGEM-ERRO
                 MOVE ST-LBD106 TO GS-MENSAGEM-ERRO(24: 5)
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
           MOVE DATA-INV        TO DATA-MOVTO-I DATA-MOVTO-L106.
           MOVE ZEROS           TO SEQ-L106 GS-SEQ.
           START LBD106 KEY IS NOT < CHAVE-L106
                    INVALID KEY MOVE "10" TO ST-LBD106.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-LBD106 = "10"
              READ LBD106 NEXT RECORD AT END MOVE "10" TO ST-LBD106
              NOT AT END
                IF DATA-MOVTO-L106 <> DATA-MOVTO-I
                   MOVE "10" TO ST-LBD106
                ELSE
                   PERFORM MOVER-DADOS-LISTA
                   MOVE SEQ-L106      TO GS-SEQ
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
           ADD 1 TO GS-SEQ.
           MOVE GS-SEQ TO ULT-SEQ.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE SEQ-L106           TO GS-LINDET(1: 4)
           MOVE CONTRATO-L106      TO GS-LINDET(5: 5)
           MOVE FOTOGRAFO-L106     TO GS-LINDET(10: 07) CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO GS-LINDET(17: 15)
           MOVE QT-FOTOG-PROB-L106 TO GS-LINDET(33: 8)
           MOVE TIPO-PROBLEMA-L106      TO GS-LINDET(41: 4) CODIGO-LB30
           READ LBD030 INVALID KEY MOVE SPACES TO DESCRICAO-LB30.
           MOVE DESCRICAO-LB30     TO GS-LINDET(45: 15).
           EVALUATE ALBUM-L106
             WHEN 0 MOVE "0-Não"   TO GS-LINDET(61: 5)
             WHEN 1 MOVE "1-Sim"   TO GS-LINDET(61: 5)
           END-EVALUATE.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP106" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT.
           MOVE DATA-MOVTO-I   TO DATA-MOVTO-L106.
           MOVE ZEROS          TO SEQ-L106.
           START LBD106 KEY IS NOT < CHAVE-L106 INVALID KEY
                 MOVE "10" TO ST-LBD106.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-LBD106 = "10"
             READ LBD106 NEXT RECORD AT END MOVE "10" TO ST-LBD106
              NOT AT END
                IF DATA-MOVTO-L106 <> DATA-MOVTO-I
                         MOVE "10" TO ST-LBD106
                ELSE
                  PERFORM MOVER-DADOS-REL
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
                END-IF
             END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       MOVER-DADOS-REL SECTION.
           MOVE SPACES             TO LINDET-REL
           MOVE SEQ-L106           TO LINDET-REL(1: 4)
           MOVE CONTRATO-L106      TO LINDET-REL(5: 5)
           MOVE FOTOGRAFO-L106     TO LINDET-REL(10: 07) CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO LINDET-REL(17: 15)
           MOVE QT-FOTOG-PROB-L106 TO LINDET-REL(33: 8)
           MOVE TIPO-PROBLEMA-L106 TO LINDET-REL(41: 4) CODIGO-LB30
           READ LBD030 INVALID KEY MOVE SPACES TO DESCRICAO-LB30.
           MOVE DESCRICAO-LB30     TO LINDET-REL(45: 15).
           EVALUATE ALBUM-L106
             WHEN 0 MOVE "0-Não"   TO LINDET-REL(61: 5)
             WHEN 1 MOVE "1-Sim"   TO LINDET-REL(61: 5)
           END-EVALUATE.

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
           CLOSE LBD106 LBD030 CGD001 COD040.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
