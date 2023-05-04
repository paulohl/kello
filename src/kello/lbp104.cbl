       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP104.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 30/05/2000.
      *FUN플O: Movimento de AVALIA플O DE AMPLIA플O


       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY LBPX025.
           COPY LBPX027.
           COPY LBPX104.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY LBPW025.
       COPY LBPW027.
       COPY LBPW104.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP104.CPB".
           COPY "LBP104.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-LBD025             PIC XX       VALUE SPACES.
           05  ST-LBD027             PIC XX       VALUE SPACES.
           05  ST-LBD104             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
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
           "CONFER-MOVTO AVALIACAO AMPLIACAO ".
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
           "SEQ FUNCIO-NOME            TIPO FOTO          QT-FOTO FOT-PR
      -    "OB TIPO-PROBLEMA".

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
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "LBD025" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD025.
           MOVE "LBD027" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD027.
           MOVE "LBD104" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD104.
           OPEN I-O LBD104.
           OPEN INPUT CGD001 LBD025 LBD027.
           IF ST-LBD104 = "35"
              CLOSE LBD104      OPEN OUTPUT LBD104
              CLOSE LBD104      OPEN I-O LBD104
           END-IF.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD025 <> "00"
              MOVE "ERRO ABERTURA LBD025: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD025 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD027 <> "00"
              MOVE "ERRO ABERTURA LBD027: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD027 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD104 <> "00"
              MOVE "ERRO ABERTURA LBD104: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD104 TO GS-MENSAGEM-ERRO(23: 02)
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
                   MOVE DATA-MOVTO-I     TO DATA-MOVTO-L104
                   MOVE GS-LINDET(1: 3)  TO SEQ-L104
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-PROBLEMA-TRUE
                   PERFORM LE-PROBLEMA
               WHEN GS-LE-TIPO-FOTO-TRUE
                   PERFORM LE-TIPO-FOTO
               WHEN GS-LE-FUNCIONARIO-TRUE
                   PERFORM LE-FUNCIONARIO
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
             WHEN 2 CALL   "LBP027T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP027T"
                    MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-TP-FOTO
                    MOVE PASSAR-STRING-1(33: 2) TO GS-TIPO-FOTO
             WHEN 3 CALL   "LBP025T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP025T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-PROBLEMA
                    MOVE PASSAR-STRING-1(33: 3) TO GS-TIPO-PROBLEMA
           END-EVALUATE.
      *----------------------------------------------------------------
       LE-FUNCIONARIO SECTION.
           MOVE GS-FUNCIONARIO     TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-FUNCIONARIO.
       LE-TIPO-FOTO SECTION.
           MOVE GS-TIPO-FOTO       TO CODIGO-LB27.
           READ LBD027 INVALID KEY MOVE SPACES TO DESCRICAO-LB27.
           MOVE DESCRICAO-LB27     TO GS-DESC-TP-FOTO.
       LE-PROBLEMA SECTION.
           MOVE GS-TIPO-PROBLEMA   TO CODIGO-LB25.
           READ LBD025 INVALID KEY MOVE SPACES TO DESCRICAO-LB25.
           MOVE DESCRICAO-LB25     TO GS-DESC-PROBLEMA.
      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           START LBD104 KEY IS = CHAVE-L104 INVALID KEY CONTINUE.
           READ LBD104 INVALID KEY INITIALIZE REG-LBD104.
           MOVE DATA-MOVTO-W         TO GS-DATA-MOVTO
           MOVE SEQ-L104             TO GS-SEQ
           MOVE FUNCIONARIO-L104     TO GS-FUNCIONARIO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01            TO GS-NOME-FUNCIONARIO.
           MOVE QTDE-FOTOS-L104      TO GS-QTDE-FOTO
           MOVE TIPO-FOTO-L104       TO GS-TIPO-FOTO CODIGO-LB27
           READ LBD027 INVALID KEY MOVE SPACES TO DESCRICAO-LB27.
           MOVE DESCRICAO-LB27       TO GS-DESC-TP-FOTO.
           MOVE TIPO-PROBLEMA-L104   TO GS-TIPO-PROBLEMA CODIGO-LB25.
           READ LBD025 INVALID KEY MOVE SPACES TO DESCRICAO-LB25.
           MOVE DESCRICAO-LB25       TO GS-DESC-PROBLEMA.
           MOVE FOTOS-PROB-L104      TO GS-FOTO-PROB.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-LBD104
           INITIALIZE GS-DATA-BLOCK
           MOVE ULT-SEQ TO GS-SEQ
           MOVE DATA-MOVTO-W TO GS-DATA-MOVTO.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

       EXCLUI SECTION.
           DELETE LBD104.
           PERFORM LIMPAR-DADOS.
      *    PERFORM CARREGA-ULTIMOS.

       SALVAR-DADOS SECTION.
           MOVE DATA-MOVTO-I           TO DATA-MOVTO-L104
           MOVE GS-SEQ                 TO SEQ-L104
           MOVE GS-FUNCIONARIO         TO FUNCIONARIO-L104
           MOVE GS-QTDE-FOTO           TO QTDE-FOTOS-L104
           MOVE GS-TIPO-FOTO           TO TIPO-FOTO-L104.
           MOVE USUARIO-W              TO DIGITADOR-L104.
           MOVE GS-FOTO-PROB           TO FOTOS-PROB-L104.
           MOVE GS-TIPO-PROBLEMA       TO TIPO-PROBLEMA-L104.
       GRAVA-DADOS SECTION.
           MOVE ZEROS TO ST-LBD104.
           PERFORM UNTIL ST-LBD104 = "10"
             WRITE REG-LBD104 INVALID KEY
                 ADD 1 TO SEQ-L104
               NOT INVALID KEY
                 MOVE "10" TO ST-LBD104.
           PERFORM MOVER-DADOS-LISTA.
           ADD 1 TO ULT-SEQ.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           REWRITE REG-LBD104 INVALID KEY
                 MOVE "Erro Regravacao LBD104" TO GS-MENSAGEM-ERRO
                 MOVE ST-LBD104 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVA플O" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-DATA-MOVTO   TO DATA-MOVTO-W DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV        TO DATA-MOVTO-I DATA-MOVTO-L104.
           MOVE ZEROS           TO SEQ-L104 GS-SEQ.
           START LBD104 KEY IS NOT < CHAVE-L104
                    INVALID KEY MOVE "10" TO ST-LBD104.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-LBD104 = "10"
              READ LBD104 NEXT RECORD AT END MOVE "10" TO ST-LBD104
              NOT AT END
                IF DATA-MOVTO-L104 <> DATA-MOVTO-I
                   MOVE "10" TO ST-LBD104
                ELSE
                   PERFORM MOVER-DADOS-LISTA
                   MOVE SEQ-L104      TO GS-SEQ
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
           ADD 1 TO GS-SEQ.
           MOVE GS-SEQ TO ULT-SEQ.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE SEQ-L104           TO GS-LINDET(1: 4)
           MOVE FUNCIONARIO-L104   TO GS-LINDET(5: 07) CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO GS-LINDET(12: 15)
           MOVE TIPO-FOTO-L104     TO GS-LINDET(28: 3) CODIGO-LB27
           READ LBD027 INVALID KEY MOVE SPACES TO DESCRICAO-LB27.
           MOVE DESCRICAO-LB27     TO GS-LINDET(31: 15).
           MOVE QTDE-FOTOS-L104    TO GS-LINDET(47: 8)
           MOVE FOTOS-PROB-L104    TO GS-LINDET(55: 9)
           MOVE TIPO-PROBLEMA-L104 TO GS-LINDET(64: 4) CODIGO-LB25.
           READ LBD025 INVALID KEY MOVE SPACES TO DESCRICAO-LB25.
           MOVE DESCRICAO-LB25     TO GS-LINDET(68: 12).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP104" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT.
           MOVE DATA-MOVTO-I   TO DATA-MOVTO-L104.
           MOVE ZEROS          TO SEQ-L104.
           START LBD104 KEY IS NOT < CHAVE-L104 INVALID KEY
                 MOVE "10" TO ST-LBD104.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-LBD104 = "10"
             READ LBD104 NEXT RECORD AT END MOVE "10" TO ST-LBD104
              NOT AT END
                IF DATA-MOVTO-L104 <> DATA-MOVTO-I
                         MOVE "10" TO ST-LBD104
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
           MOVE SEQ-L104           TO LINDET-REL(1: 4)
           MOVE FUNCIONARIO-L104   TO LINDET-REL(5: 07) CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO LINDET-REL(12: 15)
           MOVE TIPO-FOTO-L104     TO LINDET-REL(28: 3) CODIGO-LB27
           READ LBD027 INVALID KEY MOVE SPACES TO DESCRICAO-LB27.
           MOVE DESCRICAO-LB27     TO LINDET-REL(31: 15).
           MOVE QTDE-FOTOS-L104    TO LINDET-REL(47: 8)
           MOVE FOTOS-PROB-L104    TO LINDET-REL(55: 9)
           MOVE TIPO-PROBLEMA-L104 TO LINDET-REL(64: 4) CODIGO-LB25.
           READ LBD025 INVALID KEY MOVE SPACES TO DESCRICAO-LB25.
           MOVE DESCRICAO-LB25     TO LINDET-REL(68: 12).

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
           CLOSE LBD104 LBD025 LBD027 CGD001.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
