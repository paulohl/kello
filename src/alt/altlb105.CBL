       IDENTIFICATION DIVISION.
       PROGRAM-ID. altlb105.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 18/09/2003
      *DESCRIÇÃO: AUMENTA CAMPO OPERACAO PARA 3 DIGITOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT LBD105 ASSIGN TO "\999\LBD105"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-LBD105
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-L105 = DATA-MOVTO-L105 SEQ-L105
                  ALTERNATE RECORD KEY IS ALT1-L105 = FUNCIONARIO-L105
                            DATA-MOVTO-L105 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-L105 = OPERACAO-L105
                            DATA-MOVTO-L105 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-L105 = TIPO-INTERR-L105
                            DATA-MOVTO-L105 WITH DUPLICATES.

           SELECT LBD105A ASSIGN TO "\999\LBD105A"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-LBD105A
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-L105A = DATA-MOVTO-L105A SEQ-L105A
                  ALTERNATE RECORD KEY IS ALT1-L105A = FUNCIONARIO-L105A
                            DATA-MOVTO-L105A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-L105A = OPERACAO-L105A
                            DATA-MOVTO-L105A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-L105A = TIPO-INTERR-L105A
                            DATA-MOVTO-L105A WITH DUPLICATES.



       DATA DIVISION.
       FILE SECTION.

      * MOVIMENTO DE ATIVIDADES GERAIS DE PRODUÇÃO
       FD  LBD105.
       01  REG-LBD105.
           05  DATA-MOVTO-L105           PIC 9(8).
           05  SEQ-L105                  PIC 9(3).
           05  FUNCIONARIO-L105          PIC 9(6).
           05  TURNO-L105                PIC 9.
           05  OPERACAO-L105             PIC 99.
           05  QTDE-OPER-L105            PIC 9(5).
           05  QT-ALBUM-L105             PIC 9(3).
           05  HORA-INIC-L105            PIC 9(4).
           05  HORA-FIM-L105             PIC 9(4).
           05  TEMPO-INTERRUPC-L105      PIC 9(4).
           05  TIPO-INTERR-L105          PIC 99.
           05  TEMPO-INTERVALO-L105      PIC 9(4).
           05  DIGITADOR-L105            PIC X(4).

      * MOVIMENTO DE ATIVIDADES GERAIS DE PRODUÇÃO
       FD  LBD105A.
       01  REG-LBD105A.
           05  DATA-MOVTO-L105A           PIC 9(8).
           05  SEQ-L105A                  PIC 9(3).
           05  FUNCIONARIO-L105A          PIC 9(6).
           05  TURNO-L105A                PIC 9.
           05  OPERACAO-L105A             PIC 999.
           05  QTDE-OPER-L105A            PIC 9(5).
           05  QT-ALBUM-L105A             PIC 9(3).
           05  HORA-INIC-L105A            PIC 9(4).
           05  HORA-FIM-L105A             PIC 9(4).
           05  TEMPO-INTERRUPC-L105A      PIC 9(4).
           05  TIPO-INTERR-L105A          PIC 99.
           05  TEMPO-INTERVALO-L105A      PIC 9(4).
           05  DIGITADOR-L105A            PIC X(4).



       WORKING-STORAGE SECTION.
           COPY "altlb105.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-LBD105             PIC XX       VALUE SPACES.
           05  ST-LBD105A            PIC XX       VALUE SPACES.

           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  parametros-w          pic x(40)    value spaces.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO.
           OPEN INPUT LBD105.
           OPEN OUTPUT LBD105A.
           IF ST-LBD105 <> "00"
              MOVE "ERRO ABERTURA LBD105: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD105 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-ALTERA-FLG-TRUE
                   PERFORM ALTERA-LAYOUT
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.
       ALTERA-LAYOUT SECTION.
           MOVE ZEROS TO DATA-MOVTO-L105 SEQ-L105.
           START LBD105 KEY IS NOT < CHAVE-L105 INVALID KEY
                 MOVE "10" TO ST-LBD105.
           PERFORM UNTIL ST-LBD105 = "10"
             READ LBD105 NEXT RECORD AT END MOVE "10" TO ST-LBD105
               NOT AT END
                 MOVE SEQ-L105           TO GS-TALAO
                 MOVE "EXIBE-PROCESSAMENTO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                   MOVE DATA-MOVTO-L105             TO  DATA-MOVTO-L105A
                   MOVE SEQ-L105                    TO  SEQ-L105A
                   MOVE FUNCIONARIO-L105           TO  FUNCIONARIO-L105a
                   MOVE TURNO-L105                  TO  TURNO-L105A
                   MOVE OPERACAO-L105               TO  OPERACAO-L105A
                   MOVE QTDE-OPER-L105              TO  QTDE-OPER-L105A
                   MOVE QT-ALBUM-L105               TO  QT-ALBUM-L105A
                   MOVE HORA-INIC-L105              TO  HORA-INIC-L105A
                   MOVE HORA-FIM-L105               TO  HORA-FIM-L105A
                   MOVE TEMPO-INTERRUPC-L105  TO  TEMPO-INTERRUPC-L105A
                   MOVE TIPO-INTERR-L105      TO  TIPO-INTERR-L105A
                   MOVE TEMPO-INTERVALO-L105  TO  TEMPO-INTERVALO-L105A
                   MOVE DIGITADOR-L105        TO  DIGITADOR-L105A

                  WRITE REG-LBD105A
                  END-WRITE
             END-READ
           END-PERFORM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "altlb105" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE LBD105 LBD105A.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
