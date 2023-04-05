       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRP103.
      *-------------------------------------------------------*
      *    DATA  : 10/12/2000
      *    AUTOR : ALFREDO SAVIOLLI NETO
      *    FUNCAO: IMPRESSÃO DO PLANEJAMENTO
      *-------------------------------------------------------*
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

       FILE-CONTROL.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
           COPY "PRP103.CPB".
           COPY "DS-CNTRL.MF".

       77  DISPLAY-ERROR-NO          PIC 9(4).
       01 VARIAVEIS.
           05 NR-PLAN-W             PIC 9(14)    VALUE ZEROS.
           05 NR-REL                PIC  9999.99.99.9999.99.
           05 ARQUIVO-IMPRESSAO     PIC X(50).
           05 MENSAGEM              PIC X(200).
           05 TIPO-MSG              PIC X(01).
           05 RESP-MSG              PIC X(01).
           05 COMPACTA              PIC X(01).
           05 AUX-CONTRATO          PIC 9(04).
           05 AUX-IMPRESSORA        PIC 9(02).

       01 file-details.
          05 file-size          pic x(8) comp-x.
          05 file-date.
             10 dia             pic x comp-x.
             10 month           pic x comp-x.
             10 year            pic x(2) comp-x.
             10 file-time.
                15 hours        pic x comp-x.
                15 minutes      pic x comp-x.
                15 seconds      pic x comp-x.
                15 hundredths   pic x comp-x.

       01 status-code           PIC X(2) COMP-5.

           COPY "PARAMETR".

       copy impressora.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO

           PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-IMPRIME-RELATORIO-TRUE
                    PERFORM VERIFICA-CODIGO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICA-CODIGO SECTION.
           IF GS-IMPRESSORA(1:1) <> "1" AND "2"
              MOVE "Impressora Informada Inválida" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE GS-ACP-CAMINHO TO ARQUIVO-IMPRESSAO
              MOVE "S" TO COMPACTA
              call "CBL_CHECK_FILE_EXIST" using ARQUIVO-IMPRESSAO
                                                file-details
                                          returning status-code

              if status-code <> "0000"
                 move "Número do Planejamento Informado Inválido" to
                                                                mensagem
                 move "C" to tipo-msg
                 perform exibir-mensagem
              else
                 if gs-impressora(1:1) = "1"
                    move 01 to aux-impressora
                 else
                    move 02 to aux-impressora
                 end-if
                 CALL "PRP102" USING PARAMETROS-W
                                     ARQUIVO-IMPRESSAO COMPACTA
                                     AUX-IMPRESSORA
                 CANCEL "PRP102".

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "PRP103" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.


       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
      *--------------------------------------------------------------
       FINALIZAR-PROGRAMA SECTION.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

