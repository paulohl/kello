       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAP000.
       AUTHOR: MARELI AMANCIO VOLPATO.
       PROGRAM: ALTERAÇÃO DE SENHA
       DATE: 17/11/1998
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
           COPY "CAP000.CPB".
           COPY "DS-CNTRL.MF".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  PROGRAMA              PIC X(8)    VALUE ZEROS.
           05  command-line          pic x(30).
           05  command-line-len      pic x(4) comp-5.
           05  stack-size            pic x(4) comp-5.
           05  flags                 pic x(4) comp-5.
           05  tty-cmd               pic x(8).
           05  tty-cmd-len           pic x(04) comp-5.
           05  status-code           pic 9(5) comp-5.
       01  run-unit-id               pic x(8) comp-5.
       01  PARAMETROS-USUARIO.
           05  USUARIO-W      PIC X(5).
           05  SENHA-W        PIC 9(4) COMP-3.
           05  IMPRESSORA-W   PIC 99.
           05  COD-USUARIO-W  PIC 9(3).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE ZEROS TO ERRO-W.
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CHAMAR-PROGRAMA-TRUE
                    PERFORM CHAMAR-PROGRAMA
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.
       CHAMAR-PROGRAMA SECTION.
           MOVE GS-PROGRAMA TO command-line.
           MOVE "MARA"      TO USUARIO-W.
           MOVE 2           TO IMPRESSORA-W.
           MOVE 1           TO COD-USUARIO-W.
           MOVE ZEROS       TO SENHA-W.
           move PARAMETROS-USUARIO  to command-line(10: 20)
           move 30                  to command-line-len

      *    CALL GS-PROGRAMA USING PARAMETROS-USUARIO.
           call "CBL_EXEC_RUN_UNIT" using        command-line
                                    by value     command-line-len
                                    by reference run-unit-id
                                    by value     stack-size
                                                 flags
                                    by reference tty-cmd
                                    by value     tty-cmd-len
                                    returning    status-code
           END-CALL.

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
      *  INICIALIZA UMA NOVA PILHA
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "CAP000" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
               IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           STOP RUN.
