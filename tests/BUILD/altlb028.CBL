       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTLB028.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 18/09/2003
      *DESCRIÇÃO: AUMENTA CAMPO OPERACAO PARA 3 DIGITOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT LBD028 ASSIGN TO "\999\LBD028"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-LBD028
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-LB28
                  ALTERNATE RECORD KEY IS DESCRICAO-LB28
                               WITH DUPLICATES.

           SELECT LBD028A ASSIGN TO "\999\LBD028A"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-LBD028A
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-LB28A
                  ALTERNATE RECORD KEY IS DESCRICAO-LB28A
                               WITH DUPLICATES.


       DATA DIVISION.
       FILE SECTION.

      *CADASTRO DE OPERAÇÕES
       FD  LBD028.
       01  REG-LBD028.
           05  CODIGO-LB28              PIC 9(02).
           05  DESCRICAO-LB28           PIC X(30).

      *CADASTRO DE OPERAÇÕES
       FD  LBD028A.
       01  REG-LBD028A.
           05  CODIGO-LB28A              PIC 9(03).
           05  DESCRICAO-LB28A           PIC X(30).

       WORKING-STORAGE SECTION.
           COPY "ALTLB028.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-LBD028             PIC XX       VALUE SPACES.
           05  ST-LBD028A            PIC XX       VALUE SPACES.

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
           OPEN INPUT LBD028.
           OPEN OUTPUT LBD028A.
           IF ST-LBD028 <> "00"
              MOVE "ERRO ABERTURA LBD028: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD028 TO GS-MENSAGEM-ERRO(23: 02)
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
           MOVE ZEROS TO CODIGO-LB28.
           START LBD028 KEY IS NOT < CODIGO-LB28 INVALID KEY
                 MOVE "10" TO ST-LBD028.
           PERFORM UNTIL ST-LBD028 = "10"
             READ LBD028 NEXT RECORD AT END MOVE "10" TO ST-LBD028
               NOT AT END
                 MOVE CODIGO-LB28        TO GS-TALAO
                 MOVE "EXIBE-PROCESSAMENTO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                  MOVE CODIGO-LB28       TO CODIGO-LB28A
                  MOVE DESCRICAO-LB28    TO DESCRICAO-LB28A

                  WRITE REG-LBD028A
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
           MOVE "ALTLB028" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE LBD028 LBD028A.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
