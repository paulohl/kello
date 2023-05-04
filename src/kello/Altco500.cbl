       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTCO500.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 20/10/2003
      *DESCRIÇÃO: AUMENTA CAMPO ITEM DE 2 PARA 3 DIGITOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  COD500 ASSIGN TO "\999\COD500"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD500
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO500 =
                      NR-CONTRATO-CO500 ITEM-CO500
                   ALTERNATE RECORD KEY IS ALT2-CO500 =
                      DATA-AGENDA-CO500 USUARIO-CO500 WITH DUPLICATES.

           SELECT  COD500A ASSIGN TO "\999\COD500A"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD500A
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO500A =
                      NR-CONTRATO-CO500A ITEM-CO500A
                   ALTERNATE RECORD KEY IS ALT2-CO500A =
                      DATA-AGENDA-CO500A USUARIO-CO500A WITH DUPLICATES.




       DATA DIVISION.
       FILE SECTION.

      *  arquivo de contatos do contrato - cabecalho
       FD  COD500.
       01  REG-COD500.
           05  NR-CONTRATO-CO500  PIC 9(4).
           05  ITEM-CO500         PIC 9(2).
           05  DATA-CO500         PIC 9(8).
           05  HORA-CO500         PIC 9(4).
           05  USUARIO-CO500      PIC X(5).
           05  DATA-AGENDA-CO500  PIC 9(8).
      *    DATA-AGENDA = AAAA/MM/DD
           05  SITUACAO-CO500     PIC 9.
      *    0-PENDENTE   1-CHECADO

      *  arquivo de contatos do contrato - cabecalho
       FD  COD500A.
       01  REG-COD500A.
           05  NR-CONTRATO-CO500A  PIC 9(4).
           05  ITEM-CO500A         PIC 9(3).
           05  DATA-CO500A         PIC 9(8).
           05  HORA-CO500A         PIC 9(4).
           05  USUARIO-CO500A      PIC X(5).
           05  DATA-AGENDA-CO500A  PIC 9(8).
      *    DATA-AGENDA = AAAA/MM/DD
           05  SITUACAO-CO500A     PIC 9.
      *    0-PENDENTE   1-CHECADO


       WORKING-STORAGE SECTION.
           COPY "ALTCO500.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD500             PIC XX       VALUE SPACES.
           05  ST-COD500A            PIC XX       VALUE SPACES.

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

           OPEN INPUT COD500.
           OPEN OUTPUT COD500A.

           IF ST-COD500 <> "00"
              MOVE "ERRO ABERTURA COD500: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD500 TO GS-MENSAGEM-ERRO(23: 02)
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
           MOVE ZEROS TO NR-CONTRATO-CO500 ITEM-CO500.

           START COD500 KEY IS NOT < CHAVE-CO500 INVALID KEY
                 MOVE "10" TO ST-COD500.
           PERFORM UNTIL ST-COD500 = "10"
             READ COD500 NEXT RECORD AT END MOVE "10" TO ST-COD500
               NOT AT END
                 MOVE NR-CONTRATO-CO500            TO GS-TALAO
                 MOVE "EXIBE-PROCESSAMENTO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM

                 MOVE NR-CONTRATO-CO500         TO NR-CONTRATO-CO500A
                 MOVE ITEM-CO500                TO ITEM-CO500A
                 MOVE DATA-CO500                TO DATA-CO500A
                 MOVE HORA-CO500                TO HORA-CO500A
                 MOVE USUARIO-CO500             TO USUARIO-CO500A
                 MOVE DATA-AGENDA-CO500         TO DATA-AGENDA-CO500A
                 MOVE SITUACAO-CO500            TO SITUACAO-CO500A


                  WRITE REG-COD500A
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
           MOVE "ALTCO500" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD500 COD500A.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
