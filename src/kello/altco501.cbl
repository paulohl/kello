       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTCO501.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 20/10/2003
      *DESCRIÇÃO: AUMENTA CAMPO ITEM DE 2 PARA 3 DIGITOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  COD501 ASSIGN TO "\999\COD501"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD501
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO501 =
                      NR-CONTRATO-CO501 ITEM-CO501 SUBITEM-CO501.

           SELECT  COD501A ASSIGN TO "\999\COD501A"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD501A
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO501A =
                      NR-CONTRATO-CO501A ITEM-CO501A SUBITEM-CO501A.



       DATA DIVISION.
       FILE SECTION.

      *   arquivo de contatos do contrato - cont-cod500
       FD  COD501.
       01  REG-COD501.
           05  NR-CONTRATO-CO501  PIC 9(4).
           05  ITEM-CO501         PIC 9(2).
           05  SUBITEM-CO501      PIC 9(2).
           05  ANOTACAO-CO501     PIC X(80).

      *   arquivo de contatos do contrato - cont-cod500
       FD  COD501A.
       01  REG-COD501A.
           05  NR-CONTRATO-CO501A  PIC 9(4).
           05  ITEM-CO501A         PIC 9(3).
           05  SUBITEM-CO501A      PIC 9(2).
           05  ANOTACAO-CO501A     PIC X(80).


       WORKING-STORAGE SECTION.
           COPY "ALTCO501.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD501             PIC XX       VALUE SPACES.
           05  ST-COD501A            PIC XX       VALUE SPACES.

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

           OPEN INPUT COD501.
           OPEN OUTPUT COD501A.

           IF ST-COD501 <> "00"
              MOVE "ERRO ABERTURA COD501: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD501 TO GS-MENSAGEM-ERRO(23: 02)
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
           MOVE ZEROS TO NR-CONTRATO-CO501 ITEM-CO501.

           START COD501 KEY IS NOT < CHAVE-CO501 INVALID KEY
                 MOVE "10" TO ST-COD501.
           PERFORM UNTIL ST-COD501 = "10"
             READ COD501 NEXT RECORD AT END MOVE "10" TO ST-COD501
               NOT AT END
                 MOVE NR-CONTRATO-CO501          TO GS-TALAO
                 MOVE "EXIBE-PROCESSAMENTO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM

                 MOVE NR-CONTRATO-CO501         TO NR-CONTRATO-CO501A
                 MOVE ITEM-CO501                TO ITEM-CO501A
                 MOVE SUBITEM-CO501             TO SUBITEM-CO501A
                 MOVE ANOTACAO-CO501            TO ANOTACAO-CO501A


                  WRITE REG-COD501A
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
           MOVE "ALTCO501" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD501 COD501A.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
