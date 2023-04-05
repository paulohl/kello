       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. galho57.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 15-08-2005
      *exclui anotações do contrato
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX500.
           COPY COPX501.

       DATA DIVISION.
       FILE SECTION.
           COPY COPW500.
           COPY COPW501.

       WORKING-STORAGE SECTION.
           COPY "GALHO57.CPB".
           COPY "GALHO57.CPY".

           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-COD500             PIC XX       VALUE SPACES.
           05  ST-COD501             PIC XX       VALUE SPACES.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
      *   tipo de ordem de impressão
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *  IMPRIME-W = 0 (o registro nao faz parte da opcao) e = 1 Sim
           05  ULT-CODIGO            PIC 9(6)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de CADASTRO utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
      *    Grava = 0 (regravar)   grava = 1 (gravar)
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
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

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).


       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO

           MOVE EMPRESA-W          TO EMP-REC

           MOVE "COD500" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD500.
           MOVE "COD501" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD501.
           MOVE ZEROS TO ERRO-W.
           OPEN I-O COD500 COD501
           IF ST-COD500 = "35"
              CLOSE COD500      OPEN OUTPUT COD500
              CLOSE COD500      OPEN I-O COD500
           END-IF.
           IF ST-COD501 = "35"
              CLOSE COD501      OPEN OUTPUT COD501
              CLOSE COD501      OPEN I-O    COD501
           END-IF.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.


       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-APAGA-DADOS-TRUE
                    PERFORM APAGA-DADOS
           END-EVALUATE

           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       LOAD-SCREENSET SECTION.
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "GALHO57" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       APAGA-DADOS SECTION.
           MOVE "Você Deseja Apagar Realmente as Anotações do Contrato?"
           TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              INITIALIZE REG-COD500
              INITIALIZE REG-COD501
              MOVE GS-CONTRATO TO NR-CONTRATO-CO500
              START COD500 KEY IS NOT LESS CHAVE-CO500 INVALID KEY
                  MOVE "10" TO ST-COD500
              END-START
              PERFORM UNTIL ST-COD500 = "10"
                   READ COD500 NEXT AT END
                       MOVE "10" TO ST-COD500
                   NOT AT END
                       IF GS-CONTRATO <> NR-CONTRATO-CO500
                          MOVE "10" TO ST-COD500
                       ELSE
                          DELETE COD500 INVALID KEY
                            MOVE "Erro de Exclusão...COD500" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                          END-DELETE
                       END-IF
                   END-READ
              END-PERFORM

              MOVE GS-CONTRATO TO NR-CONTRATO-CO501
              START COD501 KEY IS NOT LESS CHAVE-CO501 INVALID KEY
                    MOVE "10" TO ST-COD501
              END-START
              PERFORM UNTIL ST-COD501 = "10"
                  READ COD501 NEXT AT END
                      MOVE "10" TO ST-COD501
                  NOT AT END
                      IF GS-CONTRATO <> NR-CONTRATO-CO501
                         MOVE "10" TO ST-COD501
                      ELSE
                         DELETE COD501 INVALID KEY
                            MOVE "Erro de Exclusão...COD501" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                         END-DELETE
                      END-IF
                  END-READ.

       exibir-mensagem section.
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
           CLOSE COD500 COD501
           EXIT PROGRAM.

