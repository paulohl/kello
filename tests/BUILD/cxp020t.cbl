       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP020T.
      * AUTORA: MARELI AMÂNCIO VOLPATO
      * DATA: 28/08/1998
      * FUNCAO: CONSULTA POP-UP CONTAS APURACAO DE RESULTADOS
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CXPX020.

       DATA DIVISION.
       FILE SECTION.
       COPY CXPW020.
       WORKING-STORAGE SECTION.
           COPY "CXP020T.CPB".
           COPY "CXP020T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(5)   VALUE SPACES.
           05  CODIGO-E              PIC 9.99.99.99.

       01 TAB-PALAVRAS               PIC X(60).
       01 TAB-PALAVRAS-R REDEFINES TAB-PALAVRAS OCCURS 60 TIMES.
          05 TAB-PALAVRA             PIC X(01).

       01 TAB-PALAVRAS2              PIC X(60).
       01 TAB-PALAVRAS2-R REDEFINES TAB-PALAVRAS2 OCCURS 60 TIMES.
          05 TAB-PALAVRA2            PIC X(01).

       01 IND                        PIC 9(02).
       01 IND2                       PIC 9(02).
       01 IND3                       PIC 9(02).
       01 TAMANHO-PALAVRA            PIC 9(02).
       01 ACHEI                      PIC X(01).

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       LINKAGE SECTION.
       01  STRING-1               PIC X(65) VALUE SPACES.

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP020T-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE "00000" TO STRING-1(52: 5).
           INITIALIZE CXP020T-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP020T-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP020T-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           OPEN INPUT CXD020
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CXP020T-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CXP020T-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP020T-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CXP020T-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN CXP020T-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN CXP020T-LOCALIZA-TRUE
                    PERFORM LOCALIZA-PALAVRA
               WHEN CXP020T-PROXIMA-TRUE
                    PERFORM LOCALIZA-PROXIMA-PALAVRA
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LOCALIZA-PALAVRA SECTION.
           IF CXP020T-LOCALIZACAO <> SPACES
              MOVE CXP020T-LOCALIZACAO            TO TAB-PALAVRAS

              MOVE 0 TO IND
                        TAMANHO-PALAVRA
              PERFORM UNTIL IND = 60
                  ADD 1 TO IND
                  IF TAB-PALAVRA(IND) <> SPACES
                     ADD 1 TO TAMANHO-PALAVRA
                  ELSE
                     IF TAB-PALAVRA(IND + 1) <> SPACES
                        ADD 1 TO TAMANHO-PALAVRA
                     ELSE
                        MOVE 60 TO IND
                     END-IF
                  END-IF
              END-PERFORM

              MOVE "N" TO ACHEI
              MOVE 1   TO CXP020T-CONT
              MOVE SPACES TO CXP020T-LINDET
              MOVE "LER-APURACAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM

              PERFORM UNTIL CXP020T-LINDET = SPACES OR ACHEI = "S"
                  MOVE CXP020T-LINDET TO TAB-PALAVRAS2
                  PERFORM PROCURAR-PALAVRA
                  IF ACHEI = "N"
                     ADD 1 TO CXP020T-CONT
                     MOVE SPACES TO CXP020T-LINDET
                     MOVE "LER-APURACAO" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  ELSE
                     MOVE "SELECIONAR-APURACAO" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-PERFORM

              IF ACHEI = "N"
                 MOVE "Nenhuma Palavra Encontrada" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              END-IF
           ELSE
              MOVE "Nenhuma Palavra Informada para Busca" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       PROCURAR-PALAVRA SECTION.
           MOVE 0 TO IND
           PERFORM UNTIL IND = 60 OR ACHEI = "S"
               ADD 1 TO IND
               IF TAB-PALAVRA2(IND) = TAB-PALAVRA(1)
                  MOVE "S"   TO ACHEI
                  MOVE IND   TO IND2
                  MOVE ZEROS TO IND3
                  PERFORM UNTIL IND2 = (IND + TAMANHO-PALAVRA) OR
                                IND2 = 60 OR ACHEI = "N"
                       ADD 1 TO IND3

                       IF TAB-PALAVRA(IND3) = TAB-PALAVRA2(IND2)
                          MOVE "S" TO ACHEI
                       ELSE
                          MOVE "N" TO ACHEI
                       END-IF

                       ADD 1 TO IND2
                  END-PERFORM
               END-IF
           END-PERFORM.

       LOCALIZA-PROXIMA-PALAVRA SECTION.
           IF CXP020T-LOCALIZACAO <> SPACES
              MOVE "DESMARCAR-APURACAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM

              MOVE "N" TO ACHEI
              ADD 1    TO CXP020T-CONT
              MOVE SPACES TO CXP020T-LINDET
              MOVE "LER-APURACAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM

              PERFORM UNTIL CXP020T-LINDET = SPACES OR ACHEI = "S"
                  MOVE CXP020T-LINDET TO TAB-PALAVRAS2
                  PERFORM PROCURAR-PALAVRA
                  IF ACHEI = "N"
                     ADD 1 TO CXP020T-CONT
                     MOVE SPACES TO CXP020T-LINDET
                     MOVE "LER-APURACAO" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  ELSE
                     MOVE "SELECIONAR-APURACAO" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-PERFORM

              IF ACHEI = "N"
                 MOVE "Nenhuma Palavra Encontrada" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              END-IF
           ELSE
              MOVE "Nenhuma Palavra Informada para Busca" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO CXP020T-CONT.
           MOVE ZEROS TO CODIGO-COMPL-CX20.
              START CXD020 KEY IS NOT < CODIGO-COMPL-CX20
                    INVALID KEY MOVE "10" TO ST-CXD020.
           MOVE SPACES TO CXP020T-LINDET.
           PERFORM UNTIL ST-CXD020 = "10"
              READ CXD020 NEXT RECORD AT END MOVE "10" TO ST-CXD020
              NOT AT END
                MOVE SPACES TO CXP020T-LINDET
                MOVE CODIGO-COMPL-CX20 TO CODIGO-E
                EVALUATE GRAU-CX20
                  WHEN 1 PERFORM GRAU-1
                  WHEN 2 PERFORM GRAU-2
                  WHEN 3 PERFORM GRAU-3
                  WHEN 4 PERFORM GRAU-4
                END-EVALUATE
                ADD 1 TO CXP020T-CONT
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       GRAU-1 SECTION.
           MOVE CODIGO-E          TO CXP020T-LINDET(1: 11)
           MOVE DESCRICAO-CX20    TO CXP020T-LINDET(12: 31)
           MOVE CODIGO-REDUZ-CX20 TO CXP020T-LINDET(52: 05).
       GRAU-2 SECTION.
           MOVE CODIGO-E          TO CXP020T-LINDET(4: 11)
           MOVE DESCRICAO-CX20    TO CXP020T-LINDET(15: 31)
           MOVE CODIGO-REDUZ-CX20 TO CXP020T-LINDET(52: 05).
       GRAU-3 SECTION.
           MOVE CODIGO-E          TO CXP020T-LINDET(7: 11)
           MOVE DESCRICAO-CX20    TO CXP020T-LINDET(18: 31)
           MOVE CODIGO-REDUZ-CX20 TO CXP020T-LINDET(52: 05).
       GRAU-4 SECTION.
           MOVE CODIGO-E          TO CXP020T-LINDET(10: 11)
           MOVE DESCRICAO-CX20    TO CXP020T-LINDET(21: 31)
           MOVE CODIGO-REDUZ-CX20 TO CXP020T-LINDET(52: 05).
       ITEM-SELECIONADO SECTION.
           MOVE CXP020T-LINDET(1: 60) TO STRING-1.
           MOVE STRING-1(52: 5) TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(33: 5).
       CLEAR-FLAGS SECTION.
           INITIALIZE CXP020T-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP020T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP020T-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD020.
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.
