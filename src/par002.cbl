       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAR002.
       AUTHOR.  ALFREDO SAVIOLLI NETO.
       SECURITY. PARAMETRIZAÇÃO DE CODIGOS REDUZIDOS.
       DATE-WRITTEN. 07-03-2014.
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX018.

           COPY PARX002.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW018.

           COPY PARW002.

       WORKING-STORAGE SECTION.
           COPY "PAR002.CPB".
           COPY "PAR002.CPY".
           COPY "DS-CNTRL.MF".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(55).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD018             PIC X(02).
           05  ST-PAR002             PIC X(02).
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".

       77 janelaPrincipal            object reference.
       77 handle8                    pic 9(08) comp-x value zeros.
       77 wHandle                    pic 9(09) comp-5 value zeros.

       01 mensagem                   pic x(200).
       01 tipo-msg                   pic x(01).
       01 resp-msg                   pic x(01).

       01 det-01.
          05 det-programa            pic x(08).
          05 filler                  pic x(02).
          05 det-portador-r          pic zzz9.
          05 filler                  pic x(02).
          05 det-nome-r              pic x(30).
          05 filler                  pic x(03).
          05 det-portador-d          pic zzz9.
          05 filler                  pic x(02).
          05 det-nome-d              pic x(30).


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.

           INITIALIZE GS-DATA-BLOCK DS-CONTROL-BLOCK
           MOVE ZEROS TO ERRO-W.
           MOVE GS-DATA-BLOCK-VERSION-NO   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO              TO DS-VERSION-NO

           MOVE EMPRESA-W                  TO EMP-REC

           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "PAR002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR002.

           OPEN I-O   CAD018 PAR002
           CLOSE      CAD018 PAR002
           OPEN INPUT CAD018 PAR002

           OPEN I-O   PAR002
           CLOSE      PAR002
           OPEN INPUT PAR002

           IF ST-CAD018 = "35"
              CLOSE CAD018      OPEN OUTPUT CAD018
              CLOSE CAD018      OPEN I-O    CAD018
           END-IF.
           IF ST-PAR002 = "35"
              CLOSE PAR002      OPEN OUTPUT PAR002
              CLOSE PAR002      OPEN I-O    PAR002
           END-IF.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PAR002 <> "00"
              MOVE "ERRO ABERTURA PAR002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PAR002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
                    PERFORM CARREGAR-PROGRAMAS
               WHEN GS-CRITICA-TRUE
                    PERFORM LER
               WHEN GS-POPUP-TRUE
                    PERFORM POPUP
               WHEN GS-SALVAR-TRUE
                    PERFORM SALVAR-PARAMETRIZACAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGAR-PROGRAMAS SECTION.
           MOVE "APAGAR-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-PAR002
           START PAR002 KEY IS NOT LESS CHAVE-PAR002 INVALID KEY
                 MOVE "10" TO ST-PAR002.

           PERFORM UNTIL ST-PAR002 = "10"
                 READ PAR002 NEXT AT END
                      MOVE "10" TO ST-PAR002
                 NOT AT END
                      MOVE PROGRAMA-PAR002    TO DET-PROGRAMA
                      MOVE PORTADOR-PAR002-R  TO DET-PORTADOR-R
                                                 PORTADOR
                      READ CAD018 INVALID KEY
                           MOVE "*****"       TO NOME-PORT
                      END-READ
                      MOVE NOME-PORT          TO DET-NOME-R

                      MOVE PORTADOR-PAR002-D  TO DET-PORTADOR-D
                                                 PORTADOR
                      READ CAD018 INVALID KEY
                           MOVE "*****"       TO NOME-PORT
                      END-READ
                      MOVE NOME-PORT          TO DET-NOME-D

                      MOVE DET-01             TO GS-LINHA-DETALHE
                      MOVE "INSERIR-LB"       TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER SECTION.
           EVALUATE GS-CAMPO
               WHEN "EF-PROGRAMA"   PERFORM CRITICAR-PROGRAMA
               WHEN "EF-PORTADOR-R" PERFORM CRITICAR-PORTADOR-R
               WHEN "EF-PORTADOR-D" PERFORM CRITICAR-PORTADOR-D
               WHEN "LER"           PERFORM LER-LINHA
               WHEN "INSERIR"       PERFORM CRITICAR-PROGRAMA
                                       THRU CRITICAR-PORTADOR-D
                                    IF GS-FLAG-CRITICA = 0
                                       PERFORM INSERIR
                                    END-IF

           END-EVALUATE.

       CRITICAR-PROGRAMA SECTION.
           IF GS-ACP-PROGRAMA EQUAL SPACES
              MOVE "Nome do Programa Não Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM
           ELSE
              PERFORM VERIFICAR-JA.

       CRITICAR-PORTADOR-R SECTION.
           IF GS-ACP-PORTADOR-R EQUAL ZEROS
              MOVE "Portador da Entrada Rejeitada Não Informado"
                       TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM
           ELSE
              MOVE GS-ACP-PORTADOR-R TO PORTADOR
              READ CAD018 INVALID KEY
                   MOVE "Portador Inválido" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM 140-EXIBIR-MENSAGEM
              NOT INVALID KEY
                   MOVE NOME-PORT           TO GS-DESC-PORTADOR-R
                   REFRESH-OBJECT PRINCIPAL.

       CRITICAR-PORTADOR-D SECTION.
           IF GS-ACP-PORTADOR-D EQUAL ZEROS
              MOVE "Portador Devolvido Não Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM
           ELSE
              MOVE GS-ACP-PORTADOR-D TO PORTADOR
              READ CAD018 INVALID KEY
                   MOVE "Portador Inválido" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM 140-EXIBIR-MENSAGEM
              NOT INVALID KEY
                   MOVE NOME-PORT           TO GS-DESC-PORTADOR-D
                   REFRESH-OBJECT PRINCIPAL.

       VERIFICAR-JA SECTION.
           MOVE 1        TO GS-LINHA
           MOVE SPACES   TO GS-LINHA-DETALHE
           MOVE "LER-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-DETALHE = SPACES
               MOVE GS-LINHA-DETALHE TO DET-01
               IF DET-PROGRAMA = GS-ACP-PROGRAMA
                  MOVE "Programa já Informado" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM 140-EXIBIR-MENSAGEM
                  EXIT PERFORM
               END-IF
               ADD 1 TO GS-LINHA
               MOVE SPACES   TO GS-LINHA-DETALHE
               MOVE "LER-LB" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       INSERIR SECTION.
           MOVE GS-ACP-PROGRAMA    TO DET-PROGRAMA
           MOVE GS-ACP-PORTADOR-R  TO DET-PORTADOR-R
           MOVE GS-DESC-PORTADOR-R TO DET-NOME-R
           MOVE GS-ACP-PORTADOR-D  TO DET-PORTADOR-D
           MOVE GS-DESC-PORTADOR-D TO DET-NOME-D
           MOVE DET-01             TO GS-LINHA-DETALHE
           MOVE "INSERIR-LB"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LER-LINHA SECTION.
           MOVE GS-LINHA-DETALHE   TO DET-01
           MOVE DET-PROGRAMA       TO GS-ACP-PROGRAMA
           MOVE DET-PORTADOR-R     TO GS-ACP-PORTADOR-R
           MOVE DET-NOME-R         TO GS-DESC-PORTADOR-R
           MOVE DET-PORTADOR-D     TO GS-ACP-PORTADOR-D
           MOVE DET-NOME-D         TO GS-DESC-PORTADOR-D
           REFRESH-OBJECT PRINCIPAL
           SET-FOCUS EF-PROGRAMA.

       POPUP SECTION.
           EVALUATE GS-CAMPO
               WHEN "EF-PORTADOR-R"  PERFORM SUGESTAO-PORTADOR-R
               WHEN "EF-PORTADOR-D"  PERFORM SUGESTAO-PORTADOR-D
               WHEN OTHER            MOVE "Sugestão Inexistente" TO
                                                 MENSAGEM
                                     MOVE "C" TO TIPO-MSG
                                     PERFORM 140-EXIBIR-MENSAGEM
           END-EVALUATE.

       SUGESTAO-PORTADOR-R SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-PORTADOR-R.
           MOVE PASSAR-STRING-1(33: 4) TO GS-ACP-PORTADOR-R.
           REFRESH-OBJECT PRINCIPAL.

       SUGESTAO-PORTADOR-D SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-PORTADOR-D.
           MOVE PASSAR-STRING-1(33: 4) TO GS-ACP-PORTADOR-D.
           REFRESH-OBJECT PRINCIPAL.

       SALVAR-PARAMETRIZACAO SECTION.
           CLOSE    PAR002
           OPEN I-O PAR002
           INITIALIZE REG-PAR002
           START PAR002 KEY IS NOT LESS CHAVE-PAR002 INVALID KEY
                 MOVE "10" TO ST-PAR002.

           PERFORM UNTIL ST-PAR002 = "10"
                 READ PAR002 NEXT AT END
                      MOVE "10" TO ST-PAR002
                 NOT AT END
                      DELETE PAR002 INVALID KEY
                             MOVE "Erro de Exclusão...PAR002" TO
                             MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM 140-EXIBIR-MENSAGEM
                      END-DELETE
                 END-READ
           END-PERFORM

           MOVE 1        TO GS-LINHA
           MOVE SPACES   TO GS-LINHA-DETALHE
           MOVE "LER-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-DETALHE = SPACES
               MOVE GS-LINHA-DETALHE       TO DET-01
               MOVE DET-PROGRAMA           TO PROGRAMA-PAR002
               MOVE DET-PORTADOR-R         TO PORTADOR-PAR002-R
               MOVE DET-PORTADOR-D         TO PORTADOR-PAR002-D
               WRITE REG-PAR002 INVALID KEY
                     MOVE "Erro de Gravação...PAR002" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM 140-EXIBIR-MENSAGEM
               END-WRITE
               ADD  1        TO GS-LINHA
               MOVE SPACES   TO GS-LINHA-DETALHE
               MOVE "LER-LB" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      PAR002
           OPEN INPUT PAR002.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to gs-flag-critica
           move spaces to mensagem.

       LOAD-SCREENSET SECTION.
      *  INICIALIZA UMA NOVA PILHA
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "PAR002" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
               IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD018 PAR002
           STOP RUN.
