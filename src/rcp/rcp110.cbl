       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP110.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 24-10-2007
      *DESCRIÇÃO: Cadastro de PLANO DE PAGAMENTOS
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY RCPX110.
           COPY RCPX1101.

       DATA DIVISION.
       FILE SECTION.
       COPY RCPW110.
       COPY RCPW1101.

       WORKING-STORAGE SECTION.
           COPY "RCP110.CPB".
           COPY "RCP110.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RCD110             PIC XX       VALUE SPACES.
           05  ST-RCD1101            PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ,ZZZ.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MASC-COND1            PIC ZZ.
           05  MASC-COND2            PIC ZZ9.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 DET-PARCELAS.
          05 DET-CONDICAO.
             10 DET-CONDICAO1    PIC X(03).
             10 DET-CONDICAO2    PIC X(04).
          05 FILLER              PIC X(09).
          05 DET-JUROS           PIC X(03).




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
           MOVE "RCD110"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD110
           MOVE "RCD1101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD1101
           OPEN I-O RCD110 RCD1101
           CLOSE    RCD110 RCD1101
           OPEN I-O RCD110 RCD1101
           IF ST-RCD110 = "35"
              CLOSE RCD110      OPEN OUTPUT RCD110
              CLOSE RCD110      OPEN I-O    RCD110
           END-IF.
           IF ST-RCD1101 = "35"
              CLOSE RCD1101     OPEN OUTPUT RCD1101
              CLOSE RCD1101     OPEN I-O    RCD1101
           END-IF.
           IF ST-RCD110 <> "00"
              MOVE "ERRO ABERTURA RCD110: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD110 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD1101 <> "00"
              MOVE "ERRO ABERTURA RCD1101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD1101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM CARREGAR-ULTIMO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM CARREGAR-ULTIMO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-GERAR-PARCELA-TRUE
                   PERFORM GERAR-PARCELA
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGAR-ULTIMO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-VERIFICA-CODIGO-TRUE
                   PERFORM VERIFICA-CODIGO
               WHEN GS-ALTERAR-JUROS-TRUE
                   PERFORM ALTERAR-JUROS
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       ALTERAR-JUROS SECTION.
           MOVE GS-LINDET      TO DET-PARCELAS
           EVALUATE DET-JUROS
               WHEN "SIM"    MOVE "NÃO" TO DET-JUROS
               WHEN "NÃO"    MOVE "SIM" TO DET-JUROS
           END-EVALUATE
           MOVE DET-PARCELAS   TO GS-LINDET.

       VERIFICA-CODIGO SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-CODIGO TO CODIGO-RC110
           READ RCD110 INVALID KEY
               MOVE 0                            TO GS-GRAVA
               MOVE SPACES                       TO GS-DESCRICAO
               MOVE 0                            TO GS-LIMITE
               MOVE 0                            TO GS-SEM-ENTRADA
               MOVE 0                            TO GS-CALCULA-JUROS
           NOT INVALID KEY
               MOVE 1                            TO GS-GRAVA
               MOVE DESCRICAO-RC110              TO GS-DESCRICAO
               MOVE LIMITE-PARCELA-RC110         TO GS-LIMITE
               MOVE SEM-ENTRADA-RC110            TO GS-SEM-ENTRADA
               MOVE CALCULA-JUROS-RC110          TO GS-CALCULA-JUROS
               INITIALIZE REG-RCD1101 gs-ind
               MOVE GS-CODIGO                    TO CODIGO-RC1101
               START RCD1101 KEY IS NOT LESS CHAVE-RC1101 INVALID KEY
                   MOVE "10" TO ST-RCD1101
               END-START
               PERFORM UNTIL ST-RCD1101 = "10"
                   READ RCD1101 NEXT AT END
                       MOVE "10" TO ST-RCD1101
                   NOT AT END
                       IF GS-CODIGO <> CODIGO-RC1101
                          MOVE "10" TO ST-RCD1101
                       ELSE
                          ADD 1                  TO GS-IND

                          MOVE PARCELA-RC1101    TO DET-CONDICAO
                          MOVE JUROS-RC1101      TO DET-JUROS

                          MOVE DET-PARCELAS      TO GS-LINHA-PARCELA
                          MOVE "INSERIR-LB"      TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                   END-READ
               END-PERFORM.


       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LIMPAR-DADOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-RCD110
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       EXCLUI-RECORD SECTION.
           DELETE  RCD110
           PERFORM LIMPAR-DADOS.

       SALVAR-DADOS SECTION.
           MOVE GS-CODIGO         TO CODIGO-RC110
           MOVE GS-DESCRICAO      TO DESCRICAO-RC110
           MOVE GS-LIMITE         TO LIMITE-PARCELA-RC110
           MOVE GS-SEM-ENTRADA    TO SEM-ENTRADA-RC110
           MOVE GS-CALCULA-JUROS  TO CALCULA-JUROS-RC110
           IF GS-GRAVA = 0
              WRITE REG-RCD110 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE
              REWRITE REG-RCD110 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           END-IF.

           INITIALIZE REG-RCD1101
           MOVE GS-CODIGO         TO CODIGO-RC1101
           START RCD1101 KEY IS NOT LESS CHAVE-RC1101 INVALID KEY
               MOVE "10" TO ST-RCD1101.

           PERFORM UNTIL ST-RCD1101 = "10"
               READ RCD1101 NEXT AT END
                   MOVE "10" TO ST-RCD1101
               NOT AT END
                   IF GS-CODIGO <> CODIGO-RC1101
                      MOVE "10" TO ST-RCD1101
                   ELSE
                      DELETE RCD1101
                   END-IF
               END-READ
           END-PERFORM

           MOVE 1 TO GS-LINHA
           MOVE SPACES TO GS-LINHA-PARCELA
           MOVE "LER-PRAZOS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-PARCELA = SPACES
               MOVE GS-LINHA-PARCELA     TO DET-PARCELAS
               MOVE GS-CODIGO            TO CODIGO-RC1101
               MOVE DET-CONDICAO         TO PARCELA-RC1101
               MOVE DET-JUROS            TO JUROS-RC1101

               WRITE REG-RCD1101

               ADD 1                     TO GS-LINHA
               MOVE SPACES               TO GS-LINHA-PARCELA
               MOVE "LER-PRAZOS"         TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-CODIGO         TO CODIGO-RC110
           READ RCD110 INVALID KEY
               MOVE "Plano de Pagamentos Inválido" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               INITIALIZE REG-RCD1101 gs-ind
               MOVE GS-CODIGO                    TO CODIGO-RC1101
               START RCD1101 KEY IS NOT LESS CHAVE-RC1101 INVALID KEY
                   MOVE "10" TO ST-RCD1101
               END-START
               PERFORM UNTIL ST-RCD1101 = "10"
                   READ RCD1101 NEXT AT END
                       MOVE "10" TO ST-RCD1101
                   NOT AT END
                       IF GS-CODIGO <> CODIGO-RC1101
                          MOVE "10" TO ST-RCD1101
                       ELSE
                          ADD 1                  TO GS-IND
                          MOVE PARCELA-RC1101    TO DET-CONDICAO
                          MOVE JUROS-RC1101      TO DET-JUROS

                          MOVE DET-PARCELAS      TO GS-LINHA-PARCELA
                          MOVE "INSERIR-LB"      TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                   END-READ
               END-PERFORM.

       GERAR-PARCELA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           IF GS-SEM-ENTRADA = 0
              COMPUTE GS-LIMITE = GS-LIMITE - 1.

           MOVE 0                TO GS-IND

           PERFORM UNTIL GS-IND = GS-LIMITE
               ADD 1              TO GS-IND
               EVALUATE GS-CALCULA-JUROS
                  WHEN 0 MOVE "NÃO" TO DET-JUROS
                  WHEN 1 MOVE "SIM" TO DET-JUROS
               END-EVALUATE

               IF GS-SEM-ENTRADA = 1
                  MOVE "0 + "         TO DET-CONDICAO1
               ELSE
                  MOVE "1 + "         TO DET-CONDICAO1
               END-IF

               IF GS-IND > 9
                  MOVE GS-IND         TO MASC-COND2
                  MOVE MASC-COND2     TO DET-CONDICAO2
               ELSE
                  MOVE GS-IND         TO MASC-COND1
                  MOVE MASC-COND1     TO DET-CONDICAO2
               END-IF

               MOVE DET-PARCELAS  TO GS-LINHA-PARCELA
               MOVE "INSERIR-LB"  TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM
           IF GS-SEM-ENTRADA = 0
              COMPUTE GS-LIMITE = GS-LIMITE + 1.

       CARREGAR-ULTIMO SECTION.
           INITIALIZE REG-RCD110
           MOVE ALL "9" TO CODIGO-RC110
           START RCD110 KEY IS LESS THAN CODIGO-RC110 INVALID KEY
               MOVE 0 TO GS-CODIGO
           NOT INVALID KEY
               READ RCD110 PREVIOUS AT END
                   MOVE 0 TO GS-CODIGO
               NOT AT END
                   MOVE CODIGO-RC110 TO GS-CODIGO
               END-READ
           END-START
           ADD 1 TO GS-CODIGO.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-RCD110       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP110"    TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE RCD110 RCD1101
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
