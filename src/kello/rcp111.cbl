       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP111.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 24-10-2007
      *DESCRIÇÃO: Cadastro de FAIXA DE VALORES
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY RCPX111.
           COPY RCPX112.
           COPY CADPRO.SEL.

       DATA DIVISION.
       FILE SECTION.

           COPY RCPW111.
           COPY RCPW112.
           COPY CADPRO.FD.

       WORKING-STORAGE SECTION.
           COPY "RCP111.CPB".
           COPY "RCP111.CPy".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RCD111             PIC XX       VALUE SPACES.
           05  ST-RCD112             PIC XX       VALUE SPACES.
           05  ST-CADPRO             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  DIAS-PARCELA          PIC 9(04)    VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ,ZZZ.
           05  ERRO                  PIC X(01)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 AUX-QTD-INI         PIC 9(06).
       01 AUX-QTD-FIN         PIC 9(06).



       01 DET-FAIXA.
          05 DET-PRODUTO             PIC X(40).
          05 FILLER                  PIC X(01).
          05 DET-QTD-INI             PIC ZZZ.ZZ9.
          05 FILLER                  PIC X(06).
          05 DET-QTD-FIN             PIC ZZZ.ZZ9.
          05 FILLER                  PIC X(01).
          05 DET-DES-PER             PIC ZZ9,99.
          05 FILLER                  PIC X(01).
          05 FILLER                  PIC X(01) VALUE "%".
          05 DET-DES-VLR             PIC ZZ.ZZ9,99.


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
           MOVE "RCD111" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD111
           MOVE "RCD112" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD112
           MOVE "CADPRO" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADPRO
           OPEN I-O   RCD111 RCD112 CADPRO
           CLOSE                    CADPRO
           OPEN INPUT CADPRO
           IF ST-RCD111 = "35"
              CLOSE RCD111      OPEN OUTPUT RCD111
              CLOSE RCD111      OPEN I-O    RCD111
           END-IF.
           IF ST-RCD112 = "35"
              CLOSE RCD112      OPEN OUTPUT RCD112
              CLOSE RCD112      OPEN I-O    RCD112
           END-IF.
           IF ST-CADPRO = "35"
              CLOSE CADPRO      OPEN OUTPUT CADPRO
              CLOSE CADPRO      OPEN I-O    CADPRO
           END-IF.
           IF ST-RCD111 <> "00"
              MOVE "ERRO ABERTURA RCD111: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD111 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD112 <> "00"
              MOVE "ERRO ABERTURA RCD112: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD112 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADPRO <> "00"
              MOVE "ERRO ABERTURA CADPRO: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADPRO TO GS-MENSAGEM-ERRO(23: 02)
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
                   PERFORM PREENCHER-SB-PRODUTO
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
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGAR-ULTIMO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-VERIFICA-CODIGO-TRUE
                   PERFORM VERIFICA-CODIGO
               WHEN GS-INCLUIR-FAIXA-TRUE
                   PERFORM INCLUIR-FAIXAS
               WHEN GS-VALIDA-FAIXA-TRUE
                   PERFORM VALIDA-FAIXA
               WHEN GS-MOSTRA-FAIXA-TRUE
                   PERFORM MOSTRAR-FAIXA
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       PREENCHER-SB-PRODUTO SECTION.
           MOVE "APAGAR-SB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-CADPRO
                      GS-LINHA
           START CADPRO KEY IS NOT LESS CADPRO-CH-NOME INVALID KEY
                 MOVE "10" TO ST-CADPRO.

           PERFORM UNTIL ST-CADPRO = "10"
                 READ CADPRO NEXT AT END
                      MOVE "10" TO ST-CADPRO
                 NOT AT END
                      ADD 1 TO GS-LINHA
                      MOVE SPACES TO GS-PRODUTO
                      STRING CADPRO-NOME(1:35) "-" CADPRO-CODIGO INTO
                             GS-PRODUTO
                      MOVE "INSERIR-SB" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.

       MOSTRAR-FAIXA SECTION.
           MOVE GS-LINHA-FAIXAS TO DET-FAIXA
           MOVE DET-PRODUTO     TO GS-PRODUTO
           MOVE DET-QTD-INI     TO GS-QTDE-INI
           MOVE DET-QTD-FIN     TO GS-QTDE-FIM
           MOVE DET-DES-PER     TO GS-DESC-PERC
           MOVE DET-DES-VLR     TO GS-DESC-VLR.


       VERIFICA-CODIGO SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE GS-FAIXA
                      GS-PRODUTO
                      GS-QTDE-INI
                      GS-QTDE-FIM
                      GS-DESC-PERC
                      GS-DESC-VLR

           MOVE GS-CODIGO TO CODIGO-RC111
           READ RCD111 INVALID KEY
               MOVE 0                            TO GS-GRAVA
               MOVE SPACES                       TO GS-DESCRICAO
           NOT INVALID KEY
               MOVE 1                            TO GS-GRAVA
               MOVE DESCRICAO-RC111              TO GS-DESCRICAO
               INITIALIZE REG-RCD112
               MOVE GS-CODIGO                    TO CODIGO-RC112
               START RCD112 KEY IS NOT LESS CHAVE-RC112 INVALID KEY
                   MOVE "10" TO ST-RCD112
               END-START
               PERFORM UNTIL ST-RCD112 = "10"
                   READ RCD112 NEXT AT END
                       MOVE "10" TO ST-RCD112
                   NOT AT END
                       IF GS-CODIGO <> CODIGO-RC112
                          MOVE "10" TO ST-RCD112
                       ELSE
                          MOVE PRODUTO-RC112      TO CADPRO-CODIGO
                          READ CADPRO INVALID KEY
                               MOVE "******"      TO CADPRO-NOME
                          END-READ
                          MOVE SPACES             TO DET-PRODUTO
                          STRING CADPRO-NOME(1:35) "-" CADPRO-CODIGO
                                                INTO DET-PRODUTO
                          MOVE QTDE-INICIAL-RC112 TO DET-QTD-INI
                          MOVE QTDE-FINAL-RC112   TO DET-QTD-FIN
                          MOVE DESC-PERC-RC112    TO DET-DES-PER
                          MOVE DESC-VLR-RC112     TO DET-DES-VLR
                          MOVE DET-FAIXA          TO GS-LINHA-FAIXAS

                          EVALUATE FAIXA-RC112
                              WHEN 1
                                     MOVE "INSERIR-LB1" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                              WHEN 2
                                     MOVE "INSERIR-LB2" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                              WHEN 3
                                     MOVE "INSERIR-LB3" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                              WHEN 4
                                     MOVE "INSERIR-LB4" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                              WHEN 5
                                     MOVE "INSERIR-LB5" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                          END-EVALUATE
                       END-IF
                   END-READ
               END-PERFORM.


       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1      to gs-flag-critica
           move spaces to mensagem.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LIMPAR-DADOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-RCD111 REG-RCD112
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       EXCLUI-RECORD SECTION.
           DELETE  RCD111 NOT INVALID KEY
               INITIALIZE REG-RCD112
               MOVE GS-CODIGO TO CODIGO-RC112
               START RCD112 KEY IS NOT LESS CHAVE-RC112 INVALID KEY
                   MOVE "10" TO ST-RCD112
               END-START
               PERFORM UNTIL ST-RCD112 = "10"
                   READ RCD112 NEXT AT END
                       MOVE "10" TO ST-RCD112
                   NOT AT END
                       IF GS-CODIGO <> CODIGO-RC112
                          MOVE "10" TO ST-RCD112
                       ELSE
                          DELETE RCD112
                       END-IF
                   END-READ
               END-PERFORM
           END-DELETE
           PERFORM LIMPAR-DADOS.

       SALVAR-DADOS SECTION.
           MOVE GS-CODIGO         TO CODIGO-RC111
           MOVE GS-DESCRICAO      TO DESCRICAO-RC111
           IF GS-GRAVA = 0
              WRITE REG-RCD111 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE
              REWRITE REG-RCD111 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           END-IF

           INITIALIZE REG-RCD112
           MOVE GS-CODIGO TO CODIGO-RC112
           START RCD112 KEY IS NOT LESS CHAVE-RC112 INVALID KEY
               MOVE "10" TO ST-RCD112
           END-START
           PERFORM UNTIL ST-RCD112 = "10"
               READ RCD112 NEXT AT END
                   MOVE "10" TO ST-RCD112
               NOT AT END
                   IF GS-CODIGO <> CODIGO-RC112
                      MOVE "10" TO ST-RCD112
                   ELSE
                      DELETE RCD112
                   END-IF
               END-READ
           END-PERFORM

           MOVE 1         TO GS-LINHA
           MOVE SPACES    TO GS-LINHA-FAIXAS
           MOVE "LER-LB1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-FAIXAS = SPACES
                MOVE GS-LINHA-FAIXAS TO DET-FAIXA

                MOVE GS-CODIGO         TO CODIGO-RC112
                MOVE 1                 TO FAIXA-RC112
                MOVE DET-PRODUTO(37:4) TO PRODUTO-RC112
                MOVE DET-QTD-INI       TO QTDE-INICIAL-RC112
                MOVE DET-QTD-FIN       TO QTDE-FINAL-RC112
                MOVE DET-DES-PER       TO DESC-PERC-RC112
                MOVE DET-DES-VLR       TO DESC-VLR-RC112

                WRITE REG-RCD112 INVALID KEY
                      MOVE ST-RCD112 TO ST-RCD111
                      PERFORM ERRO-GRAVACAO
                END-WRITE

                ADD 1 TO GS-LINHA
                MOVE SPACES          TO GS-LINHA-FAIXAS
                MOVE "LER-LB1"       TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           MOVE 1         TO GS-LINHA
           MOVE SPACES    TO GS-LINHA-FAIXAS
           MOVE "LER-LB2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-FAIXAS = SPACES
                MOVE GS-LINHA-FAIXAS TO DET-FAIXA

                MOVE GS-CODIGO         TO CODIGO-RC112
                MOVE 2                 TO FAIXA-RC112
                MOVE DET-PRODUTO(37:4) TO PRODUTO-RC112
                MOVE DET-QTD-INI       TO QTDE-INICIAL-RC112
                MOVE DET-QTD-FIN       TO QTDE-FINAL-RC112
                MOVE DET-DES-PER       TO DESC-PERC-RC112
                MOVE DET-DES-VLR       TO DESC-VLR-RC112

                WRITE REG-RCD112 INVALID KEY
                      MOVE ST-RCD112 TO ST-RCD111
                      PERFORM ERRO-GRAVACAO
                END-WRITE

                ADD 1 TO GS-LINHA
                MOVE SPACES          TO GS-LINHA-FAIXAS
                MOVE "LER-LB2"       TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           MOVE 1         TO GS-LINHA
           MOVE SPACES    TO GS-LINHA-FAIXAS
           MOVE "LER-LB3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-FAIXAS = SPACES
                MOVE GS-LINHA-FAIXAS TO DET-FAIXA

                MOVE GS-CODIGO         TO CODIGO-RC112
                MOVE 3                 TO FAIXA-RC112
                MOVE DET-PRODUTO(37:4) TO PRODUTO-RC112
                MOVE DET-QTD-INI       TO QTDE-INICIAL-RC112
                MOVE DET-QTD-FIN       TO QTDE-FINAL-RC112
                MOVE DET-DES-PER       TO DESC-PERC-RC112
                MOVE DET-DES-VLR       TO DESC-VLR-RC112

                WRITE REG-RCD112 INVALID KEY
                      MOVE ST-RCD112 TO ST-RCD111
                      PERFORM ERRO-GRAVACAO
                END-WRITE

                ADD 1 TO GS-LINHA
                MOVE SPACES          TO GS-LINHA-FAIXAS
                MOVE "LER-LB3"       TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           MOVE 1         TO GS-LINHA
           MOVE SPACES    TO GS-LINHA-FAIXAS
           MOVE "LER-LB4" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-FAIXAS = SPACES
                MOVE GS-LINHA-FAIXAS TO DET-FAIXA

                MOVE GS-CODIGO         TO CODIGO-RC112
                MOVE 4                 TO FAIXA-RC112
                MOVE DET-PRODUTO(37:4) TO PRODUTO-RC112
                MOVE DET-QTD-INI       TO QTDE-INICIAL-RC112
                MOVE DET-QTD-FIN       TO QTDE-FINAL-RC112
                MOVE DET-DES-PER       TO DESC-PERC-RC112
                MOVE DET-DES-VLR       TO DESC-VLR-RC112

                WRITE REG-RCD112 INVALID KEY
                      MOVE ST-RCD112 TO ST-RCD111
                      PERFORM ERRO-GRAVACAO
                END-WRITE

                ADD 1 TO GS-LINHA
                MOVE SPACES          TO GS-LINHA-FAIXAS
                MOVE "LER-LB4"       TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           MOVE 1         TO GS-LINHA
           MOVE SPACES    TO GS-LINHA-FAIXAS
           MOVE "LER-LB5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-FAIXAS = SPACES
                MOVE GS-LINHA-FAIXAS TO DET-FAIXA

                MOVE GS-CODIGO         TO CODIGO-RC112
                MOVE 5                 TO FAIXA-RC112
                MOVE DET-PRODUTO(37:4) TO PRODUTO-RC112
                MOVE DET-QTD-INI       TO QTDE-INICIAL-RC112
                MOVE DET-QTD-FIN       TO QTDE-FINAL-RC112
                MOVE DET-DES-PER       TO DESC-PERC-RC112
                MOVE DET-DES-VLR       TO DESC-VLR-RC112

                WRITE REG-RCD112 INVALID KEY
                      MOVE ST-RCD112 TO ST-RCD111
                      PERFORM ERRO-GRAVACAO
                END-WRITE

                ADD 1 TO GS-LINHA
                MOVE SPACES          TO GS-LINHA-FAIXAS
                MOVE "LER-LB5"       TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.


       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-CODIGO         TO CODIGO-RC111
           READ RCD111 INVALID KEY
               MOVE "Faixa de Valores Inválido" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE DESCRICAO-RC111              TO GS-DESCRICAO
               INITIALIZE REG-RCD112
               MOVE GS-CODIGO                    TO CODIGO-RC112
               START RCD112 KEY IS NOT LESS CHAVE-RC112 INVALID KEY
                   MOVE "10" TO ST-RCD112
               END-START
               PERFORM UNTIL ST-RCD112 = "10"
                   READ RCD112 NEXT AT END
                       MOVE "10" TO ST-RCD112
                   NOT AT END
                       IF GS-CODIGO <> CODIGO-RC112
                          MOVE "10" TO ST-RCD112
                       ELSE
                          MOVE PRODUTO-RC112      TO CADPRO-CODIGO
                          READ CADPRO INVALID KEY
                               MOVE "*******"     TO CADPRO-NOME
                          END-READ
                          MOVE SPACES             TO DET-PRODUTO
                          STRING CADPRO-NOME(1:35) "-" PRODUTO-RC112
                                                INTO DET-PRODUTO
                          MOVE QTDE-INICIAL-RC112 TO DET-QTD-INI
                          MOVE QTDE-FINAL-RC112   TO DET-QTD-FIN
                          MOVE DESC-PERC-RC112    TO DET-DES-PER
                          MOVE DESC-VLR-RC112     TO DET-DES-VLR
                          MOVE DET-FAIXA          TO GS-LINHA-FAIXAS

                          EVALUATE FAIXA-RC112
                              WHEN 1
                                     MOVE "INSERIR-LB1" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                              WHEN 2
                                     MOVE "INSERIR-LB2" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                              WHEN 3
                                     MOVE "INSERIR-LB3" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                              WHEN 4
                                     MOVE "INSERIR-LB4" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                              WHEN 5
                                     MOVE "INSERIR-LB5" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                          END-EVALUATE
                       END-IF
                   END-READ
               END-PERFORM.

       CARREGAR-ULTIMO SECTION.
           INITIALIZE REG-RCD111
           MOVE ALL "9" TO CODIGO-RC111
           START RCD111 KEY IS LESS THAN CODIGO-RC111 INVALID KEY
               MOVE 0 TO GS-CODIGO
           NOT INVALID KEY
               READ RCD111 PREVIOUS AT END
                   MOVE 0 TO GS-CODIGO
               NOT AT END
                   MOVE CODIGO-RC111 TO GS-CODIGO
               END-READ
           END-START
           ADD 1 TO GS-CODIGO.

       INCLUIR-FAIXAS SECTION.
           MOVE GS-PRODUTO            TO DET-PRODUTO
           MOVE GS-QTDE-INI           TO DET-QTD-INI
           MOVE GS-QTDE-FIM           TO DET-QTD-FIN
           MOVE GS-DESC-PERC          TO DET-DES-PER
           MOVE GS-DESC-VLR           TO DET-DES-VLR
           MOVE DET-FAIXA             TO GS-LINHA-FAIXAS
           EVALUATE GS-FAIXA
               WHEN 1
                      MOVE "INSERIR-LB1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 2
                      MOVE "INSERIR-LB2" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 3
                      MOVE "INSERIR-LB3" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 4
                      MOVE "INSERIR-LB4" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 5
                      MOVE "INSERIR-LB5" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
           END-EVALUATE.

       VALIDA-FAIXA SECTION.
           EVALUATE GS-FAIXA
               WHEN 1 PERFORM VALIDA-LB1
               WHEN 2 PERFORM VALIDA-LB2
               WHEN 3 PERFORM VALIDA-LB3
               WHEN 4 PERFORM VALIDA-LB4
               WHEN 5 PERFORM VALIDA-LB5
           END-EVALUATE.

       VALIDA-LB1 SECTION.
           MOVE "N"       TO ERRO

           MOVE 1         TO GS-LINHA
           MOVE SPACES    TO GS-LINHA-FAIXAS
           MOVE "LER-LB1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-FAIXAS = SPACES OR ERRO = "S"
               MOVE GS-LINHA-FAIXAS TO DET-FAIXA

               IF DET-PRODUTO = GS-PRODUTO
                  MOVE DET-QTD-INI   TO AUX-QTD-INI
                  MOVE DET-QTD-FIN   TO AUX-QTD-FIN

                  IF GS-QTDE-INI NOT > AUX-QTD-INI
                     IF GS-QTDE-FIM NOT < AUX-QTD-INI
                        MOVE "S" TO ERRO
                     END-IF
                  END-IF

                  IF GS-QTDE-INI NOT < AUX-QTD-INI
                     IF GS-QTDE-INI NOT > AUX-QTD-FIN
                        MOVE "S" TO ERRO
                     END-IF
                  END-IF

               END-IF

               ADD 1                TO GS-LINHA
               MOVE SPACES          TO GS-LINHA-FAIXAS
               MOVE "LER-LB1"       TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           IF GS-QTDE-INI > GS-QTDE-FIM
              MOVE "Intervalo Inicial Maior que o Final" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ERRO = "S"
              MOVE "Intervalo das Quantidades Já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       VALIDA-LB2 SECTION.
           MOVE "N"       TO ERRO

           MOVE 1         TO GS-LINHA
           MOVE SPACES    TO GS-LINHA-FAIXAS
           MOVE "LER-LB2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-FAIXAS = SPACES OR ERRO = "S"
               MOVE GS-LINHA-FAIXAS TO DET-FAIXA

               IF DET-PRODUTO = GS-PRODUTO
                  MOVE DET-QTD-INI   TO AUX-QTD-INI
                  MOVE DET-QTD-FIN   TO AUX-QTD-FIN

                  IF GS-QTDE-INI NOT > AUX-QTD-INI
                     IF GS-QTDE-FIM NOT < AUX-QTD-INI
                        MOVE "S" TO ERRO
                     END-IF
                  END-IF

                  IF GS-QTDE-INI NOT < AUX-QTD-INI
                     IF GS-QTDE-INI NOT > AUX-QTD-FIN
                        MOVE "S" TO ERRO
                     END-IF
                  END-IF

               END-IF

               ADD 1                TO GS-LINHA
               MOVE SPACES          TO GS-LINHA-FAIXAS
               MOVE "LER-LB2"       TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           IF GS-QTDE-INI > GS-QTDE-FIM
              MOVE "Intervalo Inicial Maior que o Final" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ERRO = "S"
              MOVE "Intervalo das Quantidades Já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       VALIDA-LB3 SECTION.
           MOVE "N"       TO ERRO

           MOVE 1         TO GS-LINHA
           MOVE SPACES    TO GS-LINHA-FAIXAS
           MOVE "LER-LB3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-FAIXAS = SPACES OR ERRO = "S"
               MOVE GS-LINHA-FAIXAS TO DET-FAIXA

               IF DET-PRODUTO = GS-PRODUTO
                  MOVE DET-QTD-INI   TO AUX-QTD-INI
                  MOVE DET-QTD-FIN   TO AUX-QTD-FIN

                  IF GS-QTDE-INI NOT > AUX-QTD-INI
                     IF GS-QTDE-FIM NOT < AUX-QTD-INI
                        MOVE "S" TO ERRO
                     END-IF
                  END-IF

                  IF GS-QTDE-INI NOT < AUX-QTD-INI
                     IF GS-QTDE-INI NOT > AUX-QTD-FIN
                        MOVE "S" TO ERRO
                     END-IF
                  END-IF

               END-IF

               ADD 1                TO GS-LINHA
               MOVE SPACES          TO GS-LINHA-FAIXAS
               MOVE "LER-LB3"       TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           IF GS-QTDE-INI > GS-QTDE-FIM
              MOVE "Intervalo Inicial Maior que o Final" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ERRO = "S"
              MOVE "Intervalo das Quantidades Já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       VALIDA-LB4 SECTION.
           MOVE "N"       TO ERRO

           MOVE 1         TO GS-LINHA
           MOVE SPACES    TO GS-LINHA-FAIXAS
           MOVE "LER-LB4" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-FAIXAS = SPACES OR ERRO = "S"
               MOVE GS-LINHA-FAIXAS TO DET-FAIXA

               IF DET-PRODUTO = GS-PRODUTO
                  MOVE DET-QTD-INI   TO AUX-QTD-INI
                  MOVE DET-QTD-FIN   TO AUX-QTD-FIN

                  IF GS-QTDE-INI NOT > AUX-QTD-INI
                     IF GS-QTDE-FIM NOT < AUX-QTD-INI
                        MOVE "S" TO ERRO
                     END-IF
                  END-IF

                  IF GS-QTDE-INI NOT < AUX-QTD-INI
                     IF GS-QTDE-INI NOT > AUX-QTD-FIN
                        MOVE "S" TO ERRO
                     END-IF
                  END-IF

               END-IF

               ADD 1                TO GS-LINHA
               MOVE SPACES          TO GS-LINHA-FAIXAS
               MOVE "LER-LB4"       TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           IF GS-QTDE-INI > GS-QTDE-FIM
              MOVE "Intervalo Inicial Maior que o Final" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ERRO = "S"
              MOVE "Intervalo das Quantidades Já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       VALIDA-LB5 SECTION.
           MOVE "N"       TO ERRO

           MOVE 1         TO GS-LINHA
           MOVE SPACES    TO GS-LINHA-FAIXAS
           MOVE "LER-LB5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINHA-FAIXAS = SPACES OR ERRO = "S"
               MOVE GS-LINHA-FAIXAS TO DET-FAIXA

               IF DET-PRODUTO = GS-PRODUTO
                  MOVE DET-QTD-INI   TO AUX-QTD-INI
                  MOVE DET-QTD-FIN   TO AUX-QTD-FIN

                  IF GS-QTDE-INI NOT > AUX-QTD-INI
                     IF GS-QTDE-FIM NOT < AUX-QTD-INI
                        MOVE "S" TO ERRO
                     END-IF
                  END-IF

                  IF GS-QTDE-INI NOT < AUX-QTD-INI
                     IF GS-QTDE-INI NOT > AUX-QTD-FIN
                        MOVE "S" TO ERRO
                     END-IF
                  END-IF

               END-IF

               ADD 1                TO GS-LINHA
               MOVE SPACES          TO GS-LINHA-FAIXAS
               MOVE "LER-LB5"       TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           IF GS-QTDE-INI > GS-QTDE-FIM
              MOVE "Intervalo Inicial Maior que o Final" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ERRO = "S"
              MOVE "Intervalo das Quantidades Já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.


       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-RCD111       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP111"    TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE RCD111 RCD112 CADPRO
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
