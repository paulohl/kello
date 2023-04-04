       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP040.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 06/08/1998
      *DESCRIÇÃO: Gera arquivo de saldos de caixa
      *  arquivo saldo mensal      - cxp040
      *  arquivo de contas extrato - cxp041
      *  arquivo de contas resultado - cxp042
      *  Estes arquivos é para melhorar o desempenho dos relatórios,
      *  já que antes perdia-se muito tempo, aguardando para acumular
      *  os saldos dos meses anteriores.
      *  O mês corrente o saldo será acumulado no momento de execução
      *  do relatório.
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CXPX040.
           COPY CXPX041.
           COPY CXPX042.
           COPY CXPX100.

       DATA DIVISION.
       FILE SECTION.
       COPY CXPW040.
       COPY CXPW041.
       COPY CXPW042.
       COPY CXPW100.

       WORKING-STORAGE SECTION.
           COPY "CXP040.CPB".
           COPY "CXP040.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD040             PIC XX       VALUE SPACES.
           05  ST-CXD041             PIC XX       VALUE SPACES.
           05  ST-CXD042             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W(FLAG)- PARA SABER SE OCORREU ERRO-ABERTURA ARQUIVO
      *  ERRO-W = 0 (NÃO)  ERRO-W = 1 (SIM)
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  VALORE-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE ENTRADA
           05  VALORS-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE SAIDA
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP040-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CXP040-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP040-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP040-VERSION-NO  TO DS-VERSION-NO

           MOVE EMPRESA-W          TO EMP-REC

           MOVE "CXD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD040.
           MOVE "CXD041" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD041.
           MOVE "CXD042" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD042.
           MOVE "CXD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD100.
           OPEN I-O CXD040 CXD041 CXD042.
           OPEN INPUT CXD100.
           IF ST-CXD040 = "35"
              CLOSE CXD040      OPEN OUTPUT CXD040
              CLOSE CXD040      OPEN I-O CXD040
           END-IF.
           IF ST-CXD041 = "35"
              CLOSE CXD041      OPEN OUTPUT CXD041
              CLOSE CXD041      OPEN I-O CXD041
           END-IF.
           IF ST-CXD042 = "35"
              CLOSE CXD042      OPEN OUTPUT CXD042
              CLOSE CXD042      OPEN I-O CXD042
           END-IF.
           IF ST-CXD040 <> "00"
              MOVE "ERRO ABERTURA CXD040: "  TO CXP040-MENSAGEM-ERRO
              MOVE ST-CXD040 TO CXP040-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD041 <> "00"
              MOVE "ERRO ABERTURA CXD041: "  TO CXP040-MENSAGEM-ERRO
              MOVE ST-CXD041 TO CXP040-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD042 <> "00"
              MOVE "ERRO ABERTURA CXD042: "  TO CXP040-MENSAGEM-ERRO
              MOVE ST-CXD042 TO CXP040-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO CXP040-MENSAGEM-ERRO
              MOVE ST-CXD100 TO CXP040-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP040-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP040-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CXP040-GERA-FLG-TRUE
                   PERFORM GERA-SALDO
               WHEN CXP040-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       ZERAR-SALDO SECTION.
           IF CXP040-ORDEM = 0
              CLOSE CXD040 CXD041 CXD042
              OPEN OUTPUT CXD040 CXD041 CXD042
              CLOSE CXD040 CXD041 CXD042
              OPEN I-O CXD040 CXD041 CXD042
           ELSE PERFORM ZERAR-SALDO-INTERVALO.
       ZERAR-SALDO-INTERVALO SECTION.
           MOVE MESANO-INI       TO ANOMES-CX40.
           START CXD040 KEY IS NOT < ANOMES-CX40 INVALID KEY
                 MOVE "10" TO ST-CXD040.
           MOVE "Zerando Saldo: " TO CXP040-MENSAGEM(01: 17).
           PERFORM UNTIL ST-CXD040 = "10"
              READ CXD040 NEXT RECORD AT END MOVE "10" TO ST-CXD040
                NOT AT END
                  MOVE "CXD040"    TO CXP040-MENSAGEM(18: 10)
                  MOVE ANOMES-CX40 TO CXP040-MENSAGEM(28: 10)
                  MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  IF ANOMES-CX40 > MESANO-FIM MOVE "10" TO ST-CXD040
                  ELSE
                     DELETE CXD040
                  END-IF
              END-READ
           END-PERFORM.
           MOVE MESANO-INI       TO ANOMES-CX41.
           MOVE ZEROS            TO CONTAPART-CX41.
           START CXD041 KEY IS NOT < CHAVE-CX41  INVALID KEY
                 MOVE "10" TO ST-CXD041.
           MOVE "Zerando Saldo: " TO CXP040-MENSAGEM(01: 17).
           PERFORM UNTIL ST-CXD041 = "10"
              READ CXD041 NEXT RECORD AT END MOVE "10" TO ST-CXD041
                NOT AT END
                  MOVE "CXD041"    TO CXP040-MENSAGEM(18: 10)
                  MOVE ANOMES-CX41 TO CXP040-MENSAGEM(28: 10)
                  MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  IF ANOMES-CX41 > MESANO-FIM MOVE "10" TO ST-CXD041
                  ELSE
                     DELETE CXD041
                  END-IF
              END-READ
           END-PERFORM.
           MOVE MESANO-INI       TO ANOMES-CX42.
           MOVE ZEROS            TO CONTAREDUZ-CX42.
           START CXD042 KEY IS NOT < CHAVE-CX42  INVALID KEY
                 MOVE "10" TO ST-CXD042.
           MOVE "Zerando Saldo: " TO CXP040-MENSAGEM(01: 17).
           PERFORM UNTIL ST-CXD042 = "10"
              READ CXD042 NEXT RECORD AT END MOVE "10" TO ST-CXD042
                NOT AT END
                  MOVE "CXD042"    TO CXP040-MENSAGEM(18: 10)
                  MOVE ANOMES-CX42 TO CXP040-MENSAGEM(28: 10)
                  MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  IF ANOMES-CX42 > MESANO-FIM MOVE "10" TO ST-CXD041
                  ELSE
                     DELETE CXD042
                  END-IF
              END-READ
           END-PERFORM.
       GERA-SALDO SECTION.
           MOVE 01                        TO DATA-INI(7: 2).
           MOVE CXP040-MESANO-INI(01: 02) TO DATA-INI(5: 2).
           MOVE CXP040-MESANO-INI(03: 04) TO DATA-INI(1: 4).
           MOVE 31                        TO DATA-FIM(7: 2).
           MOVE CXP040-MESANO-FIM(01: 02) TO DATA-FIM(5: 2).
           MOVE CXP040-MESANO-FIM(03: 04) TO DATA-FIM(1: 4).
           MOVE DATA-INI(01: 06) TO MESANO-INI.
           MOVE DATA-FIM(01: 06) TO MESANO-FIM.
           PERFORM ZERAR-SALDO.
           MOVE DATA-INI TO DATA-MOV-CX100.
           MOVE ZEROS TO SEQ-CX100.
           MOVE "Gerando saldo: " TO CXP040-MENSAGEM(01: 17).
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
            READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
               NOT AT END
                 IF DATA-MOV-CX100 > DATA-FIM MOVE "10" TO ST-CXD100
                 ELSE
                  MOVE "CXD100"           TO CXP040-MENSAGEM(18: 10)
                  MOVE DATA-MOV-CX100     TO CXP040-MENSAGEM(18: 10)
                  MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  MOVE DATA-MOV-CX100(1: 6) TO ANOMES-CX40
                  IF TIPO-LCTO-CX100 NOT < 50
                     MOVE VALOR-CX100 TO VALORE-W
                     MOVE ZEROS       TO VALORS-W
                  ELSE MOVE VALOR-CX100 TO VALORS-W
                  MOVE ZEROS       TO VALORE-W
                  END-IF
                  READ CXD040 INVALID KEY
                                MOVE VALORE-W TO SALDOE-CX40
                                MOVE VALORS-W TO SALDOS-CX40
                                WRITE REG-CXD040
                                END-WRITE
                        NOT INVALID KEY
                               ADD VALORE-W TO SALDOE-CX40
                               ADD VALORS-W TO SALDOS-CX40
                               REWRITE REG-CXD040
                               END-REWRITE
                  END-READ
                  MOVE DATA-MOV-CX100(1: 6) TO ANOMES-CX41
                  MOVE CONTAPART-CX100    TO CONTAPART-CX41
                  READ CXD041
                    INVALID KEY
                       MOVE VALORE-W TO SALDOE-CX41
                       MOVE VALORS-W TO SALDOS-CX41
                       WRITE REG-CXD041
                       END-WRITE
                    NOT INVALID KEY
                      ADD VALORE-W TO SALDOE-CX41
                      ADD VALORS-W TO SALDOS-CX41
                      REWRITE REG-CXD041
                      END-REWRITE
                  END-READ
                  MOVE DATA-MOV-CX100(1: 6) TO ANOMES-CX42
                  MOVE CONTA-REDUZ-CX100    TO CONTAREDUZ-CX42
                  READ CXD042
                    INVALID KEY
                      MOVE VALORE-W TO SALDOE-CX42
                      MOVE VALORS-W TO SALDOS-CX42
                      WRITE REG-CXD042
                      END-WRITE
                    NOT INVALID KEY
                        ADD VALORE-W TO SALDOE-CX42
                        ADD VALORS-W TO SALDOS-CX42
                        REWRITE REG-CXD042
                        END-REWRITE
                   END-READ
              END-READ
           END-PERFORM.
           MOVE SPACES TO CXP040-MENSAGEM.
           MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           MOVE 1 TO ERRO-W.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CXD040
           INITIALIZE REG-CXD041
           INITIALIZE REG-CXD042.
           INITIALIZE CXP040-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CXP040-MENSAGEM-ERRO
           MOVE ST-CXD040       TO CXP040-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE CXP040-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP040" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP040-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD040 CXD041 CXD042 CXD100.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

