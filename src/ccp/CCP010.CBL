       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP010.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 26/01/1999
      *DESCRIÇÃO: Gera arquivo de saldos de CONTA CORRENTE
      *  arquivo saldo mensal      - CCD010
      *  Estes arquivos é para melhorar o desempenho dos relatórios.
      *  O mês corrente o saldo será acumulado no momento de execução
      *  do relatório. O saldo será calculado por intervalo de vencto
      *  e para cada conta corrente
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CCPX010.
           COPY CCPX100.

       DATA DIVISION.
       FILE SECTION.
       COPY CCPW010.
       COPY CCPW100.
       WORKING-STORAGE SECTION.
           COPY "CCP010.CPB".
           COPY "CCP010.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CCD010             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
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
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CCD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD010.
           MOVE "CCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD100.
           OPEN I-O CCD010.
           OPEN INPUT CCD100.
           IF ST-CCD010 = "35"
              CLOSE CCD010      OPEN OUTPUT CCD010
              CLOSE CCD010      OPEN I-O CCD010
           END-IF.
           IF ST-CCD010 <> "00"
              MOVE "ERRO ABERTURA CCD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-GERA-FLG-TRUE
                   PERFORM GERA-SALDO
               WHEN GS-CLR-FLG-TRUE
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
           IF GS-ORDEM = 0
              CLOSE CCD010  OPEN OUTPUT CCD010   CLOSE CCD010
              OPEN I-O CCD010
           ELSE PERFORM ZERAR-SALDO-INTERVALO.
       ZERAR-SALDO-INTERVALO SECTION.
           MOVE ZEROS            TO FORNEC-CC10
           MOVE MESANO-INI       TO ANOMES-VCTO-CC10
           START CCD010 KEY IS NOT < CHAVE-CC10 INVALID KEY
                 MOVE "10" TO ST-CCD010.
           MOVE "Zerando Saldo: " TO GS-MENSAGEM(01: 17).
           PERFORM UNTIL ST-CCD010 = "10"
              READ CCD010 NEXT RECORD AT END MOVE "10" TO ST-CCD010
                NOT AT END
                  MOVE "CCD010"    TO GS-MENSAGEM(18: 10)
                  MOVE ANOMES-VCTO-CC10 TO GS-MENSAGEM(28: 10)
                  MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  IF ANOMES-VCTO-CC10 > MESANO-FIM
                       MOVE "10" TO ST-CCD010
                  ELSE DELETE CCD010
                  END-IF
              END-READ
           END-PERFORM.
       GERA-SALDO SECTION.
           MOVE 01                    TO DATA-INI(7: 2).
           MOVE GS-MESANO-INI(01: 02) TO DATA-INI(5: 2).
           MOVE GS-MESANO-INI(03: 04) TO DATA-INI(1: 4).
           MOVE 31                        TO DATA-FIM(7: 2).
           MOVE GS-MESANO-FIM(01: 02) TO DATA-FIM(5: 2).
           MOVE GS-MESANO-FIM(03: 04) TO DATA-FIM(1: 4).
           MOVE DATA-INI(01: 06) TO MESANO-INI.
           MOVE DATA-FIM(01: 06) TO MESANO-FIM.
           PERFORM ZERAR-SALDO.
           MOVE ZEROS TO SITUACAO-CC100.
           MOVE ZEROS TO FORNEC-CC100.
           MOVE DATA-INI TO DATA-VENCTO-CC100.
           MOVE "Gerando saldo: " TO GS-MENSAGEM(01: 17).
           START CCD100 KEY IS NOT < ALT3-CC100 INVALID KEY
                 MOVE "10" TO ST-CCD100.
           PERFORM UNTIL ST-CCD100 = "10"
            READ CCD100 NEXT RECORD AT END MOVE "10" TO ST-CCD100
               NOT AT END
                 IF DATA-VENCTO-CC100 > DATA-FIM OR
                    SITUACAO-CC100 > 0
                    MOVE "10" TO ST-CCD100
                 ELSE
                  MOVE "CCD100"           TO GS-MENSAGEM(18: 10)
                  MOVE DATA-VENCTO-CC100  TO GS-MENSAGEM(18: 10)
                  MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  MOVE DATA-VENCTO-CC100(1: 6) TO ANOMES-VCTO-CC10
                  MOVE FORNEC-CC100            TO FORNEC-CC10
                  MOVE ZEROS TO VALORE-W VALORS-W
                  IF CRED-DEB-CC100 = 0 MOVE VALOR-CC100 TO VALORS-W
                  ELSE MOVE VALOR-CC100 TO VALORE-W
                  END-IF
                  READ CCD010
                    INVALID KEY
                       MOVE VALORE-W TO SALDOE-CC10
                       MOVE VALORS-W TO SALDOS-CC10
                       WRITE REG-CCD010
                       END-WRITE
                    NOT INVALID KEY
                      ADD VALORE-W TO SALDOE-CC10
                      ADD VALORS-W TO SALDOS-CC10
                      REWRITE REG-CCD010
                      END-REWRITE
                  END-READ
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-MENSAGEM.
           MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           MOVE 1 TO ERRO-W.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CCD010
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-CCD010       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP010" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CCD010 CCD100.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
