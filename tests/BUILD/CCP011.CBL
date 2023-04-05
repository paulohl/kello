       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP011.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 17/07/2000
      *DESCRIÇÃO: Gera arquivo de saldos de CONTA CORRENTE -
      *           POR DATA-MOVTO - P/ RELATORIO ANALITICO
      *  arquivo saldo mensal      - CCD011
      *  Estes arquivos é para melhorar o desempenho dos relatórios.
      *  O mês corrente o saldo será acumulado no momento de execução
      *  do relatório. O saldo será calculado por intervalo de MOVTO
      *  e para cada conta corrente
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CCPX011.
           COPY CCPX100.

       DATA DIVISION.
       FILE SECTION.
       COPY CCPW011.
       COPY CCPW100.
       WORKING-STORAGE SECTION.
           COPY "CCP011.CPB".
           COPY "CCP011.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CCD011             PIC XX       VALUE SPACES.
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
           MOVE "CCD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD011.
           MOVE "CCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD100.
           OPEN I-O CCD011.
           OPEN INPUT CCD100.
           IF ST-CCD011 = "35"
              CLOSE CCD011      OPEN OUTPUT CCD011
              CLOSE CCD011      OPEN I-O CCD011
           END-IF.
           IF ST-CCD011 <> "00"
              MOVE "ERRO ABERTURA CCD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
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
              CLOSE CCD011  OPEN OUTPUT CCD011   CLOSE CCD011
              OPEN I-O CCD011
           ELSE PERFORM ZERAR-SALDO-INTERVALO.
       ZERAR-SALDO-INTERVALO SECTION.
           MOVE ZEROS            TO FORNEC-CC11
           MOVE MESANO-INI       TO ANOMES-MVTO-CC11
           START CCD011 KEY IS NOT < CHAVE-CC11 INVALID KEY
                 MOVE "10" TO ST-CCD011.
           MOVE "Zerando Saldo: " TO GS-MENSAGEM(01: 17).
           PERFORM UNTIL ST-CCD011 = "10"
              READ CCD011 NEXT RECORD AT END MOVE "10" TO ST-CCD011
                NOT AT END
                  MOVE "CCD011"    TO GS-MENSAGEM(18: 10)
                  MOVE ANOMES-MVTO-CC11 TO GS-MENSAGEM(28: 10)
                  MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  IF ANOMES-MVTO-CC11 > MESANO-FIM
                       MOVE "10" TO ST-CCD011
                  ELSE DELETE CCD011
                  END-IF
              END-READ
           END-PERFORM.
       GERA-SALDO SECTION.
           MOVE 01                    TO DATA-INI(7: 2).
           MOVE GS-MESANO-INI(01: 02) TO DATA-INI(5: 2).
           MOVE GS-MESANO-INI(03: 04) TO DATA-INI(1: 4).
           MOVE 31                    TO DATA-FIM(7: 2).
           MOVE GS-MESANO-FIM(01: 02) TO DATA-FIM(5: 2).
           MOVE GS-MESANO-FIM(03: 04) TO DATA-FIM(1: 4).
           MOVE DATA-INI(01: 06) TO MESANO-INI.
           MOVE DATA-FIM(01: 06) TO MESANO-FIM.
           PERFORM ZERAR-SALDO.
           MOVE ZEROS TO SITUACAO-CC100.
           MOVE ZEROS TO FORNEC-CC100.
           MOVE DATA-INI TO DATA-MOVTO-CC100.
           MOVE "Gerando saldo: " TO GS-MENSAGEM(01: 17).
           START CCD100 KEY IS NOT < DATA-MOVTO-CC100 INVALID KEY
                 MOVE "10" TO ST-CCD100.
           PERFORM UNTIL ST-CCD100 = "10"
            READ CCD100 NEXT RECORD AT END MOVE "10" TO ST-CCD100
               NOT AT END
                IF DATA-MOVTO-CC100 > DATA-FIM
                   MOVE "10" TO ST-CCD100
                ELSE
                 IF SITUACAO-CC100 > 0 CONTINUE
                 ELSE
                  MOVE "CCD100"           TO GS-MENSAGEM(18: 10)
                  MOVE DATA-MOVTO-CC100   TO GS-MENSAGEM(18: 10)
                  MOVE "EXIBE-MENSAGEM"   TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  MOVE DATA-MOVTO-CC100(1: 6) TO ANOMES-MVTO-CC11
                  MOVE FORNEC-CC100            TO FORNEC-CC11
                  MOVE ZEROS TO VALORE-W VALORS-W
                  IF CRED-DEB-CC100 = 0 MOVE VALOR-CC100 TO VALORS-W
                  ELSE MOVE VALOR-CC100 TO VALORE-W
                  END-IF
                  READ CCD011
                    INVALID KEY
                       MOVE VALORE-W TO SALDOE-CC11
                       MOVE VALORS-W TO SALDOS-CC11
                       WRITE REG-CCD011
                       END-WRITE
                    NOT INVALID KEY
                      ADD VALORE-W TO SALDOE-CC11
                      ADD VALORS-W TO SALDOS-CC11
                      REWRITE REG-CCD011
                      END-REWRITE
                  END-READ
                 END-IF
                END-IF
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
           INITIALIZE REG-CCD011
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-CCD011       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP011" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CCD011 CCD100.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
