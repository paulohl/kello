       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP040e.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 11/07/1998
      *DESCRIÇÃO: Gera arquivo de saldos de caixa executavel (agendador)
      *  arquivo saldo mensal      - cxp040
      *  arquivo de contas extrato - cxp041
      *  arquivo de contas resultado - cxp042
      *  Estes arquivos é para melhorar o desempenho dos relatórios,
      *  já que antes perdia-se muito tempo, aguardando para acumular
      *  os saldos dos meses anteriores.
      *  O mês corrente o saldo será acumulado no momento de execução
      *  do relatório.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CXPX040.
           COPY CXPX041.
           COPY CXPX042.
           COPY CXPX100.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CXPW040.
       COPY CXPW041.
       COPY CXPW042.
       COPY CXPW100.

       WORKING-STORAGE SECTION.
           COPY "CXP040.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
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
           05  FLAG-CRITICA          PIC 9        VALUE ZEROS.
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

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           MOVE ZEROS TO FLAG-CRITICA
           INITIALIZE CXP040-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP040-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP040-VERSION-NO  TO DS-VERSION-NO

           open i-o   cad001
           close      cad001
           open input cad001

           if st-cad001 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura...CAD001" x"0da0"
                     "Status . . . " st-cad001 into mensagem
              move "C" to tipo-msg
              perform exibir-mensagem.


           initialize reg-cad001
           start cad001 key is not less codigo-ca001 invalid key
                 move "10" to st-cad001.

           perform until st-cad001 = "10"
                 read cad001 next at end
                      move "10" to st-cad001
                 not at end
                      perform abrir-arquivos
                      perform gera-saldo
                      perform fechar-arquivos
                 end-read
           end-perform

           close cad001

           STOP RUN.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CXD040"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CXD040
           MOVE "CXD041"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CXD041
           MOVE "CXD042"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CXD042
           MOVE "CXD100"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CXD100
           OPEN I-O   CXD040 CXD041 CXD042 CXD100
           CLOSE      CXD040 CXD041 CXD042 CXD100
           OPEN I-O   CXD040 CXD041 CXD042

           OPEN INPUT CXD100
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
              STRING "ERRO ABERTURA CXD040: " ST-CXD040 X"0DA0"
                      PATH-CXD040 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CXD041 <> "00"
              STRING "ERRO ABERTURA CXD041: " ST-CXD041 X"0DA0"
                      PATH-CXD041 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CXD042 <> "00"
              STRING "ERRO ABERTURA CXD042: " ST-CXD042 X"0DA0"
                      PATH-CXD042 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CXD100 <> "00"
              STRING "ERRO ABERTURA CXD100: " ST-CXD100 X"0DA0"
                      PATH-CXD100 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       FECHAR-ARQUIVOS SECTION.
           CLOSE CXD040 CXD041 CXD042 CXD100.

       ZERAR-SALDO SECTION.
      *    IF CXP040-ORDEM = 0
      *       CLOSE       CXD040 CXD041 CXD042
      *       OPEN OUTPUT CXD040 CXD041 CXD042
      *       CLOSE       CXD040 CXD041 CXD042
      *       OPEN I-O    CXD040 CXD041 CXD042
      *    ELSE
              PERFORM ZERAR-SALDO-INTERVALO.

       ZERAR-SALDO-INTERVALO SECTION.
           MOVE MESANO-INI       TO ANOMES-CX40.
           START CXD040 KEY IS NOT < ANOMES-CX40 INVALID KEY
                 MOVE "10" TO ST-CXD040.
           PERFORM UNTIL ST-CXD040 = "10"
              READ CXD040 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD040
              NOT AT END
                  IF ANOMES-CX40 > MESANO-FIM
                     MOVE "10" TO ST-CXD040
                  ELSE
                     DELETE CXD040
                  END-IF
              END-READ
           END-PERFORM.
           MOVE MESANO-INI       TO ANOMES-CX41.
           MOVE ZEROS            TO CONTAPART-CX41.
           START CXD041 KEY IS NOT < CHAVE-CX41  INVALID KEY
                 MOVE "10" TO ST-CXD041.
           PERFORM UNTIL ST-CXD041 = "10"
              READ CXD041 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD041
              NOT AT END
                  IF ANOMES-CX41 > MESANO-FIM
                     MOVE "10" TO ST-CXD041
                  ELSE
                     DELETE CXD041
                  END-IF
              END-READ
           END-PERFORM.
           MOVE MESANO-INI       TO ANOMES-CX42.
           MOVE ZEROS            TO CONTAREDUZ-CX42.
           START CXD042 KEY IS NOT < CHAVE-CX42  INVALID KEY
                 MOVE "10" TO ST-CXD042.
           PERFORM UNTIL ST-CXD042 = "10"
              READ CXD042 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD042
              NOT AT END
                   IF ANOMES-CX42 > MESANO-FIM
                      MOVE "10" TO ST-CXD041
                   ELSE
                      DELETE CXD042
                   END-IF
              END-READ
           END-PERFORM.

       GERA-SALDO SECTION.
           MOVE 01                        TO DATA-INI(7: 2).
           MOVE 01                        TO DATA-INI(5: 2).
           MOVE 0001                      TO DATA-INI(1: 4).
           MOVE 31                        TO DATA-FIM(7: 2).
           MOVE 12                        TO DATA-FIM(5: 2).
           MOVE 9999                      TO DATA-FIM(1: 4).
           MOVE DATA-INI(01: 06) TO MESANO-INI.
           MOVE DATA-FIM(01: 06) TO MESANO-FIM.
           PERFORM ZERAR-SALDO.
           MOVE DATA-INI TO DATA-MOV-CX100.
           MOVE ZEROS TO SEQ-CX100.

           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.

           PERFORM UNTIL ST-CXD100 = "10"
                 READ CXD100 NEXT RECORD WITH IGNORE LOCK AT END
                      MOVE "10" TO ST-CXD100
                 NOT AT END
                      IF DATA-MOV-CX100 > DATA-FIM
                         MOVE "10" TO ST-CXD100
                      ELSE
                         MOVE DATA-MOV-CX100(1: 6) TO ANOMES-CX40
                         IF TIPO-LCTO-CX100 NOT < 50
                            MOVE VALOR-CX100 TO VALORE-W
                            MOVE ZEROS       TO VALORS-W
                         ELSE
                            MOVE VALOR-CX100 TO VALORS-W
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
                         READ CXD041 INVALID KEY
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
                         READ CXD042 INVALID KEY
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
                      END-IF
                 END-READ
           END-PERFORM.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           MOVE 1 TO FLAG-CRITICA
           move spaces to mensagem.

