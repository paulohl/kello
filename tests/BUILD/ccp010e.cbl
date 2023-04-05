       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP010e.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 16-08-2005
      *DESCRIÇÃO: Gera arquivo de saldos de CONTA CORRENTE
      *  arquivo saldo mensal      - CCD010
      *  Estes arquivos é para melhorar o desempenho dos relatórios.
      *  O mês corrente o saldo será acumulado no momento de execução
      *  do relatório. O saldo será calculado por intervalo de vencto
      *  e para cada conta corrente
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CCPX010.

           SELECT CCD010e ASSIGN TO PATH-CCD010e
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CCD010e
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CC10e = ANOMES-VCTO-CC10e
                                             FORNEC-CC10e
                  ALTERNATE RECORD KEY IS ALT-CC10e =
                            FORNEC-CC10e ANOMES-VCTO-CC10e.

           COPY CCPX100.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CCPW010.
       COPY CCPW100.

      *  arquivo que acumula saldo do c.corr-mes/ano de vencto e fornec
       FD  CCD010e.
       01  REG-CCD010e.
           05  ANOMES-VCTO-CC10e          PIC 9(6).
           05  FORNEC-CC10e               PIC 9(6).
           05  SALDOE-CC10e               PIC 9(8)V99.
           05  SALDOS-CC10e               PIC 9(8)V99.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-CCD010             PIC XX       VALUE SPACES.
           05  ST-CCD010e            PIC XX       VALUE SPACES.
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

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE DS-CONTROL-BLOCK

           open i-o   cad001
           close      cad001
           open input cad001

           if st-cad001 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura...CAD001" x"0da0"
                     "Status . . . " st-cad001 into mensagem
              move "C" to tipo-msg
              perform exibir-mensagem.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CORPO-PROGRAMA SECTION.
           INITIALIZE REG-CAD001
           START CAD001 KEY IS NOT LESS CODIGO-CA001 INVALID KEY
                 MOVE "10" TO ST-CAD001.

           PERFORM UNTIL ST-CAD001 = "10"
                 READ CAD001 NEXT AT END
                      MOVE "10" TO ST-CAD001
                 NOT AT END
                      PERFORM ABRIR-ARQUIVOS
                      PERFORM GERA-ARQUIVO
                      PERFORM FECHAR-ARQUIVOS
                 END-READ
           END-PERFORM

           CLOSE CAD001.

       ABRIR-ARQUIVOS SECTION.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CCD010"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CCD010
           MOVE "CCD010e"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CCD010e
           MOVE "CCD100"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CCD100

           OPEN OUTPUT CCD010e
           CLOSE       CCD010e
           OPEN I-O    CCD010e

           OPEN I-O    CCD100
           CLOSE       CCD100
           OPEN INPUT  CCD100

           OPEN I-O    CCD010
           IF ST-CCD010 = "35"
              CLOSE CCD010      OPEN OUTPUT CCD010
              CLOSE CCD010      OPEN I-O    CCD010
           END-IF

           IF ST-CCD010 <> "00"
              STRING "ERRO ABERTURA CCD010: " ST-CCD010 X"0DA0"
                      PATH-CCD010  INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CCD010e <> "00"
              STRING "ERRO ABERTURA CCD010e: " ST-CCD010 X"0DA0"
                      PATH-CCD010E INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CCD100 <> "00"
              STRING "ERRO ABERTURA CCD100: " ST-CCD010 X"0DA0"
                      PATH-CCD100  INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       GERA-ARQUIVO SECTION.
           INITIALIZE REG-CCD100
      *    MOVE 01                    TO DATA-INI(7: 2)
      *    MOVE 10                    TO DATA-INI(5: 2)
      *    MOVE 0001                  TO DATA-INI(1: 4)
      *    MOVE 31                    TO DATA-FIM(7: 2)
      *    MOVE 12                    TO DATA-FIM(5: 2)
      *    MOVE 9999                  TO DATA-FIM(1: 4)
      *    MOVE DATA-INI(01: 06)      TO MESANO-INI
      *    MOVE DATA-FIM(01: 06)      TO MESANO-FIM
      *    MOVE ZEROS                 TO SITUACAO-CC100
      *    MOVE ZEROS                 TO FORNEC-CC100

           INITIALIZE REG-CCD010e
           START CCD010e KEY IS NOT LESS CHAVE-CC10e INVALID KEY
                MOVE "10" TO ST-CCD010e.

           PERFORM UNTIL ST-CCD010e = "10"
                READ CCD010e NEXT AT END
                     MOVE "10" TO ST-CCD010e
                NOT AT END
                     delete ccd010e
                END-READ
           END-PERFORM.


           START CCD100 KEY IS NOT < ALT3-CC100 INVALID KEY
                 MOVE "10" TO ST-CCD100.

           PERFORM UNTIL ST-CCD100 = "10"
                 READ CCD100 NEXT RECORD AT END
                      MOVE "10" TO ST-CCD100
                 NOT AT END
                      IF SITUACAO-CC100 > 0
                         MOVE "10" TO ST-CCD100
                      ELSE
                         MOVE DATA-VENCTO-CC100(1: 6) TO
                              ANOMES-VCTO-CC10e
                         MOVE FORNEC-CC100            TO FORNEC-CC10e
                         MOVE ZEROS                   TO VALORE-W
                                                         VALORS-W
                         IF CRED-DEB-CC100 = 0
                            MOVE VALOR-CC100          TO VALORS-W
                         ELSE
                            MOVE VALOR-CC100          TO VALORE-W
                         END-IF

                         display reg-ccd100

                         READ CCD010e INVALID KEY
                              MOVE VALORE-W TO SALDOE-CC10e
                              MOVE VALORS-W TO SALDOS-CC10e
                              WRITE REG-CCD010e
                              END-WRITE
                         NOT INVALID KEY
                             ADD VALORE-W TO SALDOE-CC10e
                             ADD VALORS-W TO SALDOS-CC10e
                             REWRITE REG-CCD010e
                             END-REWRITE
                         END-READ
                      END-IF
                 END-READ
           END-PERFORM

           INITIALIZE REG-CCD010
           START CCD010 KEY IS NOT LESS CHAVE-CC10 INVALID KEY
                MOVE "10" TO ST-CCD010.

           PERFORM UNTIL ST-CCD010 = "10"
                READ CCD010 NEXT AT END
                     MOVE "10" TO ST-CCD010
                NOT AT END
                     DELETE CCD010
                     END-DELETE
                END-READ
           END-PERFORM

           INITIALIZE REG-CCD010e
           START CCD010e KEY IS NOT LESS CHAVE-CC10e INVALID KEY
                MOVE "10" TO ST-CCD010e.

           PERFORM UNTIL ST-CCD010e = "10"
                READ CCD010e NEXT AT END
                     MOVE "10" TO ST-CCD010e
                NOT AT END
                     move reg-ccd010e to reg-ccd010
                     write reg-ccd010 invalid key
                       rewrite reg-ccd010
                END-READ
           END-PERFORM.


       FECHAR-ARQUIVOS SECTION.
           CLOSE CCD010 CCD010E CCD100.

       ERRO-GRAVACAO SECTION.
           STRING "ERRO GRAVAÇÃO " REG-CCD010 INTO MENSAGEM
           MOVE   "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM.

       FINALIZAR-PROGRAMA SECTION.
           EXIT PROGRAM
           STOP RUN.
