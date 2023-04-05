       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP101e.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 16-08-2005
      *DESCRIÇÃO: Gera arquivo de

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY CBPX101.
           COPY CBPX102.


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY CBPW101.
           COPY CBPW102.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-CBD101             PIC XX       VALUE SPACES.
           05  ST-CBD102             PIC XX       VALUE SPACES.
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
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
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
           MOVE "CBD101"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CBD101
           MOVE "CBD102"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CBD102

           OPEN I-O   CBD101
                      CBD102

           CLOSE      CBD101
                      CBD102

           OPEN INPUT CBD101
           OPEN I-O   CBD102

           IF ST-CBD101 <> "00"
              STRING "ERRO ABERTURA CBD101: " ST-CBD101 X"0DA0"
                      PATH-CBD101 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CBD102 <> "00"
              STRING "ERRO ABERTURA CBD102: " ST-CBD102 X"0DA0"
                      PATH-CBD102 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       GERA-ARQUIVO SECTION.
           INITIALIZE REG-CBD102
           START CBD102 KEY IS NOT < CHAVE-CB102 INVALID KEY
                 MOVE "10" TO ST-CBD102.

           PERFORM UNTIL ST-CBD102 = "10"
                 READ CBD102 NEXT AT END
                      MOVE "10" TO ST-CBD102
                 NOT AT END
                      MOVE ZEROS TO SALDO-CB102
                      REWRITE REG-CBD102 INVALID KEY
                          MOVE "Erro de Regravação...CBD102" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                      END-REWRITE
                 END-READ
           END-PERFORM

           INITIALIZE REG-CBD101
           START CBD101 KEY IS NOT < ALT-CB101 INVALID KEY
                 MOVE "10" TO ST-CBD101.

           PERFORM UNTIL ST-CBD101 = "10"
                 READ CBD101 NEXT RECORD AT END
                      MOVE "10"                   TO ST-CBD101
                 NOT AT END
                      MOVE DATA-EXTRATO-CB101  TO DATA-EXTRATO-CB102
                      MOVE CONTA-BANCO-CB101   TO CONTA-BANCO-CB102
                      READ CBD102 INVALID KEY
                           MOVE ZEROS TO SALDO-CB102
                           IF HISTORICO-CB101 < 50
                              SUBTRACT VALOR-CB101 FROM SALDO-CB102
                           ELSE
                              ADD VALOR-CB101 TO SALDO-CB102
                           END-IF
                           WRITE REG-CBD102
                           END-WRITE
                      NOT INVALID KEY
                           IF HISTORICO-CB101 < 50
                              SUBTRACT VALOR-CB101 FROM SALDO-CB102
                           ELSE
                              ADD VALOR-CB101 TO SALDO-CB102
                           END-IF
                           REWRITE REG-CBD102
                           END-REWRITE
                      END-READ
                 END-READ
           END-PERFORM.

       FECHAR-ARQUIVOS SECTION.
           CLOSE CBD101 CBD102.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM.

       FINALIZAR-PROGRAMA SECTION.
           EXIT PROGRAM.
           STOP RUN.

