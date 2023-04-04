       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCOD050.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14-02-2011
      *DESCRIÇÃO: Conversão GALHOCOD060

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY COPX050.

           SELECT  COD050A ASSIGN       TO              PATH-COD050A
                           ORGANIZATION IS                   INDEXED
                           ACCESS MODE  IS                   DYNAMIC
                           STATUS       IS                ST-COD050A
                           LOCK MODE                    IS AUTOMATIC
                           WITH LOCK                    ON    RECORD
                           RECORD KEY   IS             CHAVE-CO50A =
                                                   NR-CONTRATO-CO50A
                                                          ITEM-CO50A
                           ALTERNATE RECORD KEY IS DATA-VENCTO-CO50A
                           WITH                           DUPLICATES
                           ALTERNATE RECORD KEY IS      ALT1-CO50A =
                                                   NR-CONTRATO-CO50A
                                                     REALIZADO-CO50A
                                                   DATA-VENCTO-CO50A
                                                          ITEM-CO50A
                           ALTERNATE RECORD KEY                   IS
                                DATA-PAGTO-CO50A WITH     DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY COPW050.

      * MOVIMENTO DE BRINDES
       FD  COD050A.
       01  REG-COD050A.
           05  NR-CONTRATO-CO50A    PIC 9(04).
           05  ITEM-CO50A           PIC 99.
           05  CODBRINDE-CO50A      PIC 999.
           05  CURSO-CO50A          PIC 9(3).
           05  TURMA-CO50A          PIC XX.
           05  QTDE-POR-FORM-CO50A  PIC 9(5).
      *    qtde-por-form = qtde-brindes ou qtde por formando
      *    quando for qtde-brindes o campo qtde-form-co50 deverá ficar
      *    zerado
           05  QTDE-FORM-CO50A      PIC 9(4).
           05  CUSTO-UNIT-CO50A     PIC 9(8)V99 COMP-3.
      *    se custo-unit-co50 = zeros o custo a ser considerado será
      *    do cadastro de brinde
           05  VALOR-PREVISTO-CO50A PIC 9(8)V99 COMP-3.
           05  DATA-VENCTO-CO50A    PIC 9(8).
      *    DATA-VENCTO - AAAA/MM/DD.
           05  DATA-SOLICIT-CO50A   PIC 9(8)    COMP-3.
           05  SUSP-PREV-DEF-CO50A  PIC 9.
      *    0-PREVISTO   1-DEFINITIVO  2-SUSPENSO
           05  VALOR-PAGO-CO50A     PIC 9(8)V99 COMP-3.
           05  DATA-PAGTO-CO50A     PIC 9(8).
           05  REALIZADO-CO50A      PIC 9.
      *    0-NAO  1-SIM
           05  DIAS-PRAZO-CO50A     PIC 9(4).
      *    Prazo médio até a data prevista de venda (do contrato)
      *    Se DATA-PAGTO-CO50 = zeros
      *       dias entre DATA-VENCTO-CO50 até DATA-PREV-VENDA-CO40
      *    Senão dias entre DATA-PAGTO-CO50 até DATA-PREV-VENDA-CO40.
      *    Qualquer alteração efetuada nos campos data-pagto-co50
      *    data-vencto-co50 e data-prev-venda-co40 deve-se alterar o
      *    campo DIAS-PRAZO-CO50.
           05  COD-FORNEC-CO50A     PIC 9(6)    COMP-3.


      **** QTDE REALIZADO = 1 (SIM) O CUSTO SERµ GRAVADO, PODENDO
      **** SER ALTERADO E A DATAPREVISTA P/ A DATA DE PAGTO

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-COD050A            PIC XX       VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MENSAGEM              PIC X(200).
           05  FLAG-CRITICA          PIC 9(01) VALUE ZEROS.
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  STATUS-CODE           PIC X(02) COMP-5.
           05  ACP-CAMINHO1          PIC X(255) VALUE SPACES.
           05  ACP-CAMINHO2          PIC X(255) VALUE SPACES.
           05  ACP-DTINI             PIC 9(08)  VALUE ZEROS.
           05  ACP-DTFIM             PIC 9(08)  VALUE ZEROS.
           05  WS-OK                 PIC X(01)  VALUE SPACES.
           05  DATA-INI              PIC 9(08)  VALUE ZEROS.
           05  DATA-FIM              PIC 9(08)  VALUE ZEROS.
           05  RESP                  PIC X(01)  VALUE SPACES.
           05  ACHEI                 PIC X(01)  VALUE SPACES.
           05  QTDE-FORM             PIC 9(05)  VALUE ZEROS.
           05  MASC-VALOR            PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  AUX-PREVISTO          PIC 9(09)V99 VALUE ZEROS.
           05  AUX-VLRPAGO           PIC 9(09)V99 VALUE ZEROS.
           05  MASC1                 PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  MASC2                 PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  MASC3                 PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  MASC4                 PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.

       01 ws-data-sys.
          05 ws-data-cpu.
             10 ws-ano-cpu           pic 9(04).
             10 ws-mes-cpu           pic 9(02).
             10 ws-dia-cpu           pic 9(02).
          05 filler                  pic x(13).


       01 file-details.
          05 file-size               pic x(8) comp-x.
          05 file-date.
             10 dia                  pic x comp-x.
             10 month                pic x comp-x.
             10 year                 pic x(2) comp-x.
          05 file-time.
             10 hours                pic x comp-x.
             10 minutes              pic x comp-x.
             10 seconds              pic x comp-x.
             10 hundredths           pic x comp-x.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           display erase at 0101.

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
                      DISPLAY "CODIGO-CA001 = " CODIGO-CA001 STOP " "

                      perform renomear-arquivos
                      perform abrir-arquivos
                      perform converter-arquivo
                      perform fechar-arquivos

                      display "ACABEI ESSA EMPRESA" STOP " "
                 end-read
           end-perform

           close cad001

           DISPLAY "ACABOU" STOP "  ".

           STOP " "

           stop run.

       renomear-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD050"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD050A

           OPEN I-O COD050A
           CLOSE    COD050A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD050"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD050

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD050A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD050A

           call "CBL_RENAME_FILE" using PATH-COD050
                                        PATH-COD050A
                              returning status-code

           STRING PATH-COD050 ".idx" DELIMITED BY " " INTO PATH-COD050

           STRING PATH-COD050A ".idx" DELIMITED BY " " INTO PATH-COD050A

           call "CBL_RENAME_FILE" using PATH-COD050
                                        PATH-COD050A
                              returning status-code.
       renomear-arquivos-fim.
           exit.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD050"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD050

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD050A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD050A

           open i-o   cod050
           close      cod050
           open i-o   cod050

           open input cod050a.

       converter-arquivo section.

           INITIALIZE REG-COD050A
           START COD050A KEY IS NOT LESS CHAVE-CO50A INVALID KEY
                MOVE "10" TO ST-COD050A.

           PERFORM UNTIL ST-COD050A = "10"
                READ COD050A NEXT AT END
                     MOVE "10" TO ST-COD050A
                NOT AT END
                     DISPLAY "REG-COD050A = " REG-COD050A

                     INITIALIZE REG-COD050

                     MOVE NR-CONTRATO-CO50A    TO NR-CONTRATO-CO50
                     MOVE ITEM-CO50A           TO ITEM-CO50
                     MOVE CODBRINDE-CO50A      TO CODBRINDE-CO50
                     MOVE CURSO-CO50A          TO CURSO-CO50
                     MOVE TURMA-CO50A          TO TURMA-CO50
                     MOVE QTDE-POR-FORM-CO50A  TO QTDE-POR-FORM-CO50
                     MOVE QTDE-FORM-CO50A      TO QTDE-FORM-CO50
                     MOVE CUSTO-UNIT-CO50A     TO CUSTO-UNIT-CO50
                     MOVE VALOR-PREVISTO-CO50A TO VALOR-PREVISTO-CO50
                     MOVE DATA-VENCTO-CO50A    TO DATA-VENCTO-CO50
                     MOVE DATA-SOLICIT-CO50A   TO DATA-SOLICIT-CO50
                     MOVE SUSP-PREV-DEF-CO50A  TO SUSP-PREV-DEF-CO50
                     MOVE VALOR-PAGO-CO50A     TO VALOR-PAGO-CO50
                     MOVE DATA-PAGTO-CO50A     TO DATA-PAGTO-CO50
                     MOVE REALIZADO-CO50A      TO REALIZADO-CO50
                     MOVE DIAS-PRAZO-CO50A     TO DIAS-PRAZO-CO50
                     MOVE COD-FORNEC-CO50A     TO COD-FORNEC-CO50
                     MOVE DATA-PAGTO-CO50A     TO DATA-ENVIO-CO50
                     MOVE SPACES               TO OBSERVACAO-CO50


                     WRITE REG-COD050 INVALID KEY
                           MOVE "Erro de Gravação...COD050" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close cod050 cod050a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
