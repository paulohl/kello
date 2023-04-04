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

           COPY REPX101.

           SELECT RED101A ASSIGN TO PATH-RED101A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RED101A
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-R101A = DOCTO-R101A
                                              CONTRATO-R101A
                                              EVENTO-R101A
                  ALTERNATE RECORD KEY IS ALT1-R101A = DOCTO-R101A
                                                       EVENTO-R101A
                                                       CONTRATO-R101A
                  ALTERNATE RECORD KEY IS CONTRATO-R101A
                            WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY REPW101.

      *CADASTRO DE EVENTOS E CONTRATOS POR DOCTO-REPORTAGEM
       FD  RED101A.
       01  REG-RED101A.
           05  DOCTO-R101A          PIC 9(6).
           05  CONTRATO-R101A       PIC 9(4).
           05  EVENTO-R101A         PIC 9(3).
           05  QT-PARTIC-R101A      PIC 9(4).


       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-RED101A            PIC XX       VALUE SPACES.
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
           MOVE "RED101"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED101A

           OPEN I-O RED101A
           CLOSE    RED101A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED101"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED101

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED101W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED101A

           call "CBL_RENAME_FILE" using PATH-RED101
                                        PATH-RED101A
                              returning status-code

           STRING PATH-RED101 ".idx" DELIMITED BY " " INTO PATH-RED101

           STRING PATH-RED101A ".idx" DELIMITED BY " " INTO PATH-RED101A

           call "CBL_RENAME_FILE" using PATH-RED101
                                        PATH-RED101A
                              returning status-code.
       renomear-arquivos-fim.
           exit.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED101"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED101

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED101W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED101A

           open i-o   red101
           close      red101
           open i-o   red101

           open input red101a.

       converter-arquivo section.

           INITIALIZE REG-RED101A
           START RED101A KEY IS NOT LESS CHAVE-R101A INVALID KEY
                MOVE "10" TO ST-RED101A.

           PERFORM UNTIL ST-RED101A = "10"
                READ RED101A NEXT AT END
                     MOVE "10" TO ST-RED101A
                NOT AT END
                     DISPLAY "REG-RED101A = " REG-RED101A

                     MOVE DOCTO-R101A            TO DOCTO-R101
                     MOVE CONTRATO-R101A         TO CONTRATO-R101
                     MOVE EVENTO-R101A           TO EVENTO-R101
                     MOVE QT-PARTIC-R101A        TO QT-PARTIC-R101

                     WRITE REG-RED101 INVALID KEY
                           MOVE "Erro de Gravação...RED101" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close red101 red101a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
