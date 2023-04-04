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

           COPY REPX302.

           SELECT RED302A ASSIGN TO PATH-RED302A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RED302A
                  RECORD KEY IS CHAVE-R302A = DOCTO-R302A EVENTO-R302A
                  ALTERNATE RECORD KEY IS EVENTO-R302A WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY REPW302.

       FD  RED302A.
       01  REG-RED302A.
           05  DOCTO-R302A          PIC 9(6).
           05  EVENTO-R302A         PIC 999.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-RED302             PIC XX       VALUE SPACES.
           05  ST-RED302A            PIC XX       VALUE SPACES.
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
           MOVE "RED302"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED302A

           OPEN I-O RED302A
           CLOSE    RED302A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED302"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED302

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED302W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED302A

           call "CBL_RENAME_FILE" using PATH-RED302
                                        PATH-RED302A
                              returning status-code

           STRING PATH-RED302 ".idx" DELIMITED BY " " INTO PATH-RED302

           STRING PATH-RED302A ".idx" DELIMITED BY " " INTO PATH-RED302A

           call "CBL_RENAME_FILE" using PATH-RED302
                                        PATH-RED302A
                              returning status-code.
       renomear-arquivos-fim.
           exit.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED302"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED302

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED302W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED302A

           open i-o   red302
           close      red302
           open i-o   red302

           open input red302a.

       converter-arquivo section.

           INITIALIZE REG-RED302A
           START RED302A KEY IS NOT LESS CHAVE-R302A INVALID KEY
                MOVE "10" TO ST-RED302A.

           PERFORM UNTIL ST-RED302A = "10"
                READ RED302A NEXT AT END
                     MOVE "10" TO ST-RED302A
                NOT AT END
                     DISPLAY "REG-RED302A = " REG-RED302A

                     MOVE DOCTO-R302A      TO DOCTO-R302
                     MOVE EVENTO-R302A     TO EVENTO-R302

                     WRITE REG-RED302 INVALID KEY
                           MOVE "Erro de Gravação...RED302" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close red302 red302a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
