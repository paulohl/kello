       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCOP042.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14-05-2010
      *DESCRIÇÃO: Conversão COD042

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY REPX002.

           SELECT RED002A ASSIGN TO PATH-RED002A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RED002A
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-RE02A
                  ALTERNATE RECORD KEY IS DESCRICAO-RE02A.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY REPW002.

      *CADASTRO DE FUNÇÕES - REPORTAGEM
       FD  RED002A.
       01  REG-RED002A.
           05  CODIGO-RE02A        PIC 9(2).
           05  DESCRICAO-RE02A     PIC X(20).

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-RED002             PIC XX       VALUE SPACES.
           05  ST-RED002A            PIC XX       VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  STATUS-CODE           PIC X(02) COMP-5.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
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

           STOP RUN.

       renomear-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED002"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED002A

           OPEN I-O RED002A
           CLOSE    RED002A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED002"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED002

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED002A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED002A

           call "CBL_RENAME_FILE" using PATH-RED002
                                        PATH-RED002A
                              returning status-code

           STRING PATH-RED002 ".idx" DELIMITED BY " " INTO PATH-RED002

           STRING PATH-RED002A ".idx" DELIMITED BY " " INTO PATH-RED002A

           call "CBL_RENAME_FILE" using PATH-RED002
                                        PATH-RED002A
                              returning status-code.
       renomear-arquivos-fim.
           exit.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED002"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED002

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RED002A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RED002A

           open i-o   red002
           close      red002
           open i-o   red002

           open input red002a.

       abrir-arquivos-fim.
           exit.

       converter-arquivo section.
           initialize reg-red002a
           start red002a key is not less codigo-re02a invalid key
                 move "10" to st-red002a.

           perform until st-red002a = "10"
                 read red002a next at end
                      move "10" to st-red002a
                 not at end
                      move codigo-re02a         to codigo-re02
                      move descricao-re02a      to descricao-re02
                      move zeros                to acumular-re02

                      display "reg-red002 = " reg-red002
                      write reg-red002 invalid key
                           move "Erro de Gravação...RED002" to mensagem
                           move "C" to tipo-msg
                           perform exibir-mensagem
                      end-write
                 end-read
           end-perform.

       converter-arquivo-fim.
           exit.

       fechar-arquivos section.
           close red002 red002a.
       fechar-arquivos-fim.
           exit.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
