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

           COPY COPX002.

           SELECT  COD002A ASSIGN TO PATH-COD002A
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   LOCK MODE IS AUTOMATIC
                   WITH LOCK ON RECORD
                   STATUS IS ST-COD002A
                   RECORD KEY IS CODIGO-CO02A
                   ALTERNATE RECORD KEY IS NOME-CO02A WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY COPW002.

      * CADASTRO DE BRINDE
       FD  COD002A.
       01  REG-COD002A.
           05  CODIGO-CO02A      PIC 999.
           05  NOME-CO02A        PIC X(20).
           05  VALOR-CO02A       PIC 9(8)V99.
           05  MULT-FORM-CO02A   PIC 9.
      ***** MULT-FORM  = 1--> SIM  2-NÃO             *****
           05  GERAR-PAGAR-CO02A PIC 9.
      *    GERAR-PAGAR-CO02 = 1-SIM  2-NAO
           05  FORNEC1-CO02A     PIC 9(6).
           05  FORNEC2-CO02A     PIC 9(6).
           05  FORNEC3-CO02A     PIC 9(6).


       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD002A            PIC XX       VALUE SPACES.
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
           MOVE "COD002"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD002A

           OPEN I-O COD002A
           CLOSE    COD002A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD002"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD002

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD002A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD002A

           call "CBL_RENAME_FILE" using PATH-COD002
                                        PATH-COD002A
                              returning status-code

           STRING PATH-COD002 ".idx" DELIMITED BY " " INTO PATH-COD002

           STRING PATH-COD002A ".idx" DELIMITED BY " " INTO PATH-COD002A

           call "CBL_RENAME_FILE" using PATH-COD002
                                        PATH-COD002A
                              returning status-code.
       renomear-arquivos-fim.
           exit.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD002"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD002

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD002A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD002A

           open i-o   cod002
           close      cod002
           open i-o   cod002

           open input cod002a.

       converter-arquivo section.

           INITIALIZE REG-COD002A
           START COD002A KEY IS NOT LESS CODIGO-CO02A INVALID KEY
                MOVE "10" TO ST-COD002A.

           PERFORM UNTIL ST-COD002A = "10"
                READ COD002A NEXT AT END
                     MOVE "10" TO ST-COD002A
                NOT AT END
                     DISPLAY "REG-COD002A = " REG-COD002A

                     INITIALIZE REG-COD002

                     MOVE CODIGO-CO02A      TO CODIGO-CO02
                     MOVE NOME-CO02A        TO NOME-CO02
                     MOVE VALOR-CO02A       TO VALOR-CO02
                     MOVE MULT-FORM-CO02A   TO MULT-FORM-CO02
                     MOVE GERAR-PAGAR-CO02A TO GERAR-PAGAR-CO02
                     MOVE FORNEC1-CO02A     TO FORNEC1-CO02
                     MOVE FORNEC2-CO02A     TO FORNEC2-CO02
                     MOVE FORNEC3-CO02A     TO FORNEC3-CO02
                     MOVE 1                 TO SITUACAO-CO02

                     WRITE REG-COD002 INVALID KEY
                           MOVE "Erro de Gravação...COD002" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close cod002 cod002a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
