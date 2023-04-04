       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCPAR001.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14-02-2011
      *DESCRIÇÃO: Conversão GALHOCPAR001

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY PARX001.

           SELECT PAR001A ASSIGN       TO  PATH-PAR001A
                          ORGANIZATION IS      INDEXED
                          ACCESS MODE  IS      DYNAMIC
                          LOCK MODE    IS    AUTOMATIC
                          WITH LOCK    ON       RECORD
                          STATUS       IS    ST-PAR001A
                          RECORD KEY   IS CHAVE-PAR001A.

           COPY CPARX001.

           SELECT CPAR001A ASSIGN       TO  PATH-CPAR001A
                           ORGANIZATION IS       INDEXED
                           ACCESS MODE  IS       DYNAMIC
                           LOCK MODE    IS     AUTOMATIC
                           WITH LOCK    ON        RECORD
                           STATUS       IS    ST-CPAR001A
                           RECORD KEY   IS CHAVE-CPAR001A.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY PARW001.

      * Arquivo de parametrizacao de codigos reduzidos
       FD  PAR001A.
       01  REG-PAR001A.
           05  CHAVE-PAR001A                  PIC 9(01).
           05  CODRED-BRINDE-PAR001A          PIC 9(05).
           05  COLACAO-GRAU-PAR001A           PIC 9(03).
           05  STATUS-ANALISE-PAR001A         PIC 9(02).
           05  STATUS-REVENDIDO-PAR001A       PIC 9(02).
           05  CODRED-FATURAMENTO-PAR001A     PIC 9(05).
           05  STATUS-TIT-FATURAMENTO-PAR001A PIC 9(02).
           05  CAMINHO-IMAGEM-PAR001A         PIC X(50).
           05  FORN-CHEQUE-PAR001A            PIC 9(06).
           05  CODRED-CHEQUE-PAR001A          PIC 9(05).
           05  FORN-RECEBER-PAR001A           PIC 9(06).
           05  CODRED-RECEBER-PAR001A         PIC 9(05).
           05  FILLER                         PIC X(14).

           COPY CPARW001.

      * Arquivo de parametrizacao de codigos reduzidos
      * (continuacao PAR001)

       FD  CPAR001A.
       01  REG-CPAR001A.
           05  CHAVE-CPAR001A                 PIC 9(01).
           05  CAMINHO-IMAGEM-CX-CPAR001A     PIC X(50).
           05  TABELA-COLACAOA OCCURS 30 TIMES.
               10 TAB-COLACAOA                PIC 9(03).
           05  FILLER                         PIC X(510).

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-CPAR001            PIC XX       VALUE SPACES.
           05  ST-PAR001A            PIC XX       VALUE SPACES.
           05  ST-CPAR001A           PIC XX       VALUE SPACES.
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
           05  IND                   PIC 9(2) VALUE ZEROS.

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
           MOVE "PAR001"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-PAR001A

           OPEN I-O PAR001A
           CLOSE    PAR001A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "PAR001"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-PAR001

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "PAR001W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-PAR001A

           call "CBL_RENAME_FILE" using PATH-PAR001
                                        PATH-PAR001A
                              returning status-code

           STRING PATH-PAR001 ".idx" DELIMITED BY " " INTO PATH-PAR001

           STRING PATH-PAR001A ".idx" DELIMITED BY " " INTO PATH-PAR001A

           call "CBL_RENAME_FILE" using PATH-PAR001
                                        PATH-PAR001A
                              returning status-code.

      *>>>>>>>>>>>
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CPAR001"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CPAR001A

           OPEN I-O CPAR001A
           CLOSE    CPAR001A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CPAR001"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CPAR001

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CPAR001W"             TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CPAR001A

           call "CBL_RENAME_FILE" using PATH-CPAR001
                                        PATH-CPAR001A
                              returning status-code

           STRING PATH-CPAR001 ".idx" DELIMITED BY " " INTO PATH-CPAR001

           STRING PATH-CPAR001A ".idx" DELIMITED BY " " INTO
                                                           PATH-CPAR001A

           call "CBL_RENAME_FILE" using PATH-CPAR001
                                        PATH-CPAR001A
                              returning status-code.

       renomear-arquivos-fim.
           exit.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "PAR001"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-PAR001

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "PAR001W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-PAR001A

           open i-o   par001
           close      par001
           open i-o   par001

           open input par001a.

      *>>>>>>>>>>>>>>>>>>>>>>>>>
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CPAR001"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CPAR001

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CPAR001W"             TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CPAR001A

           open i-o   cpar001
           close      cpar001
           open i-o   cpar001

           open input cpar001a.

       converter-arquivo section.

           INITIALIZE REG-PAR001A
           START PAR001A KEY IS NOT LESS CHAVE-PAR001A INVALID KEY
                MOVE "10" TO ST-PAR001A.

           PERFORM UNTIL ST-PAR001A = "10"
                READ PAR001A NEXT AT END
                     MOVE "10" TO ST-PAR001A
                NOT AT END
                     DISPLAY "REG-PAR001A = " REG-PAR001A

                     MOVE CHAVE-PAR001A                   TO
                          CHAVE-PAR001
                     MOVE CODRED-BRINDE-PAR001A           TO
                          CODRED-BRINDE-PAR001
                     MOVE COLACAO-GRAU-PAR001A            TO
                          COLACAO-GRAU-PAR001
                     MOVE STATUS-ANALISE-PAR001A          TO
                          STATUS-ANALISE-PAR001
                     MOVE STATUS-REVENDIDO-PAR001A        TO
                          STATUS-REVENDIDO-PAR001
                     MOVE CODRED-FATURAMENTO-PAR001A      TO
                          CODRED-FATURAMENTO-PAR001
                     MOVE STATUS-TIT-FATURAMENTO-PAR001A  TO
                          STATUS-TIT-FATURAMENTO-PAR001
                     MOVE CAMINHO-IMAGEM-PAR001A          TO
                          CAMINHO-IMAGEM-PAR001
                     MOVE FORN-CHEQUE-PAR001A             TO
                          FORN-CHEQUE-PAR001
                     MOVE CODRED-CHEQUE-PAR001A           TO
                          CODRED-CHEQUE-PAR001
                     MOVE FORN-RECEBER-PAR001A            TO
                          FORN-RECEBER-PAR001
                     MOVE CODRED-RECEBER-PAR001A          TO
                          CODRED-RECEBER-PAR001

                     WRITE REG-PAR001 INVALID KEY
                           MOVE "Erro de Gravação...PAR001" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.
      *>>>>>>>>>>>>>
           INITIALIZE REG-CPAR001A
           START CPAR001A KEY IS NOT LESS CHAVE-CPAR001A INVALID KEY
                MOVE "10" TO ST-CPAR001A.

           PERFORM UNTIL ST-CPAR001A = "10"
                READ CPAR001A NEXT AT END
                     MOVE "10" TO ST-CPAR001A
                NOT AT END
                     DISPLAY "REG-CPAR001A = " REG-CPAR001A

                     MOVE CHAVE-CPAR001A             TO
                          CHAVE-CPAR001
                     MOVE CAMINHO-IMAGEM-CX-CPAR001A TO
                          CAMINHO-IMAGEM-CX-CPAR001

                     MOVE 0 TO IND
                     PERFORM UNTIL IND = 30
                         ADD 1 TO IND
                         MOVE TAB-COLACAOA(IND) TO TAB-COLACAO(IND)
                     END-PERFORM

                     WRITE REG-CPAR001 INVALID KEY
                           MOVE "Erro de Gravação...CPAR001" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close par001 par001a cpar001 cpar001a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
