       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOLBD100.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14-02-2011
      *DESCRIÇÃO: Conversão GALHOLBD100

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY LBPX100.

           SELECT LBD100A ASSIGN TO PATH-LBD100A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-LBD100A
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-L100A = DATA-MOVTO-L100A SEQ-L100A
                  ALTERNATE RECORD KEY IS ALT1-L100A = CONTRATO-L100A
                     FOTOGRAFO-L100A WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY LBPW100.

      * MOVIMENTO DE RECEBIMENTO DE FILMES - LABORATÓRIO
       FD  LBD100A.
       01  REG-LBD100A.
           05  DATA-MOVTO-L100A          PIC 9(8).
           05  SEQ-L100A                 PIC 9(3).
           05  CONTRATO-L100A            PIC 9(4).
           05  EVENTO-L100A              PIC 999.
           05  DATA-EVENTO-L100A         PIC 9(8).  *> DD/MM/AAAA
           05  FOTOGRAFO-L100A           PIC 9(6).
           05  QTDE-FILMES-L100A         PIC 9(4).
           05  TIPO-FILME-L100A          PIC 9(2).
           05  DIGITADOR-L100A           PIC X(4).
           05  IDENTIFICADOR-L100A       PIC 9(9).
           05  FILLER                    PIC X(50).

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-LBD100             PIC XX       VALUE SPACES.
           05  ST-LBD100A            PIC XX       VALUE SPACES.
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
           MOVE "LBD100"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-LBD100A

           OPEN I-O LBD100A
           CLOSE    LBD100A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "LBD100"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-LBD100

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "LBD100W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-LBD100A

           call "CBL_RENAME_FILE" using PATH-LBD100
                                        PATH-LBD100A
                              returning status-code

           STRING PATH-LBD100 ".idx" DELIMITED BY " " INTO PATH-LBD100

           STRING PATH-LBD100A ".idx" DELIMITED BY " " INTO PATH-LBD100A

           call "CBL_RENAME_FILE" using PATH-LBD100
                                        PATH-LBD100A
                              returning status-code.
       renomear-arquivos-fim.
           exit.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "LBD100"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-LBD100

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "LBD100W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-LBD100A

           open i-o   lbd100
           close      lbd100
           open i-o   lbd100

           open input lbd100a.

       converter-arquivo section.

           INITIALIZE REG-LBD100A
           START LBD100A KEY IS NOT LESS CHAVE-L100A INVALID KEY
                MOVE "10" TO ST-LBD100A.

           PERFORM UNTIL ST-LBD100A = "10"
                READ LBD100A NEXT AT END
                     MOVE "10" TO ST-LBD100A
                NOT AT END
                     DISPLAY "REG-LBD100A = " REG-LBD100A

                     MOVE DATA-MOVTO-L100A    TO DATA-MOVTO-L100
                     MOVE SEQ-L100A           TO SEQ-L100
                     MOVE CONTRATO-L100A      TO CONTRATO-L100
                     MOVE EVENTO-L100A        TO EVENTO-L100
                     MOVE DATA-EVENTO-L100A   TO DATA-EVENTO-L100
                     MOVE FOTOGRAFO-L100A     TO FOTOGRAFO-L100
                     MOVE QTDE-FILMES-L100A   TO QTDE-FILMES-L100
                     MOVE TIPO-FILME-L100A    TO TIPO-FILME-L100
                     MOVE DIGITADOR-L100A     TO DIGITADOR-L100
                     MOVE IDENTIFICADOR-L100A TO IDENTIFICADOR-L100

                     WRITE REG-LBD100 INVALID KEY
                           MOVE "Erro de Gravação...LBD100" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close lbd100 lbd100a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
