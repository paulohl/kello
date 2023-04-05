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

           COPY VIPX100.

           SELECT VID100A ASSIGN TO PATH-VID100A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-VID100A
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-V100A = DATA-MOVTO-V100A
                                              SEQ-V100A
                  ALTERNATE RECORD KEY IS ALT-V100A = NR-FITAS-V100A
                            DATA-EVENTO-V100A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-V100A = CONTRATO-V100A
                            DATA-EVENTO-V100A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-V100A =
                            CINEGRAFISTA-V100A
                            DATA-EVENTO-V100A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-V100A = DATA-EVENTO-V100A
                            DATA-MOVTO-V100A SEQ-V100A
                  ALTERNATE RECORD KEY IS ALT4-V100A =
                            IDENTIFICADOR-V100A
                            DATA-EVENTO-V100A
                            WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY VIPW100.

      *MOVIMENTO E RECEBIMENTO DE FITAS BRUTAS
       FD  VID100A.
       01  REG-VID100A.
           05  DATA-MOVTO-V100A          PIC 9(8).
           05  SEQ-V100A                 PIC 9(3).
           05  NR-FITAS-V100A.
               10 CONTRATO-V100A         PIC 9(4).
               10 NR-FITA-V100A          PIC 9(5).
           05  CURSO-V100A               PIC X(10).
           05  EVENTO-V100A              PIC 999.
           05  DATA-EVENTO-V100A         PIC 9(8).
           05  CINEGRAFISTA-V100A        PIC 9(6).
           05  FILMADORA-V100A           PIC 9(2).
           05  LOCALIZACAO-V100A         PIC X(5).
           05  DIGITADOR-V100A           PIC X(4).
           05  IDENTIFICADOR-V100A       PIC 9(09).
           05  QTDE-ARQUIVOS-V100A       PIC 9(03).
           05  FILLER                    PIC X(88).


       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-VID100             PIC XX       VALUE SPACES.
           05  ST-VID100A            PIC XX       VALUE SPACES.
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
           MOVE "VID100"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-VID100A

           OPEN I-O VID100A
           CLOSE    VID100A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "VID100"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-VID100

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "VID100W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-VID100A

           call "CBL_RENAME_FILE" using PATH-VID100
                                        PATH-VID100A
                              returning status-code

           STRING PATH-VID100 ".idx" DELIMITED BY " " INTO PATH-VID100

           STRING PATH-VID100A ".idx" DELIMITED BY " " INTO PATH-VID100A

           call "CBL_RENAME_FILE" using PATH-VID100
                                        PATH-VID100A
                              returning status-code.
       renomear-arquivos-fim.
           exit.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "VID100"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-VID100

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "VID100W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-VID100A

           open i-o   vid100
           close      vid100
           open i-o   vid100

           open input vid100a.

       converter-arquivo section.

           INITIALIZE REG-VID100A
           START VID100A KEY IS NOT LESS CHAVE-V100A INVALID KEY
                MOVE "10" TO ST-VID100A.

           PERFORM UNTIL ST-VID100A = "10"
                READ VID100A NEXT AT END
                     MOVE "10" TO ST-VID100A
                NOT AT END
                     DISPLAY "REG-VID100A = " REG-VID100A

                     MOVE DATA-MOVTO-V100A    TO DATA-MOVTO-V100
                     MOVE SEQ-V100A           TO SEQ-V100
                     MOVE CONTRATO-V100A      TO CONTRATO-V100
                     MOVE NR-FITA-V100A       TO NR-FITA-V100
                     MOVE CURSO-V100A         TO CURSO-V100
                     MOVE EVENTO-V100A        TO EVENTO-V100
                     MOVE DATA-EVENTO-V100A   TO DATA-EVENTO-V100
                     MOVE CINEGRAFISTA-V100A  TO CINEGRAFISTA-V100
                     MOVE FILMADORA-V100A     TO FILMADORA-V100
                     MOVE LOCALIZACAO-V100A   TO LOCALIZACAO-V100
                     MOVE DIGITADOR-V100A     TO DIGITADOR-V100
                     MOVE IDENTIFICADOR-V100A TO IDENTIFICADOR-V100
                     MOVE QTDE-ARQUIVOS-V100A TO QTDE-ARQUIVOS-V100

                     WRITE REG-VID100 INVALID KEY
                           MOVE "Erro de Gravação...VID100" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close vid100 vid100a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
