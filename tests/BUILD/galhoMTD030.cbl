       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOMTD030.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14-02-2011
      *DESCRIÇÃO: Conversão GALHOMTD030

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY MTPX030.

           SELECT MTD030A ASSIGN TO PATH-MTD030A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD030A
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-MT30A = ALBUM-MT30A,
                                       ITEM-EVENTO-MT30A,
                                       COD-EVENTO-MT30A
                  ALTERNATE RECORD KEY IS ALT-MT30A =
                            COD-EVENTO-MT30A,
                            ITEM-EVENTO-MT30A,
                            ALBUM-MT30A.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY MTPW030.

      *Formando x evento x qtde de imagens
       FD  MTD030A.
       01  REG-MTD030A.
           05  ALBUMMT30A.
               10  CONTRATO-MT30A   PIC 9(4).
               10  SEQ-MT30A        PIC 9(4).
           05  ALBUM-MT30A REDEFINES ALBUMMT30A PIC 9(8).
           05  ITEM-EVENTO-MT30A    PIC 9(3).
           05  COD-EVENTO-MT30A     PIC 9(3).
           05  QTDE-IMAGENS-MT30A   PIC 9(6).
           05  FILLER               PIC X(20).

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-MTD030             PIC XX       VALUE SPACES.
           05  ST-MTD030A            PIC XX       VALUE SPACES.
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
           MOVE "MTD030"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD030A

           OPEN I-O MTD030A
           CLOSE    MTD030A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "MTD030"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD030

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "MTD030W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD030A

           call "CBL_RENAME_FILE" using PATH-MTD030
                                        PATH-MTD030A
                              returning status-code

           STRING PATH-MTD030 ".idx" DELIMITED BY " " INTO PATH-MTD030

           STRING PATH-MTD030A ".idx" DELIMITED BY " " INTO PATH-MTD030A

           call "CBL_RENAME_FILE" using PATH-MTD030
                                        PATH-MTD030A
                              returning status-code.
       renomear-arquivos-fim.
           exit.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "MTD030"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD030

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "MTD030W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD030A

           open i-o   mtd030
           close      mtd030
           open i-o   mtd030

           open input mtd030a.

       converter-arquivo section.

           INITIALIZE REG-MTD030A
           START MTD030A KEY IS NOT LESS CHAVE-MT30A INVALID KEY
                MOVE "10" TO ST-MTD030A.

           PERFORM UNTIL ST-MTD030A = "10"
                READ MTD030A NEXT AT END
                     MOVE "10" TO ST-MTD030A
                NOT AT END
                     DISPLAY "REG-MTD030A = " REG-MTD030A

                     MOVE CONTRATO-MT30A     TO CONTRATO-MT30
                     MOVE SEQ-MT30A          TO SEQ-MT30
                     MOVE ITEM-EVENTO-MT30A  TO ITEM-EVENTO-MT30
                     MOVE COD-EVENTO-MT30A   TO COD-EVENTO-MT30
                     MOVE QTDE-IMAGENS-MT30A TO QTDE-IMAGENS-MT30

                     WRITE REG-MTD030 INVALID KEY
                           MOVE "Erro de Gravação...MTD030" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close mtd030 mtd030a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
