       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOMTD001.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14-05-2010
      *DESCRIÇÃO: Conversão MTD001

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY MTPX001.

           SELECT MTD001A ASSIGN       TO   PATH-MTD001A
                          ORGANIZATION IS        INDEXED
                          ACCESS MODE  IS        DYNAMIC
                          LOCK MODE    IS      AUTOMATIC
                          WITH LOCK    ON         RECORD
                          STATUS       IS     ST-MTD001A
                          RECORD KEY   IS CONTRATO-MT01A.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY MTPW001.

      *CADASTRO DE CONTRATO P/ MONTAGEM
       FD  MTD001A.
       01  REG-MTD001A.
           05  CONTRATO-MT01A       PIC 9(4).
           05  GUIA-MT01A           PIC 9(5).
           05  PRODUZIDA-MT01A      PIC 9(5).
           05  MONTADA-MT01A        PIC 9(5).
           05  PERDIDA-MT01A        PIC 9(5).
           05  AVULSA-MT01A         PIC 9(5).
           05  CLIEN-ALBUM-MT01A    PIC 9999.
           05  CODALBUM-MT01A       PIC 9(5).
           05  CODFOLHA-MT01A       PIC 9(5).
           05  CODSEDA-MT01A        PIC 9(5).
           05  CODFOTO-MT01A        PIC 9(5).

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-MTD001             PIC XX       VALUE SPACES.
           05  ST-MTD001A            PIC XX       VALUE SPACES.
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
           MOVE "MTD001"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD001A

           OPEN I-O MTD001A
           CLOSE    MTD001A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "MTD001"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD001

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "MTD001A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD001A

           call "CBL_RENAME_FILE" using PATH-MTD001
                                        PATH-MTD001A
                              returning status-code

           STRING PATH-MTD001 ".idx" DELIMITED BY " " INTO PATH-MTD001

           STRING PATH-MTD001A ".idx" DELIMITED BY " " INTO PATH-MTD001A

           call "CBL_RENAME_FILE" using PATH-MTD001
                                        PATH-MTD001A
                              returning status-code.
       renomear-arquivos-fim.
           exit.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "MTD001"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD001

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "MTD001A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD001A

           open i-o   mtd001
           close      mtd001
           open i-o   mtd001

           open input mtd001a.
       abrir-arquivos-fim.
           exit.

       converter-arquivo section.
           initialize reg-mtd001a
           start mtd001a key is not less contrato-mt01a invalid key
                 move "10" to st-mtd001a.

           perform until st-mtd001a = "10"
                 read mtd001a next at end
                      move "10" to st-mtd001a
                 not at end
                      move CONTRATO-MT01A      to CONTRATO-MT01
                      move GUIA-MT01A          to GUIA-MT01
                      move PRODUZIDA-MT01A     to PRODUZIDA-MT01
                      move MONTADA-MT01A       to MONTADA-MT01
                      move PERDIDA-MT01A       to PERDIDA-MT01
                      move AVULSA-MT01A        to AVULSA-MT01
                      move CLIEN-ALBUM-MT01A   to CLIEN-ALBUM-MT01
                      move CODALBUM-MT01A      to CODALBUM-MT01
                      move CODFOLHA-MT01A      to CODFOLHA-MT01
                      move CODSEDA-MT01A       to CODSEDA-MT01
                      move CODFOTO-MT01A       to CODFOTO-MT01

                      display "reg-mtd001 = " reg-mtd001
                      write reg-mtd001 invalid key
                           move "Erro de Gravação...MTD001" to mensagem
                           move "C" to tipo-msg
                           perform exibir-mensagem
                      end-write
                 end-read
           end-perform.
       converter-arquivo-fim.
           exit.

       fechar-arquivos section.
           close mtd001 mtd001a.
       fechar-arquivos-fim.
           exit.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
