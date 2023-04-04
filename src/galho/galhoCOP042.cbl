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
           COPY COPX042.

           SELECT COD042A ASSIGN TO PATH-COD042A
                  ORGANIZATION INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS SEQ-CO42A
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  ALTERNATE RECORD KEY IS IDENTIFICACAO-CO42A
                             WITH DUPLICATES
                  STATUS IS ST-COD042A.

           COPY COPX043.

           SELECT COD043A ASSIGN TO PATH-COD043A
                  ORGANIZATION INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CHAVE-CO43A = SEQ-CO43A ITEM-CO43A
                  STATUS IS ST-COD043A.

      *    COPY MTPX021.
      *
      *    SELECT  MTD021A ASSIGN TO PATH-MTD021A
      *            ORGANIZATION IS INDEXED
      *            ACCESS MODE IS DYNAMIC
      *            LOCK MODE IS MANUAL WITH LOCK ON RECORD
      *            STATUS IS ST-MTD021A
      *            RECORD KEY IS CONTRATO-MT21A
      *            ALTERNATE RECORD KEY IS CHAVE-PRI-MT21A =
      *                                    CAMPANHA-MT21A
      *                                    PRIORIDADE-MT21A
      *                                    CONTRATO-MT21A.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY COPW042.

       FD  COD042A.
       01  REG-COD042A.
           05  SEQ-CO42A            PIC 9(3).
           05  IDENTIFICACAO-CO42A  PIC X(12).
           05  INSTITUICAO-CO42A    PIC X(15).
           05  CIDADE-CO42A         PIC X(13).
           05  QT-FORM-CO42A        PIC 9(4).
           05  PADRAO-CO42A         PIC X.
           05  QT-FOTOS-CO42A       PIC 9(6).
           05  MESANO-CO42A         PIC 9(6).
           05  ASSINATURA-CO42A     PIC 9(8).
           05  VLR-COMISSAO-CO42A   PIC 9(8)V99.
           05  REPRESENTANTE-CO42A  PIC X(15).

           COPY COPW043.

      *  CADASTRO DE BRINDES P/ ARQUIVO SIMULADO
      *  COD043 É COMPLEMENTO DO COD042
       FD  COD043A.
       01  REG-COD043A.
      * SEQ-CO43 QUE FAZ RELACAO COM SEQ-CO42.
           05  SEQ-CO43A            PIC 9(3).
           05  ITEM-CO43A           PIC 9(2).
           05  BRINDE-CO43A         PIC 9(3).
           05  QTDE-BRINDE-CO43A    PIC 9(5).
           05  DATA-PAGTO-CO43A     PIC 9(8).
      *    DATA-PAGTO-CO43 - AAAAMMDD
           05  CUSTO-PREVISTO-CO43A PIC 9(8)V99.
           05  DEB-CRED-CO43A       PIC X.

      *    COPY MTPW021.

      *ARQUIVO DE PRIORIODADE P/ PRODUÇÃO
      *FD  MTD021A.
      *01  REG-MTD021A.
      *    05  CONTRATO-MT21A     PIC 9(4).
      *    05  PRIORIDADE-MT21A   PIC 999.
      *    05  TAMANHO-MT21A      PIC X(7).
      *    05  OBS-MT21A          PIC X(40).
      *    05  ORDEM-MT21A        PIC 9(4).
      *    05  CAMPANHA-MT21A     PIC 9(2).

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-COD042             PIC XX       VALUE SPACES.
           05  ST-COD042A            PIC XX       VALUE SPACES.
           05  ST-COD043             PIC XX       VALUE SPACES.
           05  ST-COD043A            PIC XX       VALUE SPACES.
           05  ST-MTD021             PIC XX       VALUE SPACES.
           05  ST-MTD021A            PIC XX       VALUE SPACES.
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
           MOVE "COD042"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD042A

           OPEN I-O COD042A
           CLOSE    COD042A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD042"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD042

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD042A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD042A

           call "CBL_RENAME_FILE" using PATH-COD042
                                        PATH-COD042A
                              returning status-code

           STRING PATH-COD042 ".idx" DELIMITED BY " " INTO PATH-COD042

           STRING PATH-COD042A ".idx" DELIMITED BY " " INTO PATH-COD042A

           call "CBL_RENAME_FILE" using PATH-COD042
                                        PATH-COD042A
                              returning status-code.


           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD043"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD043A

           OPEN I-O COD043A
           CLOSE    COD043A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD043"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD043

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD043A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD043A

           call "CBL_RENAME_FILE" using PATH-COD043
                                        PATH-COD043A
                              returning status-code

           STRING PATH-COD043 ".idx" DELIMITED BY " " INTO PATH-COD043

           STRING PATH-COD043A ".idx" DELIMITED BY " " INTO PATH-COD043A

           call "CBL_RENAME_FILE" using PATH-COD043
                                        PATH-COD043A
                              returning status-code.


      *    MOVE CODIGO-CA001           TO EMP-REC
      *    MOVE "MTD021"               TO ARQ-REC
      *    MOVE EMPRESA-REF            TO PATH-MTD021A
      *
      *    OPEN I-O MTD021A
      *    CLOSE    MTD021A
      *
      *    MOVE CODIGO-CA001           TO EMP-REC
      *    MOVE "MTD021"               TO ARQ-REC
      *    MOVE EMPRESA-REF            TO PATH-MTD021
      *
      *    MOVE CODIGO-CA001           TO EMP-REC
      *    MOVE "MTD021A"              TO ARQ-REC
      *    MOVE EMPRESA-REF            TO PATH-MTD021A
      *
      *    call "CBL_RENAME_FILE" using PATH-MTD021
      *                                 PATH-MTD021A
      *                       returning status-code
      *
      *    STRING PATH-MTD021 ".idx" DELIMITED BY " " INTO PATH-MTD021
      *
      *    STRING PATH-MTD021A ".idx" DELIMITED BY " " INTO PATH-MTD021A
      *
      *    call "CBL_RENAME_FILE" using PATH-MTD021
      *                                 PATH-MTD021A
      *                       returning status-code.
       renomear-arquivos-fim.
           exit.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD042"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD042

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD042A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD042A

           open i-o   cod042
           close      cod042
           open i-o   cod042

           open input cod042a.


           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD043"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD043

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD043A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD043A

           open i-o   cod043
           close      cod043
           open i-o   cod043

           open input cod043a.


      *    MOVE CODIGO-CA001           TO EMP-REC
      *    MOVE "MTD021"               TO ARQ-REC
      *    MOVE EMPRESA-REF            TO PATH-MTD021
      *
      *    MOVE CODIGO-CA001           TO EMP-REC
      *    MOVE "MTD021A"              TO ARQ-REC
      *    MOVE EMPRESA-REF            TO PATH-MTD021A
      *
      *    open i-o   mtd021
      *    close      mtd021
      *    open i-o   mtd021
      *
      *    open input mtd021.
       abrir-arquivos-fim.
           exit.

       converter-arquivo section.
           initialize reg-cod042a
           start cod042a key is not less seq-co42a invalid key
                 move "10" to st-cod042a.

           perform until st-cod042a = "10"
                 read cod042a next at end
                      move "10" to st-cod042a
                 not at end
                      move seq-co42a            to seq-co42
                      move identificacao-co42a  to identificacao-co42
                      move instituicao-co42a    to instituicao-co42
                      move cidade-co42a         to cidade-co42
                      move qt-form-co42a        to qt-form-co42
                      move padrao-co42a         to padrao-co42
                      move qt-fotos-co42a       to qt-fotos-co42
                      move mesano-co42a         to mesano-co42
                      move assinatura-co42a     to assinatura-co42
                      move vlr-comissao-co42a   to vlr-comissao-co42
                      move representante-co42a  to representante-co42

                      display "reg-cod042 = " reg-cod042
                      write reg-cod042 invalid key
                           move "Erro de Gravação...COD042" to mensagem
                           move "C" to tipo-msg
                           perform exibir-mensagem
                      end-write
                 end-read
           end-perform.

           initialize reg-cod043a
           start cod043a key is not less chave-co43a invalid key
                 move "10" to st-cod043a.

           perform until st-cod043a = "10"
                 read cod043a next at end
                      move "10" to st-cod043a
                 not at end
                      move seq-co43a            to seq-co43
                      move item-co43a           to item-co43
                      move brinde-co43a         to brinde-co43
                      move qtde-brinde-co43a    to qtde-brinde-co43
                      move data-pagto-co43a     to data-pagto-co43
                      move custo-previsto-co43a to custo-previsto-co43
                      move deb-cred-co43a       to deb-cred-co43

                      display "reg-cod043 = " reg-cod043
                      write reg-cod043 invalid key
                           move "Erro de Gravação...COD043" to mensagem
                           move "C" to tipo-msg
                           perform exibir-mensagem
                      end-write
                 end-read
           end-perform.

      *    initialize reg-mtd021a
      *    start mtd021a key is not less contrato-mt21a invalid key
      *          move "10" to st-mtd021a.
      *
      *    perform until st-mtd021a = "10"
      *          read mtd021a next at end
      *               move "10" to st-mtd021a
      *          not at end
      *               MOVE CONTRATO-MT21A    TO CONTRATO-MT21
      *               MOVE PRIORIDADE-MT21A  TO PRIORIDADE-MT21
      *               MOVE TAMANHO-MT21A     TO TAMANHO-MT21
      *               MOVE OBS-MT21A         TO OBS-MT21
      *               MOVE ORDEM-MT21A       TO ORDEM-MT21
      *               MOVE CAMPANHA-MT21A    TO CAMPANHA-MT21
      *
      *               display "reg-mtd021 = " reg-mtd021
      *               write reg-mtd021 invalid key
      *                    move "Erro de Gravação...MTD021" to mensagem
      *                    move "C" to tipo-msg
      *                    perform exibir-mensagem
      *               end-write
      *          end-read
      *    end-perform.

       converter-arquivo-fim.
           exit.

       fechar-arquivos section.
           close cod042 cod042a
           close cod043 cod043a.
      *    close mtd021 mtd021a.
       fechar-arquivos-fim.
           exit.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
