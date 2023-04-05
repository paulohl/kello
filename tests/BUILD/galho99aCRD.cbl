       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO99.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 03/11/2009.
      *FUNÇÃO: AUMENTAR O TAMANHO DO REGISTRO CHD010

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CRPX020.
           COPY CRPX020B.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CRPW020.
       COPY CRPW020B.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.

           05  CUSTO-PREVISTO-W      PIC 9(8)V99  VALUE ZEROS.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  SEM-CRD020B           PIC X(01).
           05  TOTAL-BAIXA           PIC 9(09)V99 VALUE ZEROS.

           COPY "PARAMETR".

       01  EMP-REFERENCIA.
           05  FILLER            PIC X(15)
               VALUE "\PROGRAMA\KELLO".
           05  VAR1              PIC X VALUE "\".
           05  EMP-REC           PIC XXX.
           05  VAR2              PIC X VALUE "\".
           05  ARQ-REC           PIC X(10).
       01  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
       01  resposta              pic x(01).

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           display erase                      at 0101
           display "Deseja Atualizar apenas 1 empresa (S/N) ? "
                at 0101
           accept resposta                    at 0143

           move function upper-case(resposta) to resposta

           evaluate resposta
               when "S"   perform uma-empresa
               when "N"   perform mais-empresas
               when other go to main-process.

       uma-empresa section.
           open i-o   cad001
           close      cad001
           open input cad001

           if st-cad001 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura...CAD001" x"0da0"
                     "Status . . . " st-cad001 into mensagem
              move "C" to tipo-msg
              perform exibir-mensagem
              close cad001
              stop run.

       informar-empresa.
           display "Informar o Codigo da Empresa: " at 0501
           accept codigo-ca001                      at 0531

           read cad001 invalid key
                display "Empresa Nao Encontrada"    at 1501
                stop " "
                go to informar-empresa
           not invalid key
                perform abrir-arquivos
                perform ajustar-saldo
                perform fechar-arquivos
           end-read

           close cad001

           DISPLAY "ACABOU" STOP "  ".

           STOP " "

           stop run.

       mais-empresas section.
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
                      perform abrir-arquivos
                      perform ajustar-saldo
                      perform fechar-arquivos
                      display "ACABEI ESSA EMPRESA" STOP " "
                 end-read
           end-perform

           close cad001

           DISPLAY "ACABOU" STOP "  ".

           STOP " "

           stop run.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CRD020"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CRD020

           display "path-crd020 = " at 0401
           display  path-crd020     at 0415
           stop " "

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CRD020B"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CRD020B

           OPEN I-O CRD020 CRD020B.

       ajustar-saldo section.

           INITIALIZE REG-CRD020
           START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                READ CRD020 NEXT AT END
                     MOVE "10" TO ST-CRD020
                NOT AT END
                     PERFORM VERIFICAR-CRD020B
                     DISPLAY "REG-CRD020 = " REG-CRD020
                     IF VALOR-SALDO-CR20 > VALOR-TOT-CR20
                        MOVE VALOR-TOT-CR20 TO VALOR-SALDO-CR20
                        MOVE 0              TO SITUACAO-CR20
                        REWRITE REG-CRD020
                     END-IF
                     IF SEM-CRD020B = "S"
                        MOVE VALOR-TOT-CR20 TO VALOR-SALDO-CR20
                        MOVE 0              TO SITUACAO-CR20
                        REWRITE REG-CRD020
                     ELSE
                        COMPUTE VALOR-SALDO-CR20 = VALOR-TOT-CR20 -
                                TOTAL-BAIXA
                        REWRITE REG-CRD020
                     END-IF
                END-READ
           END-PERFORM.

       VERIFICAR-CRD020B SECTION.
           MOVE "S" TO SEM-CRD020B

           INITIALIZE REG-CRD020B TOTAL-BAIXA
           MOVE COD-COMPL-CR20         TO COD-COMPL-CR20B
           MOVE SEQ-CR20               TO SEQ-CR20B
           START CRD020B KEY IS NOT LESS CHAVE-CR20B INVALID KEY
                MOVE "10" TO ST-CRD020B.

           PERFORM UNTIL ST-CRD020B = "10"
                READ CRD020B NEXT AT END
                     MOVE "10" TO ST-CRD020B
                NOT AT END
                     IF COD-COMPL-CR20 <> COD-COMPL-CR20B OR
                        SEQ-CR20       <> SEQ-CR20B
                        MOVE "10" TO ST-CRD020B
                     ELSE
                        ADD VALOR-BAIXA-CR20B TO TOTAL-BAIXA
                        MOVE "N" TO SEM-CRD020B
                     END-IF
                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close crd020 crd020b.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.

