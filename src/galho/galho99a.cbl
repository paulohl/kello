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
           COPY CHPX010.
           COPY CHPX013.
           COPY CHPX010B.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CHPW010.
       COPY CHPW013.
       COPY CHPW010B.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CHD010B            PIC XX       VALUE SPACES.
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
           05  SEM-CHD010B           PIC X(01).
           05  JUROS                 PIC 9(06)V99  VALUE ZEROS.
           05  MULTA                 PIC 9(06)V99  VALUE ZEROS.
           05  DESCONTO              PIC 9(06)V99  VALUE ZEROS.
           05  VLR-BAIXA             PIC 9(06)V99  VALUE ZEROS.

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
           MOVE "CHD010"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD010

           display "path-chd010 = " at 0401
           display  path-chd010     at 0415
           stop " "

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD013"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD013

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD010B"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD010B

           OPEN I-O CHD010 CHD013 CHD010B.

       ajustar-saldo section.

           INITIALIZE REG-CHD010
           START CHD010 KEY IS NOT LESS CHAVE-CH10 INVALID KEY
                MOVE "10" TO ST-CHD010.

           PERFORM UNTIL ST-CHD010 = "10"
                READ CHD010 NEXT AT END
                     MOVE "10" TO ST-CHD010
                NOT AT END
                     IF SITUACAO-CH10 = 0 OR 1 OR 2
                        PERFORM VERIFICAR-CHD010B
                        DISPLAY "REG-CHD010 = " REG-CHD010
                        IF VALOR-SALDO-CH10 > VALOR-CH10
                           MOVE VALOR-CH10 TO VALOR-SALDO-CH10
                           MOVE 0          TO SITUACAO-CH10
                        END-IF

                        COMPUTE VALOR-SALDO-CH10 = VALOR-CH10
                                                 - VLR-BAIXA
                        IF VALOR-SALDO-CH10 = 0
                           MOVE 2 TO SITUACAO-CH10
                           PERFORM ATUALIZAR-CHD013
                        ELSE
                           IF VALOR-SALDO-CH10 = VALOR-CH10
                              MOVE 0 TO SITUACAO-CH10
                              PERFORM APAGAR-CHD013
                           ELSE
                              MOVE 1 TO SITUACAO-CH10
                              PERFORM ATUALIZAR-CHD013
                           END-IF
                        END-IF

                        REWRITE REG-CHD010
                     END-IF
                END-READ
           END-PERFORM.

       ATUALIZAR-CHD013 SECTION.
           INITIALIZE REG-CHD010B
                      VLR-JUROS-CH13
                      VLR-MULTA-CH13
                      VLR-DESCONTO-CH13
                      VLR-LIQUIDO-CH13

           MOVE DATA-MOVTO-CH10  TO DATA-MOVTO-CH13
           MOVE SEQ-CH10         TO SEQ-CH13
           READ CHD013 INVALID KEY
                INITIALIZE ALINEA-CH13
                           DATA-ENTRADA-CH13
                           DATA-COMPRA-CH13
                           DATA-APRES-CH13
                           DATA-REAPRES-CH13
                           DATA-RECTO-CH13
                           VLR-JUROS-CH13
                           VLR-MULTA-CH13
                           VLR-DESCONTO-CH13
                           FORMA-PAGTO-CH13
                           DCR-MEM-CH13
                           DCR-MEM-R-CH13
                           VLR-LIQUIDO-CH13
                           RECEBEDOR-CH13

                WRITE REG-CHD013 INVALID KEY
                    MOVE "Erro de GRAVACAO...CHD013" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-WRITE
           NOT INVALID KEY
                INITIALIZE ALINEA-CH13
                           DATA-ENTRADA-CH13
                           DATA-COMPRA-CH13
                           DATA-APRES-CH13
                           DATA-REAPRES-CH13
                           DATA-RECTO-CH13
                           VLR-JUROS-CH13
                           VLR-MULTA-CH13
                           VLR-DESCONTO-CH13
                           FORMA-PAGTO-CH13
                           DCR-MEM-CH13
                           DCR-MEM-R-CH13
                           VLR-LIQUIDO-CH13
                           RECEBEDOR-CH13

                REWRITE REG-CHD013 INVALID KEY
                    MOVE "Erro de REGRAVACAO...CHD013" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ

           MOVE DATA-MOVTO-CH10    TO DATA-MOVTO-CH10B
           MOVE SEQ-CH10           TO SEQ-CH10B
           START CHD010B KEY IS NOT LESS CHAVE-CH10B INVALID KEY
                 MOVE "10" TO ST-CHD010B.

           PERFORM UNTIL ST-CHD010B = "10"
                 READ CHD010B NEXT AT END
                      MOVE "10" TO ST-CHD010B
                 NOT AT END
                      IF DATA-MOVTO-CH10 <> DATA-MOVTO-CH10B OR
                         SEQ-CH10        <> SEQ-CH10B
                         MOVE "10" TO ST-CHD010B
                      ELSE
                         ADD  JURO-RCTO-CH10B   TO VLR-JUROS-CH13
                         ADD  MULTA-RCTO-CH10B  TO VLR-MULTA-CH13
                         ADD  DESCONTO-CH10B    TO VLR-DESCONTO-CH13
                         ADD  VALOR-LIQ-CH10B   TO VLR-LIQUIDO-CH13
                         MOVE FORMA-PAGTO-CH10B TO FORMA-PAGTO-CH13
                         MOVE RECEBEDOR-CH10B   TO RECEBEDOR-CH13
                         MOVE DCR-MEM-CH10B     TO DCR-MEM-R-CH13
                         MOVE DATA-RCTO-CH10B   TO DATA-RECTO-CH13

                         REWRITE REG-CHD013 INVALID KEY
                             MOVE "Erro de Regravação...CHD013" TO
                             MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                         END-REWRITE
                      END-IF
                 END-READ
           END-PERFORM.
       ATUALIZAR-CHD013-FIM.
           EXIT.


       VERIFICAR-CHD010B SECTION.
           MOVE "S" TO SEM-CHD010B

           INITIALIZE REG-CHD010B
                      JUROS
                      MULTA
                      DESCONTO
                      VLR-BAIXA

           MOVE DATA-MOVTO-CH10        TO DATA-MOVTO-CH10B
           MOVE SEQ-CH10               TO SEQ-CH10B
           START CHD010B KEY IS NOT LESS CHAVE-CH10B INVALID KEY
                MOVE "10" TO ST-CHD010B.

           PERFORM UNTIL ST-CHD010B = "10"
                READ CHD010B NEXT AT END
                     MOVE "10" TO ST-CHD010B
                NOT AT END
                     IF DATA-MOVTO-CH10 <> DATA-MOVTO-CH10B OR
                        SEQ-CH10        <> SEQ-CH10B
                        MOVE "10" TO ST-CHD010B
                     ELSE
                        MOVE "N" TO SEM-CHD010B
                        ADD JURO-RCTO-CH10B   TO JUROS
                        ADD MULTA-RCTO-CH10B  TO MULTA
                        ADD DESCONTO-CH10B    TO DESCONTO
                        ADD VALOR-BAIXA-CH10B TO VLR-BAIXA
                     END-IF
                END-READ
           END-PERFORM.

       APAGAR-CHD013 SECTION.
           MOVE DATA-MOVTO-CH10 TO DATA-MOVTO-CH13
           MOVE SEQ-CH10        TO SEQ-CH13
           READ CHD013 NOT INVALID KEY
                DELETE CHD013
                END-DELETE.

       fechar-arquivos section.
           close chd010 chd013 chd010b.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.

