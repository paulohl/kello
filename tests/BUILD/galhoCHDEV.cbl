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

           SELECT CHD010b2 ASSIGN TO PATH-CHD010b2
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH10b2 = DATA-MOVTO-CH10b2
                                               SEQ-CH10b2
                  ALTERNATE RECORD KEY ALT-CH1b2 =
                     NOME-CH10b2, PORTADOR-CH10b2 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH2b2 = SITUACAO-CH10b2
                     DATA-VENCTO-CH10b2 PORTADOR-CH10b2 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH3b2 = DATA-MOVTO-CH10b2
                     VENDEDOR-CH10b2 SEQ-CH10b2 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH4b2 = COD-COMPL-CH10b2
                     DATA-VENCTO-CH10b2 WITH DUPLICATES
                  STATUS IS ST-CHD010b2.

           SELECT CHD013B ASSIGN TO PATH-CHD013B
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CHD013B
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH13B = DATA-MOVTO-CH13B SEQ-CH13B
                  ALTERNATE RECORD KEY IS DATA-ENTRADA-CH13B
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-RECTO-CH13B
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-APRES-CH13B
                            WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CHPW010.
       COPY CHPW013.
       COPY CHPW010B.

       FD  CHD013B.
       01  REG-CHD013B.
           05  DATA-MOVTO-CH13B            PIC 9(8).
           05  SEQ-CH13B                   PIC 9(4).
           05  ALINEA-CH13B                PIC 99.
           05  DATA-ENTRADA-CH13B          PIC 9(8)      COMP-3.
           05  DATA-COMPRA-CH13B           PIC 9(8)      COMP-3.
           05  DATA-APRES-CH13B            PIC 9(8)      COMP-3.
           05  DATA-REAPRES-CH13B          PIC 9(08)     COMP-3.
           05  DATA-RECTO-CH13B            PIC 9(8)      COMP-3.
           05  VLR-JUROS-CH13B             PIC 9(6)V99   COMP-3.
           05  VLR-MULTA-CH13B             PIC 9(6)V99   COMP-3.
           05  VLR-DESCONTO-CH13B          PIC 9(6)V99   COMP-3.
           05  FORMA-PAGTO-CH13B           PIC X(10).
           05  DCR-MEM-CH13B               PIC X(15).
           05  DCR-MEM-R-CH13B             PIC X(15).
           05  VLR-LIQUIDO-CH13B           PIC 9(6)V99.
           05  RECEBEDOR-CH13B             PIC X(12).

      * ARQUIVO DO MOVIMENTO DE CHEQUES
       FD  CHD010b2.
       01  REG-CHD010b2.
           05  DATA-MOVTO-CH10b2        PIC 9(8).
           05  SEQ-CH10b2               PIC 9(4).
           05  COD-COMPL-CH10b2.
               10  CLASS-CLIENTE-CH10b2 PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH10b2       PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH10b2              PIC X(30).
           05  CIDADE-CH10b2            PIC X(19).
           05  LOTE-CH10b2              PIC X(01).
           05  CONTA-CORR-CH10b2        PIC 9(8)   COMP-3.
           05  DV-CONTA-CH10b2          PIC X.
           05  BANCO-CH10b2             PIC 9(4)   COMP-3.
           05  AGENCIA-CH10b2           PIC 9(5)   COMP-3.
           05  DV-AGENCIA-CH10b2        PIC X.
           05  COMPENSACAO-CH10b2       PIC 9(3).
           05  CPF-CH10b2               PIC 9(11)  COMP-3.
           05  NR-CHEQUE-CH10b2         PIC X(7).
           05  OUTRO-DOCTO-CH10b2       PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-CH10b2       PIC 9(8).
           05  PORTADOR-CH10b2          PIC 9999.
           05  CARTEIRA-CH10b2          PIC 9.
      *    CARTEIRA-CH10  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH10b2      PIC 99.
           05  SITUACAO-CH10b2          PIC 9.
      *    SITUACAO = 0-OK 1-PARCIAL 2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH10b2     PIC 9(5).
           05  VALOR-CH10b2             PIC 9(8)V99.
           05  VENDEDOR-CH10b2          PIC 9(6)   COMP-3.
           05  DIGITADOR-CH10b2         PIC X(5).
           05  SEQ-CAIXA-CH10b2         PIC 9(3).
           05  NR-NOTA-FISCAL-CH10b2    PIC X(10).
           05  DATA-NTA-FISCAL-CH10b2   PIC 9(8)   COMP-3.
           05  ORIGEM-CH10b2            PIC 99.
           05  VALOR-SALDO-CH10b2       PIC 9(08)V99.
           05  FILLER                   PIC X(100).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD010b2           PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CHD013B            PIC XX       VALUE SPACES.
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
           05  QTDE                  PIC 9(09) VALUE ZEROS.
           05  QTDE2                 PIC 9(09) VALUE ZEROS.
           05  MASC-QTDE             PIC ZZZ.ZZZ.ZZ9.

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

       informar-empresa section.
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
       mais-empresas-fim.
           exit.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD010"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD010

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD010B2"             TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD010B2


           display "path-chd010 = " at 0401
           display  path-chd010     at 0415
           stop " "

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD013"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD013

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD013B"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD013B


           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD010B"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD010B

           OPEN I-O CHD010 CHD010B2 CHD013 CHD013B CHD010B.
       abrir-arquivos-fim.
           exit.

       ajustar-saldo section.
           INITIALIZE REG-CHD010B2
                      QTDE
                      QTDE2
           MOVE 3                      TO SITUACAO-CH10B2
           START CHD010B2 KEY IS NOT LESS ALT-CH2B2 INVALID KEY
                MOVE "10" TO ST-CHD010B2.

           PERFORM UNTIL ST-CHD010B2 = "10"
                READ CHD010B2 NEXT AT END
                     MOVE "10" TO ST-CHD010B2
                NOT AT END
                     IF SITUACAO-CH10B2 = 0 OR 1 OR 2
                        MOVE "10" TO SITUACAO-CH10B2
                     ELSE
                        MOVE DATA-MOVTO-CH10B2 TO DATA-MOVTO-CH10
                        MOVE SEQ-CH10B2        TO SEQ-CH10
                        READ CHD010 INVALID KEY
                             MOVE "Nao Achei" TO MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                        NOT INVALID KEY
                             IF SITUACAO-CH10 <> SITUACAO-CH10B2
                                PERFORM VERIFICAR-CHD010B
                                IF SEM-CHD010B = "S"
                                   DISPLAY "REG-CHD010 = " REG-CHD010
                                   MOVE SITUACAO-CH10B2 TO SITUACAO-CH10
                                   REWRITE REG-CHD010 INVALID KEY
                                       MOVE "Erro de Regravação...CHD010
      -                                     ""  TO MENSAGEM
                                       MOVE "C" TO TIPO-MSG
                                       PERFORM EXIBIR-MENSAGEM
                                   NOT INVALID KEY
                                       ADD 1 TO QTDE
                                       PERFORM ATUALIZAR-CHD013
                                   END-REWRITE
                                ELSE
                                   IF VALOR-CH10 <> VLR-BAIXA
                                      MOVE 1 TO SITUACAO-CH10
                                      REWRITE REG-CHD010 INVALID KEY
                                       MOVE "Erro de Regravação...CHD010
      -                                     ""  TO MENSAGEM
                                       MOVE "C" TO TIPO-MSG
                                       PERFORM EXIBIR-MENSAGEM
                                      NOT INVALID KEY
                                       ADD 1 TO QTDE2
                                      END-REWRITE
                                   END-IF
                                END-IF
                             END-IF
                        END-READ
                     END-IF
                END-READ
           END-PERFORM

           MOVE QTDE   TO MASC-QTDE

           MOVE SPACES TO MENSAGEM
           STRING "Atualizado " MASC-QTDE " Registros DEVOLVIDOS"
            INTO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM

           MOVE QTDE2  TO MASC-QTDE
           MOVE SPACES TO MENSAGEM
           STRING "Atualizado " MASC-QTDE " Registros PARCIAIS"
            INTO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM.
       AJUSTAR-SALDO-FIM.
           EXIT.

       ATUALIZAR-CHD013 SECTION.
           MOVE DATA-MOVTO-CH10        TO DATA-MOVTO-CH13B
           MOVE SEQ-CH10               TO SEQ-CH13B
           READ CHD013B NOT INVALID KEY
                MOVE REG-CHD013B TO REG-CHD013
                WRITE REG-CHD013 INVALID KEY
                      REWRITE REG-CHD013 INVALID KEY
                          MOVE "Erro de Regravação...CHD013" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.
       ATUALIZAR-CHD013-FIM.
           EXIT.

       VERIFICAR-CHD010B SECTION.
           MOVE "S" TO SEM-CHD010B

           INITIALIZE REG-CHD010B
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
                        MOVE "N"  TO SEM-CHD010B
                        ADD VALOR-BAIXA-CH10B TO VLR-BAIXA
                     END-IF
                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close chd010 chd010b2 chd013 chd013b chd010b.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.

