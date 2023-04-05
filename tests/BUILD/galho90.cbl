       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO90.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 07/12/2004.
      *FUNÇÃO: LOCALIZA UMA PROGRAMAÇÃO FINANCEIRA

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CPPX020.
           COPY CPPX024.
           COPY CPPX099.
           COPY LOGX003.

       DATA DIVISION.
       FILE SECTION.
           COPY CPPW020.
           COPY CPPW024.
           COPY CPPW099.
           COPY LOGW003.


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD024             PIC XX       VALUE SPACES.
           05  ST-CPD099             PIC XX       VALUE SPACES.
           05  ST-LOG003             PIC XX       VALUE SPACES.
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
           05  CONTADOR              PIC 9(09)    VALUE ZEROS.
           05  LISTEI                PIC X(01)    VALUE SPACES.
           05  ACP-PROGRAMACAO       PIC 9(09)    VALUE ZEROS.
           05  MASC-PROGRAMACAO      PIC ZZZ.ZZZ.ZZ9.
           05  MASC-VALOR            PIC ZZZ.ZZ9,99 VALUE ZEROS.
           05  ACP-DATA              PIC 9(08).
           05  AUX-DATA              PIC 9(08).
           05  MASC-DT               PIC 99/99/9999 BLANK WHEN ZEROS.
           05  ACP-VALOR             PIC 9(06)V99 VALUE ZEROS.
           05  RESPOSTA              PIC X(01)    VALUE SPACES.
           05  ACP-FORNECEDOR        PIC 9(06)    VALUE ZEROS.
           05  ACHEI                 PIC X(01)    VALUE SPACES.
           05  ACP-DATA-EMISSAO      PIC 9(08)    VALUE ZEROS.
           05  WS-DATA               PIC 9(08)    VALUE ZEROS.
           05  WS-EMISSAO            PIC 9(08)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

           COPY "PARAMETR".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".

       01 MASC-DATA                PIC X(10)    VALUE SPACES.

       01 AUX-PROGRAMA             PIC X(20)    VALUE SPACES.
       01 AUX-ARQUIVO              PIC X(20)    VALUE SPACES.
       01 AUX-FORNECEDOR           PIC 9(06)    VALUE ZEROS.
       01 AUX-VALOR                PIC 9(09)V99 VALUE ZEROS.



       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.

           DISPLAY "Informar a Empresa Desejada: " AT 0101
           ACCEPT EMP-REC                          AT 0129

           MOVE "CPD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD020
           MOVE "CPD024"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD024
           MOVE "CPD099"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD099
           MOVE "LOG003"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG003

           OPEN INPUT CPD020
           IF ST-CPD020 = "35"
              CLOSE CPD020      OPEN OUTPUT CPD020
              CLOSE CPD020      OPEN I-O    CPD020
           END-IF.
           OPEN INPUT CPD024
           IF ST-CPD024 = "35"
              CLOSE CPD024      OPEN OUTPUT CPD024
              CLOSE CPD024      OPEN I-O    CPD024
           END-IF.
           OPEN INPUT CPD099
           IF ST-CPD099 = "35"
              CLOSE CPD099      OPEN OUTPUT CPD099
              CLOSE CPD099      OPEN I-O    CPD099
           END-IF.
           OPEN INPUT LOG003
           IF ST-LOG003 = "35"
              CLOSE LOG003       OPEN OUTPUT LOG003
              CLOSE LOG003       OPEN I-O    LOG003
           END-IF.

       0010-PROCURAR-PROGRAMACAO.
           DISPLAY ERASE AT 0101

           DISPLAY "Achar Programacao Excluida (S/N/T/2)? " AT 0101
           ACCEPT RESPOSTA                        AT 0140

           MOVE FUNCTION UPPER-CASE(RESPOSTA) TO RESPOSTA

           EVALUATE RESPOSTA
               WHEN "N" PERFORM CONTINUAR
               WHEN "S" PERFORM ACHAR-EXCLUSAO
               WHEN "T" PERFORM ACHAR-TODAS
               WHEN "2" PERFORM PROCURA
               WHEN OTHER GO TO 0010-PROCURAR-PROGRAMACAO
           END-EVALUATE.

       0015-OUTRA.

           DISPLAY "Deseja procurar outra programacao: " AT 1001
           MOVE ZEROS                      TO LISTEI
           ACCEPT LISTEI                   AT 1036

           MOVE FUNCTION UPPER-CASE(LISTEI) TO LISTEI

           IF LISTEI = "S"
              GO TO 0010-PROCURAR-PROGRAMACAO.

           IF LISTEI <> "S" AND "N"
              GO TO 0015-OUTRA.


           CLOSE CPD020 CPD024 CPD099 LOG003
           EXIT PROGRAM
           STOP RUN.


       ACHAR-EXCLUSAO SECTION.
           DISPLAY ERASE AT 0101

           DISPLAY "Informar o Código do Fornecedor: " AT 0101
           MOVE 0                TO ACP-FORNECEDOR
           ACCEPT ACP-FORNECEDOR AT 0133

           IF ACP-FORNECEDOR = 0
              GO TO ACHAR-EXCLUSAO.

       VENCIMENTO.
           DISPLAY "Informar o Vencimento: " AT 0201
           MOVE ZEROS TO ACP-DATA
           MOVE ACP-DATA TO MASC-DT
           DISPLAY MASC-DT AT 0223
           ACCEPT MASC-DT  AT 0223
           MOVE MASC-DT TO ACP-DATA

           IF ACP-DATA = 0
              GO TO VENCIMENTO.

           INITIALIZE REG-CPD099
           MOVE ACP-FORNECEDOR   TO FORNEC-CP99
           STRING ACP-DATA(5:4) ACP-DATA(3:2) ACP-DATA(1:2) INTO
                  DATA-VENCTO-CP99
           MOVE DATA-VENCTO-CP99 TO AUX-DATA
           START CPD099 KEY IS NOT LESS ALT1-CP99 INVALID KEY
               MOVE "10" TO ST-CPD099.

           PERFORM UNTIL ST-CPD099 = "10"
               READ CPD099 NEXT AT END
                   MOVE "10" TO ST-CPD099
               NOT AT END
                   IF ACP-FORNECEDOR   <> FORNEC-CP99 OR
                      DATA-VENCTO-CP99 <> AUX-DATA
                      MOVE "10" TO ST-CPD099
                   ELSE
                      DISPLAY ERASE AT 0101
                      PERFORM LISTAR-PROGRAMACAO2
                   END-IF
               END-READ
           END-PERFORM.
       ACHAR-EXCLUSAO-FIM.
           EXIT.

       CONTINUAR SECTION.
           DISPLAY "Informar a Programcao Financeira: " AT 0101
           MOVE ZEROS           TO ACP-PROGRAMACAO
           MOVE ACP-PROGRAMACAO TO MASC-PROGRAMACAO
           DISPLAY MASC-PROGRAMACAO                     AT 0135
           ACCEPT MASC-PROGRAMACAO                      AT 0135

           MOVE MASC-PROGRAMACAO       TO ACP-PROGRAMACAO

           IF ACP-PROGRAMACAO = 0
              GO TO 0010-PROCURAR-PROGRAMACAO.

           INITIALIZE REG-CPD024
           MOVE "N" TO LISTEI
           START CPD024 KEY IS NOT LESS CHAVE-CP24 INVALID KEY
                 MOVE "10" TO ST-CPD024
                 DISPLAY ST-CPD024 STOP "ST-CPD24 1".

           PERFORM UNTIL ST-CPD024 = "10"
                 READ CPD024 NEXT RECORD AT END
                      MOVE "10" TO ST-CPD024
                 NOT AT END
                      IF NUMERO-PROGRAMACAO-CP24 = ACP-PROGRAMACAO
                         DISPLAY ERASE AT 0101
                         PERFORM LISTAR-PROGRAMACAO
                      END-IF
                 END-READ
           END-PERFORM

           DISPLAY ERASE AT 0101

           DISPLAY "VOU LISTAR OS EXCLUIDOS NA DATA EXPECIFICADA" STOP
           "  ".

       0018-DATA.

           DISPLAY "Informar Data: " AT 0301
           MOVE ZEROS TO ACP-DATA
           MOVE ACP-DATA TO MASC-DT
           DISPLAY MASC-DT AT 0316
           ACCEPT MASC-DT  AT 0316
           MOVE MASC-DT TO ACP-DATA

           IF ACP-DATA = 0
              GO TO 0018-DATA.


           INITIALIZE REG-CPD099
           STRING ACP-DATA(5:4) ACP-DATA(3:2) ACP-DATA(1:2) INTO
                  DATA-MOVTO-CP99
           MOVE DATA-MOVTO-CP99 TO AUX-DATA
           START CPD099 KEY IS NOT LESS DATA-MOVTO-CP99 INVALID KEY
               MOVE "10" TO ST-CPD099.

           PERFORM UNTIL ST-CPD099 = "10"
               READ CPD099 NEXT AT END
                   MOVE "10" TO ST-CPD099
               NOT AT END
                   IF DATA-MOVTO-CP99 <> AUX-DATA
                      MOVE "10" TO ST-CPD099
                   ELSE
                      DISPLAY ERASE AT 0101
                      PERFORM LISTAR-PROGRAMACAO2
                   END-IF
               END-READ
           END-PERFORM


           DISPLAY ERASE AT 0101

           DISPLAY "VOU LISTAR POR VALOR ESPECIFICADO" STOP
           "  ".

       0018-VALOR.

           DISPLAY "Informar Valor: " AT 0301
           MOVE ZEROS         TO ACP-VALOR
           MOVE ACP-VALOR     TO MASC-VALOR
           DISPLAY MASC-VALOR AT 0316
           ACCEPT MASC-VALOR  AT 0316
           MOVE MASC-VALOR    TO ACP-VALOR

           IF ACP-VALOR = 0
              GO TO 0018-VALOR.


           INITIALIZE REG-CPD020
           START CPD020 KEY IS NOT LESS CHAVE-CP20 INVALID KEY
               MOVE "10" TO ST-CPD020.

           PERFORM UNTIL ST-CPD020 = "10"
               READ CPD020 NEXT AT END
                   MOVE "10" TO ST-CPD020
               NOT AT END
                   IF ACP-VALOR = VALOR-TOT-CP20
                      DISPLAY ERASE AT 0101
                      PERFORM LISTAR-PROGRAMACAO3
                   END-IF
               END-READ
           END-PERFORM


           IF LISTEI = "N"
              DISPLAY "NAO ENCONTREI A PROGRAMACAO FINANCEIRA" AT 0501.
       CONTINUAR-FIM.
           EXIT.

       ACHAR-TODAS SECTION.
           DISPLAY ERASE AT 0101

           DISPLAY "Informar o Fornecedor: " at 0101
           ACCEPT acp-fornecedor             at 0124

           DISPLAY "Informar o Vencimento: " at 0201
           ACCEPT ACP-DATA                   at 0224

           STRING ACP-DATA(5:4) ACP-DATA(3:2) ACP-DATA(1:2)
             INTO WS-DATA

           DISPLAY "Informar a Emissão...: " at 0301
           ACCEPT ACP-DATA-EMISSAO           at 0324
           STRING ACP-DATA-EMISSAO(5:4)
                  ACP-DATA-EMISSAO(3:2)
                  ACP-DATA-EMISSAO(1:2)
             INTO WS-EMISSAO

           DISPLAY "Informar o Valor.....: " at 0401
           ACCEPT ACP-VALOR                  at 0424

           INITIALIZE REG-CPD099

           START CPD099 KEY IS NOT LESS CHAVE-CP99 INVALID KEY
                 MOVE "10" TO ST-CPD099.

           PERFORM UNTIL ST-CPD099 = "10"
                 READ CPD099 NEXT AT END
                      MOVE "10" TO ST-CPD099
                 NOT AT END
                      IF ACP-FORNECEDOR = 0 OR FORNEC-CP99
                         IF ACP-DATA = 0 OR
                            (DATA-VENCTO-CP99 = WS-DATA)
                            IF ACP-DATA-EMISSAO = 0 OR
                               (DATA-MOVTO-CP99 = WS-EMISSAO)
                               IF ACP-VALOR = 0 OR VALOR-TOT-CP99
                                  DISPLAY ERASE AT 0101
                                  DISPLAY "CPD099" AT 0101
                                  PERFORM LISTAR-PROGRAMACAO2
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           INITIALIZE REG-CPD020
           START CPD020 KEY IS NOT LESS CHAVE-CP20 INVALID KEY
               MOVE "10" TO ST-CPD020.

           PERFORM UNTIL ST-CPD020 = "10"
               READ CPD020 NEXT AT END
                    MOVE "10" TO ST-CPD020
               NOT AT END
                    IF ACP-FORNECEDOR = 0 OR FORNEC-CP20
                       IF ACP-DATA = 0 OR
                          (DATA-VENCTO-CP20 = WS-DATA)
                          IF ACP-DATA-EMISSAO = 0 OR
                             (DATA-MOVTO-CP20 = WS-EMISSAO)
                             IF ACP-VALOR = 0 OR VALOR-TOT-CP20
                                DISPLAY ERASE AT 0101
                                DISPLAY "CPD020" AT 0101
                                PERFORM LISTAR-PROGRAMACAO3
                             END-IF
                          END-IF
                       END-IF
                    END-IF
               END-READ
           END-PERFORM.



       LISTAR-PROGRAMACAO SECTION.
           MOVE "S" TO LISTEI

           MOVE FORNEC-CP24            TO FORNEC-CP20
           MOVE SEQ-CP24               TO SEQ-CP20
           READ CPD020 INVALID KEY
                DISPLAY "NAO ENCONTRADO CPD020" AT 2501
                MOVE FORNEC-CP20       TO FORNEC-CP99
                MOVE SEQ-CP20          TO SEQ-CP99
                READ CPD099 INVALID KEY
                     DISPLAY "NAO ENCONTRADO CPD099" AT 2501
                NOT INVALID KEY
                     MOVE REG-CPD099   TO REG-CPD020
                     DISPLAY "DATA MOVIMENTO: "   AT 0201
                     MOVE SPACES TO MASC-DATA
                     STRING DATA-MOVTO-CP20(7:2) "/"
                            DATA-MOVTO-CP20(5:2) "/"
                            DATA-MOVTO-CP20(1:4) INTO MASC-DATA
                     DISPLAY MASC-DATA            AT 0217

                     DISPLAY "FORNECEDOR: "       AT 0230
                     DISPLAY FORNEC-CP20          AT 0243

                     DISPLAY "SEQUEN: "           AT 0252
                     DISPLAY SEQ-CP20             AT 0260

                     DISPLAY "NR-DOCTO: "         AT 0301
                     DISPLAY NR-DOCTO-CP20        AT 0311

                     DISPLAY "DATA EMISSAO: "     AT 0330
                     MOVE SPACES TO MASC-DATA
                     STRING DATA-EMISSAO-CP20(7:2) "/"
                            DATA-EMISSAO-CP20(5:2) "/"
                            DATA-EMISSAO-CP20(1:4) INTO MASC-DATA
                     DISPLAY MASC-DATA            AT 0344

                     DISPLAY "DATA VENCTO: "      AT 0401
                     MOVE SPACES TO MASC-DATA
                     STRING DATA-VENCTO-CP20(7:2) "/"
                            DATA-VENCTO-CP20(5:2) "/"
                            DATA-VENCTO-CP20(1:4) INTO MASC-DATA
                     DISPLAY MASC-DATA              AT 0417

                     DISPLAY "DESCRICAO: "          AT 0430
                     DISPLAY DESCRICAO-CP20         AT 0441

                     DISPLAY "TIPO LCT: "           AT 0501
                     IF PREV-DEF-CP20 = 0
                        DISPLAY "DEFINITIVO"        AT 0511
                     ELSE
                        DISPLAY "PREVISTO"          AT 0511
                     END-IF

                     DISPLAY "SITUACAO: "           AT 0525
                     EVALUATE SITUACAO-CP20
                         WHEN 0 DISPLAY "OK"        AT 0535
                         WHEN 1 DISPLAY "SUSPENSA"  AT 0535
                         WHEN 2 DISPLAY "PAGA"      AT 0535
                         WHEN 3 DISPLAY "ESTONADA"  AT 0535
                         WHEN 4 DISPLAY "CANCELADA" AT 0535
                     END-EVALUATE

                     DISPLAY "LIBERADO: "           AT 0548
                     EVALUATE LIBERADO-CP20
                         WHEN 0 DISPLAY "NAO"       AT 0558
                         WHEN 1 DISPLAY "SIM"       AT 0558
                     END-EVALUATE


                     DISPLAY "RESPONSAVEL: "        AT 0601
                     DISPLAY RESPONSAVEL-CP20       AT 0613

                     DISPLAY "DIGITADOR: "          AT 0630
                     DISPLAY DIGITADOR-CP20         AT 0640

                     DISPLAY "VALOR: "              AT 0701
                     MOVE VALOR-TOT-CP20            TO MASC-VALOR
                     DISPLAY MASC-VALOR             AT 0708

                     DISPLAY "LIDO NO CPD099 ==== REGISTRO EXCLUIDO"
                                                    AT 1510

                     DISPLAY "DADOS LOG003"         AT 1601

                     DISPLAY "NAO ACHEI O LOG"      AT 1620

                     INITIALIZE REG-LOG003
                     MOVE "CPD020"                  TO LOG3-ARQUIVO
                     START LOG003 KEY IS NOT LESS LOG3-CH-ARQUIVO
                                                             INVALID KEY
                           MOVE "10" TO ST-LOG003
                     END-START

                     PERFORM UNTIL ST-LOG003 = "10"
                           READ LOG003 NEXT AT END
                                MOVE "10" TO ST-LOG003
                           NOT AT END
                                IF LOG3-ARQUIVO  <> "CPD020"
                                   MOVE "10" TO ST-LOG003
                                ELSE
                                   IF LOG3-REGISTRO(1:181) =
                                      REG-CPD020
                                      DISPLAY "ACHEI O LOG     " AT 1620
                                      MOVE "10" TO ST-LOG003
                                   END-IF
                                END-IF
                           END-READ
                     END-PERFORM
                END-READ
           NOT INVALID KEY
                DISPLAY "DATA MOVIMENTO: "   AT 0201
                MOVE SPACES TO MASC-DATA
                STRING DATA-MOVTO-CP20(7:2) "/" DATA-MOVTO-CP20(5:2)
                       "/" DATA-MOVTO-CP20(1:4) INTO MASC-DATA
                DISPLAY MASC-DATA            AT 0217

                DISPLAY "FORNECEDOR: "       AT 0230
                DISPLAY FORNEC-CP20          AT 0243

                DISPLAY "SEQUEN: "           AT 0252
                DISPLAY SEQ-CP20             AT 0260

                DISPLAY "NR-DOCTO: "         AT 0301
                DISPLAY NR-DOCTO-CP20        AT 0311

                DISPLAY "DATA EMISSAO: "     AT 0330
                MOVE SPACES TO MASC-DATA
                STRING DATA-EMISSAO-CP20(7:2) "/" DATA-EMISSAO-CP20(5:2)
                       "/" DATA-EMISSAO-CP20(1:4) INTO MASC-DATA
                DISPLAY MASC-DATA            AT 0344

                DISPLAY "DATA VENCTO: "      AT 0401
                MOVE SPACES TO MASC-DATA
                STRING DATA-VENCTO-CP20(7:2) "/" DATA-VENCTO-CP20(5:2)
                       "/" DATA-VENCTO-CP20(1:4) INTO MASC-DATA
                DISPLAY MASC-DATA              AT 0417

                DISPLAY "DESCRICAO: "          AT 0430
                DISPLAY DESCRICAO-CP20         AT 0441

                DISPLAY "TIPO LCT: "           AT 0501
                IF PREV-DEF-CP20 = 0
                   DISPLAY "DEFINITIVO"        AT 0511
                ELSE
                   DISPLAY "PREVISTO"          AT 0511
                END-IF

                DISPLAY "SITUACAO: "           AT 0525
                EVALUATE SITUACAO-CP20
                    WHEN 0 DISPLAY "OK"        AT 0535
                    WHEN 1 DISPLAY "SUSPENSA"  AT 0535
                    WHEN 2 DISPLAY "PAGA"      AT 0535
                    WHEN 3 DISPLAY "ESTONADA"  AT 0535
                    WHEN 4 DISPLAY "CANCELADA" AT 0535
                END-EVALUATE

                DISPLAY "LIBERADO: "           AT 0548
                EVALUATE LIBERADO-CP20
                    WHEN 0 DISPLAY "NAO"       AT 0558
                    WHEN 1 DISPLAY "SIM"       AT 0558
                END-EVALUATE


                DISPLAY "RESPONSAVEL: "        AT 0601
                DISPLAY RESPONSAVEL-CP20       AT 0613

                DISPLAY "DIGITADOR: "          AT 0630
                DISPLAY DIGITADOR-CP20         AT 0640

                DISPLAY "VALOR: "              AT 0701
                MOVE VALOR-TOT-CP20            TO MASC-VALOR
                DISPLAY MASC-VALOR             AT 0708

                DISPLAY "LIDO NO CPD020 ==== REGISTRO EXISTENTE"
                                               AT 1510

           END-READ

           DISPLAY "DEPARTAMENTO: "   AT 0801
           DISPLAY DEPARTAMENTO-CP24  AT 0815
           DISPLAY "BANCO: "          AT 0840
           DISPLAY BANCO-CP24         AT 0847
           DISPLAY "AGENCIA: "        AT 0901
           DISPLAY AGENCIA-CP24       AT 0910
           DISPLAY "CONTA: "          AT 0930
           DISPLAY CONTA-CP24         AT 0937.


           STOP " ".

       LISTAR-PROGRAMACAO-FIM.
           EXIT.

       LISTAR-PROGRAMACAO2 SECTION.
           MOVE REG-CPD099 TO REG-CPD020

           DISPLAY "DATA MOVIMENTO: "   AT 0201
           MOVE SPACES TO MASC-DATA
           STRING DATA-MOVTO-CP20(7:2) "/" DATA-MOVTO-CP20(5:2)
                  "/" DATA-MOVTO-CP20(1:4) INTO MASC-DATA
           DISPLAY MASC-DATA            AT 0217

           DISPLAY "FORNECEDOR: "       AT 0230
           DISPLAY FORNEC-CP20          AT 0243

           DISPLAY "SEQUEN: "           AT 0252
           DISPLAY SEQ-CP20             AT 0260

           DISPLAY "NR-DOCTO: "         AT 0301
           DISPLAY NR-DOCTO-CP20        AT 0311

           DISPLAY "DATA EMISSAO: "     AT 0330
           MOVE SPACES TO MASC-DATA
           STRING DATA-EMISSAO-CP20(7:2) "/" DATA-EMISSAO-CP20(5:2)
                  "/" DATA-EMISSAO-CP20(1:4) INTO MASC-DATA
           DISPLAY MASC-DATA            AT 0344

           DISPLAY "DATA VENCTO: "      AT 0401
           MOVE SPACES TO MASC-DATA
           STRING DATA-VENCTO-CP20(7:2) "/" DATA-VENCTO-CP20(5:2)
                  "/" DATA-VENCTO-CP20(1:4) INTO MASC-DATA
           DISPLAY MASC-DATA              AT 0417

           DISPLAY "DESCRICAO: "          AT 0430
           DISPLAY DESCRICAO-CP20         AT 0441

           DISPLAY "TIPO LCT: "           AT 0501
           IF PREV-DEF-CP20 = 0
              DISPLAY "DEFINITIVO"        AT 0511
           ELSE
              DISPLAY "PREVISTO"          AT 0511
           END-IF

           DISPLAY "SITUACAO: "           AT 0525
           EVALUATE SITUACAO-CP20
               WHEN 0 DISPLAY "OK"        AT 0535
               WHEN 1 DISPLAY "SUSPENSA"  AT 0535
               WHEN 2 DISPLAY "PAGA"      AT 0535
               WHEN 3 DISPLAY "ESTONADA"  AT 0535
               WHEN 4 DISPLAY "CANCELADA" AT 0535
           END-EVALUATE

           DISPLAY "LIBERADO: "           AT 0548
           EVALUATE LIBERADO-CP20
               WHEN 0 DISPLAY "NAO"       AT 0558
               WHEN 1 DISPLAY "SIM"       AT 0558
           END-EVALUATE


           DISPLAY "RESPONSAVEL: "        AT 0601
           DISPLAY RESPONSAVEL-CP20       AT 0613

           DISPLAY "DIGITADOR: "          AT 0630
           DISPLAY DIGITADOR-CP20         AT 0640

           DISPLAY "VALOR: "              AT 0701
           MOVE VALOR-TOT-CP20            TO MASC-VALOR
           DISPLAY MASC-VALOR             AT 0708

           DISPLAY "LIDO NO CPD099 ==== REGISTRO EXCLUIDO"
                                          AT 1010

           DISPLAY "DADOS LOG003"         AT 1201

           DISPLAY "NAO ACHEI O LOG"      AT 1220

           INITIALIZE REG-LOG003
           MOVE "N"                       TO ACHEI
           MOVE "CPD020"                  TO LOG3-ARQUIVO
           START LOG003 KEY IS NOT LESS LOG3-CH-ARQUIVO
                                                   INVALID KEY
                 MOVE "10" TO ST-LOG003
           END-START

           PERFORM UNTIL ST-LOG003 = "10"
                 READ LOG003 NEXT AT END
                      MOVE "10" TO ST-LOG003
                 NOT AT END
                      IF LOG3-ARQUIVO  <> "CPD020"
                         MOVE "10" TO ST-LOG003
                      ELSE
                         IF ACHEI = "S"
                            IF LOG3-REGISTRO(1:181) <> REG-CPD020
                               MOVE "10" TO ST-LOG003
                            ELSE
                               PERFORM LISTAR-LOG
                            END-IF
                         ELSE
                            IF LOG3-REGISTRO(1:181) =
                               REG-CPD020
                               MOVE "S" TO ACHEI
                               PERFORM LISTAR-LOG
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           STOP " ".

       LISTAR-PROGRAMACAO2-FIM.
           EXIT.

       LISTAR-LOG SECTION.
           DISPLAY "ACHEI O LOG     " AT 1220

           DISPLAY "OPERACAO = "      AT 1401
           DISPLAY LOG3-OPERACAO      AT 1412

           STRING LOG3-DIA "/" LOG3-MES "/" LOG3-ANO INTO MASC-DATA

           DISPLAY "DATA.... = "      AT 1430
           DISPLAY MASC-DATA          AT 1441

           MOVE SPACES                TO MASC-DATA
           STRING LOG3-HORA ":" LOG3-MINU INTO MASC-DATA

           DISPLAY "HORAS... = "      AT 1501
           DISPLAY MASC-DATA          AT 1512

           DISPLAY "USUARIO. = "      AT 1530
           DISPLAY LOG3-USUARIO       AT 1541

           DISPLAY "PROGRAMA = "      AT 1601
           DISPLAY LOG3-PROGRAMA      AT 1612

           STOP "  ".



       LISTAR-PROGRAMACAO3 SECTION.

           DISPLAY "DATA MOVIMENTO: "   AT 0201
           MOVE SPACES TO MASC-DATA
           STRING DATA-MOVTO-CP20(7:2) "/" DATA-MOVTO-CP20(5:2)
                  "/" DATA-MOVTO-CP20(1:4) INTO MASC-DATA
           DISPLAY MASC-DATA            AT 0217

           DISPLAY "FORNECEDOR: "       AT 0230
           DISPLAY FORNEC-CP20          AT 0243

           DISPLAY "SEQUEN: "           AT 0252
           DISPLAY SEQ-CP20             AT 0260

           DISPLAY "NR-DOCTO: "         AT 0301
           DISPLAY NR-DOCTO-CP20        AT 0311

           DISPLAY "DATA EMISSAO: "     AT 0330
           MOVE SPACES TO MASC-DATA
           STRING DATA-EMISSAO-CP20(7:2) "/" DATA-EMISSAO-CP20(5:2)
                  "/" DATA-EMISSAO-CP20(1:4) INTO MASC-DATA
           DISPLAY MASC-DATA            AT 0344

           DISPLAY "DATA VENCTO: "      AT 0401
           MOVE SPACES TO MASC-DATA
           STRING DATA-VENCTO-CP20(7:2) "/" DATA-VENCTO-CP20(5:2)
                  "/" DATA-VENCTO-CP20(1:4) INTO MASC-DATA
           DISPLAY MASC-DATA              AT 0417

           DISPLAY "DESCRICAO: "          AT 0430
           DISPLAY DESCRICAO-CP20         AT 0441

           DISPLAY "TIPO LCT: "           AT 0501
           IF PREV-DEF-CP20 = 0
              DISPLAY "DEFINITIVO"        AT 0511
           ELSE
              DISPLAY "PREVISTO"          AT 0511
           END-IF

           DISPLAY "SITUACAO: "           AT 0525
           EVALUATE SITUACAO-CP20
               WHEN 0 DISPLAY "OK"        AT 0535
               WHEN 1 DISPLAY "SUSPENSA"  AT 0535
               WHEN 2 DISPLAY "PAGA"      AT 0535
               WHEN 3 DISPLAY "ESTONADA"  AT 0535
               WHEN 4 DISPLAY "CANCELADA" AT 0535
           END-EVALUATE

           DISPLAY "LIBERADO: "           AT 0548
           EVALUATE LIBERADO-CP20
               WHEN 0 DISPLAY "NAO"       AT 0558
               WHEN 1 DISPLAY "SIM"       AT 0558
           END-EVALUATE


           DISPLAY "RESPONSAVEL: "        AT 0601
           DISPLAY RESPONSAVEL-CP20       AT 0613

           DISPLAY "DIGITADOR: "          AT 0630
           DISPLAY DIGITADOR-CP20         AT 0640

           DISPLAY "VALOR: "              AT 0701
           MOVE VALOR-TOT-CP20            TO MASC-VALOR
           DISPLAY MASC-VALOR             AT 0708

           MOVE FORNEC-CP20           TO FORNEC-CP24
           MOVE SEQ-CP20              TO SEQ-CP24
           READ CPD024 INVALID KEY
                INITIALIZE REG-CPD024.

           DISPLAY "DEPARTAMENTO: "   AT 0801
           DISPLAY DEPARTAMENTO-CP24  AT 0815
           DISPLAY "BANCO: "          AT 0840
           DISPLAY BANCO-CP24         AT 0847
           DISPLAY "AGENCIA: "        AT 0901
           DISPLAY AGENCIA-CP24       AT 0910
           DISPLAY "CONTA: "          AT 0930
           DISPLAY CONTA-CP24         AT 0937.


           DISPLAY "DADOS LOG003"         AT 1201

           DISPLAY "NAO ACHEI O LOG"      AT 1220

           INITIALIZE REG-LOG003
           MOVE "N"                       TO ACHEI
           MOVE "CPD020"                  TO LOG3-ARQUIVO
           START LOG003 KEY IS NOT LESS LOG3-CH-ARQUIVO
                                                   INVALID KEY
                 MOVE "10" TO ST-LOG003
           END-START

           PERFORM UNTIL ST-LOG003 = "10"
                 READ LOG003 NEXT AT END
                      MOVE "10" TO ST-LOG003
                 NOT AT END
                      IF LOG3-ARQUIVO  <> "CPD020"
                         MOVE "10" TO ST-LOG003
                      ELSE
                         IF ACHEI = "S"
                            IF LOG3-REGISTRO(1:181) <> REG-CPD020
                               MOVE "10" TO ST-LOG003
                            ELSE
                               PERFORM LISTAR-LOG
                            END-IF
                         ELSE
      *                     IF LOG3-REGISTRO(1:181) =
      *                        REG-CPD020
                            IF LOG3-REGISTRO(1:14) =
                               REG-CPD020(1:14)
                               MOVE "S" TO ACHEI
                               PERFORM LISTAR-LOG
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM


           IF SITUACAO-CP20 = 3 OR 4
              DISPLAY "Situacao do Título " AT 2001
              IF SITUACAO-CP20 = 3
                 DISPLAY "ESTORNADA"        AT 2020
              ELSE
                 DISPLAY "CANCELADA"        AT 2020
              END-IF

              DISPLAY " , Voce deseja Reverter a Situacao ? " AT 2029
              MOVE SPACES TO RESPOSTA
              ACCEPT RESPOSTA AT 2066

              MOVE FUNCTION UPPER-CASE(RESPOSTA) TO RESPOSTA

              IF RESPOSTA = "S"
                 CLOSE    CPD020
                 OPEN I-O CPD020

                 MOVE 0 TO SITUACAO-CP20
                 REWRITE REG-CPD020

                 CLOSE      CPD020
                 OPEN INPUT CPD020
              END-IF
           END-IF




           STOP " ".

       LISTAR-PROGRAMACAO3-FIM.
           EXIT.

       LISTAR-PROGRAMACAO4 SECTION.

           DISPLAY ERASE AT 0101

           DISPLAY "DATA MOVIMENTO: "   AT 0201
           MOVE SPACES TO MASC-DATA
           STRING DATA-MOVTO-CP20(7:2) "/" DATA-MOVTO-CP20(5:2)
                  "/" DATA-MOVTO-CP20(1:4) INTO MASC-DATA
           DISPLAY MASC-DATA            AT 0217

           DISPLAY "FORNECEDOR: "       AT 0230
           DISPLAY FORNEC-CP20          AT 0243

           DISPLAY "SEQUEN: "           AT 0252
           DISPLAY SEQ-CP20             AT 0260

           DISPLAY "NR-DOCTO: "         AT 0301
           DISPLAY NR-DOCTO-CP20        AT 0311

           DISPLAY "DATA EMISSAO: "     AT 0330
           MOVE SPACES TO MASC-DATA
           STRING DATA-EMISSAO-CP20(7:2) "/" DATA-EMISSAO-CP20(5:2)
                  "/" DATA-EMISSAO-CP20(1:4) INTO MASC-DATA
           DISPLAY MASC-DATA            AT 0344

           DISPLAY "DATA VENCTO: "      AT 0401
           MOVE SPACES TO MASC-DATA
           STRING DATA-VENCTO-CP20(7:2) "/" DATA-VENCTO-CP20(5:2)
                  "/" DATA-VENCTO-CP20(1:4) INTO MASC-DATA
           DISPLAY MASC-DATA              AT 0417

           DISPLAY "DESCRICAO: "          AT 0430
           DISPLAY DESCRICAO-CP20         AT 0441

           DISPLAY "TIPO LCT: "           AT 0501
           IF PREV-DEF-CP20 = 0
              DISPLAY "DEFINITIVO"        AT 0511
           ELSE
              DISPLAY "PREVISTO"          AT 0511
           END-IF

           DISPLAY "SITUACAO: "           AT 0525
           EVALUATE SITUACAO-CP20
               WHEN 0 DISPLAY "OK"        AT 0535
               WHEN 1 DISPLAY "SUSPENSA"  AT 0535
               WHEN 2 DISPLAY "PAGA"      AT 0535
               WHEN 3 DISPLAY "ESTONADA"  AT 0535
               WHEN 4 DISPLAY "CANCELADA" AT 0535
           END-EVALUATE

           DISPLAY "LIBERADO: "           AT 0548
           EVALUATE LIBERADO-CP20
               WHEN 0 DISPLAY "NAO"       AT 0558
               WHEN 1 DISPLAY "SIM"       AT 0558
           END-EVALUATE


           DISPLAY "RESPONSAVEL: "        AT 0601
           DISPLAY RESPONSAVEL-CP20       AT 0613

           DISPLAY "DIGITADOR: "          AT 0630
           DISPLAY DIGITADOR-CP20         AT 0640

           DISPLAY "VALOR: "              AT 0701
           MOVE VALOR-TOT-CP20            TO MASC-VALOR
           DISPLAY MASC-VALOR             AT 0708

           MOVE FORNEC-CP20           TO FORNEC-CP24
           MOVE SEQ-CP20              TO SEQ-CP24
           READ CPD024 INVALID KEY
                INITIALIZE REG-CPD024.

           DISPLAY "DEPARTAMENTO: "   AT 0801
           DISPLAY DEPARTAMENTO-CP24  AT 0815
           DISPLAY "BANCO: "          AT 0840
           DISPLAY BANCO-CP24         AT 0847
           DISPLAY "AGENCIA: "        AT 0901
           DISPLAY AGENCIA-CP24       AT 0910
           DISPLAY "CONTA: "          AT 0930
           DISPLAY CONTA-CP24         AT 0937.


           DISPLAY "DADOS LOG003"         AT 1201

           PERFORM LISTAR-LOG

           IF SITUACAO-CP20 = 3 OR 4
              DISPLAY "Situacao do Título " AT 2001
              IF SITUACAO-CP20 = 3
                 DISPLAY "ESTORNADA"        AT 2020
              ELSE
                 DISPLAY "CANCELADA"        AT 2020.

           STOP " ".

       LISTAR-PROGRAMACAO4-FIM.
           EXIT.

       PROCURA SECTION.
           DISPLAY ERASE AT 0101

           DISPLAY "Informar o Nome do Programa: "  at 0101
           ACCEPT AUX-PROGRAMA                      AT 0130
           DISPLAY "Informar o Nome do Arquivo.: "  at 0201
           ACCEPT AUX-ARQUIVO                       AT 0230
           DISPLAY "Informar o Fornecedor......: "  at 0301
           ACCEPT AUX-FORNECEDOR                    at 0330
           DISPLAY "Informar o Valor...........: "  at 0401
           ACCEPT MASC-VALOR                        at 0430

           MOVE MASC-VALOR                 TO AUX-VALOR

           INITIALIZE REG-LOG003
           MOVE AUX-ARQUIVO                TO LOG3-ARQUIVO
           START LOG003 KEY IS NOT LESS LOG3-CH-ARQUIVO INVALID KEY
                MOVE "10" TO ST-LOG003.

           PERFORM UNTIL ST-LOG003 = "10"
                READ LOG003 NEXT AT END
                     MOVE "10" TO ST-LOG003
                NOT AT END
                     IF AUX-ARQUIVO <> LOG3-ARQUIVO
                        MOVE "10" TO ST-LOG003
                     ELSE
                        IF AUX-PROGRAMA = LOG3-PROGRAMA
                           MOVE LOG3-REGISTRO TO REG-CPD020
                           IF AUX-FORNECEDOR = FORNEC-CP20
                              IF AUX-VALOR = VALOR-TOT-CP20
                                 PERFORM LISTAR-PROGRAMACAO4
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                END-READ
           END-PERFORM.
       PROCURA-FIM.
           EXIT.
