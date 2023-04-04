       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO15.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 10/11/2004.
      *FUNÇÃO: GERA UM NOVO ARQUIVO CRD998 E CRD999
      * CRD998 => IGUAL O CRD200 APARTIR DO DIA 8/11/2004
      * CRD999 => IGUAL 0 CRD201 APARTIR DO DIA 8/11/2004

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CRPX200.
           COPY CRPX201.

          SELECT CRD998 ASSIGN TO PATH-CRD998
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD998
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR998 = COD-COMPL-CR998 SEQ-CR998
                  ALTERNATE RECORD KEY IS ALT1-CR998 = COD-COMPL-CR998
                         DATA-MOVTO-CR998 HORA-MOVTO-CR998
                         WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR998 =
                      DATA-RETORNO-CR998 USUARIO-CR998 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR998 = DATA-MOVTO-CR998
                      HORA-MOVTO-CR998 USUARIO-CR998 WITH DUPLICATES.

          SELECT CRD300 ASSIGN TO PATH-CRD300
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD300
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR300 = COD-COMPL-CR300 SEQ-CR300
                  ALTERNATE RECORD KEY IS ALT1-CR300 = COD-COMPL-CR300
                         DATA-MOVTO-CR300 HORA-MOVTO-CR300
                         WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR300 =
                      DATA-RETORNO-CR300 USUARIO-CR300 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR300 = DATA-MOVTO-CR300
                      HORA-MOVTO-CR300 USUARIO-CR300 WITH DUPLICATES.


          SELECT CRD999 ASSIGN TO PATH-CRD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR999 = COD-COMPL-CR999 SEQ-CR999
                                              SUBSEQ-CR999.

          SELECT CRD301 ASSIGN TO PATH-CRD301
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD301
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR301 = COD-COMPL-CR301 SEQ-CR301
                                              SUBSEQ-CR301.


       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.
           COPY CRPW200.
           COPY CRPW201.

      *CADASTRO DE ANOTAÇÕES DO CONTAS A RECEBER E CHEQUES
      * ARQUIVO NOVO
       FD  CRD998.
       01  REG-CRD998.
           05  COD-COMPL-CR998     PIC 9(9).
           05  SEQ-CR998           PIC 9(3).
           05  DATA-MOVTO-CR998    PIC 9(8).
           05  HORA-MOVTO-CR998    PIC 9(4).
           05  DATA-RETORNO-CR998  PIC 9(8).
           05  USUARIO-CR998       PIC X(5).
           05  SITUACAO-ANOTACAO-CR998 PIC 9.
      *    SITUACAO-ANOTACAO-CR998 = 0-PENDENTE  1-CHECADO

      *COMPLEMENTO DE ANOTAÇÕES DO CONTAS A RECEBER E CHEQUE-CONT CRD998
      * ARQUIVO NOVO
       FD  CRD999.
       01  REG-CRD999.
           05  COD-COMPL-CR999     PIC 9(9).
           05  SEQ-CR999           PIC 9(3).
           05  SUBSEQ-CR999        PIC 9(2).
           05  ANOTACAO-CR999      PIC X(80).

      *CADASTRO DE ANOTAÇÕES DO CONTAS A RECEBER E CHEQUES
      * ARQUIVO ANTIGO

       FD  CRD300.
       01  REG-CRD300.
           05  COD-COMPL-CR300     PIC 9(9).
           05  SEQ-CR300           PIC 9(3).
           05  DATA-MOVTO-CR300    PIC 9(8).
           05  HORA-MOVTO-CR300    PIC 9(4).
           05  DATA-RETORNO-CR300  PIC 9(8).
           05  USUARIO-CR300       PIC X(5).
           05  SITUACAO-ANOTACAO-CR300 PIC 9.
      *    SITUACAO-ANOTACAO-CR300 = 0-PENDENTE  1-CHECADO

      *COMPLEMENTO DE ANOTAÇÕES DO CONTAS A RECEBER E CHEQUE-CONT CRD300
      * ARQUIVO ANTIGO
       FD  CRD301.
       01  REG-CRD301.
           05  COD-COMPL-CR301     PIC 9(9).
           05  SEQ-CR301           PIC 9(3).
           05  SUBSEQ-CR301        PIC 9(2).
           05  ANOTACAO-CR301      PIC X(80).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-CRD998             PIC XX       VALUE SPACES.
           05  ST-CRD999             PIC XX       VALUE SPACES.
           05  ST-CRD300             PIC XX       VALUE SPACES.
           05  ST-CRD301             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
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

           COPY "PARAMETR".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "CRD998" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD998.
           MOVE "CRD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD999.
           MOVE "CRD300" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD300.
           MOVE "CRD301" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD301.
           OPEN I-O CRD200 CRD201 CRD998 CRD999 CRD300 CRD301.
           CLOSE CONTROLE.
           IF ST-CRD200 = "35"
              CLOSE CRD200      OPEN OUTPUT CRD200
              CLOSE CRD200      OPEN I-O CRD200
           END-IF.
           IF ST-CRD201 = "35"
              CLOSE CRD201      OPEN OUTPUT CRD201
              CLOSE CRD201      OPEN I-O CRD201
           END-IF.
           IF ST-CRD998 = "35"
              CLOSE CRD998      OPEN OUTPUT CRD998
              CLOSE CRD998      OPEN I-O CRD998
           END-IF.
           IF ST-CRD999 = "35"
              CLOSE CRD999      OPEN OUTPUT CRD999
              CLOSE CRD999      OPEN I-O CRD999
           END-IF.
           IF ST-CRD300 = "35"
              CLOSE CRD300      OPEN OUTPUT CRD300
              CLOSE CRD300      OPEN I-O CRD300
           END-IF.
           IF ST-CRD301 = "35"
              CLOSE CRD301      OPEN OUTPUT CRD301
              CLOSE CRD301      OPEN I-O CRD301
           END-IF.
           DISPLAY "VOU COMEÇAR O GALHO16" STOP " ".
           DISPLAY "VOU COMEÇAR O GALHO16" STOP " ".

           INITIALIZE REG-CRD200

           MOVE 20041108       TO  DATA-MOVTO-CR200
           MOVE ZEROS          TO  HORA-MOVTO-CR200
           MOVE SPACES         TO  USUARIO-CR200

           START CRD200 KEY IS NOT LESS ALT3-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.

           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
              NOT AT END
                      display reg-crd200 "CRD200"
                      MOVE REG-CRD200 TO REG-CRD998
                      WRITE REG-CRD998
                      END-WRITE
                      MOVE ZEROS      TO COD-COMPL-CR201
                                         SEQ-CR201
                                         SUBSEQ-CR201
                      MOVE COD-COMPL-CR200 TO COD-COMPL-CR201
                      START CRD201 KEY IS NOT LESS CHAVE-CR201
                                                             INVALID KEY
                            MOVE "10" TO ST-CRD201
                      END-START
                      PERFORM UNTIL ST-CRD201 = "10"
                           READ CRD201 NEXT RECORD AT END
                               MOVE "10" TO ST-CRD201
                           END-READ
                           IF COD-COMPL-CR200 <> COD-COMPL-CR201
                               MOVE "10" TO ST-CRD201
                           ELSE
                               DISPLAY REG-CRD201 "CRD201"
                               MOVE REG-CRD201 TO REG-CRD999
                               WRITE REG-CRD999
                               END-WRITE
                           END-IF
                      END-PERFORM
              END-READ
           END-PERFORM.

           INITIALIZE REG-CRD300

           MOVE ZEROS          TO  DATA-MOVTO-CR300
           MOVE ZEROS          TO  HORA-MOVTO-CR300
           MOVE SPACES         TO  USUARIO-CR300

           START CRD300 KEY IS NOT LESS ALT3-CR300 INVALID KEY
                 MOVE "10" TO ST-CRD300.

           PERFORM UNTIL ST-CRD300 = "10"
             READ CRD300 NEXT RECORD AT END MOVE "10" TO ST-CRD300
              NOT AT END
                      display reg-crd300 "CRD300"
                      MOVE REG-CRD300 TO REG-CRD998
                      WRITE REG-CRD998
                      END-WRITE
                      MOVE ZEROS      TO COD-COMPL-CR301
                                         SEQ-CR301
                                         SUBSEQ-CR301
                      MOVE COD-COMPL-CR300 TO COD-COMPL-CR301
                      START CRD301 KEY IS NOT LESS CHAVE-CR301
                                                             INVALID KEY
                            MOVE "10" TO ST-CRD301
                      END-START
                      PERFORM UNTIL ST-CRD301 = "10"
                           READ CRD301 NEXT RECORD AT END
                               MOVE "10" TO ST-CRD301
                           END-READ
                           IF COD-COMPL-CR300 <> COD-COMPL-CR301
                               MOVE "10" TO ST-CRD301
                           ELSE
                               DISPLAY REG-CRD301 "CRD301"
                               MOVE REG-CRD301 TO REG-CRD999
                               WRITE REG-CRD999
                               END-WRITE
                           END-IF
                      END-PERFORM
              END-READ
           END-PERFORM.





           DISPLAY "ACABOU DE EXECUTAR O GALHO16" STOP " ".

           CLOSE CRD200 CRD201 CRD998 CRD999 CRD300 CRD301
           EXIT PROGRAM
           STOP RUN.
