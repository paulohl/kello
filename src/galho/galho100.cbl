       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO100.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 26-02-2010.
      *FUNÇÃO: VOLTAR SITUAÇÃO DOS TITULOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CRPX020.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CRPW020.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
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
           05  masc-dtini            pic 99/99/9999 VALUE ZEROS.
           05  masc-dtfim            pic 99/99/9999 VALUE ZEROS.
           05  acp-dtini             pic 9(08)    VALUE ZEROS.
           05  acp-dtfim             pic 9(08)    VALUE ZEROS.
           05  DATA-FIM              PIC 9(08)    VALUE ZEROS.

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
           MOVE "CRD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020

           OPEN I-O CRD020

           CLOSE    CONTROLE
           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT  CRD020
              CLOSE CRD020      OPEN I-O     CRD020
           END-IF.

       0010-DATA.
           display "Informar o Periodo: " at 0101
           accept masc-dtini              at 0121
           move masc-dtini                to acp-dtini
           if acp-dtini = 0
              go to 0010-data.

       0010-data2.
           display " ate "                at 0132
           accept masc-dtfim              at 0137
           move masc-dtfim                to acp-dtfim
           if acp-dtfim = 0
              go to 0010-data2.


           STRING ACP-DTFIM(5:4) ACP-DTFIM(3:2) ACP-DTFIM(1:2)
                                        INTO DATA-FIM
           INITIALIZE REG-CRD020
           MOVE 5                         TO SITUACAO-CR20
           STRING ACP-DTINI(5:4) ACP-DTINI(3:2) ACP-DTINI(1:2)
                                        INTO DATA-VENCTO-CR20

           START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      IF SITUACAO-CR20 <> 5
                         MOVE "10" TO ST-CRD020
                      ELSE
                         IF DATA-VENCTO-CR20 > DATA-FIM
                            MOVE "10" TO ST-CRD020
                         ELSE
                            MOVE 0 TO SITUACAO-CR20
                            REWRITE REG-CRD020 INVALID KEY
                                DISPLAY "Erro de Regravacao...CRD020"
                                   STOP " "
                            END-REWRITE
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           DISPLAY "ACABOU"
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CRD020
           EXIT PROGRAM
           STOP RUN.
