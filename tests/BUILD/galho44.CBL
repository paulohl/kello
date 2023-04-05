       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO44.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 15-04-2005.
      *FUNÇÃO: GERA UM NOVO ARQUIVO mtd025

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY MTPX025.

           SELECT MTD999 ASSIGN TO PATH-MTD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-MTD999
                  RECORD KEY IS CHAVE1-MT99 =
                       CHAVE-MT99
                       MOTIVO-MT99
                  ALTERNATE RECORD KEY IS ALT-MT99 =
                       MOTIVO-MT99
                       CONTRATO-MT99
                  ALTERNATE RECORD KEY IS ALT2-MT99 =
                       DATA-MT99
                       MOTIVO-MT99.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY MTPW025.

      *   Arquivo de inclusao de motivos
       FD  MTD999.
       01  REG-MTD999.
           05  CHAVE-MT99.
               10 CONTRATO-MT99    PIC 9(04).
               10 ALBUM-MT99       PIC 9(04).
           05  MOTIVO-MT99         PIC 9(03).
           05  DATA-MT99           PIC 9(08).
           05  FILLER              PIC X(30).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD025             PIC XX       VALUE SPACES.
           05  ST-MTD999             PIC XX       VALUE SPACES.
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
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.

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
           MOVE "MTD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD999
           MOVE "MTD025" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD025
           OPEN I-O   MTD025
           CLOSE      MTD025
           OPEN I-O   MTD025
           OPEN INPUT MTD999

           CLOSE CONTROLE.
           IF ST-MTD025 = "35"
              CLOSE MTD025      OPEN OUTPUT MTD025
              CLOSE MTD025      OPEN I-O MTD025
           END-IF.

           display "Vou comecar a atualizar o MTD025" stop " ".
           display "Vou comecar a atualizar o MTD025" stop " ".

           IF ST-MTD025 <> "00" OR ST-MTD999 <> "00"
              CLOSE MTD025
                    MTD999
              stop run.

           INITIALIZE REG-MTD999

           START MTD999 KEY IS NOT LESS CHAVE1-MT99 INVALID KEY
                 MOVE "10" TO ST-MTD999.


           PERFORM UNTIL ST-MTD999 = "10"
             READ MTD999 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD999
             NOT AT END
                  INITIALIZE REG-MTD025
                  MOVE REG-MTD999        TO REG-MTD025
                  display reg-MTD025
                  WRITE REG-MTD025
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE MTD025
           CLOSE MTD999
           EXIT PROGRAM.
