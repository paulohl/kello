       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO31.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 25/01/2005.
      *FUNÇÃO: GERA UM NOVO ARQUIVO PRD012

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY PRPX012.

           SELECT PRD999 ASSIGN TO PATH-PRD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-PRD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CONTRATO-PR99.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY PRPW012.

       FD  PRD999.
       01  REG-PRD999.
           05  CONTRATO-PR99            PIC 9(04).
           05  OBSERVACAO-PR99          PIC X(80).


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-PRD012             PIC XX       VALUE SPACES.
           05  ST-PRD999             PIC XX       VALUE SPACES.
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
           MOVE "PRD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PRD999
           MOVE "PRD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PRD012
           OPEN I-O   PRD012.
           OPEN INPUT PRD999.
           CLOSE CONTROLE.
           IF ST-PRD999 = "35"
              CLOSE PRD999      OPEN OUTPUT PRD999
              CLOSE PRD999      OPEN I-O PRD999
           END-IF.
           IF ST-PRD012 = "35"
              CLOSE PRD012      OPEN OUTPUT PRD012
              CLOSE PRD012      OPEN I-O PRD012
           END-IF.

           INITIALIZE REG-PRD999

           MOVE ZEROS TO PAG-W.

           START PRD999 KEY IS NOT LESS CONTRATO-PR99 INVALID KEY
                 MOVE "10" TO ST-PRD999.


           PERFORM UNTIL ST-PRD999 = "10"
             READ PRD999 NEXT RECORD AT END MOVE "10" TO ST-PRD999
              NOT AT END
                   MOVE CONTRATO-PR99      TO CONTRATO-PR12
                   MOVE 1                  TO SEQUENCIA-PR12
                   MOVE OBSERVACAO-PR99    TO OBSERVACAO-PR12
                   WRITE REG-PRD012
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE PRD999
           CLOSE PRD012
           EXIT PROGRAM.
