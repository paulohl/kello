       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO43.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 01-04-2005.
      *FUNÇÃO: GERA UM NOVO ARQUIVO CGD006

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY CGPX006.

           SELECT CGD999 ASSIGN TO PATH-CGD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CG99
                  STATUS IS ST-CGD999.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CGPW006.

       FD  CGD999.
       01  REG-CGD999.
           05  CHAVE-CG99.
               10  CODIGO-CG99     PIC 9(6).
               10  BANCO-CG99      PIC 9(4).
               10  AGENCIA-CG99    PIC X(9).
               10  NR-CONTA-CG99   PIC X(15).
           05  USUARIO-CG99        PIC 9(02).
           05  DESC-USUARIO-CG99   PIC X(04).
           05  FILLER              PIC X(10).


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD999             PIC XX       VALUE SPACES.
           05  ST-CGD006             PIC XX       VALUE SPACES.
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
           MOVE "CGD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD999
           MOVE "CGD006" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD006
           OPEN I-O   CGD006
           OPEN INPUT CGD999

           CLOSE CONTROLE.
           IF ST-CGD006 = "35"
              CLOSE CGD006      OPEN OUTPUT CGD006
              CLOSE CGD006      OPEN I-O CGD006
           END-IF.

           display "Vou comecar a atualizar o CGD006" stop " ".
           display "Vou comecar a atualizar o CGD006" stop " ".

           IF ST-CGD006 <> "00" OR ST-CGD999 <> "00"
              CLOSE CGD006
                    CGD999
              stop run.

           INITIALIZE REG-CGD999

           START CGD999 KEY IS NOT LESS CHAVE-CG99 INVALID KEY
                 MOVE "10" TO ST-CGD999.


           PERFORM UNTIL ST-CGD999 = "10"
             READ CGD999 NEXT RECORD AT END
                  MOVE "10" TO ST-CGD999
             NOT AT END
                  INITIALIZE REG-CGD006
                  MOVE CHAVE-CG99        TO CHAVE-CG06
                  MOVE USUARIO-CG99      TO USUARIO-CG06
                  MOVE DESC-USUARIO-CG99 TO DESC-USUARIO-CG06
                  MOVE SPACES            TO TITULAR-CONTA-CG06
                  MOVE ZEROS             TO CPF-TITULAR-CG06
                  display reg-cgd006
                  WRITE REG-CGD006
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CGD006
           CLOSE CGD999
           EXIT PROGRAM.
