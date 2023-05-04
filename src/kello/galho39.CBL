       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO39.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 22-03-2005.
      *FUNÇÃO: GERA UM NOVO ARQUIVO RCD001

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY RCPX001.

           SELECT RCD999 ASSIGN TO PATH-RCD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-RCD999
                  RECORD KEY IS CODIGO-COMIS-RC99.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY RCPW001.

       FD  RCD999.
       01  REG-RCD999.
           05  CODIGO-COMIS-RC99               PIC 9.
           05  VALOR-COMIS-RC99                PIC 99V999.


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RCD999             PIC XX       VALUE SPACES.
           05  ST-RCD001             PIC XX       VALUE SPACES.
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
           MOVE "RCD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD999
           MOVE "RCD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD001
           OPEN I-O   RCD001
           OPEN INPUT RCD999.

           CLOSE CONTROLE.
           IF ST-RCD001 = "35"
              CLOSE RCD001      OPEN OUTPUT RCD001
              CLOSE RCD001      OPEN I-O RCD001
           END-IF.
           IF ST-RCD999 = "35"
              CLOSE RCD999      OPEN OUTPUT RCD999
              CLOSE RCD999      OPEN I-O RCD999
           END-IF.

           display "Vou comecar a atualizar o CRD099" stop " ".
           display "Vou comecar a atualizar o CRD099" stop " ".

           IF ST-RCD999 <> "00"
              CLOSE RCD999
                    RCD001
              stop run.

           INITIALIZE REG-RCD999

           MOVE ZEROS TO PAG-W.

           START RCD999 KEY IS NOT LESS CODIGO-COMIS-RC99 INVALID KEY
                 MOVE "10" TO ST-RCD999.


           PERFORM UNTIL ST-RCD999 = "10"
             READ RCD999 NEXT RECORD AT END
                  MOVE "10" TO ST-RCD999
             NOT AT END
                  INITIALIZE REG-RCD001
                  MOVE CODIGO-COMIS-RC99 TO CODIGO-COMIS-RC01
                  MOVE VALOR-COMIS-RC99  TO COMIS-CHEQUE-RC01
                  MOVE 0                 TO COMIS-MOEDA-RC01
                  MOVE 0                 TO COMIS-ANTECIPADA-RC01
                  MOVE 0                 TO COMIS-DUPLICATA-RC01
                  MOVE 0                 TO COMIS-DEBITO-AUTOMATICO-RC01
                  MOVE 0                 TO COMIS-CARTAO-CREDITO-RC01
                  display reg-rcd001
                  WRITE REG-RCD001
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE RCD001
           CLOSE RCD999
           EXIT PROGRAM.
