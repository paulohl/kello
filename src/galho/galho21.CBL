       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO21.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 15/12/2004.
      *FUNÇÃO: LISTA OS ARQUIVOS DO RCD101 QUE ESTÃO SEM BAIXA

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY RCPX101.

       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.
           COPY RCPW101.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RCD101             PIC XX       VALUE SPACES.
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
           05  CONTADOR              PIC 9(09)    VALUE ZEROS.

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
           MOVE "RCD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD101.
           OPEN I-O RCD101
           CLOSE CONTROLE.
           IF ST-RCD101 = "35"
              CLOSE RCD101      OPEN OUTPUT RCD101
              CLOSE RCD101      OPEN I-O RCD101
           END-IF.

           DISPLAY "VOU COMEÇAR O GALHO21" STOP " ".

           INITIALIZE REG-RCD101

           START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101.

           DISPLAY ST-RCD101 STOP "ST-RCD101 1"

           MOVE 0 TO CONTADOR

           PERFORM UNTIL ST-RCD101 = "10"
             READ RCD101 NEXT RECORD AT END
                  MOVE "10" TO ST-RCD101
             NOT AT END
                IF DTA-BAIXA-REC1 = 0


                END-IF
             END-READ
           END-PERFORM.

           DISPLAY "ACABOU DE EXECUTAR O GALHO21" STOP " ".

           CLOSE RCD101
           EXIT PROGRAM
           STOP RUN.
