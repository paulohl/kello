       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO13.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 18/10/2004.
      *FUNÇÃO: ATUALIZAR O VALOR INICIAL DO COD051 (BRINDES FORMANDOS)

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY COPX050.
           COPY COPX051.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY COPW050.
       COPY COPW051.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-COD051             PIC XX       VALUE SPACES.
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
           MOVE "COD050" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "COD051" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD051.
           OPEN I-O COD050.
           OPEN I-O COD051.
           CLOSE CONTROLE.
           IF ST-COD050 = "35"
              CLOSE COD050      OPEN OUTPUT COD050
              CLOSE COD050      OPEN I-O COD050
           END-IF.
           IF ST-COD051 = "35"
              CLOSE COD051      OPEN OUTPUT COD051
              CLOSE COD051      OPEN I-O COD051
           END-IF.

           INITIALIZE REG-COD050

           MOVE ZEROS TO PAG-W.

           MOVE ZEROS          TO NR-CONTRATO-CO50.
           MOVE ZEROS          TO ITEM-CO50.

           START COD050 KEY IS NOT LESS CHAVE-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.


           PERFORM UNTIL ST-COD050 = "10"
             READ COD050 NEXT RECORD AT END MOVE "10" TO ST-COD050
              NOT AT END
                   DISPLAY REG-COD050
                   MOVE REG-COD050          TO REG-COD051
                   WRITE REG-COD051
                   END-WRITE
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE COD050
           CLOSE COD051
           EXIT PROGRAM.
