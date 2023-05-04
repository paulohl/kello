       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO17.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 07/12/2004.
      *FUNÇÃO: GERA UM NOVO ARQUIVO MTD999
      * MTD999 => IGUAL O MTD020 APARTIR DO DIA 8/11/2004

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY MTPX020.

       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.
           COPY MTPW020.


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD020             PIC XX       VALUE SPACES.
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
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           OPEN INPUT MTD020
           CLOSE CONTROLE.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.

           DISPLAY "VOU COMEÇAR O GALHO18" STOP " ".
           DISPLAY "VOU COMEÇAR O GALHO18" STOP " ".

           INITIALIZE REG-MTD020

           START MTD020 KEY IS NOT LESS CHAVE-MTG INVALID KEY
                 MOVE "10" TO ST-MTD020.
              DISPLAY ST-MTD020 STOP "ST-MTD020 1"

           MOVE ZEROS TO CONTADOR

           PERFORM UNTIL ST-MTD020 = "10"
             READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
              NOT AT END
                   IF ANOMES-VISITA-MTG = 0
                      ADD 1 TO CONTADOR
                   END-IF
              END-READ
           END-PERFORM.

           DISPLAY "CONTADOR = " CONTADOR STOP " "


           DISPLAY "ACABOU DE EXECUTAR O GALHO18" STOP " ".

           CLOSE MTD020
           EXIT PROGRAM
           STOP RUN.
