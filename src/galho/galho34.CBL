       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO34.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 01/02/2005.
      *FUNÇÃO: GERA UM NOVO ARQUIVO CHD013 C/ O DCR-MEMORANDO

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CHPX013.

           SELECT CHD999 ASSIGN TO PATH-CHD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CHD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH99 = DATA-MOVTO-CH99 SEQ-CH99
                  ALTERNATE RECORD KEY IS DATA-ENTRADA-CH99
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-RECTO-CH99
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-APRES-CH99
                            WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CHPW013.


       FD  CHD999.
       01  REG-CHD999.
           05  DATA-MOVTO-CH99            PIC 9(8).
           05  SEQ-CH99                   PIC 9(4).
           05  ALINEA-CH99                PIC 99.
           05  DATA-ENTRADA-CH99          PIC 9(8)      COMP-3.
           05  DATA-COMPRA-CH99           PIC 9(8)      COMP-3.
           05  DATA-APRES-CH99            PIC 9(8)      COMP-3.
           05  DATA-REAPRES-CH99          PIC 9(08)     COMP-3.
           05  DATA-RECTO-CH99            PIC 9(8)      COMP-3.
           05  VLR-JUROS-CH99             PIC 9(6)V99   COMP-3.
           05  VLR-MULTA-CH99             PIC 9(6)V99   COMP-3.
           05  VLR-DESCONTO-CH99          PIC 9(6)V99   COMP-3.
           05  FORMA-PAGTO-CH99           PIC X(10).
           05  DCR-MEM-CH99               PIC X(15).


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CHD999             PIC XX       VALUE SPACES.
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
           MOVE "CHD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD999
           MOVE "CHD013" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD013
           OPEN I-O   CHD013.
           OPEN INPUT CHD999.
           CLOSE CONTROLE.
           IF ST-CHD999 = "35"
              CLOSE CHD999      OPEN OUTPUT CHD999
              CLOSE CHD999      OPEN I-O CHD999
           END-IF.
           IF ST-CHD013 = "35"
              CLOSE CHD013      OPEN OUTPUT CHD013
              CLOSE CHD013      OPEN I-O CHD013
           END-IF.

           INITIALIZE REG-CHD999

           MOVE ZEROS TO PAG-W.

           START CHD999 KEY IS NOT LESS CHAVE-CH99 INVALID KEY
                 MOVE "10" TO ST-CHD999.


           PERFORM UNTIL ST-CHD999 = "10"
             READ CHD999 NEXT RECORD AT END MOVE "10" TO ST-CHD999
              NOT AT END
                   INITIALIZE REG-CHD013
                   MOVE DATA-MOVTO-CH99      TO DATA-MOVTO-CH13
                   MOVE SEQ-CH99             TO SEQ-CH13
                   MOVE ALINEA-CH99          TO ALINEA-CH13
                   MOVE DATA-ENTRADA-CH99    TO DATA-ENTRADA-CH13
                   MOVE DATA-COMPRA-CH99     TO DATA-COMPRA-CH13
                   MOVE DATA-APRES-CH99      TO DATA-APRES-CH13
                   MOVE DATA-REAPRES-CH99    TO DATA-REAPRES-CH13
                   MOVE DATA-RECTO-CH99      TO DATA-RECTO-CH13
                   MOVE VLR-JUROS-CH99       TO VLR-JUROS-CH13
                   MOVE VLR-MULTA-CH99       TO VLR-MULTA-CH13
                   MOVE VLR-DESCONTO-CH99    TO VLR-DESCONTO-CH13
                   MOVE FORMA-PAGTO-CH99     TO FORMA-PAGTO-CH13
                   MOVE DCR-MEM-CH99         TO DCR-MEM-CH13
                   MOVE SPACES               TO DCR-MEM-R-CH13
                   WRITE REG-CHD013
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CHD999
           CLOSE CHD013
           EXIT PROGRAM.
