       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO99.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 03/11/2009.
      *FUNÇÃO: AUMENTAR O TAMANHO DO REGISTRO CHD010

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CHPX010.
           COPY CHPX013.
           COPY CHPX010B.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CHPW010.
       COPY CHPW013.
       COPY CHPW010B.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CHD010B            PIC XX       VALUE SPACES.
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
           MOVE "CHD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010
           MOVE "CHD013"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD013
           MOVE "CHD010B" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010B

           OPEN I-O CHD010   CHD013 CHD010B
           CLOSE    CHD010   CHD013 CHD010B
           OPEN I-O CHD010   CHD013 CHD010B

           CLOSE    CONTROLE
           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT  CHD010
              CLOSE CHD010      OPEN I-O     CHD010
           END-IF.

           IF ST-CHD013 = "35"
              CLOSE CHD013      OPEN OUTPUT  CHD013
              CLOSE CHD013      OPEN I-O     CHD013
           END-IF.

           IF ST-CHD010B = "35"
              CLOSE CHD010B      OPEN OUTPUT CHD010B
              CLOSE CHD010B      OPEN I-O    CHD010B
           END-IF.

           display "vou comecar" stop " "

           INITIALIZE REG-CHD013
           MOVE 20091109 TO DATA-RECTO-CH13
           START CHD013 KEY IS NOT LESS DATA-RECTO-CH13 INVALID KEY
                MOVE "10" TO ST-CHD013.

           PERFORM UNTIL ST-CHD013 = "10"
                READ CHD013 NEXT AT END
                     MOVE "10" TO ST-CHD013
                NOT AT END
                     MOVE DATA-MOVTO-CH13   TO DATA-MOVTO-CH10
                     MOVE SEQ-CH13          TO SEQ-CH10
                     READ CHD010 INVALID KEY
                          INITIALIZE REG-CHD010
                     END-READ

                     MOVE VALOR-CH10        TO  VALOR-TOT-CH10B
                     MOVE VALOR-CH10        TO  VALOR-BAIXA-CH10B


                     MOVE DATA-MOVTO-CH13   TO DATA-MOVTO-CH10B
                     MOVE SEQ-CH13          TO SEQ-CH10B

                     MOVE DATA-RECTO-CH13   TO DATA-RCTO-CH10B

                     MOVE VLR-JUROS-CH13    TO JURO-RCTO-CH10B
                     MOVE VLR-MULTA-CH13    TO MULTA-RCTO-CH10B
                     MOVE VLR-DESCONTO-CH13 TO DESCONTO-CH10B
                     MOVE FORMA-PAGTO-CH13  TO FORMA-PAGTO-CH10B
                     MOVE DCR-MEM-R-CH13    TO DCR-MEM-CH10B
                     MOVE VALOR-CH10        TO VALOR-TOT-CH10B
                     MOVE VALOR-CH10        TO VALOR-BAIXA-CH10B

                     COMPUTE VALOR-LIQ-CH10B = VALOR-BAIXA-CH10B +
                                               VLR-JUROS-CH13  +
                                               VLR-MULTA-CH13  -
                                               VLR-DESCONTO-CH13

                     MOVE SEQ-CAIXA-CH10    TO SEQ-CAIXA-CH10B
                     MOVE VALOR-LIQ-CH10B   TO VLR-LIQUIDO-CH13

                     display "REG-CHD010B = " REG-CHD010B STOP " "

                     WRITE REG-CHD010B

                     DISPLAY "ST-CHD010B = " ST-CHD010B STOP " "

                     REWRITE REG-CHD013

                     MOVE ZEROS TO VALOR-SALDO-CH10
                     REWRITE REG-CHD010
                     END-REWRITE

                END-READ
           END-PERFORM

           DISPLAY "ACABOU"
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CHD010 CHD010B CHD013
           EXIT PROGRAM
           STOP RUN.
