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
           SELECT CHD010a ASSIGN TO PATH-CHD010a
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH10a = DATA-MOVTO-CH10a SEQ-CH10a
                  ALTERNATE RECORD KEY ALT-CH1a =
                     NOME-CH10a, PORTADOR-CH10a WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH2a = SITUACAO-CH10a
                     DATA-VENCTO-CH10a PORTADOR-CH10a WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH3a = DATA-MOVTO-CH10a
                     VENDEDOR-CH10a SEQ-CH10a WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH4a = COD-COMPL-CH10a
                     DATA-VENCTO-CH10a WITH DUPLICATES
                  STATUS IS ST-CHD010a.
           COPY CHPX013.
           COPY CHPX010B.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CHPW010.

      * ARQUIVO DO MOVIMENTO DE CHEQUES
       FD  CHD010a.
       01  REG-CHD010a.
           05  DATA-MOVTO-CH10a          PIC 9(8).
           05  SEQ-CH10a                 PIC 9(4).
           05  COD-COMPL-CH10a.
               10  CLASS-CLIENTE-CH10a   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH10a         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH10a                PIC X(30).
           05  CIDADE-CH10a              PIC X(19).
           05  LOTE-CH10a                PIC X(01).
           05  CONTA-CORR-CH10a          PIC 9(8)   COMP-3.
           05  DV-CONTA-CH10a            PIC X.
           05  BANCO-CH10a               PIC 9(4)   COMP-3.
           05  AGENCIA-CH10a             PIC 9(5)   COMP-3.
           05  DV-AGENCIA-CH10a          PIC X.
           05  COMPENSACAO-CH10a         PIC 9(3).
           05  CPF-CH10a                 PIC 9(11)  COMP-3.
           05  NR-CHEQUE-CH10a           PIC X(7).
           05  OUTRO-DOCTO-CH10a         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-CH10a         PIC 9(8).
           05  PORTADOR-CH10a            PIC 9999.
           05  CARTEIRA-CH10a            PIC 9.
      *    CARTEIRA-CH10  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH10a        PIC 99.
           05  SITUACAO-CH10a            PIC 9.
      *    SITUACAO = 0-OK  2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH10a       PIC 9(5).
           05  VALOR-CH10a               PIC 9(8)V99.
           05  VENDEDOR-CH10a            PIC 9(6)   COMP-3.
           05  DIGITADOR-CH10a           PIC X(5).
           05  SEQ-CAIXA-CH10a           PIC 9(3).
           05  NR-NOTA-FISCAL-CH10a      PIC X(10).
           05  DATA-NTA-FISCAL-CH10a     PIC 9(8)   COMP-3.
           05  ORIGEM-CH10a              PIC 99.

       COPY CHPW013.
       COPY CHPW010B.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD010a            PIC XX       VALUE SPACES.
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
           MOVE "CHD010A" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010A
           MOVE "CHD013"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD013
           MOVE "CHD010B" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010B

           OPEN I-O CHD010   CHD010A CHD013 CHD010B
           CLOSE    CHD010   CHD010A CHD013 CHD010B
           OPEN I-O CHD010   CHD010A CHD013 CHD010B

           CLOSE    CONTROLE
           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT  CHD010
              CLOSE CHD010      OPEN I-O     CHD010
           END-IF.

           IF ST-CHD010A = "35"
              CLOSE CHD010A      OPEN OUTPUT CHD010A
              CLOSE CHD010A      OPEN I-O    CHD010A
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

           INITIALIZE REG-CHD010a

           MOVE ZEROS          TO DATA-MOVTO-CH10a SEQ-CH10a

           START CHD010a KEY IS NOT LESS CHAVE-CH10a INVALID KEY
                 MOVE "10" TO ST-CHD010a.

           PERFORM UNTIL ST-CHD010a = "10"
                 READ CHD010a NEXT RECORD AT END
                      MOVE "10" TO ST-CHD010a
                 NOT AT END
                      INITIALIZE REG-CHD010

                      MOVE DATA-MOVTO-CH10a      TO DATA-MOVTO-CH10
                      MOVE SEQ-CH10a             TO SEQ-CH10
                      MOVE CLASS-CLIENTE-CH10a   TO CLASS-CLIENTE-CH10
                      MOVE CLIENTE-CH10a         TO CLIENTE-CH10
                      MOVE NOME-CH10a            TO NOME-CH10
                      MOVE CIDADE-CH10a          TO CIDADE-CH10
                      MOVE LOTE-CH10a            TO LOTE-CH10
                      MOVE CONTA-CORR-CH10a      TO CONTA-CORR-CH10
                      MOVE DV-CONTA-CH10a        TO DV-CONTA-CH10
                      MOVE BANCO-CH10a           TO BANCO-CH10
                      MOVE AGENCIA-CH10a         TO AGENCIA-CH10
                      MOVE DV-AGENCIA-CH10a      TO DV-AGENCIA-CH10
                      MOVE COMPENSACAO-CH10a     TO COMPENSACAO-CH10
                      MOVE CPF-CH10a             TO CPF-CH10
                      MOVE NR-CHEQUE-CH10a       TO NR-CHEQUE-CH10
                      MOVE OUTRO-DOCTO-CH10a     TO OUTRO-DOCTO-CH10
                      MOVE DATA-VENCTO-CH10a     TO DATA-VENCTO-CH10
                      MOVE PORTADOR-CH10a        TO PORTADOR-CH10
                      MOVE CARTEIRA-CH10a        TO CARTEIRA-CH10
                      MOVE SITUACAO-TIT-CH10a    TO SITUACAO-TIT-CH10
                      MOVE SITUACAO-CH10a        TO SITUACAO-CH10
                      MOVE CODREDUZ-APUR-CH10a   TO CODREDUZ-APUR-CH10
                      MOVE VALOR-CH10a           TO VALOR-CH10
                      MOVE VENDEDOR-CH10a        TO VENDEDOR-CH10
                      MOVE DIGITADOR-CH10a       TO DIGITADOR-CH10
                      MOVE SEQ-CAIXA-CH10a       TO SEQ-CAIXA-CH10
                      MOVE NR-NOTA-FISCAL-CH10a  TO NR-NOTA-FISCAL-CH10
                      MOVE DATA-NTA-FISCAL-CH10a TO DATA-NTA-FISCAL-CH10

                      MOVE ORIGEM-CH10a          TO ORIGEM-CH10

                      IF SITUACAO-CH10A = 2
                         MOVE ZEROS              TO VALOR-SALDO-CH10
                      ELSE
                         MOVE VALOR-CH10a        TO VALOR-SALDO-CH10
                      END-IF

                      DISPLAY REG-CHD010

                      WRITE REG-CHD010
                      END-WRITE
                 END-READ
           END-PERFORM.

           INITIALIZE REG-CHD013
           START CHD013 KEY IS NOT LESS CHAVE-CH13 INVALID KEY
                 MOVE "10" TO ST-CHD013.

           PERFORM UNTIL ST-CHD013 = "10"
                 READ CHD013 NEXT AT END
                      MOVE "10" TO ST-CHD013
                 NOT AT END
                      IF DATA-RECTO-CH13 > 0
                         INITIALIZE REG-CHD010B

                         MOVE DATA-MOVTO-CH13   TO DATA-MOVTO-CH10B
                                                   DATA-MOVTO-CH10
                         MOVE SEQ-CH13          TO SEQ-CH10B
                                                   SEQ-CH10
                         READ CHD010 INVALID KEY
                              INITIALIZE REG-CHD010
                         END-READ

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

                         DISPLAY REG-CHD010B
                         WRITE REG-CHD010B

                         MOVE VALOR-LIQ-CH10B   TO VLR-LIQUIDO-CH13
                         REWRITE REG-CHD013

                      END-IF
                 END-READ
           END-PERFORM

           DISPLAY "ACABOU"
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CHD010 CHD010a CHD010B CHD013
           EXIT PROGRAM
           STOP RUN.
