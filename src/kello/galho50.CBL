       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO48.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 09/06/2005.
      *FUNÇÃO: ALTERA LAYOUT DO COP041

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CRPX020.

          SELECT CRD999 ASSIGN TO PATH-CRD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR99 = COD-COMPL-CR99 SEQ-CR99
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR99
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CR99 = COD-COMPL-CR99
                        DATA-VENCTO-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR99 = SITUACAO-CR99
                        DATA-VENCTO-CR99 COD-COMPL-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR99 = SITUACAO-CR99
                        DATA-MOVTO-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-CR99 = SITUACAO-CR99
                        CLIENTE-CR99 DATA-VENCTO-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-CR99 = DATA-RCTO-CR99
                       SEQ-CAIXA-CR99 WITH DUPLICATES.


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CRPW020.

       FD  CRD999.
       01  REG-CRD999.
           05  DATA-MOVTO-CR99          PIC 9(8).
           05  COD-COMPL-CR99.
               10  CLASS-CLIENTE-CR99   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR99         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR99                 PIC 9(5).
           05  PORTADOR-CR99            PIC 99.
           05  CARTEIRA-CR99            PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR99        PIC 99.
           05  NR-DOCTO-CR99            PIC X(10).
           05  OUTRO-DOCTO-CR99         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR99          PIC 9.
      *    TIPO-DOCTO = 0-DUPLICATA  1-NTA-PROMISSÓRIA  2-ORG.EVENTO
      *                 3-DEBITO AUTOMATICO 4-CARTAO CREDITO
           05  DATA-EMISSAO-CR99        PIC 9(8).
           05  DATA-VENCTO-CR99         PIC 9(8).
      *    DATA-VENCTO-CR20 - AAAAMMDD
           05  DESCRICAO-CR99           PIC X(30).
           05  SITUACAO-CR99            PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA  4-CANCELADA
           05  TIPO-MOEDA-CR99          PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR99.
               10  NR-PARC-CR99         PIC 99.
               10  TOT-PARC-CR99        PIC 99.
           05  CODREDUZ-APUR-CR99       PIC 9(3).
           05  VALOR-TOT-CR99           PIC 9(8)V99.
           05  JURO-RCTO-CR99           PIC 9(6)V99.
           05  MULTA-RCTO-CR99          PIC 9(6)V99.
           05  DESCONTO-CR99            PIC 9(6)V99.
           05  DATA-RCTO-CR99           PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR99           PIC 9(8)V99.
           05  CONTABILIZADO-CR99       PIC 9.
           05  VENDEDOR-CR99            PIC 9(6).
           05  RESPONSAVEL-CR99         PIC X(5).
           05  DIGITADOR-CR99           PIC X(5).
           05  SEQ-CAIXA-CR99           PIC 9(3).
           05  NR-NOTA-FISCAL-CR99      PIC X(10).
           05  DATA-NTA-FISCAL-CR99     PIC 9(8).
           05  FORMA-PAGTO-CR99         PIC X(10).
           05  DCR-MEM-CR99             PIC X(15).
           05  CARTAO-CRED-CR99         PIC 9(02).
           05  TAXA-ADMINISTRATIVA-CR99 PIC 9(03)V99.
           05  FILLER                   PIC X(23).



       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD999             PIC XX       VALUE SPACES.
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
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020
           MOVE "CRD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD999
           OPEN I-O   CRD020
           CLOSE      CRD020
           OPEN I-O   CRD020
           OPEN INPUT CRD999

           CLOSE CONTROLE.
           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O CRD020
           END-IF.

           display "Vou comecar a atualizar o CRD020" stop " ".
           display "Vou comecar a atualizar o CRD020" stop " ".

           IF ST-CRD020 <> "00" OR ST-CRD999 <> "00"
              CLOSE CRD020
                    CRD999
              stop run.

           INITIALIZE REG-CRD999

           START CRD999 KEY IS NOT LESS CHAVE-CR99 INVALID KEY
                 MOVE "10" TO ST-CRD999.


           PERFORM UNTIL ST-CRD999 = "10"
             READ CRD999 NEXT RECORD AT END
                  MOVE "10" TO ST-CRD999
             NOT AT END
                  MOVE DATA-MOVTO-CR99          TO DATA-MOVTO-CR20
                  MOVE COD-COMPL-CR99           TO COD-COMPL-CR20
                  MOVE SEQ-CR99                 TO SEQ-CR20
                  MOVE PORTADOR-CR99            TO PORTADOR-CR20
                  MOVE CARTEIRA-CR99            TO CARTEIRA-CR20
                  MOVE SITUACAO-TIT-CR99        TO SITUACAO-TIT-CR20
                  MOVE NR-DOCTO-CR99            TO NR-DOCTO-CR20
                  MOVE OUTRO-DOCTO-CR99         TO OUTRO-DOCTO-CR20
                  MOVE TIPO-DOCTO-CR99          TO TIPO-DOCTO-CR20
                  MOVE DATA-EMISSAO-CR99        TO DATA-EMISSAO-CR20
                  MOVE DATA-VENCTO-CR99         TO DATA-VENCTO-CR20
                  MOVE DESCRICAO-CR99           TO DESCRICAO-CR20
                  MOVE SITUACAO-CR99            TO SITUACAO-CR20
                  MOVE TIPO-MOEDA-CR99          TO TIPO-MOEDA-CR20
                  MOVE NR-PARCELA-CR99          TO NR-PARCELA-CR20
                  MOVE CODREDUZ-APUR-CR99       TO CODREDUZ-APUR-CR20
                  MOVE VALOR-TOT-CR99           TO VALOR-TOT-CR20
                  MOVE JURO-RCTO-CR99           TO JURO-RCTO-CR20
                  MOVE MULTA-RCTO-CR99          TO MULTA-RCTO-CR20
                  MOVE DESCONTO-CR99            TO DESCONTO-CR20
                  MOVE DATA-RCTO-CR99           TO DATA-RCTO-CR20
                  MOVE VALOR-LIQ-CR99           TO VALOR-LIQ-CR20
                  MOVE CONTABILIZADO-CR99       TO CONTABILIZADO-CR20
                  MOVE VENDEDOR-CR99            TO VENDEDOR-CR20
                  MOVE RESPONSAVEL-CR99         TO RESPONSAVEL-CR20
                  MOVE DIGITADOR-CR99           TO DIGITADOR-CR20
                  MOVE SEQ-CAIXA-CR99           TO SEQ-CAIXA-CR20
                  MOVE NR-NOTA-FISCAL-CR99      TO NR-NOTA-FISCAL-CR20
                  MOVE DATA-NTA-FISCAL-CR99     TO DATA-NTA-FISCAL-CR20
                  MOVE FORMA-PAGTO-CR99         TO FORMA-PAGTO-CR20
                  MOVE DCR-MEM-CR99             TO DCR-MEM-CR20
                  MOVE CARTAO-CRED-CR99         TO CARTAO-CRED-CR20
                  MOVE TAXA-ADMINISTRATIVA-CR99 TO
                                                TAXA-ADMINISTRATIVA-CR20
                  DISPLAY REG-CRD020
                  WRITE REG-CRD020
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CRD999
           CLOSE CRD020
           EXIT PROGRAM.
