       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO76.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 13-02-2007.
      *FUNÇÃO: gera um novo arquivo de portadores

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CAPX018.

           COPY CHPX010.

           COPY CRPX020.

           SELECT CAD999 ASSIGN TO PATH-CAD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-CAD999
                  RECORD KEY IS PORTADOR-99
                  ALTERNATE RECORD KEY IS NOME-PORT-99.

           SELECT CHD999 ASSIGN TO PATH-CHD999
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH99 = DATA-MOVTO-CH99 SEQ-CH99
                  ALTERNATE RECORD KEY ALT-CH91 =
                     NOME-CH99, PORTADOR-CH99 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH92 = SITUACAO-CH99
                     DATA-VENCTO-CH99 PORTADOR-CH99 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH93 = DATA-MOVTO-CH99
                     VENDEDOR-CH99 SEQ-CH99 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH94 = COD-COMPL-CH99
                     DATA-VENCTO-CH99 WITH DUPLICATES
                  STATUS IS ST-CHD999.

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
                       SEQ-CAIXA-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT7-CR99 = NR-DOCTO-CR99
                       DATA-RCTO-CR99
                       SEQ-CAIXA-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT8-CR99 = OUTRO-DOCTO-CR99
                       DATA-RCTO-CR99
                       SEQ-CAIXA-CR99 WITH DUPLICATES.


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CAPW018.

           COPY CHPW010.

           COPY CRPW020.

      *  Cadastro de Portador
       FD  CAD999.
       01  REG-CAD999.
           05  PORTADOR-99            PIC 99.
           05  NOME-PORT-99           PIC X(20).

      * ARQUIVO DO MOVIMENTO DE CHEQUES
       FD  CHD999.
       01  REG-CHD999.
           05  DATA-MOVTO-CH99          PIC 9(8).
           05  SEQ-CH99                 PIC 9(4).
           05  COD-COMPL-CH99.
               10  CLASS-CLIENTE-CH99   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH99         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH99                PIC X(30).
           05  CIDADE-CH99              PIC X(19).
           05  LOTE-CH99                PIC X(01).
           05  CONTA-CORR-CH99          PIC 9(8)   COMP-3.
           05  DV-CONTA-CH99            PIC X.
           05  BANCO-CH99               PIC 9(4)   COMP-3.
           05  AGENCIA-CH99             PIC 9(5)   COMP-3.
           05  DV-AGENCIA-CH99          PIC X.
           05  COMPENSACAO-CH99         PIC 9(3).
           05  CPF-CH99                 PIC 9(11)  COMP-3.
           05  NR-CHEQUE-CH99           PIC X(7).
           05  OUTRO-DOCTO-CH99         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-CH99         PIC 9(8).
           05  PORTADOR-CH99            PIC 99.
           05  CARTEIRA-CH99            PIC 9.
      *    CARTEIRA-CH10  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH99        PIC 99.
           05  SITUACAO-CH99            PIC 9.
      *    SITUACAO = 0-OK  2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH99       PIC 9(3).
           05  VALOR-CH99               PIC 9(8)V99.
           05  VENDEDOR-CH99            PIC 9(6)   COMP-3.
           05  DIGITADOR-CH99           PIC X(5).
           05  SEQ-CAIXA-CH99           PIC 9(3).
           05  NR-NOTA-FISCAL-CH99      PIC X(10).
           05  DATA-NTA-FISCAL-CH99     PIC 9(8)   COMP-3.
           05  ORIGEM-CH99              PIC 99.

      * Arquivo de Movimento de contas a receber
       FD  CRD999.
       01  REG-CRD999.
           05  DATA-MOVTO-CR99                  PIC 9(8).
           05  COD-COMPL-CR99.
               10  CLASS-CLIENTE-CR99           PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR99                 PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR99                         PIC 9(5).
           05  PORTADOR-CR99                    PIC 99.
           05  CARTEIRA-CR99                    PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR99                PIC 99.
           05  NR-DOCTO-CR99                    PIC X(10).
           05  OUTRO-DOCTO-CR99                 PIC X(25).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR99                  PIC 9.
      *    TIPO-DOCTO = 0-DUPLICATA  1-NTA-PROMISSÓRIA  2-ORG.EVENTO
      *                 3-DEBITO AUTOMATICO 4-CARTAO CREDITO
           05  DATA-EMISSAO-CR99                PIC 9(8).
           05  DATA-VENCTO-CR99                 PIC 9(8).
      *    DATA-VENCTO-CR20 - AAAAMMDD
           05  DESCRICAO-CR99                   PIC X(30).
           05  SITUACAO-CR99                    PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA  4-CANCELADA
           05  TIPO-MOEDA-CR99                  PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR99.
               10  NR-PARC-CR99                 PIC 99.
               10  TOT-PARC-CR99                PIC 99.
           05  CODREDUZ-APUR-CR99               PIC 9(3).
           05  VALOR-TOT-CR99                   PIC 9(8)V99.
           05  JURO-RCTO-CR99                   PIC 9(6)V99.
           05  MULTA-RCTO-CR99                  PIC 9(6)V99.
           05  DESCONTO-CR99                    PIC 9(6)V99.
           05  DATA-RCTO-CR99                   PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR99                   PIC 9(8)V99.
           05  CONTABILIZADO-CR99               PIC 9.
           05  VENDEDOR-CR99                    PIC 9(6).
           05  RESPONSAVEL-CR99                 PIC X(5).
           05  DIGITADOR-CR99                   PIC X(5).
           05  SEQ-CAIXA-CR99                   PIC 9(3).
           05  NR-NOTA-FISCAL-CR99              PIC X(10).
           05  DATA-NTA-FISCAL-CR99             PIC 9(8).
           05  FORMA-PAGTO-CR99                 PIC X(10).
           05  DCR-MEM-CR99                     PIC X(15).
           05  CARTAO-CRED-CR99                 PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR99       PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR99       PIC 9(03)V99.
           05  FILLER                           PIC X(18).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD999             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD999             PIC XX       VALUE SPACES.
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
           05  ACP-DATA-MOVTO        PIC 9(08)    VALUE ZEROS.
           05  ACP-SEQUENCIA         PIC 9(04)    VALUE ZEROS.
           05  ACP-CODRED            PIC 9(03)    VALUE ZEROS.
           05  CONTINUAR             PIC X(01)    VALUE SPACES.
           05  MASC-DATA             PIC 99/99/9999 VALUE ZEROS.
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
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018
           MOVE "CAD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD999
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010
           MOVE "CHD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD999
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020
           MOVE "CRD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD999
           OPEN I-O   CAD018 CAD999 CHD010 CHD999  CRD020 CRD999
           CLOSE      CAD018 CAD999 CHD010 CHD999  CRD020 CRD999
           OPEN I-O   CAD018 CAD999 CHD010 CHD999  CRD020 CRD999

           CLOSE CONTROLE.
           IF ST-CAD018 = "35"
              CLOSE CAD018      OPEN OUTPUT CAD018
              CLOSE CAD018      OPEN I-O    CAD018
           END-IF.

           IF ST-CAD999 = "35"
              CLOSE CAD999      OPEN OUTPUT CAD999
              CLOSE CAD999      OPEN I-O    CAD999
           END-IF.

           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O    CHD010
           END-IF.

           IF ST-CHD999 = "35"
              CLOSE CHD999      OPEN OUTPUT CHD999
              CLOSE CHD999      OPEN I-O    CHD999
           END-IF.

           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O    CRD020
           END-IF.

           IF ST-CRD999 = "35"
              CLOSE CRD999      OPEN OUTPUT CRD999
              CLOSE CRD999      OPEN I-O    CRD999
           END-IF.


           IF ST-CAD018 <> "00" OR ST-CAD999 <> "00" OR
              ST-CHD010 <> "00" OR ST-CHD999 <> "00" OR
              ST-CRD020 <> "00" OR ST-CRD999
              CLOSE CAD018 CAD999 CHD010 CHD999 CRD020 CRD999
              stop run.


           display "VOU EXECUTAR O GALHO76" STOP " ".


           initialize reg-cad999
           start cad999 key is not less portador-99 invalid key
               move "10" to st-cad999.
           perform until st-cad999 = "10"
               read cad999 next at end
                   move "10" to st-cad999
               not at end
                   move portador-99  to portador
                   move nome-port-99 to nome-port
                   display reg-cad018
                   write reg-cad018 invalid key
                     display "Erro de Gravação...CAD018"
                     stop " "
                   end-write
               end-read
           end-perform

           CLOSE CAD018 CAD999

           initialize reg-chd999
           start chd999 key is not less chave-ch99 invalid key
               move "10" to st-chd999.
           perform until st-chd999 = "10"
               read chd999 next at end
                   move "10" to st-chd999
               not at end
                   INITIALIZE REG-CHD010

                   MOVE DATA-MOVTO-CH99       TO DATA-MOVTO-CH10
                   MOVE SEQ-CH99              TO SEQ-CH10
                   MOVE CLASS-CLIENTE-CH99    TO CLASS-CLIENTE-CH10
                   MOVE CLIENTE-CH99          TO CLIENTE-CH10
                   MOVE NOME-CH99             TO NOME-CH10
                   MOVE CIDADE-CH99           TO CIDADE-CH10
                   MOVE LOTE-CH99             TO LOTE-CH10
                   MOVE CONTA-CORR-CH99       TO CONTA-CORR-CH10
                   MOVE DV-CONTA-CH99         TO DV-CONTA-CH10
                   MOVE BANCO-CH99            TO BANCO-CH10
                   MOVE AGENCIA-CH99          TO AGENCIA-CH10
                   MOVE DV-AGENCIA-CH99       TO DV-AGENCIA-CH10
                   MOVE COMPENSACAO-CH99      TO COMPENSACAO-CH10
                   MOVE CPF-CH99              TO CPF-CH10
                   MOVE NR-CHEQUE-CH99        TO NR-CHEQUE-CH10
                   MOVE OUTRO-DOCTO-CH99      TO OUTRO-DOCTO-CH10
                   MOVE DATA-VENCTO-CH99      TO DATA-VENCTO-CH10
                   MOVE PORTADOR-CH99         TO PORTADOR-CH10
                   MOVE CARTEIRA-CH99         TO CARTEIRA-CH10
                   MOVE SITUACAO-TIT-CH99     TO SITUACAO-TIT-CH10
                   MOVE SITUACAO-CH99         TO SITUACAO-CH10
                   MOVE CODREDUZ-APUR-CH99    TO CODREDUZ-APUR-CH10
                   MOVE VALOR-CH99            TO VALOR-CH10
                   MOVE VENDEDOR-CH99         TO VENDEDOR-CH10
                   MOVE DIGITADOR-CH99        TO DIGITADOR-CH10
                   MOVE SEQ-CAIXA-CH99        TO SEQ-CAIXA-CH10
                   MOVE NR-NOTA-FISCAL-CH99   TO NR-NOTA-FISCAL-CH10
                   MOVE DATA-NTA-FISCAL-CH99  TO DATA-NTA-FISCAL-CH10
                   MOVE ORIGEM-CH99           TO ORIGEM-CH10

                   display reg-chd010
                   write reg-chd010 invalid key
                     display "Erro de Gravação...CHD010"
                     stop " "
                   end-write
               end-read
           end-perform

           CLOSE CHD010 CHD999


           initialize reg-crd999
           start crd999 key is not less chave-cr99 invalid key
               move "10" to st-crd999.
           perform until st-crd999 = "10"
               read crd999 next at end
                   move "10" to st-crd999
               not at end
                   initialize reg-crd020
                   MOVE CLASS-CLIENTE-CR99         TO
                        CLASS-CLIENTE-CR20
                   MOVE CLIENTE-CR99               TO
                        CLIENTE-CR20
                   MOVE SEQ-CR99                   TO
                        SEQ-CR20
                   MOVE PORTADOR-CR99              TO
                        PORTADOR-CR20
                   MOVE CARTEIRA-CR99              TO
                        CARTEIRA-CR20
                   MOVE SITUACAO-TIT-CR99          TO
                        SITUACAO-TIT-CR20
                   MOVE NR-DOCTO-CR99              TO
                        NR-DOCTO-CR20
                   MOVE OUTRO-DOCTO-CR99           TO
                        OUTRO-DOCTO-CR20
                   MOVE TIPO-DOCTO-CR99            TO
                        TIPO-DOCTO-CR20
                   MOVE DATA-EMISSAO-CR99          TO
                        DATA-EMISSAO-CR20
                   MOVE DATA-VENCTO-CR99           TO
                        DATA-VENCTO-CR20
                   MOVE DESCRICAO-CR99             TO
                        DESCRICAO-CR20
                   MOVE SITUACAO-CR99              TO
                        SITUACAO-CR20
                   MOVE TIPO-MOEDA-CR99            TO
                        TIPO-MOEDA-CR20
                   MOVE NR-PARC-CR99               TO
                        NR-PARC-CR20
                   MOVE TOT-PARC-CR99              TO
                        TOT-PARC-CR20
                   MOVE CODREDUZ-APUR-CR99         TO
                        CODREDUZ-APUR-CR20
                   MOVE VALOR-TOT-CR99             TO
                        VALOR-TOT-CR20
                   MOVE JURO-RCTO-CR99             TO
                        JURO-RCTO-CR20
                   MOVE MULTA-RCTO-CR99            TO
                        MULTA-RCTO-CR20
                   MOVE DESCONTO-CR99              TO
                        DESCONTO-CR20
                   MOVE DATA-RCTO-CR99             TO
                        DATA-RCTO-CR20
                   MOVE VALOR-LIQ-CR99             TO
                        VALOR-LIQ-CR20
                   MOVE CONTABILIZADO-CR99         TO
                        CONTABILIZADO-CR20
                   MOVE VENDEDOR-CR99              TO
                        VENDEDOR-CR20
                   MOVE RESPONSAVEL-CR99           TO
                        RESPONSAVEL-CR20
                   MOVE DIGITADOR-CR99             TO
                        DIGITADOR-CR20
                   MOVE SEQ-CAIXA-CR99             TO
                        SEQ-CAIXA-CR20
                   MOVE NR-NOTA-FISCAL-CR99        TO
                        NR-NOTA-FISCAL-CR20
                   MOVE DATA-NTA-FISCAL-CR99       TO
                        DATA-NTA-FISCAL-CR20
                   MOVE FORMA-PAGTO-CR99           TO
                        FORMA-PAGTO-CR20
                   MOVE DCR-MEM-CR99               TO
                        DCR-MEM-CR20
                   MOVE CARTAO-CRED-CR99           TO
                        CARTAO-CRED-CR20
                   MOVE TAXA-ADMINIST-CREDITO-CR99 TO
                        TAXA-ADMINIST-CREDITO-CR20
                   MOVE TAXA-ADMINIST-PARCELA-CR99 TO
                        TAXA-ADMINIST-PARCELA-CR20

                   display reg-crd020
                   write reg-crd020 invalid key
                     display "Erro de Gravação...CRD020"
                     stop " "
                   end-write
               end-read
           end-perform

           CLOSE CRD020 CRD999


           display "ACABEI" STOP " ".
           display "ACABEI" STOP " ".
           EXIT PROGRAM
           STOP RUN.
