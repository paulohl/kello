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

           COPY CHPX099.

           COPY CRPX099.

           SELECT CHD999 ASSIGN TO PATH-CHD9999
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH999 = DATA-MOVTO-CH999 SEQ-CH999
                  ALTERNATE RECORD KEY ALT1-CH999 =
                     NOME-CH999, PORTADOR-CH999 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT2-CH999 = SITUACAO-CH999
                     DATA-VENCTO-CH999 PORTADOR-CH999 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT3-CH999 = DATA-MOVTO-CH999
                     VENDEDOR-CH999 SEQ-CH999 WITH DUPLICATES
                  STATUS IS ST-CHD999.

          SELECT CRD999 ASSIGN TO PATH-CRD9999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR999 = CLASS-CLIENTE-CR999
                        CLIENTE-CR999 SEQ-CR999
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR999
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CR999 =
                     CLASS-CLIENTE-CR999 CLIENTE-CR999
                     DATA-VENCTO-CR999 WITH DUPLICATES.


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CHPW099.

           COPY CRPW099.

      * ARQUIVO DO MOVIMENTO DE CHEQUES - CHEQUES EXCLUÍDOS
       FD  CHD999.
       01  REG-CHD999.
           05  DATA-MOVTO-CH999          PIC 9(8).
           05  SEQ-CH999                 PIC 9(4).
           05  COD-COMPL-CH999.
               10  CLASS-CLIENTE-CH999   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH999         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH999                PIC X(30).
           05  CIDADE-CH999              PIC X(20).
           05  CONTA-CORR-CH999          PIC 9(8)   COMP-3.
           05  DV-CONTA-CH999            PIC X.
           05  BANCO-CH999               PIC 9(4)   COMP-3.
           05  AGENCIA-CH999             PIC 9(5)   COMP-3.
           05  DV-AGENCIA-CH999          PIC X.
           05  COMPENSACAO-CH999         PIC 9(3).
           05  CPF-CH999                 PIC 9(11)  COMP-3.
           05  NR-CHEQUE-CH999           PIC X(7).
           05  OUTRO-DOCTO-CH999         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-CH999         PIC 9(8).
           05  PORTADOR-CH999            PIC 99.
           05  CARTEIRA-CH999            PIC 9.
      *    CARTEIRA-CH99  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH999        PIC 99.
           05  SITUACAO-CH999            PIC 9.
      *    SITUACAO = 0-OK  2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH999       PIC 9(3).
           05  VALOR-CH999               PIC 9(8)V99.
           05  VENDEDOR-CH999            PIC 9(6)   COMP-3.
           05  DIGITADOR-CH999           PIC X(5).
           05  SEQ-CAIXA-CH999           PIC 9(3).
           05  NR-NOTA-FISCAL-CH999      PIC X(10).
           05  DATA-NTA-FISCAL-CH999     PIC 9(8)   COMP-3.
           05  ORIGEM-CH999              PIC 99.
           05  USUARIO-EXCLUSAO-CH999    PIC X(5).
           05  DATA-EXCLUSAO-CH999       PIC 9(08).
           05  HORA-EXCLUSAO-CH999       PIC 9(06).
           05  FILLER                    PIC X(30).

      * registro excluido do contas a receber
       FD  CRD999.
       01  REG-CRD999.
           05  DATA-MOVTO-CR999          PIC 9(8).
           05  CLASS-CLIENTE-CR999       PIC 9.
      *    classificação cliente =  0-contrato  1-comum
           05  CLIENTE-CR999             PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR999                 PIC 9(5).
           05  PORTADOR-CR999            PIC 99.
           05  CARTEIRA-CR999            PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR999        PIC 99.
           05  NR-DOCTO-CR999            PIC X(10).
           05  OUTRO-DOCTO-CR999         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR999          PIC 9.
      *    TIPO-DOCTO = 0-DUPLICATA  1-NTA-PROMISSÓRIA  2-ORG.EVENTO
           05  DATA-EMISSAO-CR999        PIC 9(8).
           05  DATA-VENCTO-CR999         PIC 9(8).
           05  DESCRICAO-CR999           PIC X(30).
           05  SITUACAO-CR999            PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA  4-CANCELADA
           05  TIPO-MOEDA-CR999          PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR999.
               10  NR-PARC-CR999                 PIC 99.
               10  TOT-PARC-CR999                PIC 99.
           05  CODREDUZ-APUR-CR999               PIC 9(3).
           05  VALOR-TOT-CR999                   PIC 9(8)V99.
           05  JURO-RCTO-CR999                   PIC 9(6)V99.
           05  MULTA-RCTO-CR999                  PIC 9(6)V99.
           05  DESCONTO-CR999                    PIC 9(6)V99.
           05  DATA-RCTO-CR999                   PIC 9(8).
           05  VALOR-LIQ-CR999                   PIC 9(8)V99.
           05  CONTABILIZADO-CR999               PIC 9.
           05  RESPONSAVEL-CR999                 PIC X(5).
           05  DIGITADOR-CR999                   PIC X(5).
           05  SEQ-CAIXA-CR999                   PIC 9(3).
           05  NR-NOTA-FISCAL-CR999              PIC X(10).
           05  DATA-NTA-FISCAL-CR999             PIC 9(8).
           05  FORMA-PAGTO-CR999                 PIC X(10).
           05  DCR-MEM-CR999                     PIC X(15).
           05  CARTAO-CRED-CR999                 PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR999       PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR999       PIC 9(03)V99.
           05  USUARIO-EXCLUSAO-CR999            PIC 9(05).
           05  DATA-EXCLUSAO-CR999               PIC 9(08).
           05  HORA-EXCLUSAO-CR999               PIC 9(04).
           05  FILLER                            PIC X(01).


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD099             PIC XX       VALUE SPACES.
           05  ST-CRD099             PIC XX       VALUE SPACES.
           05  ST-CHD999             PIC XX       VALUE SPACES.
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
           MOVE "CRD099"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD099
           MOVE "CRD9999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD9999
           MOVE "CHD099"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD099
           MOVE "CHD9999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD9999
           OPEN I-O   CRD099 CRD999 CHD099 CHD999
           CLOSE      CRD099 CRD999 CHD099 CHD999
           OPEN I-O   CRD099 CRD999 CHD099 CHD999

           CLOSE CONTROLE.
           IF ST-CRD099 = "35"
              CLOSE CRD099      OPEN OUTPUT CRD099
              CLOSE CRD099      OPEN I-O    CRD099
           END-IF.

           IF ST-CRD999 = "35"
              CLOSE CRD999      OPEN OUTPUT CRD999
              CLOSE CRD999      OPEN I-O    CRD999
           END-IF.

           IF ST-CHD099 = "35"
              CLOSE CHD099      OPEN OUTPUT CHD099
              CLOSE CHD099      OPEN I-O    CHD099
           END-IF.

           IF ST-CHD999 = "35"
              CLOSE CHD999      OPEN OUTPUT CHD999
              CLOSE CHD999      OPEN I-O    CHD999
           END-IF.

           IF ST-CHD099 <> "00" OR ST-CHD999 <> "00" OR
              ST-CRD099 <> "00" OR ST-CRD999
              CLOSE CHD099 CHD999 CRD099 CRD999
              stop run.


           display "VOU EXECUTAR O GALHO77" STOP " ".



           initialize reg-chd999
           start chd999 key is not less chave-ch999 invalid key
               move "10" to st-chd999.
           perform until st-chd999 = "10"
               read chd999 next at end
                   move "10" to st-chd999
               not at end
                   INITIALIZE REG-CHD099

                   MOVE DATA-MOVTO-CH999       TO DATA-MOVTO-CH99
                   MOVE SEQ-CH999              TO SEQ-CH99
                   MOVE CLASS-CLIENTE-CH999    TO CLASS-CLIENTE-CH99
                   MOVE CLIENTE-CH999          TO CLIENTE-CH99
                   MOVE NOME-CH999             TO NOME-CH99
                   MOVE CIDADE-CH999           TO CIDADE-CH99
                   MOVE CONTA-CORR-CH999       TO CONTA-CORR-CH99
                   MOVE DV-CONTA-CH999         TO DV-CONTA-CH99
                   MOVE BANCO-CH999            TO BANCO-CH99
                   MOVE AGENCIA-CH999          TO AGENCIA-CH99
                   MOVE DV-AGENCIA-CH999       TO DV-AGENCIA-CH99
                   MOVE COMPENSACAO-CH999      TO COMPENSACAO-CH99
                   MOVE CPF-CH999              TO CPF-CH99
                   MOVE NR-CHEQUE-CH999        TO NR-CHEQUE-CH99
                   MOVE OUTRO-DOCTO-CH999      TO OUTRO-DOCTO-CH99
                   MOVE DATA-VENCTO-CH999      TO DATA-VENCTO-CH99
                   MOVE PORTADOR-CH999         TO PORTADOR-CH99
                   MOVE CARTEIRA-CH999         TO CARTEIRA-CH99
                   MOVE SITUACAO-TIT-CH999     TO SITUACAO-TIT-CH99
                   MOVE SITUACAO-CH999         TO SITUACAO-CH99
                   MOVE CODREDUZ-APUR-CH999    TO CODREDUZ-APUR-CH99
                   MOVE VALOR-CH999            TO VALOR-CH99
                   MOVE VENDEDOR-CH999         TO VENDEDOR-CH99
                   MOVE DIGITADOR-CH999        TO DIGITADOR-CH99
                   MOVE SEQ-CAIXA-CH999        TO SEQ-CAIXA-CH99
                   MOVE NR-NOTA-FISCAL-CH999   TO NR-NOTA-FISCAL-CH99
                   MOVE DATA-NTA-FISCAL-CH999  TO DATA-NTA-FISCAL-CH99
                   MOVE ORIGEM-CH999           TO ORIGEM-CH99
                   MOVE USUARIO-EXCLUSAO-CH999 TO USUARIO-EXCLUSAO-CH99
                   MOVE DATA-EXCLUSAO-CH999    TO DATA-EXCLUSAO-CH99
                   MOVE HORA-EXCLUSAO-CH999    TO HORA-EXCLUSAO-CH99


                   display reg-chd099
                   write reg-chd099 invalid key
                     display "Erro de Gravação...CHD099"
                     stop " "
                   end-write
               end-read
           end-perform

           CLOSE CHD099 CHD999


           initialize reg-crd999
           start crd999 key is not less chave-cr999 invalid key
               move "10" to st-crd999.
           perform until st-crd999 = "10"
               read crd999 next at end
                   move "10" to st-crd999
               not at end
                   initialize reg-crd099

                   MOVE DATA-MOVTO-CR999               TO
                        DATA-MOVTO-CR99
                   MOVE CLASS-CLIENTE-CR999            TO
                        CLASS-CLIENTE-CR99
                   MOVE CLIENTE-CR999                  TO
                        CLIENTE-CR99
                   MOVE SEQ-CR999                      TO
                        SEQ-CR99
                   MOVE PORTADOR-CR999                 TO
                        PORTADOR-CR99
                   MOVE CARTEIRA-CR999                 TO
                        CARTEIRA-CR99
                   MOVE SITUACAO-TIT-CR999             TO
                        SITUACAO-TIT-CR99
                   MOVE NR-DOCTO-CR999                 TO
                        NR-DOCTO-CR99
                   MOVE OUTRO-DOCTO-CR999              TO
                        OUTRO-DOCTO-CR99
                   MOVE TIPO-DOCTO-CR999               TO
                        TIPO-DOCTO-CR99
                   MOVE DATA-EMISSAO-CR999             TO
                        DATA-EMISSAO-CR99
                   MOVE DATA-VENCTO-CR999              TO
                        DATA-VENCTO-CR99
                   MOVE DESCRICAO-CR999                TO
                        DESCRICAO-CR99
                   MOVE SITUACAO-CR999                 TO
                        SITUACAO-CR99
                   MOVE TIPO-MOEDA-CR999               TO
                        TIPO-MOEDA-CR99
                   MOVE NR-PARC-CR999                  TO
                        NR-PARC-CR99
                   MOVE TOT-PARC-CR999                 TO
                        TOT-PARC-CR99
                   MOVE CODREDUZ-APUR-CR999            TO
                        CODREDUZ-APUR-CR99
                   MOVE VALOR-TOT-CR999                TO
                        VALOR-TOT-CR99
                   MOVE JURO-RCTO-CR999                TO
                        JURO-RCTO-CR99
                   MOVE MULTA-RCTO-CR999               TO
                        MULTA-RCTO-CR99
                   MOVE DESCONTO-CR999                 TO
                        DESCONTO-CR99
                   MOVE DATA-RCTO-CR999                TO
                        DATA-RCTO-CR99
                   MOVE VALOR-LIQ-CR999                TO
                        VALOR-LIQ-CR99
                   MOVE CONTABILIZADO-CR999            TO
                        CONTABILIZADO-CR99
                   MOVE RESPONSAVEL-CR999              TO
                        RESPONSAVEL-CR99
                   MOVE DIGITADOR-CR999                TO
                        DIGITADOR-CR99
                   MOVE SEQ-CAIXA-CR999                TO
                        SEQ-CAIXA-CR99
                   MOVE NR-NOTA-FISCAL-CR999           TO
                        NR-NOTA-FISCAL-CR99
                   MOVE DATA-NTA-FISCAL-CR999          TO
                        DATA-NTA-FISCAL-CR99
                   MOVE FORMA-PAGTO-CR999              TO
                        FORMA-PAGTO-CR99
                   MOVE DCR-MEM-CR999                  TO
                        DCR-MEM-CR99
                   MOVE CARTAO-CRED-CR999              TO
                        CARTAO-CRED-CR99
                   MOVE TAXA-ADMINIST-CREDITO-CR999    TO
                        TAXA-ADMINIST-CREDITO-CR99
                   MOVE TAXA-ADMINIST-PARCELA-CR999    TO
                        TAXA-ADMINIST-PARCELA-CR99
                   MOVE USUARIO-EXCLUSAO-CR999         TO
                        USUARIO-EXCLUSAO-CR99
                   MOVE DATA-EXCLUSAO-CR999            TO
                        DATA-EXCLUSAO-CR99
                   MOVE HORA-EXCLUSAO-CR999            TO
                        HORA-EXCLUSAO-CR99


                   display reg-crd099
                   write reg-crd099 invalid key
                     display "Erro de Gravação...CRD099"
                     stop " "
                   end-write
               end-read
           end-perform

           CLOSE CRD099 CRD999


           display "ACABEI" STOP " ".
           display "ACABEI" STOP " ".
           EXIT PROGRAM
           STOP RUN.
