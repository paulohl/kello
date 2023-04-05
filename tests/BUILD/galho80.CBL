       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO80.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 21-03-2007.
      *FUNÇÃO: gera um novo arquivo de contas a pagar

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CPPX020.

          SELECT CPD999 ASSIGN TO PATH-CPD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CPD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CP999 = FORNEC-CP999 SEQ-CP999
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CP999
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CP999 = FORNEC-CP999
                        DATA-VENCTO-CP999 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CP999 = SITUACAO-CP999
                        DATA-VENCTO-CP999 FORNEC-CP999 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CP999 = SITUACAO-CP999
                        DATA-MOVTO-CP999 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-CP999 = SITUACAO-CP999
                        FORNEC-CP999 DATA-VENCTO-CP999 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT5-CP999 = SITUACAO-CP999
                       TIPO-FORN-CP999 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-CP999 = DATA-PGTO-CP999
                       SEQ-CAIXA-CP999 WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CPPW020.

      * Arquivo de Movimento de contas a pagar
       FD  CPD999.
       01  REG-CPD999.
           05  DATA-MOVTO-CP999         PIC 9(8).
      *    DATA-MOVTO-CP20 - DATA-INVERTIDA
           05  FORNEC-CP999             PIC 9(6).
           05  SEQ-CP999                PIC 9(5).
           05  TIPO-FORN-CP999          PIC 9(2).
           05  PORTADOR-CP999           PIC 99.
           05  NR-DOCTO-CP999           PIC X(10).
           05  DATA-EMISSAO-CP999       PIC 9(8).
      *    DATA-EMISSAO-CP20 - DATA-INVERTIDA
           05  DATA-VENCTO-CP999        PIC 9(8).
           05  DESCRICAO-CP999          PIC X(30).
           05  PREV-DEF-CP999           PIC 9.
      *    PREV-DEF = 0-DEFIN  1-PREVISTO
           05  SITUACAO-CP999           PIC 9.
      *    SITUACAO = 0-OK  1-SUSPENSA  2-PAGA  3-ESTONADA  4-CANCELADA
           05  LIBERADO-CP999           PIC 9.
      *    LIBERADO = 0-NÃO     1-SIM
           05  TIPO-MOEDA-CP999         PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CP999.
               10  NR-PARC-CP999        PIC 99.
               10  TOT-PARC-CP999       PIC 99.
           05  CODREDUZ-APUR-CP999      PIC 9(3).
           05  JUROS-MORA-CP999         PIC 9(6)V99.
           05  MULTA-ATRASO-CP999       PIC 9(6)V99.
           05  TAXA-APLIC-CP999         PIC 99V99.
           05  VALOR-TOT-CP999          PIC 9(8)V99.
           05  JURO-PAGO-CP999          PIC 9(6)V99.
           05  MULTA-PAGA-CP999         PIC 9(6)V99.
           05  DESCONTO-CP999           PIC 9(6)V99.
           05  DATA-PGTO-CP999          PIC 9(8).
           05  VALOR-LIQ-CP999          PIC 9(8)V99.
           05  CONTABILIZADO-CP999      PIC 9.
           05  RESPONSAVEL-CP999        PIC X(5).
           05  DIGITADOR-CP999          PIC X(5).
           05  SEQ-CAIXA-CP999          PIC 9(3).
           05  TIPO-CONTA-CP999         PIC 9.
      *  TIPO-CONTA = 0(TEMPORARIA)   1-PERMANENTE


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD999             PIC XX       VALUE SPACES.
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
           05  nao-gravado           pic 9(06)    VALUE ZEROS.
           05  erro                  pic 9(06)    VALUE ZEROS.
           05  sucesso               pic 9(06)    VALUE ZEROS.
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
           MOVE "CPD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD020
           MOVE "CPD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD999
           OPEN I-O   CPD020 CPD999
           CLOSE      CPD020 CPD999
           OPEN I-O   CPD020 CPD999

           CLOSE CONTROLE.
           IF ST-CPD020 = "35"
              CLOSE CPD020      OPEN OUTPUT CPD020
              CLOSE CPD020      OPEN I-O    CPD020
           END-IF.

           IF ST-CPD999 = "35"
              CLOSE CPD999      OPEN OUTPUT CPD999
              CLOSE CPD999      OPEN I-O    CPD999
           END-IF.


           IF ST-CPD020 <> "00" OR ST-CPD999
              CLOSE CPD020 CPD999
              stop run.


           display "VOU EXECUTAR O GALHO80" STOP " ".

           initialize reg-cpd999 nao-gravado erro sucesso
           start cpd999 key is not less chave-cp999 invalid key
               move "10" to st-cpd999.
           perform until st-cpd999 = "10"
               read cpd999 next at end
                   move "10" to st-cpd999
               not at end
                   display reg-cpd999
                   initialize reg-cpd020
                   MOVE FORNEC-CP999               TO
                        FORNEC-CP20
                   MOVE SEQ-CP999                  TO
                        SEQ-CP20
                   READ CPD020 INVALID KEY
                        MOVE DATA-MOVTO-CP999      TO
                             DATA-MOVTO-CP20
                        MOVE FORNEC-CP999          TO
                             FORNEC-CP20
                        MOVE SEQ-CP999             TO
                             SEQ-CP20
                        MOVE TIPO-FORN-CP999       TO
                             TIPO-FORN-CP20
                        MOVE PORTADOR-CP999        TO
                             PORTADOR-CP20
                        MOVE NR-DOCTO-CP999        TO
                             NR-DOCTO-CP20
                        MOVE DATA-EMISSAO-CP999    TO
                             DATA-EMISSAO-CP20
                        MOVE DATA-VENCTO-CP999     TO
                             DATA-VENCTO-CP20
                        MOVE DESCRICAO-CP999       TO
                             DESCRICAO-CP20
                        MOVE PREV-DEF-CP999        TO
                             PREV-DEF-CP20
                        MOVE SITUACAO-CP999        TO
                             SITUACAO-CP20
                        MOVE LIBERADO-CP999        TO
                             LIBERADO-CP20
                        MOVE TIPO-MOEDA-CP999      TO
                             TIPO-MOEDA-CP20
                        MOVE NR-PARC-CP999         TO
                             NR-PARC-CP20
                        MOVE TOT-PARC-CP999        TO
                             TOT-PARC-CP20
                        MOVE CODREDUZ-APUR-CP999   TO
                             CODREDUZ-APUR-CP20
                        MOVE JUROS-MORA-CP999      TO
                             JUROS-MORA-CP20
                        MOVE MULTA-ATRASO-CP999    TO
                             MULTA-ATRASO-CP20
                        MOVE TAXA-APLIC-CP999      TO
                             TAXA-APLIC-CP20
                        MOVE VALOR-TOT-CP999       TO
                             VALOR-TOT-CP20
                        MOVE JURO-PAGO-CP999       TO
                             JURO-PAGO-CP20
                        MOVE MULTA-PAGA-CP999      TO
                             MULTA-PAGA-CP20
                        MOVE DESCONTO-CP999        TO
                             DESCONTO-CP20
                        MOVE DATA-PGTO-CP999       TO
                             DATA-PGTO-CP20
                        MOVE VALOR-LIQ-CP999       TO
                             VALOR-LIQ-CP20
                        MOVE CONTABILIZADO-CP999   TO
                             CONTABILIZADO-CP20
                        MOVE RESPONSAVEL-CP999     TO
                             RESPONSAVEL-CP20
                        MOVE DIGITADOR-CP999       TO
                             DIGITADOR-CP20
                        MOVE SEQ-CAIXA-CP999       TO
                             SEQ-CAIXA-CP20
                        MOVE TIPO-CONTA-CP999      TO
                             TIPO-CONTA-CP20
      *                 display reg-cpd020
                        write reg-cpd020 invalid key
                          add 1 to erro
                          display "Erro de Gravação.......CPD020"
                          stop " "
                        not invalid key
                          add 1 to sucesso
      *                   display "Gravação com Sucesso...CRD020"
      *                   stop " "
                        end-write

                   NOT INVALID KEY

                       IF DATA-MOVTO-CP20 = ZEROS
                          MOVE DATA-MOVTO-CP999 TO
                               DATA-MOVTO-CP20
      *                   display reg-cpd020
                          REWRITE REG-CPD020 INVALID KEY
                              add 1 to erro
                              display "Erro de Regravação.......CPD020"
                              stop " "
                          END-REWRITE
                       END-IF
                   END-READ
               end-read
           end-perform

           CLOSE CPD020 CPD999

           display all spaces


           display "ACABEI" STOP " ".
           display "ACABEI" STOP " ".
           EXIT PROGRAM
           STOP RUN.
