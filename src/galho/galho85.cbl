       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO84.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 23-05-2007
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CPPX020.

          SELECT CPD920 ASSIGN TO PATH-CPD920
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CPD920
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CP92 = FORNEC-CP92 SEQ-CP92
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CP92
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CP92 = FORNEC-CP92
                        DATA-VENCTO-CP92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CP92 = SITUACAO-CP92
                        DATA-VENCTO-CP92 FORNEC-CP92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CP92 = SITUACAO-CP92
                        DATA-MOVTO-CP92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-CP92 = SITUACAO-CP92
                        FORNEC-CP92 DATA-VENCTO-CP92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT5-CP92 = SITUACAO-CP92
                       TIPO-FORN-CP92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-CP92 = DATA-PGTO-CP92
                       SEQ-CAIXA-CP92 WITH DUPLICATES.

           COPY CPPX099.

          SELECT CPD999 ASSIGN TO PATH-CPD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CPD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CP999 = FORNEC-CP999 SEQ-CP999
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CP999
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CP999 =
                     FORNEC-CP999 DATA-VENCTO-CP999 WITH DUPLICATES.

           COPY CPPX023.

          SELECT CPD923 ASSIGN TO PATH-CPD923
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CPD923
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CP93 = FORNEC-CP93 SEQ-CP93
                                             ITEM-CP93.

           COPY PFPX010.

           SELECT PFD910 ASSIGN TO PATH-PFD910
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-PFD910
                  RECORD KEY IS CHAVE-PF91 = DATA-PROGR-PF91
                         SEQ-PF91
                  ALTERNATE RECORD KEY IS ALT1-PF91 = FORNECEDOR-PF91
                        DATA-MOV-PAGAR-PF91 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-MOV-PAGAR-PF91
                        WITH DUPLICATES.

           COPY CCPX100.

           SELECT CCD999 ASSIGN TO PATH-CCD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-CCD999
                  RECORD KEY IS CHAVE-CC999 = FORNEC-CC999 SEQ-CC999
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CC999
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CC999 = SITUACAO-CC999
                       FORNEC-CC999 DATA-VENCTO-CC999 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CC999 = DATA-MOVTO-CC999
                       SEQ-CAIXA-CC999 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CC999 = SITUACAO-CC999
                       DATA-VENCTO-CC999 FORNEC-CC999 WITH DUPLICATES.

           COPY CXPX042.

           SELECT CXD942 ASSIGN TO PATH-CXD942
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CXD942
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CX92 = ANOMES-CX92 CONTAREDUZ-CX92
                  ALTERNATE RECORD KEY IS ALT-CX92 =
                            CONTAREDUZ-CX92 ANOMES-CX92 WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CPPW020.

      * Arquivo de Movimento de contas a pagar
       FD  CPD920.
       01  REG-CPD920.
           05  DATA-MOVTO-CP92          PIC 9(8).
      *    DATA-MOVTO-CP20 - DATA-INVERTIDA
           05  FORNEC-CP92              PIC 9(6).
           05  SEQ-CP92                 PIC 9(5).
           05  TIPO-FORN-CP92           PIC 9(2).
           05  PORTADOR-CP92            PIC 9999.
           05  NR-DOCTO-CP92            PIC X(10).
           05  DATA-EMISSAO-CP92        PIC 9(8).
      *    DATA-EMISSAO-CP20 - DATA-INVERTIDA
           05  DATA-VENCTO-CP92         PIC 9(8).
           05  DESCRICAO-CP92           PIC X(30).
           05  PREV-DEF-CP92            PIC 9.
      *    PREV-DEF = 0-DEFIN  1-PREVISTO
           05  SITUACAO-CP92            PIC 9.
      *    SITUACAO = 0-OK  1-SUSPENSA  2-PAGA  3-ESTONADA  4-CANCELADA
           05  LIBERADO-CP92            PIC 9.
      *    LIBERADO = 0-NÃO     1-SIM
           05  TIPO-MOEDA-CP92          PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CP92.
               10  NR-PARC-CP92         PIC 99.
               10  TOT-PARC-CP92        PIC 99.
           05  CODREDUZ-APUR-CP92       PIC 9(3).
           05  JUROS-MORA-CP92          PIC 9(6)V99.
           05  MULTA-ATRASO-CP92        PIC 9(6)V99.
           05  TAXA-APLIC-CP92          PIC 99V99.
           05  VALOR-TOT-CP92           PIC 9(8)V99.
           05  JURO-PAGO-CP92           PIC 9(6)V99.
           05  MULTA-PAGA-CP92          PIC 9(6)V99.
           05  DESCONTO-CP92            PIC 9(6)V99.
           05  DATA-PGTO-CP92           PIC 9(8).
           05  VALOR-LIQ-CP92           PIC 9(8)V99.
           05  CONTABILIZADO-CP92       PIC 9.
           05  RESPONSAVEL-CP92         PIC X(5).
           05  DIGITADOR-CP92           PIC X(5).
           05  SEQ-CAIXA-CP92           PIC 9(3).
           05  TIPO-CONTA-CP92          PIC 9.
      *  TIPO-CONTA = 0(TEMPORARIA)   1-PERMANENTE

           COPY CPPW099.

       FD  CPD999.
       01  REG-CPD999.
           05  DATA-MOVTO-CP999          PIC 9(8).
           05  FORNEC-CP999              PIC 9(6).
           05  SEQ-CP999                 PIC 9(5).
           05  TIPO-FORN-CP999           PIC 9(2).
           05  PORTADOR-CP999            PIC 9999.
           05  NR-DOCTO-CP999            PIC X(10).
           05  DATA-EMISSAO-CP999        PIC 9(8).
           05  DATA-VENCTO-CP999         PIC 9(8).
           05  DESCRICAO-CP999           PIC X(30).
           05  PREV-DEF-CP999            PIC 9.
      *    PREV-DEF = 0-DEFIN  1-PREVISTO
           05  SITUACAO-CP999            PIC 9.
      *    SITUACAO = 0-OK  1-SUSPENSA  2-PAGA  3-ESTONADA  4-CANCELADA
           05  LIBERADO-CP999            PIC 9.
      *    LIBERADO = 0-NÃO     1-SIM
           05  TIPO-MOEDA-CP999          PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CP999.
               10  NR-PARC-CP999         PIC 99.
               10  TOT-PARC-CP999        PIC 99.
           05  CODREDUZ-APUR-CP999       PIC 9(3).
           05  JUROS-MORA-CP999          PIC 9(6)V99.
           05  MULTA-ATRASO-CP999        PIC 9(6)V99.
           05  TAXA-APLIC-CP999          PIC 99V99.
           05  VALOR-TOT-CP999           PIC 9(8)V99.
           05  JURO-PAGO-CP999           PIC 9(6)V99.
           05  MULTA-PAGA-CP999          PIC 9(6)V99.
           05  DESCONTO-CP999            PIC 9(6)V99.
           05  DATA-PGTO-CP999           PIC 9(8).
           05  VALOR-LIQ-CP999           PIC 9(8)V99.
           05  CONTABILIZADO-CP999       PIC 9.
           05  RESPONSAVEL-CP999         PIC X(5).
           05  DIGITADOR-CP999           PIC X(5).
           05  SEQ-CAIXA-CP999           PIC 9(3).
           05  TIPO-CONTA-CP999          PIC 9.
      *  TIPO-CONTA = 0(TEMPORARIA)   1-PERMANENTE

           COPY CPPW023.

       FD  CPD923.
       01  REG-CPD923.
           05  FORNEC-CP93              PIC 9(6).
           05  SEQ-CP93                 PIC 9(5).
           05  ITEM-CP93                PIC 9(2).
           05  CODREDUZ-APUR-CP93       PIC 9(3).
           05  VALOR-CP93               PIC 9(8)V99.

           COPY PFPW010.

      * Arquivo de controle de programações financeiras
       FD  PFD910.
       01  REG-PFD910.
           05  DATA-PROGR-PF91     PIC 9(8).
           05  SEQ-PF91            PIC 9(3).
           05  FORNECEDOR-PF91     PIC 9(6).
           05  DESCRICAO-PF91      PIC X(30).
           05  COND-PGTO-PF91      PIC X(20).
           05  DATA-ENTREGA-PF91   PIC 9(8).
           05  VALOR-PF91          PIC 9(8)V99.
           05  USUARIO-PF91        PIC 9(3).
      *  USUARIO-PF10 - automático do cod-usuario-w.
           05  CONTA-APURAC-PF91   PIC 9(3).
           05  DATA-MOV-PAGAR-PF91 PIC 9(8).
           05  APROVADO-POR-PF91   PIC 9(3).
           05  DATA-APROVACAO-PF91 PIC 9(8).
      *  APROVADO-POR - usuario responsável pela aprovação, será preen-
      *  chido qdo o responsável liberar.
           05  OBS-PF91            PIC X(80).


           COPY CCPW100.

       FD  CCD999.
       01  REG-CCD999.
           05  DATA-MOVTO-CC999         PIC 9(8).
           05  FORNEC-CC999             PIC 9(6).
           05  SEQ-CC999                PIC 9(5).
           05  TIPO-LCTO-CC999          PIC 99.
      *    TIPO-LCTO-CC100 = 00-FORNEC  01-FUNCION  02-VENDEDOR
      *                                 03-REPRES   04-REPORT
           05  TIPO-FORN-CC999          PIC 9(2).
           05  NR-DOCTO-CC999           PIC X(10).
           05  DATA-EMISSAO-CC999       PIC 9(8).
           05  DATA-VENCTO-CC999        PIC 9(8).
           05  DESCRICAO-CC999          PIC X(30).
           05  SITUACAO-CC999           PIC 9.
      *    SITUACAO =   00-OK   01-PAGA  02-ESTORNADA
           05  LIBERADO-CC999           PIC 9.
      *    LIBERADO =   00-NÃO  01-SIM
           05  TIPO-MOEDA-CC999         PIC 9.
      *    TIPO-MOEDA = 00-REAL 01-DOLAR
           05  NR-PARCELA-CC999.
               10  NR-PARC-CC999        PIC 99.
               10  TOT-PARC-CC999       PIC 99.
           05  CODREDUZ-APUR-CC999      PIC 9(3).
           05  VALOR-CC999              PIC 9(8)V99.
           05  CRED-DEB-CC999           PIC 9.
      *    0-DEBITO   1-CREDITO
           05  DATA-PGTO-CC999          PIC 9(8).
           05  JUROS-PAGO-CC999         PIC 9(6)V99.
           05  MULTA-PAGA-CC999         PIC 9(6)V99.
           05  DESCONTO-CC999           PIC 9(6)V99.
           05  VALOR-PAGO-CC999         PIC 9(8)V99.
           05  RESPONSAVEL-CC999        PIC X(5).
           05  DIGITADOR-CC999          PIC X(5).
           05  SEQ-CAIXA-CC999          PIC 9(3).

           COPY CXPW042.

       FD  CXD942.
       01  REG-CXD942.
           05  ANOMES-CX92                PIC 9(06).
           05  CONTAREDUZ-CX92            PIC 9(03).
           05  SALDOE-CX92                PIC 9(08)V99.
           05  SALDOS-CX92                PIC 9(08)V99.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CXD999             PIC XX       VALUE SPACES.
           05  ST-CBD004             PIC XX       VALUE SPACES.
           05  ST-CBD999             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CXD900             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD920             PIC XX       VALUE SPACES.
           05  ST-CRD099             PIC XX       VALUE SPACES.
           05  ST-CRD999             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD920             PIC XX       VALUE SPACES.
           05  ST-CPD099             PIC XX       VALUE SPACES.
           05  ST-CPD999             PIC XX       VALUE SPACES.
           05  ST-CPD023             PIC XX       VALUE SPACES.
           05  ST-CPD923             PIC XX       VALUE SPACES.
           05  ST-PFD010             PIC XX       VALUE SPACES.
           05  ST-PFD910             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD999             PIC XX       VALUE SPACES.
           05  ST-CXD042             PIC XX       VALUE SPACES.
           05  ST-CXD942             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "PARAMETR".


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC

           MOVE "CPD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD020
           MOVE "CPD920"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD920
           MOVE "CPD099"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD099
           MOVE "CPD999"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD999
           MOVE "CPD023"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD023
           MOVE "CPD923"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD923
           MOVE "PFD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PFD010
           MOVE "PFD910"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PFD910
           MOVE "CCD100"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD100
           MOVE "CCD999"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD999
           MOVE "CXD042"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD042
           MOVE "CXD942"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD942


           MOVE ZEROS     TO ERRO-W.
           OPEN OUTPUT CPD020 CPD099
                       CPD023 PFD010 CCD100 CXD042
           CLOSE       CPD020 CPD099
                       CPD023 PFD010 CCD100 CXD042
           OPEN I-O    CPD020 CPD099
                       CPD023 PFD010 CCD100 CXD042
           OPEN INPUT  CPD920 CPD999
                       CPD923 PFD910 CCD999 CXD942

           CLOSE CONTROLE.

           IF ST-CPD023 = "35"
              CLOSE CPD023      OPEN OUTPUT CPD023
              CLOSE CPD023      OPEN I-O    CPD023
           END-IF.
           IF ST-CPD923 = "35"
              CLOSE CPD923      OPEN OUTPUT CPD923
              CLOSE CPD923      OPEN I-O    CPD923
           END-IF.

           IF ST-PFD010 = "35"
              CLOSE PFD010      OPEN OUTPUT PFD010
              CLOSE PFD010      OPEN I-O    PFD010
           END-IF.
           IF ST-PFD910 = "35"
              CLOSE PFD910      OPEN OUTPUT PFD910
              CLOSE PFD910      OPEN I-O    PFD910
           END-IF.

           IF ST-CCD100 = "35"
              CLOSE CCD100      OPEN OUTPUT CCD100
              CLOSE CCD100      OPEN I-O    CCD100
           END-IF.
           IF ST-CCD999 = "35"
              CLOSE CCD999      OPEN OUTPUT CCD999
              CLOSE CCD999      OPEN I-O    CCD999
           END-IF.

           IF ST-CXD042 = "35"
              CLOSE CXD042      OPEN OUTPUT CXD042
              CLOSE CXD042      OPEN I-O    CXD042
           END-IF.
           IF ST-CXD942 = "35"
              CLOSE CXD942      OPEN OUTPUT CXD942
              CLOSE CXD942      OPEN I-O    CXD942
           END-IF.



          DISPLAY "VOU COMECAR A CONVERSAO DOS ARQUIVOS CXD020 CBD004 CX
      -   "D900" STOP " ".


           INITIALIZE REG-CPD920
           START CPD920 KEY IS NOT LESS CHAVE-CP92 INVALID KEY
               MOVE "10" TO ST-CPD920.

           PERFORM UNTIL ST-CPD920 = "10"
               READ CPD920 NEXT AT END
                   MOVE "10" TO ST-CPD920
               NOT AT END
                   MOVE DATA-MOVTO-CP92     TO DATA-MOVTO-CP20
                   MOVE FORNEC-CP92         TO FORNEC-CP20
                   MOVE SEQ-CP92            TO SEQ-CP20
                   MOVE TIPO-FORN-CP92      TO TIPO-FORN-CP20
                   MOVE PORTADOR-CP92       TO PORTADOR-CP20
                   MOVE NR-DOCTO-CP92       TO NR-DOCTO-CP20
                   MOVE DATA-EMISSAO-CP92   TO DATA-EMISSAO-CP20
                   MOVE DATA-VENCTO-CP92    TO DATA-VENCTO-CP20
                   MOVE DESCRICAO-CP92      TO DESCRICAO-CP20
                   MOVE PREV-DEF-CP92       TO PREV-DEF-CP20
                   MOVE SITUACAO-CP92       TO SITUACAO-CP20
                   MOVE LIBERADO-CP92       TO LIBERADO-CP20
                   MOVE TIPO-MOEDA-CP92     TO TIPO-MOEDA-CP20
                   MOVE NR-PARC-CP92        TO NR-PARC-CP20
                   MOVE TOT-PARC-CP92       TO TOT-PARC-CP20
                   MOVE CODREDUZ-APUR-CP92  TO CODREDUZ-APUR-CP20
                   MOVE JUROS-MORA-CP92     TO JUROS-MORA-CP20
                   MOVE MULTA-ATRASO-CP92   TO MULTA-ATRASO-CP20
                   MOVE TAXA-APLIC-CP92     TO TAXA-APLIC-CP20
                   MOVE VALOR-TOT-CP92      TO VALOR-TOT-CP20
                   MOVE JURO-PAGO-CP92      TO JURO-PAGO-CP20
                   MOVE MULTA-PAGA-CP92     TO MULTA-PAGA-CP20
                   MOVE DESCONTO-CP92       TO DESCONTO-CP20
                   MOVE DATA-PGTO-CP92      TO DATA-PGTO-CP20
                   MOVE VALOR-LIQ-CP92      TO VALOR-LIQ-CP20
                   MOVE CONTABILIZADO-CP92  TO CONTABILIZADO-CP20
                   MOVE RESPONSAVEL-CP92    TO RESPONSAVEL-CP20
                   MOVE DIGITADOR-CP92      TO DIGITADOR-CP20
                   MOVE SEQ-CAIXA-CP92      TO SEQ-CAIXA-CP20
                   MOVE TIPO-CONTA-CP92     TO TIPO-CONTA-CP20

                   display "CPD020 => " reg-cpd020
                   WRITE REG-CPD020 INVALID KEY
                      DISPLAY "Erro de Gravacao...CPD020" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CPD999
           START CPD999 KEY IS NOT LESS CHAVE-CP999 INVALID KEY
               MOVE "10" TO ST-CPD999.

           PERFORM UNTIL ST-CPD999 = "10"
               READ CPD999 NEXT AT END
                   MOVE "10" TO ST-CPD999
               NOT AT END
                   MOVE DATA-MOVTO-CP999    TO DATA-MOVTO-CP99
                   MOVE FORNEC-CP999        TO FORNEC-CP99
                   MOVE SEQ-CP999           TO SEQ-CP99
                   MOVE TIPO-FORN-CP999     TO TIPO-FORN-CP99
                   MOVE PORTADOR-CP999      TO PORTADOR-CP99
                   MOVE NR-DOCTO-CP999      TO NR-DOCTO-CP99
                   MOVE DATA-EMISSAO-CP999  TO DATA-EMISSAO-CP99
                   MOVE DATA-VENCTO-CP999   TO DATA-VENCTO-CP99
                   MOVE DESCRICAO-CP999     TO DESCRICAO-CP99
                   MOVE PREV-DEF-CP999      TO PREV-DEF-CP99
                   MOVE SITUACAO-CP999      TO SITUACAO-CP99
                   MOVE LIBERADO-CP999      TO LIBERADO-CP99
                   MOVE TIPO-MOEDA-CP999    TO TIPO-MOEDA-CP99
                   MOVE NR-PARC-CP999       TO NR-PARC-CP99
                   MOVE TOT-PARC-CP999      TO TOT-PARC-CP99
                   MOVE CODREDUZ-APUR-CP999 TO CODREDUZ-APUR-CP99
                   MOVE JUROS-MORA-CP999    TO JUROS-MORA-CP99
                   MOVE MULTA-ATRASO-CP999  TO MULTA-ATRASO-CP99
                   MOVE TAXA-APLIC-CP999    TO TAXA-APLIC-CP99
                   MOVE VALOR-TOT-CP999     TO VALOR-TOT-CP99
                   MOVE JURO-PAGO-CP999     TO JURO-PAGO-CP99
                   MOVE MULTA-PAGA-CP999    TO MULTA-PAGA-CP99
                   MOVE DESCONTO-CP999      TO DESCONTO-CP99
                   MOVE DATA-PGTO-CP999     TO DATA-PGTO-CP99
                   MOVE VALOR-LIQ-CP999     TO VALOR-LIQ-CP99
                   MOVE CONTABILIZADO-CP999 TO CONTABILIZADO-CP99
                   MOVE RESPONSAVEL-CP999   TO RESPONSAVEL-CP99
                   MOVE DIGITADOR-CP999     TO DIGITADOR-CP99
                   MOVE SEQ-CAIXA-CP999     TO SEQ-CAIXA-CP99
                   MOVE TIPO-CONTA-CP999    TO TIPO-CONTA-CP99

                   display "CPD099 => " reg-cpd099
                   WRITE REG-CPD099 INVALID KEY
                      DISPLAY "Erro de Gravacao...CPD099" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CPD923
           START CPD923 KEY IS NOT LESS CHAVE-CP93 INVALID KEY
               MOVE "10" TO ST-CPD923.

           PERFORM UNTIL ST-CPD923 = "10"
               READ CPD923 NEXT AT END
                   MOVE "10" TO ST-CPD923
               NOT AT END
                   MOVE FORNEC-CP93          TO FORNEC-CP23
                   MOVE SEQ-CP93             TO SEQ-CP23
                   MOVE ITEM-CP93            TO ITEM-CP23
                   MOVE CODREDUZ-APUR-CP93   TO CODREDUZ-APUR-CP23
                   MOVE VALOR-CP93           TO VALOR-CP23

                   display "CPD023 => " reg-cpd023
                   WRITE REG-CPD023 INVALID KEY
                      DISPLAY "Erro de Gravacao...CPD023" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-PFD910
           START PFD910 KEY IS NOT LESS CHAVE-PF91 INVALID KEY
               MOVE "10" TO ST-PFD910.

           PERFORM UNTIL ST-PFD910 = "10"
               READ PFD910 NEXT AT END
                   MOVE "10" TO ST-PFD910
               NOT AT END
                   MOVE DATA-PROGR-PF91     TO DATA-PROGR-PF10
                   MOVE SEQ-PF91            TO SEQ-PF10
                   MOVE FORNECEDOR-PF91     TO FORNECEDOR-PF10
                   MOVE DESCRICAO-PF91      TO DESCRICAO-PF10
                   MOVE COND-PGTO-PF91      TO COND-PGTO-PF10
                   MOVE DATA-ENTREGA-PF91   TO DATA-ENTREGA-PF10
                   MOVE VALOR-PF91          TO VALOR-PF10
                   MOVE USUARIO-PF91        TO USUARIO-PF10
                   MOVE CONTA-APURAC-PF91   TO CONTA-APURAC-PF10
                   MOVE DATA-MOV-PAGAR-PF91 TO DATA-MOV-PAGAR-PF10
                   MOVE APROVADO-POR-PF91   TO APROVADO-POR-PF10
                   MOVE DATA-APROVACAO-PF91 TO DATA-APROVACAO-PF10
                   MOVE OBS-PF91            TO OBS-PF10

                   display "PFD010 => " reg-pfd010
                   WRITE REG-PFD010 INVALID KEY
                      DISPLAY "Erro de Gravacao...PFD010" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CCD999
           START CCD999 KEY IS NOT LESS CHAVE-CC999 INVALID KEY
               MOVE "10" TO ST-CCD999.

           PERFORM UNTIL ST-CCD999 = "10"
               READ CCD999 NEXT AT END
                   MOVE "10" TO ST-CCD999
               NOT AT END
                   MOVE DATA-MOVTO-CC999     TO DATA-MOVTO-CC100
                   MOVE FORNEC-CC999         TO FORNEC-CC100
                   MOVE SEQ-CC999            TO SEQ-CC100
                   MOVE TIPO-LCTO-CC999      TO TIPO-LCTO-CC100
                   MOVE TIPO-FORN-CC999      TO TIPO-FORN-CC100
                   MOVE NR-DOCTO-CC999       TO NR-DOCTO-CC100
                   MOVE DATA-EMISSAO-CC999   TO DATA-EMISSAO-CC100
                   MOVE DATA-VENCTO-CC999    TO DATA-VENCTO-CC100
                   MOVE DESCRICAO-CC999      TO DESCRICAO-CC100
                   MOVE SITUACAO-CC999       TO SITUACAO-CC100
                   MOVE LIBERADO-CC999       TO LIBERADO-CC100
                   MOVE TIPO-MOEDA-CC999     TO TIPO-MOEDA-CC100
                   MOVE NR-PARC-CC999        TO NR-PARC-CC100
                   MOVE TOT-PARC-CC999       TO TOT-PARC-CC100
                   MOVE CODREDUZ-APUR-CC999  TO CODREDUZ-APUR-CC100
                   MOVE VALOR-CC999          TO VALOR-CC100
                   MOVE CRED-DEB-CC999       TO CRED-DEB-CC100
                   MOVE DATA-PGTO-CC999      TO DATA-PGTO-CC100
                   MOVE JUROS-PAGO-CC999     TO JUROS-PAGO-CC100
                   MOVE MULTA-PAGA-CC999     TO MULTA-PAGA-CC100
                   MOVE DESCONTO-CC999       TO DESCONTO-CC100
                   MOVE VALOR-PAGO-CC999     TO VALOR-PAGO-CC100
                   MOVE RESPONSAVEL-CC999    TO RESPONSAVEL-CC100
                   MOVE DIGITADOR-CC999      TO DIGITADOR-CC100
                   MOVE SEQ-CAIXA-CC999      TO SEQ-CAIXA-CC100


                   display "CCD100 => " reg-ccd100
                   WRITE REG-CCD100 INVALID KEY
                      DISPLAY "Erro de Gravacao...CCD100" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CXD942
           START CXD942 KEY IS NOT LESS CHAVE-CX92 INVALID KEY
               MOVE "10" TO ST-CXD942.

           PERFORM UNTIL ST-CXD942 = "10"
               READ CXD942 NEXT AT END
                   MOVE "10" TO ST-CXD942
               NOT AT END
                   MOVE ANOMES-CX92       TO ANOMES-CX42
                   MOVE CONTAREDUZ-CX92   TO CONTAREDUZ-CX42
                   MOVE SALDOE-CX92       TO SALDOE-CX42
                   MOVE SALDOS-CX92       TO SALDOS-CX42

                   display "CXD042 => " reg-cxd042
                   WRITE REG-CXD042 INVALID KEY
                      DISPLAY "Erro de Gravacao...CXD042" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           CLOSE CPD020 CPD920 CPD099 CPD999 CPD023 CPD923
                 PFD010 PFD910 CCD100 CCD999 CXD042 CXD942
           STOP RUN.
