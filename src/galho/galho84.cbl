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

           COPY CXPX020.

           SELECT CXD999 ASSIGN TO PATH-CXD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-CXD999
                  RECORD KEY IS CODIGO-REDUZ-CX99
                  ALTERNATE RECORD KEY IS CODIGO-COMPL-CX99
                  ALTERNATE RECORD KEY IS DESCRICAO-CX99
                            WITH DUPLICATES.

           COPY CBPX004.

           SELECT CBD999 ASSIGN TO PATH-CBD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-CBD999
                  RECORD KEY IS CODIGO-CB99
                  ALTERNATE RECORD KEY IS HISTORICO-CB99.

           COPY CXPX100.

           SELECT CXD900 ASSIGN TO PATH-CXD900
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CXD900
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CX900 = DATA-MOV-CX900, SEQ-CX900
                  ALTERNATE RECORD KEY IS ALT-CX900 = CONTAPART-CX900,
                            DATA-MOV-CX900 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-CX920 = CONTA-REDUZ-CX900,
                            DATA-MOV-CX900 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-CX930 = DATA-MOV-CX900
                            SEQ-DESM-CX900 WITH DUPLICATES.

           COPY CRPX020.

          SELECT CRD920 ASSIGN TO PATH-CRD920
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD920
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR92 = COD-COMPL-CR92 SEQ-CR92
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR92
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CR92 = COD-COMPL-CR92
                        DATA-VENCTO-CR92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR92 = SITUACAO-CR92
                        DATA-VENCTO-CR92 COD-COMPL-CR92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR92 = SITUACAO-CR92
                        DATA-MOVTO-CR92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-CR92 = SITUACAO-CR92
                        CLIENTE-CR92 DATA-VENCTO-CR92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-CR92 = DATA-RCTO-CR92
                       SEQ-CAIXA-CR92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT7-CR92 = NR-DOCTO-CR92
                       DATA-RCTO-CR92
                       SEQ-CAIXA-CR92 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT8-CR92 = OUTRO-DOCTO-CR92
                       DATA-RCTO-CR92
                       SEQ-CAIXA-CR92 WITH DUPLICATES.

           COPY CRPX099.

          SELECT CRD999 ASSIGN TO PATH-CRD999
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

           COPY CXPW020.


       FD  CXD999.
       01  REG-CXD999.
           05  CODIGO-COMPL-CX99   PIC 9(07).
           05  CODIGO-REDUZ-CX99   PIC 9(3).
           05  GRAU-CX99           PIC 9.
           05  DESCRICAO-CX99      PIC X(30).
           05  TIPO-CONTA-CX99     PIC 9.
      *  TIPO-CONTA-CX99 = 1 ==> CONTA TOTALIZADORA
      *  TIPO-CONTA-CX99 = 0 ==> CONTA NORMAL

           COPY CBPW004.

       FD  CBD999.
       01  REG-CBD999.
           05  CODIGO-CB99         PIC 99.
           05  HISTORICO-CB99      PIC X(30).
           05  APURACAO-RED-C-CB99 PIC 9(3).
           05  APURACAO-RED-D-CB99 PIC 9(3).

           COPY CXPW100.

       FD  CXD900.
       01  REG-CXD900.
           05  SEQ-CX900              PIC 9(04).
           05  DATA-MOV-CX900         PIC 9(08).
           05  TIPO-LCTO-CX900        PIC 99.
      * Tipo-lcto < 50 - saídas     e tipo-lcto >= 50 - entradas
           05  HISTORICO-CX900        PIC X(30).
           05  DOCUMENTO-CX900        PIC X(10).
           05  VALOR-CX900            PIC 9(10)V99.
           05  CONTAPART-CX900        PIC 9(06).
           05  CONTA-REDUZ-CX900      PIC 9(03).
           05  CONTABIL-CX900         PIC 9.
      * será usado no futuro - para informar se o lancto é contábil
           05  RESPONSAVEL-CX900      PIC X(5).
           05  SEQ-DESM-CX900         PIC 9(4).

           COPY CRPW020.

       FD  CRD920.
       01  REG-CRD920.
           05  DATA-MOVTO-CR92                  PIC 9(8).
           05  COD-COMPL-CR92.
               10  CLASS-CLIENTE-CR92           PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR92                 PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR92                         PIC 9(5).
           05  PORTADOR-CR92                    PIC 9999.
           05  CARTEIRA-CR92                    PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR92                PIC 99.
           05  NR-DOCTO-CR92                    PIC X(10).
           05  OUTRO-DOCTO-CR92                 PIC X(25).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR92                  PIC 9.
      *    TIPO-DOCTO = 0-DUPLICATA  1-NTA-PROMISSÓRIA  2-ORG.EVENTO
      *                 3-DEBITO AUTOMATICO 4-CARTAO CREDITO
           05  DATA-EMISSAO-CR92                PIC 9(8).
           05  DATA-VENCTO-CR92                 PIC 9(8).
      *    DATA-VENCTO-CR20 - AAAAMMDD
           05  DESCRICAO-CR92                   PIC X(30).
           05  SITUACAO-CR92                    PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA  4-CANCELADA
           05  TIPO-MOEDA-CR92                  PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR92.
               10  NR-PARC-CR92                 PIC 99.
               10  TOT-PARC-CR92                PIC 99.
           05  CODREDUZ-APUR-CR92               PIC 9(3).
           05  VALOR-TOT-CR92                   PIC 9(8)V99.
           05  JURO-RCTO-CR92                   PIC 9(6)V99.
           05  MULTA-RCTO-CR92                  PIC 9(6)V99.
           05  DESCONTO-CR92                    PIC 9(6)V99.
           05  DATA-RCTO-CR92                   PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR92                   PIC 9(8)V99.
           05  CONTABILIZADO-CR92               PIC 9.
           05  VENDEDOR-CR92                    PIC 9(6).
           05  RESPONSAVEL-CR92                 PIC X(5).
           05  DIGITADOR-CR92                   PIC X(5).
           05  SEQ-CAIXA-CR92                   PIC 9(3).
           05  NR-NOTA-FISCAL-CR92              PIC X(10).
           05  DATA-NTA-FISCAL-CR92             PIC 9(8).
           05  FORMA-PAGTO-CR92                 PIC X(10).
           05  DCR-MEM-CR92                     PIC X(15).
           05  CARTAO-CRED-CR92                 PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR92       PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR92       PIC 9(03)V99.
           05  FILLER                           PIC X(18).

           COPY CRPW099.

       FD  CRD999.
       01  REG-CRD999.
           05  DATA-MOVTO-CR999             PIC 9(8).
           05  CLASS-CLIENTE-CR999          PIC 9.
      *    classificação cliente =  0-contrato  1-comum
           05  CLIENTE-CR999                PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR999                    PIC 9(5).
           05  PORTADOR-CR999               PIC 9999.
           05  CARTEIRA-CR999               PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR999           PIC 99.
           05  NR-DOCTO-CR999               PIC X(10).
           05  OUTRO-DOCTO-CR999            PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR999             PIC 9.
      *    TIPO-DOCTO = 0-DUPLICATA  1-NTA-PROMISSÓRIA  2-ORG.EVENTO
           05  DATA-EMISSAO-CR999           PIC 9(8).
           05  DATA-VENCTO-CR999            PIC 9(8).
           05  DESCRICAO-CR999              PIC X(30).
           05  SITUACAO-CR999               PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA  4-CANCELADA
           05  TIPO-MOEDA-CR999             PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR999.
               10  NR-PARC-CR999            PIC 99.
               10  TOT-PARC-CR999           PIC 99.
           05  CODREDUZ-APUR-CR999          PIC 9(3).
           05  VALOR-TOT-CR999              PIC 9(8)V99.
           05  JURO-RCTO-CR999              PIC 9(6)V99.
           05  MULTA-RCTO-CR999             PIC 9(6)V99.
           05  DESCONTO-CR999               PIC 9(6)V99.
           05  DATA-RCTO-CR999              PIC 9(8).
           05  VALOR-LIQ-CR999              PIC 9(8)V99.
           05  CONTABILIZADO-CR999          PIC 9.
           05  RESPONSAVEL-CR999            PIC X(5).
           05  DIGITADOR-CR999              PIC X(5).
           05  SEQ-CAIXA-CR999              PIC 9(3).
           05  NR-NOTA-FISCAL-CR999         PIC X(10).
           05  DATA-NTA-FISCAL-CR999        PIC 9(8).
           05  FORMA-PAGTO-CR999            PIC X(10).
           05  DCR-MEM-CR999                PIC X(15).
           05  CARTAO-CRED-CR999            PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR999  PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR999  PIC 9(03)V99.
           05  USUARIO-EXCLUSAO-CR999       PIC 9(05).
           05  DATA-EXCLUSAO-CR999          PIC 9(08).
           05  HORA-EXCLUSAO-CR999          PIC 9(04).
           05  FILLER                       PIC X(01).


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

           MOVE "CXD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020
           MOVE "CXD999"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD999
           MOVE "CBD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD004
           MOVE "CBD999"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD999
           MOVE "CXD100"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD100
           MOVE "CXD900"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD900
           MOVE "CRD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020
           MOVE "CRD920"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD920
           MOVE "CRD099"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD099
           MOVE "CRD999"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD999


           MOVE ZEROS     TO ERRO-W.
           OPEN OUTPUT CXD020 CBD004 CXD100 CRD020 CRD099
           CLOSE       CXD020 CBD004 CXD100 CRD020 CRD099
           OPEN I-O    CXD020 CBD004 CXD100 CRD020 CRD099
           OPEN INPUT  CXD999 CBD999 CXD900 CRD920 CRD999

           CLOSE CONTROLE.

           IF ST-CXD020 = "35"
              CLOSE CXD020      OPEN OUTPUT CXD020
              CLOSE CXD020      OPEN I-O    CXD020
           END-IF.
           IF ST-CXD999 = "35"
              CLOSE CXD999      OPEN OUTPUT CXD999
              CLOSE CXD999      OPEN I-O    CXD999
           END-IF.

           IF ST-CBD004 = "35"
              CLOSE CBD004      OPEN OUTPUT CBD004
              CLOSE CBD004      OPEN I-O    CBD004
           END-IF.
           IF ST-CBD999 = "35"
              CLOSE CBD999      OPEN OUTPUT CBD999
              CLOSE CBD999      OPEN I-O    CBD999
           END-IF.

           IF ST-CXD100 = "35"
              CLOSE CXD100      OPEN OUTPUT CXD100
              CLOSE CXD100      OPEN I-O    CXD100
           END-IF.
           IF ST-CXD900 = "35"
              CLOSE CXD900      OPEN OUTPUT CXD900
              CLOSE CXD900      OPEN I-O    CXD900
           END-IF.

           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O    CRD020
           END-IF.
           IF ST-CRD920 = "35"
              CLOSE CRD920      OPEN OUTPUT CRD920
              CLOSE CRD920      OPEN I-O    CRD920
           END-IF.

           IF ST-CRD099 = "35"
              CLOSE CRD099      OPEN OUTPUT CRD099
              CLOSE CRD099      OPEN I-O    CRD099
           END-IF.
           IF ST-CRD999 = "35"
              CLOSE CRD999      OPEN OUTPUT CRD999
              CLOSE CRD999      OPEN I-O    CRD999
           END-IF.

          DISPLAY "VOU COMECAR A CONVERSAO DOS ARQUIVOS CXD020 CBD004 CX
      -   "D900" STOP " ".

           INITIALIZE REG-CXD999
           START CXD999 KEY IS NOT LESS CODIGO-REDUZ-CX99 INVALID KEY
               MOVE "10" TO ST-CXD999.

           PERFORM UNTIL ST-CXD999 = "10"
               READ CXD999 NEXT AT END
                   MOVE "10" TO ST-CXD999
               NOT AT END
                   MOVE CODIGO-COMPL-CX99   TO CODIGO-COMPL-CX20
                   MOVE CODIGO-REDUZ-CX99   TO CODIGO-REDUZ-CX20
                   MOVE GRAU-CX99           TO GRAU-CX20
                   MOVE DESCRICAO-CX99      TO DESCRICAO-CX20
                   MOVE TIPO-CONTA-CX99     TO TIPO-CONTA-CX20

                   display "CXD020 => " reg-cxd020
                   WRITE REG-CXD020 INVALID KEY
                      DISPLAY "Erro de Gravacao...CXD020" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CBD999
           START CBD999 KEY IS NOT LESS CODIGO-CB99 INVALID KEY
               MOVE "10" TO ST-CBD999.

           PERFORM UNTIL ST-CBD999 = "10"
               READ CBD999 NEXT AT END
                   MOVE "10" TO ST-CBD999
               NOT AT END
                   MOVE CODIGO-CB99         TO CODIGO-CB04
                   MOVE HISTORICO-CB99      TO HISTORICO-CB04
                   MOVE APURACAO-RED-C-CB99 TO APURACAO-RED-C-CB04
                   MOVE APURACAO-RED-D-CB99 TO APURACAO-RED-D-CB04

                   display reg-cbd004
                   WRITE REG-CBD004 INVALID KEY
                      DISPLAY "Erro de Gravacao...CBD004" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CXD900
           START CXD900 KEY IS NOT LESS CHAVE-CX900 INVALID KEY
               MOVE "10" TO ST-CXD900.

           PERFORM UNTIL ST-CXD900 = "10"
               READ CXD900 NEXT AT END
                   MOVE "10" TO ST-CXD900
               NOT AT END
                   MOVE SEQ-CX900           TO SEQ-CX100
                   MOVE DATA-MOV-CX900      TO DATA-MOV-CX100
                   MOVE TIPO-LCTO-CX900     TO TIPO-LCTO-CX100
                   MOVE HISTORICO-CX900     TO HISTORICO-CX100
                   MOVE DOCUMENTO-CX900     TO DOCUMENTO-CX100
                   MOVE VALOR-CX900         TO VALOR-CX100
                   MOVE CONTAPART-CX900     TO CONTAPART-CX100
                   MOVE CONTA-REDUZ-CX900   TO CONTA-REDUZ-CX100
                   MOVE CONTABIL-CX900      TO CONTABIL-CX100
                   MOVE RESPONSAVEL-CX900   TO RESPONSAVEL-CX100
                   MOVE SEQ-DESM-CX900      TO SEQ-DESM-CX100

                   display "CXD100 => " reg-cxd100
                   WRITE REG-CXD100 INVALID KEY
                      DISPLAY "Erro de Gravacao...CXD100" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CRD920
           START CRD920 KEY IS NOT LESS CHAVE-CR92 INVALID KEY
               MOVE "10" TO ST-CRD920.

           PERFORM UNTIL ST-CRD920 = "10"
               READ CRD920 NEXT AT END
                   MOVE "10" TO ST-CRD920
               NOT AT END
                   MOVE DATA-MOVTO-CR92        TO DATA-MOVTO-CR20
                   MOVE CLASS-CLIENTE-CR92     TO CLASS-CLIENTE-CR20
                   MOVE CLIENTE-CR92           TO CLIENTE-CR20
                   MOVE SEQ-CR92               TO SEQ-CR20
                   MOVE PORTADOR-CR92          TO PORTADOR-CR20
                   MOVE CARTEIRA-CR92          TO CARTEIRA-CR20
                   MOVE SITUACAO-TIT-CR92      TO SITUACAO-TIT-CR20
                   MOVE NR-DOCTO-CR92          TO NR-DOCTO-CR20
                   MOVE OUTRO-DOCTO-CR92       TO OUTRO-DOCTO-CR20
                   MOVE TIPO-DOCTO-CR92        TO TIPO-DOCTO-CR20
                   MOVE DATA-EMISSAO-CR92      TO DATA-EMISSAO-CR20
                   MOVE DATA-VENCTO-CR92       TO DATA-VENCTO-CR20
                   MOVE DESCRICAO-CR92         TO DESCRICAO-CR20
                   MOVE SITUACAO-CR92          TO SITUACAO-CR20
                   MOVE TIPO-MOEDA-CR92        TO TIPO-MOEDA-CR20
                   MOVE NR-PARC-CR92           TO NR-PARC-CR20
                   MOVE TOT-PARC-CR92          TO TOT-PARC-CR20
                   MOVE CODREDUZ-APUR-CR92     TO CODREDUZ-APUR-CR20
                   MOVE VALOR-TOT-CR92         TO VALOR-TOT-CR20
                   MOVE JURO-RCTO-CR92         TO JURO-RCTO-CR20
                   MOVE MULTA-RCTO-CR92        TO MULTA-RCTO-CR20
                   MOVE DESCONTO-CR92          TO DESCONTO-CR20
                   MOVE DATA-RCTO-CR92         TO DATA-RCTO-CR20
                   MOVE VALOR-LIQ-CR92         TO VALOR-LIQ-CR20
                   MOVE CONTABILIZADO-CR92     TO CONTABILIZADO-CR20
                   MOVE VENDEDOR-CR92          TO VENDEDOR-CR20
                   MOVE RESPONSAVEL-CR92       TO RESPONSAVEL-CR20
                   MOVE DIGITADOR-CR92         TO DIGITADOR-CR20
                   MOVE SEQ-CAIXA-CR92         TO SEQ-CAIXA-CR20
                   MOVE NR-NOTA-FISCAL-CR92    TO NR-NOTA-FISCAL-CR20
                   MOVE DATA-NTA-FISCAL-CR92   TO DATA-NTA-FISCAL-CR20
                   MOVE FORMA-PAGTO-CR92       TO FORMA-PAGTO-CR20
                   MOVE DCR-MEM-CR92           TO DCR-MEM-CR20
                   MOVE CARTAO-CRED-CR92       TO CARTAO-CRED-CR20
                   MOVE TAXA-ADMINIST-CREDITO-CR92 TO
                                             TAXA-ADMINIST-CREDITO-CR20
                   MOVE TAXA-ADMINIST-PARCELA-CR92 TO
                                             TAXA-ADMINIST-PARCELA-CR20

                   display "CRD020 => " reg-crd020
                   WRITE REG-CRD020 INVALID KEY
                      DISPLAY "Erro de Gravacao...CRD020" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CRD999
           START CRD999 KEY IS NOT LESS CHAVE-CR999 INVALID KEY
               MOVE "10" TO ST-CRD999.

           PERFORM UNTIL ST-CRD999 = "10"
               READ CRD999 NEXT AT END
                   MOVE "10" TO ST-CRD999
               NOT AT END
                   MOVE DATA-MOVTO-CR999       TO DATA-MOVTO-CR99
                   MOVE CLASS-CLIENTE-CR999    TO CLASS-CLIENTE-CR99
                   MOVE CLIENTE-CR999          TO CLIENTE-CR99
                   MOVE SEQ-CR999              TO SEQ-CR99
                   MOVE PORTADOR-CR999         TO PORTADOR-CR99
                   MOVE CARTEIRA-CR999         TO CARTEIRA-CR99
                   MOVE SITUACAO-TIT-CR999     TO SITUACAO-TIT-CR99
                   MOVE NR-DOCTO-CR999         TO NR-DOCTO-CR99
                   MOVE OUTRO-DOCTO-CR999      TO OUTRO-DOCTO-CR99
                   MOVE TIPO-DOCTO-CR999       TO TIPO-DOCTO-CR99
                   MOVE DATA-EMISSAO-CR999     TO DATA-EMISSAO-CR99
                   MOVE DATA-VENCTO-CR999      TO DATA-VENCTO-CR99
                   MOVE DESCRICAO-CR999        TO DESCRICAO-CR99
                   MOVE SITUACAO-CR999         TO SITUACAO-CR99
                   MOVE TIPO-MOEDA-CR999       TO TIPO-MOEDA-CR99
                   MOVE NR-PARC-CR999          TO NR-PARC-CR99
                   MOVE TOT-PARC-CR999         TO TOT-PARC-CR99
                   MOVE CODREDUZ-APUR-CR999    TO CODREDUZ-APUR-CR99
                   MOVE VALOR-TOT-CR999        TO VALOR-TOT-CR99
                   MOVE JURO-RCTO-CR999        TO JURO-RCTO-CR99
                   MOVE MULTA-RCTO-CR999       TO MULTA-RCTO-CR99
                   MOVE DESCONTO-CR999         TO DESCONTO-CR99
                   MOVE DATA-RCTO-CR999        TO DATA-RCTO-CR99
                   MOVE VALOR-LIQ-CR999        TO VALOR-LIQ-CR99
                   MOVE CONTABILIZADO-CR999    TO CONTABILIZADO-CR99
                   MOVE RESPONSAVEL-CR999      TO RESPONSAVEL-CR99
                   MOVE DIGITADOR-CR999        TO DIGITADOR-CR99
                   MOVE SEQ-CAIXA-CR999        TO SEQ-CAIXA-CR99
                   MOVE NR-NOTA-FISCAL-CR999   TO NR-NOTA-FISCAL-CR99
                   MOVE DATA-NTA-FISCAL-CR999  TO DATA-NTA-FISCAL-CR99
                   MOVE FORMA-PAGTO-CR999      TO FORMA-PAGTO-CR99
                   MOVE DCR-MEM-CR999          TO DCR-MEM-CR99
                   MOVE CARTAO-CRED-CR999      TO CARTAO-CRED-CR99
                   MOVE TAXA-ADMINIST-CREDITO-CR999 TO
                        TAXA-ADMINIST-CREDITO-CR99
                   MOVE TAXA-ADMINIST-PARCELA-CR999 TO
                        TAXA-ADMINIST-PARCELA-CR99
                   MOVE USUARIO-EXCLUSAO-CR999 TO USUARIO-EXCLUSAO-CR99
                   MOVE DATA-EXCLUSAO-CR999    TO DATA-EXCLUSAO-CR99
                   MOVE HORA-EXCLUSAO-CR999    TO HORA-EXCLUSAO-CR99

                   display "CRD099 => " reg-crd099
                   WRITE REG-CRD099 INVALID KEY
                      DISPLAY "Erro de Gravacao...CRD099" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM


           CLOSE CXD020 CXD999 CBD004 CBD999 CXD100 CXD900 CRD020 CRD920
                 CRD999 CRD099
           STOP RUN.
