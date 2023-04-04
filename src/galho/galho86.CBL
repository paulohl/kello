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

           COPY CHPX010.

           SELECT CHD910 ASSIGN TO PATH-CHD910
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH91 = DATA-MOVTO-CH91 SEQ-CH91
                  ALTERNATE RECORD KEY ALT1-CH91 =
                     NOME-CH91, PORTADOR-CH91 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT2-CH91 = SITUACAO-CH91
                     DATA-VENCTO-CH91 PORTADOR-CH91 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT3-CH91 = DATA-MOVTO-CH91
                     VENDEDOR-CH91 SEQ-CH91 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT4-CH91 = COD-COMPL-CH91
                     DATA-VENCTO-CH91 WITH DUPLICATES
                  STATUS IS ST-CHD910.

           COPY CHPX099.

           SELECT CHD999 ASSIGN TO PATH-CHD999
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

           COPY CGPX004.

           SELECT CGD904 ASSIGN TO PATH-CGD904
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-CG94
                  ALTERNATE RECORD KEY IS COD-VENDEDOR-CG94
                            WITH DUPLICATES
                  STATUS IS ST-CGD904.

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

           COPY CHPW010.

       FD  CHD910.
       01  REG-CHD910.
           05  DATA-MOVTO-CH91          PIC 9(8).
           05  SEQ-CH91                 PIC 9(4).
           05  COD-COMPL-CH91.
               10  CLASS-CLIENTE-CH91   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH91         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH91                PIC X(30).
           05  CIDADE-CH91              PIC X(19).
           05  LOTE-CH91                PIC X(01).
           05  CONTA-CORR-CH91          PIC 9(8)   COMP-3.
           05  DV-CONTA-CH91            PIC X.
           05  BANCO-CH91               PIC 9(4)   COMP-3.
           05  AGENCIA-CH91             PIC 9(5)   COMP-3.
           05  DV-AGENCIA-CH91          PIC X.
           05  COMPENSACAO-CH91         PIC 9(3).
           05  CPF-CH91                 PIC 9(11)  COMP-3.
           05  NR-CHEQUE-CH91           PIC X(7).
           05  OUTRO-DOCTO-CH91         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-CH91         PIC 9(8).
           05  PORTADOR-CH91            PIC 9999.
           05  CARTEIRA-CH91            PIC 9.
      *    CARTEIRA-CH10  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH91        PIC 99.
           05  SITUACAO-CH91            PIC 9.
      *    SITUACAO = 0-OK  2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH91       PIC 9(3).
           05  VALOR-CH91               PIC 9(8)V99.
           05  VENDEDOR-CH91            PIC 9(6)   COMP-3.
           05  DIGITADOR-CH91           PIC X(5).
           05  SEQ-CAIXA-CH91           PIC 9(3).
           05  NR-NOTA-FISCAL-CH91      PIC X(10).
           05  DATA-NTA-FISCAL-CH91     PIC 9(8)   COMP-3.
           05  ORIGEM-CH91              PIC 99.

           COPY CHPW099.

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
           05  PORTADOR-CH999            PIC 9999.
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

           COPY CGPW004.

       FD  CGD904.
       01  REG-CGD904.
           05  CODIGO-CG94             PIC 9(6).
           05  DEPTO-CG94              PIC 9(2).
           05  CONTA-APUR-RED-CG94     PIC 9(3).
           05  BANCO-CG94              PIC 9(3).
           05  NOME-BANCO-CG94         PIC X(15).
           05  AGENCIA-CG94            PIC 9(4).
           05  DIGITO-AGENCIA-CG94     PIC X.
           05  NR-CONTA-CG94           PIC 9(10).
           05  DIGITO-CONTA-CG94       PIC X.
           05  COD-VENDEDOR-CG94       PIC 9(5).

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
           05  ST-CHD910             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD999             PIC XX       VALUE SPACES.
           05  ST-CHD099             PIC XX       VALUE SPACES.
           05  ST-CGD004             PIC XX       VALUE SPACES.
           05  ST-CGD904             PIC XX       VALUE SPACES.
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
           MOVE "CHD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010
           MOVE "CHD910"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD910
           MOVE "CHD099"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD099
           MOVE "CHD999"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD999
           MOVE "CGD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD004
           MOVE "CGD904"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD904


           MOVE ZEROS     TO ERRO-W.
           OPEN OUTPUT CXD020 CBD004 CXD100 CRD020 CRD099 CPD020 CPD099
                       CPD023 PFD010 CCD100 CXD042 CHD010 CHD099 CGD004
           CLOSE       CXD020 CBD004 CXD100 CRD020 CRD099 CPD020 CPD099
                       CPD023 PFD010 CCD100 CXD042 CHD010 CHD099 CGD004
           OPEN I-O    CXD020 CBD004 CXD100 CRD020 CRD099 CPD020 CPD099
                       CPD023 PFD010 CCD100 CXD042 CHD010 CHD099 CGD004
           OPEN INPUT  CXD999 CBD999 CXD900 CRD920 CRD999 CPD920 CPD999
                       CPD923 PFD910 CCD999 CXD942 CHD910 CHD999 CGD904

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

           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O    CHD010
           END-IF.
           IF ST-CHD910 = "35"
              CLOSE CHD910      OPEN OUTPUT CHD910
              CLOSE CHD910      OPEN I-O    CHD910
           END-IF.

           IF ST-CHD099 = "35"
              CLOSE CHD099      OPEN OUTPUT CHD099
              CLOSE CHD099      OPEN I-O    CHD099
           END-IF.
           IF ST-CHD999 = "35"
              CLOSE CHD999      OPEN OUTPUT CHD999
              CLOSE CHD999      OPEN I-O    CHD999
           END-IF.

           IF ST-CGD004 = "35"
              CLOSE CGD004      OPEN OUTPUT CGD004
              CLOSE CGD004      OPEN I-O    CGD004
           END-IF.
           IF ST-CGD904 = "35"
              CLOSE CGD904      OPEN OUTPUT CGD904
              CLOSE CGD904      OPEN I-O    CGD904
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

           INITIALIZE REG-CHD910
           START CHD910 KEY IS NOT LESS CHAVE-CH91 INVALID KEY
               MOVE "10" TO ST-CHD910.

           PERFORM UNTIL ST-CHD910 = "10"
               READ CHD910 NEXT AT END
                   MOVE "10" TO ST-CHD910
               NOT AT END
                   MOVE DATA-MOVTO-CH91      TO DATA-MOVTO-CH10
                   MOVE SEQ-CH91             TO SEQ-CH10
                   MOVE CLASS-CLIENTE-CH91   TO CLASS-CLIENTE-CH10
                   MOVE CLIENTE-CH91         TO CLIENTE-CH10
                   MOVE NOME-CH91            TO NOME-CH10
                   MOVE CIDADE-CH91          TO CIDADE-CH10
                   MOVE LOTE-CH91            TO LOTE-CH10
                   MOVE CONTA-CORR-CH91      TO CONTA-CORR-CH10
                   MOVE DV-CONTA-CH91        TO DV-CONTA-CH10
                   MOVE BANCO-CH91           TO BANCO-CH10
                   MOVE AGENCIA-CH91         TO AGENCIA-CH10
                   MOVE DV-AGENCIA-CH91      TO DV-AGENCIA-CH10
                   MOVE COMPENSACAO-CH91     TO COMPENSACAO-CH10
                   MOVE CPF-CH91             TO CPF-CH10
                   MOVE NR-CHEQUE-CH91       TO NR-CHEQUE-CH10
                   MOVE OUTRO-DOCTO-CH91     TO OUTRO-DOCTO-CH10
                   MOVE DATA-VENCTO-CH91     TO DATA-VENCTO-CH10
                   MOVE PORTADOR-CH91        TO PORTADOR-CH10
                   MOVE CARTEIRA-CH91        TO CARTEIRA-CH10
                   MOVE SITUACAO-TIT-CH91    TO SITUACAO-TIT-CH10
                   MOVE SITUACAO-CH91        TO SITUACAO-CH10
                   MOVE CODREDUZ-APUR-CH91   TO CODREDUZ-APUR-CH10
                   MOVE VALOR-CH91           TO VALOR-CH10
                   MOVE VENDEDOR-CH91        TO VENDEDOR-CH10
                   MOVE DIGITADOR-CH91       TO DIGITADOR-CH10
                   MOVE SEQ-CAIXA-CH91       TO SEQ-CAIXA-CH10
                   MOVE NR-NOTA-FISCAL-CH91  TO NR-NOTA-FISCAL-CH10
                   MOVE DATA-NTA-FISCAL-CH91 TO DATA-NTA-FISCAL-CH10
                   MOVE ORIGEM-CH91          TO ORIGEM-CH10

                   display "CHD010 => " reg-chd010
                   WRITE REG-CHD010 INVALID KEY
                      DISPLAY "Erro de Gravacao...CHD010" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CHD999
           START CHD999 KEY IS NOT LESS CHAVE-CH999 INVALID KEY
               MOVE "10" TO ST-CHD999.

           PERFORM UNTIL ST-CHD999 = "10"
               READ CHD999 NEXT AT END
                   MOVE "10" TO ST-CHD999
               NOT AT END
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

                   display "CHD099 => " reg-chd099
                   WRITE REG-CHD099 INVALID KEY
                      DISPLAY "Erro de Gravacao...CHD099" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD904
           START CGD904 KEY IS NOT LESS CODIGO-CG94 INVALID KEY
               MOVE "10" TO ST-CGD904.

           PERFORM UNTIL ST-CGD904 = "10"
               READ CGD904 NEXT AT END
                   MOVE "10" TO ST-CGD904
               NOT AT END
                   MOVE CODIGO-CG94         TO CODIGO-CG04
                   MOVE DEPTO-CG94          TO DEPTO-CG04
                   MOVE CONTA-APUR-RED-CG94 TO CONTA-APUR-RED-CG04
                   MOVE BANCO-CG94          TO BANCO-CG04
                   MOVE NOME-BANCO-CG94     TO NOME-BANCO-CG04
                   MOVE AGENCIA-CG94        TO AGENCIA-CG04
                   MOVE DIGITO-AGENCIA-CG94 TO DIGITO-AGENCIA-CG04
                   MOVE NR-CONTA-CG94       TO NR-CONTA-CG04
                   MOVE DIGITO-CONTA-CG94   TO DIGITO-CONTA-CG04
                   MOVE COD-VENDEDOR-CG94   TO COD-VENDEDOR-CG04

                   display "CGD004 => " reg-cgd004
                   WRITE REG-CGD004 INVALID KEY
                      DISPLAY "Erro de Gravacao...CGD004" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           CLOSE CXD020 CXD999 CBD004 CBD999 CXD100 CXD900 CRD020 CRD920
                 CRD999 CRD099 CPD020 CPD920 CPD099 CPD999 CPD023 CPD923
                 PFD010 PFD910 CCD100 CCD999 CXD042 CXD942 CHD010 CHD910
                 CHD099 CHD999 CGD004 CGD904
           STOP RUN.
