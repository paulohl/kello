      * Arquivo de Movimento de contas a receber (Baixas)
       FD  CRD020B.
       01  REG-CRD020B.
           05  COD-COMPL-CR20B.
               10  CLASS-CLIENTE-CR20B          PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR20B                PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR20B                        PIC 9(5).
           05  VALOR-TOT-CR20B                  PIC 9(8)V99.
           05  VALOR-BAIXA-CR20B                PIC 9(8)V99.
           05  JURO-RCTO-CR20B                  PIC 9(6)V99.
           05  MULTA-RCTO-CR20B                 PIC 9(6)V99.
           05  DESCONTO-CR20B                   PIC 9(6)V99.
           05  DATA-RCTO-CR20B                  PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR20B                  PIC 9(8)V99.
           05  SEQ-CAIXA-CR20B                  PIC 9(3).
           05  FORMA-PAGTO-CR20B                PIC X(10).
           05  DCR-MEM-CR20B                    PIC X(15).
           05  USUARIO-CR20B                    PIC X(05).
           05  RECEBEDOR-CR20B                  PIC X(15).
           05  CONTA-CORRENTE-CR20B             PIC 9(06).
           05  DATA-NEGOCIACAO-CR20B            PIC 9(08).
           05  FILLER                           PIC X(16).
