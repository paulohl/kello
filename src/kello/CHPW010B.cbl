      * Arquivo de Movimento de cheques (Baixas)
       FD  CHD010B.
       01  REG-CHD010B.
           05  DATA-MOVTO-CH10B                 PIC 9(8).
           05  SEQ-CH10B                        PIC 9(4).
           05  VALOR-TOT-CH10B                  PIC 9(8)V99.
           05  VALOR-BAIXA-CH10B                PIC 9(8)V99.
           05  JURO-RCTO-CH10B                  PIC 9(6)V99.
           05  MULTA-RCTO-CH10B                 PIC 9(6)V99.
           05  DESCONTO-CH10B                   PIC 9(6)V99.
           05  DATA-RCTO-CH10B                  PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CH10B                  PIC 9(8)V99.
           05  SEQ-CAIXA-CH10B                  PIC 9(3).
           05  FORMA-PAGTO-CH10B                PIC X(10).
           05  DCR-MEM-CH10B                    PIC X(15).
           05  RECEBEDOR-CH10B                  PIC X(15).
           05  CODIGO-BANCARIO-CH10B            PIC 9(06).
           05  DATA-NEGOCIACAO-CH10B            PIC 9(08).
           05  FILLER                           PIC X(21).
