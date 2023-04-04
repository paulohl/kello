      * Arquivo de emissão de cheques.
       FD  CBD100.
       01  REG-CBD100.
           05  CODIGO-FORN-CB100      PIC 9(6).
           05  NR-CHEQUE-CB100        PIC 9(6).
           05  SEQ-CTA-PAGAR-CB100    PIC 9(5).
           05  DATA-EMISSAO-CB100     PIC 9(8).
           05  VALOR-CB100            PIC 9(8)V99.
           05  NOMINAL-A-CB100        PIC 9(6).
           05  DATA-VENCTO-CB100      PIC 9(8).
           05  SITUACAO-CB100         PIC 9(2).
      *  Situação = Cadastro de situacao cbd003
