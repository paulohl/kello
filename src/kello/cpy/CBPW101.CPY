      * movto de extratos bancários
       FD  CBD101.
       01  REG-CBD101.
           05  DATA-EXTRATO-CB101 PIC 9(8).
           05  SEQ-CB101          PIC 9(3).
           05  CONTA-BANCO-CB101  PIC 9(6).
           05  HISTORICO-CB101    PIC 9(2).
      *    Historico-cb101 = codigo-cb04 (tipo lcto extrato bancario)
           05  NR-CHEQUE-CB101    PIC 9(6).
           05  VALOR-CB101        PIC 9(8)V99.
           05  RESOLVER-CB101     PIC 9.
      * resolver = 0 (não)  1(sim) - caso tenha alguma divergencia
