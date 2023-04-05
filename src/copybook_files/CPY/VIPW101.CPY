       FD  VID101.
       01  REG-VID101.
           05  NR-FITAS-V101.
               10  CONTRATO-V101         PIC 9(04).
               10  NR-FITA-V101          PIC 9(5).
           05  REVISOR-V101              PIC 9(6).
           05  DATA-REVISAO-V101         PIC 9(8).
           05  AVALIACAO-GERAL-V101      PIC 9.
      *    AVALIACAO-GERAL = 1-PESSIMA  2-RUIM  3-REGULAR 4-BOA  5-OTIMA
