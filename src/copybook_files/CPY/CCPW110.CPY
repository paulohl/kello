      *arquivo de salarios de reportagem
       FD  CCD110.
       01  REG-CCD110.
           05  DATA-MOVTO-CC110         PIC 9(8).
           05  MESANO-BASE-CC110        PIC 9(6).
           05  CODIGO-CC110             PIC 9(6).
           05  DOCTO-CC110              PIC X(10).
           05  VALOR-CREDITO-CC110      PIC 9(8)V99.
           05  VALOR-LIBERADO-CC110     PIC 9(8)V99.
           05  ATUALIZADO-CC-CC110      PIC 9.
      *   ATUALIZADO-CC = 0 (Não foi atualizado no contas correntes)
      *                   1 (foi atualizado)
           05  DIGITADOR-CC110          PIC X(5).
