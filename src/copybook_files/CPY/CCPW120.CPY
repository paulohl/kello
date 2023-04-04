      *arquivo de salarios de VENDEDORES
       FD  CCD120.
       01  REG-CCD120.
           05  DATA-MOVTO-CC120         PIC 9(8).
           05  MESANO-BASE-CC120        PIC 9(6).  *> AA/MMMM
           05  CODIGO-CC120             PIC 9(6).
           05  DOCTO-CC120              PIC X(10).
           05  VALOR-CREDITO-CC120      PIC 9(8)V99.
           05  VALOR-LIBERADO-CC120     PIC 9(8)V99.
           05  ATUALIZADO-CC-CC120      PIC 9.
      *   ATUALIZADO-CC = 0 (NÆo foi atualizado no contas correntes)
      *                   1 (foi atualizado)
           05  DIGITADOR-CC120          PIC X(5).
