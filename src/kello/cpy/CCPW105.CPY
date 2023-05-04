      * arquivo de salarios de funcionários
       FD  CCD105.
       01  REG-CCD105.
           05  DATA-MOVTO-CC105         PIC 9(8).
           05  MESANO-BASE-CC105        PIC 9(6).
           05  CODIGO-CC105             PIC 9(6).
           05  SALARIO1-CC105           PIC 9(8)V99.
           05  SALARIO2-CC105           PIC 9(8)V99.
           05  VALOR-LIBERADO-CC105     PIC 9(8)V99.
           05  ATUALIZADO-CC-CC105      PIC 9.
      *   ATUALIZADO-CC = 0 (Não foi atualizado no contas correntes)
      *                   1 (foi atualizado)
           05  DIGITADOR-CC105          PIC X(5).
