      * arquivo de salarios de representantes
       FD  CCD115.
       01  REG-CCD115.
           05  DATA-MOVTO-CC115         PIC 9(8).
           05  MESANO-BASE-CC115        PIC 9(6).
           05  CODIGO-CC115             PIC 9(6).
           05  SALARIO1-CC115           PIC 9(8)V99.
           05  SALARIO2-CC115           PIC 9(8)V99.
           05  VALOR-LIBERADO-CC115     PIC 9(8)V99.
           05  ATUALIZADO-CC-CC115      PIC 9.
      *   ATUALIZADO-CC = 0 (Não foi atualizado no contas correntes)
      *                   1 (foi atualizado)
           05  DIGITADOR-CC115          PIC X(5).
