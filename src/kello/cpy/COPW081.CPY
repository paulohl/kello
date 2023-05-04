      * MOVIMENTO DE DESPESAS FIXAS
       FD  COD081.
       01  REG-COD081.
           05  NR-CONTRATO-CO81    PIC 9(04).
           05  ITEM-CO81           PIC 99.
           05  TABELA-CO81 OCCURS 10 TIMES.
               10  QTDE-CO81           PIC 9(6).
               10  VALOR-CO81          PIC 9(8)V99.
      *            VALOR UNITARIO
