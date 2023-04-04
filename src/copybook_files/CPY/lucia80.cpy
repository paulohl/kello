           SELECT  COD080 ASSIGN TO PATH-COD080
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD080
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO80 =
                           NR-CONTRATO-CO80 ITEM-CO80.
      * ARQUIVO DE MOVIMENTO DE OUTRAS DESPESAS
       FD  COD080.
       01  REG-COD080.
           05  NR-CONTRATO-CO80    PIC 9(04).
           05  ITEM-CO80           PIC 99.
           05  DESCRICAO-CO80      PIC X(30).
           05  DATA-PREV-CO80      PIC 9(8).
      *    DATA-PREV-CO80 - INVERTIDA
           05  QTDE-PREV-CO80      PIC 9(6).
           05  VALOR-PREV-CO80     PIC 9(8)V99.
           05  DATA-REAL-CO80      PIC 9(8).
      *    DATA-REAL INVERTIDA
           05  QTDE-REAL-CO80      PIC 9(6).
           05  VALOR-REAL-CO80     PIC 9(8)V99.
