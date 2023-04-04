           SELECT  COD061 ASSIGN TO PATH-COD061
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD061
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO61 = NR-CONTRATO-CO61
                      ITEM-CO61 TURMA-CO61 CURSO-CO61.
      *CADASTRO DE TURMAS/CURSO P/ CADA EVENTO
       FD  COD061.
       01  REG-COD061.
           05  NR-CONTRATO-CO61  PIC 9(04).
           05  ITEM-CO61         PIC 9(3).
           05  CURSO-CO61        PIC 9(3).
           05  TURMA-CO61        PIC XX.
