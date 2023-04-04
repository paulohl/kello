           SELECT  COD006 ASSIGN TO PATH-COD006
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   STATUS IS ST-COD006
                   RECORD KEY IS NR-CONTRATO-CO06.
      *  ARQUIVO DE PREÇO DE VENDA PREVISTO
       FD  COD006.
       01  REG-COD006.
           05  NR-CONTRATO-CO06    PIC 9(4).
           05  FOTO-CO06           PIC 9(3)V99.
           05  IND-FOTO-CO06       PIC 99.
           05  ALBUM-CO06          PIC 9(3)V99.
           05  IND-ALBUM-CO06      PIC 99.
           05  CAPA-ALBUM-CO06     PIC 9(3)V99.
           05  IND-CAPA-ALBUM-CO06 PIC 99.
           05  FITA-CO06           PIC 9(3)V99.
           05  IND-FITA-CO06       PIC 99.
           05  CAPA-FITA-CO06      PIC 9(3)V99.
           05  IND-CAPA-FITA-CO06  PIC 99.
