           SELECT  COD004 ASSIGN TO PATH-COD004
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   STATUS IS ST-COD004
                   RECORD KEY IS CODIGO-CO04
                   ALTERNATE RECORD KEY IS DATA-INI-CO04
                                        WITH DUPLICATES.
      * CADASTRO DE CAMPANHAS DE CONTRATO
       FD  COD004.
       01  REG-COD004.
           05  CODIGO-CO04       PIC 99.
           05  DATA-INI-CO04     PIC 9(8).
           05  DATA-FIM-CO04     PIC 9(8).
