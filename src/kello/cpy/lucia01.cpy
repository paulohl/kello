           SELECT COD001 ASSIGN TO PATH-COD001
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-COD001
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-CO01
                  ALTERNATE RECORD KEY IS STATUS-CO01.
      *  Cadastro de Status de Contrato
       FD  COD001.
       01  REG-COD001.
           05  CODIGO-CO01              PIC 99.
           05  STATUS-CO01              PIC X(30).
