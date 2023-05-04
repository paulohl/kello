           SELECT  COD060C ASSIGN TO PATH-COD060C
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD060C
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO60C =
                                 NR-CONTRATO-CO60C
                                 ITEM-CO60C.
