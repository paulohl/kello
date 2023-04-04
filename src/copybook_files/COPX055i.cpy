           SELECT  COD055i ASSIGN TO PATH-COD055i
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD055i
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO55i =
                           NR-CONTRATO-CO55i ITEM-CO55i
                   ALTERNATE RECORD KEY IS DATA-VENCTO-CO55i
                             WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ALT1-CO55i =
                        NR-CONTRATO-CO55i
                        REALIZADO-CO55i DATA-VENCTO-CO55i ITEM-CO55i.
