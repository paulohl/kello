           SELECT  COD070 ASSIGN TO PATH-COD070
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD070
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO70 =
                           NR-CONTRATO-CO70 ITEM-CO70
                   ALTERNATE RECORD KEY IS DATA-VENCTO-CO70
                             WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ALT1-CO70 =  NR-CONTRATO-CO70
                        REALIZADO-CO70 DATA-VENCTO-CO70 ITEM-CO70.
