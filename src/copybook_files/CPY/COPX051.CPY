           SELECT  COD051 ASSIGN TO PATH-COD051
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD051
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO51 =
                           NR-CONTRATO-CO51 ITEM-CO51
                   ALTERNATE RECORD KEY IS DATA-VENCTO-CO51
                             WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ALT1-CO51 =  NR-CONTRATO-CO51
                        REALIZADO-CO51 DATA-VENCTO-CO51 ITEM-CO51.
