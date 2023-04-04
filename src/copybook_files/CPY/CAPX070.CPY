           SELECT  CAD070 ASSIGN TO PATH-CAD070
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-CAD070
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-70 = NR-CONTRATO-70 ITEM-70
                   ALTERNATE RECORD KEY IS DATAPREVISTA-70
                            WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ALT1-70 =  NR-CONTRATO-70
                             REALIZADO-70 DATAPREVISTA-70 ITEM-70.
