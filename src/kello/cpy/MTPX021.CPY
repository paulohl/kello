           SELECT  MTD021 ASSIGN TO PATH-MTD021
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   STATUS IS ST-MTD021
                   RECORD KEY IS CHAVE-MT21
                   ALTERNATE RECORD KEY IS CHAVE-PRI-MT21 =
                                           CAMPANHA-MT21
                                           PRIORIDADE-MT21
                                           CHAVE-MT21.
