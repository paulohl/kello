           SELECT ACD120 ASSIGN TO PATH-ACD120
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-ACD120
                  RECORD KEY IS CHAVE-AC120 =
                       NUMERO-AC120
                  ALTERNATE RECORD KEY IS ALT1-AC120 =
                       CONTRATO-AC120
                       NUMERO-AC120.
