           SELECT ACD140 ASSIGN TO PATH-ACD140
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-ACD140
                  RECORD KEY IS CHAVE-AC140 =
                       NUMERO-AC140
                  ALTERNATE RECORD KEY IS ALT1-AC140 =
                       CONTRATO-AC140
                       NUMERO-AC140.
