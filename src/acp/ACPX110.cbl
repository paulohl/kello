           SELECT ACD110 ASSIGN TO PATH-ACD110
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-ACD110
                  RECORD KEY IS CHAVE-AC110 =
                       NUMERO-AC110
                  ALTERNATE RECORD KEY IS ALT1-AC110 =
                       CONTRATO-AC110
                       NUMERO-AC110.
