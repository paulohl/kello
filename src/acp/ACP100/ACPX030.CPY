           SELECT ACD030 ASSIGN TO PATH-ACD030
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-ACD030
                  RECORD KEY IS CHAVE-AC30 =
                       CODIGO-AC30
                  ALTERNATE RECORD KEY IS ALT-AC30 =
                       DESCRICAO-AC30.
