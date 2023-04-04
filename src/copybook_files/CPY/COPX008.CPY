           SELECT COD008 ASSIGN TO PATH-COD008
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-COD008
                  RECORD KEY IS CHAVE-CO08 =
                       CODIGO-CO08
                  ALTERNATE RECORD KEY IS ALT-CO08 =
                       DESCRICAO-CO08.
