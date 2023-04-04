           SELECT CAD030 ASSIGN TO PATH-CAD030
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CAD030
                  RECORD KEY IS CHAVE-CAD30 =
                       CODIGO-CAD30
                  ALTERNATE RECORD KEY IS ALT-CAD30 =
                       DESCRICAO-CAD30.
