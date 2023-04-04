           SELECT GED001 ASSIGN TO PATH-GED001
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-GED001
                  RECORD KEY IS CHAVE-GE01 =
                       USUARIO-GE01
                       PROGRAMA-GE01
                  ALTERNATE RECORD KEY ALT-GE01 =
                       PROGRAMA-GE01
                       USUARIO-GE01.
