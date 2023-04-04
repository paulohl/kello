           SELECT GED002 ASSIGN TO PATH-GED002
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-GED002
                  RECORD KEY IS CHAVE-GE02 =
                       USUARIO-C-GE02
                       PROGRAMA-GE02
                       USUARIO-P-GE02
                       IDENTIFICACAO-GE02
                  ALTERNATE RECORD KEY ALT-GE02 =
                       PROGRAMA-GE02
                       USUARIO-C-GE02
                       IDENTIFICACAO-GE02
                       WITH DUPLICATES.
