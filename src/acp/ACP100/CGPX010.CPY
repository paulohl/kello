           SELECT CGD010 ASSIGN TO PATH-CGD010
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS COD-COMPL-CG10
                  STATUS IS ST-CGD010
                  ALTERNATE RECORD KEY IS COMPRADOR-CG10
                            WITH DUPLICATES.
