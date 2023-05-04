           SELECT MTD030 ASSIGN TO PATH-MTD030
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD030
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-MT30 = ALBUM-MT30,
                                       ITEM-EVENTO-MT30,
                                       COD-EVENTO-MT30
                  ALTERNATE RECORD KEY IS ALT-MT30 =
                            COD-EVENTO-MT30,
                            ITEM-EVENTO-MT30,
                            ALBUM-MT30.
