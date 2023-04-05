           SELECT MTD020 ASSIGN TO PATH-MTD020
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD020
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS ALBUM-MTG
                  ALTERNATE RECORD KEY IS CHAVE-MTG =
                            DATAMOV-MTG, ALBUM-MTG
                  ALTERNATE RECORD KEY IS ANOMES-VISITA-MTG
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-MTG =
                            DATAROMANEIO-MTG, ALBUM-MTG.
