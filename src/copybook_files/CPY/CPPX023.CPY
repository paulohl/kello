          SELECT CPD023 ASSIGN TO PATH-CPD023
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CPD023
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CP23 = FORNEC-CP23 SEQ-CP23
                                             ITEM-CP23.
