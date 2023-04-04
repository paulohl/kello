           SELECT VID110 ASSIGN TO PATH-VID110
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-V110 = CONTRATO-V110 ITEM-V110
                                               TIPO-V110
                  STATUS IS ST-VID110.
