           SELECT RED303 ASSIGN TO PATH-RED303
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RED303
                  RECORD KEY IS CHAVE-R303 = DOCTO-R303 REPRES-R303
                         FUNCAO-R303
                  ALTERNATE RECORD KEY IS ALT-R303 = REPRES-R303
                            FUNCAO-R303 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-R303 = DOCTO-R303
                            SEQ-R303.
