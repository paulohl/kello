           SELECT RED101 ASSIGN TO PATH-RED101
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RED101
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-R101 = DOCTO-R101 CONTRATO-R101
                                             EVENTO-R101
                  ALTERNATE RECORD KEY IS ALT1-R101 = DOCTO-R101
                                             EVENTO-R101 CONTRATO-R101
                  ALTERNATE RECORD KEY IS CONTRATO-R101 WITH DUPLICATES.
