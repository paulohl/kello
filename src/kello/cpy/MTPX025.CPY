           SELECT MTD025 ASSIGN TO PATH-MTD025
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-MTD025
                  RECORD KEY IS CHAVE1-MT25 =
                       CHAVE-MT25
                       MOTIVO-MT25
                  ALTERNATE RECORD KEY IS ALT-MT25 =
                       MOTIVO-MT25
                       CONTRATO-MT25
                       WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-MT25 =
                       DATA-MT25
                       MOTIVO-MT25
                       WITH DUPLICATES.
