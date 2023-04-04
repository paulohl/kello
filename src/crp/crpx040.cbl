           SELECT CRD040 ASSIGN TO PATH-CRD040
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS CHAVE-40 = NOME-40 SEQ-40
                  ALTERNATE RECORD KEY IS ALT-40 =
                           NOME-40 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-40 = VENC-40
                           BANCO-40 NR-DUPLICATA-40 WITH DUPLICATES
                  STATUS IS ST-CRD040.
