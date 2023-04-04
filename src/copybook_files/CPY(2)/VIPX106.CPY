           SELECT VID106 ASSIGN TO PATH-VID106
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-V106 = CONTRATO-V106 ITEM-V106
                                NR-MASTER-V106
                  ALTERNATE RECORD KEY IS ALT-V106 = CONTRATO-V106
                          NR-FITA-V106 WITH DUPLICATES
                  STATUS IS ST-VID106.
