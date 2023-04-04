           SELECT COD040 ASSIGN TO PATH-COD040
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-COD040
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS NR-CONTRATO-CO40
                  ALTERNATE RECORD KEY IS ALT1-CO40 = MESANO-PREV-CO40
                            NR-CONTRATO-CO40
                  ALTERNATE RECORD KEY IS ALT2-CO40 = MESANO-PREV-CO40
                            CIDADE-CO40 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CO40 = ORIGEM-CO40
                            NR-CONTRATO-CO40
                  ALTERNATE RECORD KEY IS ASSINATURA-CO40
                            WITH DUPLICATES.
