           SELECT CAD040 ASSIGN TO PATH-CAD040
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CAD040
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS NR-CONTRATO-040
                  ALTERNATE RECORD KEY IS ALT1-040 = MESANO-PREV-040
                            NR-CONTRATO-040
                  ALTERNATE RECORD KEY IS ALT2-040 = MESANO-PREV-040
                            CIDADE-040 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-040 = ORIGEM-040
                            NR-CONTRATO-040.
