           SELECT CGD011 ASSIGN TO PATH-CGD011
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS COD-COMPL-CG11
                  ALTERNATE RECORD KEY IS
                  ALT2-CG11 = CPF1-CG11
                              CODIGO-CG11
                  WITH DUPLICATES
                  STATUS IS ST-CGD011.
