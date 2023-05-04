           SELECT CGD002 ASSIGN TO PATH-CGD002
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-CG02
                  ALTERNATE RECORD KEY IS
                  ALT2-CG02 = CGC-CG02
                              CODIGO-CG02
                  STATUS IS ST-CGD002.
