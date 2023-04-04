           SELECT CGD003 ASSIGN TO PATH-CGD003
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-CG03
                  ALTERNATE RECORD KEY IS
                  ALT2-CG03 = CPF-CG03
                              CODIGO-CG03
                  STATUS IS ST-CGD003.
