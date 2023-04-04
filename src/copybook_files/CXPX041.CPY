           SELECT CXD041 ASSIGN TO PATH-CXD041
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CXD041
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CX41 = ANOMES-CX41 CONTAPART-CX41
                  ALTERNATE RECORD KEY IS
                            ALT-CX41 = CONTAPART-CX41 ANOMES-CX41.
