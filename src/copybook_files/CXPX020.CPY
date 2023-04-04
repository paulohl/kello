           SELECT CXD020 ASSIGN TO PATH-CXD020
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CXD020
                  RECORD KEY IS CODIGO-REDUZ-CX20
                  ALTERNATE RECORD KEY IS CODIGO-COMPL-CX20
                  ALTERNATE RECORD KEY IS DESCRICAO-CX20
                            WITH DUPLICATES.
