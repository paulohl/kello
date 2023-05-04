      *// Adição de novos produtos
           SELECT RCD100C ASSIGN TO PATH-RCD100C
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-RCD100C
                  RECORD KEY IS CHAVE-ALBUM-REC-C.
