      *// Adição de novos produtos
           SELECT MTD020C ASSIGN TO PATH-MTD020C
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD020C
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS ALBUM-MTG-C.
