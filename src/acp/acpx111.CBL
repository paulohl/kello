           SELECT ACD111 ASSIGN TO PATH-ACD111
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-ACD111
                  RECORD KEY IS CHAVE-AC111 =
                       NUMERO-AC111
                       TIPO-AC111
                       SEQ-AC111.
