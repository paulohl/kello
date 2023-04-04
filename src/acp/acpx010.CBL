           SELECT ACD010 ASSIGN TO PATH-ACD010
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-ACD010
                  RECORD KEY IS CHAVE-AC10 =
                       TIPO-AC10
                       CODIGO-AC10
                  ALTERNATE RECORD KEY IS ALT-AC10 =
                       TIPO-AC10
                       DESCRICAO-AC10.
