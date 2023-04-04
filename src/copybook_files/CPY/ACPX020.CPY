           SELECT ACD020 ASSIGN TO PATH-ACD020
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-ACD020
                  RECORD KEY IS CHAVE-AC20 =
                       CODIGO-AC20
                  ALTERNATE RECORD KEY IS ALT-AC20 =
                       TIPO-AC20
                       DESCRICAO-AC20.
