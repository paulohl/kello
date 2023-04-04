           SELECT ACD130 ASSIGN TO PATH-ACD130
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-ACD130
                  RECORD KEY IS CHAVE-AC130 =
                       NUMERO-AC130
                  ALTERNATE RECORD KEY IS ALT1-AC130 =
                       PROCEDIMENTO-AC130
                       NUMERO-AC130
                  ALTERNATE RECORD KEY IS ALT2-AC130 =
                       DESTINO-AC130
                       NUMERO-AC130.
