           SELECT RCD100P ASSIGN       TO PATH-RCD100P
                          ORGANIZATION IS      INDEXED
                          ACCESS MODE  IS      DYNAMIC
                          LOCK MODE    IS    AUTOMATIC
                          WITH LOCK    ON       RECORD
                          STATUS       IS   ST-RCD100P
                          RECORD KEY   IS CHAVE-RECP =
                                      CHAVE-ALBUM-RECP
                                          PRODUTO-RECP
                                           MODELO-RECP.
