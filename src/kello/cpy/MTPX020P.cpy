           SELECT MTD020P ASSIGN       TO PATH-MTD020P
                          ORGANIZATION IS      INDEXED
                          ACCESS MODE  IS      DYNAMIC
                          STATUS       IS   ST-MTD020P
                          LOCK MODE    IS    AUTOMATIC
                          WITH LOCK    ON       RECORD
                          RECORD KEY   IS CHAVE-MTGP =
                                            ALBUM-MTGP
                                          PRODUTO-MTGP
                                           MODELO-MTGP.
