           SELECT MTD001P ASSIGN       TO PATH-MTD001P
                          ORGANIZATION IS      INDEXED
                          ACCESS MODE  IS      DYNAMIC
                          LOCK MODE    IS    AUTOMATIC
                          WITH LOCK    ON       RECORD
                          STATUS       IS   ST-MTD001P
                          RECORD KEY IS CHAVE-MT01P  =
                                        CONTRATO-MT01P
                                         PRODUTO-MT01P
                                          MODELO-MT01P.
