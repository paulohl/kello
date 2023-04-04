           SELECT COD045 ASSIGN       TO  PATH-COD045
                         ORGANIZATION IS      INDEXED
                         ACCESS MODE  IS      DYNAMIC
                         LOCK MODE    IS    AUTOMATIC
                         WITH LOCK    ON       RECORD
                         RECORD KEY   IS CHAVE-CO45 =
                                     NR-CONTRATO-CO45
                                         PRODUTO-CO45
                                          MODELO-CO45
                         ALTERNATE    RECORD      KEY
                         IS          CHAVE-PRO-CO45 =
                                         PRODUTO-CO45
                                          MODELO-CO45
                                     NR-CONTRATO-CO45
                         STATUS       IS    ST-COD045.
