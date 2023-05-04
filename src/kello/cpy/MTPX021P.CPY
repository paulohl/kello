           SELECT  MTD021P ASSIGN       TO PATH-MTD021P
                           ORGANIZATION IS      INDEXED
                           ACCESS MODE  IS      DYNAMIC
                           LOCK   MODE  IS    AUTOMATIC
                           WITH   LOCK  ON       RECORD
                           STATUS       IS   ST-MTD021P
                           RECORD KEY   IS  CHAVE-MT21P
                           ALTERNATE    RECORD KEY   IS
                           DATA-PREVISTA-MT21P
                           WITH              DUPLICATES
                           ALTERNATE    RECORD KEY   IS
                                   CHAVE-STATUS-MT21P =
                                           STATUS-MT21P
                                         CONTRATO-MT21P
                                            ALBUM-MT21P
                                              SEQ-MT21P
                                          PRODUTO-MT21P
                                           MODELO-MT21P
                           ALTERNATE    RECORD KEY   IS
                               CHAVE-FORNECEDOR-MT21P =
                                       FORNECEDOR-MT21P
                                         CONTRATO-MT21P
                                            ALBUM-MT21P
                                              SEQ-MT21P
                                          PRODUTO-MT21P
                                           MODELO-MT21P.
