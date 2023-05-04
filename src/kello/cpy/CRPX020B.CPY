          SELECT CRD020B ASSIGN       TO PATH-CRD020B
                         ORGANIZATION IS      INDEXED
                         ACCESS MODE  IS      DYNAMIC
                         STATUS       IS   ST-CRD020B
                         LOCK MODE    IS    AUTOMATIC
                         WITH LOCK    ON       RECORD
                         RECORD KEY  IS CHAVE-CR20B =
                                      COD-COMPL-CR20B
                                            SEQ-CR20B
                                      DATA-RCTO-CR20B
                         ALTERNATE RECORD KEY IS
                                         ALT6-CR20B =
                                      DATA-RCTO-CR20B
                                      SEQ-CAIXA-CR20B
                         WITH              DUPLICATES.
