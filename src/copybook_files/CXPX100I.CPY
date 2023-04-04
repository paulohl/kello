           SELECT CXD100I ASSIGN       TO PATH-CXD100I
                          ORGANIZATION IS      INDEXED
                          ACCESS MODE  IS      DYNAMIC
                          STATUS       IS   ST-CXD100I
                          LOCK MODE    IS    AUTOMATIC
                          WITH LOCK    ON       RECORD
                          RECORD KEY IS CHAVE-CX100I =
                                       DATA-MOV-CX100I,
                                            SEQ-CX100I
                                         IMAGEM-CX100I.
