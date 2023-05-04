          SELECT CHD010B ASSIGN       TO PATH-CHD010B
                         ORGANIZATION IS      INDEXED
                         ACCESS MODE  IS      DYNAMIC
                         STATUS       IS   ST-CHD010B
                         LOCK MODE    IS    AUTOMATIC
                         WITH LOCK    ON       RECORD
                         RECORD KEY  IS CHAVE-CH10B =
                                     DATA-MOVTO-CH10B
                                            SEQ-CH10B
                                      DATA-RCTO-CH10B
                         ALTERNATE RECORD KEY IS
                                         ALT6-CH10B =
                                      DATA-RCTO-CH10B
                                      SEQ-CAIXA-CH10B
                         WITH              DUPLICATES.
