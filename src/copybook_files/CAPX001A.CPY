           SELECT CAD001A ASSIGN "\PROGRAMA\KELLO\PADRAO\CAD001A"
                          ORGANIZATION IS           INDEXED
                          ACCESS MODE  IS           DYNAMIC
                          LOCK MODE    IS         AUTOMATIC
                          WITH LOCK    ON            RECORD
                          STATUS       IS        ST-CAD001A
                          RECORD KEY   IS    CHAVE-CA001A =
                                             EMPRESA-CA001A
                                         COD-USUARIO-CA001A
                                                  HD-CA001A.
