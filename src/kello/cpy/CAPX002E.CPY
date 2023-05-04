           SELECT CAD002E ASSIGN TO "\PROGRAMA\KELLO\PADRAO\CAD002E"
                          ORGANIZATION IS      INDEXED
                          ACCESS MODE  IS      DYNAMIC
                          LOCK MODE    IS    AUTOMATIC
                          WITH LOCK    ON       RECORD
                          STATUS       IS   ST-CAD002E
                          RECORD KEY   IS
                          CHAVE-CA002E = EMPRESA-CA002E
                                     COD-USUARIO-CA002E
                          ALTERNATE    RECORD KEY IS
                          CHAVE-USU-CA002E =
                                     COD-USUARIO-CA002E
                                         EMPRESA-CA002E.
