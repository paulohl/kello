           SELECT CCD011 ASSIGN TO PATH-CCD011
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CCD011
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CC11 = ANOMES-MVTO-CC11
                                             FORNEC-CC11
                  ALTERNATE RECORD KEY IS ALT-CC11 =
                            FORNEC-CC11 ANOMES-MVTO-CC11.
