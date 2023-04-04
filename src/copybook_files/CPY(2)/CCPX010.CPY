           SELECT CCD010 ASSIGN TO PATH-CCD010
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CCD010
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CC10 = ANOMES-VCTO-CC10
                                             FORNEC-CC10
                  ALTERNATE RECORD KEY IS ALT-CC10 =
                            FORNEC-CC10 ANOMES-VCTO-CC10.
