           SELECT RCD100 ASSIGN TO PATH-RCD100
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-RCD100
                  RECORD KEY IS CHAVE-ALBUM-REC
                  ALTERNATE RECORD KEY IS ALT-REC = DATA-MOVTO-REC
                                          CHAVE-ALBUM-REC
                  ALTERNATE RECORD KEY IS DATAVEN-REC WITH DUPLICATES.
