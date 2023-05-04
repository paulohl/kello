           SELECT CHD013 ASSIGN TO PATH-CHD013
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CHD013
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH13 = DATA-MOVTO-CH13 SEQ-CH13
                  ALTERNATE RECORD KEY IS DATA-ENTRADA-CH13
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-RECTO-CH13
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-APRES-CH13
                            WITH DUPLICATES.
