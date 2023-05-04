           SELECT CRD013 ASSIGN TO PATH-CRD013
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD013
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR13 = DATA-MOVTO-CR13 SEQ-CR13
                  ALTERNATE RECORD KEY IS DATA-ENTRADA-CR13
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-RECTO-CR13
                            WITH DUPLICATES.
