          SELECT CRD099 ASSIGN TO PATH-CRD099
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD099
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR99 = CLASS-CLIENTE-CR99
                        CLIENTE-CR99 SEQ-CR99
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR99
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CR99 =
                     CLASS-CLIENTE-CR99 CLIENTE-CR99
                     DATA-VENCTO-CR99 WITH DUPLICATES.
