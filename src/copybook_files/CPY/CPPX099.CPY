          SELECT CPD099 ASSIGN TO PATH-CPD099
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CPD099
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CP99 = FORNEC-CP99 SEQ-CP99
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CP99
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CP99 =
                     FORNEC-CP99 DATA-VENCTO-CP99 WITH DUPLICATES.
