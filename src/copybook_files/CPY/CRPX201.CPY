          SELECT CRD201 ASSIGN TO PATH-CRD201
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD201
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR201 = COD-COMPL-CR201 SEQ-CR201
                                              SUBSEQ-CR201.
