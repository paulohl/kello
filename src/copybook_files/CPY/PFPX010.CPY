           SELECT PFD010 ASSIGN TO PATH-PFD010
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-PFD010
                  RECORD KEY IS CHAVE-PF10 = DATA-PROGR-PF10
                         SEQ-PF10
                  ALTERNATE RECORD KEY IS ALT1-PF10 = FORNECEDOR-PF10
                        DATA-MOV-PAGAR-PF10 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-MOV-PAGAR-PF10
                        WITH DUPLICATES.
