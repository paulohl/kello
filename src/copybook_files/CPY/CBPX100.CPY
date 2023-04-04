           SELECT CBD100 ASSIGN TO PATH-CBD100
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CBD100
                  RECORD KEY IS CHAVE-CB100 =
                          CODIGO-FORN-CB100 NR-CHEQUE-CB100
                  ALTERNATE RECORD KEY IS ALT1-CB100 = CODIGO-FORN-CB100
                          SITUACAO-CB100 NR-CHEQUE-CB100
                  ALTERNATE RECORD KEY IS ALT2-CB100 = NOMINAL-A-CB100
                     SEQ-CTA-PAGAR-CB100 WITH DUPLICATES.
