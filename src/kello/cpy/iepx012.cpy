       SELECT IED012 ASSIGN TO PATH-IED012
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              LOCK MODE IS MANUAL WITH LOCK ON RECORD
              STATUS IS ST-IED012
              RECORD KEY IS CHAVE-IE12 = COD-INSTITUICAO-IE12,
                     COD-CURSO-IE12.