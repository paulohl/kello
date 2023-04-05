           SELECT CAD010 ASSIGN TO PATH-CAD010
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CAD010
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CIDADE
                  ALTERNATE RECORD KEY IS NOME-CID
                  ALTERNATE RECORD KEY IS NOME-COMPL-CID.
