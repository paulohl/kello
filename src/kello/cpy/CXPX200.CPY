           SELECT CXD200 ASSIGN TO PATH-CXD200
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  STATUS IS ST-CXD200
                  RECORD KEY IS CHAVE-CX200 = DATA-OCORRENCIA-CX200
                                              SEQ-CX200
                  ALTERNATE RECORD KEY IS ALT1-CX200 =
                     SITUACAO-ANOTACAO-CX200 DATA-OCORRENCIA-CX200
                       WITH DUPLICATES.
