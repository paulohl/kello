           SELECT COD041 ASSIGN TO PATH-COD041
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CO41 = NR-CONTRATO-CO41
                                             CURSO-CO41 TURMA-CO41
                  STATUS IS ST-COD041.
