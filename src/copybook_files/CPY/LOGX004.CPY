           SELECT LOG004 ASSIGN TO PATH-LOG004
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS LOG4-CHAVE =
                  LOG4-USUARIO
                  LOG4-PERIODO
                  ALTERNATE RECORD KEY IS
                  LOG4-CH-OPERACAO =
                      LOG4-OPERACAO
                      LOG4-PERIODO
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  LOG4-CH-ARQUIVO =
                      LOG4-ARQUIVO
                      LOG4-PERIODO
                  WITH DUPLICATES
                  STATUS IS ST-LOG004.
