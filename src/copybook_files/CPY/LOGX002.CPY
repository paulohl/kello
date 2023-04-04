           SELECT LOG002 ASSIGN TO PATH-LOG002
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS LOG2-CHAVE =
                  LOG2-USUARIO
                  LOG2-PERIODO
                  ALTERNATE RECORD KEY IS
                  LOG2-CH-OPERACAO =
                      LOG2-OPERACAO
                      LOG2-PERIODO
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  LOG2-CH-ARQUIVO =
                      LOG2-ARQUIVO
                      LOG2-PERIODO
                  WITH DUPLICATES
                  STATUS IS ST-LOG002.
