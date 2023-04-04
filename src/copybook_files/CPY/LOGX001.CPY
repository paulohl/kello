           SELECT LOG001 ASSIGN TO PATH-LOG001
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS LOG1-CHAVE =
                  LOG1-USUARIO
                  LOG1-PERIODO
                  ALTERNATE RECORD KEY IS
                  LOG1-CH-OPERACAO =
                      LOG1-OPERACAO
                      LOG1-PERIODO
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  LOG1-CH-ARQUIVO =
                      LOG1-ARQUIVO
                      LOG1-PERIODO
                  WITH DUPLICATES
                  STATUS IS ST-LOG001.
