           SELECT LOG003 ASSIGN TO PATH-LOG003
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS LOG3-CHAVE =
                  LOG3-USUARIO
                  LOG3-PERIODO
                  ALTERNATE RECORD KEY IS
                  LOG3-CH-OPERACAO =
                      LOG3-OPERACAO
                      LOG3-PERIODO
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  LOG3-CH-ARQUIVO =
                      LOG3-ARQUIVO
                      LOG3-PERIODO
                  WITH DUPLICATES
                  STATUS IS ST-LOG003.
