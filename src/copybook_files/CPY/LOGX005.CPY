           SELECT LOG005 ASSIGN TO PATH-LOG005
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS LOG5-CHAVE =
                  LOG5-USUARIO
                  LOG5-PERIODO
                  ALTERNATE RECORD KEY IS
                  LOG5-CH-OPERACAO =
                      LOG5-OPERACAO
                      LOG5-PERIODO
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  LOG5-CH-ARQUIVO =
                      LOG5-ARQUIVO
                      LOG5-PERIODO
                  WITH DUPLICATES
                  STATUS IS ST-LOG005.
