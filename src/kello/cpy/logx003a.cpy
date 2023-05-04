           SELECT LOG003a ASSIGN TO PATH-LOG003a
                          ORGANIZATION IS INDEXED
                          ACCESS MODE IS DYNAMIC
                          LOCK MODE IS AUTOMATIC
                          WITH LOCK ON RECORD
                          RECORD KEY IS LOG3a-CHAVE =
                          LOG3a-USUARIO
                          LOG3a-PERIODO
                          ALTERNATE RECORD KEY IS
                          LOG3a-CH-OPERACAO =
                              LOG3a-OPERACAO
                              LOG3a-PERIODO
                          WITH DUPLICATES
                          ALTERNATE RECORD KEY IS
                          LOG3a-CH-ARQUIVO =
                              LOG3a-ARQUIVO
                              LOG3a-PERIODO
                          WITH DUPLICATES
                          ALTERNATE RECORD KEY IS
                          LOG3a-CH-REGISTRO =
                              LOG3a-ARQUIVO
                              LOG3a-REGISTRO
                          WITH DUPLICATES
                          STATUS IS ST-LOG003a.
