           SELECT RCD101 ASSIGN TO PATH-RCD101
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RCD101
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-REC1 =
                         CHAVE-ALBUM-REC1 VENCTO-REC1
                         BANCO-REC1 NUMERO-REC1 PARCELA-REC1
                  ALTERNATE RECORD KEY IS
                  CHAVE-ALBUM-REC1 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-REC1 = DTA-BAIXA-REC1
                            CHAVE-ALBUM-REC1 WITH DUPLICATES.
