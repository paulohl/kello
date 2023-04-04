           SELECT RED103 ASSIGN TO PATH-RED103
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RED103
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-R103 = DOCTO-R103 CODIGO-R103
                         FUNCAO-R103
                  ALTERNATE RECORD KEY IS ALT-R103 = CODIGO-R103
                            FUNCAO-R103 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-R103 = DOCTO-R103
                            SEQ-R103.
