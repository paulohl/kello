           SELECT  COD055 ASSIGN        TO              PATH-COD055
                          ORGANIZATION  IS                  INDEXED
                          ACCESS MODE   IS                  DYNAMIC
                          STATUS        IS                ST-COD055
                          LOCK MODE     IS                AUTOMATIC
                          WITH LOCK     ON                   RECORD
                          RECORD KEY    IS             CHAVE-CO55 =
                                         NR-CONTRATO-CO55 ITEM-CO55
                          ALTERNATE RECORD KEY IS  DATA-VENCTO-CO55
                                    WITH DUPLICATES
                          ALTERNATE RECORD KEY IS       ALT1-CO55 =
                                                   NR-CONTRATO-CO55
                          REALIZADO-CO55 DATA-VENCTO-CO55 ITEM-CO55
                          ALTERNATE RECORD KEY                   IS
                               DATA-PAGTO-CO55 WITH      DUPLICATES
                          ALTERNATE RECORD KEY                   IS
                               CHAVE-ENVIO-CO55 =   DATA-ENVIO-CO55
                                                   NR-CONTRATO-CO55
                                                          ITEM-CO55.
