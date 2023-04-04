           SELECT  COD050 ASSIGN        TO              PATH-COD050
                          ORGANIZATION  IS                  INDEXED
                          ACCESS MODE   IS                  DYNAMIC
                          STATUS        IS                ST-COD050
                          LOCK MODE     IS                AUTOMATIC
                          WITH LOCK     ON                   RECORD
                          RECORD KEY    IS             CHAVE-CO50 =
                                         NR-CONTRATO-CO50 ITEM-CO50
                          ALTERNATE RECORD KEY IS  DATA-VENCTO-CO50
                                    WITH DUPLICATES
                          ALTERNATE RECORD KEY IS       ALT1-CO50 =
                                                   NR-CONTRATO-CO50
                          REALIZADO-CO50 DATA-VENCTO-CO50 ITEM-CO50
                          ALTERNATE RECORD KEY                   IS
                               DATA-PAGTO-CO50 WITH      DUPLICATES
                          ALTERNATE RECORD KEY                   IS
                               CHAVE-ENVIO-CO50 =   DATA-ENVIO-CO50
                                                   NR-CONTRATO-CO50
                                                          ITEM-CO50.
