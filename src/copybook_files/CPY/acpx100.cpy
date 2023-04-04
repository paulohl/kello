           SELECT ACD100 ASSIGN TO PATH-ACD100
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-ACD100
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-AC100 = NR-ATENDIMENTO-AC100
                  ALTERNATE RECORD KEY IS CLI-AC100 = CLIENTE-AC100,
                                          DATA-ABERTURA-AC100
                                          WITH DUPLICATES                                              
                  ALTERNATE RECORD KEY IS ALT-AC100 =
                                          DATA-ABERTURA-AC100
                                          WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-AC100 =
                                          STATUS-ATUAL-AC100
                                          DATA-ABERTURA-AC100
                                          WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-AC100 = 
                                          DATA-ENCERRAMENTO-AC100
                                          WITH DUPLICATES.
