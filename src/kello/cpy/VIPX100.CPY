           SELECT VID100 ASSIGN TO PATH-VID100
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-VID100
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-V100 = DATA-MOVTO-V100 SEQ-V100
                  ALTERNATE RECORD KEY IS ALT-V100 = NR-FITAS-V100
                           DATA-EVENTO-V100 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-V100 = CONTRATO-V100
                            DATA-EVENTO-V100 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-V100 = CINEGRAFISTA-V100
                            DATA-EVENTO-V100 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-V100 = DATA-EVENTO-V100
                            DATA-MOVTO-V100 SEQ-V100
                  ALTERNATE RECORD KEY IS ALT4-V100 =
                            IDENTIFICADOR-V100
                            DATA-EVENTO-V100
                            WITH DUPLICATES.
