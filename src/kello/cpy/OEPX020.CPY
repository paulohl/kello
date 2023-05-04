           SELECT  OED020 ASSIGN TO PATH-OED020
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-OED020
                   LOCK MODE IS AUTOMATIC
                   WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-OE20 =
                           NR-CONTRATO-OE20 ITEM-OE20
                   ALTERNATE RECORD KEY IS DATA-VENCTO-OE20
                             WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ALT1-OE20 =  NR-CONTRATO-OE20
                        REALIZADO-OE20 DATA-VENCTO-OE20 ITEM-OE20.
