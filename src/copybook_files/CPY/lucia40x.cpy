           SELECT COD040X ASSIGN TO PATH-COD040X
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-COD040X
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS SEQ-CO40X
                  ALTERNATE RECORD KEY IS ALT-CO40X = CIDADE-CO40X
                    IDENTIFICACAO-CO40X INSTITUICAO-CO40X
                    WITH DUPLICATES.
      * arquivo p/ consulta popup-up no extrato de contrato
       FD  COD040X.
       01  REG-COD040X.
           05  SEQ-CO40X           PIC 9(4).
           05  CIDADE-CO40X        PIC X(20).
           05  NR-CONTRATO-CO40X   PIC 9(4).
           05  IDENTIFICACAO-CO40X PIC X(10).
           05  INSTITUICAO-CO40X   PIC X(10).
           05  MESANO-CO40X        PIC 9(6).
