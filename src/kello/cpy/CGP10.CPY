           SELECT CGD010 ASSIGN TO PATH-CGD010
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS COD-COMPL-CG10
                  STATUS IS ST-CGD010
                  ALTERNATE RECORD KEY IS COMPRADOR-CG10
                            WITH DUPLICATES.
      *  Cadastro DE CLIENTE - arquivo simples
       FD  CGD010.
       01  REG-CGD010.
           05  COD-COMPL-CG10.
               10  CLASSIF-CG10        PIC 9.
      *    TIPO 0-CONTRATO   1-COMUM
               10  CODIGO-CG10         PIC 9(8).
      *    CODIGO, quando se tratar de tipo contrato o código a ser
      *    considerado será do tipo contrato/album(9999-9999)
           05  COMPRADOR-CG10      PIC X(30).
