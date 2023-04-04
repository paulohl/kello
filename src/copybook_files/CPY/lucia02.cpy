           SELECT  COD002 ASSIGN TO PATH-COD002
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   STATUS IS ST-COD002
                   RECORD KEY IS CODIGO-CO02
                   ALTERNATE RECORD KEY IS NOME-CO02 WITH DUPLICATES.
      * CADASTRO DE BRINDE
       FD  COD002.
       01  REG-COD002.
           05  CODIGO-CO02       PIC 999.
           05  NOME-CO02         PIC X(20).
           05  VALOR-CO02        PIC 9(8)V99.
           05  MULT-FORM-CO02    PIC 9.
      ***** MULT-FORM  = 1--> SIM  2-NÃO             *****
           05  GERAR-PAGAR-CO02  PIC 9.
      *    GERAR-PAGAR-CO02 = 1-SIM  2-NAO
           05  FORNEC1-CO02      PIC 9(6).
           05  FORNEC2-CO02      PIC 9(6).
           05  FORNEC3-CO02      PIC 9(6).
