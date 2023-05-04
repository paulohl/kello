      * CADASTRO DE CUSTEIOS
       FD  COD009.
       01  REG-COD009.
           05  CODIGO-CO09       PIC 99999.
           05  NOME-CO09         PIC X(20).
           05  VALOR-CO09        PIC 9(8)V99.
           05  MULT-FORM-CO09    PIC 9.
      ***** MULT-FORM  = 1--> SIM  2-NÃO             *****
           05  GERAR-PAGAR-CO09  PIC 9.
      *    GERAR-PAGAR-CO09 = 1-SIM  2-NAO
           05  FORNEC1-CO09      PIC 9(6).
           05  FORNEC2-CO09      PIC 9(6).
           05  FORNEC3-CO09      PIC 9(6).
      *SITUACAO-CO09 => 1 ATIVO , 2 INATIVO
           05  SITUACAO-CO09     PIC 9(1).
           05  FILLER            PIC X(100).
