      * Complemento do arquivo de CLIENTE (entregas)
       FD  CGD013.
       01  REG-CGD013.
           05  COD-COMPL-CG13.
               10  CLASSIF-CG13    PIC 9.
      *     CLASSIF = 0-CONTRATO  1-COMUM
               10  CODIGO-CG13     PIC 9(8).
           05  ENDERECO-CG13       PIC X(30).
           05  BAIRRO-CG13         PIC X(15).
           05  CEP-CG13            PIC 9(08).
           05  CIDADE-CG13         PIC 9(04).
           05  FILLER              PIC X(30).
