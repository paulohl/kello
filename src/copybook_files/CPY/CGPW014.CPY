      * Complemento do arquivo de CLIENTE (avalista)
       FD  CGD014.
       01  REG-CGD014.
           05  COD-COMPL-CG14.
               10  CLASSIF-CG14    PIC 9.
      *     CLASSIF = 0-CONTRATO  1-COMUM
               10  CODIGO-CG14     PIC 9(8).
           05  NOME-CG14           PIC X(40).
           05  ENDERECO-CG14       PIC X(30).
           05  BAIRRO-CG14         PIC X(15).
           05  CEP-CG14            PIC 9(08).
           05  CIDADE-CG14         PIC 9(04).
           05  TELEFONE-CG14       PIC 9(08).
           05  CPF-CG14            PIC 9(14).
           05  RG-CG14             PIC X(15).
           05  COMP-TEL-AVAL-CG14  PIC 9(01).
           05  FILLER              PIC X(29).
