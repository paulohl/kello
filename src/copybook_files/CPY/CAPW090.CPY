      *  Cadastro de Compromissos contratuais(brindes) e eventos
       FD  CAD090.
       01  REG-CAD090.
           05  CODIGO-090        PIC 999.
           05  NOME-090          PIC X(20).
           05  VALOR-090         PIC 9(9)V99.
           05  BRIN-EVEN-090     PIC 9.
           05  MULT-FORM-090     PIC X.
           05  EVENTO-PRE-090    PIC 9.
      ***** MULT-FORM  = 1--> BRINDE    2--> EVENTO ******
      ***** EVENTO-PRE = 1--> EVENTO    2--> PRE-EVENTO **
