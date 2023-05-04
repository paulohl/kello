      *  Cadastro geral de clientes - arquivo de bancos
      *  TIPO-DE-CONTA-CG16 -> 1 Conta Corrente, 2 - Poupança
       FD  CGD016.
       01  REG-CGD016.
           05  CHAVE-CG16.
               10  CODIGO-CG16     PIC 9(9).
               10  BANCO-CG16      PIC 9(4).
               10  AGENCIA-CG16    PIC X(9).
               10  NR-CONTA-CG16   PIC X(15).
           05  USUARIO-CG16        PIC 9(02).
           05  DESC-USUARIO-CG16   PIC X(04).
           05  TITULAR-CONTA-CG16  PIC X(40).
           05  CPF-TITULAR-CG16    PIC 9(11).
           05  TIPO-DE-CONTA-CG16  PIC 9(01).
           05  FILLER              PIC X(46).
