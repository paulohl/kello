      *  Cadastro geral de clientes - arquivo de cartoes
       FD  CGD015.
       01  REG-CGD015.
           05  CHAVE-CG15.
               10  CODIGO-CG15            PIC 9(9).
               10  CODIGO-CARTAO-CG15     PIC 9(2).
               10  NUMERO-CARTAO-CG15     PIC 9(16).
               10  VALIDACAO-NUMERO-CG15  PIC 9(04).
               10  DATA-LIMITE-CG15       PIC 9(06).
           05  USUARIO-CG15               PIC 9(02).
           05  DESC-USUARIO-CG15          PIC X(04).
           05  TITULAR-CONTA-CG15         PIC X(40).
           05  CPF-TITULAR-CG15           PIC 9(11).
           05  FILLER                     PIC X(47).
