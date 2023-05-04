      *  Cadastro geral de fornecedores - arquivo de bancos
      *  tipo-de-conta-cg06 -> 1 - Conta Corrente, 2 - Poupança
      *  preferencial-cg06 -> 1 conta preferencial p/ deposito

       FD  CGD006.
       01  REG-CGD006.
           05  CHAVE-CG06.
               10  CODIGO-CG06        PIC 9(6).
               10  BANCO-CG06         PIC 9(4).
               10  AGENCIA-CG06       PIC X(9).
               10  NR-CONTA-CG06      PIC X(15).
           05  USUARIO-CG06           PIC 9(03).
           05  DESC-USUARIO-CG06      PIC X(04).
           05  TITULAR-CONTA-CG06     PIC X(40).
           05  CPF-TITULAR-CG06       PIC 9(11).
           05  TIPO-DE-CONTA-CG06     PIC 9(01).
           05  COMPLEMENTO-CONTA-CG06 PIC X(11).
           05  PREFERENCIAL-CG06      PIC 9(01).
           05  CNPJ-TITULAR-CG06      PIC 9(14).
           05  FILLER                 PIC X(23).
