      *  Cadastro geral de clientes - arquivo de recibos
      *  escaneados
       FD  CGD017.
       01  REG-CGD017.
           05  CHAVE-CG17.
               10  CLIENTE-CG17     PIC 9(9).
           05  USUARIO-CG17         PIC 9(02).
           05  DESC-USUARIO-CG17    PIC X(05).
           05  CAMINHO1-CG17        PIC X(100).
           05  CAMINHO2-CG17        PIC X(100).
           05  FILLER               PIC X(100).
