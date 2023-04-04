      * Arquivo de parametrizacao de codigos reduzidos
      * (continuacao PAR001)

       FD  CPAR001.
       01  REG-CPAR001.
           05  CHAVE-CPAR001                 PIC 9(01).
           05  CAMINHO-IMAGEM-CX-CPAR001     PIC X(50).
           05  TABELA-COLACAO OCCURS 30 TIMES.
               10 TAB-COLACAO                PIC 9(05).
           05  FILLER                        PIC X(510).
