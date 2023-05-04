      * Arquivo de cadastro de conta de resultado (caixa)
       FD  CXD020.
       01  REG-CXD020.
           05  CODIGO-COMPL-CX20   PIC 9(07).
           05  CODIGO-REDUZ-CX20   PIC 9(5).
           05  GRAU-CX20           PIC 9.
           05  DESCRICAO-CX20      PIC X(30).
           05  TIPO-CONTA-CX20     PIC 9.
      *  TIPO-CONTA-CX20 = 1 ==> CONTA TOTALIZADORA
      *  TIPO-CONTA-CX20 = 0 ==> CONTA NORMAL

