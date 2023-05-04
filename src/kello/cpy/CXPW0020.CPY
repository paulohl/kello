       FD  CXD0020
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 42 CHARACTERS
           VALUE OF FILE-ID IS CX020.
       01  REG-CXD0020.
           05  CODIGO-CX20         PIC 9(07).
           05  CODIGO-REDUZ-CX20   PIC 9(3).
           05  GRAU-CX20           PIC 9.
           05  DESC-CX20           PIC X(30).
           05  TIPO-CONTA-CX20     PIC 9.
      *  TIPO-CONTA-CX20 = 1 ==> CONTA TOTALIZADORA
      *  TIPO-CONTA-CX20 = 0 ==> CONTA NORMAL
