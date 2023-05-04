      *  arquivo de contatos do contrato - cabecalho
       FD  COD500.
       01  REG-COD500.
           05  NR-CONTRATO-CO500  PIC 9(4).
           05  ITEM-CO500         PIC 9(3).
           05  DATA-CO500         PIC 9(8).
           05  HORA-CO500         PIC 9(4).
           05  USUARIO-CO500      PIC X(5).
           05  DATA-AGENDA-CO500  PIC 9(8).
      *    DATA-AGENDA = AAAA/MM/DD
           05  SITUACAO-CO500     PIC 9.
      *    0-PENDENTE   1-CHECADO
