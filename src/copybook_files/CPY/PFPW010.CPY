      * Arquivo de controle de programações financeiras
       FD  PFD010.
       01  REG-PFD010.
           05  DATA-PROGR-PF10     PIC 9(8).
           05  SEQ-PF10            PIC 9(3).
           05  FORNECEDOR-PF10     PIC 9(6).
           05  DESCRICAO-PF10      PIC X(30).
           05  COND-PGTO-PF10      PIC X(20).
           05  DATA-ENTREGA-PF10   PIC 9(8).
           05  VALOR-PF10          PIC 9(8)V99.
           05  USUARIO-PF10        PIC 9(3).
      *  USUARIO-PF10 - automático do cod-usuario-w.
           05  CONTA-APURAC-PF10   PIC 9(5).
           05  DATA-MOV-PAGAR-PF10 PIC 9(8).
           05  APROVADO-POR-PF10   PIC 9(3).
           05  DATA-APROVACAO-PF10 PIC 9(8).
      *  APROVADO-POR - usuario responsável pela aprovação, será preen-
      *  chido qdo o responsável liberar.
           05  OBS-PF10            PIC X(80).
