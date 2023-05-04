      *   Cadastro de eventos
       FD  COD003.
       01  REG-COD003.
           05  CODIGO-CO03       PIC 9(5).
           05  NOME-CO03         PIC X(20).
           05  EVENTO-PRE-CO03   PIC 9.
           05  RELAT-ESTAT-CO03  PIC 9.
      *    1-EVENTO  2-PRE-EVENTO
      *    RELAT-ESTAT-CO03 => 1 ENTRA, 2 NÃO ENTRA
