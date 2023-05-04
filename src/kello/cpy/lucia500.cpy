           SELECT  COD500 ASSIGN TO PATH-COD500
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD500
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO500 =
                      NR-CONTRATO-CO500 ITEM-CO500
                   ALTERNATE RECORD KEY IS ALT2-CO500 =
                      DATA-AGENDA-CO500 USUARIO-CO500 WITH DUPLICATES.
      *  arquivo de contatos do contrato - cabecalho
       FD  COD500.
       01  REG-COD500.
           05  NR-CONTRATO-CO500  PIC 9(4).
           05  ITEM-CO500         PIC 9(2).
           05  DATA-CO500         PIC 9(8).
           05  HORA-CO500         PIC 9(4).
           05  USUARIO-CO500      PIC X(5).
           05  DATA-AGENDA-CO500  PIC 9(8).
      *    DATA-AGENDA = AAAA/MM/DD
           05  SITUACAO-CO500     PIC 9.
      *    0-PENDENTE   1-CHECADO
