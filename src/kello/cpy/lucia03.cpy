           SELECT  COD003 ASSIGN TO PATH-COD003
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   STATUS IS ST-COD003
                   RECORD KEY IS CODIGO-CO03
                   ALTERNATE RECORD KEY IS NOME-CO03 WITH DUPLICATES.
      *   Cadastro de eventos
       FD  COD003.
       01  REG-COD003.
           05  CODIGO-CO03       PIC 999.
           05  NOME-CO03         PIC X(20).
           05  EVENTO-PRE-CO03   PIC 9.
      *    1-EVENTO  2-PRE-EVENTO
