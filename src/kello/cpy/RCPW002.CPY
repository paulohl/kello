      *VALOR DA PERCENTAGEM DA COMISSÃO DO VENDEDOR
       FD  RCD002.
       01  REG-RCD002.
           05 CHAVE-RC02.
              10 TIPO-RC02                        PIC X(15).
              10 CODIGO-COMIS-RC02                PIC 999.
           05  COMIS-0-30-DIAS                    PIC 99V999.
           05  COMIS-31-120-DIAS                  PIC 99V999.
           05  COMIS-121-240-DIAS                 PIC 99V999.
           05  COMIS-241-000-DIAS                 PIC 99V999.
           05  FILLER                             PIC X(30).

