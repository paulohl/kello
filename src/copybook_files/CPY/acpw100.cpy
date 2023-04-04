      *  Movimento de atendimento a cliente
       FD  ACD100.
       01  REG-ACD100.
           05 NR-ATENDIMENTO-AC100      PIC 9(08).
           05 ULTIMO-ATENDENTE-AC100    PIC X(05).
           05 DATA-ABERTURA-AC100       PIC 9(08).
           05 HORA-ABERTURA-AC100       PIC X(10).
           05 DATA-ENCERRAMENTO-AC100   PIC 9(08).
           05 HORA-ENCERRAMENTO-AC100   PIC X(10).
           05 ATENDIMENTO1-AC100        PIC 9(06).
           05 ATENDIMENTO2-AC100        PIC 9(06).
           05 ADICIONAL1-AC100          PIC 9(06).
           05 ADICIONAL2-AC100          PIC 9(06).
           05 ADICIONAL3-AC100          PIC 9(06).
           05 STATUS-ATUAL-AC100        PIC X(15).
           05 CLIENTE-AC100             PIC 9(08).
           05 FILLER                    PIC X(100).
