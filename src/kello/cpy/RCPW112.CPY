      *ARQUIVO DOS ITENS DAS FAIXA DE VALORES
       FD  RCD112.
       01  REG-RCD112.
           05 CHAVE-RC112.
              10 CODIGO-RC112                     PIC 9(06).
              10 FAIXA-RC112                      PIC 9(02).
              10 PRODUTO-RC112                    PIC 9(04).
              10 FILLER                           PIC X(16).
              10 QTDE-INICIAL-RC112               PIC 9(06).
           05 QTDE-FINAL-RC112                    PIC 9(06).
           05 DESC-PERC-RC112                     PIC 9(03)V99.
           05 DESC-VLR-RC112                      PIC 9(06)V99.
           05 FILLER                              PIC X(50).

