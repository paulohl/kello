       FD  RCD100P.
       01  REG-RCD100P.
           05  CHAVE-ALBUM-RECP.
               10  ALBUM-RECP       PIC 9(08).
               10  SEQ-RECP         PIC 9(03).
           05  PRODUTO-RECP         PIC 9(04).
           05  MODELO-RECP          PIC 9(04).
           05  QTDE-PLANILHADA-RECP PIC 9(06).
           05  QTDE-VENDIDA-RECP    PIC 9(06).
           05  VLRUNI-RECP          PIC 9(06)V99.
           05  VLRTOTAL-RECP        PIC 9(06)V99.
           04  FILLER               PIC X(50).
