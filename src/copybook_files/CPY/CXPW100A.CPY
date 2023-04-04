       FD  CXD100.
      *    LABEL RECORD IS STANDARD
      *    VALUE OF FILE-ID IS "\TESTE\CXD100".
       01  REG-CXD100.
           05  DATA-MOV-CX100      PIC 9(08).
           05  SEQ-CX100           PIC 9(4).
           05  HISTORICO-CX100     PIC X(30).
           05  DOCUMENTO-CX100     PIC X(10).
           05  VALOR-CX100         PIC 9(8)V99.
           05  ES-CX100            PIC X.
           05  CONTAPART-CX100     PIC 9(5).
           05  CONTA-REDUZ-CX100   PIC 9(3).
           05  DIGITADOR-CX100     PIC X(5).
           05  TIPO-LCTO-CX100     PIC 9.
      * TIPO-LCTO-CX100 -> TIPO DE CONTA QUE FAZ A LIGAÇÃO P/ATUALIZAÇÃO
      *                    1-NORMAL 2-RECEBIM 3-PGTO 4-BAIXA-BRINDE)
