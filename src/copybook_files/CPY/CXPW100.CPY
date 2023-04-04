      * Arquivo de movimento de caixa
       FD  CXD100.
       01  REG-CXD100.
           05  SEQ-CX100              PIC 9(04).
           05  DATA-MOV-CX100         PIC 9(08).
           05  TIPO-LCTO-CX100        PIC 99.
      * Tipo-lcto < 50 - saídas     e tipo-lcto >= 50 - entradas
           05  HISTORICO-CX100        PIC X(30).
           05  DOCUMENTO-CX100        PIC X(10).
           05  VALOR-CX100            PIC 9(10)V99.
           05  CONTAPART-CX100        PIC 9(06).
           05  CONTA-REDUZ-CX100      PIC 9(05).
      *    05  CONTABIL-CX100         PIC 9.
      * será usado no futuro - para informar se o lancto é contábil
           05  CONTROLE-LOTE-CX100    PIC 9.
           05  RESPONSAVEL-CX100      PIC X(5).
           05  SEQ-DESM-CX100         PIC 9(4).
      * seq.p/determinar qual lançamento de caixa que gerou esse desmem-
      * bramento. Ex. Pagto de uma conta desdobrada, vai existir um
      * lançamento principal com o total, e outros com o desdobramento
      * sendo a seq-desm a mesma seq. que o lançamento principal, outro
      * exemplo o pagto de uma conta com juro, multa
      * CONTROLE-LOTE-CX100 foi colocado no lugar do CONTABIL-CX100
