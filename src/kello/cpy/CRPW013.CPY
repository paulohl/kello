      *    arquivo de duplicatas em cobranca (COMPLEMENTO CRD020)
      *    TODAS AS DATAS SÃO INVERTIDAS   AAAAMMDD
       FD  CRD013.
       01  REG-CRD013.
           05  DATA-MOVTO-CR13            PIC 9(8).
           05  SEQ-CR13                   PIC 9(4).
           05  DATA-ENTRADA-CR13          PIC 9(8)      COMP-3.
           05  DATA-COMPRA-CR13           PIC 9(8)      COMP-3.
           05  DATA-RECTO-CR13            PIC 9(8)      COMP-3.
           05  VLR-JUROS-CR13             PIC 9(6)V99   COMP-3.
           05  VLR-MULTA-CR13             PIC 9(6)V99   COMP-3.
           05  VLR-DESCONTO-CR13          PIC 9(6)V99   COMP-3.
           05  FORMA-PAGTO-CR13           PIC X(10).
           05  DCR-MEM-CR13               PIC X(15).
           05  DCR-MEM-R-CR13             PIC X(15).
           05  STATUS-CR13                PIC X(12).
           05  FILLER                     PIC X(30).
