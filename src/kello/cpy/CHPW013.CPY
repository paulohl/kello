      *    arquivo de cheques devolvidos(COMPLEMENTO CHD010)
      *    TODAS AS DATAS SÃO INVERTIDAS   AAAAMMDD
       FD  CHD013.
       01  REG-CHD013.
           05  DATA-MOVTO-CH13            PIC 9(8).
           05  SEQ-CH13                   PIC 9(4).
           05  ALINEA-CH13                PIC 99.
           05  DATA-ENTRADA-CH13          PIC 9(8)      COMP-3.
           05  DATA-COMPRA-CH13           PIC 9(8)      COMP-3.
           05  DATA-APRES-CH13            PIC 9(8)      COMP-3.
           05  DATA-REAPRES-CH13          PIC 9(08)     COMP-3.
           05  DATA-RECTO-CH13            PIC 9(8)      COMP-3.
           05  VLR-JUROS-CH13             PIC 9(6)V99   COMP-3.
           05  VLR-MULTA-CH13             PIC 9(6)V99   COMP-3.
           05  VLR-DESCONTO-CH13          PIC 9(6)V99   COMP-3.
           05  FORMA-PAGTO-CH13           PIC X(10).
           05  DCR-MEM-CH13               PIC X(15).
           05  DCR-MEM-R-CH13             PIC X(15).
           05  VLR-LIQUIDO-CH13           PIC 9(6)V99.
           05  RECEBEDOR-CH13             PIC X(12).
