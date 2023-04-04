       FD  CCD100.
       01  REG-CCD100.
           05  DATA-MOVTO-CC100         PIC 9(8).
           05  FORNEC-CC100             PIC 9(6).
           05  SEQ-CC100                PIC 9(5).
           05  TIPO-LCTO-CC100          PIC 99.
      *    TIPO-LCTO-CC100 = 00-FORNEC  01-FUNCION  02-VENDEDOR
      *                                 03-REPRES   04-REPORT
           05  TIPO-FORN-CC100          PIC 9(2).
           05  NR-DOCTO-CC100           PIC X(10).
           05  DATA-EMISSAO-CC100       PIC 9(8).
           05  DATA-VENCTO-CC100        PIC 9(8).
           05  DESCRICAO-CC100          PIC X(30).
           05  SITUACAO-CC100           PIC 9.
      *    SITUACAO =   00-OK   01-PAGA  02-ESTORNADA
           05  LIBERADO-CC100           PIC 9.
      *    LIBERADO =   00-NÃO  01-SIM
           05  TIPO-MOEDA-CC100         PIC 9.
      *    TIPO-MOEDA = 00-REAL 01-DOLAR
           05  NR-PARCELA-CC100.
               10  NR-PARC-CC100        PIC 99.
               10  TOT-PARC-CC100       PIC 99.
           05  CODREDUZ-APUR-CC100      PIC 9(5).
           05  VALOR-CC100              PIC 9(8)V99.
           05  CRED-DEB-CC100           PIC 9.
      *    0-DEBITO   1-CREDITO
           05  DATA-PGTO-CC100          PIC 9(8).
           05  JUROS-PAGO-CC100         PIC 9(6)V99.
           05  MULTA-PAGA-CC100         PIC 9(6)V99.
           05  DESCONTO-CC100           PIC 9(6)V99.
           05  VALOR-PAGO-CC100         PIC 9(8)V99.
           05  RESPONSAVEL-CC100        PIC X(5).
           05  DIGITADOR-CC100          PIC X(5).
           05  SEQ-CAIXA-CC100          PIC 9(3).
