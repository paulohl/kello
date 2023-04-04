       FD  RCD101.
       01  REG-RCD101.
           05  CHAVE-ALBUM-REC1.
               10  ALBUM-REC1                   PIC 9(8).
      *        10  SEQ-REC1                     PIC 9(8).
           05  VENCTO-REC1                      PIC 9(8).
           05  VALOR-REC1                       PIC 9(8)V99.
           05  NUMERO-REC1                      PIC 9(6).
           05  PARCELA-REC1                     PIC 9(2).
           05  BANCO-REC1                       PIC 9(3).
           05  TIPO-REC1                        PIC 9.
      *    1-CHEQUE  2-MOEDA  3-ANTECIPADO  4-DUPL/PROMIS
      *    5-DEB.AUTOM. 6-CARTAO CRED.        .
           05  DTA-BAIXA-REC1                   PIC 9(8).
           05  COMIS-PARC-REC1                  PIC 99V999.
           05  CARTAO-CRED-REC1                 PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-REC1       PIC 9(03)V99.
           05  QT-PARCELA-REC1                  PIC 9(02).
           05  TAXA-ADMINIST-PARCELA-REC1       PIC 9(03)V99.
           05  FILLER                           PIC X(25).
