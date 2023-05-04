           SELECT RCD101 ASSIGN TO PATH-RCD101
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RCD101
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-REC1 = ALBUM-REC1 VENCTO-REC1
                         BANCO-REC1 NUMERO-REC1
                  ALTERNATE RECORD KEY IS ALBUM-REC1 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-REC1 = DTA-BAIXA-REC1
                            ALBUM-REC1 WITH DUPLICATES.
       FD  RCD101.
       01  REG-RCD101.
           05  ALBUM-REC1       PIC 9(8)     COMP-3.
           05  VENCTO-REC1      PIC 9(8)     COMP-3.
           05  VALOR-REC1       PIC 9(8)V99  COMP-3.
           05  NUMERO-REC1      PIC 9(6)     COMP-3.
           05  BANCO-REC1       PIC 9(3).
           05  TIPO-REC1        PIC 9.
      *    1-CHEQUE  2-MOEDA  3-ANTECIPADO  4-DUPL/PROMIS
           05  DTA-BAIXA-REC1   PIC 9(8)     COMP-3.
