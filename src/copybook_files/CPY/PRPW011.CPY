       FD  PRD011.
       01  REG-PRD011.
           05  DOCTO-PR11               PIC 9(06).
           05  CODIGO-PR11              PIC 9(06).
           05  CIDADE-PR11              PIC 9(04).
           05  DATA-REPORT-PR11         PIC 9(08).
           05  VEZ-APRESENTA-PR11       PIC 9.
           05  OBSERVACAO-PR11 OCCURS 3 TIMES PIC X(80).
