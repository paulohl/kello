       FD  PRD105.
       01  REG-PRD105.
           05  NR-PLAN-PR105.
               10 CIDADE-PR105   PIC 9(4).
               10 MESDIA-PR105   PIC 9(4).
               10 ANO-PR105      PIC 9(4).
               10 SEQ-PR105      PIC 9(2).
           05  HOTEL-PR105       PIC 9(4).
           05  CONTATO-PR105     PIC X(10).
           05  VALOR-DIARIA-PR105 PIC 9(8)V99.
           05  OBS-PR105         PIC X(15).
           05  PREV-VEIC-PR105   PIC 9(8)V99.
           05  PREV-HOSP-PR105   PIC 9(8)V99.
           05  PREV-REFEIC-PR105 PIC 9(8)V99.
           05  PREV-OUTROS-PR105 PIC 9(8)V99.
           05  QT-VEICULO-PR105  PIC 99.
           05  QT-DIAS-PR105     PIC 99.
      * {PLANEJAMENTO            }
