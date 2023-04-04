       FD  CRD041.
       01  REG-CRD041.
           05  NOME-41             PIC X(16).
           05  CARTEIRA-41         PIC 9(2) OCCURS 3 TIMES.
           05  DATA-BASE-41        PIC 9(8).
           05  TAXA-JUROS-41       PIC 99V99.
           05  DATA-INI-41         PIC 9(8).
           05  DATA-FIM-41         PIC 9(8).
           05  DIAS-INI-41         PIC 9(3).
           05  DIAS-FIM-41         PIC 9(3).
           05  FERIADOS-41         PIC 9(6) OCCURS 10 TIMES.
           05  QTDE-DUPLI-41       PIC 9(4).
           05  VLR-BRUTO-41        PIC 9(10)V99.
           05  PM-41               PIC 9(3)V99.
           05  DIAS-30-41          PIC 9(3)V99.
           05  TAXA-30-41          PIC 9V999999.
           05  JURO-30-41          PIC 9(10)V99.
           05  SALDO-30-41         PIC 9(10)V99.
           05  DIAS-60-41          PIC 9(3)V99.
           05  TAXA-60-41          PIC 9V999999.
           05  JURO-60-41          PIC 9(10)V99.
           05  SALDO-60-41         PIC 9(10)V99.
           05  DIAS-90-41          PIC 9(3)V99.
           05  TAXA-90-41          PIC 9V999999.
           05  JURO-90-41          PIC 9(10)V99.
           05  SALDO-90-41         PIC 9(10)V99.
           05  DIAS-120-41         PIC 9(3)V99.
           05  TAXA-120-41         PIC 9V999999.
           05  JURO-120-41         PIC 9(10)V99.
           05  SALDO-120-41        PIC 9(10)V99.
           05  DIAS-150-41         PIC 9(3)V99.
           05  TAXA-150-41         PIC 9V999999.
           05  JURO-150-41         PIC 9(10)V99.
           05  SALDO-150-41        PIC 9(10)V99.
           05  DIAS-180-41         PIC 9(3)V99.
           05  TAXA-180-41         PIC 9V999999.
           05  JURO-180-41         PIC 9(10)V99.
           05  SALDO-180-41        PIC 9(10)V99.
           05  PORTADOR-DESTINO-41 PIC 9(04).

