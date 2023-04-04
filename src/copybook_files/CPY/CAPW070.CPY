      *  Arquivo de movimento de compromisso contratual
       FD  CAD070.
       01  REG-CAD070.
           05  DATAMOV-70        PIC 9(08).
           05  NR-CONTRATO-70    PIC 9(04).
           05  ITEM-70           PIC 99.
           05  CODBRINDE-70      PIC 999.
           05  QTDE-70           PIC 9(5).
           05  DATAPREVISTA-70   PIC 9(08).
           05  CUSTOPREVISTO-70  PIC 9(08)V99.
           05  CUSTOTOTAL-70     PIC 9(08)V99.
           05  DIAS-PRAZO-70     PIC 9(5).
           05  REALIZADO-70      PIC 9.
           05  FORMA-PAGTO-70    PIC X(20).
           05  DIGITADOR-70      PIC X(4).
           05  DEB-CRED-70       PIC X.

      **** SE REALIZADO = ZEROS (NAO) O CUSTO CONSIDERADO SERA
      **** A DO CADASTRO DE BRINDE E A DATA SERµ PREVISTA.
      **** QTDE REALIZADO = 1 (SIM) O CUSTO SERµ GRAVADO, PODENDO
      **** SER ALTERADO E A DATAPREVISTA P/ A DATA DA REALIZA€ÇO
