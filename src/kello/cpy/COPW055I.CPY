      * MOVIMENTO DE CUSTEIO INICIAL

       FD  COD055i.
       01  REG-COD055i.
           05  NR-CONTRATO-CO55i   PIC 9(04).
           05  ITEM-CO55i          PIC 9999.
           05  CODBRINDE-CO55i     PIC 99999.
           05  CURSO-CO55i         PIC 9(3).
           05  TURMA-CO55i         PIC XX.
           05  QTDE-POR-FORM-CO55i PIC 9(5).
      *    qtde-por-form = qtde-brindes ou qtde por formando
      *    quando for qtde-brindes o campo qtde-form-CO55ideverá ficar
      *    zerado
           05  QTDE-FORM-CO55i     PIC 9(4).
           05  CUSTO-UNIT-CO55i    PIC 9(8)V99 COMP-3.
      *    se custo-unit-CO55i= zeros o custo a ser considerado será
      *    do cadastro de brinde
           05  VALOR-PREVISTO-CO55i PIC 9(8)V99 COMP-3.
           05  DATA-VENCTO-CO55i    PIC 9(8).
      *    DATA-VENCTO - AAAA/MM/DD.
           05  DATA-SOLICIT-CO55i   PIC 9(8)    COMP-3.
           05  SUSP-PREV-DEF-CO55i  PIC 9.
      *    0-PREVISTO   1-DEFINITIVO  2-SUSPENSO
           05  VALOR-PAGO-CO55i     PIC 9(8)V99 COMP-3.
           05  DATA-PAGTO-CO55i     PIC 9(8)    COMP-3.
           05  REALIZADO-CO55i      PIC 9.
      *    0-NAO  1-SIM
           05  DIAS-PRAZO-CO55i     PIC 9(4).
      *    Prazo médio até a data prevista de venda (do contrato)
      *    Se DATA-PAGTO-CO55i= zeros
      *       dias entre DATA-VENCTO-CO55iaté DATA-PREV-VENDA-CO40
      *    Senão dias entre DATA-PAGTO-CO55iaté DATA-PREV-VENDA-CO40.
      *    Qualquer alteração efetuada nos campos data-pagto-CO55i
      *    data-vencto-CO55ie data-prev-venda-co40 deve-se alterar o
      *    campo DIAS-PRAZO-CO55i.
           05  COD-FORNEC-CO55i     PIC 9(6)    COMP-3.

      **** QTDE REALIZADO = 1 (SIM) O CUSTO SERµ GRAVADO, PODENDO
      **** SER ALTERADO E A DATAPREVISTA P/ A DATA DE PAGTO
