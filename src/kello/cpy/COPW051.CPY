      * MOVIMENTO DE BRINDES INICIAL

       FD  COD051.
       01  REG-COD051.
           05  NR-CONTRATO-CO51    PIC 9(04).
           05  ITEM-CO51           PIC 9999.
           05  CODBRINDE-CO51      PIC 999.
           05  CURSO-CO51          PIC 9(3).
           05  TURMA-CO51          PIC XX.
           05  QTDE-POR-FORM-CO51  PIC 9(5).
      *    qtde-por-form = qtde-brindes ou qtde por formando
      *    quando for qtde-brindes o campo qtde-form-CO51 deverá ficar
      *    zerado
           05  QTDE-FORM-CO51      PIC 9(4).
           05  CUSTO-UNIT-CO51     PIC 9(8)V99 COMP-3.
      *    se custo-unit-CO51 = zeros o custo a ser considerado será
      *    do cadastro de brinde
           05  VALOR-PREVISTO-CO51 PIC 9(8)V99 COMP-3.
           05  DATA-VENCTO-CO51    PIC 9(8).
      *    DATA-VENCTO - AAAA/MM/DD.
           05  DATA-SOLICIT-CO51   PIC 9(8)    COMP-3.
           05  SUSP-PREV-DEF-CO51  PIC 9.
      *    0-PREVISTO   1-DEFINITIVO  2-SUSPENSO
           05  VALOR-PAGO-CO51     PIC 9(8)V99 COMP-3.
           05  DATA-PAGTO-CO51     PIC 9(8)    COMP-3.
           05  REALIZADO-CO51      PIC 9.
      *    0-NAO  1-SIM
           05  DIAS-PRAZO-CO51     PIC 9(4).
      *    Prazo médio até a data prevista de venda (do contrato)
      *    Se DATA-PAGTO-CO51 = zeros
      *       dias entre DATA-VENCTO-CO51 até DATA-PREV-VENDA-CO40
      *    Senão dias entre DATA-PAGTO-CO51 até DATA-PREV-VENDA-CO40.
      *    Qualquer alteração efetuada nos campos data-pagto-CO51
      *    data-vencto-CO51 e data-prev-venda-co40 deve-se alterar o
      *    campo DIAS-PRAZO-CO51.
           05  COD-FORNEC-CO51     PIC 9(6)    COMP-3.

      **** QTDE REALIZADO = 1 (SIM) O CUSTO SERµ GRAVADO, PODENDO
      **** SER ALTERADO E A DATAPREVISTA P/ A DATA DE PAGTO
