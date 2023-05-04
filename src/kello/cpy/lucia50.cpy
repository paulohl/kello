           SELECT  COD050 ASSIGN TO PATH-COD050
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD050
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO50 =
                           NR-CONTRATO-CO50 ITEM-CO50
                   ALTERNATE RECORD KEY IS DATA-VENCTO-CO50
                             WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ALT1-CO50 =  NR-CONTRATO-CO50
                        REALIZADO-CO50 DATA-VENCTO-CO50 ITEM-CO50.
       FD  COD050.
       01  REG-COD050.
           05  NR-CONTRATO-CO50    PIC 9(04).
           05  ITEM-CO50           PIC 99.
           05  CODBRINDE-CO50      PIC 999.
           05  CURSO-CO50          PIC 9(3).
           05  TURMA-CO50          PIC XX.
           05  QTDE-POR-FORM-CO50  PIC 9(5).
      *    qtde-por-form = qtde-brindes ou qtde por formando
      *    quando for qtde-brindes o campo qtde-form-co50 deverá ficar
      *    zerado
           05  QTDE-FORM-CO50      PIC 9(4).
           05  CUSTO-UNIT-CO50     PIC 9(8)V99 COMP-3.
      *    se custo-unit-co50 = zeros o custo a ser considerado será
      *    do cadastro de brinde
           05  VALOR-PREVISTO-CO50 PIC 9(8)V99 COMP-3.
           05  DATA-VENCTO-CO50    PIC 9(8).
      *    DATA-VENCTO - AAAA/MM/DD.
           05  DATA-SOLICIT-CO50   PIC 9(8)    COMP-3.
           05  SUSP-PREV-DEF-CO50  PIC 9.
      *    0-PREVISTO   1-DEFINITIVO  2-SUSPENSO
           05  VALOR-PAGO-CO50     PIC 9(8)V99 COMP-3.
           05  DATA-PAGTO-CO50     PIC 9(8)    COMP-3.
           05  REALIZADO-CO50      PIC 9.
      *    0-NAO  1-SIM
           05  DIAS-PRAZO-CO50     PIC 9(4).
      *    Prazo médio até a data prevista de venda (do contrato)
      *    Se DATA-PAGTO-CO50 = zeros
      *       dias entre DATA-VENCTO-CO50 até DATA-PREV-VENDA-CO40
      *    Senão dias entre DATA-PAGTO-CO50 até DATA-PREV-VENDA-CO40.
      *    Qualquer alteração efetuada nos campos data-pagto-co50
      *    data-vencto-co50 e data-prev-venda-co40 deve-se alterar o
      *    campo DIAS-PRAZO-CO50.
           05  COD-FORNEC-CO50     PIC 9(6)    COMP-3.

      **** QTDE REALIZADO = 1 (SIM) O CUSTO SERµ GRAVADO, PODENDO
      **** SER ALTERADO E A DATAPREVISTA P/ A DATA DE PAGTO
