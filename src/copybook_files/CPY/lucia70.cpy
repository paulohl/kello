           SELECT  COD070 ASSIGN TO PATH-COD070
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD070
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO70 =
                           NR-CONTRATO-CO70 ITEM-CO70
                   ALTERNATE RECORD KEY IS DATA-VENCTO-CO70
                             WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ALT1-CO70 =  NR-CONTRATO-CO70
                        REALIZADO-CO70 DATA-VENCTO-CO70 ITEM-CO70.
       FD  COD070.
       01  REG-COD070.
           05  NR-CONTRATO-CO70    PIC 9(04).
           05  ITEM-CO70           PIC 99.
           05  DESCRICAO-CO70      PIC X(40).
           05  CURSO-CO70          PIC 9(3).
           05  TURMA-CO70          PIC XX.
           05  VALOR-PREVISTO-CO70 PIC 9(8)V99 COMP-3.
           05  DATA-VENCTO-CO70    PIC 9(8).
      *    DATA-VENCTO - AAAA/MM/DD.
           05  DATA-MOVTO-CO70     PIC 9(8)    COMP-3.
           05  SUSP-PREV-DEF-CO70  PIC 9.
      *    0-PREVISTO   1-DEFINITIVO  2-SUSPENSO
           05  VALOR-RECTO-CO70    PIC 9(8)V99 COMP-3.
           05  DATA-RECTO-CO70     PIC 9(8)    COMP-3.
           05  REALIZADO-CO70      PIC 9.
      *    0-NAO  1-SIM
           05  DIAS-PRAZO-CO70     PIC 9(4).
      *    Prazo médio até a data prevista de venda (do contrato)
      *    Se DATA-RECTO-CO70 = zeros
      *       dias entre DATA-VENCTO-CO70 até DATA-PREV-VENDA-CO40
      *    Senão dias entre DATA-RECTO-CO70 até DATA-PREV-VENDA-CO40.
      *    Qualquer alteração efetuada nos campos data-RECTo-CO70
      *    data-vencto-co70 e data-prev-venda-co40 deve-se alterar o
      *    campo DIAS-PRAZO-CO70.
