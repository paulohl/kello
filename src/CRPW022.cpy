      *  Arquivo que armazena observações referente a lançamentos no
      *  contas a pagar
       FD  CRD022.
       01  REG-CRD022.
           05 COD-COMPL-CR22.
              10 CLASS-CLIENTE-CR22           PIC 9.
      *    classificação cliente =  0-contrato  1-comum
              10 CLIENTE-CR22                 PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05 SEQ-CR22                        PIC 9(5).
           05 OBS-CR22                        PIC X(120).
