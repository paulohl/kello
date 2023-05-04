      *  Arquivo que armazena os dados complementares dos lançamentos 
      *  no contas a pagar
      *  TIPO-CP24 => 1 - EM MAOS
      *               2 - DUPLICATA
      *               3 - DEPOSITO EM BANCO
      *               4 - GUIA BOLETO ETC
       FD  CRD024.
       01  REG-CRD024.
           05 COD-COMPL-CR24.
              10 CLASS-CLIENTE-CR24           PIC 9.
      *    classificação cliente =  0-contrato  1-comum
              10 CLIENTE-CR24                 PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05 SEQ-CR24                        PIC 9(5).
           05 DEPARTAMENTO-CR24               PIC X(20).
           05 BANCO-CR24                      PIC 9(04).
           05 AGENCIA-CR24                    PIC X(09).
           05 CONTA-CR24                      PIC X(15).
           05 TIPO-CR24                       PIC 9(01).
           05 TIPO-CONTA-BANCO-CR24           PIC 9(01).
           05 TITULAR-CONTA-CR24              PIC X(40).
           05 NUMERO-PROGRAMACAO-CR24         PIC 9(09).
           05 FILLER                          PIC X(50).
