      * Complemento do arquivo de CLIENTE - DADOS CONTRATO
       FD  CGD012.
       01  REG-CGD012.
           05  CODIGO-CG12         PIC 9(8).
           05  CURSO-CG12          PIC 9(3).
           05  TURMA-CG12          PIC XX.
           05  TAMANHO-BECA-CG12   PIC XX.
           05  FOTO-IDENTIFIC-CG12 PIC 9.
           05  CARGO-COMISSAO-CG12 PIC 9.
      *> Essa documentação é a correta identificada no dia 10/03/2016
      *    CARGO-COMISSAO  1-PRESIDENTE   2-TESOUREIRO    3-SECRETÁRIO
      *                    4-NORMAL


      *> Essa documentação está incorreta identificada no dia 10/03/2016
      *    CARGO-COMISSAO  1-PRESIDENTE   2-VICE-PRESID   3-TESOUREIRO
      *                    4-SECRETARIO   5-FORMANDO
