           SELECT CGD012 ASSIGN TO PATH-CGD012
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-CG12
                  STATUS IS ST-CGD012.
      * Complemento do arquivo de CLIENTE - DADOS CONTRATO
       FD  CGD012.
       01  REG-CGD012.
           05  CODIGO-CG12         PIC 9(8).
           05  CURSO-CG12          PIC 9(3).
           05  TURMA-CG12          PIC XX.
           05  TAMANHO-BECA-CG12   PIC XX.
           05  FOTO-IDENTIFIC-CG12 PIC 9.
           05  CARGO-COMISSAO-CG12 PIC 9.
      *    CARGO-COMISSAO  1-PRESIDENTE   2-VICE-PRESID   3-TESOUREIRO
      *                    4-SECRETARIO   5-FORMANDO
