      *  Arquivo que armazena os dados complementares dos lançamentos 
      *  no contas a pagar
      *  TIPO-CP24 => 1 - EM MAOS
      *               2 - DUPLICATA
      *               3 - DEPOSITO EM BANCO
      *               4 - GUIA BOLETO ETC
       FD  CPD024.
       01  REG-CPD024.
           05  FORNEC-CP24              PIC 9(6).
           05  SEQ-CP24                 PIC 9(5).
           05  DEPARTAMENTO-CP24        PIC X(20).
           05  BANCO-CP24               PIC 9(04).
           05  AGENCIA-CP24             PIC X(09).
           05  CONTA-CP24               PIC X(15).
           05  TIPO-CP24                PIC 9(01).
           05  TIPO-CONTA-BANCO-CP24    PIC 9(01).
           05  TITULAR-CONTA-CP24       PIC X(40).
           05  NUMERO-PROGRAMACAO-CP24  PIC 9(09).
           05  FILLER                   PIC X(50).
