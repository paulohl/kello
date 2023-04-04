      *   Cadastro de CONTAS A RECEBER (KAC ATENDIMENTO AO CLIENTE)
      *   complemento do acd120

       FD  ACD121.
       01  REG-ACD121.
           05 NUMERO-AC121                       PIC 9(11).
           05 TIPO-AC121                         PIC 9(01).
           05 SEQ-AC121                          PIC 9(02).
           05 NUMERO-DOCUMENTO-AC121             PIC X(15).
           05 PARCELA-AC121                      PIC 9(04).
           05 TIPO-DOCUMENTO-AC121               PIC X(04).
           05 BANCO-AC121                        PIC 9(03).
           05 VALOR-AC121                        PIC 9(05)V99.
           05 VENCIMENTO-AC121                   PIC 9(08).
           05 OBSERVACAO-AC121                   PIC X(10).
           05 FILLER                             PIC X(100).

      * TIPO-AC121 = 1 - ORIGINAL , 2 - RENEGOCIADOS

