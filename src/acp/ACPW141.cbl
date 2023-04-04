      *   Cadastro de DEVOLUCAO PARCIAL TOTAL
      *   complemento do ACD140
       FD  ACD141.
       01  REG-ACD141.
           05 NUMERO-AC141                       PIC 9(11).
           05 TIPO-AC141                         PIC 9(01).
           05 SEQ-AC141                          PIC 9(02).
           05 NUMERO-DOCUMENTO-AC141             PIC X(15).
           05 PARCELA-AC141                      PIC 9(04).
           05 TIPO-DOCUMENTO-AC141               PIC X(04).
           05 BANCO-AC141                        PIC 9(03).
           05 VALOR-AC141                        PIC 9(05)V99.
           05 VENCIMENTO-AC141                   PIC 9(08).
           05 PORTADOR-AC141                     PIC 9(02).
           05 CIDADE-AC141                       PIC 9(04).
           05 FILLER                             PIC X(200).

      * TIPO-AC141 = 1 - ORIGINAL , 2 - NOVOS

