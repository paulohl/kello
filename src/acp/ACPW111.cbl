      *   Cadastro de MEMORANDO INTERNO (KAC ATENDIMENTO AO CLIENTE)
      *   complemento do acd110
       FD  ACD111.
       01  REG-ACD111.
           05 NUMERO-AC111                       PIC 9(11).
           05 TIPO-AC111                         PIC 9(01).
           05 SEQ-AC111                          PIC 9(02).
           05 NUMERO-DOCUMENTO-AC111             PIC X(15).
           05 PARCELA-AC111                      PIC 9(04).
           05 TIPO-DOCUMENTO-AC111               PIC X(04).
           05 BANCO-AC111                        PIC 9(03).
           05 VALOR-AC111                        PIC 9(05)V99.
           05 VENCIMENTO-AC111                   PIC 9(08).
           05 PORTADOR-AC111                     PIC 9(02).
           05 CIDADE-AC111                       PIC 9(04).
           05 FILLER                             PIC X(200).

      * TIPO-AC111 = 1 - ORIGINAL , 2 - NOVOS

