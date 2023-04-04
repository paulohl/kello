      *   Cadastro DEVOLUCAO PARCIAL TOTAL
       FD  ACD140.
       01  REG-ACD140.
           05 NUMERO-AC140                       PIC 9(11).
           05 ASSUNTO-AC140                      PIC X(70).
           05 CONTRATO-AC140                     PIC 9(08).
           05 EMITENTE-ORIGINAL-AC140            PIC X(30).
           05 CIDADE-COLACAO-AC140               PIC 9(04).
           05 TIPO-PROPOSTA-AC140                PIC 9(01).
           05 DATA-PROPOSTA-AC140                PIC 9(08).
           05 BANCO-AGENCIA-AC140                PIC X(15).
           05 NUMERO-IDENTIFICACAO-AC140         PIC X(15).
           05 VALOR-PROPOSTA-AC140               PIC 9(05)V99.
           05 EMITENTE-NOVOS-AC140               PIC X(30).
           05 OBSERVACAO1-AC140                  PIC X(70).
           05 OBSERVACAO2-AC140                  PIC X(70).
           05 OBSERVACAO3-AC140                  PIC X(70).
           05 OBSERVACAO4-AC140                  PIC X(70).
           05 OBSERVACAO5-AC140                  PIC X(70).
           05 OBSERVACAO6-AC140                  PIC X(70).
           05 OBSERVACAO7-AC140                  PIC X(70).
           05 OBSERVACAO8-AC140                  PIC X(70).
           05 OBSERVACAO9-AC140                  PIC X(70).
           05 DATA-EMITENTE-AC140                PIC 9(08).
           05 DATA-CPD-AC140                     PIC 9(08).
           05 DATA-ASSISTENTE-AC140              PIC 9(08).
           05 DATA-DCE-AC140                     PIC 9(08).
           05 FILLER                             PIC X(200).

      * TIPO-PROPOSTA-AC140 = 1 - Ordem de Pagamento , 2 - Recibo

