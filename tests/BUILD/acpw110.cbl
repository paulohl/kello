      *   Cadastro MEMORANDO INTERNO (KAC ATENDIMENTO AO CLIENTE)
       FD  ACD110.
       01  REG-ACD110.
           05 NUMERO-AC110                       PIC 9(11).
           05 ASSUNTO-AC110                      PIC X(70).
           05 CONTRATO-AC110                     PIC 9(08).
           05 EMITENTE-ORIGINAL-AC110            PIC X(30).
           05 CIDADE-COLACAO-AC110               PIC 9(04).
           05 TIPO-PROPOSTA-AC110                PIC 9(01).
           05 DATA-PROPOSTA-AC110                PIC 9(08).
           05 BANCO-AGENCIA-AC110                PIC X(15).
           05 NUMERO-IDENTIFICACAO-AC110         PIC X(15).
           05 VALOR-PROPOSTA-AC110               PIC 9(05)V99.
           05 EMITENTE-NOVOS-AC110               PIC X(30).
           05 OBSERVACAO1-AC110                  PIC X(70).
           05 OBSERVACAO2-AC110                  PIC X(70).
           05 OBSERVACAO3-AC110                  PIC X(70).
           05 OBSERVACAO4-AC110                  PIC X(70).
           05 OBSERVACAO5-AC110                  PIC X(70).
           05 OBSERVACAO6-AC110                  PIC X(70).
           05 OBSERVACAO7-AC110                  PIC X(70).
           05 OBSERVACAO8-AC110                  PIC X(70).
           05 OBSERVACAO9-AC110                  PIC X(70).
           05 DATA-EMITENTE-AC110                PIC 9(08).
           05 DATA-CPD-AC110                     PIC 9(08).
           05 DATA-ASSISTENTE-AC110              PIC 9(08).
           05 DATA-DCE-AC110                     PIC 9(08).
           05 FILLER                             PIC X(200).

      * TIPO-PROPOSTA-AC110 = 1 - Ordem de Pagamento , 2 - Recibo

