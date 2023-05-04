      * ITEM-REC-E ->
      *01 -> Encadernação
      *02 -> Estojo
      *03 -> Folha
      *04 -> Foto
      *05 -> Fita
      *06 -> Porta Fita
      *07 -> Poster
      *08 -> DVD
      *09 -> Porta DVD
      *10 -> Foto CD
      *11 -> Moldura
      *12 -> Book
      *13 -> Porta Retrato
      *14 -> Pendrive
      *15 -> Video HD
      *16 -> Revista
      *17 -> Calendário
      ******************************************************************
      *ARQUIVO TABELA DAS ESCALAS DE PREÇO
      ******************************************************************
       FD  RCD100E.
       01  REG-RCD100E.
           05  CHAVE-ALBUM-REC-E.
               10  ALBUM-REC-E                 PIC 9(08).
               10  ITEM-REC-E                  PIC 9(02).
           05  PRODUTO-REC-E                   PIC X(30).
           05  MONTAGEM-REC-E                  PIC 9(06).
           05  RECIBO-REC-E                    PIC 9(06).
           05  DIFERENCA-REC-E                 PIC 9(06).
           05  PRECO-ESCALA-RCD-E              PIC 9(6)V9(4).
           05  QTDE-ACIMA-DE-RCD-E             PIC 9(5).
           05  PERC-DESC-FORM-RCD-E            PIC 9(3)V99.
           05  TOTAL-ESCALA-RCD-E              PIC 9(6)V9(2).
           05  TAXA-JUROS-RCD-E                PIC 9(3)V99.
           05  CARENCIA-JUROS-RCD-E            PIC 9(4).
           05  TAXA-DESCONTO-RCD-E             PIC 9(3)V99.
           05  PERC-DESC-COMISSAO-RCD-E        PIC 9(3)V9(2).
