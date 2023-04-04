      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  EF-CODIGO                       PIC 9(8) COMP-X VALUE 2.
       03  EF-MODELO                       PIC 9(8) COMP-X VALUE 3.
       03  EF-COD-BARRAS                   PIC 9(8) COMP-X VALUE 4.
       03  EF-REFERENCIA                   PIC 9(8) COMP-X VALUE 5.
       03  EF-NOME                         PIC 9(8) COMP-X VALUE 6.
       03  EF-UNIDADE                      PIC 9(8) COMP-X VALUE 7.
       03  EF-GRUPO                        PIC 9(8) COMP-X VALUE 8.
       03  EF-SEGMENTO                     PIC 9(8) COMP-X VALUE 9.
       03  EF-QTDE-MINIMA                  PIC 9(8) COMP-X VALUE 10.
       03  EF-QTDE-ESTOQUE                 PIC 9(8) COMP-X VALUE 11.
       03  EF-ICMS                         PIC 9(8) COMP-X VALUE 12.
       03  EF-IPI                          PIC 9(8) COMP-X VALUE 13.
       03  EF-ULT-COMPRA                   PIC 9(8) COMP-X VALUE 14.
       03  EF-CUSTO-COMPRA                 PIC 9(8) COMP-X VALUE 15.
       03  EF-CUSTO-MEDIO                  PIC 9(8) COMP-X VALUE 16.
       03  EF-ULT-VENDA                    PIC 9(8) COMP-X VALUE 17.
       03  EF-PRECO-VENDA                  PIC 9(8) COMP-X VALUE 18.
       03  EF-PERC-LUCRO                   PIC 9(8) COMP-X VALUE 19.
       03  EF-PESO                         PIC 9(8) COMP-X VALUE 20.
       03  EF-PERC-COMIS                   PIC 9(8) COMP-X VALUE 21.
       03  EF-VLR-COMIS                    PIC 9(8) COMP-X VALUE 22.
       03  EF-CLASSIF-FISCAL               PIC 9(8) COMP-X VALUE 23.
       03  EF-FORN-PREF                    PIC 9(8) COMP-X VALUE 24.
       03  EF-ULT-FORN                     PIC 9(8) COMP-X VALUE 25.
       03  MLE1                            PIC 9(8) COMP-X VALUE 26.
       03  PB3                             PIC 9(8) COMP-X VALUE 27.
       03  PB2                             PIC 9(8) COMP-X VALUE 28.
       03  PB1                             PIC 9(8) COMP-X VALUE 29.
       03  RB1                             PIC 9(8) COMP-X VALUE 31.
       03  RB2                             PIC 9(8) COMP-X VALUE 32.
       03  GBOX2                           PIC 9(8) COMP-X VALUE 33.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 34.
       03  PB5                             PIC 9(8) COMP-X VALUE 35.
       03  PB4                             PIC 9(8) COMP-X VALUE 36.
       03  LB1                             PIC 9(8) COMP-X VALUE 37.
       03  EF21                            PIC 9(8) COMP-X VALUE 38.
       03  EF-DESC-ULT-FORN                PIC 9(8) COMP-X VALUE 39.
       03  EF1                             PIC 9(8) COMP-X VALUE 40.
       03  EF2                             PIC 9(8) COMP-X VALUE 41.
       03  EF3                             PIC 9(8) COMP-X VALUE 42.
       03  CB1                             PIC 9(8) COMP-X VALUE 43.
       03  CB2                             PIC 9(8) COMP-X VALUE 44.
       03  MBOX6                           PIC 9(8) COMP-X VALUE 45.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 46.
       03  AJUDA                           PIC 9(8) COMP-X VALUE 47.
       03  PB-AJUDA                        PIC 9(8) COMP-X VALUE 48.
       03  MBOX1                           PIC 9(8) COMP-X VALUE 49.
