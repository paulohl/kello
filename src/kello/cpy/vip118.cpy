      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  TAB1                            PIC 9(8) COMP-X VALUE 2.
       03  D-CONTRATO                      PIC 9(8) COMP-X VALUE 3.
       03  EF29                            PIC 9(8) COMP-X VALUE 4.
       03  EF30                            PIC 9(8) COMP-X VALUE 5.
       03  EF31                            PIC 9(8) COMP-X VALUE 6.
       03  PB12                            PIC 9(8) COMP-X VALUE 7.
       03  PB13                            PIC 9(8) COMP-X VALUE 8.
       03  PB14                            PIC 9(8) COMP-X VALUE 9.
       03  PB15                            PIC 9(8) COMP-X VALUE 10.
       03  PB16                            PIC 9(8) COMP-X VALUE 11.
       03  PB17                            PIC 9(8) COMP-X VALUE 12.
       03  GBOX11                          PIC 9(8) COMP-X VALUE 13.
       03  MBOX1                           PIC 9(8) COMP-X VALUE 14.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 15.
       03  MBOX6                           PIC 9(8) COMP-X VALUE 16.
       03  AJUDA                           PIC 9(8) COMP-X VALUE 17.
       03  PB8                             PIC 9(8) COMP-X VALUE 18.
       03  MBOX5                           PIC 9(8) COMP-X VALUE 19.
       03  TABP3                           PIC 9(8) COMP-X VALUE 20.
       03  MLE2                            PIC 9(8) COMP-X VALUE 21.
       03  GBOX6                           PIC 9(8) COMP-X VALUE 22.
       03  TABP4                           PIC 9(8) COMP-X VALUE 23.
       03  MLE3                            PIC 9(8) COMP-X VALUE 24.
       03  GBOX8                           PIC 9(8) COMP-X VALUE 25.
