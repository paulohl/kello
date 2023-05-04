      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  PB3                             PIC 9(8) COMP-X VALUE 2.
       03  PB1                             PIC 9(8) COMP-X VALUE 3.
       03  GBOX2                           PIC 9(8) COMP-X VALUE 4.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 5.
       03  PB5                             PIC 9(8) COMP-X VALUE 6.
       03  EF1                             PIC 9(8) COMP-X VALUE 7.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 8.
       03  AJUDA                           PIC 9(8) COMP-X VALUE 9.
       03  PB-AJUDA                        PIC 9(8) COMP-X VALUE 10.
       03  MBOX1                           PIC 9(8) COMP-X VALUE 11.
