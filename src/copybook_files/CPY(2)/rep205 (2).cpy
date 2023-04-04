      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  EF16                            PIC 9(8) COMP-X VALUE 2.
       03  EF1                             PIC 9(8) COMP-X VALUE 3.
       03  EF11                            PIC 9(8) COMP-X VALUE 4.
       03  PB1                             PIC 9(8) COMP-X VALUE 5.
       03  PB2                             PIC 9(8) COMP-X VALUE 6.
       03  PB5                             PIC 9(8) COMP-X VALUE 7.
       03  PB4                             PIC 9(8) COMP-X VALUE 8.
       03  GBOX2                           PIC 9(8) COMP-X VALUE 9.
       03  EF17                            PIC 9(8) COMP-X VALUE 10.
       03  GBOX3                           PIC 9(8) COMP-X VALUE 11.
       03  RB1                             PIC 9(8) COMP-X VALUE 12.
       03  RB2                             PIC 9(8) COMP-X VALUE 13.
       03  RB3                             PIC 9(8) COMP-X VALUE 14.
       03  EF4                             PIC 9(8) COMP-X VALUE 15.
       03  EF5                             PIC 9(8) COMP-X VALUE 16.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 17.
       03  GBOX4                           PIC 9(8) COMP-X VALUE 18.
       03  EF23                            PIC 9(8) COMP-X VALUE 33.
       03  GBOX5                           PIC 9(8) COMP-X VALUE 36.
       03  GBOX6                           PIC 9(8) COMP-X VALUE 37.
       03  EF34                            PIC 9(8) COMP-X VALUE 46.
       03  EF6                             PIC 9(8) COMP-X VALUE 53.
       03  EF2                             PIC 9(8) COMP-X VALUE 57.
       03  PB13                            PIC 9(8) COMP-X VALUE 60.
       03  WIN2                            PIC 9(8) COMP-X VALUE 61.
       03  PB3                             PIC 9(8) COMP-X VALUE 62.
       03  MBOX1                           PIC 9(8) COMP-X VALUE 63.
       03  WIN1                            PIC 9(8) COMP-X VALUE 64.
       03  EF3                             PIC 9(8) COMP-X VALUE 65.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 66.
       03  MBOX6                           PIC 9(8) COMP-X VALUE 67.
       03  WIN3                            PIC 9(8) COMP-X VALUE 68.
       03  EF31                            PIC 9(8) COMP-X VALUE 69.
       03  EF32                            PIC 9(8) COMP-X VALUE 70.
       03  PB10                            PIC 9(8) COMP-X VALUE 71.
       03  LB3                             PIC 9(8) COMP-X VALUE 72.
       03  PB15                            PIC 9(8) COMP-X VALUE 73.
       03  PB12                            PIC 9(8) COMP-X VALUE 74.
