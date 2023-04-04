      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  EF2                             PIC 9(8) COMP-X VALUE 2.
       03  EF4                             PIC 9(8) COMP-X VALUE 3.
       03  EF5                             PIC 9(8) COMP-X VALUE 4.
       03  EF11                            PIC 9(8) COMP-X VALUE 5.
       03  EF12                            PIC 9(8) COMP-X VALUE 6.
       03  EF6                             PIC 9(8) COMP-X VALUE 7.
       03  EF10                            PIC 9(8) COMP-X VALUE 8.
       03  EF1                             PIC 9(8) COMP-X VALUE 11.
       03  SB1                             PIC 9(8) COMP-X VALUE 12.
       03  SB2                             PIC 9(8) COMP-X VALUE 13.
       03  EF13                            PIC 9(8) COMP-X VALUE 14.
       03  EF14                            PIC 9(8) COMP-X VALUE 15.
       03  PB3                             PIC 9(8) COMP-X VALUE 16.
       03  PB6                             PIC 9(8) COMP-X VALUE 17.
       03  EF3                             PIC 9(8) COMP-X VALUE 18.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 19.
       03  EF8                             PIC 9(8) COMP-X VALUE 20.
       03  EF9                             PIC 9(8) COMP-X VALUE 21.
       03  LB1                             PIC 9(8) COMP-X VALUE 23.
       03  PB2                             PIC 9(8) COMP-X VALUE 24.
       03  GBOX2                           PIC 9(8) COMP-X VALUE 25.
       03  PB4                             PIC 9(8) COMP-X VALUE 26.
       03  MBOX6                           PIC 9(8) COMP-X VALUE 27.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 28.
       03  AJUDA                           PIC 9(8) COMP-X VALUE 29.
       03  PB-AJUDA                        PIC 9(8) COMP-X VALUE 30.
       03  GBOX3                           PIC 9(8) COMP-X VALUE 31.
       03  MBOX1                           PIC 9(8) COMP-X VALUE 32.
       03  WIN3                            PIC 9(8) COMP-X VALUE 33.
       03  EF16                            PIC 9(8) COMP-X VALUE 34.
       03  EF7                             PIC 9(8) COMP-X VALUE 35.
       03  EF15                            PIC 9(8) COMP-X VALUE 36.
       03  EF17                            PIC 9(8) COMP-X VALUE 37.
       03  PB7                             PIC 9(8) COMP-X VALUE 38.
