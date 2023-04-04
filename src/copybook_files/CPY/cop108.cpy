      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  EF1                             PIC 9(8) COMP-X VALUE 2.
       03  EF11                            PIC 9(8) COMP-X VALUE 3.
       03  EF6                             PIC 9(8) COMP-X VALUE 4.
       03  CB1                             PIC 9(8) COMP-X VALUE 5.
       03  CB2                             PIC 9(8) COMP-X VALUE 6.
       03  PB1                             PIC 9(8) COMP-X VALUE 7.
       03  PB2                             PIC 9(8) COMP-X VALUE 8.
       03  PB5                             PIC 9(8) COMP-X VALUE 9.
       03  PB4                             PIC 9(8) COMP-X VALUE 10.
       03  GBOX2                           PIC 9(8) COMP-X VALUE 11.
       03  LB1                             PIC 9(8) COMP-X VALUE 12.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 13.
       03  GBOX4                           PIC 9(8) COMP-X VALUE 14.
       03  PB7                             PIC 9(8) COMP-X VALUE 15.
       03  WIN2                            PIC 9(8) COMP-X VALUE 16.
       03  PB3                             PIC 9(8) COMP-X VALUE 17.
       03  LB2                             PIC 9(8) COMP-X VALUE 18.
       03  MBOX1                           PIC 9(8) COMP-X VALUE 19.
       03  WIN1                            PIC 9(8) COMP-X VALUE 20.
       03  EF3                             PIC 9(8) COMP-X VALUE 21.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 22.
       03  MBOX6                           PIC 9(8) COMP-X VALUE 23.
       03  WIN3                            PIC 9(8) COMP-X VALUE 24.
       03  GBOX5                           PIC 9(8) COMP-X VALUE 25.
       03  GBOX6                           PIC 9(8) COMP-X VALUE 26.
       03  GBOX8                           PIC 9(8) COMP-X VALUE 27.
       03  GBOX9                           PIC 9(8) COMP-X VALUE 28.
       03  EF4                             PIC 9(8) COMP-X VALUE 29.
       03  EF5                             PIC 9(8) COMP-X VALUE 65.
       03  GBOX7                           PIC 9(8) COMP-X VALUE 71.
       03  EF7                             PIC 9(8) COMP-X VALUE 80.
       03  EF8                             PIC 9(8) COMP-X VALUE 81.
       03  EF9                             PIC 9(8) COMP-X VALUE 82.
       03  EF10                            PIC 9(8) COMP-X VALUE 83.
       03  EF12                            PIC 9(8) COMP-X VALUE 84.
       03  EF13                            PIC 9(8) COMP-X VALUE 85.
       03  EF14                            PIC 9(8) COMP-X VALUE 86.
       03  EF15                            PIC 9(8) COMP-X VALUE 87.
       03  EF16                            PIC 9(8) COMP-X VALUE 88.
       03  EF17                            PIC 9(8) COMP-X VALUE 89.
       03  EF18                            PIC 9(8) COMP-X VALUE 90.
       03  EF19                            PIC 9(8) COMP-X VALUE 91.
       03  EF20                            PIC 9(8) COMP-X VALUE 92.
       03  EF21                            PIC 9(8) COMP-X VALUE 93.
       03  EF22                            PIC 9(8) COMP-X VALUE 94.
       03  EF23                            PIC 9(8) COMP-X VALUE 95.
       03  EF24                            PIC 9(8) COMP-X VALUE 96.
