      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 2.
       03  D-CONTRATO                      PIC 9(8) COMP-X VALUE 3.
       03  EF9                             PIC 9(8) COMP-X VALUE 4.
       03  D-CURSO                         PIC 9(8) COMP-X VALUE 5.
       03  EF10                            PIC 9(8) COMP-X VALUE 6.
       03  EF4                             PIC 9(8) COMP-X VALUE 7.
       03  EF15                            PIC 9(8) COMP-X VALUE 8.
       03  EF2                             PIC 9(8) COMP-X VALUE 9.
       03  EF3                             PIC 9(8) COMP-X VALUE 10.
       03  EF5                             PIC 9(8) COMP-X VALUE 11.
       03  EF6                             PIC 9(8) COMP-X VALUE 12.
       03  EF8                             PIC 9(8) COMP-X VALUE 13.
       03  SB6                             PIC 9(8) COMP-X VALUE 14.
       03  SB3                             PIC 9(8) COMP-X VALUE 15.
       03  EF11                            PIC 9(8) COMP-X VALUE 16.
       03  EF21                            PIC 9(8) COMP-X VALUE 17.
       03  EF14                            PIC 9(8) COMP-X VALUE 18.
       03  EF48                            PIC 9(8) COMP-X VALUE 19.
       03  PB1                             PIC 9(8) COMP-X VALUE 20.
       03  PB2                             PIC 9(8) COMP-X VALUE 21.
       03  PB3                             PIC 9(8) COMP-X VALUE 22.
       03  PB11                            PIC 9(8) COMP-X VALUE 23.
       03  LB1                             PIC 9(8) COMP-X VALUE 24.
       03  PB6                             PIC 9(8) COMP-X VALUE 25.
       03  PB7                             PIC 9(8) COMP-X VALUE 26.
       03  EF20                            PIC 9(8) COMP-X VALUE 27.
       03  EF16                            PIC 9(8) COMP-X VALUE 28.
       03  GBOX2                           PIC 9(8) COMP-X VALUE 29.
       03  EF52                            PIC 9(8) COMP-X VALUE 30.
       03  EF1                             PIC 9(8) COMP-X VALUE 31.
       03  MBOX1                           PIC 9(8) COMP-X VALUE 32.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 33.
       03  MBOX6                           PIC 9(8) COMP-X VALUE 34.
       03  MBOX2                           PIC 9(8) COMP-X VALUE 35.
       03  AJUDA                           PIC 9(8) COMP-X VALUE 36.
       03  PB8                             PIC 9(8) COMP-X VALUE 37.
       03  MBOX4                           PIC 9(8) COMP-X VALUE 38.
       03  WIN4                            PIC 9(8) COMP-X VALUE 39.
       03  SB4                             PIC 9(8) COMP-X VALUE 40.
       03  WIN5                            PIC 9(8) COMP-X VALUE 41.
       03  SB5                             PIC 9(8) COMP-X VALUE 42.
       03  MBOX9                           PIC 9(8) COMP-X VALUE 43.
       03  WIN3                            PIC 9(8) COMP-X VALUE 44.
       03  SB7                             PIC 9(8) COMP-X VALUE 45.
       03  WIN6                            PIC 9(8) COMP-X VALUE 46.
       03  MLE1                            PIC 9(8) COMP-X VALUE 47.
       03  PB5                             PIC 9(8) COMP-X VALUE 48.
       03  PB14                            PIC 9(8) COMP-X VALUE 49.
       03  MBOX3                           PIC 9(8) COMP-X VALUE 50.
       03  MBOX7                           PIC 9(8) COMP-X VALUE 51.
       03  MBOX5                           PIC 9(8) COMP-X VALUE 52.
