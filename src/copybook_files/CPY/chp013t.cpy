      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  MBOX1                           PIC 9(8) COMP-X VALUE 1.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 2.
       03  MBOX6                           PIC 9(8) COMP-X VALUE 3.
       03  MBOX4                           PIC 9(8) COMP-X VALUE 4.
       03  MBOX9                           PIC 9(8) COMP-X VALUE 5.
       03  WIN1                            PIC 9(8) COMP-X VALUE 6.
       03  EF1                             PIC 9(8) COMP-X VALUE 7.
       03  EF23                            PIC 9(8) COMP-X VALUE 8.
       03  EF32                            PIC 9(8) COMP-X VALUE 9.
       03  EF24                            PIC 9(8) COMP-X VALUE 10.
       03  EF25                            PIC 9(8) COMP-X VALUE 11.
       03  EF33                            PIC 9(8) COMP-X VALUE 12.
       03  EF34                            PIC 9(8) COMP-X VALUE 13.
       03  EF36                            PIC 9(8) COMP-X VALUE 14.
       03  EF35                            PIC 9(8) COMP-X VALUE 15.
       03  PB7                             PIC 9(8) COMP-X VALUE 16.
       03  PB9                             PIC 9(8) COMP-X VALUE 17.
       03  PB10                            PIC 9(8) COMP-X VALUE 18.
       03  GBOX3                           PIC 9(8) COMP-X VALUE 19.
       03  GBOX5                           PIC 9(8) COMP-X VALUE 20.
       03  EF2                             PIC 9(8) COMP-X VALUE 21.
       03  EF3                             PIC 9(8) COMP-X VALUE 22.
       03  MBOX2                           PIC 9(8) COMP-X VALUE 23.
