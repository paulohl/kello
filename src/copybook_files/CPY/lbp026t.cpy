      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  SB1                             PIC 9(8) COMP-X VALUE 2.
       03  WIN1                            PIC 9(8) COMP-X VALUE 3.
       03  EF1                             PIC 9(8) COMP-X VALUE 4.
