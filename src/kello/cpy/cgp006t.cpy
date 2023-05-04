      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  WIN1                            PIC 9(8) COMP-X VALUE 1.
       03  LB1                             PIC 9(8) COMP-X VALUE 2.
       03  PB-CURU                         PIC 9(8) COMP-X VALUE 8.
       03  PB-CURD                         PIC 9(8) COMP-X VALUE 9.
       03  WIN2                            PIC 9(8) COMP-X VALUE 10.
