      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  MBOX-INFORMACAO                 PIC 9(8) COMP-X VALUE 1.
       03  MBOX-CRITICA                    PIC 9(8) COMP-X VALUE 2.
       03  MBOX-QUESTIONA                  PIC 9(8) COMP-X VALUE 3.
