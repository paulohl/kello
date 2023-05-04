       01 DSC-Control-Block.
          03 DSC-Session-ID                      PIC 9(8)  COMP-5.
          03 DSC-SS-Identifier                   PIC X(8).
          03 DSC-Call-Type                       PIC 9(2)  COMP-5.
             78 DSC-Synchronous   VALUE 1.
             78 DSC-Asynchronous  VALUE 2.
          03 DSC-Event-Block.
             05 DSC-Ancestor                     PIC 9(9)  COMP-5.
             05 DSC-Descendant                   PIC 9(9)  COMP-5.
             05 DSC-SS-ID                        PIC X(8).
             05 DSC-Event-SS-Details.
                07 DSC-Event-SS-ID               PIC X(8).
                07 DSC-Event-SS-Instance-No      PIC 9(2) COMP-5.

             05 DSC-Event-Reserved               PIC X(11).

             05 DSC-Event-Type                   PIC 9(4)  COMP-5.
             05 DSC-Event-Data.

                07 DSC-Gadget-Event-Data.
                   09 DSC-Gadget-Type            PIC 9(2)  COMP-5.
                   09 DSC-Gadget-Command         PIC 9(2)  COMP-5.
                   09 DSC-Gadget-Id              PIC 9(4)  COMP-5.
                   09 DSC-Gadget-Return          PIC 9(4)  COMP-5.

                07 DSC-Mouse-Event-Data.
                   09 DSC-Mouse-X                PIC S9(4) COMP-5.
                   09 DSC-Mouse-Y                PIC S9(4) COMP-5.
                   09 DSC-Mouse-State            PIC 9(2)  COMP-5.
                   09 DSC-Mouse-Moved-Flag       PIC 9(2)  COMP-5.
                   09 DSC-Mouse-Over             PIC 9(2)  COMP-5.
                   09 FILLER                     PIC X(3).

                07 DSC-Keyboard-Event-Data
                                   REDEFINES DSC-Mouse-Event-Data.
                   09 DSC-Char-1.
                      11 DSC-Byte-1              PIC 9(2)  COMP-5.
                   09 DSC-Char-2.
                      11 DSC-Byte-2              PIC 9(2)  COMP-5.
                   09 FILLER                     PIC X(8).

                07 DSC-Window-Event-Data REDEFINES DSC-Mouse-Event-Data.
                   09 DSC-Window-X               PIC S9(9) COMP-5.
                   09 DSC-Window-Y               PIC S9(9) COMP-5.
                   09 DSC-Window-Command         PIC 9(2)  COMP-5.
                   09 FILLER                     PIC X.
             05 DSC-Param-Data.
                07 DSC-Param-Length-1            PIC 9(4)  COMP-5.
                07 DSC-Param-Length-2            PIC 9(4)  COMP-5.
                07 DSC-Param-Length-3            PIC 9(4)  COMP-5.
                07 DSC-Param-Length-4            PIC 9(4)  COMP-5.
                07 DSC-Param-Length-5            PIC 9(4)  COMP-5.
                07 DSC-Param-Length-6            PIC 9(4)  COMP-5.
                07 DSC-Param-Length-7            PIC 9(4)  COMP-5.
                07 DSC-Param-Length-8            PIC 9(4)  COMP-5.
