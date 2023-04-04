
      *****************************************************************
      *
      *  Dialog System Event Block
      *
      *  For use with Dialog System Version 2.
      *
      *****************************************************************

       01 DS-Event-Block.
       78 DS-EB-Start         VALUE NEXT.
          03 DS-Ancestor                     PIC 9(9)  COMP-5.
          03 DS-Descendant                   PIC 9(9)  COMP-5.
          03 DS-Screenset-ID                 PIC X(8).
          03 DS-Event-Screenset-Details.
             05 DS-Event-Screenset-ID           PIC X(8).
             05 DS-Event-Screenset-Instance-No  PIC 9(2) COMP-X.

          03 DS-Event-Reserved               PIC X(11).

          03 DS-Event-Type                   PIC 9(4)  COMP-5.
          03 DS-Event-Data.

             05 DS-Gadget-Event-Data.
                07 DS-Gadget-Type            PIC 9(2)  COMP-X.
                07 DS-Gadget-Command         PIC 9(2)  COMP-X.
                07 DS-Gadget-Id              PIC 9(4)  COMP-5.
                07 DS-Gadget-Return          PIC 9(4)  COMP-5.

             05 DS-Mouse-Event-Data.
                07 DS-Mouse-X                PIC S9(4) COMP-5.
                07 DS-Mouse-Y                PIC S9(4) COMP-5.
                07 DS-Mouse-State            PIC 9(2)  COMP-X.
                07 DS-Mouse-Moved-Flag       PIC 9(2)  COMP-X.
                07 DS-Mouse-Over             PIC 9(2)  COMP-X.
                07 FILLER                    PIC X(3).
       78 DS-EB-Size          VALUE NEXT - DS-EB-Start.

             05 DS-Keyboard-Event-Data REDEFINES DS-Mouse-Event-Data.
                07 DS-Char-1.
                   09 DS-Byte-1              PIC 9(2)  COMP-X.
                07 DS-Char-2.
                   09 DS-Byte-2              PIC 9(2)  COMP-X.
                07                            PIC X(2).
                07 DS-Shift-Lock-States      PIC 9(9) COMP-5.
                07                            PIC X(2).

             05 DS-Window-Event-Data REDEFINES DS-Mouse-Event-Data.
                07 DS-Window-X               PIC S9(9) COMP-5.
                07 DS-Window-Y               PIC S9(9) COMP-5.
                07 DS-Window-Command         PIC 9(2)  COMP-X.
                07 FILLER                    PIC X.
