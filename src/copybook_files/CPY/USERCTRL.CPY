       01 EntryPoint-Data.
          03 EntryPanels-Session-ID     PIC 9(9) Comp-5.
          03 EntryWindow-Data           PIC X(4) Comp-5.
          03 EntryPosition-X            PIC X(4) Comp-5.
          03 EntryPosition-Y            PIC X(4) Comp-5.
          03 EntrySize-Width            PIC X(4) Comp-5.
          03 EntrySize-Height           PIC X(4) Comp-5.
          03 EntryObject                PIC X(4) Comp-5.
          03 EntryObject-Reference
                         REDEFINES EntryObject usage object reference.
          03 EntryText-Buffer           PIC X(80).
          03 EntryOCX-Data.
             05 EntryOCXProperty-Pointer pointer.
             05 EntryOCXProperty-Size    PIC X(4) Comp-5.
