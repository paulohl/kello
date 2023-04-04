       01  PARAMETROS-GRBOXF.
           05 GRBOXF-FUNCTION                PIC  X(001) VALUE "S".
              88 GRBOXF-SHOW                             VALUE "S" "s".
              88 GRBOXF-POP-UP                           VALUE "P" "p".
           05 GRBOXF-PROGRAM                 PIC  X(008) VALUE "......".
           05 GRBOXF-WORK-AREA               PIC  X(050) VALUE SPACES.
           05 GRBOXF-LINE                    PIC  9(002) VALUE 1.
           05 GRBOXF-COLUMN                  PIC  9(002) VALUE 1.
           05 GRBOXF-TYPE                    PIC  9(001) VALUE 0.
           05 GRBOXF-VERTICAL-LENGTH         PIC  9(002) VALUE 0.
           05 GRBOXF-HORIZONTAL-LENGTH       PIC  9(002) VALUE 0.
           05 GRBOXF-OPTION                  PIC  X(040) VALUE SPACE.
           05 GRBOXF-TITLE                   PIC  X(078) VALUE SPACES.
           05 GRBOXF-COLOR-SCREEN     COMP-X PIC  9(002) VALUE 110.
           05 GRBOXF-COLOR-FRAME      COMP-X PIC  9(002) VALUE 110.
           05 GRBOXF-COLOR-SHADE      COMP-X PIC  9(002) VALUE 008.
           05 GRBOXF-COLOR-BARR-MENU  COMP-X PIC  9(002) VALUE 112.
           05 GRBOXF-ORDER                   PIC  9(001) VALUE 0.
           05 GRBOXF-RETURN                  PIC  9(001) VALUE 1.
           05 GRBOXF-STRING-1                PIC  X(040) VALUE SPACES.
           05 GRBOXF-STRING-1-LENGTH         PIC  9(002) VALUE 0.
           05 GRBOXF-STRING-2                PIC  X(040) VALUE SPACES.
           05 GRBOXF-STRING-2-LENGTH         PIC  9(002) VALUE 0.
