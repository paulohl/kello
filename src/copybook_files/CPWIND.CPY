
       01  PARAMETROS-GRWIND.
           05 GRWIND-LINE                    PIC  9(002) VALUE 0.
           05 GRWIND-COLUMN                  PIC  9(002) VALUE 0.
           05 GRWIND-TYPE                    PIC  9(001) VALUE 0.
           05 GRWIND-VERTICAL-LENGTH         PIC  9(002) VALUE 0.
           05 GRWIND-HORIZONTAL-LENGTH       PIC  9(002) VALUE 0.
           05 GRWIND-COLOR-SCREEN     COMP-X PIC  9(002) VALUE 007.
           05 GRWIND-COLOR-FRAME      COMP-X PIC  9(002) VALUE 031.
           05 GRWIND-COLOR-SHADE      COMP-X PIC  9(002) VALUE 008.
           05 GRWIND-FUNCTION                PIC  X(005) VALUE "OPEN".
              88 GRWIND-ABRE  VALUE "OPEN".
              88 GRWIND-FECHA VALUE "CLOSE".
              88 GRWIND-POPUP VALUE "POPUP".

