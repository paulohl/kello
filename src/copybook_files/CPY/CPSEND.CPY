
       01  PARAMETROS-GRSEND.
           05 GRSEND-MSG                     PIC  X(060)  VALUE SPACES.
           05 GRSEND-SCREENS                              VALUE SPACES.
              10 GRSEND-CHAR   OCCURS 8      PIC  X(001).
              10 GRSEND-SCREEN OCCURS 8      PIC  X(010).
           05 GRSEND-OPTION                  PIC  9(001)  VALUE 0.
           05 GRSEND-OPTION-CHAR             PIC  X(001)  VALUE SPACE.

