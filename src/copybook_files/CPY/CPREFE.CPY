
       01  PARAMETROS-GRREFE.
           05 GRREFE-ALINHA                  PIC  X(001) VALUE "E".
           05 GRREFE-MMAA                    PIC  9(004) VALUE ZERO.
           05 FILLER REDEFINES GRREFE-MMAA.
              10 GRREFE-MM                   PIC  9(002).
              10 GRREFE-AA                   PIC  9(002).
           05 GRREFE-EXTENSO                 PIC  X(012) VALUE SPACES.

