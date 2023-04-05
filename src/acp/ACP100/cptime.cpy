
       01  PARAMETROS-GRTIME.
           05 GRTIME-TYPE                    PIC  9(002) VALUE 2.
              88 GRTIME-NORMAL                           VALUE 1.
              88 GRTIME-REVERSED                         VALUE 2.
           05 GRTIME-FUNCTION                PIC  9(002) VALUE 0.
              88 GRTIME-ADD-DAYS                         VALUE 1.
              88 GRTIME-EDIT                             VALUE 2.
              88 GRTIME-INTERVAL                         VALUE 3.
              88 GRTIME-REVERSE                          VALUE 4.
              88 GRTIME-SUBTRACT-DAYS                    VALUE 5.
              88 GRTIME-TODAY                            VALUE 6.
              88 GRTIME-VALIDATE                         VALUE 7.
              88 GRTIME-WEEK                             VALUE 8.
              88 GRTIME-MIN-YEAR                         VALUE 9.
              88 GRTIME-MAX-YEAR                         VALUE 0.
           05 GRTIME-DATE                    PIC  9(008) VALUE 0.
           05 GRTIME-TIME                    PIC  9(006) VALUE 0.
           05 GRTIME-DAYS                    PIC  9(007) VALUE 0.
           05 GRTIME-DATE-FINAL              PIC  9(008) VALUE 0.
           05 GRTIME-TIME-FINAL              PIC  9(006) VALUE 0.
           05 GRTIME-DAYS-FINAL              PIC S9(007) VALUE 0.
           05 GRTIME-TOTAL-TIME              PIC  9(012).
           05 GRTIME-TOTAL-HOURS             PIC  9(009).
           05 GRTIME-TOTAL-MINUTES           PIC  9(002).
           05 GRTIME-TOTAL-SECONDS           PIC  9(002).
           05 GRTIME-YEARS-OLD               PIC S9(004).
           05 GRTIME-MOUNTHS-OLD             PIC S9(002).
           05 GRTIME-DAYS-OLD                PIC S9(002).
           05 GRTIME-DATE-EDITED             PIC  X(010) VALUE SPACES.
           05 GRTIME-DATE-EDITED-LONG        PIC  X(023) VALUE SPACES.
           05 GRTIME-MOUNTH-EDITED           PIC  X(014) VALUE SPACES.
           05 GRTIME-TIME-EDITED             PIC  X(008) VALUE SPACES.
           05 GRTIME-WEEK-NUM                PIC  9(001) VALUE 0.
           05 GRTIME-WEEK-CHAR               PIC  X(007) VALUE SPACES.

