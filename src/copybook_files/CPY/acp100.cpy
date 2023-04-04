      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  PB6                             PIC 9(8) COMP-X VALUE 2.
       03  GBOX2                           PIC 9(8) COMP-X VALUE 3.
       03  PB1                             PIC 9(8) COMP-X VALUE 4.
       03  PB2                             PIC 9(8) COMP-X VALUE 5.
       03  TAB1                            PIC 9(8) COMP-X VALUE 6.
       03  PB3                             PIC 9(8) COMP-X VALUE 7.
       03  MBOX1                           PIC 9(8) COMP-X VALUE 8.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 9.
       03  MBOX6                           PIC 9(8) COMP-X VALUE 10.
       03  AJUDA                           PIC 9(8) COMP-X VALUE 11.
       03  PB8                             PIC 9(8) COMP-X VALUE 12.
       03  MBOX5                           PIC 9(8) COMP-X VALUE 13.
       03  WIN1                            PIC 9(8) COMP-X VALUE 14.
       03  LB2                             PIC 9(8) COMP-X VALUE 15.
       03  PB4                             PIC 9(8) COMP-X VALUE 16.
       03  TAB-01                          PIC 9(8) COMP-X VALUE 17.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 18.
       03  GBOX3                           PIC 9(8) COMP-X VALUE 19.
       03  GBOX4                           PIC 9(8) COMP-X VALUE 20.
       03  EF13                            PIC 9(8) COMP-X VALUE 21.
       03  EF14                            PIC 9(8) COMP-X VALUE 22.
       03  EF15                            PIC 9(8) COMP-X VALUE 23.
       03  EF18                            PIC 9(8) COMP-X VALUE 24.
       03  EF16                            PIC 9(8) COMP-X VALUE 25.
       03  EF17                            PIC 9(8) COMP-X VALUE 26.
       03  EF19                            PIC 9(8) COMP-X VALUE 27.
       03  EF20                            PIC 9(8) COMP-X VALUE 28.
       03  EF21                            PIC 9(8) COMP-X VALUE 29.
       03  EF22                            PIC 9(8) COMP-X VALUE 30.
       03  EF23                            PIC 9(8) COMP-X VALUE 31.
       03  EF24                            PIC 9(8) COMP-X VALUE 32.
       03  EF25                            PIC 9(8) COMP-X VALUE 33.
       03  EF26                            PIC 9(8) COMP-X VALUE 34.
       03  EF27                            PIC 9(8) COMP-X VALUE 35.
       03  EF28                            PIC 9(8) COMP-X VALUE 36.
       03  EF29                            PIC 9(8) COMP-X VALUE 37.
       03  EF30                            PIC 9(8) COMP-X VALUE 38.
       03  EF31                            PIC 9(8) COMP-X VALUE 39.
       03  EF32                            PIC 9(8) COMP-X VALUE 40.
       03  EF33                            PIC 9(8) COMP-X VALUE 41.
       03  EF34                            PIC 9(8) COMP-X VALUE 42.
       03  EF35                            PIC 9(8) COMP-X VALUE 43.
       03  EF36                            PIC 9(8) COMP-X VALUE 44.
       03  EF37                            PIC 9(8) COMP-X VALUE 45.
       03  EF38                            PIC 9(8) COMP-X VALUE 46.
       03  EF39                            PIC 9(8) COMP-X VALUE 47.
       03  EF40                            PIC 9(8) COMP-X VALUE 48.
       03  EF41                            PIC 9(8) COMP-X VALUE 49.
       03  EF1                             PIC 9(8) COMP-X VALUE 50.
       03  EF12                            PIC 9(8) COMP-X VALUE 51.
       03  EF49                            PIC 9(8) COMP-X VALUE 52.
       03  EF55                            PIC 9(8) COMP-X VALUE 53.
       03  EF56                            PIC 9(8) COMP-X VALUE 54.
       03  EF57                            PIC 9(8) COMP-X VALUE 55.
       03  TAB-02                          PIC 9(8) COMP-X VALUE 56.
       03  GBOX6                           PIC 9(8) COMP-X VALUE 57.
       03  GBOX7                           PIC 9(8) COMP-X VALUE 58.
       03  EF42                            PIC 9(8) COMP-X VALUE 59.
       03  PB7                             PIC 9(8) COMP-X VALUE 60.
       03  EF43                            PIC 9(8) COMP-X VALUE 61.
       03  LB1                             PIC 9(8) COMP-X VALUE 62.
       03  EF46                            PIC 9(8) COMP-X VALUE 63.
       03  PB9                             PIC 9(8) COMP-X VALUE 64.
       03  LB4                             PIC 9(8) COMP-X VALUE 65.
       03  TAB-03                          PIC 9(8) COMP-X VALUE 66.
       03  GBOX8                           PIC 9(8) COMP-X VALUE 67.
       03  EF44                            PIC 9(8) COMP-X VALUE 68.
       03  EF45                            PIC 9(8) COMP-X VALUE 69.
       03  PB5                             PIC 9(8) COMP-X VALUE 70.
       03  LB3                             PIC 9(8) COMP-X VALUE 71.
       03  GBOX9                           PIC 9(8) COMP-X VALUE 72.
       03  EF47                            PIC 9(8) COMP-X VALUE 73.
       03  PB10                            PIC 9(8) COMP-X VALUE 74.
       03  LB5                             PIC 9(8) COMP-X VALUE 75.
       03  TAB-04                          PIC 9(8) COMP-X VALUE 76.
       03  EF48                            PIC 9(8) COMP-X VALUE 77.
       03  PB12                            PIC 9(8) COMP-X VALUE 78.
       03  LB6                             PIC 9(8) COMP-X VALUE 79.
       03  TAB-05                          PIC 9(8) COMP-X VALUE 80.
       03  EF-ATENDIMENTO                  PIC 9(8) COMP-X VALUE 81.
       03  EF2                             PIC 9(8) COMP-X VALUE 82.
       03  EF3                             PIC 9(8) COMP-X VALUE 83.
       03  EF4                             PIC 9(8) COMP-X VALUE 84.
       03  EF5                             PIC 9(8) COMP-X VALUE 85.
       03  EF6                             PIC 9(8) COMP-X VALUE 86.
       03  EF7                             PIC 9(8) COMP-X VALUE 87.
       03  EF8                             PIC 9(8) COMP-X VALUE 88.
       03  EF9                             PIC 9(8) COMP-X VALUE 89.
       03  EF11                            PIC 9(8) COMP-X VALUE 90.
       03  EF10                            PIC 9(8) COMP-X VALUE 91.
       03  GBOX5                           PIC 9(8) COMP-X VALUE 92.
       03  SB1                             PIC 9(8) COMP-X VALUE 93.
       03  EF50                            PIC 9(8) COMP-X VALUE 94.
       03  EF51                            PIC 9(8) COMP-X VALUE 95.
       03  EF52                            PIC 9(8) COMP-X VALUE 96.
       03  EF53                            PIC 9(8) COMP-X VALUE 97.
       03  EF54                            PIC 9(8) COMP-X VALUE 98.
       03  EF58                            PIC 9(8) COMP-X VALUE 99.
       03  PB11                            PIC 9(8) COMP-X VALUE 100.
       03  LB7                             PIC 9(8) COMP-X VALUE 101.