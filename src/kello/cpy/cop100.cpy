      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  EF1                             PIC 9(8) COMP-X VALUE 2.
       03  EF53                            PIC 9(8) COMP-X VALUE 3.
       03  TAB1                            PIC 9(8) COMP-X VALUE 4.
       03  PB1                             PIC 9(8) COMP-X VALUE 5.
       03  PB2                             PIC 9(8) COMP-X VALUE 6.
       03  EF3                             PIC 9(8) COMP-X VALUE 7.
       03  EF4                             PIC 9(8) COMP-X VALUE 8.
       03  EF5                             PIC 9(8) COMP-X VALUE 9.
       03  EF6                             PIC 9(8) COMP-X VALUE 10.
       03  EF7                             PIC 9(8) COMP-X VALUE 11.
       03  EF8                             PIC 9(8) COMP-X VALUE 12.
       03  EF9                             PIC 9(8) COMP-X VALUE 13.
       03  EF10                            PIC 9(8) COMP-X VALUE 14.
       03  EF11                            PIC 9(8) COMP-X VALUE 15.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 16.
       03  EF12                            PIC 9(8) COMP-X VALUE 17.
       03  EF13                            PIC 9(8) COMP-X VALUE 18.
       03  EF14                            PIC 9(8) COMP-X VALUE 19.
       03  EF15                            PIC 9(8) COMP-X VALUE 20.
       03  EF16                            PIC 9(8) COMP-X VALUE 21.
       03  EF17                            PIC 9(8) COMP-X VALUE 22.
       03  EF18                            PIC 9(8) COMP-X VALUE 23.
       03  GBOX2                           PIC 9(8) COMP-X VALUE 24.
       03  EF19                            PIC 9(8) COMP-X VALUE 25.
       03  EF20                            PIC 9(8) COMP-X VALUE 26.
       03  EF21                            PIC 9(8) COMP-X VALUE 27.
       03  EF22                            PIC 9(8) COMP-X VALUE 28.
       03  EF23                            PIC 9(8) COMP-X VALUE 29.
       03  EF24                            PIC 9(8) COMP-X VALUE 30.
       03  EF25                            PIC 9(8) COMP-X VALUE 31.
       03  PB3                             PIC 9(8) COMP-X VALUE 32.
       03  EF28                            PIC 9(8) COMP-X VALUE 33.
       03  PB6                             PIC 9(8) COMP-X VALUE 34.
       03  PB10                            PIC 9(8) COMP-X VALUE 35.
       03  EF33                            PIC 9(8) COMP-X VALUE 36.
       03  GBOX3                           PIC 9(8) COMP-X VALUE 37.
       03  EF34                            PIC 9(8) COMP-X VALUE 38.
       03  PB11                            PIC 9(8) COMP-X VALUE 39.
       03  EF64                            PIC 9(8) COMP-X VALUE 40.
       03  EF77                            PIC 9(8) COMP-X VALUE 41.
       03  EF26                            PIC 9(8) COMP-X VALUE 42.
       03  EF80                            PIC 9(8) COMP-X VALUE 43.
       03  EF81                            PIC 9(8) COMP-X VALUE 44.
       03  EF82                            PIC 9(8) COMP-X VALUE 45.
       03  MBOX6                           PIC 9(8) COMP-X VALUE 46.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 47.
       03  MBOX1                           PIC 9(8) COMP-X VALUE 48.
       03  AJUDA                           PIC 9(8) COMP-X VALUE 49.
       03  PB9                             PIC 9(8) COMP-X VALUE 50.
       03  WIN6                            PIC 9(8) COMP-X VALUE 51.
       03  MLE1                            PIC 9(8) COMP-X VALUE 52.
       03  PB5                             PIC 9(8) COMP-X VALUE 53.
       03  PB14                            PIC 9(8) COMP-X VALUE 54.
       03  PB4                             PIC 9(8) COMP-X VALUE 55.
       03  EF2                             PIC 9(8) COMP-X VALUE 56.
       03  EF27                            PIC 9(8) COMP-X VALUE 57.
       03  WIN1                            PIC 9(8) COMP-X VALUE 58.
       03  RB1                             PIC 9(8) COMP-X VALUE 59.
       03  RB2                             PIC 9(8) COMP-X VALUE 60.
       03  EF29                            PIC 9(8) COMP-X VALUE 61.
       03  EF30                            PIC 9(8) COMP-X VALUE 62.
       03  EF31                            PIC 9(8) COMP-X VALUE 63.
       03  EF32                            PIC 9(8) COMP-X VALUE 64.
       03  EF68                            PIC 9(8) COMP-X VALUE 65.
       03  EF71                            PIC 9(8) COMP-X VALUE 66.
       03  EF74                            PIC 9(8) COMP-X VALUE 67.
       03  EF78                            PIC 9(8) COMP-X VALUE 68.
       03  EF79                            PIC 9(8) COMP-X VALUE 69.
       03  PB7                             PIC 9(8) COMP-X VALUE 70.
       03  PB8                             PIC 9(8) COMP-X VALUE 71.
       03  GBOX5                           PIC 9(8) COMP-X VALUE 72.
       03  CB11                            PIC 9(8) COMP-X VALUE 73.
       03  PB17                            PIC 9(8) COMP-X VALUE 74.
       03  MBOX2                           PIC 9(8) COMP-X VALUE 75.
       03  WIN2                            PIC 9(8) COMP-X VALUE 76.
       03  GBOX4                           PIC 9(8) COMP-X VALUE 77.
       03  EF35                            PIC 9(8) COMP-X VALUE 78.
       03  EF36                            PIC 9(8) COMP-X VALUE 79.
       03  EF37                            PIC 9(8) COMP-X VALUE 80.
       03  EF38                            PIC 9(8) COMP-X VALUE 81.
       03  EF39                            PIC 9(8) COMP-X VALUE 82.
       03  EF40                            PIC 9(8) COMP-X VALUE 83.
       03  EF41                            PIC 9(8) COMP-X VALUE 84.
       03  EF43                            PIC 9(8) COMP-X VALUE 85.
       03  EF44                            PIC 9(8) COMP-X VALUE 86.
       03  EF45                            PIC 9(8) COMP-X VALUE 87.
       03  EF47                            PIC 9(8) COMP-X VALUE 88.
       03  EF48                            PIC 9(8) COMP-X VALUE 89.
       03  EF49                            PIC 9(8) COMP-X VALUE 90.
       03  EF50                            PIC 9(8) COMP-X VALUE 91.
       03  EF51                            PIC 9(8) COMP-X VALUE 92.
       03  EF52                            PIC 9(8) COMP-X VALUE 93.
       03  EF54                            PIC 9(8) COMP-X VALUE 94.
       03  EF55                            PIC 9(8) COMP-X VALUE 95.
       03  EF56                            PIC 9(8) COMP-X VALUE 96.
       03  EF57                            PIC 9(8) COMP-X VALUE 97.
       03  EF58                            PIC 9(8) COMP-X VALUE 98.
       03  EF59                            PIC 9(8) COMP-X VALUE 99.
       03  EF60                            PIC 9(8) COMP-X VALUE 100.
       03  EF61                            PIC 9(8) COMP-X VALUE 101.
       03  EF62                            PIC 9(8) COMP-X VALUE 102.
       03  PB12                            PIC 9(8) COMP-X VALUE 103.
       03  EF42                            PIC 9(8) COMP-X VALUE 104.
       03  GBOX6                           PIC 9(8) COMP-X VALUE 105.
       03  EF46                            PIC 9(8) COMP-X VALUE 106.
       03  EF63                            PIC 9(8) COMP-X VALUE 107.
       03  EF65                            PIC 9(8) COMP-X VALUE 108.
       03  EF66                            PIC 9(8) COMP-X VALUE 109.
       03  EF67                            PIC 9(8) COMP-X VALUE 110.
       03  EF69                            PIC 9(8) COMP-X VALUE 111.
       03  EF70                            PIC 9(8) COMP-X VALUE 112.
       03  EF72                            PIC 9(8) COMP-X VALUE 113.
       03  EF73                            PIC 9(8) COMP-X VALUE 114.
       03  EF75                            PIC 9(8) COMP-X VALUE 115.
       03  EF76                            PIC 9(8) COMP-X VALUE 116.
       03  WIN3                            PIC 9(8) COMP-X VALUE 117.
       03  LB14                            PIC 9(8) COMP-X VALUE 118.
       03  WIN4                            PIC 9(8) COMP-X VALUE 121.
       03  CB1                             PIC 9(8) COMP-X VALUE 122.
       03  CB2                             PIC 9(8) COMP-X VALUE 123.
       03  CB3                             PIC 9(8) COMP-X VALUE 124.
       03  CB4                             PIC 9(8) COMP-X VALUE 125.
       03  CB5                             PIC 9(8) COMP-X VALUE 126.
       03  PB13                            PIC 9(8) COMP-X VALUE 127.
       03  PB15                            PIC 9(8) COMP-X VALUE 128.
       03  CB6                             PIC 9(8) COMP-X VALUE 129.
       03  CB7                             PIC 9(8) COMP-X VALUE 130.
       03  PB16                            PIC 9(8) COMP-X VALUE 131.
       03  GBOX7                           PIC 9(8) COMP-X VALUE 132.
       03  GBOX8                           PIC 9(8) COMP-X VALUE 133.
       03  CB8                             PIC 9(8) COMP-X VALUE 134.
       03  CB9                             PIC 9(8) COMP-X VALUE 135.
       03  CB10                            PIC 9(8) COMP-X VALUE 136.
       03  CB12                            PIC 9(8) COMP-X VALUE 137.
       03  CB13                            PIC 9(8) COMP-X VALUE 138.
       03  TABP1                           PIC 9(8) COMP-X VALUE 139.
       03  LB1                             PIC 9(8) COMP-X VALUE 140.
       03  TABP3                           PIC 9(8) COMP-X VALUE 141.
       03  LB2                             PIC 9(8) COMP-X VALUE 142.
       03  TABP4                           PIC 9(8) COMP-X VALUE 143.
       03  LB3                             PIC 9(8) COMP-X VALUE 144.
       03  TABP5                           PIC 9(8) COMP-X VALUE 145.
       03  LB4                             PIC 9(8) COMP-X VALUE 146.
       03  TABP2                           PIC 9(8) COMP-X VALUE 147.
       03  LB5                             PIC 9(8) COMP-X VALUE 148.
       03  TABP7                           PIC 9(8) COMP-X VALUE 149.
       03  LB7                             PIC 9(8) COMP-X VALUE 150.
       03  TABP8                           PIC 9(8) COMP-X VALUE 151.
       03  LB8                             PIC 9(8) COMP-X VALUE 152.
       03  TABP9                           PIC 9(8) COMP-X VALUE 153.
       03  LB9                             PIC 9(8) COMP-X VALUE 154.
       03  TABP10                          PIC 9(8) COMP-X VALUE 155.
       03  LB10                            PIC 9(8) COMP-X VALUE 156.
       03  TABP11                          PIC 9(8) COMP-X VALUE 157.
       03  LB11                            PIC 9(8) COMP-X VALUE 158.
       03  TABP6                           PIC 9(8) COMP-X VALUE 159.
       03  LB6                             PIC 9(8) COMP-X VALUE 160.
       03  TABP12                          PIC 9(8) COMP-X VALUE 161.
       03  LB12                            PIC 9(8) COMP-X VALUE 162.
       03  TABP13                          PIC 9(8) COMP-X VALUE 163.
       03  LB13                            PIC 9(8) COMP-X VALUE 164.
       03  TABP14                          PIC 9(8) COMP-X VALUE 165.
       03  LB15                            PIC 9(8) COMP-X VALUE 166.
