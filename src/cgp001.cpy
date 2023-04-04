      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  PRINCIPAL                       PIC 9(8) COMP-X VALUE 1.
       03  TAB1                            PIC 9(8) COMP-X VALUE 2.
       03  MBOX6                           PIC 9(8) COMP-X VALUE 3.
       03  MBOX-ERROS                      PIC 9(8) COMP-X VALUE 4.
       03  MBOX1                           PIC 9(8) COMP-X VALUE 5.
       03  AJUDA                           PIC 9(8) COMP-X VALUE 6.
       03  PB9                             PIC 9(8) COMP-X VALUE 7.
       03  WIN1                            PIC 9(8) COMP-X VALUE 8.
       03  RB3                             PIC 9(8) COMP-X VALUE 9.
       03  RB4                             PIC 9(8) COMP-X VALUE 10.
       03  RB1                             PIC 9(8) COMP-X VALUE 11.
       03  SB1                             PIC 9(8) COMP-X VALUE 12.
       03  EF22                            PIC 9(8) COMP-X VALUE 13.
       03  PB10                            PIC 9(8) COMP-X VALUE 14.
       03  PB11                            PIC 9(8) COMP-X VALUE 15.
       03  GBOX6                           PIC 9(8) COMP-X VALUE 16.
       03  PB15                            PIC 9(8) COMP-X VALUE 17.
       03  EF-CODIGO                       PIC 9(8) COMP-X VALUE 18.
       03  EF21                            PIC 9(8) COMP-X VALUE 19.
       03  PB16                            PIC 9(8) COMP-X VALUE 20.
       03  LB2                             PIC 9(8) COMP-X VALUE 21.
       03  GBOX9                           PIC 9(8) COMP-X VALUE 22.
       03  RB2                             PIC 9(8) COMP-X VALUE 23.
       03  RB5                             PIC 9(8) COMP-X VALUE 24.
       03  RB6                             PIC 9(8) COMP-X VALUE 25.
       03  EF23                            PIC 9(8) COMP-X VALUE 26.
       03  EF26                            PIC 9(8) COMP-X VALUE 27.
       03  EF28                            PIC 9(8) COMP-X VALUE 28.
       03  EF27                            PIC 9(8) COMP-X VALUE 29.
       03  PB20                            PIC 9(8) COMP-X VALUE 30.
       03  PB21                            PIC 9(8) COMP-X VALUE 31.
       03  PB22                            PIC 9(8) COMP-X VALUE 32.
       03  TABP1                           PIC 9(8) COMP-X VALUE 33.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 34.
       03  D-CODIGO                        PIC 9(8) COMP-X VALUE 35.
       03  D-NOME                          PIC 9(8) COMP-X VALUE 36.
       03  EF1                             PIC 9(8) COMP-X VALUE 37.
       03  SB2                             PIC 9(8) COMP-X VALUE 38.
       03  SB3                             PIC 9(8) COMP-X VALUE 39.
       03  EF30                            PIC 9(8) COMP-X VALUE 40.
       03  GBOX3                           PIC 9(8) COMP-X VALUE 41.
       03  GBOX4                           PIC 9(8) COMP-X VALUE 42.
       03  CB1                             PIC 9(8) COMP-X VALUE 43.
       03  CB2                             PIC 9(8) COMP-X VALUE 44.
       03  CB3                             PIC 9(8) COMP-X VALUE 45.
       03  CB4                             PIC 9(8) COMP-X VALUE 46.
       03  CB5                             PIC 9(8) COMP-X VALUE 47.
       03  CB6                             PIC 9(8) COMP-X VALUE 48.
       03  CB7                             PIC 9(8) COMP-X VALUE 49.
       03  CB8                             PIC 9(8) COMP-X VALUE 50.
       03  CB9                             PIC 9(8) COMP-X VALUE 51.
       03  CB10                            PIC 9(8) COMP-X VALUE 52.
       03  PB1                             PIC 9(8) COMP-X VALUE 53.
       03  PB2                             PIC 9(8) COMP-X VALUE 54.
       03  PB3                             PIC 9(8) COMP-X VALUE 55.
       03  PB6                             PIC 9(8) COMP-X VALUE 56.
       03  PB4                             PIC 9(8) COMP-X VALUE 57.
       03  PB5                             PIC 9(8) COMP-X VALUE 58.
       03  LB1                             PIC 9(8) COMP-X VALUE 59.
       03  PB13                            PIC 9(8) COMP-X VALUE 60.
       03  EF11                            PIC 9(8) COMP-X VALUE 61.
       03  CB12                            PIC 9(8) COMP-X VALUE 62.
       03  CB13                            PIC 9(8) COMP-X VALUE 63.
       03  EF31                            PIC 9(8) COMP-X VALUE 64.
       03  TABP2                           PIC 9(8) COMP-X VALUE 65.
       03  EF3                             PIC 9(8) COMP-X VALUE 66.
       03  D-CIDADE1-J                     PIC 9(8) COMP-X VALUE 68.
       03  D-NOME-CID1-J                   PIC 9(8) COMP-X VALUE 69.
       03  D-CIDADE2-J                     PIC 9(8) COMP-X VALUE 75.
       03  D-NOME-CID2-J                   PIC 9(8) COMP-X VALUE 76.
       03  EF-CGC                          PIC 9(8) COMP-X VALUE 85.
       03  GBOX5                           PIC 9(8) COMP-X VALUE 90.
       03  PB7                             PIC 9(8) COMP-X VALUE 94.
       03  D-DDD1-J                        PIC 9(8) COMP-X VALUE 95.
       03  D-DDD2-J                        PIC 9(8) COMP-X VALUE 96.
       03  D-DDD0-J                        PIC 9(8) COMP-X VALUE 97.
       03  PB17                            PIC 9(8) COMP-X VALUE 98.
       03  TABP3                           PIC 9(8) COMP-X VALUE 99.
       03  ENDF-D                          PIC 9(8) COMP-X VALUE 100.
       03  D-CIDADE1-F                     PIC 9(8) COMP-X VALUE 102.
       03  D-NOME-CID1-F                   PIC 9(8) COMP-X VALUE 103.
       03  D-CIDADE2-F                     PIC 9(8) COMP-X VALUE 108.
       03  D-NOME-CID2-F                   PIC 9(8) COMP-X VALUE 109.
       03  EF-CPF                          PIC 9(8) COMP-X VALUE 117.
       03  GBOX2                           PIC 9(8) COMP-X VALUE 120.
       03  PB8                             PIC 9(8) COMP-X VALUE 123.
       03  D-DDD1-F                        PIC 9(8) COMP-X VALUE 124.
       03  D-DDD2-F                        PIC 9(8) COMP-X VALUE 125.
       03  D-DDD0-F                        PIC 9(8) COMP-X VALUE 126.
       03  TABP5                           PIC 9(8) COMP-X VALUE 127.
       03  GBOX8                           PIC 9(8) COMP-X VALUE 128.
       03  EF17                            PIC 9(8) COMP-X VALUE 130.
       03  EF18                            PIC 9(8) COMP-X VALUE 131.
       03  EF19                            PIC 9(8) COMP-X VALUE 132.
       03  EF20                            PIC 9(8) COMP-X VALUE 133.
       03  EF12                            PIC 9(8) COMP-X VALUE 134.
       03  EF14                            PIC 9(8) COMP-X VALUE 135.
       03  PB14                            PIC 9(8) COMP-X VALUE 136.
       03  TABP6                           PIC 9(8) COMP-X VALUE 137.
       03  EF100                           PIC 9(8) COMP-X VALUE 138.
       03  EF89                            PIC 9(8) COMP-X VALUE 139.
       03  EF101                           PIC 9(8) COMP-X VALUE 140.
       03  EF104                           PIC 9(8) COMP-X VALUE 141.
       03  EF103                           PIC 9(8) COMP-X VALUE 142.
       03  EF102                           PIC 9(8) COMP-X VALUE 143.
       03  EF29                            PIC 9(8) COMP-X VALUE 144.
       03  EF24                            PIC 9(8) COMP-X VALUE 145.
       03  EF25                            PIC 9(8) COMP-X VALUE 146.
       03  EF2                             PIC 9(8) COMP-X VALUE 147.
       03  PB18                            PIC 9(8) COMP-X VALUE 148.
       03  LB3                             PIC 9(8) COMP-X VALUE 149.
       03  PB19                            PIC 9(8) COMP-X VALUE 150.
       03  RB7                             PIC 9(8) COMP-X VALUE 151.
       03  RB8                             PIC 9(8) COMP-X VALUE 152.
       03  CB11                            PIC 9(8) COMP-X VALUE 153.
