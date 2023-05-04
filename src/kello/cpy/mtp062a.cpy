      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  WIN1                            PIC 9(8) COMP-X VALUE 1.
       03  EF4                             PIC 9(8) COMP-X VALUE 2.
       03  PB2                             PIC 9(8) COMP-X VALUE 3.
       03  PB3                             PIC 9(8) COMP-X VALUE 4.
       03  EF-ETIQUETA1                    PIC 9(8) COMP-X VALUE 5.
       03  EF-ETIQUETA2                    PIC 9(8) COMP-X VALUE 6.
       03  EF-ETIQUETA3                    PIC 9(8) COMP-X VALUE 7.
       03  EF-ETIQUETA4                    PIC 9(8) COMP-X VALUE 8.
       03  EF-ETIQUETA5                    PIC 9(8) COMP-X VALUE 9.
       03  EF-ETIQUETA6                    PIC 9(8) COMP-X VALUE 10.
       03  EF-ETIQUETA8                    PIC 9(8) COMP-X VALUE 11.
       03  EF-ETIQUETA7                    PIC 9(8) COMP-X VALUE 12.
       03  EF-ETIQUETA9                    PIC 9(8) COMP-X VALUE 13.
       03  EF-ETIQUETA10                   PIC 9(8) COMP-X VALUE 14.
       03  EF-ETIQUETA12                   PIC 9(8) COMP-X VALUE 15.
       03  EF-ETIQUETA11                   PIC 9(8) COMP-X VALUE 16.
       03  EF-ETIQUETA13                   PIC 9(8) COMP-X VALUE 17.
       03  EF-ETIQUETA14                   PIC 9(8) COMP-X VALUE 18.
       03  EF-ETIQUETA16                   PIC 9(8) COMP-X VALUE 19.
       03  EF-ETIQUETA15                   PIC 9(8) COMP-X VALUE 20.
       03  EF-ETIQUETA17                   PIC 9(8) COMP-X VALUE 21.
       03  EF-ETIQUETA18                   PIC 9(8) COMP-X VALUE 22.
       03  EF-ETIQUETA20                   PIC 9(8) COMP-X VALUE 23.
       03  EF-ETIQUETA19                   PIC 9(8) COMP-X VALUE 24.
       03  EF-CONTRATO                     PIC 9(8) COMP-X VALUE 25.
       03  EF-ALBUM-INI                    PIC 9(8) COMP-X VALUE 26.
       03  EF-ALBUM-FIM                    PIC 9(8) COMP-X VALUE 27.
       03  EF-QTDE-COPIAS                  PIC 9(8) COMP-X VALUE 28.
       03  PB1                             PIC 9(8) COMP-X VALUE 29.
       03  RB1                             PIC 9(8) COMP-X VALUE 30.
       03  RB2                             PIC 9(8) COMP-X VALUE 31.
       03  PB4                             PIC 9(8) COMP-X VALUE 32.
       03  PB5                             PIC 9(8) COMP-X VALUE 33.
       03  CB1                             PIC 9(8) COMP-X VALUE 34.
       03  CB2                             PIC 9(8) COMP-X VALUE 35.
       03  CB3                             PIC 9(8) COMP-X VALUE 36.
       03  CB4                             PIC 9(8) COMP-X VALUE 37.
       03  CB5                             PIC 9(8) COMP-X VALUE 38.
       03  CB6                             PIC 9(8) COMP-X VALUE 39.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 40.
