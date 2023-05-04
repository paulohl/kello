
       01  PARAMETROS-GRLINE.
           05 GRLINE-LINE                    PIC  9(002) VALUE 7.
           05 GRLINE-COLUMN                  PIC  9(002) VALUE 4.
           05 GRLINE-LINE-END                PIC  9(002) VALUE 20.
           05 GRLINE-COLUMN-END              PIC  9(002) VALUE 77.
           05 GRLINE-TYPE                    PIC  X(001) VALUE "B".
              88 GRLINE-BUTTONS                          VALUE "B".
              88 GRLINE-REVERSED                         VALUE "R".
           05 GRLINE-SCREENS                             VALUE SPACES.
              10 GRLINE-CHAR   OCCURS 26     PIC  X(001).
              10 GRLINE-SCREEN OCCURS 26     PIC  X(034).
           05                                            VALUE ZEROS.
              15 GRLINE-POSITION OCCURS 26.
                 20 GRLINE-POSITION-LINE     PIC  9(002).
                 20 GRLINE-POSITION-COLUMN   PIC  9(002).
           05 GRLINE-COLOR-LOW        COMP-X PIC  9(002) VALUE 112.
           05 GRLINE-COLOR-HIGH       COMP-X PIC  9(002) VALUE 127.
           05 GRLINE-OPTION                  PIC  9(003) VALUE 0.
           05 GRLINE-OPTION-CHAR             PIC  X(001) VALUE SPACE.
           05 GRLINE-MOUSE                   PIC  X(001) VALUE "N".

