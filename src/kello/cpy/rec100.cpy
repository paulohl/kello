           SELECT RCD100 ASSIGN TO PATH-RCD100
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-RCD100
                  RECORD KEY IS ALBUM-REC
                  ALTERNATE RECORD KEY IS ALT-REC = DATA-MOVTO-REC
                                          ALBUM-REC
                  ALTERNATE RECORD KEY IS DATAVEN-REC WITH DUPLICATES.
       FD  RCD100.
       01  REG-RCD100.
           05  DATA-MOVTO-REC      PIC 9(08).
      *    DATA-MOVTO-REC - INVERTIDA
           05  ALBUM-REC           PIC 9(08).
           05  DATAVEN-REC         PIC 9(08)     COMP-3.
      *    DATAVEN-REC - INVERTIDA
           05  VISITA-REC          PIC 9.
           05  QENCADER-REC        PIC 9.
      *    CAPA OU ENCADERNACAO  -
           05  QESTOJO-REC         PIC 9.
      *    ESTOJO(MALETA DO ALBUM)
           05  QFOTOS-REC          PIC 999.
           05  QFOLHAS-REC         PIC 999.
           05  QFITAS-REC          PIC 9.
           05  QPFITA-REC          PIC 9.
      *    PORTA-FITA
           05  QCOBERTURA-REC      PIC 9(2).
           05  QABERTURA-REC       PIC 9.
           05  QPOSTER-REC         PIC 9(2).
           05  VENDEDOR-REC        PIC 9(06).
           05  PM-REC              PIC 999V99.
           05  TOTAL-REC           PIC 9(08)V99   COMP-3.
           05  TOTAL-DEF-REC       PIC 9(08)V99   COMP-3.
           05  CODPED-REC          PIC 9(05)      COMP-3.
           05  QUANTPED-REC        PIC 999.
           05  QAVULSAS-REC        PIC 999.
           05  QCOMISSAO-REC       PIC 999.
           05  TAXA-REC            PIC 99V99.
