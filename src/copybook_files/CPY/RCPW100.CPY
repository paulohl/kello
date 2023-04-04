       FD  RCD100.
       01  REG-RCD100.
           05  DATA-MOVTO-REC       PIC 9(08).
      *    DATA-MOVTO-REC - INVERTIDA
           05  CHAVE-ALBUM-REC.
               10  ALBUM-REC        PIC 9(08).
      *        10  SEQ-REC          PIC 9(03).
           05  DATAVEN-REC          PIC 9(08)     COMP-3.
      *    DATAVEN-REC - INVERTIDA
           05  VISITA-REC           PIC 9.
           05  QENCADER-REC         PIC 9.
      *    CAPA OU ENCADERNACAO  -
           05  QESTOJO-REC          PIC 9.
      *    ESTOJO(MALETA DO ALBUM)
           05  QFOTOS-REC           PIC 999.
           05  QFOLHAS-REC          PIC 999.
           05  QFITAS-REC           PIC 9.
           05  QPFITA-REC           PIC 9.
      *    PORTA-FITA
           05  QCOBERTURA-REC       PIC 9(2).
           05  QABERTURA-REC        PIC 9.
           05  QPOSTER-REC          PIC 9(2).
           05  VENDEDOR-REC         PIC 9(06).
           05  PM-REC               PIC 999V99.
           05  TOTAL-REC            PIC 9(08)V99   COMP-3.
           05  TOTAL-DEF-REC        PIC 9(08)V99   COMP-3.
           05  QT-COBERTURA-DVD-REC PIC 9(05)      COMP-3.
           05  QDVD-REC             PIC 999.
           05  QFOTO-CD-REC         PIC 99.
           05  QMOLDURA-REC         PIC 99.
           05  QPORTA-DVD-REC       PIC 99.
           05  QAVULSAS-REC         PIC 999.
           05  QCOMISSAO-REC        PIC 999.
           05  TAXA-REC             PIC 99V99.
           05  QFOTOS-EXTRA1-REC    PIC 9(03).
           05  QFOTOS-EXTRA2-REC    PIC 9(03).
           05  POSTER-ABERTURA-REC  PIC 9(01).
           05  POSTER-XEROX-REC     PIC 9(01).
           05  EXTRA-ENVIADO1-REC   PIC X(01).
           05  EXTRA-ENVIADO2-REC   PIC X(01).
      * GERADO-KAC = "S" => foi gerado no KAC
           05  GERADO-KAC           PIC X(01).
      * NÚMERO DO ATENDIMENTO DO KAC
           05  KAC-ATENDIMENTO      PIC 9(08).
           05  QBOOK-REC            PIC 9(02).
           05  PEDIDO-EXTRA-REC     PIC 9(01).
           05  MEMBRO-COMISSAO-REC  PIC 9(01).
           04  FILLER               PIC X(07).
