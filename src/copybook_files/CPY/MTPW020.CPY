      *ARQUIVO DE MONTAGEM NO ALBUM
       FD  MTD020.
       01  REG-MTD020.
           05  ALBUM-MTG.
               10  CONTRATO-MTG    PIC 9(4).
               10  NRALBUM-MTG     PIC 9(4).
      *        10  SEQUEN-MTG      PIC 9(3).
           05  DATAMOV-MTG         PIC 9(8).
           05  QT-ESTOJO-MTG       PIC 9.
           05  QT-ENCADER-MTG      PIC 9.
      *    CAPA OU ENCADERNACAO
           05  QT-FOLHAS-MTG       PIC 9999.
           05  QT-FOTOS-MTG        PIC 9999.
           05  QT-FITAS-MTG        PIC 9.
           05  QT-POSTER-MTG       PIC 9.
           05  QT-PORTA-FITA-MTG   PIC 9.
           05  QT-FOTO-CD-MTG      PIC 99.
           05  QT-MOLDURA-MTG      PIC 99.
           05  QT-PORTA-DVD-MTG    PIC 99.
           05  FOGO-MTG            PIC 9. *> 0-Montagem   1-vendido
                                          *> 8-Vend-Fogo  9-Fogo
           05  DATA-FOGO-MTG       PIC 9(8).  *> DATA-INVERTIDA
           05  ANOMES-VISITA-MTG   PIC 9(6).
           05  VISITA-MTG          PIC 999.
           05  POSSE-MTG           PIC 9.
      *    1-EM ESTOQUE    2-COM VENDEDOR  3-montagem  4-revendido
           05  CODIGO-POSSE-MTG    PIC 9(6).
           05  QT-DVD-MTG          PIC 9(1).
           05  NAO-GEROU-ALBUM-MTG PIC 9(1).
           05  DATAROMANEIO-MTG    PIC 9(8).
           05  QT-BOOK-MTG         PIC 9(2).
           05  FILLER              PIC X(38).
