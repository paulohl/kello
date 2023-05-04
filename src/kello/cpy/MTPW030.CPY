      *Formando x evento x qtde de imagens
       FD  MTD030.
       01  REG-MTD030.
           05  ALBUMMT30.
               10  CONTRATO-MT30   PIC 9(4).
               10  SEQ-MT30        PIC 9(4).
           05  ALBUM-MT30 REDEFINES ALBUMMT30 PIC 9(8).
           05  ITEM-EVENTO-MT30    PIC 9(3).
           05  COD-EVENTO-MT30     PIC 9(5).
           05  QTDE-IMAGENS-MT30   PIC 9(6).
           05  FILLER              PIC X(20).
