       FD  MTD019F.
       01  REG-MTD019F.
           05  ALBUMMT19F.
               10  CONTRATO-MT19F   PIC 9(4).
               10  SEQ-MT19F        PIC 9(4).
           05  ALBUM-MT19F REDEFINES ALBUMMT19F PIC 9(8).
           05  IMAGEM-MT19F         PIC X(255).
           05  FILLER               PIC X(30).
