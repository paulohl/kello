       FD  MTD022.
       01  REG-MTD022.
           05  ALBUMMT22.
               10  CONTRATO-MT22   PIC 9(4).
               10  SEQ-MT22        PIC 9(4).
           05  ALBUM-MT22 REDEFINES ALBUMMT22 PIC 9(8).
           05  NOME-FORM-MT22      PIC X(30).
