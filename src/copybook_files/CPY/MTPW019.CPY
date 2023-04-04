       FD  MTD019.
       01  REG-MTD019.
           05  ALBUMMT19.
               10  CONTRATO-MT19   PIC 9(4).
               10  SEQ-MT19        PIC 9(4).
           05  ALBUM-MT19 REDEFINES ALBUMMT19 PIC 9(8).
           05  DATAMOV-MT19        PIC 9(8).
           05  NOME-FORM-MT19      PIC X(30).
           05  CIDADE-MT19         PIC 9(04).
           05  CURSO-MT19          PIC 9(03).
           05  UF-MT19             PIC XX.
           05  FONE-MT19           PIC 9(8).
           05  IDENTIFICADO-MT19   PIC 9.
           05  TURMA-MT19          PIC X(03).
           05  TURNO-MT19          PIC X(10).
           05  COMP-TEL-MT19       PIC 9(01).
           05  FILLER              PIC X(29).
      * identificado = 0(nao)  1(sim)
