      * cadastro de turma/contrato
       FD  COD041.
       01  REG-COD041.
           05  NR-CONTRATO-CO41   PIC 9(4).
           05  CURSO-CO41         PIC 9(3).
           05  TURMA-CO41         PIC XX.
           05  NR-PREV-FORM-CO41  PIC 9(4).
           05  LISTA-ALUNOS-CO41  PIC 9.
      *    0-Não está OK    1-Lista completa
           05  RESPONSAVEL-CO41   PIC 9(4).
      *    Nr-contrato + responsavel = nome presidente/responsavel
           05  FOTO-TURMA-CO41    PIC 9.
           05  MEDIDA-BECA-CO41   PIC 9.
      *    Foto-turma e Medida-beca = 0-não  1-sim
           05  COR-FAIXA-CO41     PIC X(10).
           05  COR-CANUDO-CO41    PIC X(10).
           05  TIPO-ALBUM-CO41    PIC 9(5).
           05  TURNO-CO41         PIC X(10).
           05  CHECK-CONVITE-CO41 PIC 9(01).
      *    Check-convite = 0-não  1-sim
           05  FILLER             PIC X(49).
