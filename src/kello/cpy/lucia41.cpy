           SELECT COD041 ASSIGN TO PATH-COD041
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CO41 = NR-CONTRATO-CO41
                                             CURSO-CO41 TURMA-CO41
                  STATUS IS ST-COD041.
      * cadastro de turma/contrato
       FD  COD041.
       01  REG-COD041.
           05  NR-CONTRATO-CO41  PIC 9(4).
           05  CURSO-CO41        PIC 9(3).
           05  TURMA-CO41        PIC XX.
           05  NR-PREV-FORM-CO41 PIC 9(4).
           05  LISTA-ALUNOS-CO41 PIC 9.
      *    0-Não está OK    1-Lista completa
           05  RESPONSAVEL-CO41  PIC 9(4).
      *    Nr-contrato + responsavel = nome presidente/responsavel
           05  FOTO-TURMA-CO41   PIC 9.
           05  MEDIDA-BECA-CO41  PIC 9.
      *    Foto-turma e Medida-beca = 0-não  1-sim
           05  COR-FAIXA-CO41    PIC X(10).
           05  COR-CANUDO-CO41   PIC X(10).
           05  TIPO-ALBUM-CO41   PIC 9(5).
