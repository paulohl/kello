      * ARQUIVO DE OBS E INSTRUÇÕES DE VÍDEO - PARTE CABECALHO
      * SE TIPO = 1-EDIÇÃO   2-CÓPIA   3-INSERT
       FD  VID110.
       01  REG-VID110.
           05  CONTRATO-V110           PIC 9(4).
           05  ITEM-V110               PIC 9(2).
           05  TIPO-V110               PIC 9.
           05  FUNCIONARIO-V110        PIC 9(6).
      *    SE TIPO = 1 FUNCIONARIO = EDITOR
           05  REVISOR-V110            PIC 9(6).
           05  DATA-V110               PIC 9(8).
           05  HORARIO-V110            PIC 9(4).
           05  USUARIO-V110            PIC X(5).
