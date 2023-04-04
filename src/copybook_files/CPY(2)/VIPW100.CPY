      *MOVIMENTO E RECEBIMENTO DE FITAS BRUTAS
       FD  VID100.
       01  REG-VID100.
           05  DATA-MOVTO-V100           PIC 9(8).
           05  SEQ-V100                  PIC 9(3).
           05  NR-FITAS-V100.
               10 CONTRATO-V100          PIC 9(4).
               10 NR-FITA-V100           PIC 9(5).
           05  CURSO-V100                PIC X(10).
           05  EVENTO-V100               PIC 9(5).
           05  DATA-EVENTO-V100          PIC 9(8).
           05  CINEGRAFISTA-V100         PIC 9(6).
           05  FILMADORA-V100            PIC 9(2).
           05  LOCALIZACAO-V100          PIC X(5).
           05  DIGITADOR-V100            PIC X(4).
           05  IDENTIFICADOR-V100        PIC 9(09).
           05  QTDE-ARQUIVOS-V100        PIC 9(03).
           05  FILLER                    PIC X(88).
