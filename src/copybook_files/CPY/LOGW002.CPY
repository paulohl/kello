      *  ARQUIVO DE LOG
       FD  LOG002.
       01  REG-LOG002.
           05  LOG2-USUARIO            PIC X(05).
           05  LOG2-PERIODO.
               10 LOG2-DATA            PIC 9(08).
               10 FILLER REDEFINES LOG2-DATA.
                  15 LOG2-ANO          PIC 9(04).
                  15 LOG2-MES          PIC 9(02).
                  15 LOG2-DIA          PIC 9(02).
               10 LOG2-HORAS           pic 9(08).
               10 FILLER REDEFINES LOG2-HORAS.
                  15 LOG2-HORA         PIC 9(02).
                  15 LOG2-MINU         PIC 9(02).
                  15 LOG2-SEGU         PIC 9(02).
                  15 LOG2-MILE         PIC 9(02).
           05  LOG2-OPERACAO           PIC X(01).     
           05  LOG2-ARQUIVO            PIC X(10).
           05  LOG2-PROGRAMA           PIC X(10).
           05  LOG2-REGISTRO           PIC X(100).
