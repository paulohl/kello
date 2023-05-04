      *  ARQUIVO DE LOG
       FD  LOG003.
       01  REG-LOG003.
           05  LOG3-USUARIO            PIC X(05).
           05  LOG3-PERIODO.
               10 LOG3-DATA.
                  15 LOG3-ANO          PIC 9(04).
                  15 LOG3-MES          PIC 9(02).
                  15 LOG3-DIA          PIC 9(02).
               10 LOG3-HORAS.
                  15 LOG3-HORA         PIC 9(02).
                  15 LOG3-MINU         PIC 9(02).
                  15 LOG3-SEGU         PIC 9(02).
                  15 LOG3-MILE         PIC 9(02).
           05  LOG3-OPERACAO           PIC X(01).     
           05  LOG3-ARQUIVO            PIC X(10).
           05  LOG3-PROGRAMA           PIC X(10).
           05  LOG3-REGISTRO           PIC X(200).
