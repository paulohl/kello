      *  ARQUIVO DE LOG
       FD  LOG001.
       01  REG-LOG001.
           05  LOG1-USUARIO            PIC X(05).
           05  LOG1-PERIODO.
               10 LOG1-DATA.
                  15 LOG1-ANO          PIC 9(04).
                  15 LOG1-MES          PIC 9(02).
                  15 LOG1-DIA          PIC 9(02).
               10 LOG1-HORAS.
                  15 LOG1-HORA         PIC 9(02).
                  15 LOG1-MINU         PIC 9(02).
                  15 LOG1-SEGU         PIC 9(02).
                  15 LOG1-MILE         PIC 9(02).
           05  LOG1-OPERACAO           PIC X(01).     
           05  LOG1-ARQUIVO            PIC X(10).
           05  LOG1-PROGRAMA           PIC X(10).
           05  LOG1-REGISTRO           PIC X(50).
