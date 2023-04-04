      *  ARQUIVO DE LOG
       FD  LOG004.
       01  REG-LOG004.
           05  LOG4-USUARIO            PIC X(05).
           05  LOG4-PERIODO.
               10 LOG4-DATA.
                  15 LOG4-ANO          PIC 9(04).
                  15 LOG4-MES          PIC 9(02).
                  15 LOG4-DIA          PIC 9(02).
               10 LOG4-HORAS.
                  15 LOG4-HORA         PIC 9(02).
                  15 LOG4-MINU         PIC 9(02).
                  15 LOG4-SEGU         PIC 9(02).
                  15 LOG4-MILE         PIC 9(02).
           05  LOG4-OPERACAO           PIC X(01).     
           05  LOG4-ARQUIVO            PIC X(10).
           05  LOG4-PROGRAMA           PIC X(10).
           05  LOG4-REGISTRO           PIC X(350).
