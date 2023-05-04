      *  ARQUIVO DE LOG
       FD  LOG005.
       01  REG-LOG005.
           05  LOG5-USUARIO            PIC X(05).
           05  LOG5-PERIODO.
               10 LOG5-DATA.
                  15 LOG5-ANO          PIC 9(05).
                  15 LOG5-MES          PIC 9(02).
                  15 LOG5-DIA          PIC 9(02).
               10 LOG5-HORAS.
                  15 LOG5-HORA         PIC 9(02).
                  15 LOG5-MINU         PIC 9(02).
                  15 LOG5-SEGU         PIC 9(02).
                  15 LOG5-MILE         PIC 9(02).
           05  LOG5-OPERACAO           PIC X(01).
           05  LOG5-ARQUIVO            PIC X(10).
           05  LOG5-PROGRAMA           PIC X(10).
           05  LOG5-REGISTRO           PIC X(500).
