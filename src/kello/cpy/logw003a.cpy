       FD  LOG003a.
       01  REG-LOG003a.
           05  LOG3a-USUARIO            PIC X(05).
           05  LOG3a-PERIODO.
               10 LOG3a-DATA.
                  15 LOG3a-ANO          PIC 9(04).
                  15 LOG3a-MES          PIC 9(02).
                  15 LOG3a-DIA          PIC 9(02).
               10 LOG3a-HORAS.
                  15 LOG3a-HORA         PIC 9(02).
                  15 LOG3a-MINU         PIC 9(02).
                  15 LOG3a-SEGU         PIC 9(02).
                  15 LOG3a-MILE         PIC 9(02).
           05  LOG3a-OPERACAO           PIC X(01).
           05  LOG3a-ARQUIVO            PIC X(10).
           05  LOG3a-PROGRAMA           PIC X(10).
           05  LOG3a-REGISTRO           PIC X(200).
