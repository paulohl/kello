           OPEN OUTPUT RELAT
           IF LNK-TIPO  = 1
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0
           END-IF
