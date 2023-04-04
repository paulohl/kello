           SELECT COD007 ASSIGN TO PATH-COD007
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-COD007
                  RECORD KEY IS contrato-CO007
                  alternate key is alt1-CO007  with duplicates
                  alternate key is nome-cid-CO007 with duplicates
                  alternate key is abc-CO007 with duplicates
                  alternate key is mesano-CO007 with duplicates
                  alternate key is representante-CO007 with duplicates.
       FD  COD007.
       01  REG-COD007.
           05 alt1-CO007.
              10 num-cid-CO007    pic 9999.
              10 mesano-CO007     pic 9(6).
              10 nome-curso-CO007 pic x(10).
              10 contrato-CO007   pic 9999.
           05 nome-cid-CO007      pic x(20).
           05 abc-CO007           pic 9(8)v99.
           05 representante-CO007 pic X(9).
           05 part1-CO007         pic x(130).
           05 part2-CO007         pic x(130).
