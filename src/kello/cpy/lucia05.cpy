           SELECT  COD005 ASSIGN TO PATH-COD005
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   STATUS IS ST-COD005
                   RECORD KEY IS PADRAO-CO05.
       FD  COD005.
       01  REG-COD005.
           05  PADRAO-CO05         PIC X.
           05  PREV-FOTOS-CO05     PIC 9(3).
