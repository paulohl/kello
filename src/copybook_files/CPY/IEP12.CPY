       SELECT IED012 ASSIGN TO PATH-IED012
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              LOCK MODE IS MANUAL WITH LOCK ON RECORD
              STATUS IS ST-IED012
              RECORD KEY IS CHAVE-IE12 = COD-INSTITUICAO-IE12,
                     COD-CURSO-IE12.
       FD  IED012.
       01  REG-IED012.
           05  COD-INSTITUICAO-IE12  PIC 9(5).
           05  COD-CURSO-IE12        PIC 9(3).
           05  CONDIC-EXT-IE12       PIC X(10).
           05  DATA-INICIO-IE12      PIC 9(8).
           05  SITUACAO-EXT-IE12     PIC X(14).
           05  MODALIDADE-IE12       PIC X(20).
           05  CODIGO-NA-IE12        PIC 9(9).
           05  VAGAS-IE12            PIC 9(3).
           05  CONCLUSAO-ANO-IE12    PIC 9.
