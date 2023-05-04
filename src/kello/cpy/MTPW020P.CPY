      *ARQUIVO DE MONTAGEM NO ALBUM
       FD  MTD020P.
       01  REG-MTD020P.
           05 ALBUM-MTGP.
              10  CONTRATO-MTGP    PIC 9(4).
              10  NRALBUM-MTGP     PIC 9(4).
              10  SEQUEN-MTGP      PIC 9(3).

           05 PRODUTO-MTGP         PIC 9(4).
           05 MODELO-MTGP          PIC 9(4).
           05 QTDE-CONTRATO-MTGP   PIC 9(6).
           05 QTDE-PLANILHA-MTGP   PIC 9(6).
           05 CUSTO-UNIT-MTGP      PIC 9(06)V999.
           05 VENDA-UNIT-MTGP      PIC 9(06)V999.
           05 FILLER               PIC X(100).
