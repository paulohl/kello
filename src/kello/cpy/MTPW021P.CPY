      *ARQUIVO DE PRIORIODADE P/ PRODUÇÃO POR PRODUTOS
       FD  MTD021P.
       01  REG-MTD021P.
           05 CHAVE-MT21P.
              10 CONTRATO-MT21P      PIC 9(04).
              10 ALBUM-MT21P         PIC 9(04).
              10 SEQ-MT21P           PIC 9(03).
              10 PRODUTO-MT21P       PIC 9(04).
              10 MODELO-MT21P        PIC 9(04).
           05 STATUS-MT21P           PIC 9(01).
           05 FORNECEDOR-MT21P       PIC 9(06).
           05 DATA-PREVISTA-MT21P    PIC 9(08).
           05 DATA-ENTREGA-MT21P     PIC 9(08).
           05 OBSERVACAO-MT21P       PIC X(300).
           05 USUARIO-MT21P          PIC X(05).
           05 QTDE-CONTRATO-MT21P    PIC 9(06).
           05 QTDE-PLANILHADA-MT21P  PIC 9(06).
           05 FILLER                 PIC X(100).
