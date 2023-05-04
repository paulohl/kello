       FD  RED030.
       01  REG-RED030.
           05  CODIGO-R030         PIC 999.
           05  REDUZIDO-R030       PIC X(8).
           05  NOME-R030           PIC X(30).
           05  ENDERECO-R030       PIC X(30).
           05  CIDADE-R030         PIC 9999.
           05  ENDERECO-COML-R030  PIC X(30).
           05  CIDADE-COML-R030    PIC 9999.
           05  CPFCGC-R030         PIC 9(16).
           05  RGINSC-R030         PIC X(15).
           05  NASCIMENTO-R030     PIC 9(8).
           05  FONE-R030           PIC 9(7).
           05  FONE1-R030          PIC 9(7).
           05  FONE2-R030          PIC 9(7).
           05  OCUPACAO-R030       PIC X(15).
           05  EQUIPAMENTOS OCCURS 30 TIMES.
               10  EQUIPAMENTO-R030 PIC 999.
           05  PADRAO-R030         PIC XX.
           05  FUNCAO-R030         PIC 99.
           05  ATRIBUICAO-R030     pIC 99.
           05  RESTRICAO-R030      PIC 99.
           05  CTA-BANC-R030 OCCURS 2 TIMES.
               10  CONTA-R030      PIC X(40).
           05  VEICULO-R030        PIC 99.
           05  venc-habil-r030     pic 9(8).
           05  venc-seguro-r030    pic 9(8).
           05  seguradora-r030     pic x(20).
           05  conta-extrato-r030  pic 9(5).
           05  empresa-r030        pic 99.
