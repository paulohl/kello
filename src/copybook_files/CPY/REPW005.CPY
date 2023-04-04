      *CADASTRO DE HOTEL
       FD  RED005.
       01  REG-RED005.
           05  CODIGO-RE05         PIC 9(4).
           05  HOTEL-RE05          PIC X(30).
           05  ENDERECO-RE05       PIC X(30).
           05  CIDADE-RE05         PIC 9(4).
           05  NOME-CID-RE05       PIC X(13).
      *    NOME EMBUTIDO PORQUE A BUSCA É FEITA PELA CIDADE
           05  VLR-DIARIA-RE05     PIC 9(8)V99.
           05  FONE1-RE05          PIC 9(8).
           05  FONE2-RE05          PIC 9(8).
