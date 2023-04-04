           SELECT CGD011 ASSIGN TO PATH-CGD011
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS COD-COMPL-CG11
                  STATUS IS ST-CGD011.
      * Complemento do arquivo de CLIENTE
       FD  CGD011.
       01  REG-CGD011.
           05  COD-COMPL-CG11.
               10  CLASSIF-CG11    PIC 9.
      *     CLASSIF = 0-CONTRATO  1-COMUM
               10  CODIGO-CG11     PIC 9(8).
           05  ENDERECO1-CG11      PIC X(30).
           05  BAIRRO1-CG11        PIC X(15).
           05  CIDADE1-CG11        PIC 9(4).
           05  CEP1-CG11           PIC 9(8).
           05  FONE1-CG11          PIC 9(8).
           05  CX-POSTAL1-CG11     PIC 9(5).
           05  EMPRESA-CG11        PIC X(30).
           05  ENDERECO2-CG11      PIC X(30).
           05  BAIRRO2-CG11        PIC X(15).
           05  CIDADE2-CG11        PIC 9(4).
           05  CEP2-CG11           PIC 9(8).
           05  FONE2-CG11          PIC 9(8).
           05  RAMAL2-CG11         PIC 9(3).
           05  CX-POSTAL2-CG11     PIC 9(5).
           05  E-MAIL-CG11         PIC X(30).
           05  CELULAR-CG11        PIC 9(8).
           05  FAX-CG11            PIC 9(8).
           05  CPF-CG11            PIC 9(16).
           05  RG-CG11             PIC X(15).
           05  DATA-NASC-CG11      PIC 9(8).
      *    DATA-NASC-CG11 = AAAAMMDD
           05  NOME-PAI-CG11       PIC X(30).
           05  NOME-MAE-CG11       PIC X(30).
           05  SITUACAO-CLI-CG11   PIC 9.
      *    SITUAÇÃO 0(OK)   1-PROTESTADO
           SELECT IED010 ASSIGN TO PATH-IED010
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              LOCK MODE IS MANUAL WITH LOCK ON RECORD
              STATUS IS ST-IED010
              RECORD KEY IS CODIGO-IE10
              ALTERNATE RECORD KEY IS NOME-IE10 WITH DUPLICATES
              ALTERNATE RECORD KEY IS ALT-IE10 = CIDADE-IE10 NOME-IE10
                        WITH DUPLICATES.
      *CADASTRO DE INSTITUICAO
       FD  IED010.
       01  REG-IED010.
           05  CODIGO-IE10       PIC 9(5).
           05  DEP-EXT-IE10      PIC 9(2).
           05  NATUR-EXT-IE10    PIC 9(2).
           05  NOME-IE10         PIC X(40).
           05  SIGLA-IE10        PIC X(10).
           05  ENDERECO-IE10     PIC X(30).
           05  NR-END-IE10       PIC X(6).
           05  COMPLEMENTO-IE10  PIC X(15).
           05  BAIRRO-IE10       PIC X(20).
           05  CIDADE-IE10       PIC X(32).
           05  ESTADO-IE10       PIC X(02).
           05  CEP-IE10          PIC 9(08).
           05  CX-POSTAL-IE10    PIC X(08).
           05  DDDFONE-IE10      PIC 9(9).
           05  FAX-IE10          PIC 9(9).
           05  RAMAL-IE10        PIC 9(4).
           05  INICIODE-IE10     PIC 9(8).
           05  SITE-IE10         PIC X(40).
           05  E-MAIL-IE10       PIC X(30).
