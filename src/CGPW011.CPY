      * Complemento do arquivo de CLIENTE
       FD  CGD011.
       01  REG-CGD011.
           05  COD-COMPL-CG11.
               10  CLASSIF-CG11      PIC 9.
      *     CLASSIF = 0-CONTRATO  1-COMUM
               10  CODIGO-CG11       PIC 9(8).
      *>Dados Cobrança
           05  ENDERECO1-CG11        PIC X(45).
           05  BAIRRO1-CG11          PIC X(25).
           05  CIDADE1-CG11          PIC 9(4).
           05  CEP1-CG11             PIC 9(8).
           05  FONE1-CG11            PIC 9(8).
           05  CX-POSTAL1-CG11       PIC 9(5).
           05  COMPLEMENTO1-CG11     PIC X(30).
           05  E-MAIL1-CG11          PIC X(30).
           05  PONTO-REFER1-CG11     PIC X(40).
           05  CPF1-CG11             PIC 9(16).
           05  RG1-CG11              PIC X(15).
           05  DT-EXPEDICAO1-CG11    PIC 9(8).
           05  ORGAO-EXPEDICAO1-CG11 PIC X(8).
           05  FAX1-CG11             PIC 9(8).
           05  DDD-CELULAR1-CG11     PIC 9(02).
           05  CELULAR1-CG11         PIC 9(8).
           05  SEXO1-CG11            PIC X(01).
      *    DATA-NASC-CG11 = AAAAMMDD
           05  DATA-NASC1-CG11       PIC 9(8).
           05  SITUACAO-CLI-CG11     PIC 9.
      *>Dados Formnando
           05  ENDERECO2-CG11        PIC X(45).
           05  BAIRRO2-CG11          PIC X(25).
           05  CIDADE2-CG11          PIC 9(4).
           05  CEP2-CG11             PIC 9(8).
           05  FONE2-CG11            PIC 9(8).
           05  RAMAL2-CG11           PIC 9(3).
           05  COMPLEMENTO2-CG11     PIC X(30).
           05  CX-POSTAL2-CG11       PIC 9(5).
           05  PONTO-REFER2-CG11     PIC X(40).
           05  E-MAIL2-CG11          PIC X(30).
           05  CPF2-CG11             PIC 9(16).
           05  RG2-CG11              PIC X(15).
           05  DT-EXPEDICAO2-CG11    PIC 9(8).
           05  ORGAO-EXPEDICAO2-CG11 PIC X(8).
           05  DDD-CELULAR2-CG11     PIC 9(02).
           05  CELULAR2-CG11         PIC 9(8).
           05  FAX2-CG11             PIC 9(8).
      *    DATA-NASC-CG11 = AAAAMMDD
           05  DATA-NASC2-CG11       PIC 9(8).
           05  SEXO2-CG11            PIC X(01).
      *>Dados dos pai
           05  NOME-PAI-CG11         PIC X(30).
           05  NOME-MAE-CG11         PIC X(30).
           05  ENDERECO-PAIS-CG11    PIC X(45).
           05  COMPLEMENTO-PAIS-CG11 PIC X(30).
           05  BAIRRO-PAIS-CG11      PIC X(25).
           05  FONE-PAIS-CG11        PIC 9(8).
           05  CELULAR-PAIS-CG11     PIC 9(8).
           05  CIDADE-PAIS-CG11      PIC 9(4).
           05  CEP-PAIS-CG11         PIC 9(8).
      *Dados da República
           05  ENDERECO-REP-CG11     PIC X(45).
           05  BAIRRO-REP-CG11       PIC X(25).
           05  CIDADE-REP-CG11       PIC 9(4).
           05  CEP-REP-CG11          PIC 9(8).
      *>Dados Empresa
           05  EMPRESA-CG11         PIC X(30).
           05  ENDERECO3-CG11       PIC X(45).
           05  COMPLEMENTO3-CG11    PIC X(30).
           05  PONTO-REFER3-CG11    PIC X(40).
           05  BAIRRO3-CG11         PIC X(25).
           05  CIDADE3-CG11         PIC 9(4).
           05  CEP3-CG11            PIC 9(8).
           05  FONE3-CG11           PIC 9(8).
           05  RAMAL3-CG11          PIC 9(3).
           05  CX-POSTAL3-CG11      PIC 9(5).
           05  TURMA-CG11           PIC X(03).
           05  TURNO-CG11           PIC X(10).
      *>Complemento de Telefones
           05  COMP-TEL1-CG11       PIC 9(01).
           05  COMP-FAX1-CG11       PIC 9(01).
           05  COMP-CEL1-CG11       PIC 9(01).
           05  COMP-TEL2-CG11       PIC 9(01).
           05  COMP-CEL2-CG11       PIC 9(01).
           05  COMP-FAX2-CG11       PIC 9(01).
           05  COMP-TEL-PAIS-CG11   PIC 9(01).
           05  COMP-CEL-PAIS-CG11   PIC 9(01).
           05  COMP-TEL3-CG11       PIC 9(01).
           05  TIPO-PESSOA-CG11     PIC X(01).
           05  FILLER               PIC X(50).
      *    SITUAÇÃO 0(OK)   1-PROTESTADO *** TIPO-PESSOA-CG11 =
      *                                      " " -> FISICA
      *                                      "F" -> FISICA
      *                                      "J" -> JURIDICA
