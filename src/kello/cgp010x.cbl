       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGP010X.
      *AUTHOR: MARELI AMANCIO VOLPATO
      *DATA: 07/04/1999
      *DESCRIÇÃO: Cadastro de cliente
      *  Este cadastro terá 3 arquivos. Um arquivo simples contendo
      *  apenas o nome do comprador e código (cgd010), e o complemento
      *  deste arquivo o CGD011 e também o complemento do contrato o
      *  cgd012.
      *  O código do cliente terá uma classificação do tipo =
      *  0(contrato)   1(comum). O do tipo 0 são clientes relacionados
      *  com o recibo de vendas e o comum são os demais clientes.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX010.
           COPY CGPX011.
           COPY CGPX012.
           COPY CAPX010.
           COPY IEPX011.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       COPY CGPW011.
       COPY CGPW012.
       COPY CAPW010.
       COPY IEPW011.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CGP010X.CPB".
           COPY "CGP010X.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD012             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
      *   tipo de ordem de impressão
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *  IMPRIME-W = 0 (o registro nao faz parte da opcao) e = 1 Sim
           05  ULT-CODIGO            PIC 9(8)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de CADASTRO utilizado do tipo classificao = 1(comum)
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
           05  HORA-W                   PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  FOTO-IDENT-W          PIC X        VALUE SPACES.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "RELACAO CADASTRO DE CLIENTE - SIMPLES".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "CLASS  CODIGO        COMPRADOR".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.

       01  POP-UP                  PIC X(30).

           COPY "PARAMETR".
       PROCEDURE DIVISION USING PARAMETROS-W POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD012.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           MOVE ZEROS TO ERRO-W.
           OPEN I-O   CGD010 CGD011 CGD012.
           OPEN INPUT CAD010 IED011.
           IF ST-CGD010 = "35"
              CLOSE CGD010      OPEN OUTPUT CGD010
              CLOSE CGD010      OPEN I-O CGD010
           END-IF.
           IF ST-CGD011 = "35"
              CLOSE CGD011      OPEN OUTPUT CGD011
              CLOSE CGD011      OPEN I-O CGD011
           END-IF.
           IF ST-CGD012 = "35"
              CLOSE CGD012      OPEN OUTPUT CGD012
              CLOSE CGD012      OPEN I-O CGD012
           END-IF.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD012 <> "00"
              MOVE "ERRO ABERTURA CGD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
      *    IF COD-USUARIO-W NOT NUMERIC
      *       MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
      *       PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              MOVE 1 TO GS-ORDER
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
               EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                    PERFORM SALVAR-DADOS
                    PERFORM INSERE-ITEM
                    PERFORM LIMPAR-DADOS
      *             PERFORM INCREMENTA-CODIGO
                    MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-ACHAR-CODIGO-TRUE
                    IF GS-CLASSIFICACAO = SPACES
                       MOVE 0 TO GS-CLASSIFICACAO(1: 1)
                    END-IF
                    MOVE GS-CLASSIFICACAO(1: 1) TO CLASSIF-W
                    IF CLASSIF-W = 1 or 9
                       PERFORM ACHAR-CODIGO
                    ELSE
                       MOVE ZEROS TO GS-CODIGO
                    END-IF
               WHEN GS-LOAD-FLG-TRUE
                    PERFORM CARREGAR-DADOS
                    MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI-RECORD
                    PERFORM CARREGA-ULTIMOS
               WHEN GS-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LER-CIDADE
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                    PERFORM CARREGA-ULTIMOS
      *             MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                    MOVE GS-LINDET(1: 1) TO CLASSIF-W
                    MOVE GS-LINDET(7: 8) TO GS-CODIGO
                    PERFORM CARREGAR-DADOS
               WHEN GS-VERIFICA-CODIGO-TRUE
                    PERFORM VERIFICA-CODIGO
               WHEN GS-LE-CURSO-TRUE
                    PERFORM LER-CURSO
               WHEN GS-POPUP-CURSO-TRUE
                    PERFORM CHAMAR-POPUP-CURSO
               WHEN GS-POPUP-CIDADE1-TRUE
                    PERFORM SUGESTAO-CIDADE1
               WHEN GS-POPUP-CIDADE2-TRUE
                    PERFORM SUGESTAO-CIDADE2
               WHEN GS-POPUP-CIDADE-TRUE
                    PERFORM CHAMAR-POPUP-CIDADE
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CHAMAR-POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP010T".
           EVALUATE GS-ORDEM-CIDADE
            WHEN 1 MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE1
            WHEN 2 MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE2
            WHEN 5 MOVE PASSAR-STRING-1(35:4)  TO GS-CIDADE-REPUBLICA
            WHEN 9 MOVE PASSAR-STRING-1(35:4)  TO GS-CIDADE-PAIS
           END-EVALUATE.
           PERFORM LER-CIDADE.

       SUGESTAO-CIDADE1 SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4)      TO GS-CIDADE1
                                               CIDADE
           READ CAD010 NOT INVALID KEY
                MOVE NOME-CID               TO GS-NOME-CID1
                MOVE DDD-CID                TO GS-DDD-CID1.

           REFRESH-OBJECT TABP2.


       SUGESTAO-CIDADE2 SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4)      TO GS-CIDADE2
                                               CIDADE
           READ CAD010 NOT INVALID KEY
                MOVE NOME-CID               TO GS-NOME-CID2
                MOVE DDD-CID                TO GS-DDD-CID2.

           REFRESH-OBJECT TABP2.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POPUP-CURSO SECTION.
           CALL   "IEP011T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "IEP011T".
           MOVE PASSAR-STRING-1(43: 3) TO GS-CURSO
           MOVE PASSAR-STRING-1(1: 40) TO GS-NOME-CURSO.
       LER-CIDADE SECTION.
           EVALUATE GS-ORDEM-CIDADE
             WHEN 1 MOVE GS-CIDADE1          TO CIDADE
             WHEN 2 MOVE GS-CIDADE2          TO CIDADE
             WHEN 5 MOVE GS-CIDADE-REPUBLICA TO CIDADE
             WHEN 9 MOVE GS-CIDADE-PAIS      TO CIDADE
           END-EVALUATE.
           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-CID UF-CID NOME-COMPL-CID.

           EVALUATE GS-ORDEM-CIDADE
             WHEN 1 MOVE NOME-CID TO GS-NOME-CID1
                    MOVE DDD-CID  TO GS-DDD-CID1
             WHEN 2 MOVE NOME-CID TO GS-NOME-CID2
                    MOVE DDD-CID  TO GS-DDD-CID2
             WHEN 5 MOVE NOME-COMPL-CID TO GS-DESC-CIDADE-REPUBLICA
                    MOVE UF-CID         TO GS-DESC-UF-REPUBLICA
             WHEN 9 MOVE NOME-CID TO GS-NOME-CIDADE-PAIS
                    MOVE UF-CID   TO GS-ESTADO-PAIS
           END-EVALUATE.

       LER-CURSO SECTION.
           MOVE GS-CURSO TO CODIGO-IE11.
           READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11.
           MOVE NOME-IE11 TO GS-NOME-CURSO.
       VERIFICA-CODIGO SECTION.
           MOVE GS-CODIGO              TO CODIGO-CG10
                                          CODIGO-CG11
                                          CODIGO-CG12.
           MOVE GS-CLASSIFICACAO(1: 1) TO CLASSIF-CG10
                                          CLASSIF-CG11.
           MOVE 1 TO GS-GRAVA-W.
           READ CGD010 INVALID KEY
                INITIALIZE REG-CGD010
                MOVE 0               TO GS-GRAVA-W.

           MOVE COMPRADOR-CG10       TO GS-NOME

           READ CGD011 INVALID KEY
                INITIALIZE REG-CGD011
           NOT INVALID KEY
                PERFORM CARREGAR-DADOS-CGD011.

           READ CGD012 INVALID KEY
                INITIALIZE REG-CGD012
           NOT INVALID KEY
                PERFORM CARREGAR-DADOS-CGD012.

           REFRESH-OBJECT TABP1
           REFRESH-OBJECT TABP2
           REFRESH-OBJECT TABP3
           REFRESH-OBJECT TABP4.

       CARREGAR-DADOS SECTION.
           MOVE GS-CODIGO       TO CODIGO-CG10 CODIGO-CG11 CODIGO-CG12.
           INITIALIZE GS-DATA-BLOCK
           EVALUATE CLASSIF-W
             WHEN 0 MOVE "0-Contrato "      TO GS-CLASSIFICACAO
             WHEN 1 MOVE "1-Comum    "      TO GS-CLASSIFICACAO
             WHEN 9 MOVE "9-Unificado"      TO GS-CLASSIFICACAO
           END-EVALUATE.
           MOVE CLASSIF-W             TO CLASSIF-CG10 CLASSIF-CG11.

           READ CGD010 INVALID KEY
                INITIALIZE REG-CGD010.

           MOVE CODIGO-CG10          TO GS-CODIGO
           MOVE COMPRADOR-CG10       TO GS-NOME

           READ CGD011 INVALID KEY
                INITIALIZE REG-CGD011
           NOT INVALID KEY
                PERFORM CARREGAR-DADOS-CGD011.

           READ CGD012 INVALID KEY
                INITIALIZE REG-CGD012
           NOT INVALID KEY
                PERFORM CARREGAR-DADOS-CGD012.

           REFRESH-OBJECT TABP1
           REFRESH-OBJECT TABP2
           REFRESH-OBJECT TABP3
           REFRESH-OBJECT TABP4.

       CARREGAR-DADOS-CGD011 SECTION.
      *>Dados da Cobrança
           MOVE ENDERECO1-CG11             TO GS-ENDERECO1
           MOVE BAIRRO1-CG11               TO GS-BAIRRO1
           MOVE CIDADE1-CG11               TO GS-CIDADE1 CIDADE
           READ CAD010 INVALID KEY
               MOVE SPACES                 TO NOME-CID
               MOVE ZEROS                  TO DDD-CID
           END-READ
           MOVE NOME-CID                   TO GS-NOME-CID1
           MOVE DDD-CID                    TO GS-DDD-CID1
           MOVE CEP1-CG11                  TO GS-CEP1
           MOVE FONE1-CG11                 TO GS-FONE1
           MOVE COMP-TEL1-CG11             TO GS-COMP-TEL1
           MOVE CX-POSTAL1-CG11            TO GS-CX-POSTAL1
           MOVE COMPLEMENTO1-CG11          TO GS-COMPLEMENTO1
           MOVE PONTO-REFER1-CG11          TO GS-PONTO-REFERENCIA1

           MOVE E-MAIL1-CG11               TO GS-E-MAIL1
           MOVE CPF1-CG11                  TO GS-CPF1
           MOVE RG1-CG11                   TO GS-REG-IDENTID1
           MOVE DT-EXPEDICAO1-CG11         TO GS-DT-EXPEDICAO1
           MOVE ORGAO-EXPEDICAO1-CG11      TO GS-ORGAO-EXPEDICAO1
           MOVE FAX1-CG11                  TO GS-FAX1
           MOVE COMP-FAX1-CG11             TO GS-COMP-FAX1
           MOVE DDD-CELULAR1-CG11          TO GS-DDD-CELULAR1
           MOVE COMP-CEL1-CG11             TO GS-COMP-CEL1
           MOVE CELULAR1-CG11              TO GS-CELULAR1
           EVALUATE SEXO1-CG11
               WHEN "F"   MOVE "Feminino " TO GS-SEXO1
               WHEN "M"   MOVE "Masculino" TO GS-SEXO1
               WHEN OTHER MOVE SPACES      TO GS-SEXO1
           END-EVALUATE

           MOVE DATA-NASC1-CG11            TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                   TO GS-DATA-NASC1
           EVALUATE SITUACAO-CLI-CG11
             WHEN 0 MOVE "0-OK        "    TO GS-SITUACAO-CLIENTE
             WHEN 1 MOVE "1-PROTESTADO"    TO GS-SITUACAO-CLIENTE
           END-EVALUATE
      *>Dados do Formando
           MOVE ENDERECO2-CG11             TO GS-ENDERECO2
           MOVE BAIRRO2-CG11               TO GS-BAIRRO2
           MOVE CIDADE2-CG11               TO GS-CIDADE2 CIDADE
           READ CAD010 INVALID KEY
                MOVE ZEROS                 TO DDD-CID
                MOVE SPACES                TO NOME-CID
           END-READ
           MOVE NOME-CID                   TO GS-NOME-CID2
           MOVE DDD-CID                    TO GS-DDD-CID2
           MOVE CEP2-CG11                  TO GS-CEP2
           MOVE FONE2-CG11                 TO GS-FONE2
           MOVE COMP-TEL2-CG11             TO GS-COMP-TEL2
           MOVE RAMAL2-CG11                TO GS-RAMAL2
           MOVE COMPLEMENTO2-CG11          TO GS-COMPLEMENTO2
           MOVE CX-POSTAL2-CG11            TO GS-CX-POSTAL2
           MOVE PONTO-REFER2-CG11          TO GS-PONTO-REFERENCIA2
           MOVE CPF2-CG11                  TO GS-CPF2
           MOVE RG2-CG11                   TO GS-REG-IDENTID2
           MOVE DT-EXPEDICAO2-CG11         TO GS-DT-EXPEDICAO2
           MOVE ORGAO-EXPEDICAO2-CG11      TO GS-ORGAO-EXPEDICAO2
           MOVE FAX2-CG11                  TO GS-FAX2
           MOVE COMP-FAX2-CG11             TO GS-COMP-FAX2
           MOVE DDD-CELULAR2-CG11          TO GS-DDD-CELULAR2
           MOVE COMP-CEL2-CG11             TO GS-COMP-CEL2
           MOVE CELULAR2-CG11              TO GS-CELULAR2
           EVALUATE SEXO2-CG11
               WHEN "F"   MOVE "Feminino " TO GS-SEXO2
               WHEN "M"   MOVE "Masculino" TO GS-SEXO2
               WHEN OTHER MOVE SPACES      TO GS-SEXO2
           END-EVALUATE
           MOVE DATA-NASC2-CG11            TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                   TO GS-DATA-NASC2

      *>Dados dos Pais
           MOVE NOME-PAI-CG11              TO GS-NOME-PAI
           MOVE NOME-MAE-CG11              TO GS-NOME-MAE
           MOVE ENDERECO-PAIS-CG11         TO GS-ENDERECO-PAIS
           MOVE COMPLEMENTO-PAIS-CG11      TO GS-COMPLEMENTO-PAIS
           MOVE BAIRRO-PAIS-CG11           TO GS-BAIRRO-PAIS
           MOVE FONE-PAIS-CG11             TO GS-FONE-PAIS
           MOVE COMP-TEL-PAIS-CG11         TO GS-COMP-TEL-PAIS
           MOVE CELULAR-PAIS-CG11          TO GS-CELULAR-PAIS
           MOVE COMP-CEL-PAIS-CG11         TO GS-COMP-CEL-PAIS
           MOVE CIDADE-PAIS-CG11           TO GS-CIDADE-PAIS CIDADE
           READ CAD010 INVALID KEY
               MOVE SPACES                 TO GS-NOME-CIDADE-PAIS
           NOT INVALID KEY
               MOVE NOME-CID               TO GS-NOME-CIDADE-PAIS
               MOVE UF-CID                 TO GS-ESTADO-PAIS
           END-READ
           MOVE CEP-PAIS-CG11              TO GS-CEP-PAIS
      *>Dados da República
           MOVE ENDERECO-REP-CG11          TO GS-ENDERECO-REPUBLICA
           MOVE BAIRRO-REP-CG11            TO GS-BAIRRO-REPUBLICA
           MOVE CIDADE-REP-CG11            TO GS-CIDADE-REPUBLICA CIDADE
           READ CAD010 INVALID KEY
               MOVE SPACES                 TO NOME-CID
               MOVE SPACES                 TO UF-CID
           END-READ
           MOVE NOME-CID                   TO GS-DESC-CIDADE-REPUBLICA
           MOVE UF-CID                     TO GS-DESC-UF-REPUBLICA
           MOVE CEP-REP-CG11               TO GS-CEP-REPUBLICA
      *>Dados da Empresa
           MOVE EMPRESA-CG11               TO GS-EMPRESA3
           MOVE ENDERECO3-CG11             TO GS-ENDERECO3
           MOVE PONTO-REFER3-CG11          TO GS-PONTO-REFERENCIA3
           MOVE BAIRRO3-CG11               TO GS-BAIRRO3
           MOVE CIDADE3-CG11               TO CIDADE GS-CIDADE3
           READ CAD010 INVALID KEY
               MOVE SPACES                 TO NOME-CID
               MOVE ZEROS                  TO DDD-CID
           END-READ
           MOVE NOME-CID                   TO GS-NOME-CID3
           MOVE DDD-CID                    TO GS-DDD-CID3
           MOVE FONE3-CG11                 TO GS-FONE3
           MOVE COMP-TEL3-CG11             TO GS-COMP-TEL3
           MOVE RAMAL3-CG11                TO GS-RAMAL3.

       CARREGAR-DADOS-CGD012 SECTION.
           MOVE CURSO-CG12           TO GS-CURSO CODIGO-IE11.
           READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11.
           MOVE NOME-IE11            TO GS-NOME-CURSO.
           MOVE TURMA-CG12           TO GS-TURMA.
           MOVE TAMANHO-BECA-CG12    TO GS-TAMANHO-BECA.
           EVALUATE FOTO-IDENTIFIC-CG12
             WHEN 0 MOVE "0-Não"     TO GS-FOTO-IDENTIFICACAO
             WHEN 1 MOVE "1-Sim"     TO GS-FOTO-IDENTIFICACAO
           END-EVALUATE.

      *    CARGO-COMISSAO  1-PRESIDENTE   2-VICE-PRESID   3-TESOUREIRO
      *                    4-SECRETARIO   5-FORMANDO









           EVALUATE CARGO-COMISSAO-CG12
             WHEN 1 MOVE "1-Presidente" TO GS-CARGO-COMISSAO
             WHEN 2 MOVE "2-Tesoureiro" TO GS-CARGO-COMISSAO
             WHEN 3 MOVE "3-Secretário" TO GS-CARGO-COMISSAO
             WHEN 4 MOVE "4-Normal    " TO GS-CARGO-COMISSAO
           END-EVALUATE.




      *>comentado no dia 10/03/2016 pois não estava condizente com o que
      * estava na tela de lançamento
      *    EVALUATE CARGO-COMISSAO-CG12
      *      WHEN 1 MOVE "1-Presidente" TO GS-CARGO-COMISSAO
      *      WHEN 2 MOVE "2-Vice-Pres." TO GS-CARGO-COMISSAO
      *      WHEN 3 MOVE "2-Tesoureiro" TO GS-CARGO-COMISSAO
      *      WHEN 4 MOVE "3-Secretario" TO GS-CARGO-COMISSAO
      *      WHEN 5 MOVE "4-Formando  " TO GS-CARGO-COMISSAO
      *    END-EVALUATE.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CGD010
           INITIALIZE REG-CGD011
           INITIALIZE REG-CGD012.
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       EXCLUI-RECORD SECTION.
           DELETE CGD010.
           PERFORM LIMPAR-DADOS.
       SALVAR-DADOS SECTION.
           MOVE GS-CLASSIFICACAO(1: 1) TO  CLASSIF-CG10 CLASSIF-CG11
           MOVE GS-CODIGO        TO  CODIGO-CG10 CODIGO-CG11.
           MOVE GS-NOME          TO  COMPRADOR-CG10.

           IF GS-GRAVA-W = 0
              WRITE REG-CGD010 INVALID KEY
                      PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-CGD010 INVALID KEY
                PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                IF CLASSIF-CG10 = 1
                   SUBTRACT 1 FROM ULT-CODIGO
                END-IF
           END-IF.
           IF GS-GRAVA-CGP011 = 1
                 PERFORM SALVAR-DADOS-CGD011.
           IF GS-GRAVA-CGP012 = 1
                 PERFORM SALVAR-DADOS-CGD012.
       SALVAR-DADOS-CGD011 SECTION.
      *>Dados da Cobrança
           MOVE GS-ENDERECO1           TO ENDERECO1-CG11
           MOVE GS-BAIRRO1             TO BAIRRO1-CG11
           MOVE GS-CIDADE1             TO CIDADE1-CG11
           MOVE GS-CEP1                TO CEP1-CG11
           MOVE GS-FONE1               TO FONE1-CG11
           MOVE GS-COMP-TEL1           TO COMP-TEL1-CG11
           MOVE GS-CX-POSTAL1          TO CX-POSTAL1-CG11
           MOVE GS-COMPLEMENTO1        TO COMPLEMENTO1-CG11
           MOVE GS-E-MAIL1             TO E-MAIL1-CG11
           MOVE GS-PONTO-REFERENCIA1   TO PONTO-REFER1-CG11
           MOVE GS-CPF1                TO CPF1-CG11
           MOVE GS-REG-IDENTID1        TO RG1-CG11
           MOVE GS-DT-EXPEDICAO1       TO DT-EXPEDICAO1-CG11
           MOVE GS-ORGAO-EXPEDICAO1    TO ORGAO-EXPEDICAO1-CG11
           MOVE GS-FAX1                TO FAX1-CG11
           MOVE GS-COMP-FAX1           TO COMP-FAX1-CG11
           MOVE GS-DDD-CELULAR1        TO DDD-CELULAR1-CG11
           MOVE GS-COMP-CEL1           TO COMP-CEL1-CG11
           MOVE GS-CELULAR1            TO CELULAR1-CG11
           MOVE GS-SEXO1(1:1)          TO SEXO1-CG11
           MOVE GS-DATA-NASC1          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV               TO DATA-NASC1-CG11
           IF GS-SITUACAO-CLIENTE = SPACES
              MOVE 0                         TO SITUACAO-CLI-CG11
           ELSE
              MOVE GS-SITUACAO-CLIENTE(1: 1) TO SITUACAO-CLI-CG11
           END-IF
      *>Dados do Formando
           MOVE GS-ENDERECO2            TO ENDERECO2-CG11
           MOVE GS-BAIRRO2              TO BAIRRO2-CG11
           MOVE GS-CIDADE2              TO CIDADE2-CG11
           MOVE GS-CEP2                 TO CEP2-CG11
           MOVE GS-FONE2                TO FONE2-CG11
           MOVE GS-COMP-TEL2            TO COMP-TEL2-CG11
           MOVE GS-RAMAL2               TO RAMAL2-CG11
           MOVE GS-COMPLEMENTO2         TO COMPLEMENTO2-CG11
           MOVE GS-CX-POSTAL2           TO CX-POSTAL2-CG11
           MOVE GS-PONTO-REFERENCIA2    TO PONTO-REFER2-CG11
           MOVE GS-E-MAIL2              TO E-MAIL2-CG11
           MOVE GS-CPF2                 TO CPF2-CG11
           MOVE GS-REG-IDENTID2         TO RG2-CG11
           MOVE GS-DT-EXPEDICAO2        TO DT-EXPEDICAO2-CG11
           MOVE GS-ORGAO-EXPEDICAO2     TO ORGAO-EXPEDICAO2-CG11
           MOVE GS-DDD-CELULAR2         TO DDD-CELULAR2-CG11
           MOVE GS-COMP-CEL2            TO COMP-CEL2-CG11
           MOVE GS-CELULAR2             TO CELULAR2-CG11
           MOVE GS-FAX2                 TO FAX2-CG11
           MOVE GS-COMP-FAX2            TO COMP-FAX2-CG11
           MOVE GS-DATA-NASC2           TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-NASC2-CG11
           MOVE GS-SEXO2(1:1)           TO SEXO2-CG11
      *>Dados dos Pais
           MOVE GS-NOME-PAI             TO NOME-PAI-CG11
           MOVE GS-NOME-MAE             TO NOME-MAE-CG11
           MOVE GS-ENDERECO-PAIS        TO ENDERECO-PAIS-CG11
           MOVE GS-COMPLEMENTO-PAIS     TO COMPLEMENTO-PAIS-CG11
           MOVE GS-BAIRRO-PAIS          TO BAIRRO-PAIS-CG11
           MOVE GS-FONE-PAIS            TO FONE-PAIS-CG11
           MOVE GS-COMP-TEL-PAIS        TO COMP-TEL-PAIS-CG11
           MOVE GS-CELULAR-PAIS         TO CELULAR-PAIS-CG11
           MOVE GS-COMP-CEL-PAIS        TO COMP-CEL-PAIS-CG11
           MOVE GS-CIDADE-PAIS          TO CIDADE-PAIS-CG11
           MOVE GS-CEP-PAIS             TO CEP-PAIS-CG11
      *Dados da República
           MOVE GS-ENDERECO-REPUBLICA   TO ENDERECO-REP-CG11
           MOVE GS-BAIRRO-REPUBLICA     TO BAIRRO-REP-CG11
           MOVE GS-CIDADE-REPUBLICA     TO CIDADE-REP-CG11
           MOVE GS-CEP-REPUBLICA        TO CEP-REP-CG11
      *>Dados da Empresa
           MOVE GS-EMPRESA3             TO EMPRESA-CG11
           MOVE GS-ENDERECO3            TO ENDERECO3-CG11
           MOVE GS-PONTO-REFERENCIA3    TO PONTO-REFER3-CG11
           MOVE GS-CIDADE3              TO CIDADE3-CG11
           MOVE GS-BAIRRO3              TO BAIRRO3-CG11
           MOVE GS-FONE3                TO FONE3-CG11
           MOVE GS-COMP-TEL3            TO COMP-TEL3-CG11
           MOVE GS-RAMAL3               TO RAMAL3-CG11
           WRITE REG-CGD011 INVALID KEY
                    REWRITE REG-CGD011.
       SALVAR-DADOS-CGD012 SECTION.
           MOVE GS-CODIGO           TO CODIGO-CG12.
           MOVE GS-CURSO            TO CURSO-CG12.
           MOVE GS-TURMA            TO TURMA-CG12.
           MOVE GS-TAMANHO-BECA     TO TAMANHO-BECA-CG12.
           MOVE GS-FOTO-IDENTIFICACAO(1: 1) TO FOTO-IDENT-W
           IF FOTO-IDENT-W = " " CONTINUE
           ELSE
              MOVE FUNCTION NUMVAL(GS-FOTO-IDENTIFICACAO(1: 1))
                TO FOTO-IDENTIFIC-CG12.

           MOVE FUNCTION NUMVAL(GS-CARGO-COMISSAO(1: 1))
             TO CARGO-COMISSAO-CG12.
           WRITE REG-CGD012 INVALID KEY REWRITE REG-CGD012.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-CGD010       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-INICIAL(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE GS-INICIAL(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.

           MOVE INICIAL-PROCURADA TO COMPRADOR-CG10.
           START CGD010 KEY IS NOT < COMPRADOR-CG10 INVALID KEY
                 MOVE "10" TO ST-CGD010.
           PERFORM UNTIL ST-CGD010 = "10"
              READ CGD010 NEXT RECORD AT END MOVE "10" TO ST-CGD010
              NOT AT END
                MOVE SPACES TO GS-LINDET
                MOVE COMPRADOR-CG10(1: I) TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                   MOVE "10" TO ST-CGD010
                ELSE PERFORM INSERE-ITEM
                END-IF
              END-READ
           END-PERFORM.
       INSERE-ITEM SECTION.
           MOVE CLASSIF-CG10      TO GS-LINDET(1: 6)
           MOVE CODIGO-CG10       TO GS-LINDET(07: 08)
           MOVE COMPRADOR-CG10    TO GS-LINDET(21: 40)
           IF GS-GRAVA-W = 1
              MOVE "ATUALIZA-LIST" TO DS-PROCEDURE
           ELSE MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CGP010X" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           IF GS-ORDEM-IMPRESS = 1
              MOVE ZEROS TO CODIGO-CG10 CLASSIF-CG10
              START CGD010 KEY IS NOT < COD-COMPL-CG10 INVALID KEY
                           MOVE "10" TO ST-CGD010
           ELSE MOVE SPACES TO COMPRADOR-CG10
                START CGD010 KEY IS NOT < COMPRADOR-CG10 INVALID KEY
                           MOVE "10" TO ST-CGD010.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CGD010 = "10"
             READ CGD010 NEXT RECORD AT END MOVE "10" TO ST-CGD010
              NOT AT END
               MOVE SPACES TO LINDET-REL
                PERFORM OPCAO-IMPRESSAO
                IF IMPRIME-W = 0 CONTINUE
                ELSE
                   MOVE CLASSIF-CG10          TO LINDET-REL(1: 6)
                   MOVE CODIGO-CG10           TO LINDET-REL(07: 08)
                   MOVE COMPRADOR-CG10        TO LINDET-REL(21: 30)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 60 PERFORM CABECALHO
                END-IF
             END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL AFTER PAGE.
           CLOSE RELAT.
       OPCAO-IMPRESSAO SECTION.
           MOVE GS-CARACTERISTICA(1: 1) TO CARACTERISTICA-W
           MOVE ZEROS TO IMPRIME-W
           EVALUATE CARACTERISTICA-W
             WHEN 0 MOVE 1 TO IMPRIME-W
             WHEN 1 IF CLASSIF-CG10 = 0 MOVE 1 TO IMPRIME-W
             WHEN 2 IF CLASSIF-CG10 = 1 MOVE 1 TO IMPRIME-W
           END-EVALUATE.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       ACHAR-CODIGO SECTION.
           MOVE CLASSIF-W              TO CLASSIF-CG10
           MOVE ZEROS                  TO CODIGO-CG10 ULT-CODIGO.
           START CGD010 KEY IS NOT < COD-COMPL-CG10 INVALID KEY
                 MOVE "10" TO ST-CGD010
           END-START
           PERFORM UNTIL ST-CGD010 = "10"
                 READ CGD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CGD010
                 NOT AT END
                      IF CLASSIF-CG10 NOT = CLASSIF-W
                         MOVE "10" TO ST-CGD010
                      ELSE
                         MOVE CODIGO-CG10 TO ULT-CODIGO
                 END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE ULT-CODIGO TO GS-CODIGO.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE IED011 CGD010 CGD011 CGD012 CAD010.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
