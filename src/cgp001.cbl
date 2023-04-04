       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGP001.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 30/07/1998
      *DESCRIÇÃO: Cadastro Geral de Contas
      *  Este cadastro terá vários arquivos. Um arquivo simples que será
      *  obrigatório o preenchimento, e o complemento desse cadastro que
      *  será opcional. O arquivo simples conterá o codigo, nome e nome
      *  reduzido. Um cadastro simples poderá ter vários arquivos de
      *  complemento, dependerá das características do cadastrado. Por
      *  exemplo uma pessoa que possui o cargo Funcionário e Fotógrafo,
      *  ela terá o cadastro simples mais o complemente do funcionario

      *  e fotógrafo.
      *  As características podem ser: Pessoa Física, Pessoa Jurídica,
      *  Funcionario, Vendedor, Representante, fotógrafo, cinegrafista,
      *  imposto, investidor mais uma opção em aberto.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CGPX010.
           COPY CGPX001.
           COPY CGPX002.
           COPY CGPX003.
           COPY CGPX004.
           COPY CGPX005.
           COPY CGPX006.
           COPY CAPX010.
           COPY CAPX014.
           COPY CAPX030.
           COPY CXPX020.
           COPY REPX002.
           COPY REPX006.
           COPY LOGX001.
           COPY LOGX002.
           COPY LOGX003.
           COPY LOGX004.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CGPW010.
       COPY CGPW001.
       COPY CGPW002.
       COPY CGPW003.
       COPY CGPW004.
       COPY CGPW005.
       COPY CGPW006.
       COPY CAPW010.
       COPY CAPW014.
       COPY CAPW030.
       COPY CXPW020.
       COPY REPW002.
       COPY REPW006.
       COPY LOGW001.
       COPY LOGW002.
       COPY LOGW003.
       COPY LOGW004.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CGP001.CPB".
           COPY "CGP001.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       77  ETIQUETA                  PIC 9(01).
       77  LIN                       PIC 9(02).
       77  CONTADOR                  PIC 9(03).
       77  POSICAO                   PIC 9(09).
       01  OPCAO-W                   PIC 9(01).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD002             PIC XX       VALUE SPACES.
           05  ST-CGD003             PIC XX       VALUE SPACES.
           05  ST-CGD004             PIC XX       VALUE SPACES.
           05  ST-CGD005             PIC XX       VALUE SPACES.
           05  ST-CGD006             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD014             PIC XX       VALUE SPACES.
           05  ST-CAD030             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-RED002             PIC XX       VALUE SPACES.
           05  ST-RED006             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ST-LOG003             PIC XX       VALUE SPACES.
           05  ST-LOG004             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  CARACTERISTICA-W      PIC XX       VALUE SPACES.
           05  MASC-TELEFONE         PIC ZZZZ.ZZZ9 BLANK WHEN ZEROS.
      *   tipo de ordem de impressão
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *  IMPRIME-W = 0 (o registro nao faz parte da opcao) e = 1 Sim
           05  ULT-CODIGO            PIC 9(6)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de CADASTRO utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
      *    Grava = 0 (regravar)   grava = 1 (gravar)
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

           COPY "CBPRINT.CPY".

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU                 PIC 9(04).
             10 WS-MES-CPU                 PIC 9(02).
             10 WS-DIA-CPU                 PIC 9(02).
          05 FILLER                        PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       01  linka-cgc.
           05  link-cgc             pic 9(14).
           05  link-cgc-r redefines link-cgc.
               10  link-cgc-num     pic 9(12).
               10  link-cgc-dig1    pic 9(01).
               10  link-cgc-dig2    pic 9(01).
           05  link-cgc-conf        pic 9(01).

       01  linka-cpf.
           05  link-cpf             pic 9(11).
           05  link-cpf-r redefines link-cpf.
               10  link-cpf-num     pic 9(09).
               10  link-cpf-dig1    pic 9(01).
               10  link-cpf-dig2    pic 9(01).
           05  link-cpf-conf        pic 9(01).

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "RELACAO CADASTRO GERAL - SIMPLES".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "CODIGO        NOME                                         R
      -    "EDUZIDO     ".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.


       01  DET-ETIQUETA1.
           05 DET-ETIQUETA11       PIC X(33).
           05 FILLER               PIC X(06).
           05 DET-ETIQUETA21       PIC X(33).

       01  DET-ETIQUETA2.
           05 DET-ETIQUETA12       PIC X(33).
           05 FILLER               PIC X(06).
           05 DET-ETIQUETA22       PIC X(33).

       01  DET-ETIQUETA3.
           05 DET-ETIQUETA13       PIC X(33).
           05 FILLER               PIC X(06).
           05 DET-ETIQUETA23       PIC X(33).

       01  DET-ETIQUETA4.
           05 DET-ETIQUETA14       PIC X(33).
           05 FILLER               PIC X(06).
           05 DET-ETIQUETA24       PIC X(33).

       01  DET-ETIQUETA5.
           05 DET-ETIQUETA15       PIC X(33).
           05 FILLER               PIC X(06).
           05 DET-ETIQUETA25       PIC X(33).


       01  CEP-CXPOSTAL.
           05 DET-CEP              PIC 99.999.999.
           05 FILLER               PIC X(05).
           05 FILLER               PIC X(10) VALUE "CX.POSTAL ".
           05 DET-CXPOSTAL         PIC X(08).

       01 DET-AJUDA.
           05 DET-NUMERO           PIC 9(03).
           05 FILLER               PIC X(02).
           05 DET-RESTO            PIC X(01).

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
      *PROCEDURE DIVISION USING POP-UP.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CGP001-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CGP001-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CGP001-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CGP001-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD002.
           MOVE "CGD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD003.
           MOVE "CGD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD004.
           MOVE "CGD005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD005.
           MOVE "CGD006" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD006.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD014" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD014.
           MOVE "CAD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD030.
           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "RED002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED002.
           MOVE "RED006" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED006.
           MOVE "LOG001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOG002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOG003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG003.
           MOVE "LOG004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG004.
           MOVE "LOGACESS"TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           MOVE ZEROS TO ERRO-W.
           OPEN I-O CGD001 CGD002 CGD003 CGD004 CGD005 CGD006 LOG001
                    LOG002 LOG003 LOG004 CGD010

           OPEN INPUT CAD004 CAD010 CAD014 CXD020 RED002 RED006 CAD030.
           IF ST-LOG001 = "35"
              CLOSE LOG001      OPEN OUTPUT LOG001
              CLOSE LOG001      OPEN I-O LOG001
           END-IF.
           IF ST-LOG002 = "35"
              CLOSE LOG002      OPEN OUTPUT LOG002
              CLOSE LOG002      OPEN I-O LOG002
           END-IF.
           IF ST-LOG003 = "35"
              CLOSE LOG003      OPEN OUTPUT LOG003
              CLOSE LOG003      OPEN I-O LOG003
           END-IF.
           IF ST-LOG004 = "35"
              CLOSE LOG004      OPEN OUTPUT LOG004
              CLOSE LOG004      OPEN I-O LOG004
           END-IF.
           IF ST-CGD010 = "35"
              CLOSE CGD010      OPEN OUTPUT CGD010
              CLOSE CGD010      OPEN I-O CGD010
           END-IF.
           IF ST-CGD001 = "35"
              CLOSE CGD001      OPEN OUTPUT CGD001
              CLOSE CGD001      OPEN I-O CGD001
           END-IF.
           IF ST-CGD002 = "35"
              CLOSE CGD002      OPEN OUTPUT CGD002
              CLOSE CGD002      OPEN I-O CGD002
           END-IF.
           IF ST-CGD003 = "35"
              CLOSE CGD003      OPEN OUTPUT CGD003
              CLOSE CGD003      OPEN I-O CGD003
           END-IF.
           IF ST-CGD004 = "35"
              CLOSE CGD004      OPEN OUTPUT CGD004
              CLOSE CGD004      OPEN I-O CGD004.
           IF ST-CGD005 = "35"
              CLOSE CGD005      OPEN OUTPUT CGD005
              CLOSE CGD005      OPEN I-O CGD005.
           IF ST-CGD006 = "35"
              CLOSE CGD006      OPEN OUTPUT CGD006
              CLOSE CGD006      OPEN I-O CGD006.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-LOG001 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-LOG002 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG003 <> "00"
              MOVE "ERRO ABERTURA LOG003: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-LOG003 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG004 <> "00"
              MOVE "ERRO ABERTURA LOG004: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-LOG004 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CGD010 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD002 <> "00"
              MOVE "ERRO ABERTURA CGD002: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CGD002 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD003 <> "00"
              MOVE "ERRO ABERTURA CGD003: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CGD003 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD004 <> "00"
              MOVE "ERRO ABERTURA CGD004: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CGD004 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD005 <> "00"
              MOVE "ERRO ABERTURA CGD005: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CGD005 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD006 <> "00"
              MOVE "ERRO ABERTURA CGD006: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CGD006 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CAD004 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD014 <> "00"
              MOVE "ERRO ABERTURA CAD014: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CAD014 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD030 <> "00"
              MOVE "ERRO ABERTURA CAD030: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CAD030 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED002 <> "00"
              MOVE "ERRO ABERTURA RED002: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-RED002 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED006 <> "00"
              MOVE "ERRO ABERTURA RED006: "  TO CGP001-MENSAGEM-ERRO
              MOVE ST-RED006 TO CGP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CGP001-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.


           CLOSE    CGD001 CGD002 CGD003 CGD004 CGD005 CGD006 LOG001
                    LOG002 LOG003 LOG004 CGD010

           OPEN INPUT CGD001 CGD002 CGD003 CGD004 CGD005 CGD006 CGD010


           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CGP001"            to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           IF ERRO-W = ZEROS
              MOVE 1 TO CGP001-ORDER
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CGP001-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CGP001-ACHAR-CODIGO-TRUE
                   PERFORM ACHAR-CODIGO
               WHEN CGP001-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM INSERE-ITEM
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CGP001-SALVA-BANCOS-TRUE
                   PERFORM SALVAR-DADOS-BANCOS
               WHEN CGP001-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CGP001-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CGP001-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CGP001-LE-CODIGO-TRUE
                   PERFORM LER-CODIGO
               WHEN CGP001-POPUP-CODIGO-TRUE
                   PERFORM POPUP-CODIGO
               WHEN CGP001-LE-CIDADE-TRUE
                   PERFORM LER-CIDADE
               WHEN CGP001-LE-APURACAO-TRUE
                    PERFORM LER-APURACAO
               WHEN CGP001-LE-DEPTO-TRUE
                    PERFORM LER-DEPTO
               WHEN CGP001-LE-FUNCAO-TRUE
                    PERFORM LER-FUNCAO
               WHEN CGP001-LE-VEICULO-TRUE
                    PERFORM LER-VEICULO
               WHEN CGP001-LE-EMPRESA-TRUE
                    PERFORM LER-EMPRESA
               WHEN CGP001-INSERIR-NOME-TRUE
                    PERFORM INSERIR-NOME
               WHEN CGP001-INSERIR-CIDADE-TRUE
                    PERFORM INSERIR-CIDADE
               WHEN CGP001-INSERIR-CARACT-TRUE
                    PERFORM INSERIR-CARACT
               WHEN CGP001-POPUP-GERAL-TRUE
                    PERFORM POPUP-GERAL
               WHEN CGP001-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN CGP001-MALA-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-MALA
                    END-IF
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN CGP001-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CGP001-CARREGA-LIST-BOX-TRUE
                   MOVE CGP001-LINDET(1: 6) TO CGP001-CODIGO
                   PERFORM CARREGAR-DADOS
               WHEN CGP001-VERIFICA-CODIGO-TRUE
                   PERFORM CARREGAR-DADOS
               WHEN CGP001-INSERIR-BANCO-TRUE
                   PERFORM INSERIR-BANCO
               WHEN CGP001-LE-BANCO-TRUE
                   PERFORM LER-BANCO
               WHEN CGP001-POPUP-BANCO-TRUE
                   PERFORM POPUP-BANCO
               WHEN CGP001-TROCAR-BANCO-TRUE
                   PERFORM TROCAR-BANCO
               WHEN CGP001-LE-CODRED-TRUE
                    PERFORM LER-CODRED
               WHEN CGP001-POPUP-CODRED-TRUE
                    PERFORM POPUP-CODRED
               WHEN CGP001-VALIDA-CPF-TRUE
                    PERFORM VALIDA-CPF
               WHEN CGP001-VALIDA-CNPJ-TRUE
                    PERFORM VALIDA-CNPJ
               WHEN CGP001-VALIDA-FLG-TRUE
                    PERFORM VALIDA-FLG
               WHEN CGP001-ALTERAR-TRUE
                    PERFORM ALTERAR-DADOS
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       ALTERAR-DADOS SECTION.
           MOVE "Deseja Alterar os Dados da Conta do Cliente ?" TO
           MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM 140-EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE 1 TO CGP001-FLAG-CRITICA
           ELSE
              MOVE 0 TO CGP001-FLAG-CRITICA.

       VALIDA-FLG SECTION.
           MOVE 0 TO CGP001-FLAG-CRITICA
           IF CGP001-T-PES-JUR      = 0 AND
              CGP001-T-PES-FIS      = 0 AND
              CGP001-T-FUNC         = 0 AND
              CGP001-T-REPRES       = 0 AND
              CGP001-T-FOTOG        = 0 AND
              CGP001-T-CINEG        = 0 AND
              CGP001-T-VEND         = 0 AND
              CGP001-T-IMPOSTO      = 0 AND
              CGP001-T-INVESTIDOR   = 0 AND
              CGP001-T-OUTRO        = 0 AND
              CGP001-T-TERCEIRIZADO = 0 AND
              CGP001-T-FRANQUIA     = 0
              MOVE "Caracteristica do Cadastro Não Informado" TO
              MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VALIDA-CPF SECTION.
               if cgp001-cpf-f > 0
                  move  cgp001-cpf-f to   link-cpf
                  call     "digcpf"      using   linka-cpf
                  cancel   "digcpf"
                  if link-cpf-conf       =       1
                     move "C.P.F. Inválido!" to mensagem
                     move "C"                to tipo-msg
                     perform 140-exibir-mensagem.

       VALIDA-CNPJ SECTION.
               if cgp001-cgc-j > 0
                  move  cgp001-cgc-j to   link-cgc
                  call     "digcgc"      using   linka-cgc
                  cancel   "digcgc"
                  if link-cgc-conf       =       1
                     move "C.N.P.J. Inválido!" to mensagem
                     move "C"                to tipo-msg
                     perform 140-exibir-mensagem.

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move    1 to cgp001-flag-critica.


       LER-CODRED SECTION.
           MOVE CGP001-COD-REDUZIDO TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY
                MOVE "*****"    TO DESCRICAO-CX20.

           MOVE DESCRICAO-CX20  TO CGP001-DESC-COD-REDUZIDO.

       POPUP-CODRED SECTION.
           CALL   "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CXP020T"
           MOVE PASSAR-STRING-1(52: 5) TO CGP001-COD-REDUZIDO
           MOVE CGP001-COD-REDUZIDO    TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY
                MOVE "*****"    TO DESCRICAO-CX20.

           MOVE DESCRICAO-CX20  TO CGP001-DESC-COD-REDUZIDO.

       TROCAR-BANCO SECTION.
           EVALUATE CGP001-LINDET(1:1)
              WHEN "X"   MOVE 1 TO CGP001-PREFERENCIAL
              WHEN OTHER MOVE 0 TO CGP001-PREFERENCIAL
           END-EVALUATE
           EVALUATE CGP001-LINDET(3:12)
               WHEN "Cta.Corrente" MOVE 1 TO CGP001-TIPO-CONTA
                                   MOVE "SELECIONAR-CONTA" TO
                                   DS-PROCEDURE
                                   PERFORM CALL-DIALOG-SYSTEM
               WHEN "Cta.Poupança" MOVE 2 TO CGP001-TIPO-CONTA
                                   MOVE "SELECIONAR-POUPANCA" TO
                                   DS-PROCEDURE
                                   PERFORM CALL-DIALOG-SYSTEM
           END-EVALUATE
           MOVE CGP001-LINDET(16:4)       TO CGP001-BANCO
           MOVE CGP001-LINDET(21:16)      TO CGP001-NOME-BANCO
           MOVE CGP001-LINDET(38:9)       TO CGP001-AGENCIA
           MOVE CGP001-LINDET(48:15)      TO CGP001-CONTA-BANCO

           MOVE CGP001-LINDET(63:12)      TO CGP001-COMPLEMENTO-CONTA
           MOVE CGP001-LINDET(76:3)       TO CGP001-USUARIO
           MOVE CGP001-LINDET(80:04)      TO CGP001-DESC-USUARIO
           MOVE CGP001-LINDET(85:11)      TO CGP001-CPF-TITULAR
           MOVE CGP001-LINDET(97:14)      TO CGP001-ACP-CNPJ
           MOVE CGP001-LINDET(112:40)     TO CGP001-TITULAR-CONTA
           IF CGP001-LINDET(153:1) = "1"
              MOVE 1                      TO CGP001-PREFERENCIAL
           ELSE
              MOVE 0                      TO CGP001-PREFERENCIAL.

       LER-BANCO SECTION.
           MOVE CGP001-BANCO TO CODIGO-CAD30
           READ CAD030 INVALID KEY
               MOVE SPACE TO DESCRICAO-CAD30.
           MOVE DESCRICAO-CAD30 TO CGP001-NOME-BANCO.

       POPUP-BANCO SECTION.
           CALL   "CAP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CAP020T"
           MOVE PASSAR-STRING-1(59: 4) TO CGP001-BANCO
           MOVE CGP001-BANCO TO CODIGO-CAD30
           READ CAD030 INVALID KEY
               MOVE SPACE TO DESCRICAO-CAD30.
           MOVE DESCRICAO-CAD30 TO CGP001-NOME-BANCO.

       INSERIR-BANCO SECTION.
           IF CGP001-BANCO = ZEROS OR CGP001-AGENCIA = SPACES
              OR CGP001-CONTA-BANCO = SPACES
              MOVE "Dados Informados Incompletos" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE SPACES TO CGP001-LINDET
              EVALUATE CGP001-PREFERENCIAL
                 WHEN 1     MOVE "X" TO CGP001-LINDET(1:1)
                 WHEN OTHER MOVE " " TO CGP001-LINDET(1:1)
              END-EVALUATE
              EVALUATE CGP001-TIPO-CONTA
                  WHEN 1 MOVE "Cta.Corrente" TO CGP001-LINDET(3:12)
                  WHEN 2 MOVE "Cta.Poupança" TO CGP001-LINDET(3:12)
              END-EVALUATE
              MOVE CGP001-BANCO           TO CGP001-LINDET(16:4)
              MOVE CGP001-NOME-BANCO      TO CGP001-LINDET(21:16)
              MOVE CGP001-AGENCIA         TO CGP001-LINDET(38:9)
              MOVE CGP001-CONTA-BANCO     TO CGP001-LINDET(48:15)
              IF CGP001-USUARIO NOT > ZEROS
                 MOVE USUARIO-W           TO CGP001-DESC-USUARIO
                 MOVE COD-USUARIO-W       TO CGP001-USUARIO
              END-IF
              MOVE CGP001-COMPLEMENTO-CONTA TO CGP001-LINDET(63:12)
              MOVE CGP001-USUARIO         TO CGP001-LINDET(76:3)
              MOVE CGP001-DESC-USUARIO    TO CGP001-LINDET(80:04)
              MOVE CGP001-CPF-TITULAR     TO CGP001-LINDET(85:11)
              MOVE CGP001-ACP-CNPJ        TO CGP001-LINDET(97:14)
              MOVE CGP001-TITULAR-CONTA   TO CGP001-LINDET(112:40)
              MOVE CGP001-PREFERENCIAL    TO CGP001-LINDET(153:1)

              MOVE "INSERE-LIST3"         TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       INSERIR-CARACT SECTION.
           INITIALIZE REG-CGD001
           START CGD001 KEY IS NOT LESS NOME-CG01 INVALID KEY
               MOVE "10" TO ST-CGD001.

           PERFORM UNTIL ST-CGD001 = "10"
               READ CGD001 NEXT AT END
                   MOVE "10" TO ST-CGD001
               NOT AT END
                  PERFORM OPCAO-IMPRESSAO
                  IF IMPRIME-W = 1
                     MOVE CODIGO-CG01    TO CGP001-LINNOME(01: 06)
                     MOVE NOME-CG01      TO CGP001-LINNOME(13: 60)
                     MOVE "INSERE-LIST2" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
               END-READ
           END-PERFORM.

       INSERIR-NOME SECTION.
           IF CGP001-ACP-CODIGO > 0 AND CGP001-DESC-NOME <> SPACES
              MOVE CGP001-ACP-CODIGO        TO CGP001-LINNOME(01: 06)
              MOVE CGP001-DESC-NOME         TO CGP001-LINNOME(13: 60)
              MOVE "INSERE-LIST2" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       INSERIR-CIDADE SECTION.
           MOVE CGP001-OPCAO TO OPCAO-W
           IF CGP001-CIDADE-PESQ > 0
              PERFORM PELA-CIDADE
           ELSE
              IF CGP001-UF-PESQ <> SPACES
                 PERFORM PELA-UF
              ELSE
                 PERFORM TUDO
              END-IF
           END-IF
           MOVE OPCAO-W TO CGP001-OPCAO.

       TUDO SECTION.
           INITIALIZE REG-CGD001
           START CGD001 KEY IS NOT LESS NOME-CG01 INVALID KEY
               MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
               READ CGD001 NEXT AT END
                   MOVE "10" TO ST-CGD001
               NOT AT END
                  PERFORM OPCAO-IMPRESSAO

                  IF IMPRIME-W = 1
                     IF T-PESFIS-CG01 = 1
                        MOVE CODIGO-CG01 TO CODIGO-CG03
                        PERFORM MOVER-DADOS-LB
                     END-IF
                     IF T-PESJUR-CG01 = 1
                        MOVE CODIGO-CG01 TO CODIGO-CG02
                        PERFORM MOVER-DADOS-LB
                     END-IF
                  END-IF
               END-READ
           END-PERFORM.

       PELA-CIDADE SECTION.
           INITIALIZE REG-CGD001
           START CGD001 KEY IS NOT LESS NOME-CG01 INVALID KEY
               MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
               READ CGD001 NEXT AT END
                   MOVE "10" TO ST-CGD001
               NOT AT END
                  PERFORM OPCAO-IMPRESSAO

                  IF IMPRIME-W = 1
                     IF T-PESFIS-CG01 = 1
                        MOVE CODIGO-CG01 TO CODIGO-CG03
                        READ CGD003 NOT INVALID KEY
                           IF CIDADE-RES-CG03 = CGP001-CIDADE-PESQ
                              PERFORM MOVER-DADOS-LB
                           END-IF
                        END-READ
                     END-IF
                     IF T-PESJUR-CG01 = 1
                        MOVE CODIGO-CG01 TO CODIGO-CG02
                        READ CGD002 NOT INVALID KEY
                           IF CIDADE1-CG02 = CGP001-CIDADE-PESQ
                              PERFORM MOVER-DADOS-LB
                           END-IF
                           IF CIDADE2-CG02 = CGP001-CIDADE-PESQ
                              PERFORM MOVER-DADOS-LB
                           END-IF
                        END-READ
                     END-IF
                  END-IF
               END-READ
           END-PERFORM.

       MOVER-DADOS-LB SECTION.
           IF SITUACAO-CG01 IS NOT NUMERIC
              MOVE 0 TO SITUACAO-CG01
           END-IF
           IF  OPCAO-w = 3 OR
              (OPCAO-w = 1 AND SITUACAO-CG01 <> 2) OR
              (OPCAO-w = 2 AND SITUACAO-CG01 =  2)
              MOVE CODIGO-CG01 TO CODIGO-CG05
              READ CGD005 INVALID KEY
                  MOVE 0 TO FUNCAO-CG05
              END-READ
              IF CGP001-FUNCAO-FC = 0 OR
                 CGP001-FUNCAO-FC = FUNCAO-CG05
                 MOVE CODIGO-CG01              TO CGP001-LINNOME(01: 06)
                 MOVE NOME-CG01                TO CGP001-LINNOME(13: 60)
                 MOVE CODIGO-CG01              TO CODIGO-CG02
                 READ CGD002 INVALID KEY
                      MOVE ZEROS               TO FONE1-CG02
                 END-READ
                 MOVE FONE1-CG02               TO MASC-TELEFONE
                 MOVE MASC-TELEFONE            TO CGP001-LINNOME(70:20)
                 MOVE "INSERE-LIST2" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.

       PELA-UF SECTION.
           INITIALIZE REG-CGD001
           START CGD001 KEY IS NOT LESS NOME-CG01 INVALID KEY
               MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
               READ CGD001 NEXT AT END
                   MOVE "10" TO ST-CGD001
               NOT AT END
                  PERFORM OPCAO-IMPRESSAO
                  IF IMPRIME-W = 1
                     IF T-PESFIS-CG01 = 1
                        MOVE CODIGO-CG01 TO CODIGO-CG03
                        READ CGD003 NOT INVALID KEY
                             MOVE CIDADE-RES-CG03 TO CIDADE
                             READ CAD010 NOT INVALID KEY
                                  IF UF-CID = CGP001-UF-PESQ
                                     PERFORM MOVER-DADOS-LB
                                  END-IF
                             END-READ
                        END-READ
                     END-IF
                     IF T-PESJUR-CG01 = 1
                        MOVE CODIGO-CG01 TO CODIGO-CG02
                        READ CGD002 NOT INVALID KEY
                           MOVE CIDADE1-CG02 TO CIDADE
                           READ CAD010 NOT INVALID KEY
                                IF UF-CID = CGP001-UF-PESQ
                                   PERFORM MOVER-DADOS-LB
                                END-IF
                           END-READ
                           MOVE CIDADE2-CG02 TO CIDADE
                           READ CAD010 NOT INVALID KEY
                                IF UF-CID = CGP001-UF-PESQ
                                   PERFORM MOVER-DADOS-LB
                                END-IF
                           END-READ
                        END-READ
                     END-IF
                  END-IF
               END-READ
           END-PERFORM.

       LER-CODIGO SECTION.
           MOVE CGP001-ACP-CODIGO TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES TO NOME-CG01.

           MOVE NOME-CG01 TO CGP001-DESC-NOME.

       LER-CIDADE SECTION.
           EVALUATE CGP001-ORDEM-CIDADE
             WHEN 1 MOVE CGP001-CIDADE1-J TO CIDADE
             WHEN 2 MOVE CGP001-CIDADE2-J TO CIDADE
             WHEN 3 MOVE CGP001-CIDADE1-F TO CIDADE
             WHEN 4 MOVE CGP001-CIDADE2-F TO CIDADE
             WHEN 5 MOVE CGP001-CIDADE-PESQ TO CIDADE
           END-EVALUATE.
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           EVALUATE CGP001-ORDEM-CIDADE
             WHEN 1 MOVE NOME-CID TO CGP001-NOME-CID1-J
                    MOVE DDD-CID  TO CGP001-DDD1-J
             WHEN 2 MOVE NOME-CID TO CGP001-NOME-CID2-J
                    MOVE DDD-CID  TO CGP001-DDD2-J
             WHEN 3 MOVE NOME-CID TO CGP001-NOME-CID1-F
                    MOVE DDD-CID  TO CGP001-DDD1-F
             WHEN 4 MOVE NOME-CID TO CGP001-NOME-CID2-F
                    MOVE DDD-CID  TO CGP001-DDD2-F
             WHEN 5 MOVE NOME-CID TO CGP001-DESC-CIDADE-PESQ
           END-EVALUATE.
       LER-APURACAO SECTION.
           MOVE CGP001-CONTA-APUR-RED-FVR TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY MOVE SPACES TO DESCRICAO-CX20.
           MOVE DESCRICAO-CX20 TO CGP001-DESC-APUR-FVR.
       LER-DEPTO SECTION.
           MOVE CGP001-DEPTO-FVR TO CODIGO-CA14.
           READ CAD014 INVALID KEY MOVE SPACES TO DEPARTAMENTO-CA14.
           MOVE DEPARTAMENTO-CA14 TO CGP001-NOME-DEPTO-FVR.
       LER-FUNCAO SECTION.
           MOVE CGP001-FUNCAO-FC TO CODIGO-RE02.
           READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02.
           MOVE DESCRICAO-RE02   TO CGP001-DESC-FUNCAO-FC.
       LER-VEICULO SECTION.
           MOVE CGP001-VEICULO-FC TO CODIGO-RE06.
           READ RED006 INVALID KEY MOVE SPACES TO VEICULO-RE06.
           MOVE VEICULO-RE06     TO CGP001-DESC-VEICULO-FC.
       LER-EMPRESA SECTION.
           MOVE CGP001-EMPRESA-FC   TO CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01        TO CGP001-DESC-EMPRESA-FC.

       POPUP-CODIGO SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(33: 6) TO CGP001-ACP-CODIGO
           PERFORM LER-CODIGO.

       POPUP-GERAL SECTION.
           EVALUATE CGP001-TIPO-POPUP
             WHEN 1 CALL   "CAP014T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "CAP014T"
                    MOVE PASSAR-STRING-1(36: 2) TO CGP001-DEPTO-FVR
                    PERFORM LER-DEPTO
             WHEN 2  CALL   "CXP020T" USING PARAMETROS-W
                                            PASSAR-PARAMETROS
                     CANCEL "CXP020T"
                     MOVE PASSAR-STRING-1(52: 5)
                                  TO CGP001-CONTA-APUR-RED-FVR
                     PERFORM LER-APURACAO
             WHEN 5  CALL   "REP002T" USING PARAMETROS-W
                                            PASSAR-PARAMETROS
                     CANCEL "REP002T"
                     MOVE PASSAR-STRING-1(22: 2) TO CGP001-FUNCAO-FC
                     PERFORM LER-FUNCAO
             WHEN 6  CALL   "REP006T" USING PARAMETROS-W
                                            PASSAR-PARAMETROS
                     CANCEL "REP006T"
                     MOVE PASSAR-STRING-1(32: 3) TO CGP001-VEICULO-FC
                     PERFORM LER-VEICULO
             WHEN 7  CALL   "CGP001T" USING PARAMETROS-W
                                            PASSAR-PARAMETROS
                     CANCEL "CGP001T"
                     MOVE PASSAR-STRING-1(33: 6) TO CGP001-EMPRESA-FC
                     PERFORM LER-EMPRESA
             WHEN 8  CALL   "CAP010T" USING PARAMETROS-W
                                            PASSAR-PARAMETROS
                     CANCEL "CAP010T"
                     EVALUATE CGP001-ORDEM-CIDADE
                        WHEN 1 MOVE PASSAR-STRING-1(35: 4)
                                    TO CGP001-CIDADE1-J
                        WHEN 2 MOVE PASSAR-STRING-1(35: 4)
                                       TO CGP001-CIDADE2-J
                        WHEN 3 MOVE PASSAR-STRING-1(35: 4)
                                       TO CGP001-CIDADE1-F
                        WHEN 4 MOVE PASSAR-STRING-1(35: 4)
                                       TO CGP001-CIDADE2-F
                        WHEN 5 MOVE PASSAR-STRING-1(35: 4)
                                       TO CGP001-CIDADE-PESQ

                     END-EVALUATE

                     PERFORM LER-CIDADE
           END-EVALUATE.

       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE CGP001-CODIGO       TO CODIGO-CG01 CODIGO-CG02
                                       CODIGO-CG03 CODIGO-CG04
                                       CODIGO-CG05 CODIGO-CG06
           INITIALIZE CGP001-DATA-BLOCK
           MOVE CODIGO-CG01          TO CGP001-CODIGO.

           READ CGD001 INVALID KEY INITIALIZE REG-CGD001
                                   MOVE 1 TO GRAVA-W.

           EVALUATE SITUACAO-CG01
               WHEN 2     MOVE "2 - Inativo" TO CGP001-DESC-SITUACAO
               WHEN OTHER MOVE "1 - Ativo"   TO CGP001-DESC-SITUACAO
           END-EVALUATE

           MOVE NOME-CG01            TO CGP001-NOME
           MOVE NOME-RED-CG01        TO CGP001-NOME-RED
           EVALUATE SEXO-CG01
               WHEN "F"   MOVE "Feminino " TO CGP001-SEXO
               WHEN "M"   MOVE "Masculino" TO CGP001-SEXO
               WHEN OTHER MOVE SPACES TO CGP001-SEXO
           END-EVALUATE
           MOVE T-PESFIS-CG01        TO CGP001-T-PES-FIS
           MOVE T-PESJUR-CG01        TO CGP001-T-PES-JUR
           MOVE T-FUNC-CG01          TO CGP001-T-FUNC
           MOVE T-REPRES-CG01        TO CGP001-T-REPRES
           MOVE T-FOTOG-CG01         TO CGP001-T-FOTOG
           MOVE T-CINEG-CG01         TO CGP001-T-CINEG
           MOVE T-VEND-CG01          TO CGP001-T-VEND.
           MOVE T-IMPOSTO-CG01       TO CGP001-T-IMPOSTO.
           MOVE T-INVESTIDOR-CG01    TO CGP001-T-INVESTIDOR.
           MOVE OUTRO3-CG01          TO CGP001-T-OUTRO
           MOVE T-TERCEIRIZADO-CG01  TO CGP001-T-TERCEIRIZADO
           MOVE T-FRANQUIA-CG01      TO CGP001-T-FRANQUIA
           MOVE COD-RED-CG01         TO CGP001-COD-REDUZIDO
      *    MOVE "VERIFICA-CHECK-BOX" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.

      *    VERIFICA P/ LIBERAR APENAS P/ REPORTAGEM, OU SEJA,
      *    APENAS P/ CONSULTA E ALTERAÇÃO.

           MOVE "SENHA10" TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY CONTINUE
             NOT INVALID KEY
                 MOVE "LIBERA-PARA-REPORT" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.

           READ CGD002 INVALID KEY
                INITIALIZE REG-CGD002.
      *    NOT INVALID KEY

           PERFORM CARREGAR-DADOS-CGD002.

           READ CGD003 INVALID KEY
                INITIALIZE REG-CGD003.
      *    NOT INVALID KEY
           PERFORM CARREGAR-DADOS-CGD003.

           READ CGD004 INVALID KEY
                INITIALIZE REG-CGD004.
      *    NOT INVALID KEY
           PERFORM CARREGAR-DADOS-CGD004.

           READ CGD005 INVALID KEY
                INITIALIZE REG-CGD005.
      *    NOT INVALID KEY
           PERFORM CARREGAR-DADOS-CGD005.

           MOVE "APAGA-LB3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM CARREGAR-CONTAS.

       CARREGAR-CONTAS SECTION.
           INITIALIZE REG-CGD006
           MOVE CGP001-CODIGO          TO CODIGO-CG06
           START CGD006 KEY IS NOT LESS CHAVE-CG06 INVALID KEY
               MOVE "10" TO ST-CGD006.
           PERFORM UNTIL ST-CGD006 = "10"
               READ CGD006 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD006
               NOT AT END
                   IF CGP001-CODIGO <> CODIGO-CG06
                      MOVE "10" TO ST-CGD006
                   ELSE
                      MOVE SPACES TO CGP001-LINDET
                      EVALUATE PREFERENCIAL-CG06
                          WHEN 1     MOVE "X"    TO CGP001-LINDET(1:1)
                          WHEN OTHER MOVE SPACES TO CGP001-LINDET(1:1)
                      END-EVALUATE

                      EVALUATE TIPO-DE-CONTA-CG06
                          WHEN 1     MOVE "Cta.Corrente" TO
                                     CGP001-LINDET(3:12)
                          WHEN 2     MOVE "Cta.Poupança" TO
                                     CGP001-LINDET(3:12)
                          WHEN OTHER MOVE "Nao Inform. " TO
                                     CGP001-LINDET(3:12)
                      END-EVALUATE

                      MOVE BANCO-CG06         TO CODIGO-CAD30
                                                 CGP001-LINDET(16:4)
                      READ CAD030 INVALID KEY
                           MOVE SPACES        TO DESCRICAO-CAD30
                      END-READ
                      MOVE DESCRICAO-CAD30    TO CGP001-LINDET(21:16)
                      MOVE AGENCIA-CG06       TO CGP001-LINDET(38:9)
                      MOVE NR-CONTA-CG06      TO CGP001-LINDET(48:15)
                      MOVE COMPLEMENTO-CONTA-CG06 TO
                                                 CGP001-LINDET(63:12)
                      MOVE USUARIO-CG06       TO CGP001-LINDET(76:3)
                      MOVE DESC-USUARIO-CG06  TO CGP001-LINDET(80:4)
                      MOVE CPF-TITULAR-CG06   TO CGP001-LINDET(85:11)
                      MOVE CNPJ-TITULAR-CG06  TO CGP001-LINDET(97:14)
                      MOVE TITULAR-CONTA-CG06 TO CGP001-LINDET(112:40)
                      MOVE PREFERENCIAL-CG06  TO CGP001-LINDET(153:1)

                      MOVE "INSERE-LIST3"     TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.


       CARREGAR-DADOS-CGD002 SECTION.
           MOVE ENDERECO1-CG02       TO CGP001-ENDERECO1-J
           MOVE BAIRRO1-CG02         TO CGP001-BAIRRO1-J
           MOVE CIDADE1-CG02         TO CGP001-CIDADE1-J CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE DDD-CID              TO CGP001-DDD1-J
           MOVE NOME-CID             TO CGP001-NOME-CID1-J
           MOVE CEP1-CG02            TO CGP001-CEP1-J
           MOVE FONE1-CG02           TO CGP001-FONE1-J
           MOVE RAMAL1-CG02          TO CGP001-RAMAL1-J
           MOVE ENDERECO2-CG02       TO CGP001-ENDERECO2-J
           MOVE BAIRRO2-CG02         TO CGP001-BAIRRO2-J
           MOVE CIDADE2-CG02         TO CGP001-CIDADE2-J CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE DDD-CID              TO CGP001-DDD2-J
           MOVE NOME-CID             TO CGP001-NOME-CID2-J
           MOVE CEP2-CG02            TO CGP001-CEP2-J
           MOVE FONE2-CG02           TO CGP001-FONE2-J
           MOVE AGENCIA-CG02         TO CGP001-AGENCIA-J
           MOVE CX-POSTAL-CG02       TO CGP001-CX-POSTAL-J
           MOVE RAMAL2-CG02          TO CGP001-RAMAL2-J
      *    MOVE USUARIO-CG02         TO CGP001-CX-POSTAL-J
           MOVE E-MAIL-CG02          TO CGP001-E-MAIL-J
           MOVE SITE-CG02            TO CGP001-SITE-J
           MOVE CELULAR-CG02         TO CGP001-CELULAR-J
           MOVE FAX-CG02             TO CGP001-FAX-J
           MOVE CGC-CG02             TO CGP001-CGC-J
           MOVE INSC-EST-CG02        TO CGP001-INSC-J
           MOVE CONTATO-CG02         TO CGP001-CONTATO-J
           MOVE FUNCAO-CONTATO-CG02  TO CGP001-FUNCAO-J
           MOVE BANCO-CG02           TO CGP001-BANCO-J
           MOVE AGENCIA-CG02         TO CGP001-AGENCIA-J
           MOVE NR-CONTA-CG02        TO CGP001-CONTA-BANCO-J.

       CARREGAR-DADOS-CGD003 SECTION.
           MOVE ENDERECO-RES-CG03    TO CGP001-ENDERECO1-F
           MOVE BAIRRO-RES-CG03      TO CGP001-BAIRRO1-F
           MOVE CIDADE-RES-CG03      TO CGP001-CIDADE1-F CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID             TO CGP001-NOME-CID1-F
           MOVE DDD-CID              TO CGP001-DDD1-F
           MOVE CEP-RES-CG03         TO CGP001-CEP1-F
           MOVE FONE-RES-CG03        TO CGP001-FONE1-F
           MOVE ENDERECO-COML-CG03   TO CGP001-ENDERECO2-F
           MOVE BAIRRO-COML-CG03     TO CGP001-BAIRRO2-F
           MOVE CIDADE-COML-CG03     TO CGP001-CIDADE2-F CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID             TO CGP001-NOME-CID2-F
           MOVE DDD-CID              TO CGP001-DDD2-F
           MOVE CEP-COML-CG03        TO CGP001-CEP2-F
           MOVE FONE-COML-CG03       TO CGP001-FONE2-F
           MOVE E-MAIL-CG03          TO CGP001-E-MAIL-F
           MOVE CELULAR-CG03         TO CGP001-CELULAR-F
           MOVE FAX-CG03             TO CGP001-FAX-F
           MOVE CPF-CG03             TO CGP001-CPF-F
           MOVE RG-CG03              TO CGP001-RG-F
           MOVE CONTATO-KELLO-CG03   TO CGP001-CONTATO-F
           MOVE BANCO-CG03           TO CGP001-BANCO-F
           MOVE AGENCIA-CG03         TO CGP001-AGENCIA-F
           MOVE CX-POSTAL-CG03       TO CGP001-CX-POSTAL-F
           MOVE RAMAL-COML-CG03      TO CGP001-RAMAL2-F
      *    MOVE USUARIO-CG03         TO CGP001-USUARIO
           MOVE NR-CONTA-CG03        TO CGP001-CONTA-BANCO-F
           MOVE NOME-CONJUGE-CG03    TO CGP001-CONJUGE-F
           MOVE PROFIS-FORM-CG03     TO CGP001-FORMACAO-F.

       CARREGAR-DADOS-CGD004 SECTION.
           MOVE DEPTO-CG04           TO CGP001-DEPTO-FVR CODIGO-CA14.
           READ CAD014 INVALID KEY MOVE SPACES TO DEPARTAMENTO-CA14.
           MOVE DEPARTAMENTO-CA14    TO CGP001-NOME-DEPTO-FVR.
           MOVE CONTA-APUR-RED-CG04  TO CGP001-CONTA-APUR-RED-FVR
                                        CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY MOVE SPACES TO DESCRICAO-CX20.
           MOVE DESCRICAO-CX20       TO CGP001-DESC-APUR-FVR
           MOVE BANCO-CG04           TO CGP001-BANCO-FVR
           MOVE NOME-BANCO-CG04      TO CGP001-NOME-BANCO-FVR
           MOVE AGENCIA-CG04         TO CGP001-AGENCIA-FVR
           MOVE DIGITO-AGENCIA-CG04  TO CGP001-DIGITO-AGENCIA-FVR
           MOVE NR-CONTA-CG04        TO CGP001-NR-CONTA-FVR
           MOVE DIGITO-CONTA-CG04    TO CGP001-DIGITO-CONTA-FVR.
           MOVE COD-VENDEDOR-CG04    TO CGP001-COD-VENDEDOR-FVR.

       CARREGAR-DADOS-CGD005 SECTION.
           MOVE CODIGO-REPORT-CG05   TO CGP001-COD-REPORT-FC.
           MOVE DATA-NASC-CG05       TO CGP001-NASCIMENTO-FC.
           MOVE OCUPACAO-CG05        TO CGP001-OCUPACAO-FC
           MOVE PADRAO-CG05          TO CGP001-PADRAO-FC
           MOVE FUNCAO-CG05          TO CGP001-FUNCAO-FC CODIGO-RE02
           READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02
           END-READ
           MOVE DESCRICAO-RE02       TO CGP001-DESC-FUNCAO-FC
           MOVE ATRIBUICAO-CG05      TO CGP001-ATRIBUICAO-FC
           MOVE RESTRICAO-CG05       TO CGP001-RESTRICAO-FC
           MOVE VEICULO-CG05         TO CGP001-VEICULO-FC CODIGO-RE06
           READ RED006 INVALID KEY MOVE SPACES TO VEICULO-RE06
           END-READ
           MOVE VEICULO-RE06         TO CGP001-DESC-VEICULO-FC
           MOVE VENCTO-HABIL-CG05    TO CGP001-VENC-HABILIT-FC
           MOVE VENCTO-SEGURO-CG05   TO CGP001-VENC-SEGURO-FC
           MOVE SEGURADORA-CG05      TO CGP001-SEGURADORA-FC
           MOVE CONTA-EXTRATO-CG05   TO CGP001-CONTA-EXTRATO-FC
           MOVE EMPRESA-CG05         TO CGP001-EMPRESA-FC CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
           END-READ
           MOVE NOME-CG01            TO CGP001-DESC-EMPRESA-FC
           MOVE CONTA-BANCO1-CG05    TO CGP001-CONTA-BANCO1-FC
           MOVE CONTA-BANCO2-CG05    TO CGP001-CONTA-BANCO2-FC.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CGD001
           INITIALIZE REG-CGD002
           INITIALIZE REG-CGD003
           INITIALIZE REG-CGD004
           INITIALIZE REG-CGD005
           INITIALIZE REG-CGD006
           MOVE "APAGA-LB3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE CGP001-ORDER TO ORDEM-W
           INITIALIZE CGP001-DATA-BLOCK
           MOVE ORDEM-W TO CGP001-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       EXCLUI-RECORD SECTION.
           CLOSE    CGD001 CGD002 CGD003 CGD004 CGD005 CGD006 CGD010
           OPEN I-O CGD001 CGD002 CGD003 CGD004 CGD005 CGD006 CGD010
                    LOG002

           MOVE CGP001-CODIGO TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE "Cadastro Nao Encontrado" TO MENSAGEM
                MOVE "C"  TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE 9                TO CLASSIF-CG10
                MOVE CODIGO-CG01      TO CODIGO-CG10
                READ CGD010 NOT INVALID KEY
                     MOVE USUARIO-W   TO LOG1-USUARIO
                     MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                     MOVE WS-DATA-CPU TO LOG1-DATA
                     ACCEPT WS-HORA-SYS FROM TIME
                     MOVE WS-HORA-SYS TO LOG1-HORAS
                     MOVE "E"         TO LOG1-OPERACAO
                     MOVE "CGD010"    TO LOG1-ARQUIVO
                     MOVE "CGP001"    TO LOG1-PROGRAMA
                     MOVE REG-CGD010  TO LOG1-REGISTRO
                     WRITE REG-LOG001
                     END-WRITE
                END-READ
                DELETE CGD001 NOT INVALID KEY
                    MOVE USUARIO-W   TO LOG2-USUARIO
                    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                    MOVE WS-DATA-CPU TO LOG2-DATA
                    ACCEPT WS-HORA-SYS FROM TIME
                    MOVE WS-HORA-SYS TO LOG2-HORAS
                    MOVE "E"         TO LOG2-OPERACAO
                    MOVE "CGD001"    TO LOG2-ARQUIVO
                    MOVE "CGP001"    TO LOG2-PROGRAMA
                    MOVE REG-CGD001  TO LOG2-REGISTRO
                    WRITE REG-LOG002
                    END-WRITE
                END-DELETE

                MOVE CODIGO-CG01 TO CODIGO-CG02
                READ CGD002 NOT INVALID KEY
                    DELETE CGD002
                END-READ

                MOVE CODIGO-CG01 TO CODIGO-CG03
                READ CGD003 NOT INVALID KEY
                    DELETE CGD003
                END-READ

                MOVE CODIGO-CG01 TO CODIGO-CG04
                READ CGD004 NOT INVALID KEY
                    DELETE CGD004
                END-READ

                MOVE CODIGO-CG01 TO CODIGO-CG05
                READ CGD005 NOT INVALID KEY
                    DELETE CGD005
                END-READ

                MOVE CODIGO-CG01 TO CODIGO-CG06
                READ CGD006 NOT INVALID KEY
                    DELETE CGD006
                END-READ
           END-READ


           CLOSE      CGD001 CGD002 CGD003 CGD004 CGD005 CGD006 CGD010
                      LOG002
           OPEN INPUT CGD001 CGD002 CGD003 CGD004 CGD005 CGD006 CGD010
           PERFORM LIMPAR-DADOS.

       SALVAR-DADOS SECTION.
           CLOSE    CGD001 CGD010
           OPEN I-O CGD001 CGD010
                    LOG002

           MOVE CGP001-CODIGO        TO  CODIGO-CG01 CODIGO-CG02
                                         CODIGO-CG03 CODIGO-CG04
                                         CODIGO-CG05.
           MOVE CGP001-NOME          TO  NOME-CG01
           MOVE CGP001-NOME-RED      TO  NOME-RED-CG01
           IF CGP001-T-PES-FIS = ZEROS AND CGP001-T-PES-JUR = ZEROS
              AND CGP001-T-FUNC = ZEROS AND CGP001-T-REPRES = ZEROS
               AND CGP001-T-FOTOG = ZEROS AND CGP001-T-CINEG = ZEROS
                AND CGP001-T-VEND = ZEROS AND CGP001-T-IMPOSTO = ZEROS
                 AND CGP001-T-INVESTIDOR = ZEROS AND CGP001-T-OUTRO = 0
                   MOVE "Nenhuma característica foi declarada"
                        TO CGP001-MENSAGEM-ERRO
                   PERFORM LOAD-SCREENSET
                   PERFORM CARREGA-MENSAGEM-ERRO
                   MOVE "EXIBE-ERRO" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM.

           MOVE CGP001-DESC-SITUACAO(1:1) TO SITUACAO-CG01

           MOVE CGP001-T-PES-FIS      TO  T-PESFIS-CG01
           MOVE CGP001-T-PES-JUR      TO  T-PESJUR-CG01
           MOVE CGP001-T-FUNC         TO  T-FUNC-CG01
           MOVE CGP001-T-REPRES       TO  T-REPRES-CG01
           MOVE CGP001-T-FOTOG        TO  T-FOTOG-CG01
           MOVE CGP001-T-CINEG        TO  T-CINEG-CG01
           MOVE CGP001-T-VEND         TO  T-VEND-CG01
           MOVE CGP001-T-IMPOSTO      TO  T-IMPOSTO-CG01
           MOVE CGP001-T-INVESTIDOR   TO  T-INVESTIDOR-CG01
           MOVE CGP001-T-OUTRO        TO  OUTRO3-CG01
           MOVE CGP001-T-TERCEIRIZADO TO T-TERCEIRIZADO-CG01
           MOVE CGP001-T-FRANQUIA     TO T-FRANQUIA-CG01
           MOVE CGP001-SEXO(1:1)      TO  SEXO-CG01
           MOVE CGP001-COD-REDUZIDO   TO  COD-RED-CG01
           IF GRAVA-W = 1
              WRITE REG-CGD001 INVALID KEY
                   ADD 1 TO ULT-CODIGO
                   MOVE ULT-CODIGO TO CODIGO-CG01
                   WRITE REG-CGD001 INVALID KEY
                         PERFORM ERRO-GRAVACAO
                   NOT INVALID KEY
                       PERFORM GRAVAR-CGD010
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "I"         TO LOG2-OPERACAO
                       MOVE "CGD001"    TO LOG2-ARQUIVO
                       MOVE "CGP001"    TO LOG2-PROGRAMA
                       MOVE REG-CGD001  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
                       MOVE "00" TO ST-CGD001
                   END-WRITE
                   IF ST-CGD001 <> "00"
                      PERFORM ERRO-GRAVACAO
                   END-IF
              NOT INVALID KEY
                   PERFORM GRAVAR-CGD010
                   MOVE USUARIO-W   TO LOG2-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "I"         TO LOG2-OPERACAO
                   MOVE "CGD001"    TO LOG2-ARQUIVO
                   MOVE "CGP001"    TO LOG2-PROGRAMA
                   MOVE REG-CGD001  TO LOG2-REGISTRO
                   WRITE REG-LOG002
                   END-WRITE
                   MOVE "00" TO ST-CGD001
              END-WRITE
              IF ST-CGD001 <> "00"
                 PERFORM ERRO-GRAVACAO
              END-IF
           ELSE
              REWRITE REG-CGD001 INVALID KEY
                  PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                  MOVE USUARIO-W   TO LOG2-USUARIO
                  MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                  MOVE WS-DATA-CPU TO LOG2-DATA
                  ACCEPT WS-HORA-SYS FROM TIME
                  MOVE WS-HORA-SYS TO LOG2-HORAS
                  MOVE "A"         TO LOG2-OPERACAO
                  MOVE "CGD001"    TO LOG2-ARQUIVO
                  MOVE "CGP001"    TO LOG2-PROGRAMA
                  MOVE REG-CGD001  TO LOG2-REGISTRO
                  WRITE REG-LOG002
                  END-WRITE
                  SUBTRACT 1 FROM ULT-CODIGO
                  MOVE "00" TO ST-CGD001
              END-REWRITE
              IF ST-CGD001 <> "00"
                 PERFORM ERRO-GRAVACAO.

           CLOSE      CGD001 CGD010 LOG002
           OPEN INPUT CGD001 CGD010

           IF CGP001-GRAVA-CGP002 = 1
              IF CGP001-T-PES-JUR-TRUE
                 PERFORM SALVAR-DADOS-CGD002.

           IF CGP001-GRAVA-CGP003 = 1
              IF CGP001-T-PES-FIS-TRUE
                 PERFORM SALVAR-DADOS-CGD003.

           IF CGP001-GRAVA-CGP004 = 1
              IF CGP001-T-FUNC-TRUE OR CGP001-T-VEND-TRUE OR
                 CGP001-T-REPRES-TRUE
                 PERFORM SALVAR-DADOS-CGD004.

           IF CGP001-GRAVA-CGP005 = 1
              IF CGP001-T-FOTOG-TRUE OR CGP001-T-CINEG-TRUE
                 PERFORM SALVAR-DADOS-CGD005.

           IF CGP001-GRAVA-CGP006 = 1
              PERFORM SALVAR-DADOS-BANCOS.

           IF CGP001-T-PES-JUR = 0
              CLOSE CGD002
              OPEN I-O CGD002 LOG004
              READ CGD002 INVALID KEY
                   CONTINUE
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG4-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG4-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG4-HORAS
                   MOVE "E"         TO LOG4-OPERACAO
                   MOVE "CGD002"    TO LOG4-ARQUIVO
                   MOVE "CGP001"    TO LOG4-PROGRAMA
                   MOVE REG-CGD002  TO LOG4-REGISTRO
                   WRITE REG-LOG004
                   END-WRITE
                   DELETE CGD002
                   END-DELETE
              END-READ
              CLOSE CGD002 LOG004
              OPEN INPUT CGD002.

           IF CGP001-T-PES-FIS = 0
              CLOSE CGD003
              OPEN I-O CGD003 LOG004
              READ CGD003 INVALID KEY
                   CONTINUE
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG4-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG4-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG4-HORAS
                   MOVE "E"         TO LOG4-OPERACAO
                   MOVE "CGD003"    TO LOG4-ARQUIVO
                   MOVE "CGP001"    TO LOG4-PROGRAMA
                   MOVE REG-CGD003  TO LOG4-REGISTRO
                   WRITE REG-LOG004
                   END-WRITE
                   DELETE CGD003
                   END-DELETE
              END-READ
              CLOSE CGD003 LOG004
              OPEN INPUT CGD003.

           IF CGP001-T-FUNC = 0 AND CGP001-T-VEND = 0
                                                 AND CGP001-T-REPRES = 0
              CLOSE CGD004
              OPEN I-O CGD004 LOG001
              READ CGD004 INVALID KEY
                   CONTINUE
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG1-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG1-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG1-HORAS
                   MOVE "E"         TO LOG1-OPERACAO
                   MOVE "CGD004"    TO LOG1-ARQUIVO
                   MOVE "CGP001"    TO LOG1-PROGRAMA
                   MOVE REG-CGD004  TO LOG1-REGISTRO
                   WRITE REG-LOG001
                   END-WRITE
                   DELETE CGD004
                   END-DELETE
              END-READ
              CLOSE CGD004 LOG001
              OPEN INPUT CGD004.

           IF CGP001-T-FOTOG = 0 AND CGP001-T-CINEG = 0
              CLOSE CGD005
              OPEN I-O CGD005 LOG003
              READ CGD005 INVALID KEY
                   CONTINUE
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG3-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG3-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG3-HORAS
                   MOVE "E"         TO LOG3-OPERACAO
                   MOVE "CGD005"    TO LOG3-ARQUIVO
                   MOVE "CGP001"    TO LOG3-PROGRAMA
                   MOVE REG-CGD005  TO LOG3-REGISTRO
                   WRITE REG-LOG003
                   END-WRITE
                   DELETE CGD005
                   END-DELETE
              END-READ
              CLOSE CGD005 LOG003
              OPEN INPUT CGD005.

       GRAVAR-CGD010 SECTION.
           INITIALIZE REG-CGD010
           MOVE 9                  TO CLASSIF-CG10
           MOVE CODIGO-CG01        TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE NOME-CG01     TO COMPRADOR-CG10
                WRITE REG-CGD010 INVALID KEY
                      PERFORM ERRO-GRAVACAO
                NOT INVALID KEY
                      MOVE USUARIO-W   TO LOG1-USUARIO
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      MOVE WS-DATA-CPU TO LOG1-DATA
                      ACCEPT WS-HORA-SYS FROM TIME
                      MOVE WS-HORA-SYS TO LOG1-HORAS
                      MOVE "I"         TO LOG1-OPERACAO
                      MOVE "CGD010"    TO LOG1-ARQUIVO
                      MOVE "CGP001"    TO LOG1-PROGRAMA
                      MOVE REG-CGD010  TO LOG1-REGISTRO
                END-WRITE
           NOT INVALID KEY
                MOVE NOME-CG01     TO COMPRADOR-CG10
                REWRITE REG-CGD010 INVALID KEY
                      PERFORM ERRO-GRAVACAO
                NOT INVALID KEY
                      MOVE USUARIO-W   TO LOG1-USUARIO
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      MOVE WS-DATA-CPU TO LOG1-DATA
                      ACCEPT WS-HORA-SYS FROM TIME
                      MOVE WS-HORA-SYS TO LOG1-HORAS
                      MOVE "A"         TO LOG1-OPERACAO
                      MOVE "CGD010"    TO LOG1-ARQUIVO
                      MOVE "CGP001"    TO LOG1-PROGRAMA
                      MOVE REG-CGD010  TO LOG1-REGISTRO
                      WRITE REG-LOG001
                      END-WRITE
                END-REWRITE
           END-READ.

       SALVAR-DADOS-CGD002 SECTION.
           CLOSE CGD002
           OPEN I-O CGD002 LOG004
      *    IF CGP001-USUARIO NOT > ZEROS
      *       MOVE USUARIO-W         TO CGP001-USUARIO
      *    END-IF
           MOVE CODIGO-CG01          TO CODIGO-CG02
           MOVE CGP001-ENDERECO1-J   TO ENDERECO1-CG02
           MOVE CGP001-BAIRRO1-J     TO BAIRRO1-CG02
           MOVE CGP001-CIDADE1-J     TO CIDADE1-CG02
           MOVE CGP001-CEP1-J        TO CEP1-CG02
           MOVE CGP001-FONE1-J       TO FONE1-CG02
           MOVE CGP001-RAMAL1-J      TO RAMAL1-CG02
           MOVE CGP001-ENDERECO2-J   TO ENDERECO2-CG02
           MOVE CGP001-BAIRRO2-J     TO BAIRRO2-CG02
           MOVE CGP001-CIDADE2-J     TO CIDADE2-CG02
           MOVE CGP001-CEP2-J        TO CEP2-CG02
           MOVE CGP001-FONE2-J       TO FONE2-CG02
           MOVE CGP001-E-MAIL-J      TO E-MAIL-CG02
           MOVE CGP001-SITE-J        TO SITE-CG02
           MOVE CGP001-CELULAR-J     TO CELULAR-CG02
           MOVE CGP001-FAX-J         TO FAX-CG02
           MOVE CGP001-CGC-J         TO CGC-CG02
           MOVE CGP001-INSC-J        TO INSC-EST-CG02
           MOVE CGP001-CONTATO-J     TO CONTATO-CG02
           MOVE CGP001-FUNCAO-J      TO FUNCAO-CONTATO-CG02
           MOVE CGP001-BANCO-J       TO BANCO-CG02
           MOVE CGP001-AGENCIA-J     TO AGENCIA-CG02
           MOVE CGP001-CX-POSTAL-J   TO CX-POSTAL-CG02
           MOVE CGP001-RAMAL2-J      TO RAMAL2-CG02
      *    MOVE CGP001-USUARIO       TO USUARIO-CG02
           MOVE CGP001-CONTA-BANCO-J TO NR-CONTA-CG02
           WRITE REG-CGD002 INVALID KEY
                 REWRITE REG-CGD002 INVALID KEY
                    MOVE ST-CGD002 TO ST-CGD001
                    PERFORM ERRO-GRAVACAO
                 NOT INVALID KEY
                    MOVE USUARIO-W   TO LOG4-USUARIO
                    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                    MOVE WS-DATA-CPU TO LOG4-DATA
                    ACCEPT WS-HORA-SYS FROM TIME
                    MOVE WS-HORA-SYS TO LOG4-HORAS
                    MOVE "A"         TO LOG4-OPERACAO
                    MOVE "CGD002"    TO LOG4-ARQUIVO
                    MOVE "CGP001"    TO LOG4-PROGRAMA
                    MOVE REG-CGD002  TO LOG4-REGISTRO
                    WRITE REG-LOG004
                    END-WRITE
                    MOVE "00" TO ST-CGD002
                 END-REWRITE
                 IF ST-CGD002 <> "00"
                    PERFORM ERRO-GRAVACAO
                 END-IF
           NOT INVALID KEY
              MOVE USUARIO-W   TO LOG4-USUARIO
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              MOVE WS-DATA-CPU TO LOG4-DATA
              ACCEPT WS-HORA-SYS FROM TIME
              MOVE WS-HORA-SYS TO LOG4-HORAS
              MOVE "I"         TO LOG4-OPERACAO
              MOVE "CGD002"    TO LOG4-ARQUIVO
              MOVE "CGP001"    TO LOG4-PROGRAMA
              MOVE REG-CGD002  TO LOG4-REGISTRO
              WRITE REG-LOG004
              END-WRITE
              MOVE "00" TO ST-CGD002
           END-WRITE.
           IF ST-CGD002 <> "00"
              MOVE ST-CGD002 TO ST-CGD001
              PERFORM ERRO-GRAVACAO
           END-IF
           CLOSE CGD002 LOG004
           OPEN INPUT CGD002.

       SALVAR-DADOS-CGD003 SECTION.
           CLOSE CGD003
           OPEN I-O CGD003 LOG004
      *    IF CGP001-USUARIO NOT > 0
      *       MOVE USUARIO-W            TO CGP001-USUARIO
      *    END-IF
           MOVE CODIGO-CG01             TO CODIGO-CG03
           MOVE CGP001-ENDERECO1-F      TO ENDERECO-RES-CG03
           MOVE CGP001-BAIRRO1-F        TO BAIRRO-RES-CG03
           MOVE CGP001-CIDADE1-F        TO CIDADE-RES-CG03
           MOVE CGP001-CEP1-F           TO CEP-RES-CG03
           MOVE CGP001-FONE1-F          TO FONE-RES-CG03
           MOVE CGP001-ENDERECO2-F      TO ENDERECO-COML-CG03
           MOVE CGP001-BAIRRO2-F        TO BAIRRO-COML-CG03
           MOVE CGP001-CIDADE2-F        TO CIDADE-COML-CG03
           MOVE CGP001-CEP2-F           TO CEP-COML-CG03
           MOVE CGP001-FONE2-F          TO FONE-COML-CG03
           MOVE CGP001-E-MAIL-F         TO E-MAIL-CG03
           MOVE CGP001-CELULAR-F        TO CELULAR-CG03
           MOVE CGP001-FAX-F            TO FAX-CG03
           MOVE CGP001-CPF-F            TO CPF-CG03
           MOVE CGP001-RG-F             TO RG-CG03
           MOVE CGP001-CONTATO-F        TO CONTATO-KELLO-CG03
           MOVE CGP001-BANCO-F          TO BANCO-CG03
           MOVE CGP001-AGENCIA-F        TO AGENCIA-CG03
           MOVE CGP001-CX-POSTAL-F      TO CX-POSTAL-CG03
           MOVE CGP001-RAMAL2-F         TO RAMAL-COML-CG03
           MOVE CGP001-CONTA-BANCO-F    TO NR-CONTA-CG03
           MOVE CGP001-CONJUGE-F        TO NOME-CONJUGE-CG03
           MOVE CGP001-FORMACAO-F       TO PROFIS-FORM-CG03
      *    MOVE CGP001-USUARIO          TO USUARIO-CG03
           WRITE REG-CGD003 INVALID KEY
              REWRITE REG-CGD003 INVALID KEY
                 MOVE ST-CGD003 TO ST-CGD001
                 PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG4-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG4-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG4-HORAS
                 MOVE "A"         TO LOG4-OPERACAO
                 MOVE "CGD003"    TO LOG4-ARQUIVO
                 MOVE "CGP001"    TO LOG4-PROGRAMA
                 MOVE REG-CGD003  TO LOG4-REGISTRO
                 WRITE REG-LOG004
                 END-WRITE
                 MOVE "00" TO ST-CGD003
              END-REWRITE
           NOT INVALID KEY
              MOVE USUARIO-W   TO LOG4-USUARIO
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              MOVE WS-DATA-CPU TO LOG4-DATA
              ACCEPT WS-HORA-SYS FROM TIME
              MOVE WS-HORA-SYS TO LOG4-HORAS
              MOVE "I"         TO LOG4-OPERACAO
              MOVE "CGD003"    TO LOG4-ARQUIVO
              MOVE "CGP001"    TO LOG4-PROGRAMA
              MOVE REG-CGD003  TO LOG4-REGISTRO
              WRITE REG-LOG004
              END-WRITE
              MOVE "00" TO ST-CGD003
           END-WRITE
           IF ST-CGD003 <> "00"
              MOVE ST-CGD003 TO ST-CGD001
              PERFORM ERRO-GRAVACAO.

           CLOSE CGD003 LOG004
           OPEN INPUT CGD003.

       SALVAR-DADOS-CGD004 SECTION.
           CLOSE CGD004
           OPEN I-O CGD004 LOG001
           MOVE CGP001-DEPTO-FVR          TO DEPTO-CG04.
           MOVE CGP001-CONTA-APUR-RED-FVR TO CONTA-APUR-RED-CG04
           MOVE CGP001-BANCO-FVR          TO BANCO-CG04
           MOVE CGP001-NOME-BANCO-FVR     TO NOME-BANCO-CG04
           MOVE CGP001-AGENCIA-FVR        TO AGENCIA-CG04.
           MOVE CGP001-DIGITO-AGENCIA-FVR TO DIGITO-AGENCIA-CG04
           MOVE CGP001-NR-CONTA-FVR       TO NR-CONTA-CG04
           MOVE CGP001-DIGITO-CONTA-FVR   TO DIGITO-CONTA-CG04.
           MOVE CGP001-COD-VENDEDOR-FVR   TO COD-VENDEDOR-CG04.
           WRITE REG-CGD004 INVALID KEY
               REWRITE REG-CGD004 INVALID KEY
                   MOVE ST-CGD004 TO ST-CGD001
                   PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG1-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG1-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG1-HORAS
                   MOVE "A"         TO LOG1-OPERACAO
                   MOVE "CGD004"    TO LOG1-ARQUIVO
                   MOVE "CGP001"    TO LOG1-PROGRAMA
                   MOVE REG-CGD004  TO LOG1-REGISTRO
                   WRITE REG-LOG001
                   END-WRITE
                   MOVE "00" TO ST-CGD004
               END-REWRITE
           NOT INVALID KEY
               MOVE USUARIO-W   TO LOG1-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG1-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG1-HORAS
               MOVE "I"         TO LOG1-OPERACAO
               MOVE "CGD004"    TO LOG1-ARQUIVO
               MOVE "CGP001"    TO LOG1-PROGRAMA
               MOVE REG-CGD004  TO LOG1-REGISTRO
               WRITE REG-LOG001
               END-WRITE
               MOVE "00" TO ST-CGD004
           END-WRITE
           IF ST-CGD004 <> "00"
              MOVE ST-CGD004 TO ST-CGD001
              PERFORM ERRO-GRAVACAO.

           CLOSE CGD004 LOG001
           OPEN INPUT CGD004.

       SALVAR-DADOS-CGD005 SECTION.
           CLOSE CGD005
           OPEN I-O CGD005 LOG003
           MOVE CGP001-COD-REPORT-FC    TO CODIGO-REPORT-CG05
           MOVE CGP001-NASCIMENTO-FC    TO DATA-NASC-CG05
           MOVE CGP001-OCUPACAO-FC      TO OCUPACAO-CG05
           MOVE CGP001-PADRAO-FC        TO PADRAO-CG05
           MOVE CGP001-FUNCAO-FC        TO FUNCAO-CG05
           MOVE CGP001-ATRIBUICAO-FC    TO ATRIBUICAO-CG05
           MOVE CGP001-RESTRICAO-FC     TO RESTRICAO-CG05
           MOVE CGP001-VEICULO-FC       TO VEICULO-CG05
           MOVE CGP001-VENC-HABILIT-FC  TO VENCTO-HABIL-CG05
           MOVE CGP001-VENC-SEGURO-FC   TO VENCTO-SEGURO-CG05
           MOVE CGP001-SEGURADORA-FC    TO SEGURADORA-CG05
           MOVE CGP001-CONTA-EXTRATO-FC TO CONTA-EXTRATO-CG05
           MOVE CGP001-EMPRESA-FC       TO EMPRESA-CG05
           MOVE CGP001-CONTA-BANCO1-FC  TO CONTA-BANCO1-CG05
           MOVE CGP001-CONTA-BANCO2-FC  TO CONTA-BANCO2-CG05
           WRITE REG-CGD005 INVALID KEY
               REWRITE REG-CGD005 INVALID KEY
                   MOVE ST-CGD005 TO ST-CGD001
                   PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG3-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG3-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG3-HORAS
                   MOVE "A"         TO LOG3-OPERACAO
                   MOVE "CGD005"    TO LOG3-ARQUIVO
                   MOVE "CGP001"    TO LOG3-PROGRAMA
                   MOVE REG-CGD005  TO LOG3-REGISTRO
                   WRITE REG-LOG003
                   END-WRITE
                   MOVE "00" TO ST-CGD005
               END-REWRITE
           NOT INVALID KEY
               MOVE USUARIO-W   TO LOG3-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG3-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG3-HORAS
               MOVE "A"         TO LOG3-OPERACAO
               MOVE "CGD005"    TO LOG3-ARQUIVO
               MOVE "CGP001"    TO LOG3-PROGRAMA
               MOVE REG-CGD005  TO LOG3-REGISTRO
               WRITE REG-LOG003
               END-WRITE
               MOVE "00" TO ST-CGD005
           END-WRITE
           IF ST-CGD005 <> "00"
              MOVE ST-CGD005 TO ST-CGD001
              PERFORM ERRO-GRAVACAO.

           CLOSE CGD005 LOG003
           OPEN INPUT CGD005.

       SALVAR-DADOS-BANCOS SECTION.
           CLOSE    CGD006
           OPEN I-O CGD006 LOG003

           INITIALIZE REG-CGD006
           MOVE CODIGO-CG01 TO CODIGO-CG06
           START CGD006 KEY IS NOT LESS CHAVE-CG06 INVALID KEY
               MOVE "10" TO ST-CGD006.
           PERFORM UNTIL ST-CGD006 = "10"
               READ CGD006 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD006
               NOT AT END
                   IF CODIGO-CG01 <> CODIGO-CG06
                      MOVE "10" TO ST-CGD006
                   ELSE
                      DELETE CGD006
                   END-IF
               END-READ
           END-PERFORM

           MOVE 1 TO CGP001-POSICAO
           MOVE SPACES TO CGP001-LINDET
           MOVE "LER-LIST3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL CGP001-LINDET = SPACES
               MOVE CGP001-CODIGO          TO CODIGO-CG06
               EVALUATE CGP001-LINDET(3:12)
                   WHEN "Cta.Corrente" MOVE 1     TO TIPO-DE-CONTA-CG06
                   WHEN "Cta.Poupança" MOVE 2     TO TIPO-DE-CONTA-CG06
                   WHEN OTHER          MOVE ZEROS TO TIPO-DE-CONTA-CG06
               END-EVALUATE

               MOVE CGP001-LINDET(16:4)    TO BANCO-CG06
               MOVE CGP001-LINDET(38:9)    TO AGENCIA-CG06
               MOVE CGP001-LINDET(48:15)   TO NR-CONTA-CG06
               MOVE CGP001-LINDET(63:12)   TO COMPLEMENTO-CONTA-CG06
               MOVE CGP001-LINDET(76:3)    TO USUARIO-CG06
               MOVE CGP001-LINDET(80:04)   TO DESC-USUARIO-CG06
               MOVE CGP001-LINDET(85:11)   TO CPF-TITULAR-CG06
               MOVE CGP001-LINDET(97:14)   TO CNPJ-TITULAR-CG06
               MOVE CGP001-LINDET(112:40)  TO TITULAR-CONTA-CG06
               MOVE CGP001-LINDET(153:1)   TO PREFERENCIAL-CG06
               WRITE REG-CGD006 INVALID KEY
                   REWRITE REG-CGD006 INVALID KEY
                       MOVE ST-CGD006 TO ST-CGD001
                       PERFORM ERRO-GRAVACAO
                   NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "A"         TO LOG3-OPERACAO
                       MOVE "CGD006"    TO LOG3-ARQUIVO
                       MOVE "CGP001"    TO LOG3-PROGRAMA
                       MOVE REG-CGD006  TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE
                       MOVE "00" TO ST-CGD006
                   END-REWRITE
               NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG3-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG3-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG3-HORAS
                   MOVE "I"         TO LOG3-OPERACAO
                   MOVE "CGD006"    TO LOG3-ARQUIVO
                   MOVE "CGP001"    TO LOG3-PROGRAMA
                   MOVE REG-CGD006  TO LOG3-REGISTRO
                   WRITE REG-LOG003
                   END-WRITE
                   MOVE "00" TO ST-CGD006
               END-WRITE
               IF ST-CGD006 <> "00"
                  MOVE ST-CGD006 TO ST-CGD001
                  PERFORM ERRO-GRAVACAO
               END-IF

               ADD 1 TO CGP001-POSICAO
               MOVE SPACES TO CGP001-LINDET
               MOVE "LER-LIST3" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE CGD006 LOG003
           OPEN INPUT CGD006.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CGP001-MENSAGEM-ERRO
           MOVE ST-CGD001       TO CGP001-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.

       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE CGP001-INICIAL(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE CGP001-INICIAL(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.

           MOVE INICIAL-PROCURADA TO NOME-CG01.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 NEXT RECORD AT END MOVE "10" TO ST-CGD001
              NOT AT END
                MOVE SPACES          TO CGP001-LINDET
                MOVE NOME-CG01(1: I) TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                   MOVE "10" TO ST-CGD001
                ELSE PERFORM INSERE-ITEM
                END-IF
              END-READ
           END-PERFORM.

       INSERE-ITEM SECTION.
           MOVE CODIGO-CG01       TO CGP001-LINDET(01:06)
           MOVE NOME-CG01         TO CGP001-LINDET(13:40)
           MOVE NOME-RED-CG01     TO CGP001-LINDET(60:20)
           MOVE COD-RED-CG01      TO CGP001-LINDET(81:03)
           IF GRAVA-W = 0
              MOVE "ATUALIZA-LIST" TO DS-PROCEDURE
           ELSE
              MOVE "INSERE-LIST" TO DS-PROCEDURE.

           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CGP001-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "CGP001" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-MALA SECTION.
           OPEN OUTPUT RELAT.
      *    MOVE ZEROS TO CONTADOR
      *    PERFORM UNTIL CONTADOR = 150
      *        ADD 1 TO CONTADOR
      *        MOVE CONTADOR TO DET-NUMERO
      *        MOVE "A"      TO DET-RESTO
      *        MOVE DET-AJUDA TO LINDET-REL
      *        WRITE REG-RELAT FROM LINDET-REL
      *    END-PERFORM


           MOVE 0  TO ETIQUETA
           MOVE 0  TO CONTADOR POSICAO

           IF CGP001-ORDEM-ESCOLHA = 1
              MOVE SPACES TO DET-ETIQUETA11
              MOVE SPACES TO DET-ETIQUETA12
              MOVE SPACES TO DET-ETIQUETA13
              MOVE SPACES TO DET-ETIQUETA14
              MOVE SPACES TO DET-ETIQUETA15
              MOVE 1 TO CGP001-POSICAO
              MOVE SPACES TO CGP001-LINNOME
              MOVE "LER-LIST2" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              PERFORM UNTIL CGP001-LINNOME = SPACES
                    MOVE CGP001-LINNOME(1:6) TO CODIGO-CG01
                    MOVE CODIGO-CG01 TO CODIGO-CG03
                    READ CGD003 NOT INVALID KEY
                      IF ENDERECO-RES-CG03 <> SPACES AND
                         BAIRRO-RES-CG03 <> SPACES AND
                         CIDADE-RES-CG03 <> 0 AND
                         CEP-RES-CG03 > 0
                         ADD 1 TO POSICAO
                         ADD 1 TO ETIQUETA
                         IF ETIQUETA = 1
                            MOVE CGP001-LINNOME(13:60) TO DET-ETIQUETA11
                            MOVE ENDERECO-RES-CG03
                                           TO DET-ETIQUETA12
                            MOVE BAIRRO-RES-CG03
                                           TO DET-ETIQUETA13
                            MOVE CIDADE-RES-CG03 TO CIDADE
                            READ CAD010 INVALID KEY
                               MOVE SPACES TO DET-ETIQUETA14
                            NOT INVALID KEY
                               MOVE SPACES TO DET-ETIQUETA14
                               STRING NOME-CID " - " UF-CID DELIMITED
                               BY "   " INTO DET-ETIQUETA14
                            END-READ
                            MOVE CEP-RES-CG03   TO DET-CEP
                            MOVE SPACES         TO DET-CXPOSTAL

                            MOVE CEP-CXPOSTAL TO DET-ETIQUETA15

                            MOVE SPACES TO DET-ETIQUETA21
                            MOVE SPACES TO DET-ETIQUETA22
                            MOVE SPACES TO DET-ETIQUETA23
                            MOVE SPACES TO DET-ETIQUETA24
                            MOVE SPACES TO DET-ETIQUETA25

                         ELSE
                            MOVE CGP001-LINNOME(13:60) TO DET-ETIQUETA21
                            MOVE ENDERECO-RES-CG03
                                           TO DET-ETIQUETA22
                            MOVE BAIRRO-RES-CG03
                                           TO DET-ETIQUETA23
                            MOVE CIDADE-RES-CG03 TO CIDADE
                            READ CAD010 INVALID KEY
                              MOVE SPACES TO DET-ETIQUETA24
                            NOT INVALID KEY
                               MOVE SPACES TO DET-ETIQUETA24
                               STRING NOME-CID " - " UF-CID DELIMITED
                               BY "   " INTO DET-ETIQUETA24
                            END-READ
                            MOVE CEP-RES-CG03    TO DET-CEP
                            MOVE SPACES         TO DET-CXPOSTAL

                            MOVE CEP-CXPOSTAL TO DET-ETIQUETA25

                            MOVE 0               TO ETIQUETA

                            WRITE REG-RELAT FROM DET-ETIQUETA1
                            WRITE REG-RELAT FROM DET-ETIQUETA2
                            WRITE REG-RELAT FROM DET-ETIQUETA3
                            WRITE REG-RELAT FROM DET-ETIQUETA4
                            WRITE REG-RELAT FROM DET-ETIQUETA5

                            MOVE SPACES TO REG-RELAT
                            MOVE SPACES TO LINDET-REL

                            WRITE REG-RELAT FROM LINDET-REL
                            WRITE REG-RELAT FROM LINDET-REL
                            WRITE REG-RELAT FROM LINDET-REL
                            IF POSICAO <> 2
                               WRITE REG-RELAT FROM LINDET-REL
                            END-IF
                         END-IF
                      END-IF
                    END-READ

                    ADD  1 TO CGP001-POSICAO
                    MOVE SPACES TO CGP001-LINNOME
                    MOVE "LER-LIST2" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
              END-PERFORM
              IF ETIQUETA = 1
                 WRITE REG-RELAT FROM DET-ETIQUETA1
                 WRITE REG-RELAT FROM DET-ETIQUETA2
                 WRITE REG-RELAT FROM DET-ETIQUETA3
                 WRITE REG-RELAT FROM DET-ETIQUETA4
                 WRITE REG-RELAT FROM DET-ETIQUETA5
              END-IF
           ELSE
              IF CGP001-ORDEM-IMPRESS = 1
                 MOVE ZEROS TO CODIGO-CG01
                 START CGD001 KEY IS NOT < CODIGO-CG01 INVALID KEY
                              MOVE "10" TO ST-CGD001
              ELSE MOVE SPACES TO NOME-CG01
                   START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                              MOVE "10" TO ST-CGD001
              END-IF
              PERFORM UNTIL ST-CGD001 = "10"
                 READ CGD001 NEXT RECORD AT END
                      MOVE "10" TO ST-CGD001
                 NOT AT END
                      PERFORM OPCAO-IMPRESSAO
                      IF IMPRIME-W = 0
                         CONTINUE
                      ELSE
                         IF CGP001-OPCAO = 3    OR
                           (CGP001-OPCAO = 1    AND
                            SITUACAO-CG01 <> 2) OR
                           (CGP001-OPCAO = 2    AND
                            SITUACAO-CG01 =  2)
                           MOVE CODIGO-CG01 TO CODIGO-CG05
                           READ CGD005 INVALID KEY
                               MOVE 0 TO FUNCAO-CG05
                           END-READ
                           IF CGP001-FUNCAO-FC = 0 OR
                              CGP001-FUNCAO-FC = FUNCAO-CG05
                              MOVE CODIGO-CG01 TO CODIGO-CG03
                              READ CGD003 NOT INVALID KEY
                                  IF ENDERECO-RES-CG03 <> SPACES AND
                                     BAIRRO-RES-CG03 <> SPACES AND
                                     CIDADE-RES-CG03 <> 0 AND
                                     CEP-RES-CG03 > 0
                                     ADD 1 TO POSICAO
                                     ADD 1 TO ETIQUETA
                                     IF ETIQUETA = 1
                                        MOVE NOME-CG01 TO DET-ETIQUETA11
                                        MOVE ENDERECO-RES-CG03
                                                       TO DET-ETIQUETA12
                                        MOVE BAIRRO-RES-CG03
                                                       TO DET-ETIQUETA13
                                        MOVE CIDADE-RES-CG03 TO CIDADE
                                        READ CAD010 INVALID KEY
                                           MOVE SPACES TO DET-ETIQUETA14
                                        NOT INVALID KEY
                                           MOVE SPACES TO DET-ETIQUETA14
                                           STRING NOME-CID " - " UF-CID
                                           DELIMITED BY "   " INTO
                                           DET-ETIQUETA14
                                        END-READ
                                        MOVE CEP-RES-CG03   TO DET-CEP
                                        MOVE SPACES         TO
                                                            DET-CXPOSTAL

                                        MOVE CEP-CXPOSTAL TO
                                                          DET-ETIQUETA15
                                     ELSE
                                        MOVE NOME-CG01 TO DET-ETIQUETA21
                                        MOVE ENDERECO-RES-CG03
                                                       TO DET-ETIQUETA22
                                        MOVE BAIRRO-RES-CG03
                                                       TO DET-ETIQUETA23
                                        MOVE CIDADE-RES-CG03 TO CIDADE
                                        READ CAD010 INVALID KEY
                                           MOVE SPACES TO DET-ETIQUETA24
                                        NOT INVALID KEY
                                           MOVE SPACES TO DET-ETIQUETA24
                                           STRING NOME-CID " - " UF-CID
                                           DELIMITED BY "   " INTO
                                           DET-ETIQUETA24
                                        END-READ
                                        MOVE CEP-RES-CG03   TO DET-CEP
                                        MOVE SPACES         TO
                                                            DET-CXPOSTAL

                                        MOVE CEP-CXPOSTAL   TO
                                                          DET-ETIQUETA25

                                        MOVE 0              TO ETIQUETA

                                        WRITE REG-RELAT FROM
                                                           DET-ETIQUETA1
                                        WRITE REG-RELAT FROM
                                                           DET-ETIQUETA2
                                        WRITE REG-RELAT FROM
                                                           DET-ETIQUETA3
                                        WRITE REG-RELAT FROM
                                                           DET-ETIQUETA4
                                        WRITE REG-RELAT FROM
                                                           DET-ETIQUETA5

                                        MOVE SPACES TO REG-RELAT
                                        MOVE SPACES TO LINDET-REL

                                        WRITE REG-RELAT FROM LINDET-REL
                                        WRITE REG-RELAT FROM LINDET-REL
                                        WRITE REG-RELAT FROM LINDET-REL
                                        IF POSICAO <> 2
                                         WRITE REG-RELAT FROM LINDET-REL
                                        END-IF
                                     END-IF
                                  END-IF
                              END-READ
                           END-IF
                         END-IF
                      END-IF
                 END-READ
             END-PERFORM
             IF ETIQUETA = 1
                 WRITE REG-RELAT FROM DET-ETIQUETA1
                 WRITE REG-RELAT FROM DET-ETIQUETA2
                 WRITE REG-RELAT FROM DET-ETIQUETA3
                 WRITE REG-RELAT FROM DET-ETIQUETA4
                 WRITE REG-RELAT FROM DET-ETIQUETA5
             END-IF.
           MOVE SPACES TO REG-RELAT.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL AFTER PAGE.
           CLOSE RELAT.

      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO
           MOVE 1 TO CGP001-POSICAO
           MOVE SPACES TO CGP001-LINNOME
           MOVE "LER-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL CGP001-LINNOME = SPACES
                 MOVE CGP001-LINNOME(1:6)    TO LINDET-REL(01: 08)
                 MOVE CGP001-LINNOME(13:40)  TO LINDET-REL(15: 30)
                 MOVE CGP001-LINDET(60: 20)  TO LINDET-REL(60: 20)
                 WRITE REG-RELAT FROM LINDET-REL
                 ADD 1 TO LIN
                 IF LIN > 56
                    PERFORM CABECALHO
                 END-IF

                 ADD  1 TO CGP001-POSICAO
                 MOVE SPACES TO CGP001-LINNOME
                 MOVE "LER-LIST2" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           COPY DESCONDENSA.


      *    MOVE SPACES TO REG-RELAT.
      *    MOVE SPACES TO LINDET-REL.
      *    WRITE REG-RELAT FROM LINDET-REL AFTER PAGE.
      *    CLOSE RELAT.
       OPCAO-IMPRESSAO SECTION.
           MOVE CGP001-CARACTERISTICA(1: 2) TO CARACTERISTICA-W
           MOVE ZEROS TO IMPRIME-W
           EVALUATE CARACTERISTICA-W
             WHEN "00" MOVE 1 TO IMPRIME-W
             WHEN "01" IF T-PESFIS-CG01 = 1 MOVE 1 TO IMPRIME-W
             WHEN "02" IF T-PESJUR-CG01 = 1 MOVE 1 TO IMPRIME-W
             WHEN "03" IF T-FUNC-CG01 = 1 MOVE 1 TO IMPRIME-W
             WHEN "04" IF T-REPRES-CG01 = 1 MOVE 1 TO IMPRIME-W
             WHEN "05" IF T-FOTOG-CG01 = 1 MOVE 1 TO IMPRIME-W
             WHEN "06" IF T-CINEG-CG01 = 1 MOVE 1 TO IMPRIME-W
             WHEN "07" IF T-VEND-CG01 = 1 MOVE 1 TO IMPRIME-W
             WHEN "08" IF T-IMPOSTO-CG01 = 1 MOVE 1 TO IMPRIME-W
             WHEN "09" IF T-INVESTIDOR-CG01 = 1 MOVE 1 TO IMPRIME-W
             WHEN "0X" IF OUTRO3-CG01 = 1 MOVE 1 TO IMPRIME-W
             WHEN "10" IF T-TERCEIRIZADO-CG01 =  1 MOVE 1 TO IMPRIME-W
             WHEN "11" IF T-FRANQUIA-CG01 = 1 MOVE 1 TO IMPRIME-W
           END-EVALUATE.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       ACHAR-CODIGO SECTION.
           MOVE ZEROS   TO ULT-CODIGO
           MOVE ALL "9" TO CODIGO-CG01
           START CGD001 KEY IS LESS THAN CODIGO-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001
           END-START
           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 PREVIOUS RECORD AT END
                   MOVE "10" TO ST-CGD001
              NOT AT END
                   MOVE CODIGO-CG01 TO ULT-CODIGO
                   MOVE "10"        TO ST-CGD001
              END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
           MOVE ULT-CODIGO TO CODIGO-CG01.
           READ CGD001 INVALID KEY CONTINUE.

           MOVE "SENHA10" TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY CONTINUE
             NOT INVALID KEY
                 MOVE "LIBERA-PARA-REPORT" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.

           REFRESH-OBJECT TABP1.

       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CGP001-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CGP001-CODIGO
           MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CGP001-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CGP001"            to logacess-programa
           move "FECHADO"           to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           CLOSE CGD001 CGD002 CGD003 CGD004 CGD005 CAD010
                 RED002 RED006 CAD004 CAD030 CGD006 CGD010.
           EXIT PROGRAM.
