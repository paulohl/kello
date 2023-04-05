       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGP010.
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
           COPY CAPX004.
           COPY CAPX030.
           COPY CGPX010.
           COPY CGPX011.
           COPY CGPX012.
           COPY CGPX013.
           COPY CGPX014.
           COPY CGPX015.
           COPY CGPX016.
           COPY CGPX017.
           COPY CGPX020.
           COPY CGPX030.
           COPY CAPX010.
           COPY IEPX011.
           COPY MTPX019.
           COPY LOGX001.
           COPY LOGX003.
           COPY LOGX004.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT CORREIO ASSIGN TO "\ARQUIVOS\CORREIO.TXT"
                          ORGANIZATION IS SEQUENTIAL
                          ACCESS MODE IS SEQUENTIAL
                          FILE STATUS IS ST-CORREIO.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW030.
       COPY CGPW010.
       COPY CGPW011.
       COPY CGPW012.
       COPY CGPW013.
       COPY CGPW014.
       COPY CGPW015.
       COPY CGPW016.
       COPY CGPW017.
       COPY CGPW020.
       COPY CGPW030.
       COPY CAPW010.
       COPY IEPW011.
       COPY MTPW019.
       COPY LOGW001.
       COPY LOGW003.
       COPY LOGW004.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).

       FD CORREIO
          LABEL RECORD IS OMITTED.
       01 REG-CORREIO             PIC X(449).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CGP010.CPB".
           COPY "CGP010.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".

       77  CONTADOR                  PIC 9(03).
       77  ETIQUETA                  PIC 9(01).
       77  QTDE-ETIQ                 PIC 9(02).
       77  POSICAO                   PIC 9(09).

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD012             PIC XX       VALUE SPACES.
           05  ST-CGD013             PIC XX       VALUE SPACES.
           05  ST-CGD014             PIC XX       VALUE SPACES.
           05  ST-CGD015             PIC XX       VALUE SPACES.
           05  ST-CGD016             PIC XX       VALUE SPACES.
           05  ST-CGD017             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-CGD030             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD020             PIC XX       VALUE SPACES.
           05  ST-CAD030             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-LOG003             PIC XX       VALUE SPACES.
           05  ST-LOG004             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-CORREIO            PIC XX       VALUE SPACES.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
           05  JA                    PIC X        VALUE SPACES.
           05  NUMERO                PIC X(6)     VALUE SPACES.
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
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  FOTO-IDENT-W          PIC X        VALUE SPACES.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  AUX-NOME              PIC X(60)    VALUE SPACES.
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.
      *    05  MASC-MESANO           PIC 99/9999.
      *    05  MASC-NUMERO           PIC 9999.9999.9999.9999.
           05  IND                   PIC 9(03)    VALUE ZEROS.
           05  AUX-QTDE              PIC 9(06)    VALUE ZEROS.
           05  AUX-ENDERECO          PIC X(50)    VALUE SPACES.
           05  AUX-NUMERO            PIC 9(06)    VALUE ZEROS.
           05  QTDE-NUMEROS          PIC 9(06)    VALUE ZEROS.
           05  AUX-CAMINHO           PIC X(256)   VALUE SPACES.
           05  AUX-CAMINHO2          PIC X(256)   VALUE SPACES.
           05  ACHEI                 PIC 9(01)    VALUE ZEROS.
           05  IND2                  PIC 9(03)    VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU                 PIC 9(04).
             10 WS-MES-CPU                 PIC 9(02).
             10 WS-DIA-CPU                 PIC 9(02).
          05 FILLER                        PIC X(13).

       01 AUX-CLIENTE.
          05 AUX-CONTRATO                  PIC 9(04).
          05 AUX-ALBUMCLI                  PIC 9(04).

       01 AUX-CLIENTE2.
          05 AUX-CONTRATO2                 PIC 9(04).
          05 AUX-ALBUMCLI2                 PIC 9(04).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       01 TAB-ENDERECO                 PIC X(45).
       01 TAB-ENDERECO-R REDEFINES TAB-ENDERECO OCCURS 45 TIMES.
          05 TAB-END                   PIC X(01).

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

       01  DET-ETIQUETA1.
           05 DET-ETIQUETA11       PIC X(38).
           05 FILLER               PIC X(04).
           05 DET-ETIQUETA21       PIC X(35).

       01  DET-ETIQUETA2.
           05 DET-ETIQUETA12       PIC X(38).
           05 FILLER               PIC X(04).
           05 DET-ETIQUETA22       PIC X(35).

       01  DET-ETIQUETA3.
           05 DET-ETIQUETA13       PIC X(38).
           05 FILLER               PIC X(04).
           05 DET-ETIQUETA23       PIC X(35).

       01  DET-ETIQUETA4.
           05 DET-ETIQUETA14       PIC X(38).
           05 FILLER               PIC X(04).
           05 DET-ETIQUETA24       PIC X(35).

       01  DET-ETIQUETA5.
           05 DET-ETIQUETA15       PIC X(38).
           05 FILLER               PIC X(04).
           05 DET-ETIQUETA25       PIC X(35).

       01  DET-ETIQUETA6.
           05 DET-ETIQUETA16       PIC X(38).
           05 FILLER               PIC X(04).
           05 DET-ETIQUETA26       PIC X(35).

       01  DET-ETIQUETA7.
           05 DET-ETIQUETA17       PIC X(38).
           05 FILLER               PIC X(04).
           05 DET-ETIQUETA27       PIC X(35).


       01 DET-ETIQ1.
           05 FILLER               PIC X(01).
           05 DET-ETI11            PIC X(40).
           05 FILLER               PIC X(02).
           05 DET-ETI12            PIC X(40).

       01 DET-ETIQ2.
           05 FILLER               PIC X(01).
           05 DET-ETI21            PIC X(40).
           05 FILLER               PIC X(02).
           05 DET-ETI22            PIC X(40).

       01 DET-ETIQ3.
           05 FILLER               PIC X(01).
           05 DET-ETI31            PIC X(40).
           05 FILLER               PIC X(02).
           05 DET-ETI32            PIC X(40).

       01 DET-ETIQ4.
           05 FILLER               PIC X(01).
           05 DET-ETI41            PIC X(40).
           05 FILLER               PIC X(02).
           05 DET-ETI42            PIC X(40).



       01  CEP-CXPOSTAL.
           05 DET-CEP              PIC 99999.999.
           05 FILLER               PIC X(02).
           05 FILLER               PIC X(10) VALUE "CX.POSTAL ".
           05 DET-CXPOSTAL         PIC ZZZZ9 BLANK WHEN ZEROS.

       01  KELLO1.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  ALBUM-KELLO     PIC 9999.9999 BLANK WHEN ZEROS.
           05  FILLER          PIC X(3)    VALUE SPACES.
           05  NOME-KELLO      PIC X(34)   VALUE SPACES.

       01  KELLO2.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  ENDERECO-KELLO  PIC X(26)   VALUE SPACES.
           05  FILLER          PIC X(01)   VALUE SPACES.
           05  BAIRRO-KELLO    PIC X(15)   VALUE SPACES.

       01  KELLO3.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  CEP-KELLO       PIC 99.999.999.
           05  FILLER          PIC X(02)   VALUE SPACES.
           05  CIDADE-KELLO    PIC X(31)   VALUE SPACES.


       01 DET-CORREIO-1.
          05 FILLER            PIC 9(001) VALUE 1.
          05 FILLER            PIC X(027) VALUE "SIGEP DESTINATARIO NACI
      -   "ONAL".
          05 FILLER            PIC X(419).
          05 FILLER            PIC X(02)
              VALUE X"0D0A".

       01 DET-CORREIO-2.
          05 FILLER            PIC 9(001) VALUE 2.
          05 DET-CNPJ-CPF      PIC ZZZ99999999999.
          05 DET-NOME          PIC X(050).
          05 DET-EMAIL         PIC X(050).
          05 DET-AOS-CUIDADOS  PIC X(050).
          05 DET-CONTATO       PIC X(042).
          05 DET-ALBUM         PIC 9(008).
          05 DET-CEP-CORREIO   PIC 9(008).
          05 DET-LOGRADOURO    PIC X(050).
          05 DET-NUMERO        PIC 9(006).
          05 DET-COMPLEMENTO   PIC X(030).
          05 DET-BAIRRO        PIC X(050).
          05 DET-CIDADE        PIC X(050).
          05 DET-TELEFONE      PIC 9(018).
          05 DET-CELULAR       PIC 9(010).
          05 DET-FAX           PIC 9(010).
          05 FILLER            PIC X(02)
              VALUE X"0D0A".

       01 DET-CORREIO-3.
          05 FILLER            PIC 9(001) VALUE 9.
          05 DET-QTDE          PIC 9(006).
          05 FILLER            PIC X(440).
          05 FILLER            PIC X(02)
              VALUE X"0D0A".

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01  linka-cpf.
           05  link-cpf             pic 9(11).
           05  link-cpf-r redefines link-cpf.
               10  link-cpf-num     pic 9(09).
               10  link-cpf-dig1    pic 9(01).
               10  link-cpf-dig2    pic 9(01).
           05  link-cpf-conf        pic 9(01).


       LINKAGE SECTION.

       01  STRING-1       PIC X(65).

           COPY "PARAMETR".
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD012.
           MOVE "CGD013" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD013.
           MOVE "CGD014" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD014.
           MOVE "CGD015" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD015.
           MOVE "CGD016" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD016.
           MOVE "CGD017" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD017.
           MOVE "CGD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "CGD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD030.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD030.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "LOG001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOG003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG003.
           MOVE "LOG004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG004.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS.
           MOVE ZEROS TO ERRO-W.
           OPEN I-O   CGD010 CGD011 CGD012 CGD013 CGD014 CGD016 CGD017
                      CGD030 LOG001 LOG003 LOG004 CGD015.
           OPEN INPUT CAD010 IED011 MTD019 CAD030 CGD020 CAD004
           IF ST-LOG001 = "35"
              CLOSE LOG001      OPEN OUTPUT LOG001
              CLOSE LOG001      OPEN I-O LOG001
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
           IF ST-CGD011 = "35"
              CLOSE CGD011      OPEN OUTPUT CGD011
              CLOSE CGD011      OPEN I-O CGD011
           END-IF.
           IF ST-CGD012 = "35"
              CLOSE CGD012      OPEN OUTPUT CGD012
              CLOSE CGD012      OPEN I-O CGD012
           END-IF.
           IF ST-CGD013 = "35"
              CLOSE CGD013      OPEN OUTPUT CGD013
              CLOSE CGD013      OPEN I-O CGD013
           END-IF.
           IF ST-CGD014 = "35"
              CLOSE CGD014      OPEN OUTPUT CGD014
              CLOSE CGD014      OPEN I-O CGD014
           END-IF.
           IF ST-CGD015 = "35"
              CLOSE CGD015      OPEN OUTPUT CGD015
              CLOSE CGD015      OPEN I-O CGD015
           END-IF.
           IF ST-CGD016 = "35"
              CLOSE CGD016      OPEN OUTPUT CGD016
              CLOSE CGD016      OPEN I-O CGD016
           END-IF.
           IF ST-CGD017 = "35"
              CLOSE CGD017      OPEN OUTPUT CGD017
              CLOSE CGD017      OPEN I-O CGD017
           END-IF.
           IF ST-CGD030 = "35"
              CLOSE CGD030      OPEN OUTPUT CGD030
              CLOSE CGD030      OPEN I-O CGD030
           END-IF.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
           IF ST-CGD013 <> "00"
              MOVE "ERRO ABERTURA CGD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD014 <> "00"
              MOVE "ERRO ABERTURA CGD014: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD014 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD015 <> "00"
              MOVE "ERRO ABERTURA CGD015: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD015 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD016 <> "00"
              MOVE "ERRO ABERTURA CGD016: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD016 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD017 <> "00"
              MOVE "ERRO ABERTURA CGD017: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD017 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD030 <> "00"
              MOVE "ERRO ABERTURA CGD030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD030 <> "00"
              MOVE "ERRO ABERTURA CAD030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      CGD010 CGD011 CGD012 CGD013 CGD014 CGD016
                      LOG001 LOG003 LOG004 CGD030 CGD015 CGD017

           OPEN INPUT CGD010 CGD011 CGD012 CGD013 CGD014 CGD030 CGD016
                      CGD015 CGD017


           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CGP010A"           to logacess-programa
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

           evaluate STRING-1(1:1)
               when 0 move "0-Contrato"  to gs-classificacao
               when 1 move "1-Comum"     to gs-classificacao
               when 9 move "9-Unificado" to gs-classificacao
           end-evaluate

           MOVE STRING-1(2:8) TO GS-CODIGO

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
                    MOVE "SENHA24"       TO PROGRAMA-CA004
                    MOVE COD-USUARIO-W   TO COD-USUARIO-CA004

                    READ CAD004 INVALID KEY
                        MOVE "DESABILITA-RECIBO" TO DS-PROCEDURE
                    NOT INVALID KEY
                        MOVE "HABILITA-RECIBO" TO DS-PROCEDURE
                    END-READ
                    PERFORM CALL-DIALOG-SYSTEM

                    MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-ACHAR-CODIGO-TRUE
                    IF GS-CLASSIFICACAO = SPACES
                       MOVE 0 TO GS-CLASSIFICACAO(1: 1)
                    END-IF
                    MOVE GS-CLASSIFICACAO(1: 1) TO CLASSIF-W
                    IF CLASSIF-W = 1 OR 9
                       PERFORM ACHAR-CODIGO
                    ELSE
                       MOVE ZEROS TO GS-CODIGO
                    END-IF
               WHEN GS-LOAD-FLG-TRUE
                    PERFORM CARREGAR-DADOS
                    MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI-RECORD
                    MOVE "SENHA24"       TO PROGRAMA-CA004
                    MOVE COD-USUARIO-W   TO COD-USUARIO-CA004

                    READ CAD004 INVALID KEY
                        MOVE "DESABILITA-RECIBO" TO DS-PROCEDURE
                    NOT INVALID KEY
                        MOVE "HABILITA-RECIBO" TO DS-PROCEDURE
                    END-READ
                    PERFORM CALL-DIALOG-SYSTEM
      *             PERFORM CARREGA-ULTIMOS
               WHEN GS-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
                    MOVE "SENHA24"       TO PROGRAMA-CA004
                    MOVE COD-USUARIO-W   TO COD-USUARIO-CA004

                    READ CAD004 INVALID KEY
                        MOVE "DESABILITA-RECIBO" TO DS-PROCEDURE
                    NOT INVALID KEY
                        MOVE "HABILITA-RECIBO" TO DS-PROCEDURE
                    END-READ
                    PERFORM CALL-DIALOG-SYSTEM
               WHEN GS-LE-CODIGO-TRUE
                    PERFORM LER-CODIGO
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LER-CIDADE
               WHEN GS-LE-REMETENTE-TRUE
                    PERFORM LER-REMETENTE
               WHEN GS-LER-CONTA-TRUE
                    PERFORM LER-CONTA
               WHEN GS-LER-CARTAO-TRUE
                    PERFORM LER-CARTAO
               WHEN GS-POPUP-REMETENTE-TRUE
                    PERFORM CHAMAR-POPUP-REMETENTE
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    MOVE "SENHA24"       TO PROGRAMA-CA004
                    MOVE COD-USUARIO-W   TO COD-USUARIO-CA004

                    READ CAD004 INVALID KEY
                        MOVE "DESABILITA-RECIBO" TO DS-PROCEDURE
                    NOT INVALID KEY
                        MOVE "HABILITA-RECIBO" TO DS-PROCEDURE
                    END-READ
                    PERFORM CALL-DIALOG-SYSTEM
               WHEN GS-MALA-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-MALA
                    END-IF
                    MOVE "SENHA24"       TO PROGRAMA-CA004
                    MOVE COD-USUARIO-W   TO COD-USUARIO-CA004

                    READ CAD004 INVALID KEY
                        MOVE "DESABILITA-RECIBO" TO DS-PROCEDURE
                    NOT INVALID KEY
                        MOVE "HABILITA-RECIBO" TO DS-PROCEDURE
                    END-READ
                    PERFORM CALL-DIALOG-SYSTEM
               WHEN GS-CORREIO-FLG-TRUE
                    PERFORM GERAR-CORREIO
                    MOVE "SENHA24"       TO PROGRAMA-CA004
                    MOVE COD-USUARIO-W   TO COD-USUARIO-CA004

                    READ CAD004 INVALID KEY
                        MOVE "DESABILITA-RECIBO" TO DS-PROCEDURE
                    NOT INVALID KEY
                        MOVE "HABILITA-RECIBO" TO DS-PROCEDURE
                    END-READ
                    PERFORM CALL-DIALOG-SYSTEM
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
               WHEN GS-POPUP-CODIGO-TRUE
                    PERFORM CHAMAR-POPUP-CODIGO
               WHEN GS-POPUP-CURSO-TRUE
                    PERFORM CHAMAR-POPUP-CURSO
               WHEN GS-POPUP-CIDADE-TRUE
                    PERFORM CHAMAR-POPUP-CIDADE
               WHEN GS-POPUP-BANCO-TRUE
                    PERFORM CHAMAR-POPUP-CONTA
               WHEN GS-POPUP-CARTAO-TRUE
                    PERFORM CHAMAR-POPUP-CARTAO
               WHEN GS-INSERIR-NOME-TRUE
                    PERFORM INSERIR-NOME
               WHEN GS-INSERIR-CARTAO-TRUE
                   PERFORM INSERIR-CARTAO
               WHEN GS-INSERIR-BANCO-TRUE
                   PERFORM INSERIR-BANCO
               WHEN GS-VISUALIZAR-CAMINHO1-TRUE
                   PERFORM VISUALIZAR-CAMINHO1
               WHEN GS-VISUALIZAR-CAMINHO2-TRUE
                   PERFORM VISUALIZAR-CAMINHO2
               WHEN GS-RETIRAR-ASPAS-TRUE
                   PERFORM RETIRAR-ASPAS
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VISUALIZAR-CAMINHO1 SECTION.
           MOVE SPACES TO GS-CAMINHO-VISUALIZADOR
           STRING "VIL " GS-ACP-CAMINHO1
                  INTO GS-CAMINHO-VISUALIZADOR
           MOVE "CHAMAR-VISUALIZACAO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       VISUALIZAR-CAMINHO2 SECTION.
           MOVE SPACES TO GS-CAMINHO-VISUALIZADOR
           STRING "VIL " GS-ACP-CAMINHO2
                  INTO GS-CAMINHO-VISUALIZADOR
           MOVE "CHAMAR-VISUALIZACAO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       RETIRAR-ASPAS SECTION.
           MOVE SPACES               TO AUX-CAMINHO2
           MOVE GS-DSDIR-FILENAME    TO AUX-CAMINHO
           MOVE 0                    TO IND2 ACHEI
           MOVE 1                    TO IND
           PERFORM UNTIL IND = 255 OR ACHEI = 2
               IF AUX-CAMINHO(IND:1) <> '"'
                  ADD  1                   TO IND2
                  MOVE AUX-CAMINHO(IND:1)  TO AUX-CAMINHO2(IND2:1)
               ELSE
                  ADD  1                   TO ACHEI
               END-IF
               ADD 1 TO IND
           END-PERFORM
           MOVE AUX-CAMINHO2 TO GS-DSDIR-FILENAME.


       LER-CONTA SECTION.
           MOVE GS-BANCO TO CODIGO-CAD30
           READ CAD030 INVALID KEY
               MOVE SPACE TO DESCRICAO-CAD30.
           MOVE DESCRICAO-CAD30 TO GS-NOME-BANCO.

       LER-CARTAO SECTION.
           MOVE GS-CODIGO-CARTAO TO CODIGO-CG20
           READ CGD020 INVALID KEY
               MOVE SPACE TO NOME-CG20.
           MOVE NOME-CG20 TO GS-NOME-CARTAO.

       CHAMAR-POPUP-CONTA SECTION.
           CALL "CAP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CAP020T"
           MOVE PASSAR-STRING-1(59: 4) TO GS-BANCO
           MOVE GS-BANCO TO CODIGO-CAD30
           READ CAD030 INVALID KEY
               MOVE SPACE TO DESCRICAO-CAD30.
           MOVE DESCRICAO-CAD30 TO GS-NOME-BANCO.

       CHAMAR-POPUP-CARTAO SECTION.
           CALL "CGP020T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP020T"
           MOVE PASSAR-STRING-1(1:30)  TO GS-NOME-CARTAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-CODIGO-CARTAO
           PERFORM LER-CARTAO.

       INSERIR-BANCO SECTION.
           IF GS-BANCO = ZEROS OR GS-AGENCIA = SPACES
              OR GS-CONTA-BANCO = SPACES OR GS-TIPO-CONTA NOT > 0
              MOVE "Dados Informados Incompletos" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              EVALUATE GS-TIPO-CONTA
                  WHEN 1 MOVE "Cta.Corrente" TO GS-LINDET(1:12)
                  WHEN 2 MOVE "Cta.Poupança" TO GS-LINDET(1:12)
              END-EVALUATE
              MOVE GS-BANCO           TO GS-LINDET(14:4)
              MOVE GS-NOME-BANCO      TO GS-LINDET(19:16)
              MOVE GS-AGENCIA         TO GS-LINDET(36:9)
              MOVE GS-CONTA-BANCO     TO GS-LINDET(46:15)
              IF GS-USUARIO NOT > ZEROS
                 MOVE USUARIO-W           TO GS-DESC-USUARIO
                 MOVE COD-USUARIO-W       TO GS-USUARIO
              END-IF
              MOVE GS-USUARIO         TO GS-LINDET(56:2)
              MOVE GS-DESC-USUARIO    TO GS-LINDET(59:04)
              MOVE GS-CPF-TITULAR     TO GS-LINDET(65:15)
              MOVE GS-TITULAR-CONTA   TO GS-LINDET(81:40)
              MOVE "INSERIR-CONTA"         TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       INSERIR-CARTAO SECTION.
           IF GS-CODIGO-CARTAO = ZEROS OR GS-NUMERO-CARTAO = 0
              OR GS-VALIDADE-CARTAO = ZEROS OR GS-VALIDACAO-CARTAO = 0
              OR GS-NOME-TIT-CARTAO = SPACES
              MOVE "Dados Informados Incompletos" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE SPACES              TO GS-LINDET
              MOVE GS-CODIGO-CARTAO    TO GS-LINDET(1:2)
              MOVE GS-NOME-CARTAO      TO GS-LINDET(4:30)
              MOVE GS-NUMERO-CARTAO    TO GS-LINDET(35:24)
      *       MASC-NUMERO
      *       MOVE MASC-NUMERO         TO GS-LINDET(35:24)
              MOVE GS-VALIDACAO-CARTAO TO GS-LINDET(59:15)
              MOVE GS-VALIDADE-CARTAO  TO GS-LINDET(78:7)
      *       MASC-MESANO
      *       MOVE MASC-MESANO         TO GS-LINDET(78:7)
              IF GS-USUARIO NOT > ZEROS
                 MOVE USUARIO-W           TO GS-DESC-USUARIO
                 MOVE COD-USUARIO-W       TO GS-USUARIO
              END-IF
              MOVE GS-USUARIO         TO GS-LINDET(89:2)
              MOVE GS-DESC-USUARIO    TO GS-LINDET(92:04)
              MOVE GS-NOME-TIT-CARTAO  TO GS-LINDET(97:30)
              MOVE "INSERIR-CARTAO"    TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.


       CHAMAR-POPUP-CODIGO SECTION.
           EVALUATE GS-OP-NOME
               WHEN 1 CALL   "MTP019T2" USING PARAMETROS-W
                                              PASSAR-STRING-1
                      CANCEL "MTP019T2"
                      MOVE PASSAR-STRING-1(1:30)  TO GS-DESC-NOME
                      STRING PASSAR-STRING-1(40:4)
                             PASSAR-STRING-1(45:4) INTO
                             GS-ACP-CODIGO
               WHEN OTHER
                      CALL "CGP010T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                      CANCEL "CGP010T"
                      MOVE PASSAR-STRING-1(33:8) TO GS-ACP-CODIGO
                      MOVE PASSAR-STRING-1(42:1)TO GS-CLASSIFICACAO
                      PERFORM LER-CODIGO.

       CHAMAR-POPUP-REMETENTE SECTION.
           CALL   "CGP030T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CGP030T".
           MOVE PASSAR-STRING-1(54: 2) TO GS-ACP-REMETENTE
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-REMETENTE.

       CHAMAR-POPUP-CURSO SECTION.
           CALL   "IEP011T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "IEP011T".
           MOVE PASSAR-STRING-1(43: 3) TO GS-CURSO
           MOVE PASSAR-STRING-1(1: 40) TO GS-NOME-CURSO.
       CHAMAR-POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP010T".
           EVALUATE GS-ORDEM-CIDADE
            WHEN 1 MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE1
            WHEN 2 MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE2
            WHEN 3 MOVE PASSAR-STRING-1(35:4)  TO GS-ACP-CIDADE-ENT
            WHEN 4 MOVE PASSAR-STRING-1(35:4)  TO GS-CIDADE-AVAL
            WHEN 5 MOVE PASSAR-STRING-1(35:4)  TO GS-CIDADE-REPUBLICA
            WHEN 9 MOVE PASSAR-STRING-1(35:4)  TO GS-CIDADE-PAIS
           END-EVALUATE.
           PERFORM LER-CIDADE.
       LER-CIDADE SECTION.
           EVALUATE GS-ORDEM-CIDADE
             WHEN 1 MOVE GS-CIDADE1          TO CIDADE
             WHEN 2 MOVE GS-CIDADE2          TO CIDADE
             WHEN 3 MOVE GS-ACP-CIDADE-ENT   TO CIDADE
             WHEN 4 MOVE GS-CIDADE-AVAL      TO CIDADE
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
             WHEN 3 MOVE NOME-COMPL-CID TO GS-DESC-CIDADE-ENT
                    MOVE UF-CID   TO GS-DESC-UF-ENT
             WHEN 4 MOVE NOME-COMPL-CID TO GS-DESC-CIDADE
             WHEN 5 MOVE NOME-COMPL-CID TO GS-DESC-CIDADE-REPUBLICA
                    MOVE UF-CID         TO GS-DESC-UF-REPUBLICA
             WHEN 9 MOVE NOME-CID TO GS-NOME-CIDADE-PAIS
                    MOVE UF-CID   TO GS-ESTADO-PAIS
           END-EVALUATE.
       LER-CODIGO SECTION.
           MOVE GS-ACP-CODIGO      TO AUX-CLIENTE
           IF AUX-ALBUMCLI > 0
              MOVE GS-ACP-CODIGO    TO CODIGO-CG10
              MOVE GS-CLASSIFICACAO(1:1) TO CLASSIF-CG10
              READ CGD010 NOT INVALID KEY
                  MOVE COMPRADOR-CG10    TO GS-DESC-NOME
              END-READ

              MOVE GS-ACP-CODIGO     TO ALBUMMT19

              READ MTD019 INVALID KEY
                   MOVE "***********"  TO GS-FORMANDO
              NOT INVALID KEY
                  MOVE NOME-FORM-MT19 TO GS-FORMANDO
              END-READ

              EVALUATE GS-OP-NOME
                  WHEN 1 MOVE NOME-FORM-MT19 TO GS-DESC-NOME
                  WHEN 2 MOVE COMPRADOR-CG10 TO GS-DESC-NOME
              END-EVALUATE
           ELSE
              MOVE "TODOS DO CONTRATO" TO GS-DESC-NOME.

       LER-CURSO SECTION.
           MOVE GS-CURSO TO CODIGO-IE11.
           READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11.
           MOVE NOME-IE11 TO GS-NOME-CURSO.

       LER-REMETENTE SECTION.
           MOVE GS-ACP-REMETENTE  TO CODIGO-CG30.
           READ CGD030 INVALID KEY MOVE SPACES TO NOME-CG30.
           MOVE NOME-CG30 TO GS-DESC-REMETENTE.

       VERIFICA-CODIGO SECTION.
           MOVE GS-CODIGO       TO CODIGO-CG10 CODIGO-CG11 CODIGO-CG12
                                   CODIGO-CG13 CODIGO-CG14.
           MOVE GS-CLASSIFICACAO(1: 1) TO CLASSIF-CG10 CLASSIF-CG11
                                          CLASSIF-CG13 CLASSIF-CG14.
           MOVE 1 TO GS-GRAVA-W.
           READ CGD010 INVALID KEY
               INITIALIZE REG-CGD010
               MOVE 0 TO GS-GRAVA-W.

           MOVE COMPRADOR-CG10       TO GS-NOME

           MOVE GS-CODIGO     TO ALBUMMT19

           READ MTD019 INVALID KEY
               MOVE "***********"  TO GS-FORMANDO
           NOT INVALID KEY
               MOVE NOME-FORM-MT19 TO GS-FORMANDO.

           READ CGD011 INVALID KEY
               INITIALIZE REG-CGD011
           END-READ
           PERFORM CARREGAR-DADOS-CGD011.

           READ CGD012 INVALID KEY
               INITIALIZE REG-CGD012
           END-READ
           PERFORM CARREGAR-DADOS-CGD012.

           READ CGD013 INVALID KEY
               INITIALIZE REG-CGD013
           END-READ
           PERFORM CARREGAR-DADOS-CGD013.

           READ CGD014 INVALID KEY
               INITIALIZE REG-CGD014
           END-READ

           MOVE COD-COMPL-CG10       TO CLIENTE-CG17
           READ CGD017 INVALID KEY
               INITIALIZE REG-CGD017
           END-READ

           MOVE CAMINHO1-CG17 TO GS-ACP-CAMINHO1
           MOVE CAMINHO2-CG17 TO GS-ACP-CAMINHO2


           PERFORM CARREGAR-DADOS-CGD014.

           PERFORM CARREGAR-DADOS-CGD015

           PERFORM CARREGAR-DADOS-CGD016.


           MOVE "SENHA24"       TO PROGRAMA-CA004
           MOVE COD-USUARIO-W   TO COD-USUARIO-CA004

           READ CAD004 INVALID KEY
                MOVE "DESABILITA-RECIBO" TO DS-PROCEDURE
           NOT INVALID KEY
                MOVE "HABILITA-RECIBO" TO DS-PROCEDURE.

           PERFORM CALL-DIALOG-SYSTEM.


       CARREGAR-DADOS SECTION.
           MOVE GS-CODIGO       TO CODIGO-CG10 CODIGO-CG11 CODIGO-CG12
                                   CODIGO-CG13 CODIGO-CG14.

           MOVE CODIGO-CG10     TO ALBUMMT19

           READ MTD019 INVALID KEY
               MOVE "***********"  TO GS-FORMANDO
           NOT INVALID KEY
               MOVE NOME-FORM-MT19 TO GS-FORMANDO.


           INITIALIZE GS-DATA-BLOCK
           EVALUATE CLASSIF-W
             WHEN 0 MOVE "0-Contrato      " TO GS-CLASSIFICACAO
             WHEN 1 MOVE "1-Comum         " TO GS-CLASSIFICACAO
             WHEN 9 MOVE "9-Unificado     " TO GS-CLASSIFICACAO
           END-EVALUATE.
           MOVE CLASSIF-W             TO CLASSIF-CG10 CLASSIF-CG11
                                         CLASSIF-CG13 CLASSIF-CG14.

           READ CGD010 INVALID KEY INITIALIZE REG-CGD010.
           MOVE CODIGO-CG10          TO GS-CODIGO
           MOVE COMPRADOR-CG10       TO GS-NOME

           READ CGD011 INVALID KEY
               INITIALIZE REG-CGD011.

           PERFORM CARREGAR-DADOS-CGD011.

           READ CGD012 INVALID KEY
               INITIALIZE REG-CGD012.

           PERFORM CARREGAR-DADOS-CGD012.

           READ CGD013 INVALID KEY
               INITIALIZE REG-CGD013.

           PERFORM CARREGAR-DADOS-CGD013.

           READ CGD014 INVALID KEY
               INITIALIZE REG-CGD014.

           MOVE COD-COMPL-CG10       TO CLIENTE-CG17
           READ CGD017 INVALID KEY
               INITIALIZE REG-CGD017
           END-READ

           MOVE CAMINHO1-CG17 TO GS-ACP-CAMINHO1
           MOVE CAMINHO2-CG17 TO GS-ACP-CAMINHO2


           PERFORM CARREGAR-DADOS-CGD014.

           PERFORM CARREGAR-DADOS-CGD015.

           PERFORM CARREGAR-DADOS-CGD016.

           MOVE "SENHA24"       TO PROGRAMA-CA004
           MOVE COD-USUARIO-W   TO COD-USUARIO-CA004

           READ CAD004 INVALID KEY
                MOVE "DESABILITA-RECIBO" TO DS-PROCEDURE
           NOT INVALID KEY
                MOVE "HABILITA-RECIBO" TO DS-PROCEDURE.

           PERFORM CALL-DIALOG-SYSTEM.


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
           EVALUATE TIPO-PESSOA-CG11
               WHEN "F"   MOVE "Física"    TO GS-TIPO-PESSOA
               WHEN "J"   MOVE "Jurídica"  TO GS-TIPO-PESSOA
               WHEN OTHER MOVE SPACES      TO GS-TIPO-PESSOA
           END-EVALUATE
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
           READ IED011 INVALID KEY
               MOVE SPACES TO NOME-IE11.

           MOVE NOME-IE11            TO GS-NOME-CURSO.
           MOVE TURMA-CG12           TO GS-TURMA.
           MOVE TAMANHO-BECA-CG12    TO GS-TAMANHO-BECA.

           EVALUATE FOTO-IDENTIFIC-CG12
             WHEN 0 MOVE "0-Não"     TO GS-FOTO-IDENTIFICACAO
             WHEN 1 MOVE "1-Sim"     TO GS-FOTO-IDENTIFICACAO
           END-EVALUATE.

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

       CARREGAR-DADOS-CGD013 SECTION.
           MOVE ENDERECO-CG13        TO GS-ACP-ENDERECO-ENT
           MOVE BAIRRO-CG13          TO GS-ACP-BAIRRO-ENT
           MOVE CEP-CG13             TO GS-ACP-CEP-ENT
           MOVE CIDADE-CG13          TO GS-ACP-CIDADE-ENT CIDADE
           READ CAD010 INVALID KEY
                MOVE ZEROS  TO DDD-CID
                MOVE SPACES TO NOME-COMPL-CID
                MOVE SPACES TO UF-CID.

           MOVE NOME-COMPL-CID       TO GS-DESC-CIDADE-ENT
           MOVE UF-CID               TO GS-DESC-UF-ENT.

       CARREGAR-DADOS-CGD014 SECTION.
           MOVE NOME-CG14            TO GS-NOME-AVAL
           MOVE ENDERECO-CG14        TO GS-ENDERECO-AVAL
           MOVE BAIRRO-CG14          TO GS-BAIRRO-AVAL
           MOVE CEP-CG14             TO GS-CEP-AVAL
           MOVE CIDADE-CG14          TO GS-CIDADE-AVAL CIDADE
           READ CAD010 INVALID KEY
                MOVE ZEROS  TO DDD-CID
                MOVE SPACES TO NOME-COMPL-CID
                MOVE SPACES TO UF-CID.

           STRING NOME-COMPL-CID " - " UF-CID DELIMITED BY "  " INTO
                                                          GS-DESC-CIDADE

           MOVE TELEFONE-CG14        TO GS-TELEFONE-AVAL
           MOVE COMP-TEL-AVAL-CG14   TO GS-COMP-TEL-AVAL

           MOVE CPF-CG14             TO GS-CPF-AVAL
           MOVE RG-CG14              TO GS-RG-AVAL.

       CARREGAR-DADOS-CGD015 SECTION.
           INITIALIZE REG-CGD015
           MOVE COD-COMPL-CG10       TO CODIGO-CG15
           START CGD015 KEY IS NOT LESS CHAVE-CG15 INVALID KEY
               MOVE "10" TO ST-CGD015.

           PERFORM UNTIL ST-CGD015 = "10"
               READ CGD015 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD015
               NOT AT END
                   IF COD-COMPL-CG10 <> CODIGO-CG15
                      MOVE "10" TO ST-CGD015
                   ELSE
                      MOVE SPACES                TO GS-LINDET
                      MOVE CODIGO-CARTAO-CG15    TO GS-LINDET(1:2)
                                                    CODIGO-CG20
                      READ CGD020 INVALID KEY
                           MOVE "--------"       TO NOME-CG20
                      END-READ
                      MOVE NOME-CG20             TO GS-LINDET(4:30)
                      MOVE NUMERO-CARTAO-CG15    TO GS-LINDET(35:24)
      *               MASC-NUMERO
      *               MOVE MASC-NUMERO           TO
                      MOVE VALIDACAO-NUMERO-CG15 TO GS-LINDET(59:15)
                      MOVE DATA-LIMITE-CG15      TO GS-LINDET(78:7)
      *               MASC-MESANO
      *               MOVE MASC-MESANO           TO
                      MOVE USUARIO-CG15          TO GS-LINDET(89:2)
                      MOVE DESC-USUARIO-CG15     TO GS-LINDET(92:04)
                      MOVE TITULAR-CONTA-CG15    TO GS-LINDET(97:30)
                      MOVE "INSERIR-CARTAO"   TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.

       CARREGAR-DADOS-CGD016 SECTION.
           INITIALIZE REG-CGD016
           MOVE COD-COMPL-CG10       TO CODIGO-CG16
           START CGD016 KEY IS NOT LESS CHAVE-CG16 INVALID KEY
               MOVE "10" TO ST-CGD016.
           PERFORM UNTIL ST-CGD016 = "10"
               READ CGD016 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD016
               NOT AT END
                   IF COD-COMPL-CG10 <> CODIGO-CG16
                      MOVE "10" TO ST-CGD016
                   ELSE
                      EVALUATE TIPO-DE-CONTA-CG16
                          WHEN 1     MOVE "Cta.Corrente" TO
                                     GS-LINDET(1:12)
                          WHEN 2     MOVE "Cta.Poupança" TO
                                     GS-LINDET(1:12)
                          WHEN OTHER MOVE "Nao Inform. " TO
                                     GS-LINDET(1:12)
                      END-EVALUATE

                      MOVE BANCO-CG16         TO CODIGO-CAD30
                                                 GS-LINDET(14:4)
                      READ CAD030 INVALID KEY
                           MOVE SPACES        TO DESCRICAO-CAD30
                      END-READ
                      MOVE DESCRICAO-CAD30    TO GS-LINDET(19:16)
                      MOVE AGENCIA-CG16       TO GS-LINDET(36:9)
                      MOVE NR-CONTA-CG16      TO GS-LINDET(46:15)
                      MOVE USUARIO-CG16       TO GS-LINDET(56:2)
                      MOVE DESC-USUARIO-CG16  TO GS-LINDET(59:4)
                      MOVE TITULAR-CONTA-CG16 TO GS-LINDET(81:40)
                      MOVE CPF-TITULAR-CG16   TO GS-LINDET(65:15)
                      MOVE "INSERIR-CONTA"    TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.


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
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       EXCLUI-RECORD SECTION.
           CLOSE    CGD010
           OPEN I-O CGD010 LOG001

           DELETE CGD010 NOT INVALID KEY
               MOVE USUARIO-W   TO LOG1-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG1-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG1-HORAS
               MOVE "E"         TO LOG1-OPERACAO
               MOVE "CGD010"    TO LOG1-ARQUIVO
               MOVE "CGP010"    TO LOG1-PROGRAMA
               MOVE REG-CGD010  TO LOG1-REGISTRO
               WRITE REG-LOG001
               END-WRITE.

           CLOSE      CGD010 LOG001
           OPEN INPUT CGD010

           PERFORM LIMPAR-DADOS.

       SALVAR-DADOS SECTION.
           CLOSE    CGD010 CGD017
           OPEN I-O CGD010 CGD017 LOG001

           MOVE GS-CLASSIFICACAO(1: 1) TO  CLASSIF-CG10 CLASSIF-CG11
           MOVE GS-CODIGO        TO  CODIGO-CG10 CODIGO-CG11
           MOVE GS-NOME          TO  COMPRADOR-CG10.

           IF GS-GRAVA-W = 0
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
                   MOVE "CGP010"    TO LOG1-PROGRAMA
                   MOVE REG-CGD010  TO LOG1-REGISTRO
                   WRITE REG-LOG001
                   END-WRITE
              END-WRITE
           ELSE
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
                  MOVE "CGP010"    TO LOG1-PROGRAMA
                  MOVE REG-CGD010  TO LOG1-REGISTRO
                  WRITE REG-LOG001
                  END-WRITE
                  IF CLASSIF-CG10 = 1
                     SUBTRACT 1 FROM ULT-CODIGO
                  END-IF
              END-REWRITE
           END-IF.

           INITIALIZE REG-CGD017
           MOVE COD-COMPL-CG10 TO CLIENTE-CG17
           START CGD017 KEY IS NOT LESS CHAVE-CG17 INVALID KEY
               MOVE "10" TO ST-CGD017.
           PERFORM UNTIL ST-CGD017 = "10"
               READ CGD017 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD017
               NOT AT END
                   IF COD-COMPL-CG10 <> CLIENTE-CG17
                      MOVE "10" TO ST-CGD017
                   ELSE
                      DELETE CGD017
                   END-IF
               END-READ
           END-PERFORM

           MOVE COD-COMPL-CG10    TO CLIENTE-CG17

           MOVE GS-ACP-CAMINHO1   TO CAMINHO1-CG17
           MOVE GS-ACP-CAMINHO2   TO CAMINHO2-CG17
           MOVE USUARIO-W         TO DESC-USUARIO-CG17
           MOVE COD-USUARIO-W     TO USUARIO-CG17

           WRITE REG-CGD017

           CLOSE      CGD010 CGD017 LOG001
           OPEN INPUT CGD010 CGD017

           IF GS-GRAVA-CGP011 = 1
                 PERFORM SALVAR-DADOS-CGD011.

           IF GS-GRAVA-CGP012 = 1
                 PERFORM SALVAR-DADOS-CGD012.

           IF GS-GRAVA-CGP013 = 1
                 PERFORM SALVAR-DADOS-CGD013.

           IF GS-GRAVA-CGP014 = 1
                 PERFORM SALVAR-DADOS-CGD014.

           IF GS-GRAVA-CGP015 = 1
              PERFORM SALVAR-DADOS-CARTOES.

           IF GS-GRAVA-CGP016 = 1
              PERFORM SALVAR-DADOS-BANCOS.

       SALVAR-DADOS-CARTOES SECTION.
           CLOSE    CGD015
           OPEN I-O CGD015 LOG003

           INITIALIZE REG-CGD015
           MOVE COD-COMPL-CG10 TO CODIGO-CG15
           START CGD015 KEY IS NOT LESS CHAVE-CG15 INVALID KEY
               MOVE "10" TO ST-CGD015.
           PERFORM UNTIL ST-CGD015 = "10"
               READ CGD015 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD015
               NOT AT END
                   IF COD-COMPL-CG10 <> CODIGO-CG15
                      MOVE "10" TO ST-CGD015
                   ELSE
                      DELETE CGD015
                   END-IF
               END-READ
           END-PERFORM

           MOVE 1 TO GS-POSICAO
           MOVE SPACES TO GS-LINNOME
           MOVE "LER-CARTAO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINNOME = SPACES
               MOVE COD-COMPL-CG10     TO CODIGO-CG15
               MOVE GS-LINNOME(1:2)     TO CODIGO-CARTAO-CG15
               MOVE GS-LINNOME(35:16)   TO GS-NUMERO-CARTAO
               MOVE GS-NUMERO-CARTAO   TO NUMERO-CARTAO-CG15
               MOVE GS-LINNOME(59:04)   TO VALIDACAO-NUMERO-CG15
               MOVE GS-LINNOME(78:6)    TO GS-VALIDADE-CARTAO
               MOVE GS-VALIDADE-CARTAO TO DATA-LIMITE-CG15
               MOVE GS-LINNOME(89:2)    TO USUARIO-CG15
               MOVE GS-LINNOME(92:04)   TO DESC-USUARIO-CG15
               MOVE GS-LINNOME(97:30)   TO TITULAR-CONTA-CG15
               WRITE REG-CGD015 INVALID KEY
                   REWRITE REG-CGD015 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "A"         TO LOG3-OPERACAO
                       MOVE "CGD015"    TO LOG3-ARQUIVO
                       MOVE "CGP010"    TO LOG3-PROGRAMA
                       MOVE REG-CGD015  TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE
                   END-REWRITE
               NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG3-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG3-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG3-HORAS
                   MOVE "I"         TO LOG3-OPERACAO
                   MOVE "CGD015"    TO LOG3-ARQUIVO
                   MOVE "CGP010"    TO LOG3-PROGRAMA
                   MOVE REG-CGD015  TO LOG3-REGISTRO
                   WRITE REG-LOG003
                   END-WRITE
               END-WRITE

               ADD 1 TO GS-POSICAO
               MOVE SPACES TO GS-LINNOME
               MOVE "LER-CARTAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE CGD015 LOG003
           OPEN INPUT CGD015.

       SALVAR-DADOS-BANCOS SECTION.
           CLOSE    CGD016
           OPEN I-O CGD016 LOG003

           INITIALIZE REG-CGD016
           MOVE COD-COMPL-CG10 TO CODIGO-CG16
           START CGD016 KEY IS NOT LESS CHAVE-CG16 INVALID KEY
               MOVE "10" TO ST-CGD016.
           PERFORM UNTIL ST-CGD016 = "10"
               READ CGD016 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD016
               NOT AT END
                   IF COD-COMPL-CG10 <> CODIGO-CG16
                      MOVE "10" TO ST-CGD016
                   ELSE
                      DELETE CGD016
                   END-IF
               END-READ
           END-PERFORM

           MOVE 1 TO GS-POSICAO
           MOVE SPACES TO GS-LINNOME
           MOVE "LER-CONTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINNOME = SPACES
               MOVE COD-COMPL-CG10     TO CODIGO-CG16
               EVALUATE GS-LINNOME(1:12)
                   WHEN "Cta.Corrente" MOVE 1     TO TIPO-DE-CONTA-CG16
                   WHEN "Cta.Poupança" MOVE 2     TO TIPO-DE-CONTA-CG16
                   WHEN OTHER          MOVE ZEROS TO TIPO-DE-CONTA-CG16
               END-EVALUATE
               MOVE GS-LINNOME(14:4)    TO BANCO-CG16
               MOVE GS-LINNOME(36:9)    TO AGENCIA-CG16
               MOVE GS-LINNOME(46:15)   TO NR-CONTA-CG16
               MOVE GS-LINNOME(56:2)    TO USUARIO-CG16
               MOVE GS-LINNOME(59:04)   TO DESC-USUARIO-CG16
               MOVE GS-LINNOME(65:11)   TO CPF-TITULAR-CG16
               MOVE GS-LINNOME(81:40)   TO TITULAR-CONTA-CG16
               WRITE REG-CGD016 INVALID KEY
                   REWRITE REG-CGD016 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "A"         TO LOG3-OPERACAO
                       MOVE "CGD016"    TO LOG3-ARQUIVO
                       MOVE "CGP010"    TO LOG3-PROGRAMA
                       MOVE REG-CGD016  TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE
                   END-REWRITE
               NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG3-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG3-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG3-HORAS
                   MOVE "I"         TO LOG3-OPERACAO
                   MOVE "CGD016"    TO LOG3-ARQUIVO
                   MOVE "CGP010"    TO LOG3-PROGRAMA
                   MOVE REG-CGD016  TO LOG3-REGISTRO
                   WRITE REG-LOG003
                   END-WRITE
               END-WRITE

               ADD 1 TO GS-POSICAO
               MOVE SPACES TO GS-LINNOME
               MOVE "LER-CONTA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE CGD016 LOG003
           OPEN INPUT CGD016.


       SALVAR-DADOS-CGD011 SECTION.
           CLOSE    CGD011
           OPEN I-O CGD011 LOG004

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
           MOVE GS-TIPO-PESSOA(1:1)     TO TIPO-PESSOA-CG11


           WRITE REG-CGD011 INVALID KEY
               REWRITE REG-CGD011 INVALID KEY
                   MOVE ST-CGD011 TO ST-CGD010
                   PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG4-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG4-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG4-HORAS
                   MOVE "A"         TO LOG4-OPERACAO
                   MOVE "CGD011"    TO LOG4-ARQUIVO
                   MOVE "CGP010"    TO LOG4-PROGRAMA
                   MOVE REG-CGD011  TO LOG4-REGISTRO
                   WRITE REG-LOG004
                   END-WRITE
               END-REWRITE
           NOT INVALID KEY
               MOVE USUARIO-W   TO LOG4-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG4-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG4-HORAS
               MOVE "I"         TO LOG4-OPERACAO
               MOVE "CGD011"    TO LOG4-ARQUIVO
               MOVE "CGP010"    TO LOG4-PROGRAMA
               MOVE REG-CGD011  TO LOG4-REGISTRO
               WRITE REG-LOG001
               END-WRITE.

           CLOSE CGD011 LOG004
           OPEN INPUT CGD011.

       SALVAR-DADOS-CGD012 SECTION.
           CLOSE    CGD012
           OPEN I-O CGD012 LOG001

           MOVE GS-CODIGO           TO CODIGO-CG12.
           MOVE GS-CURSO            TO CURSO-CG12.
           MOVE GS-TURMA            TO TURMA-CG12.
           MOVE GS-TAMANHO-BECA     TO TAMANHO-BECA-CG12.
           MOVE GS-FOTO-IDENTIFICACAO(1: 1) TO FOTO-IDENT-W
           IF FOTO-IDENT-W = " " CONTINUE
           ELSE MOVE GS-FOTO-IDENTIFICACAO(1: 1) TO FOTO-IDENTIFIC-CG12.
           MOVE GS-CARGO-COMISSAO(1: 1)     TO CARGO-COMISSAO-CG12.
           WRITE REG-CGD012 INVALID KEY
                 REWRITE REG-CGD012 INVALID KEY
                   MOVE ST-CGD012 TO ST-CGD010
                   PERFORM ERRO-GRAVACAO
                 NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG1-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG1-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG1-HORAS
                   MOVE "A"         TO LOG1-OPERACAO
                   MOVE "CGD012"    TO LOG1-ARQUIVO
                   MOVE "CGP010"    TO LOG1-PROGRAMA
                   MOVE REG-CGD012  TO LOG1-REGISTRO
                   WRITE REG-LOG001
                   END-WRITE
                 END-REWRITE
           NOT INVALID KEY
               MOVE USUARIO-W   TO LOG1-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG1-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG1-HORAS
               MOVE "I"         TO LOG1-OPERACAO
               MOVE "CGD012"    TO LOG1-ARQUIVO
               MOVE "CGP010"    TO LOG1-PROGRAMA
               MOVE REG-CGD012  TO LOG1-REGISTRO
               WRITE REG-LOG001
               END-WRITE.

           CLOSE      CGD012 LOG001
           OPEN INPUT CGD012.

       SALVAR-DADOS-CGD013 SECTION.
           CLOSE    CGD013
           OPEN I-O CGD013 LOG001

           MOVE GS-CLASSIFICACAO(1: 1) TO  CLASSIF-CG13
           MOVE GS-CODIGO           TO CODIGO-CG13
           MOVE GS-ACP-ENDERECO-ENT TO ENDERECO-CG13
           MOVE GS-ACP-BAIRRO-ENT   TO BAIRRO-CG13
           MOVE GS-ACP-CEP-ENT      TO CEP-CG13
           MOVE GS-ACP-CIDADE-ENT   TO CIDADE-CG13

           WRITE REG-CGD013 INVALID KEY
                 REWRITE REG-CGD013 INVALID KEY
                   MOVE ST-CGD013 TO ST-CGD010
                   PERFORM ERRO-GRAVACAO
                 NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG1-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG1-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG1-HORAS
                   MOVE "A"         TO LOG1-OPERACAO
                   MOVE "CGD013"    TO LOG1-ARQUIVO
                   MOVE "CGP010"    TO LOG1-PROGRAMA
                   MOVE REG-CGD013  TO LOG1-REGISTRO
                   WRITE REG-LOG001
                   END-WRITE
                 END-REWRITE
           NOT INVALID KEY
               MOVE USUARIO-W   TO LOG1-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG1-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG1-HORAS
               MOVE "I"         TO LOG1-OPERACAO
               MOVE "CGD013"    TO LOG1-ARQUIVO
               MOVE "CGP010"    TO LOG1-PROGRAMA
               MOVE REG-CGD013  TO LOG1-REGISTRO
               WRITE REG-LOG001
               END-WRITE.

           CLOSE      CGD013 LOG001
           OPEN INPUT CGD013.

       SALVAR-DADOS-CGD014 SECTION.
           CLOSE    CGD014
           OPEN I-O CGD014 LOG003

           MOVE GS-CLASSIFICACAO(1: 1) TO  CLASSIF-CG14
           MOVE GS-CODIGO           TO CODIGO-CG14
           MOVE GS-NOME-AVAL        TO NOME-CG14
           MOVE GS-ENDERECO-AVAL    TO ENDERECO-CG14
           MOVE GS-BAIRRO-AVAL      TO BAIRRO-CG14
           MOVE GS-CEP-AVAL         TO CEP-CG14
           MOVE GS-CIDADE-AVAL      TO CIDADE-CG14
           MOVE GS-TELEFONE-AVAL    TO TELEFONE-CG14
           MOVE GS-COMP-TEL-AVAL    TO COMP-TEL-AVAL-CG14
           MOVE GS-CPF-AVAL         TO CPF-CG14
           MOVE GS-RG-AVAL          TO RG-CG14

           WRITE REG-CGD014 INVALID KEY
                 REWRITE REG-CGD014 INVALID KEY
                   MOVE ST-CGD014 TO ST-CGD010
                   PERFORM ERRO-GRAVACAO
                 NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG3-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG3-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG3-HORAS
                   MOVE "A"         TO LOG3-OPERACAO
                   MOVE "CGD014"    TO LOG3-ARQUIVO
                   MOVE "CGP010"    TO LOG3-PROGRAMA
                   MOVE REG-CGD014  TO LOG3-REGISTRO
                   WRITE REG-LOG003
                   END-WRITE
                 END-REWRITE
           NOT INVALID KEY
               MOVE USUARIO-W   TO LOG3-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG3-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG3-HORAS
               MOVE "I"         TO LOG3-OPERACAO
               MOVE "CGD014"    TO LOG3-ARQUIVO
               MOVE "CGP010"    TO LOG3-PROGRAMA
               MOVE REG-CGD014  TO LOG3-REGISTRO
               WRITE REG-LOG003
               END-WRITE.

           CLOSE      CGD014 LOG003
           OPEN INPUT CGD014.

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
           ELSE
              MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CGP010A"   TO DS-SET-NAME
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
                   IF LIN > 56 PERFORM CABECALHO
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
      *                                   CLASSIF-W.
           MOVE ZEROS                  TO CODIGO-CG10 ULT-CODIGO.
           START CGD010 KEY IS NOT < COD-COMPL-CG10 INVALID KEY
                 MOVE "10" TO ST-CGD010
           END-START
           PERFORM UNTIL ST-CGD010 = "10"
                 READ CGD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CGD010
                 NOT AT END
                      IF CLASSIF-CG10 NOT = CLASSIF-W
                         MOVE "10"        TO ST-CGD010
                      ELSE
                         MOVE CODIGO-CG10 TO ULT-CODIGO
                      END-IF
                 END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE ULT-CODIGO TO GS-CODIGO.

       INSERIR-NOME SECTION.
           MOVE GS-ACP-CODIGO      TO AUX-CLIENTE
           IF AUX-ALBUMCLI = 0
              INITIALIZE REG-CGD010
              MOVE GS-CLASSIFICACAO(1:1) TO CLASSIF-CG10
              MOVE GS-ACP-CODIGO         TO CODIGO-CG10
              START CGD010 KEY IS NOT LESS COD-COMPL-CG10 INVALID KEY
                   MOVE "10" TO ST-CGD010
              END-START
              PERFORM UNTIL ST-CGD010 = "10"
                   READ CGD010 NEXT RECORD AT END
                        MOVE "10" TO ST-CGD010
                   NOT AT END
                        MOVE CODIGO-CG10 TO AUX-CLIENTE2
                        IF AUX-CONTRATO <> AUX-CONTRATO2
                           MOVE "10" TO ST-CGD010
                        ELSE
                           IF AUX-ALBUMCLI2 > 0
                              MOVE COMPRADOR-CG10    TO GS-DESC-NOME
                              MOVE CODIGO-CG10       TO ALBUMMT19
                              READ MTD019 INVALID KEY
                                   MOVE "***********" TO GS-FORMANDO
                              NOT INVALID KEY
                                   MOVE NOME-FORM-MT19 TO GS-FORMANDO
                              END-READ
                              EVALUATE GS-OP-NOME
                                   WHEN 1 MOVE NOME-FORM-MT19 TO
                                                            GS-DESC-NOME
                                   WHEN 2 MOVE COMPRADOR-CG10 TO
                                                            GS-DESC-NOME
                              END-EVALUATE
                             MOVE CODIGO-CG10      TO GS-LINNOME(01: 08)
                             MOVE GS-DESC-NOME     TO GS-LINNOME(13: 60)
                             MOVE GS-CLASSIFICACAO TO GS-LINNOME(73:1)
                             MOVE GS-OP-ENDERECO   TO GS-LINNOME(75:1)
                             MOVE "INSERE-LIST2" TO DS-PROCEDURE
                             PERFORM CALL-DIALOG-SYSTEM
                           END-IF
                        END-IF
                   END-READ
              END-PERFORM
           ELSE
              IF GS-ACP-CODIGO > 0 AND GS-DESC-NOME <> SPACES
                 MOVE GS-ACP-CODIGO        TO GS-LINNOME(01: 08)
                 MOVE GS-DESC-NOME         TO GS-LINNOME(13: 60)
                 MOVE GS-CLASSIFICACAO     TO GS-LINNOME(73:1)
                 MOVE GS-OP-ENDERECO       TO GS-LINNOME(75:1)
                 MOVE "INSERE-LIST2" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.

       GERAR-CORREIO SECTION.
           OPEN OUTPUT CORREIO

           MOVE ZEROS TO AUX-QTDE

           MOVE DET-CORREIO-1 TO REG-CORREIO
           WRITE REG-CORREIO

           EVALUATE GS-ORDEM-ESCOLHA
            WHEN 1 MOVE 1 TO GS-POSICAO
                   MOVE SPACES TO GS-LINNOME
                   MOVE "LER-LIST2" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   PERFORM UNTIL GS-LINNOME = SPACES
                       ADD 1 TO AUX-QTDE
                       INITIALIZE DET-CNPJ-CPF
                                  DET-NOME
                                  DET-EMAIL
                                  DET-AOS-CUIDADOS
                                  DET-CONTATO
                                  DET-CEP-CORREIO
                                  DET-LOGRADOURO
                                  DET-NUMERO
                                  DET-COMPLEMENTO
                                  DET-BAIRRO
                                  DET-CIDADE
                                  DET-TELEFONE
                                  DET-CELULAR
                                  DET-FAX
                                  DET-ALBUM

                       MOVE GS-LINNOME(1:8)  TO CODIGO-CG11
                       MOVE GS-LINNOME(73:1) TO CLASSIF-CG11
                       READ CGD011 INVALID KEY
                           INITIALIZE CPF1-CG11
                       END-READ
                       MOVE CODIGO-CG11       TO DET-ALBUM
                       MOVE   CPF1-CG11       TO LINK-CPF
                       CALL   "DIGCPF"  USING LINKA-CPF
                       CANCEL "DIGCPF"
                       IF LINK-CPF-CONF       =       1
                          STRING "Cliente " COD-COMPL-CG11 " está com o
      -                   "CPF " CPF1-CG11 " inválido" INTO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                       ELSE
                          MOVE CPF1-CG11         TO DET-CNPJ-CPF
                          MOVE GS-LINNOME(13:60) TO DET-NOME
                          MOVE GS-LINNOME(75:1)  TO GS-OP-ENDERECO

                          EVALUATE GS-OP-ENDERECO
                              WHEN 1 MOVE CEP1-CG11   TO DET-CEP-CORREIO
                                     MOVE BAIRRO1-CG11 TO DET-BAIRRO
                                     MOVE CIDADE1-CG11 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE COMPLEMENTO1-CG11 TO
                                          DET-COMPLEMENTO
                                     MOVE ENDERECO1-CG11 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                              WHEN 2 MOVE CEP2-CG11   TO DET-CEP-CORREIO
                                     MOVE BAIRRO2-CG11 TO DET-BAIRRO
                                     MOVE CIDADE2-CG11 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE COMPLEMENTO2-CG11 TO
                                          DET-COMPLEMENTO
                                     MOVE ENDERECO2-CG11 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                              WHEN 3 MOVE CODIGO-CG11    TO CODIGO-CG13
                                     READ CGD013 INVALID KEY
                                          INITIALIZE REG-CGD013
                                     END-READ
                                     MOVE CEP-CG13   TO DET-CEP-CORREIO
                                     MOVE BAIRRO-CG13 TO DET-BAIRRO
                                     MOVE CIDADE-CG13 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE SPACES     TO DET-COMPLEMENTO
                                     MOVE ENDERECO-CG13 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                          END-EVALUATE

                          MOVE DET-CORREIO-2 TO REG-CORREIO
                          WRITE REG-CORREIO
                       END-IF

                       ADD  1 TO GS-POSICAO
                       MOVE SPACES TO GS-LINNOME
                       MOVE "LER-LIST2" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                   END-PERFORM
            WHEN OTHER EVALUATE GS-ORDEM-IMPRESS
                           WHEN 1 EVALUATE GS-OP-NOME
                                      WHEN 1 PERFORM CODIGO-MTD019-COR
                                      WHEN 2 PERFORM CODIGO-CGD010-COR
                                  END-EVALUATE
                           WHEN 2 EVALUATE GS-OP-NOME
                                      WHEN 1 PERFORM NOME-MTD019-COR
                                      WHEN 2 PERFORM NOME-CGD010-COR
                                  END-EVALUATE
                       END-EVALUATE
           END-EVALUATE.

           MOVE AUX-QTDE      TO DET-QTDE

           MOVE DET-CORREIO-3 TO REG-CORREIO
           WRITE REG-CORREIO

           CLOSE CORREIO.

       SEPARAR-ENDERECO SECTION.
           MOVE SPACES TO AUX-ENDERECO
           MOVE 0 TO QTDE-NUMEROS
           MOVE 1 TO IND2
           MOVE "N" TO JA
           PERFORM UNTIL IND2 = 45
               IF TAB-END(IND2) = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
                  OR "7" OR "8" OR "9" OR "0"
                  IF JA = "N"
                     MOVE TAB-ENDERECO(1:IND2 - 1) TO AUX-ENDERECO
                     MOVE TAB-ENDERECO(IND2:6) TO NUMERO
                     MOVE "S" TO JA
                  END-IF
                  ADD 1 TO QTDE-NUMEROS
               END-IF
               ADD 1 TO IND2
           END-PERFORM
           IF JA = "N"
              MOVE TAB-ENDERECO(1:IND2 - 1) TO AUX-ENDERECO
              MOVE TAB-ENDERECO(IND2:6) TO NUMERO
              MOVE "S" TO JA
              MOVE 1 TO QTDE-NUMEROS
           END-IF

           MOVE AUX-ENDERECO TO TAB-ENDERECO
           MOVE 1 TO IND2
           PERFORM UNTIL IND2 = 45
               IF TAB-END(IND2) = "," OR "-"
                  MOVE TAB-ENDERECO(1:IND2 - 1) TO AUX-ENDERECO
               END-IF
               ADD 1 TO IND2
           END-PERFORM.

           IF QTDE-NUMEROS = 1
              STRING "00000" NUMERO DELIMITED BY " " INTO AUX-NUMERO
           END-IF
           IF QTDE-NUMEROS = 2
              STRING "0000" NUMERO DELIMITED BY " " INTO AUX-NUMERO
           END-IF
           IF QTDE-NUMEROS = 3
              STRING "000" NUMERO DELIMITED BY " " INTO AUX-NUMERO
           END-IF
           IF QTDE-NUMEROS = 4
              STRING "00" NUMERO DELIMITED BY " " INTO AUX-NUMERO
           END-IF
           IF QTDE-NUMEROS = 5
              STRING "0" NUMERO DELIMITED BY " " INTO AUX-NUMERO
           END-IF
           IF QTDE-NUMEROS = 6
              STRING NUMERO DELIMITED BY " " INTO AUX-NUMERO
           END-IF.

       IMPRIME-MALA SECTION.
           OPEN OUTPUT RELAT.

           MOVE 0  TO ETIQUETA QTDE-ETIQ
           MOVE 0  TO CONTADOR POSICAO


           IF GS-SO-REMETENTE = 1
              EVALUATE GS-OP-ETIQUETA
                   WHEN 1 MOVE NOME-CG30     TO AUX-NOME
                          MOVE ENDERECO-CG30 TO ENDERECO1-CG11
                          MOVE SPACES        TO BAIRRO1-CG11
                          MOVE CIDADE-CG30   TO CIDADE1-CG11
                          MOVE CEP-CG30      TO CEP1-CG11
                          MOVE ZEROS         TO CX-POSTAL1-CG11
                          PERFORM ETIQUETA-PADRAO

                          IF ETIQUETA = 1
                             WRITE REG-RELAT FROM DET-ETIQUETA1 AFTER 0
                             WRITE REG-RELAT FROM DET-ETIQUETA2
                             WRITE REG-RELAT FROM DET-ETIQUETA3
                             WRITE REG-RELAT FROM DET-ETIQUETA4
                             WRITE REG-RELAT FROM DET-ETIQUETA5
                             WRITE REG-RELAT FROM DET-ETIQUETA6
                             WRITE REG-RELAT FROM DET-ETIQUETA7

                             MOVE SPACES TO REG-RELAT
                             MOVE SPACES TO LINDET-REL

                             WRITE REG-RELAT FROM LINDET-REL
                             WRITE REG-RELAT FROM LINDET-REL
                             WRITE REG-RELAT FROM LINDET-REL
                          END-IF


                   WHEN 2 MOVE ZEROS TO IND
                          PERFORM UNTIL IND = GS-QTDE-ETIQUETAS
                             MOVE SPACES TO REG-RELAT
                             STRING "          " NOME-CG30
                                                          INTO REG-RELAT
                             WRITE REG-RELAT
                             MOVE SPACES TO REG-RELAT
                             STRING "          " ENDERECO-CG30 INTO
                                                              REG-RELAT
                             WRITE REG-RELAT
                             MOVE SPACES TO REG-RELAT
                             MOVE CIDADE-CG30 TO CIDADE
                             READ CAD010 INVALID KEY
                                  INITIALIZE REG-CAD010
                             END-READ
                             STRING "          " NOME-COMPL-CID
                             " - " UF-CID "   " CEP-CG30
                             INTO REG-RELAT
                             WRITE REG-RELAT
                             MOVE SPACES TO REG-RELAT
                             WRITE REG-RELAT AFTER 3
                             ADD 1 TO IND
                          END-PERFORM
                   WHEN 3 MOVE ZEROS TO IND
                          PERFORM UNTIL IND = GS-QTDE-ETIQUETAS
                              ADD 1 TO ETIQUETA
                              IF ETIQUETA = 1
                                 MOVE NOME-CG30       TO DET-ETI11
                                 MOVE ENDERECO-CG30   TO DET-ETI21
                                 MOVE CIDADE-CG30     TO CIDADE
                                 READ CAD010 INVALID KEY
                                      INITIALIZE REG-CAD010
                                 END-READ
                                 MOVE SPACES          TO DET-ETI31
                                 STRING NOME-COMPL-CID
                                 " - " UF-CID "   " CEP-CG30
                                 DELIMITED BY "     "
                                 INTO DET-ETI31
                                 MOVE SPACES          TO DET-ETI12
                                                         DET-ETI22
                                                         DET-ETI32
                              ELSE
                                 MOVE NOME-CG30       TO DET-ETI12
                                 MOVE ENDERECO-CG30   TO DET-ETI22
                                 MOVE CIDADE-CG30     TO CIDADE
                                 READ CAD010 INVALID KEY
                                      INITIALIZE REG-CAD010
                                 END-READ
                                 MOVE SPACES          TO DET-ETI32
                                 STRING NOME-COMPL-CID
                                 " - " UF-CID "   " CEP-CG30
                                 DELIMITED BY "     "
                                 INTO DET-ETI32
                                 MOVE 0               TO ETIQUETA
                                 WRITE REG-RELAT FROM DET-ETIQ1
                                 WRITE REG-RELAT FROM DET-ETIQ2
                                 WRITE REG-RELAT FROM DET-ETIQ3
                                 MOVE SPACES TO REG-RELAT
                                 WRITE REG-RELAT AFTER 3
                              END-IF
                              ADD 1 TO IND
                          END-PERFORM
                          IF ETIQUETA = 1
                             WRITE REG-RELAT FROM DET-ETIQ1
                             WRITE REG-RELAT FROM DET-ETIQ2
                             WRITE REG-RELAT FROM DET-ETIQ3
                             MOVE SPACES TO REG-RELAT
                             WRITE REG-RELAT AFTER 3
                          END-IF
              END-EVALUATE
              MOVE SPACES TO REG-RELAT
              WRITE REG-RELAT AFTER PAGE
              CLOSE RELAT
           ELSE
              EVALUATE GS-ORDEM-ESCOLHA
               WHEN 1     MOVE SPACES TO DET-ETIQUETA11 DET-ETIQUETA21
                          MOVE SPACES TO DET-ETIQUETA12 DET-ETIQUETA22
                          MOVE SPACES TO DET-ETIQUETA13 DET-ETIQUETA23
                          MOVE SPACES TO DET-ETIQUETA14 DET-ETIQUETA24
                          MOVE SPACES TO DET-ETIQUETA15 DET-ETIQUETA25
                          MOVE SPACES TO DET-ETIQUETA16 DET-ETIQUETA26
                          MOVE SPACES TO DET-ETIQUETA17 DET-ETIQUETA27
                          MOVE 1 TO GS-POSICAO
                          MOVE SPACES TO GS-LINNOME
                          MOVE "LER-LIST2" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                          PERFORM UNTIL GS-LINNOME = SPACES
      *                       END-IF
                              MOVE GS-LINNOME(1:8)  TO CODIGO-CG11
                                                       AUX-ALBUM
                              MOVE GS-LINNOME(73:1) TO CLASSIF-CG11
                              READ CGD011 INVALID KEY
                                  INITIALIZE REG-CGD011
                              END-READ
                              MOVE GS-LINNOME(13:60) TO AUX-NOME
                              MOVE GS-LINNOME(75:1)  TO GS-OP-ENDERECO
                              EVALUATE GS-OP-ETIQUETA
                                  WHEN 1 PERFORM ETIQUETA-PADRAO
                                  WHEN 2 PERFORM ETIQUETA-KELLO
                                  WHEN 3 PERFORM ETIQUETA-HP
                              END-EVALUATE
                              ADD  1 TO GS-POSICAO
                              MOVE SPACES TO GS-LINNOME
                              MOVE "LER-LIST2" TO DS-PROCEDURE
                              PERFORM CALL-DIALOG-SYSTEM
                          END-PERFORM
                          IF ETIQUETA = 1 AND GS-OP-ETIQUETA = 1
                              WRITE REG-RELAT FROM DET-ETIQUETA1
                              WRITE REG-RELAT FROM DET-ETIQUETA2
                              WRITE REG-RELAT FROM DET-ETIQUETA3
                              WRITE REG-RELAT FROM DET-ETIQUETA4
                              WRITE REG-RELAT FROM DET-ETIQUETA5
                              WRITE REG-RELAT FROM DET-ETIQUETA6
                              WRITE REG-RELAT FROM DET-ETIQUETA7
                          END-IF
                          IF ETIQUETA = 1 AND GS-OP-ETIQUETA = 3
                             WRITE REG-RELAT FROM DET-ETIQ1
                             WRITE REG-RELAT FROM DET-ETIQ2
                             WRITE REG-RELAT FROM DET-ETIQ3
                             WRITE REG-RELAT FROM DET-ETIQ4

                             MOVE SPACES TO REG-RELAT
                             MOVE SPACES TO LINDET-REL

                             WRITE REG-RELAT FROM LINDET-REL
                             WRITE REG-RELAT FROM LINDET-REL
                          END-IF

                          MOVE SPACES TO REG-RELAT
                          MOVE SPACES TO LINDET-REl
                          WRITE REG-RELAT FROM LINDET-REL AFTER PAGE
                          CLOSE RELAT
               WHEN OTHER EVALUATE GS-ORDEM-IMPRESS
                              WHEN 1 EVALUATE GS-OP-NOME
                                         WHEN 1 PERFORM CODIGO-MTD019
                                         WHEN 2 PERFORM CODIGO-CGD010
                                     END-EVALUATE
                              WHEN 2 EVALUATE GS-OP-NOME
                                         WHEN 1 PERFORM NOME-MTD019
                                         WHEN 2 PERFORM NOME-CGD010
                                     END-EVALUATE
                          END-EVALUATE
              END-EVALUATE.

       CODIGO-MTD019-COR SECTION.
           INITIALIZE REG-MTD019
           START MTD019 KEY IS NOT LESS ALBUM-MT19 INVALID KEY
               MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
                 READ MTD019 NEXT RECORD AT END
                      MOVE "10" TO ST-MTD019
                 NOT AT END
                         ADD 1 TO AUX-QTDE
                         MOVE 0         TO CLASSIF-CG11
                         MOVE ALBUMMT19 TO CODIGO-CG11 AUX-ALBUM
                         READ CGD011 INVALID KEY
                              INITIALIZE REG-CGD011
                         END-READ
                         MOVE   CPF1-CG11  TO    LINK-CPF
                         CALL   "DIGCPF"  USING LINKA-CPF
                         CANCEL "DIGCPF"
                         IF LINK-CPF-CONF       =       1
                            STRING "Cliente " ALBUMMT19      " está com
      -                     "o CPF " CPF1-CG11 " inválido" INTO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                         ELSE
                            MOVE CPF1-CG11      TO DET-CNPJ-CPF
                            MOVE NOME-FORM-MT19 TO DET-NOME
                            EVALUATE GS-OP-ENDERECO
                              WHEN 1 MOVE CEP1-CG11   TO DET-CEP-CORREIO
                                     MOVE BAIRRO1-CG11 TO DET-BAIRRO
                                     MOVE CIDADE1-CG11 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE COMPLEMENTO1-CG11 TO
                                          DET-COMPLEMENTO
                                     MOVE ENDERECO1-CG11 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                              WHEN 2 MOVE CEP2-CG11   TO DET-CEP-CORREIO
                                     MOVE BAIRRO2-CG11 TO DET-BAIRRO
                                     MOVE CIDADE2-CG11 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE COMPLEMENTO2-CG11 TO
                                          DET-COMPLEMENTO
                                     MOVE ENDERECO2-CG11 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                              WHEN 3 MOVE CODIGO-CG11    TO CODIGO-CG13
                                     READ CGD013 INVALID KEY
                                          INITIALIZE REG-CGD013
                                     END-READ
                                     MOVE CEP-CG13   TO DET-CEP-CORREIO
                                     MOVE BAIRRO-CG13 TO DET-BAIRRO
                                     MOVE CIDADE-CG13 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE SPACES     TO DET-COMPLEMENTO
                                     MOVE ENDERECO-CG13 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                            END-EVALUATE

                            MOVE DET-CORREIO-2 TO REG-CORREIO
                            WRITE REG-CORREIO
                         END-IF
                 END-READ
           END-PERFORM.

       CODIGO-CGD010-COR SECTION.
           INITIALIZE REG-CGD010
           START CGD010 KEY IS NOT LESS COD-COMPL-CG10 INVALID KEY
               MOVE "10" TO ST-CGD010.

           PERFORM UNTIL ST-CGD010 = "10"
                 READ CGD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CGD010
                 NOT AT END
                      ADD 1 TO AUX-QTDE
                      MOVE COD-COMPL-CG10 TO COD-COMPL-CG11
                      READ CGD011 INVALID KEY
                           INITIALIZE REG-CGD011
                      END-READ
                         MOVE   CPF1-CG11  TO    LINK-CPF
                         CALL   "DIGCPF"  USING LINKA-CPF
                         CANCEL "DIGCPF"
                         IF LINK-CPF-CONF       =       1
                            STRING "Cliente " COD-COMPL-CG11 " está com
      -                     "o CPF " CPF1-CG11
                            " inválido" INTO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                         ELSE
                            MOVE CPF1-CG11      TO DET-CNPJ-CPF
                            MOVE COMPRADOR-CG10 TO DET-NOME
                            EVALUATE GS-OP-ENDERECO
                              WHEN 1 MOVE CEP1-CG11   TO DET-CEP-CORREIO
                                     MOVE BAIRRO1-CG11 TO DET-BAIRRO
                                     MOVE CIDADE1-CG11 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                          DET-CIDADE
                                     MOVE COMPLEMENTO1-CG11 TO
                                          DET-COMPLEMENTO
                                     MOVE ENDERECO1-CG11 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                              WHEN 2 MOVE CEP2-CG11   TO DET-CEP-CORREIO
                                     MOVE BAIRRO2-CG11 TO DET-BAIRRO
                                     MOVE CIDADE2-CG11 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                          DET-CIDADE
                                     MOVE COMPLEMENTO2-CG11 TO
                                          DET-COMPLEMENTO
                                     MOVE ENDERECO2-CG11 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                              WHEN 3 MOVE CODIGO-CG11    TO CODIGO-CG13
                                     READ CGD013 INVALID KEY
                                          INITIALIZE REG-CGD013
                                     END-READ
                                     MOVE CEP-CG13   TO DET-CEP-CORREIO
                                     MOVE BAIRRO-CG13 TO DET-BAIRRO
                                     MOVE CIDADE-CG13 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                           DET-CIDADE
                                     MOVE SPACES     TO DET-COMPLEMENTO
                                     MOVE ENDERECO-CG13 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                             END-EVALUATE

                             MOVE DET-CORREIO-2 TO REG-CORREIO
                             WRITE REG-CORREIO
                         END-IF
                 END-READ
           END-PERFORM.

       CODIGO-MTD019 SECTION.
           INITIALIZE REG-MTD019
           START MTD019 KEY IS NOT LESS ALBUM-MT19 INVALID KEY
               MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
                 READ MTD019 NEXT RECORD AT END
                      MOVE "10" TO ST-MTD019
                 NOT AT END
                         MOVE 0         TO CLASSIF-CG11
                         MOVE ALBUMMT19 TO CODIGO-CG11 AUX-ALBUM
                         READ CGD011 INVALID KEY
                              INITIALIZE REG-CGD011
                         END-READ
                         MOVE NOME-FORM-MT19 TO AUX-NOME
                         PERFORM OPCAO-IMPRESSAO
                         IF IMPRIME-W = 0
                            CONTINUE
                         ELSE
                            EVALUATE GS-OP-ETIQUETA
                                  WHEN 1 PERFORM ETIQUETA-PADRAO
                                  WHEN 2 PERFORM ETIQUETA-KELLO
                                  WHEN 3 PERFORM ETIQUETA-HP
                            END-EVALUATE
                         END-IF
                 END-READ
           END-PERFORM
           IF ETIQUETA = 1 AND GS-OP-ETIQUETA = 1
               WRITE REG-RELAT FROM DET-ETIQUETA1
               WRITE REG-RELAT FROM DET-ETIQUETA2
               WRITE REG-RELAT FROM DET-ETIQUETA3
               WRITE REG-RELAT FROM DET-ETIQUETA4
               WRITE REG-RELAT FROM DET-ETIQUETA5
               WRITE REG-RELAT FROM DET-ETIQUETA6
               WRITE REG-RELAT FROM DET-ETIQUETA7
           END-IF
           IF ETIQUETA = 1 AND GS-OP-ETIQUETA = 3
              WRITE REG-RELAT FROM DET-ETIQ1
              WRITE REG-RELAT FROM DET-ETIQ2
              WRITE REG-RELAT FROM DET-ETIQ3
              WRITE REG-RELAT FROM DET-ETIQ4

              MOVE SPACES TO REG-RELAT
              MOVE SPACES TO LINDET-REL

              WRITE REG-RELAT FROM LINDET-REL
              WRITE REG-RELAT FROM LINDET-REL
           END-IF
           MOVE SPACES TO REG-RELAT
           MOVE SPACES TO LINDET-REl
           WRITE REG-RELAT FROM LINDET-REL AFTER PAGE
           CLOSE RELAT.

       CODIGO-CGD010 SECTION.
           INITIALIZE REG-CGD010
           START CGD010 KEY IS NOT LESS COD-COMPL-CG10 INVALID KEY
               MOVE "10" TO ST-CGD010.

           PERFORM UNTIL ST-CGD010 = "10"
                 READ CGD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CGD010
                 NOT AT END
                         PERFORM OPCAO-IMPRESSAO
                         IF IMPRIME-W = 0
                            CONTINUE
                         ELSE
                            EVALUATE GS-OP-ETIQUETA
                                  WHEN 1 PERFORM ETIQUETA-PADRAO
                                  WHEN 2 PERFORM ETIQUETA-KELLO
                                  WHEN 3 PERFORM ETIQUETA-HP
                            END-EVALUATE
                         END-IF
                 END-READ
           END-PERFORM
           IF ETIQUETA = 1 AND GS-OP-ETIQUETA = 1
               WRITE REG-RELAT FROM DET-ETIQUETA1
               WRITE REG-RELAT FROM DET-ETIQUETA2
               WRITE REG-RELAT FROM DET-ETIQUETA3
               WRITE REG-RELAT FROM DET-ETIQUETA4
               WRITE REG-RELAT FROM DET-ETIQUETA5
               WRITE REG-RELAT FROM DET-ETIQUETA6
               WRITE REG-RELAT FROM DET-ETIQUETA7
           END-IF
           IF ETIQUETA = 1 AND GS-OP-ETIQUETA = 3
              WRITE REG-RELAT FROM DET-ETIQ1
              WRITE REG-RELAT FROM DET-ETIQ2
              WRITE REG-RELAT FROM DET-ETIQ3
              WRITE REG-RELAT FROM DET-ETIQ4

              MOVE SPACES TO REG-RELAT
              MOVE SPACES TO LINDET-REL

              WRITE REG-RELAT FROM LINDET-REL
              WRITE REG-RELAT FROM LINDET-REL
           END-IF
           MOVE SPACES TO REG-RELAT
           MOVE SPACES TO LINDET-REl
           WRITE REG-RELAT FROM LINDET-REL AFTER PAGE
           CLOSE RELAT.

       NOME-MTD019-COR SECTION.
           INITIALIZE REG-MTD019
           START MTD019 KEY IS NOT LESS ALT2-MT19 INVALID KEY
               MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
                 READ MTD019 NEXT RECORD AT END
                      MOVE "10" TO ST-MTD019
                 NOT AT END
                         ADD 1 TO AUX-QTDE
                         MOVE 0         TO CLASSIF-CG11
                         MOVE ALBUMMT19 TO CODIGO-CG11 AUX-ALBUM
                         READ CGD011 INVALID KEY
                              INITIALIZE REG-CGD011
                         END-READ
                         MOVE   CPF1-CG11  TO    LINK-CPF
                         CALL   "DIGCPF"  USING LINKA-CPF
                         CANCEL "DIGCPF"
                         IF LINK-CPF-CONF       =       1
                            STRING "Cliente " ALBUMMT19      " está com
      -                     "o CPF " CPF1-CG11 " inválido" INTO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                         ELSE
                            MOVE CPF1-CG11       TO DET-CNPJ-CPF
                            MOVE NOME-FORM-MT19 TO DET-NOME
                            EVALUATE GS-OP-ENDERECO
                              WHEN 1 MOVE CEP1-CG11   TO DET-CEP-CORREIO
                                     MOVE BAIRRO1-CG11 TO DET-BAIRRO
                                     MOVE CIDADE1-CG11 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                          DET-CIDADE
                                     MOVE COMPLEMENTO1-CG11 TO
                                          DET-COMPLEMENTO
                                     MOVE ENDERECO1-CG11 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                              WHEN 2 MOVE CEP2-CG11   TO DET-CEP-CORREIO
                                     MOVE BAIRRO2-CG11 TO DET-BAIRRO
                                     MOVE CIDADE2-CG11 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE COMPLEMENTO2-CG11 TO
                                          DET-COMPLEMENTO
                                     MOVE ENDERECO2-CG11 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                              WHEN 3 MOVE CODIGO-CG11    TO CODIGO-CG13
                                     READ CGD013 INVALID KEY
                                          INITIALIZE REG-CGD013
                                     END-READ
                                     MOVE CEP-CG13   TO DET-CEP-CORREIO
                                     MOVE BAIRRO-CG13 TO DET-BAIRRO
                                     MOVE CIDADE-CG13 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE SPACES     TO DET-COMPLEMENTO
                                     MOVE ENDERECO-CG13 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                            END-EVALUATE
                            MOVE DET-CORREIO-2 TO REG-CORREIO
                            WRITE REG-CORREIO
                         END-IF
                 END-READ
           END-PERFORM.

       NOME-CGD010-COR SECTION.
           INITIALIZE REG-CGD010
           START CGD010 KEY IS NOT LESS COMPRADOR-CG10 INVALID KEY
               MOVE "10" TO ST-CGD010.

           PERFORM UNTIL ST-CGD010 = "10"
                 READ CGD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CGD010
                 NOT AT END
                      ADD 1 TO AUX-QTDE
                      MOVE COD-COMPL-CG10 TO COD-COMPL-CG11
                      READ CGD011 INVALID KEY
                           INITIALIZE REG-CGD011
                      END-READ
                         MOVE   CPF1-CG11  TO    LINK-CPF
                         CALL   "DIGCPF"  USING LINKA-CPF
                         CANCEL "DIGCPF"
                         IF LINK-CPF-CONF       =       1
                            STRING "Cliente " COD-COMPL-CG11 " está com
      -                     "o CPF " CPF1-CG11 " inválido"

                                                        INTO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                         ELSE
                            MOVE CPF1-CG11      TO DET-CNPJ-CPF
                            MOVE COMPRADOR-CG10 TO DET-NOME
                            EVALUATE GS-OP-ENDERECO
                              WHEN 1 MOVE CEP1-CG11   TO DET-CEP-CORREIO
                                     MOVE BAIRRO1-CG11 TO DET-BAIRRO
                                     MOVE CIDADE1-CG11 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE COMPLEMENTO1-CG11 TO
                                          DET-COMPLEMENTO
                                     MOVE ENDERECO1-CG11 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                              WHEN 2 MOVE CEP2-CG11   TO DET-CEP-CORREIO
                                     MOVE BAIRRO2-CG11 TO DET-BAIRRO
                                     MOVE CIDADE2-CG11 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE COMPLEMENTO2-CG11 TO
                                          DET-COMPLEMENTO
                                     MOVE ENDERECO2-CG11 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                              WHEN 3 MOVE CODIGO-CG11    TO CODIGO-CG13
                                     READ CGD013 INVALID KEY
                                          INITIALIZE REG-CGD013
                                     END-READ
                                     MOVE CEP-CG13   TO DET-CEP-CORREIO
                                     MOVE BAIRRO-CG13 TO DET-BAIRRO
                                     MOVE CIDADE-CG13 TO CIDADE
                                     READ CAD010 INVALID KEY
                                          MOVE SPACES TO NOME-CID
                                          MOVE SPACES TO UF-CID
                                     END-READ
                                     MOVE SPACES TO DET-CIDADE
                                     STRING NOME-CID "-" UF-CID INTO
                                     DET-CIDADE
                                     MOVE SPACES     TO DET-COMPLEMENTO
                                     MOVE ENDERECO-CG13 TO TAB-ENDERECO
                                     PERFORM SEPARAR-ENDERECO
                                     MOVE AUX-ENDERECO TO DET-LOGRADOURO
                                     MOVE AUX-NUMERO   TO DET-NUMERO
                            END-EVALUATE

                            MOVE DET-CORREIO-2 TO REG-CORREIO
                            WRITE REG-CORREIO
                         END-IF
                 END-READ
           END-PERFORM.

       NOME-MTD019 SECTION.
           INITIALIZE REG-MTD019
           START MTD019 KEY IS NOT LESS ALT2-MT19 INVALID KEY
               MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
                 READ MTD019 NEXT RECORD AT END
                      MOVE "10" TO ST-MTD019
                 NOT AT END
                         MOVE 0         TO CLASSIF-CG11
                         MOVE ALBUMMT19 TO CODIGO-CG11 AUX-ALBUM
                         READ CGD011 INVALID KEY
                              INITIALIZE REG-CGD011
                         END-READ
                         MOVE NOME-FORM-MT19 TO AUX-NOME
                         PERFORM OPCAO-IMPRESSAO
                         IF IMPRIME-W = 0
                            CONTINUE
                         ELSE
                            EVALUATE GS-OP-ETIQUETA
                                  WHEN 1 PERFORM ETIQUETA-PADRAO
                                  WHEN 2 PERFORM ETIQUETA-KELLO
                                  WHEN 3 PERFORM ETIQUETA-HP
                            END-EVALUATE
                         END-IF
                 END-READ
           END-PERFORM
           IF ETIQUETA = 1 AND GS-OP-ETIQUETA = 1
               WRITE REG-RELAT FROM DET-ETIQUETA1
               WRITE REG-RELAT FROM DET-ETIQUETA2
               WRITE REG-RELAT FROM DET-ETIQUETA3
               WRITE REG-RELAT FROM DET-ETIQUETA4
               WRITE REG-RELAT FROM DET-ETIQUETA5
               WRITE REG-RELAT FROM DET-ETIQUETA6
               WRITE REG-RELAT FROM DET-ETIQUETA7
           END-IF
           IF ETIQUETA = 1 AND GS-OP-ETIQUETA = 3
              WRITE REG-RELAT FROM DET-ETIQ1
              WRITE REG-RELAT FROM DET-ETIQ2
              WRITE REG-RELAT FROM DET-ETIQ3
              WRITE REG-RELAT FROM DET-ETIQ4

              MOVE SPACES TO REG-RELAT
              MOVE SPACES TO LINDET-REL

              WRITE REG-RELAT FROM LINDET-REL
              WRITE REG-RELAT FROM LINDET-REL
           END-IF
           MOVE SPACES TO REG-RELAT
           MOVE SPACES TO LINDET-REl
           WRITE REG-RELAT FROM LINDET-REL AFTER PAGE
           CLOSE RELAT.

       NOME-CGD010 SECTION.
           INITIALIZE REG-CGD010
           START CGD010 KEY IS NOT LESS COMPRADOR-CG10 INVALID KEY
               MOVE "10" TO ST-CGD010.

           PERFORM UNTIL ST-CGD010 = "10"
                 READ CGD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CGD010
                 NOT AT END
                         PERFORM OPCAO-IMPRESSAO
                         IF IMPRIME-W = 0
                            CONTINUE
                         ELSE
                            EVALUATE GS-OP-ETIQUETA
                                  WHEN 1 PERFORM ETIQUETA-PADRAO
                                  WHEN 2 PERFORM ETIQUETA-KELLO
                                  WHEN 3 PERFORM ETIQUETA-HP
                            END-EVALUATE
                         END-IF
                 END-READ
           END-PERFORM
           IF ETIQUETA = 1 AND GS-OP-ETIQUETA = 1
               WRITE REG-RELAT FROM DET-ETIQUETA1
               WRITE REG-RELAT FROM DET-ETIQUETA2
               WRITE REG-RELAT FROM DET-ETIQUETA3
               WRITE REG-RELAT FROM DET-ETIQUETA4
               WRITE REG-RELAT FROM DET-ETIQUETA5
               WRITE REG-RELAT FROM DET-ETIQUETA6
               WRITE REG-RELAT FROM DET-ETIQUETA7
           END-IF
           IF ETIQUETA = 1 AND GS-OP-ETIQUETA = 3
              WRITE REG-RELAT FROM DET-ETIQ1
              WRITE REG-RELAT FROM DET-ETIQ2
              WRITE REG-RELAT FROM DET-ETIQ3
              WRITE REG-RELAT FROM DET-ETIQ4

              MOVE SPACES TO REG-RELAT
              MOVE SPACES TO LINDET-REL

              WRITE REG-RELAT FROM LINDET-REL
              WRITE REG-RELAT FROM LINDET-REL
           END-IF
           MOVE SPACES TO REG-RELAT
           MOVE SPACES TO LINDET-REl
           WRITE REG-RELAT FROM LINDET-REL AFTER PAGE
           CLOSE RELAT.

       ETIQUETA-PADRAO SECTION.
           IF GS-OP-ENDERECO = 1 AND GS-SO-REMETENTE = 0
              IF ENDERECO1-CG11 <> SPACES AND
                 BAIRRO1-CG11 <> SPACES   AND
                 CIDADE1-CG11 <> 0 AND
                 CEP1-CG11 > 0
                 PERFORM CONTINUAR.

           IF GS-OP-ENDERECO = 2 AND GS-SO-REMETENTE = 0
              IF ENDERECO2-CG11 <> SPACES AND
                 BAIRRO2-CG11 <> SPACES   AND
                 CIDADE2-CG11 <> 0 AND
                 CEP2-CG11 > 0
                 MOVE ENDERECO2-CG11 TO ENDERECO1-CG11
                 MOVE COMPLEMENTO2-CG11 TO COMPLEMENTO1-CG11
                 MOVE BAIRRO2-CG11   TO BAIRRO1-CG11
                 MOVE CIDADE2-CG11   TO CIDADE1-CG11
                 MOVE CX-POSTAL2-CG11 TO CX-POSTAL1-CG11
                 MOVE CEP2-CG11      TO CEP1-CG11
                 PERFORM CONTINUAR.

           IF GS-OP-ENDERECO = 3 AND GS-SO-REMETENTE = 0
              MOVE CODIGO-CG11    TO CODIGO-CG13
              READ CGD013 INVALID KEY
                   INITIALIZE REG-CGD013
              END-READ
              IF ENDERECO-CG13 <> SPACES AND
                 BAIRRO-CG13 <> SPACES   AND
                 CIDADE-CG13 <> 0 AND
                 CEP-CG13 > 0
                 MOVE ENDERECO-CG13 TO ENDERECO1-CG11
                 MOVE SPACES        TO COMPLEMENTO1-CG11
                 MOVE ZEROS         TO CX-POSTAL1-CG11
                 MOVE BAIRRO-CG13   TO BAIRRO1-CG11
                 MOVE CIDADE-CG13   TO CIDADE1-CG11
                 MOVE CEP-CG13      TO CEP1-CG11
                 PERFORM CONTINUAR.

           IF GS-SO-REMETENTE = 1
              MOVE GS-ACP-REMETENTE TO CODIGO-CG30
              READ CGD030 NOT INVALID KEY
                   MOVE NOME-CG30     TO AUX-NOME
                   MOVE ENDERECO-CG30 TO ENDERECO1-CG11
                   MOVE SPACES        TO BAIRRO1-CG11
                   MOVE CIDADE-CG30   TO CIDADE1-CG11
                   MOVE CEP-CG30      TO CEP1-CG11
                   MOVE ZEROS         TO CX-POSTAL1-CG11
                   PERFORM CONTINUAR2.

       CONTINUAR2 SECTION.
           MOVE 0 TO IND
           PERFORM UNTIL IND = GS-QTDE-ETIQUETAS
               ADD 1 TO POSICAO
               ADD 1 TO ETIQUETA
               IF ETIQUETA = 1
                  MOVE SPACES            TO DET-ETIQUETA11
                  MOVE AUX-NOME          TO DET-ETIQUETA12
                  MOVE ENDERECO1-CG11    TO DET-ETIQUETA13
                  MOVE CX-POSTAL1-CG11   TO DET-CXPOSTAL
                  MOVE SPACES            TO DET-ETIQUETA14
                  STRING "CX.P.: "
                  DET-CXPOSTAL INTO      DET-ETIQUETA14
                  MOVE BAIRRO1-CG11      TO DET-ETIQUETA15
                  MOVE CIDADE1-CG11      TO CIDADE
                  READ CAD010 INVALID KEY
                       MOVE SPACES         TO DET-ETIQUETA16
                  NOT INVALID KEY
                       MOVE SPACES         TO DET-ETIQUETA16
                       MOVE CEP1-CG11         TO DET-CEP
                       STRING NOME-COMPL-CID "-"
                       UF-CID              INTO DET-ETIQUETA16
                  END-READ
                  MOVE CEP1-CG11         TO DET-CEP
                  MOVE SPACES            TO DET-ETIQUETA17
                  STRING DET-CEP INTO DET-ETIQUETA17

                  MOVE SPACES            TO DET-ETIQUETA21
                  MOVE SPACES            TO DET-ETIQUETA22
                  MOVE SPACES            TO DET-ETIQUETA23
                  MOVE SPACES            TO DET-ETIQUETA24
                  MOVE SPACES            TO DET-ETIQUETA25
                  MOVE SPACES            TO DET-ETIQUETA26
                  MOVE SPACES            TO DET-ETIQUETA27
               ELSE
                  MOVE SPACES TO DET-ETIQUETA21
                  MOVE SPACES            TO DET-ETIQUETA21
                  MOVE AUX-NOME          TO DET-ETIQUETA22
                  MOVE ENDERECO1-CG11    TO DET-ETIQUETA23
                  MOVE CX-POSTAL1-CG11   TO DET-CXPOSTAL
                  MOVE SPACES            TO DET-ETIQUETA24
                  STRING "CX.P.: "
                  DET-CXPOSTAL INTO      DET-ETIQUETA24
                  MOVE BAIRRO1-CG11      TO DET-ETIQUETA25
                  MOVE CIDADE1-CG11      TO CIDADE
                  READ CAD010 INVALID KEY
                       MOVE SPACES         TO DET-ETIQUETA26
                  NOT INVALID KEY
                       MOVE SPACES         TO DET-ETIQUETA26
                       MOVE CEP1-CG11         TO DET-CEP
                       STRING NOME-COMPL-CID "-"
                       UF-CID INTO DET-ETIQUETA26
                  END-READ
                  MOVE CEP1-CG11         TO DET-CEP
                  MOVE SPACES            TO DET-ETIQUETA27
                  STRING DET-CEP INTO DET-ETIQUETA27

                  MOVE 0                  TO ETIQUETA

                 WRITE REG-RELAT FROM DET-ETIQUETA1 AFTER 0
                 WRITE REG-RELAT FROM DET-ETIQUETA2
                 WRITE REG-RELAT FROM DET-ETIQUETA3
                 WRITE REG-RELAT FROM DET-ETIQUETA4
                 WRITE REG-RELAT FROM DET-ETIQUETA5
                 WRITE REG-RELAT FROM DET-ETIQUETA6
                 WRITE REG-RELAT FROM DET-ETIQUETA7

                 MOVE SPACES TO REG-RELAT
                 MOVE SPACES TO LINDET-REL

                 WRITE REG-RELAT FROM LINDET-REL
                 WRITE REG-RELAT FROM LINDET-REL
                 WRITE REG-RELAT FROM LINDET-REL
      *          IF POSICAO <> 2
      *             WRITE REG-RELAT FROM LINDET-REL
      *          END-IF
               END-IF
               ADD 1 TO IND.


       CONTINUAR SECTION.
              MOVE 0 TO IND
              PERFORM UNTIL IND = GS-QTDE-ETIQUETAS
                  ADD 1 TO POSICAO
                  ADD 1 TO ETIQUETA
                  IF ETIQUETA = 1
                     MOVE SPACES TO DET-ETIQUETA11
                     STRING "DESTINATARIO         "
                     CODIGO-CG11(1:4) "/" CODIGO-CG11(5:4)
                                                     INTO DET-ETIQUETA11
                     MOVE AUX-NOME          TO DET-ETIQUETA12
                     MOVE ENDERECO1-CG11    TO DET-ETIQUETA13
                     MOVE CX-POSTAL1-CG11   TO DET-CXPOSTAL
                     MOVE SPACES            TO DET-ETIQUETA14
                     STRING COMPLEMENTO1-CG11(1:21) " CXP.: "
                     DET-CXPOSTAL INTO DET-ETIQUETA14
                     MOVE BAIRRO1-CG11      TO DET-ETIQUETA15
                     MOVE CIDADE1-CG11      TO CIDADE
                     READ CAD010 INVALID KEY
                          MOVE SPACES         TO DET-ETIQUETA16
                     NOT INVALID KEY
                          MOVE SPACES         TO DET-ETIQUETA16
                          MOVE CEP1-CG11         TO DET-CEP
                          STRING NOME-COMPL-CID "-"
                          UF-CID              INTO DET-ETIQUETA16
                     END-READ
                     MOVE CEP1-CG11         TO DET-CEP
                     MOVE SPACES            TO DET-ETIQUETA17
                     STRING DET-CEP INTO DET-ETIQUETA17

                     MOVE SPACES            TO DET-ETIQUETA21
                     MOVE SPACES            TO DET-ETIQUETA22
                     MOVE SPACES            TO DET-ETIQUETA23
                     MOVE SPACES            TO DET-ETIQUETA24
                     MOVE SPACES            TO DET-ETIQUETA25
                     MOVE SPACES            TO DET-ETIQUETA26
                     MOVE SPACES            TO DET-ETIQUETA27
                  ELSE
                     MOVE SPACES TO DET-ETIQUETA21
                     STRING "DESTINATARIO         "
                     CODIGO-CG11(1:4) "/" CODIGO-CG11(5:4)
                                                     INTO DET-ETIQUETA21
                     MOVE AUX-NOME          TO DET-ETIQUETA22
                     MOVE ENDERECO1-CG11    TO DET-ETIQUETA23
                     MOVE CX-POSTAL1-CG11   TO DET-CXPOSTAL
                     MOVE SPACES            TO DET-ETIQUETA24
                     STRING COMPLEMENTO1-CG11(1:21) " CXP.: "
                     DET-CXPOSTAL INTO      DET-ETIQUETA24
                     MOVE BAIRRO1-CG11      TO DET-ETIQUETA25
                     MOVE CIDADE1-CG11      TO CIDADE
                     READ CAD010 INVALID KEY
                          MOVE SPACES         TO DET-ETIQUETA26
                     NOT INVALID KEY
                          MOVE SPACES         TO DET-ETIQUETA26
                          MOVE CEP1-CG11         TO DET-CEP
                          STRING NOME-COMPL-CID "-"
                          UF-CID INTO DET-ETIQUETA26
                     END-READ
                     MOVE CEP1-CG11         TO DET-CEP
                     MOVE SPACES            TO DET-ETIQUETA27
                     STRING DET-CEP INTO DET-ETIQUETA27

                     MOVE 0                  TO ETIQUETA

                    WRITE REG-RELAT FROM DET-ETIQUETA1 AFTER 0
                    WRITE REG-RELAT FROM DET-ETIQUETA2
                    WRITE REG-RELAT FROM DET-ETIQUETA3
                    WRITE REG-RELAT FROM DET-ETIQUETA4
                    WRITE REG-RELAT FROM DET-ETIQUETA5
                    WRITE REG-RELAT FROM DET-ETIQUETA6
                    WRITE REG-RELAT FROM DET-ETIQUETA7

                    MOVE SPACES TO REG-RELAT
                    MOVE SPACES TO LINDET-REL

                    WRITE REG-RELAT FROM LINDET-REL
                    WRITE REG-RELAT FROM LINDET-REL
                    WRITE REG-RELAT FROM LINDET-REL
      *             IF POSICAO <> 2
      *                WRITE REG-RELAT FROM LINDET-REL
      *             END-IF
                  END-IF
                  ADD 1 TO IND.

       ETIQUETA-KELLO SECTION.
           MOVE ZEROS  TO ALBUM-KELLO CEP-KELLO
           MOVE SPACES TO NOME-KELLO  ENDERECO-KELLO BAIRRO-KELLO
                          CIDADE-KELLO

           MOVE ZEROS TO IND

           PERFORM UNTIL IND = GS-QTDE-ETIQUETAS

               MOVE AUX-ALBUM                  TO ALBUM-KELLO
               MOVE AUX-NOME                   TO NOME-KELLO

               EVALUATE GS-OP-ENDERECO
                   WHEN 1  MOVE ENDERECO1-CG11 TO ENDERECO-KELLO
                           MOVE BAIRRO1-CG11   TO BAIRRO-KELLO
                           MOVE CEP1-CG11      TO CEP-KELLO
                           MOVE CIDADE1-CG11   TO CIDADE
                           READ CAD010 INVALID KEY
                                MOVE SPACES    TO NOME-COMPL-CID UF-CID
                           END-READ
                           STRING NOME-COMPL-CID " - " UF-CID
                           DELIMITED BY "   " INTO CIDADE-KELLO
                   WHEN 2  MOVE ENDERECO2-CG11 TO ENDERECO-KELLO
                           MOVE BAIRRO2-CG11   TO BAIRRO-KELLO
                           MOVE CEP2-CG11      TO CEP-KELLO
                           MOVE CIDADE2-CG11   TO CIDADE
                           READ CAD010 INVALID KEY
                                MOVE SPACES    TO NOME-COMPL-CID UF-CID
                           END-READ
                           STRING NOME-COMPL-CID " - " UF-CID
                           DELIMITED BY "   " INTO CIDADE-KELLO
                   WHEN 3  MOVE CODIGO-CG11    TO CODIGO-CG13
                           READ CGD013 INVALID KEY
                                INITIALIZE REG-CGD013
                           END-READ
                           MOVE ENDERECO-CG13  TO ENDERECO-KELLO
                           MOVE BAIRRO-CG13    TO BAIRRO-KELLO
                           MOVE CEP-CG13       TO CEP-KELLO
                           MOVE CIDADE-CG13    TO CIDADE
                           READ CAD010 INVALID KEY
                                MOVE SPACES    TO NOME-COMPL-CID UF-CID
                           END-READ
                           STRING NOME-COMPL-CID " - " UF-CID
                           DELIMITED BY "   " INTO CIDADE-KELLO
               END-EVALUATE

               WRITE REG-RELAT FROM KELLO1
               WRITE REG-RELAT FROM KELLO2
               WRITE REG-RELAT FROM KELLO3
               MOVE SPACES TO REG-RELAT
               WRITE REG-RELAT AFTER 3

               IF GS-REMETENTE = 1
                  MOVE SPACES TO REG-RELAT
                  STRING "          " NOME-CG30 INTO REG-RELAT
                  WRITE REG-RELAT
                  MOVE SPACES TO REG-RELAT
                  STRING "          " ENDERECO-CG30 INTO REG-RELAT
                  WRITE REG-RELAT
                  MOVE SPACES TO REG-RELAT
                  MOVE CIDADE-CG30 TO CIDADE
                  READ CAD010 INVALID KEY
                       INITIALIZE REG-CAD010
                  END-READ
                  STRING "          " NOME-COMPL-CID " - " UF-CID
                  "   " CEP-CG30 INTO REG-RELAT
                  WRITE REG-RELAT
                  MOVE SPACES TO REG-RELAT
                  WRITE REG-RELAT AFTER 3
               END-IF
               ADD 1 TO IND
           END-PERFORM.

       ETIQUETA-HP SECTION.
           EVALUATE GS-OP-ENDERECO
               WHEN 2  MOVE ENDERECO2-CG11 TO ENDERECO1-CG11
                       MOVE BAIRRO2-CG11   TO BAIRRO1-CG11
                       MOVE CEP2-CG11      TO CEP1-CG11
                       MOVE CIDADE2-CG11   TO CIDADE1-CG11
               WHEN 3  MOVE CODIGO-CG11    TO CODIGO-CG13
                       READ CGD013 INVALID KEY
                            INITIALIZE REG-CGD013
                       END-READ
                       MOVE ENDERECO-CG13  TO ENDERECO1-CG11
                       MOVE BAIRRO-CG13    TO BAIRRO1-CG11
                       MOVE CEP-CG13       TO CEP1-CG11
                       MOVE CIDADE-CG13    TO CIDADE1-CG11
           END-EVALUATE

           IF AUX-NOME <> SPACES AND
              ENDERECO1-CG11 <> SPACES AND
              CIDADE1-CG11 <> 0 AND
              CEP1-CG11 > 0


              add 1 to qtde-etiq
      *       if qtde-etiq < 13
                 MOVE SPACES TO REG-RELAT
                 WRITE REG-RELAT
      *       end-if


              MOVE 0 TO IND
              PERFORM UNTIL IND = GS-QTDE-ETIQUETAS
                  ADD 1 TO POSICAO
                  ADD 1 TO ETIQUETA
                  IF ETIQUETA = 1
                     MOVE AUX-NOME        TO DET-ETI11
                     MOVE ENDERECO1-CG11  TO DET-ETI21
                     STRING BAIRRO1-CG11  "  " CEP1-CG11
                     INTO DET-ETI31
                     MOVE CIDADE1-CG11    TO CIDADE
                     READ CAD010 INVALID KEY
                          INITIALIZE REG-CAD010
                     END-READ
                     MOVE SPACES          TO DET-ETI41
                     STRING NOME-COMPL-CID
                     " - " UF-CID DELIMITED BY "    "
                     INTO DET-ETI41
                     MOVE SPACES          TO DET-ETI12
                                             DET-ETI22
                                             DET-ETI32
                                             DET-ETI42
                  ELSE
                     MOVE AUX-NOME        TO DET-ETI12
                     MOVE ENDERECO1-CG11  TO DET-ETI22
                     STRING BAIRRO1-CG11  "  " CEP1-CG11
                     INTO DET-ETI32
                     MOVE CIDADE1-CG11    TO CIDADE
                     READ CAD010 INVALID KEY
                          INITIALIZE REG-CAD010
                     END-READ
                     MOVE SPACES          TO DET-ETI42
                     STRING NOME-COMPL-CID
                     " - " UF-CID DELIMITED BY "    " INTO DET-ETI42

                     MOVE 0                  TO ETIQUETA

                     WRITE REG-RELAT FROM DET-ETIQ1
                     WRITE REG-RELAT FROM DET-ETIQ2
                     WRITE REG-RELAT FROM DET-ETIQ3
                     WRITE REG-RELAT FROM DET-ETIQ4

                     MOVE SPACES TO REG-RELAT
                     MOVE SPACES TO LINDET-REL

                     WRITE REG-RELAT FROM LINDET-REL
                     WRITE REG-RELAT FROM LINDET-REL
                  END-IF
                  ADD 1 TO IND
              END-PERFORM
              IF QTDE-ETIQ = 14
                 MOVE 0 TO QTDE-ETIQ
                 MOVE SPACES TO REG-RELAT
                 WRITE REG-RELAT AFTER PAGE
              END-IF.

       EXIBIR-MENSAGEM SECTION.
           move 1 to gs-flag-critica
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
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
           move "CGP010A"           to logacess-programa
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
           CLOSE IED011 CGD010 CGD011 CGD012 CGD013 CGD014 CGD017
                 CAD010 MTD019 CGD030 CAD030 CGD020 CGD016 CGD015
                 CAD004
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
