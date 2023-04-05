       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ITAU.
       AUTHOR.        ALFREDO SAVIOLLI NETO.
      *GERA ARQUIVO XXXXXXXX.REM P/ ITAU
       DATE-WRITTEN.  08-10-2010.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                      PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CAPX010.
           COPY CAPX018.
           COPY CRPX020.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX011.
           COPY CRPX200.
           COPY CRPX201.
           COPY BOLITAU.SEL.
           COPY LOGACESS.SEL.

           select remes
                  assign       to   arquivo-remes
                  organization is      sequential
                  access mode  is      sequential
                  file status  is        fs-remes.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS NOME-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           select acesso
                  assign       to   arquivo-acesso
                  organization is          indexed
                  access mode  is          dynamic
                  record key   is    acesso-codigo
                  file status  is        fs-acesso.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CAPW010.
       COPY CAPW018.
       COPY CRPW020.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CRPW200.
       COPY CRPW201.
       COPY BOLITAU.FD.
       COPY LOGACESS.FD.

       fd remes label record is omitted.
       01 reg-remes.
           05  dados-remes      pic x(400).
           05  pula-remes       pic x(002).

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK           PIC 9(3).
           05  NOME-WK          PIC X(40).
           05  ENDERECO-WK      PIC X(40).
           05  BAIRRO-WK        PIC X(12).
           05  CEP-WK           PIC 9(8).
           05  CIDADE-WK        PIC X(15).
           05  UF-WK            PIC XX.
           05  DOCTO-WK         PIC X(10).
           05  VALOR-WK         PIC 9(8)V99.

       FD  RELAT.
       01  REG-RELAT.
           05  FILLER           PIC X(132).

       FD  ACESSO.
       01  REG-ACESSO.
           05 ACESSO-CODIGO     PIC 9(01).
           05 ACESSO-USUARIO    PIC X(05).
           05 ACESSO-DATA       PIC 9(08).
           05 ACESSO-HORARIO    PIC 9(08).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "ITAU.CPB".
           COPY "ITAU.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  AREAS-DE-TRABALHO.
           05 ERRO-1                 PIC XX     VALUE SPACES.
           05 ST-CAD001              PIC XX     VALUE SPACES.
           05 ST-CAD010              PIC XX     VALUE SPACES.
           05 ST-CAD018              PIC XX     VALUE SPACES.
           05 ST-CRD020              PIC XX     VALUE SPACES.
           05 ST-CGD001              PIC XX     VALUE SPACES.
           05 ST-CGD010              PIC XX     VALUE SPACES.
           05 ST-CGD011              PIC XX     VALUE SPACES.
           05 ST-CRD200              PIC XX     VALUE SPACES.
           05 ST-CRD201              PIC XX     VALUE SPACES.
           05 FS-LOGACESS            PIC XX     VALUE SPACES.
           05 FS-BOLITAU             PIC XX     VALUE SPACES.
           05 FS-ACESSO              PIC XX     VALUE SPACES.
           05 ST-REM                 PIC XX     VALUE SPACES.
           05 ST-WORK                PIC XX     VALUE SPACES.
           05 FS-REMES               PIC XX     VALUE SPACES.
           05 VARIA-W                PIC 9(8)   VALUE ZEROS.
           05 VALOR-W                PIC 9(11)V99 VALUE ZEROS.
           05 SEQ-W                  PIC 9(6)   VALUE ZEROS.
           05 OPCAO                  PIC 9      VALUE ZEROS.
           05 SEQUENCIA-W            PIC 9(10)     VALUE ZEROS.
           05 TIPO-W                 PIC 9      VALUE ZEROS.
           05 DATA-DIA               PIC 9(6)   VALUE ZEROS.
           05 COD-COMPL-CR20-W       PIC 9(09)  VALUE ZEROS.
           05 DATA-DIA-I             PIC 9(6)   VALUE ZEROS.
           05 HORA-BRA               PIC 9(8)   VALUE ZEROS.
           05 ULT-SEQ                PIC 9(5)   VALUE ZEROS.
           05 DATA-E                 PIC 99/99/99.
           05 VALOR-ATRASO           PIC 9(11)V99 VALUE ZEROS.
           05 CONF                   PIC X      VALUE SPACES.
           05 VALOR-TOTAL            PIC 9(12)V99 VALUE ZEROS.
           05 ERRO-W                 PIC 9        VALUE ZEROS.
           05 DATA-INV               PIC 9(8)     VALUE ZEROS.
           05 VENCTO-INI-INV         PIC 9(8)     VALUE ZEROS.
           05 VENCTO-FIM-INV         PIC 9(8)     VALUE ZEROS.
           05 LIN                    PIC 9(02)    VALUE ZEROS.
           05 IND                    PIC 9(03)    VALUE ZEROS.
           05 PASSAR-STRING-1        PIC X(65).
           05 MASC-VALOR             PIC ZZ9,99   VALUE ZEROS.
           05 MOVTO-INI-INV          PIC 9(8)     VALUE ZEROS.
           05 MOVTO-FIM-INV          PIC 9(8)     VALUE ZEROS.
           05 QTDE-TIT-T2            PIC 9(8).
           05 AUX-TIPO               PIC 9(1)     VALUE ZEROS.
           05 AUX-CARTEIRA           PIC 9(1)     VALUE ZEROS.
           05 MASC-NUMERO            PIC ZZ.ZZZ.ZZ9 VALUE ZEROS.
           05 VALOR-TOT-TIT-T2       PIC 9(14).
           05  ALB-BUM.
               10  CONTR             PIC 9(04).
               10  ALB               PIC 9(04).
           05  ALBUM-W REDEFINES ALB-BUM PIC 9(08).
           05 DATAW.
              10  DIA-W              PIC 99.
              10  MES-W              PIC 99.
              10  ANO-W              PIC 99.
           05 DATA-W REDEFINES DATAW PIC 9(6).
           05 DATAI.
              10  ANO-I              PIC 99.
              10  MES-I              PIC 99.
              10  DIA-I              PIC 99.
           05 DATA-I REDEFINES DATAI PIC 9(6).
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".


       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

       01 HEADER-ARQUIVO.
          02 FILLER                PIC 9(001) VALUE 0.
          02 FILLER                PIC 9(001) VALUE 1.
          02 FILLER                PIC X(007) VALUE "REMESSA".
          02 FILLER                PIC 9(002) VALUE 01.
          02 FILLER                PIC X(015) VALUE "COBRANCA".
          02 H-AGENCIA             PIC 9(004).
          02 FILLER                PIC X(002) VALUE "00".
          02 H-CONTA               PIC 9(005).
          02 H-DAC                 PIC 9(001).
          02 FILLER                PIC X(008) VALUE SPACES.
          02 H-NOME-EMPRESA        PIC X(030).
          02 FILLER                PIC 9(003) VALUE 341.
          02 FILLER                PIC X(015) VALUE "BANCO ITAU SA".
          02 H-DATA-ARQUIVO        PIC 9(006).
          02 FILLER                PIC X(294) VALUE SPACES.
          02 FILLER                PIC X(006) VALUE "000001".

       01 DETALHE-ARQUIVO.
          02 D-TIPO-ARQUIVO        PIC 9(001) VALUE 1.
          02 D-TIPO-INSCRICAO      PIC 9(002).
          02 D-INSC-EMPRESA        PIC 9(014).
          02 D-AGENCIA             PIC 9(004).
          02 D-ZERO                PIC X(002).
          02 D-CONTA-CORRENTE      PIC 9(005).
          02 D-DAC                 PIC 9(001).
          02 FILLER                PIC X(004).
          02 D-CODIGO-ALEGACAO     PIC 9(004).
          02 D-USO-EMPRESA         PIC X(025).
          02 D-NOSSO-NUMERO        PIC 9(008).
          02 D-QTDE-MOEDA          PIC 9(008)V99999.
          02 D-NUM-CARTEIRA        PIC 9(003).
          02 FILLER                PIC X(021).
          02 D-CARTEIRA            PIC X(001).
          02 D-CODIGO-OCORRENCIA   PIC 9(002).
          02 D-NUMERO-DOCUMENTO    PIC X(010).
          02 D-VENCIMENTO          PIC 9(006).
          02 D-VALOR-TITULO        PIC 9(011)V99.
          02 D-BANCO               PIC X(003).
          02 D-AGENCIA-COBRADORA   PIC 9(005).
          02 D-ESPECIE             PIC X(002).
          02 D-ACEITE              PIC X(001).
          02 D-DATA-EMISSAO        PIC 9(006).
          02 D-INSTRUCAO-1         PIC X(002).
          02 D-INSTRUCAO-2         PIC X(002).
          02 D-JUROS-1-DIA         PIC 9(011)V99.
          02 D-DESCONTO-ATE        PIC 9(006).
          02 D-VALOR-DESCONTO      PIC 9(011)V99.
          02 D-VALOR-IOF           PIC 9(011)V99.
          02 D-ABATIMENTO          PIC 9(011)V99.
          02 D-CODIGO-INSCRICAO    PIC 9(002).
          02 D-NUMERO-INSCRICAO    PIC 9(014).
          02 D-NOME-SACADO         PIC X(030).
          02 FILLER                PIC X(010).
          02 D-LOGRADOURO          PIC X(040).
          02 D-BAIRRO              PIC X(012).
          02 D-CEP                 PIC 9(008).
          02 D-CIDADE              PIC X(015).
          02 D-ESTADO              PIC X(002).
          02 D-SACADOR-AVALISTA    PIC X(030).
          02 FILLER                PIC X(004).
          02 D-DATA-MORA           PIC 9(006).
          02 D-PRAZO               PIC 9(002).
          02 FILLER                PIC X(001).
          02 D-NUMERO-REMESSA      PIC 9(006).

       01 TRAILER-ARQUIVO.
          02 T-TIPO-ARQUIVO        PIC 9(001) VALUE 9.
          02 FILLER                PIC X(393).
          02 T-NUMERO-REMESSA      PIC 9(006).

       77 ws-int                   pic 9(02).
       01 ws-data-desconto.
          05 desconto-ano          pic 9(04).
          05 desconto-mes          pic 9(02).
          05 desconto-dia          pic 9(02).

       01 tabela-dias              pic x(024)
                                   value "312831303130313130313031".
       01 tab-mes                  pic 9(002) occurs 12 times
                                   redefines tabela-dias.

       01  linka-cpf.
           05  link-cpf             pic 9(11).
           05  link-cpf-r redefines link-cpf.
               10  link-cpf-num     pic 9(09).
               10  link-cpf-dig1    pic 9(01).
               10  link-cpf-dig2    pic 9(01).
           05  link-cpf-conf        pic 9(01).

       01  linka-cgc.
           05  link-cgc             pic 9(14).
           05  link-cgc-r redefines link-cgc.
               10  link-cgc-num     pic 9(12).
               10  link-cgc-dig1    pic 9(01).
               10  link-cgc-dig2    pic 9(01).
           05  link-cgc-conf        pic 9(01).

       01 OK                        PIC X(01).

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem                     pic x(200).
       01 tipo-msg                     pic x(01).
       01 resp-msg                     pic x(01).

       01  CAB01.
           05  FILLER               PIC X(115) VALUE
           'RELATORIO DE REMESSA - BANESTADO'.
           05  FILLER               PIC X(09) VALUE 'EMISSAO: '.
           05  EMISSAO-REL          PIC 99/99/99.
       01  CAB02.
           05  FILLER               PIC X(132) VALUE ALL "=".
       01  CAB03.
           05  FILLER               PIC X(132) VALUE
           "NOME                           ENDERECO
      -    "    BAIRRO       CEP       CIDADE          UF DOCUMENTO
      -    "     VALOR".
       01  LINDET.
           05  NOME-REL             PIC X(30) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  ENDERECO-REL         PIC X(30) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  BAIRRO-REL           PIC X(15) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  CEP-REL              PIC ZZZZZ.
           05  FILLER               PIC X     VALUE ".".
           05  SUFIXO-REL           PIC ZZZ.
           05  FILLER               PIC X     VALUE SPACES.
           05  CIDADE-REL           PIC X(15) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  UF-REL               PIC XX    VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  DOCTO-REL            PIC X(11) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  VALOR-REL            PIC ZZ.ZZZ.ZZZ,ZZ.
       01  LINDET1.
           05  FILLER               PIC X(20) VALUE 'VALOR TOTAL.: '.
           05  VALOR-TOTAL-REL      PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X(20) VALUE SPACES.
           05  FILLER               PIC X(20) VALUE 'QTDE TITULOS: '.
           05  QTDE-TIT-TOTAL-REL   PIC ZZZ.ZZZ.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU         PIC 9(04).
             10 WS-MES-CPU         PIC 9(02).
             10 WS-DIA-CPU         PIC 9(02).
          05 FILLER                PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).


       77  POP-UP                  PIC X(65).
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA-I FROM DATE.
           MOVE DIA-I TO DIA-W.
           MOVE MES-I TO MES-W.
           MOVE ANO-I TO ANO-W.
           MOVE DATA-W TO DATA-DIA.
           MOVE DATA-I(3: 4) TO DATA-DIA-I(5: 4)
           MOVE ANO-I        TO DATA-DIA-I(3: 2)
           IF ANO-I > 90 MOVE "19" TO DATA-DIA-I(1: 2)
           ELSE MOVE 20 TO DATA-DIA-I(1: 2).
           ACCEPT HORA-BRA FROM TIME.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "BOLITAU"  TO ARQ-REC.  MOVE EMPRESA-REF
                                                      TO ARQUIVO-BOLITAU
           MOVE "ACESSO"   TO ARQ-REC.  MOVE EMPRESA-REF
                                                      TO ARQUIVO-ACESSO
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CGD010 CGD011 CAD010 CAD018 CAD001 CGD001
           OPEN I-O   BOLITAU
           CLOSE      BOLITAU
           OPEN I-O   BOLITAU CRD020
           CLOSE      CRD020
           OPEN INPUT CRD020

           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "ITAU"              to logacess-programa
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

           open i-o acesso

           move 1                      to acesso-codigo
           read acesso invalid key
                move 1                 to acesso-codigo
                move usuario-w         to acesso-usuario
                move ws-data-cpu       to acesso-data
                move ws-hora-sys       to acesso-horario
                write reg-acesso invalid key
                     move "Erro de Gravação...ACESSO" to mensagem
                     move "C" to tipo-msg
                     perform exibir-mensagem
                end-write
                move "S" to ok
           not invalid key
                move "N" to ok
           end-read

           close    acesso

           if ok = "N"
              move spaces to mensagem
              string "Outro Usuário está com o programa Aberto" x"0da0"
                     "Usuário => " acesso-usuario x"0da0"
                     "Data => " acesso-data(7:2) "/"
                                acesso-data(5:2) "/"
                                acesso-data(1:4)  x"0da0"
                     "Horário => " acesso-horario(1:2) ":"
                                   acesso-horario(3:2) ":"
                                   acesso-horario(5:2)
                into mensagem
                move "C" to tipo-msg
              perform exibir-mensagem
              move 1 to erro-w
              move 1 to gs-exit-flg.

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   MOVE 1 TO GS-CEP GS-CPF GS-ENDERECO
                   REFRESH-OBJECT PRINCIPAL
                   PERFORM LER

                   MOVE BOLITAU-NOSSO-NUM       TO MASC-NUMERO
                   MOVE SPACES                  TO MENSAGEM
                   STRING "Preste Atenção na Seqüência Mostrada" x"0da0"
                          "Seqüência -> " masc-numero
                     INTO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM

               WHEN GS-IMPRIMIR-RELATORIO-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GERAR-REMESSA-TRUE
                    PERFORM GERA-ARQ-REMESSA
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-ATUALIZA-PORTADOR-TRUE
                    PERFORM ATUALIZA-PORTADOR-RECEBER
               WHEN GS-ATUALIZ-SEQUENCIA-TRUE
                    PERFORM ATUALIZA-SEQUENCIA
               WHEN GS-GRAVAR-TRUE
                    PERFORM GRAVAR
               WHEN GS-LE-PARAM-TRUE
                    PERFORM LER
               WHEN GS-CRITICAR-TRUE
                    PERFORM CRITICAR
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LE-VENDEDOR
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM POPUP-VENDEDOR
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR        TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE "****"        TO NOME-CG01.
           MOVE NOME-CG01          TO GS-DESC-VENDEDOR.

       POPUP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-VENDEDOR.

       CRITICAR SECTION.
           EVALUATE GS-CAMPO-CRITICA
               WHEN "EF-PORTADOR"  PERFORM CRITICAR-PORTADOR
           END-EVALUATE.

       CRITICAR-PORTADOR SECTION.
           IF GS-ACP-PORTADOR = ZEROS
              CALL   "CAP018T" USING PARAMETROS-W POP-UP
              CANCEL "CAP018T"
              MOVE POP-UP(1: 30)            TO GS-DESC-PORTADOR
              MOVE POP-UP(33: 4)            TO GS-ACP-PORTADOR
           END-IF
           IF GS-ACP-PORTADOR = ZEROS
              MOVE "Portador Não Informado" TO MENSAGEM
              MOVE "C"                      TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE GS-ACP-PORTADOR          TO PORTADOR
              READ CAD018 INVALID KEY
                   MOVE "*******"           TO NOME-PORT
              END-READ
              MOVE NOME-PORT                TO GS-DESC-PORTADOR
              REFRESH-OBJECT WIN1.

       LER SECTION.
           MOVE 1                       TO BOLITAU-CODIGO
           READ BOLITAU INVALID KEY
                INITIALIZE REG-BOLITAU.

           PERFORM MOVER-DADOS.

       MOVER-DADOS SECTION.
           MOVE BOLITAU-NUM-BANCO       TO GS-ACP-I-CODBAN
           MOVE BOLITAU-NOME-BANCO      TO GS-ACP-I-NOMEBAN
           MOVE BOLITAU-AGENCIA         TO GS-ACP-I-AGENCIA
           MOVE BOLITAU-DIG-AGENCIA     TO GS-ACP-I-DIG-AGENCIA
           MOVE BOLITAU-CONTA           TO GS-ACP-I-CONTA
           MOVE BOLITAU-DIG-CONTA       TO GS-ACP-I-DIG-CONTA
           MOVE BOLITAU-CNPJ            TO GS-ACP-I-CNPJ
           MOVE BOLITAU-NOSSO-NUM       TO GS-ACP-I-NOSSO-NUMERO
           MOVE BOLITAU-QTDE-DIAS       TO GS-ACP-I-DIAS-INSTRUCAO
           MOVE BOLITAU-JUROS-1-DIA     TO GS-ACP-I-JUROS-1-DIA
           MOVE BOLITAU-DESCONTO        TO GS-ACP-I-DESCONTO
           MOVE BOLITAU-MULTA           TO GS-ACP-I-MULTA
           MOVE BOLITAU-DIAS-DESCONTO   TO GS-ACP-I-DIAS-DESCONTO

           INITIALIZE GS-ACP-I-CARTEIRA
           EVALUATE BOLITAU-CARTEIRA
              WHEN 102
              MOVE "102 - I - SEM REGISTRO COM EMISSÃO INTEGRAL CARNÊ"
                TO GS-ACP-I-CARTEIRA
              WHEN 103
              MOVE "103 - I - SEM REGISTRO COM EMISSÃO E ENTREGA CARNÊ"
                TO GS-ACP-I-CARTEIRA
              WHEN 104
              MOVE "104 - I - ESCRITURAL ELETRÔNICA CARNÊ"
                TO GS-ACP-I-CARTEIRA
              WHEN 107
              MOVE "107 - I - SEM REGISTRO COM EMISSÃO INTEGRAL 15 POSIÇ
      -            "ÕES - CARNÊ" TO GS-ACP-I-CARTEIRA
              WHEN 108
              MOVE "108 - I - DIRETA ELETRÔNICA EMISSÃO INTEGRAL CARNÊ"
                TO GS-ACP-I-CARTEIRA
              WHEN 109
              MOVE "109 - I - DIRETA ELETRÔNICA SEM EMISSÃO SIMPLES"
                TO GS-ACP-I-CARTEIRA
              WHEN 112
              MOVE "112 - I - ESCRITURAL ELETRÔNICA SIMPLES"
                TO GS-ACP-I-CARTEIRA
              WHEN 121
              MOVE "121 - I - DIRETA ELETRÔNICA EMISSÃO PARCIAL SIMPLES"
                TO GS-ACP-I-CARTEIRA
              WHEN 129
              MOVE "129 - I - SEM REGISTRO EMISSÃO PARCIAL SEGUROS COM I
      -            "OF 2%" TO GS-ACP-I-CARTEIRA
              WHEN 139
              MOVE "139 - I - SEM REGISTRO EMISSÃO PARCIAL SEGUROS COM I
      -            "OF 4%" TO GS-ACP-I-CARTEIRA
              WHEN 142
              MOVE "142 - I - SEM REGISTRO SEM EMISSÃO 15 DÍGITOS IOF 4%
      -            " " TO GS-ACP-I-CARTEIRA
              WHEN 143
              MOVE "143 - I - SEM REGISTRO SEM EMISSÃO 15 DÍGITOS COM IO
      -            "F 7%" TO GS-ACP-I-CARTEIRA
              WHEN 147
              MOVE "147 - E - ESCRITURAL ELETRÔNICA DÓLAR"
                TO GS-ACP-I-CARTEIRA
              WHEN 150
              MOVE "150 - U - DIRETA ELETRÔNICA SEM EMISSÃO DÓLAR"
                TO GS-ACP-I-CARTEIRA
              WHEN 169
              MOVE "169 - I - SEM REGISTRO EMISSÃO PARCIAL SEGUROS COM I
      -            "OF 7%" TO GS-ACP-I-CARTEIRA
              WHEN 172
              MOVE "172 - I - SEM REGISTRO COM EMISSÃO INTEGRAL"
                TO GS-ACP-I-CARTEIRA
              WHEN 173
              MOVE "173 - I - SEM REGISTRO COM EMISSÃO E ENTREGA"
                TO GS-ACP-I-CARTEIRA
              WHEN 174
              MOVE " 174 - I - SEM REGISTRO EMISSÃO PARCIAL COM PROTESTO
      -            " BORDERÔ" TO GS-ACP-I-CARTEIRA
              WHEN 175
              MOVE "175 - I - SEM REGISTRO SEM EMISSÃO COM PROTESTO ELET
      -            "RÔNICO" TO GS-ACP-I-CARTEIRA
              WHEN 177
              MOVE "177 - I - SEM REGISTRO EMISSÃO PARCIAL COM PROTESTO
      -            "ELETRÔNICO" TO GS-ACP-I-CARTEIRA
              WHEN 180
              MOVE "180 - I - DIRETA ELETRÔNICA EMISSÃO INTEGRAL SIMPLES
      -            " " TO GS-ACP-I-CARTEIRA
              WHEN 188
              MOVE "188 - I - ESCRITURAL ELETRÔNICA - COBRANÇA INTELIGEN
      -            "TE (B2B)" TO GS-ACP-I-CARTEIRA
              WHEN 196
              MOVE "196 - I - SEM REGISTRO COM EMISSÃO E ENTREGA 15 POSI
      -            "ÇÕES" TO GS-ACP-I-CARTEIRA
              WHEN 198
              MOVE "198 - I - SEM REGISTRO SEM EMISSÃO 15 DÍGITOS"
                TO GS-ACP-I-CARTEIRA
           END-EVALUATE

           INITIALIZE GS-ACP-I-ESPECIE
           EVALUATE BOLITAU-ESPECIE
              when "01"
                 MOVE "01 - DUPLICATA MERCANTIL"   TO GS-ACP-I-ESPECIE
              when "02"
                 MOVE "02 - NOTA PROMISSORIA"      TO GS-ACP-I-ESPECIE
              when "03"
                 MOVE "03 - NOTA DE SEGURO"        TO GS-ACP-I-ESPECIE
              when "04"
                 MOVE "04 - MENSALIDADE ESCOLAR"   TO GS-ACP-I-ESPECIE
              when "05"
                 MOVE "05 - RECIBO"                TO GS-ACP-I-ESPECIE
              when "06"
                 MOVE "06 - CONTRATO"              TO GS-ACP-I-ESPECIE
              when "07"
                 MOVE "07 - COSSEGUROS"            TO GS-ACP-I-ESPECIE
              when "08"
                 MOVE "08 - DUPLICATA DE SERVIÇOS" TO GS-ACP-I-ESPECIE
              when "09"
                 MOVE "09 - LETRA DE CÂMBIO"       TO GS-ACP-I-ESPECIE
              when "13"
                 MOVE "13 - NOTA DE DÉBITOS"       TO GS-ACP-I-ESPECIE
              when "15"
                 MOVE "15 - DOCUMENTO DE DÍVIDA"   TO GS-ACP-I-ESPECIE
              when "99"
                 MOVE "99 - DIVERSOS"              TO GS-ACP-I-ESPECIE
           end-evaluate

           INITIALIZE GS-ACP-I-INSTUCAO-01
           evaluate bolitau-instrucao-1
              when "02"
              MOVE "02 - / - Devolver após 05 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "03"
              MOVE "03 - / - Devolver após 30 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "05"
              MOVE "05 - / - Receber conforme instruções no próprio títu
      -            "lo" TO GS-ACP-I-INSTUCAO-01
              when "06"
              MOVE "06 - / - Devolver após 10 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "07"
              MOVE "07 - / - Devolver após 15 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "08"
              MOVE "08 - / - Devolver após 20 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "09"
              MOVE "09 - A - Protestar (emite aviso após XX dias do venc
      -            "to, e envia ao cartório após 5 dias úteis)"
                TO GS-ACP-I-INSTUCAO-01
              when "10"
              MOVE "10 - G - Não protestar (inibe protesto, quando houve
      -            "r instrução permanente na conta corrente)"
                TO GS-ACP-I-INSTUCAO-01
              when "11"
              MOVE "11 - / - Devolver após 25 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "12"
              MOVE "12 - / - Devolver após 35 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "13"
              MOVE "13 - / - Devolver após 40 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "14"
              MOVE "14 - / - Devolver após 45 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "15"
              MOVE "15 - / - Devolver após 50 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "16"
              MOVE "16 - / - Devolver após 55 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "17"
              MOVE "17 - / - Devolver após 60 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "18"
              MOVE "18 - / - Devolver  após 90 dias do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "19"
              MOVE "19 - / - Não Receber após 05 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "20"
              MOVE "20 - / - Não Receber após 10 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "21"
              MOVE "21 - / - Não Receber após 15 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "22"
              MOVE "22 - / - Não Receber após 20 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "23"
              MOVE "23 - / - Não Receber após 25 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "24"
              MOVE "24 - / - Não Receber após 30 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "25"
              MOVE "25 - / - Não Receber após 35 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "26"
              MOVE "26 - / - Não Receber após 40 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "27"
              MOVE "27 - / - Não Receber após 45 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "28"
              MOVE "28 - / - Não Receber após 50 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "29"
              MOVE "29 - / - Não Receber após 55 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "30"
              MOVE "30 - E - Importância de desconto por dia"
                TO GS-ACP-I-INSTUCAO-01
              when "31"
              MOVE "31 - / - Não Receber após 60 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "32"
              MOVE "32 - / - Não Receber após 90 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "33"
              MOVE "33 - I - Conceder abatimento ref. à pis-pasep/cofin/
      -            "cssl, mesmo após vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "37"
              MOVE "37 - / - Receber até o último dia do mês de vencimen
      -            "to"  TO GS-ACP-I-INSTUCAO-01
              when "38"
              MOVE "38 - / - Conceder desconto mesmo após vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "39"
              MOVE "39 - / - Não Receber após o vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "40"
              MOVE "40 - / - Conceder desconto conforme Nota de Crédito"
                TO GS-ACP-I-INSTUCAO-01
              when "42"
              MOVE "42 - A - Protesto para fins falimentares"
                TO GS-ACP-I-INSTUCAO-01
              when "43"
              MOVE "43 - / - Sujeito a protesto se não for pago no venci
      -            "mento"
                TO GS-ACP-I-INSTUCAO-01
              when "44"
              MOVE "44 - F - Importância por dia de atraso a partir de d
      -            "dmmaa"
                TO GS-ACP-I-INSTUCAO-01
              when "45"
              MOVE "45 - / - Tem dia da Graça"
                TO GS-ACP-I-INSTUCAO-01
              when "47"
              MOVE "47 - / - Dispensar juros/comissão de permanência"
                TO GS-ACP-I-INSTUCAO-01
              when "51"
              MOVE "51 - / - Receber somente com a parcela anterior quit
      -            "ada"
                TO GS-ACP-I-INSTUCAO-01
              when "52"
              MOVE "52 - / - Favor efetuar pgto  somente através desta c
      -            "obrança bancária"  TO GS-ACP-I-INSTUCAO-01
              when "54"
              MOVE "54 - / - Após vencimento pagável somente na empresa"
                TO GS-ACP-I-INSTUCAO-01
              when "57"
              MOVE "57 - / - Somar valor do título ao valor do campo mor
      -            "a/multa caso exista"  TO GS-ACP-I-INSTUCAO-01
              when "58"
              MOVE "58 - / - Devolver após 365 dias de vencido"
                TO GS-ACP-I-INSTUCAO-01
              when "59"
              MOVE "59 - / - Cobrança negociada. pagável somente por est
      -            "e bloqueto na rede bancária" TO GS-ACP-I-INSTUCAO-01
              when "61"
              MOVE "61 - / - Título entregue em penhor em favor do ceden
      -            "te acima"  TO GS-ACP-I-INSTUCAO-01
              when "62"
              MOVE "62 - / - Título transferido a favor do cedente "
                TO GS-ACP-I-INSTUCAO-01
              when "78"
              MOVE "78 - / - Valor da ida engloba multa de 10% pro rata"
                TO GS-ACP-I-INSTUCAO-01
              when "79"
              MOVE "79 - / - Cobrar juros após 15 dias da emissão (para
      -            "títulos com vencimento à vista)"
                TO GS-ACP-I-INSTUCAO-01
              when "80"
              MOVE "80 - / - Pagamento em cheque: somente receber com ch
      -            "eque de emissão do sacado"  TO GS-ACP-I-INSTUCAO-01
              when "81"
              MOVE "81 - A - Protestar após xx dias corridos do vencimen
      -            "to"  TO GS-ACP-I-INSTUCAO-01
              when "82"
              MOVE "82 - A - Protestar após xx DIAS úteis do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "83"
              MOVE "83 - / - Operação ref a vendor"
                TO GS-ACP-I-INSTUCAO-01
              when "84"
              MOVE "84 - / - Após vencimento consultar a Agência cedente
      -            " "  TO GS-ACP-I-INSTUCAO-01
              when "86"
              MOVE "86 - / - Antes do Vencimento ou após 15 dias, pagáve
      -            "l SOMENTE em nossa sede"  TO GS-ACP-I-INSTUCAO-01
              when "88"
              MOVE "88 - / - Não receber antes do Vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "90"
              MOVE "90 - / - No vencimento pagável em qualquer agência b
      -            "ancária" TO GS-ACP-I-INSTUCAO-01
              when "91"
              MOVE "91 - A - Não receber após xx dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "92"
              MOVE "92 - A - Devolver após xx dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
              when "93"
              MOVE "93 - B - Mensagens nos bloquetos com 30 posições"
                TO GS-ACP-I-INSTUCAO-01
              when "94"
              MOVE "94 - C - Mensagens nos bloquetos com 40 posições"
                TO GS-ACP-I-INSTUCAO-01
              when "98"
              MOVE "98 - D - Duplicata / Fatura Nº"
                TO GS-ACP-I-INSTUCAO-01
           END-EVALUATE

           INITIALIZE GS-ACP-I-INSTRUCAO-02
           evaluate bolitau-instrucao-2
              when "02"
              MOVE "02 - / - Devolver após 05 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "03"
              MOVE "03 - / - Devolver após 30 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "05"
              MOVE "05 - / - Receber conforme instruções no próprio títu
      -            "lo" TO GS-ACP-I-INSTRUCAO-02
              when "06"
              MOVE "06 - / - Devolver após 10 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "07"
              MOVE "07 - / - Devolver após 15 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "08"
              MOVE "08 - / - Devolver após 20 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "09"
              MOVE "09 - A - Protestar (emite aviso após XX dias do venc
      -            "to, e envia ao cartório após 5 dias úteis)"
                TO GS-ACP-I-INSTRUCAO-02
              when "10"
              MOVE "10 - G - Não protestar (inibe protesto, quando houve
      -            "r instrução permanente na conta corrente)"
                TO GS-ACP-I-INSTRUCAO-02
              when "11"
              MOVE "11 - / - Devolver após 25 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "12"
              MOVE "12 - / - Devolver após 35 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "13"
              MOVE "13 - / - Devolver após 40 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "14"
              MOVE "14 - / - Devolver após 45 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "15"
              MOVE "15 - / - Devolver após 50 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "16"
              MOVE "16 - / - Devolver após 55 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "17"
              MOVE "17 - / - Devolver após 60 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "18"
              MOVE "18 - / - Devolver  após 90 dias do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "19"
              MOVE "19 - / - Não Receber após 05 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "20"
              MOVE "20 - / - Não Receber após 10 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "21"
              MOVE "21 - / - Não Receber após 15 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "22"
              MOVE "22 - / - Não Receber após 20 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "23"
              MOVE "23 - / - Não Receber após 25 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "24"
              MOVE "24 - / - Não Receber após 30 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "25"
              MOVE "25 - / - Não Receber após 35 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "26"
              MOVE "26 - / - Não Receber após 40 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "27"
              MOVE "27 - / - Não Receber após 45 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "28"
              MOVE "28 - / - Não Receber após 50 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "29"
              MOVE "29 - / - Não Receber após 55 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "30"
              MOVE "30 - E - Importância de desconto por dia"
                TO GS-ACP-I-INSTRUCAO-02
              when "31"
              MOVE "31 - / - Não Receber após 60 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "32"
              MOVE "32 - / - Não Receber após 90 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "33"
              MOVE "33 - I - Conceder abatimento ref. à pis-pasep/cofin/
      -            "cssl, mesmo após vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "37"
              MOVE "37 - / - Receber até o último dia do mês de vencimen
      -            "to"  TO GS-ACP-I-INSTRUCAO-02
              when "38"
              MOVE "38 - / - Conceder desconto mesmo após vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "39"
              MOVE "39 - / - Não Receber após o vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "40"
              MOVE "40 - / - Conceder desconto conforme Nota de Crédito"
                TO GS-ACP-I-INSTRUCAO-02
              when "42"
              MOVE "42 - A - Protesto para fins falimentares"
                TO GS-ACP-I-INSTRUCAO-02
              when "43"
              MOVE "43 - / - Sujeito a protesto se não for pago no venci
      -            "mento"
                TO GS-ACP-I-INSTRUCAO-02
              when "44"
              MOVE "44 - F - Importância por dia de atraso a partir de d
      -            "dmmaa"
                TO GS-ACP-I-INSTRUCAO-02
              when "45"
              MOVE "45 - / - Tem dia da Graça"
                TO GS-ACP-I-INSTRUCAO-02
              when "47"
              MOVE "47 - / - Dispensar juros/comissão de permanência"
                TO GS-ACP-I-INSTRUCAO-02
              when "51"
              MOVE "51 - / - Receber somente com a parcela anterior quit
      -            "ada"
                TO GS-ACP-I-INSTRUCAO-02
              when "52"
              MOVE "52 - / - Favor efetuar pgto  somente através desta c
      -            "obrança bancária"  TO GS-ACP-I-INSTRUCAO-02
              when "54"
              MOVE "54 - / - Após vencimento pagável somente na empresa"
                TO GS-ACP-I-INSTRUCAO-02
              when "57"
              MOVE "57 - / - Somar valor do título ao valor do campo mor
      -            "a/multa caso exista"  TO GS-ACP-I-INSTRUCAO-02
              when "58"
              MOVE "58 - / - Devolver após 365 dias de vencido"
                TO GS-ACP-I-INSTRUCAO-02
              when "59"
              MOVE "59 - / - Cobrança negociada. pagável somente por est
      -            "e bloqueto na rede bancária"
                TO GS-ACP-I-INSTRUCAO-02
              when "61"
              MOVE "61 - / - Título entregue em penhor em favor do ceden
      -            "te acima"  TO GS-ACP-I-INSTRUCAO-02
              when "62"
              MOVE "62 - / - Título transferido a favor do cedente "
                TO GS-ACP-I-INSTRUCAO-02
              when "78"
              MOVE "78 - / - Valor da ida engloba multa de 10% pro rata"
                TO GS-ACP-I-INSTRUCAO-02
              when "79"
              MOVE "79 - / - Cobrar juros após 15 dias da emissão (para
      -            "títulos com vencimento à vista)"
                TO GS-ACP-I-INSTRUCAO-02
              when "80"
              MOVE "80 - / - Pagamento em cheque: somente receber com ch
      -            "eque de emissão do sacado"  TO GS-ACP-I-INSTRUCAO-02
              when "81"
              MOVE "81 - A - Protestar após xx dias corridos do vencimen
      -            "to"  TO GS-ACP-I-INSTRUCAO-02
              when "82"
              MOVE "82 - A - Protestar após xx DIAS úteis do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "83"
              MOVE "83 - / - Operação ref a vendor"
                TO GS-ACP-I-INSTRUCAO-02
              when "84"
              MOVE "84 - / - Após vencimento consultar a Agência cedente
      -            " "  TO GS-ACP-I-INSTRUCAO-02
              when "86"
              MOVE "86 - / - Antes do Vencimento ou após 15 dias, pagáve
      -            "l SOMENTE em nossa sede"  TO GS-ACP-I-INSTRUCAO-02
              when "88"
              MOVE "88 - / - Não receber antes do Vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "90"
              MOVE "90 - / - No vencimento pagável em qualquer agência b
      -            "ancária" TO GS-ACP-I-INSTRUCAO-02
              when "91"
              MOVE "91 - A - Não receber após xx dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "92"
              MOVE "92 - A - Devolver após xx dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
              when "93"
              MOVE "93 - B - Mensagens nos bloquetos com 30 posições"
                TO GS-ACP-I-INSTRUCAO-02
              when "94"
              MOVE "94 - C - Mensagens nos bloquetos com 40 posições"
                TO GS-ACP-I-INSTRUCAO-02
              when "98"
              MOVE "98 - D - Duplicata / Fatura Nº"
                TO GS-ACP-I-INSTRUCAO-02
           end-evaluate

           MOVE BOLITAU-INSTRUCAO1      TO GS-ACP-I-INSTRUCAO1
           MOVE BOLITAU-INSTRUCAO2      TO GS-ACP-I-INSTRUCAO2
           MOVE BOLITAU-INSTRUCAO3      TO GS-ACP-I-INSTRUCAO3
           MOVE BOLITAU-INSTRUCAO4      TO GS-ACP-I-INSTRUCAO4
           MOVE BOLITAU-INSTRUCAO5      TO GS-ACP-I-INSTRUCAO5

           MOVE BOLITAU-LOCAL1          TO GS-ACP-I-LOCAL1
           MOVE BOLITAU-LOCAL2          TO GS-ACP-I-LOCAL2

           MOVE BOLITAU-PORTADOR        TO GS-ACP-PORTADOR
                                           PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*******"          TO NOME-PORT
           END-READ
           MOVE NOME-PORT               TO GS-DESC-PORTADOR

           REFRESH-OBJECT WIN1.


       GRAVAR SECTION.
           INITIALIZE REG-BOLITAU

           MOVE 1                       TO BOLITAU-CODIGO
           MOVE GS-ACP-I-CODBAN         TO BOLITAU-NUM-BANCO
           MOVE GS-ACP-I-NOMEBAN        TO BOLITAU-NOME-BANCO
           MOVE GS-ACP-I-AGENCIA        TO BOLITAU-AGENCIA
           MOVE GS-ACP-I-DIG-AGENCIA    TO BOLITAU-DIG-AGENCIA
           MOVE GS-ACP-I-CONTA          TO BOLITAU-CONTA
           MOVE GS-ACP-I-DIG-CONTA      TO BOLITAU-DIG-CONTA
           MOVE SPACES                  TO BOLITAU-EMPRESA
           MOVE GS-ACP-I-CNPJ           TO BOLITAU-CNPJ
           MOVE GS-ACP-I-NOSSO-NUMERO   TO BOLITAU-NOSSO-NUM
           MOVE GS-ACP-I-DIAS-INSTRUCAO TO BOLITAU-QTDE-DIAS
           MOVE GS-ACP-I-JUROS-1-DIA    TO BOLITAU-JUROS-1-DIA
           MOVE GS-ACP-I-DESCONTO       TO BOLITAU-DESCONTO
           MOVE GS-ACP-I-MULTA          TO BOLITAU-MULTA
           MOVE GS-ACP-I-DIAS-DESCONTO  TO BOLITAU-DIAS-DESCONTO

           MOVE GS-ACP-I-CARTEIRA(1:3)  TO BOLITAU-CARTEIRA
           MOVE GS-ACP-I-CARTEIRA(7:1)  TO BOLITAU-COD-CARTEIRA

           MOVE GS-ACP-I-ESPECIE(1:2)   TO BOLITAU-ESPECIE

           MOVE GS-ACP-I-INSTUCAO-01(1:2)  TO BOLITAU-INSTRUCAO-1
           MOVE GS-ACP-I-INSTRUCAO-02(1:2) TO BOLITAU-INSTRUCAO-2

           MOVE GS-ACP-I-INSTRUCAO1      TO BOLITAU-INSTRUCAO1
           MOVE GS-ACP-I-INSTRUCAO2      TO BOLITAU-INSTRUCAO2
           MOVE GS-ACP-I-INSTRUCAO3      TO BOLITAU-INSTRUCAO3
           MOVE GS-ACP-I-INSTRUCAO4      TO BOLITAU-INSTRUCAO4
           MOVE GS-ACP-I-INSTRUCAO5      TO BOLITAU-INSTRUCAO5

           MOVE GS-ACP-I-LOCAL1          TO BOLITAU-LOCAL1
           MOVE GS-ACP-I-LOCAL2          TO BOLITAU-LOCAL2

           MOVE GS-ACP-PORTADOR          TO BOLITAU-PORTADOR

           WRITE REG-BOLITAU INVALID KEY
                 REWRITE REG-BOLITAU INVALID KEY
                      MOVE "Erro de Regravação...BOLITAU" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GERA-ARQ-REMESSA SECTION.
           MOVE FUNCTION NUMVAL(GS-TIPO-DOCTO(1:1)) TO AUX-TIPO
           MOVE FUNCTION NUMVAL(GS-CARTEIRA(1:1))   TO AUX-CARTEIRA


           MOVE "CLEAR-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-VENCTO-INI     TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-INI-INV
           MOVE GS-VENCTO-FIM     TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-FIM-INV


           MOVE GS-MOVTO-INI      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO MOVTO-INI-INV.
           MOVE GS-MOVTO-FIM      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO MOVTO-FIM-INV.


           MOVE 1 TO BOLITAU-CODIGO
           READ BOLITAU INVALID KEY
                MOVE "Parâmetrização Não Realizada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                PERFORM MOVER-DADOS
                MOVE BOLITAU-NOSSO-NUM   TO SEQUENCIA-W

                MOVE SPACES              TO ARQUIVO-REMES
                STRING "\PROGRAMA\KELLO\TEMP\" GS-NOME-ARQ-REMESSA
                  INTO ARQUIVO-REMES
                OPEN OUTPUT REMES


                INITIALIZE GS-QTDE-TITULO
                           GS-VALOR-TOTAL
                           REG-CRD020
                MOVE ZEROS TO VALOR-TOTAL SEQ-W
                PERFORM MOVER-DADOS-TIPO0
                IF GS-ALBUM > 0
                   IF GS-CONTRATO = 0
                   STRING "9" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
                   MOVE COD-COMPL-CR20 TO COD-COMPL-CR20-W
                   ELSE
                   STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
                   MOVE COD-COMPL-CR20 TO COD-COMPL-CR20-W
                   END-IF
                   START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                        MOVE "10" TO ST-CRD020
                   END-START

                   PERFORM UNTIL ST-CRD020 = "10"
                        READ CRD020 NEXT WITH IGNORE LOCK AT END
                            MOVE "10" TO ST-CRD020
                        NOT AT END
                            IF COD-COMPL-CR20 <> COD-COMPL-CR20-W
                               MOVE "10" TO ST-CRD020
                            ELSE
                                MOVE REG-CRD020 TO GS-REGISTRO1
                                IF SITUACAO-CR20 = 0
                                   IF MOVTO-INI-INV = 0 OR
                                     (DATA-MOVTO-CR20 NOT <
                                      MOVTO-INI-INV AND
                                      DATA-MOVTO-CR20 NOT >
                                      MOVTO-FIM-INV)
                                      IF VENCTO-INI-INV = 0 OR
                                         (DATA-VENCTO-CR20 NOT <
                                          VENCTO-INI-INV AND
                                          DATA-VENCTO-CR20 NOT >
                                          VENCTO-FIM-INV)
                                         PERFORM
                                                FAZER-OUTRAS-COMPARACOES
                                      END-IF
                                   END-IF
                                END-IF
                                MOVE "REFRESH-DATA" TO DS-PROCEDURE
                                PERFORM CALL-DIALOG-SYSTEM
                            END-IF
                        END-READ
                   END-PERFORM
                ELSE
                   IF GS-CONTRATO > 0
                      STRING "0" GS-CONTRATO GS-ALBUM INTO
                                                          COD-COMPL-CR20
                      START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID
                                                                     KEY
                           MOVE "10" TO ST-CRD020
                      END-START
                      PERFORM UNTIL ST-CRD020 = "10"
                          READ CRD020 NEXT WITH IGNORE LOCK AT END
                               MOVE "10" TO ST-CRD020
                          NOT AT END
                               IF CLIENTE-CR20(1:4) <> GS-CONTRATO
                                  MOVE "10" TO ST-CRD020
                               ELSE
                                  MOVE REG-CRD020 TO GS-REGISTRO1
                                  IF SITUACAO-CR20 = 0
                                     IF MOVTO-INI-INV = 0 OR
                                        (DATA-MOVTO-CR20 NOT <
                                         MOVTO-INI-INV AND
                                         DATA-MOVTO-CR20 NOT >
                                         MOVTO-FIM-INV)
                                         IF VENCTO-INI-INV = 0 OR
                                            (DATA-VENCTO-CR20 NOT <
                                             VENCTO-INI-INV AND
                                             DATA-VENCTO-CR20 NOT >
                                             VENCTO-FIM-INV)
                                             PERFORM
                                                FAZER-OUTRAS-COMPARACOES
                                          END-IF
                                      END-IF
                                  END-IF
                                  MOVE "REFRESH-DATA" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                               END-IF
                          END-READ
                      END-PERFORM
                   ELSE
                      IF GS-MOVTO-INI > 0
                         MOVE ZEROS TO SITUACAO-CR20
                         MOVE MOVTO-INI-INV TO DATA-MOVTO-CR20
                         START CRD020 KEY IS NOT LESS ALT3-CR20 INVALID
                                                                     KEY
                               MOVE "10" TO ST-CRD020
                         END-START

                         PERFORM UNTIL ST-CRD020 = "10"
                             READ CRD020 NEXT WITH IGNORE LOCK AT END
                                  MOVE "10" TO ST-CRD020
                             NOT AT END
                                  IF DATA-MOVTO-CR20 > MOVTO-FIM-INV OR
                                     SITUACAO-CR20 <> 0
                                     MOVE "10" TO ST-CRD020
                                  ELSE
                                     MOVE REG-CRD020 TO GS-REGISTRO1
                                     IF VENCTO-INI-INV = 0 OR
                                        (DATA-VENCTO-CR20 NOT <
                                         VENCTO-INI-INV AND
                                         DATA-VENCTO-CR20 NOT >
                                         VENCTO-FIM-INV)
                                         PERFORM
                                                FAZER-OUTRAS-COMPARACOES
                                     END-IF
                                     MOVE "REFRESH-DATA" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                                  END-IF
                             END-READ
                         END-PERFORM
                      ELSE
                         MOVE ZEROS TO SITUACAO-CR20
                         MOVE VENCTO-INI-INV TO DATA-VENCTO-CR20
                         START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID
                                                                     KEY
                               MOVE "10" TO ST-CRD020
                         END-START

                         PERFORM UNTIL ST-CRD020 = "10"
                             READ CRD020 NEXT WITH IGNORE LOCK AT END
                                  MOVE "10" TO ST-CRD020
                             NOT AT END
                                  IF DATA-VENCTO-CR20 > VENCTO-FIM-INV
                                     MOVE "10" TO ST-CRD020
                                  ELSE
                                     MOVE REG-CRD020 TO GS-REGISTRO1
                                     IF SITUACAO-CR20 = 0
                                        IF MOVTO-INI-INV = 0 OR
                                           (DATA-MOVTO-CR20 NOT <
                                            MOVTO-INI-INV AND
                                            DATA-MOVTO-CR20 NOT >
                                            MOVTO-FIM-INV)
                                            PERFORM
                                                FAZER-OUTRAS-COMPARACOES
                                        END-IF
                                     END-IF
                                     MOVE "REFRESH-DATA" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                                  END-IF
                             END-READ
                         END-PERFORM
                      END-IF
                   END-IF
                END-IF
      *         MOVE ZEROS          TO GS-SEQ
      *         MOVE "REFRESH-DATA" TO DS-PROCEDURE
      *         PERFORM CALL-DIALOG-SYSTEM
      *         CLOSE REMES
      *         PERFORM CARREGA-LISTA
      *         REFRESH-OBJECT PRINCIPAL.

                MOVE ZEROS          TO GS-SEQ
                MOVE "REFRESH-DATA" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                PERFORM MOVER-DADOS-TIPO4
                CLOSE REMES
                PERFORM CARREGA-LISTA
                REFRESH-OBJECT PRINCIPAL.











































      *         INITIALIZE REG-CRD020
      *         MOVE VENCTO-INI-INV      TO DATA-VENCTO-CR20
      *         MOVE ZEROS               TO SITUACAO-CR20
      *                                     COD-COMPL-CR20
      *         START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
      *               MOVE "10" TO ST-CRD020
      *         END-START
      *         INITIALIZE GS-QTDE-TITULO
      *                    GS-VALOR-TOTAL
      *         MOVE ZEROS TO VALOR-TOTAL SEQ-W
      *         PERFORM MOVER-DADOS-TIPO0
      *         PERFORM UNTIL ST-CRD020 = "10"
      *              READ CRD020 NEXT RECORD AT END
      *                   MOVE "10" TO ST-CRD020
      *              NOT AT END
      *                   IF SITUACAO-CR20 > 0 OR
      *                      DATA-VENCTO-CR20 > VENCTO-FIM-INV
      *                      MOVE "10" TO ST-CRD020
      *                   ELSE
      *                      MOVE REG-CRD020 TO GS-REGISTRO1
      *                      IF PORTADOR-CR20 = GS-PORTADOR
      *                         IF GS-CONTRATO = 0 OR CLIENTE-CR20(1:4)
      *                            IF GS-ALBUM = 0 OR CLIENTE-CR20(5:4)
      *                               IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
      *                                 PERFORM FAZER-OUTRAS-COMPARACOES
      *                               END-IF
      *                            END-IF
      *                         END-IF
      *                      END-IF
      *                      MOVE "REFRESH-DATA" TO DS-PROCEDURE
      *                      PERFORM CALL-DIALOG-SYSTEM
      *                   END-IF
      *              END-READ
      *         END-PERFORM
      *         MOVE ZEROS          TO GS-SEQ
      *         MOVE "REFRESH-DATA" TO DS-PROCEDURE
      *         PERFORM CALL-DIALOG-SYSTEM
      *         PERFORM MOVER-DADOS-TIPO4
      *         CLOSE REMES
      *         PERFORM CARREGA-LISTA
      *         REFRESH-OBJECT PRINCIPAL.

       FAZER-OUTRAS-COMPARACOES SECTION.
           IF GS-VENDEDOR = 0 OR VENDEDOR-CR20
              IF PORTADOR-CR20 = GS-PORTADOR
                 IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                    IF AUX-CARTEIRA = 0 OR CARTEIRA-CR20
                       MOVE SEQ-W          TO GS-SEQ
                       MOVE "REFRESH-DATA" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       PERFORM MOVER-DADOS-TIPO1.

       ATUALIZA-PORTADOR-RECEBER SECTION.
           CLOSE    CRD020
           OPEN I-O CRD020
           OPEN INPUT REMES
           PERFORM ABRE-ARQUIVO-ANOTACAO
           MOVE ZEROS TO ST-REM
           PERFORM UNTIL ST-REM = "10"
                READ REMES AT END
                     MOVE "10" TO ST-REM
                NOT AT END
                     MOVE REG-REMES(395: 6) TO GS-EXIBE-SEQ
                     MOVE "REFRESH-WIN3" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     MOVE REG-REMES(1: 1)   TO TIPO-W
                     EVALUATE TIPO-W
                        WHEN 1 MOVE REG-REMES
                                 TO DETALHE-ARQUIVO
                               MOVE D-USO-EMPRESA(2:9)
                                 TO COD-COMPL-CR20
                               MOVE D-USO-EMPRESA(11:5)
                                 TO SEQ-CR20
                               MOVE D-CARTEIRA TO CARTEIRA-CR20
                               READ CRD020 INVALID KEY
                                    STRING "Contas a Receber Não Encontr
      -                                    "ado" X"0DA0"
                                           "D-USO-EMPRESA => "
                                            D-USO-EMPRESA INTO MENSAGEM
                                      MOVE "C" TO TIPO-MSG
                                    PERFORM EXIBIR-MENSAGEM
                               NOT INVALID KEY
                                    PERFORM GRAVA-ANOTACAO
                                    MOVE GS-ACP-PORTADOR TO
                                         PORTADOR-CR20
                                    REWRITE REG-CRD020
                                    END-REWRITE
                               END-READ
                     END-EVALUATE
                END-READ
           END-PERFORM
           CLOSE      CRD020
           OPEN INPUT CRD020
           MOVE "Portador Atualizado no Contas a Receber" TO MENSAGEM
           MOVE "C"           TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM

           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMES.
       ATUALIZA-SEQUENCIA SECTION.
           MOVE 1 TO BOLITAU-CODIGO
           READ BOLITAU INVALID KEY
                MOVE "Parâmetrização Não Realizada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE SEQUENCIA-W TO BOLITAU-NOSSO-NUM
                REWRITE REG-BOLITAU INVALID KEY
                    MOVE "Erro de Regravação...BOLITAU" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                NOT INVALID KEY
                    MOVE "Nosso Número Atualizado com Sucesso" TO
                    MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ.

       ABRE-ARQUIVO-ANOTACAO SECTION.
           OPEN I-O CRD200 CRD201.
           IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
                                CLOSE CRD200  OPEN I-O    CRD200
           IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
                                CLOSE CRD201  OPEN I-O    CRD201.

           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

       GRAVA-ANOTACAO SECTION.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
                 READ CRD200 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD200
                 NOT AT END
                      IF COD-COMPL-CR200 <> COD-COMPL-CR20
                         MOVE "10" TO ST-CRD200
                      ELSE
                         MOVE SEQ-CR200 TO ULT-SEQ
                         CONTINUE
                 END-READ
           END-PERFORM
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ        TO SEQ-CR200.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200.
           MOVE ZEROS          TO DATA-RETORNO-CR200
           MOVE USUARIO-W      TO USUARIO-CR200
           MOVE DATA-DIA-I     TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
                WRITE REG-CRD200 INVALID KEY
                      ADD 1     TO SEQ-CR200
                      CONTINUE
                NOT INVALID KEY
                      MOVE "10" TO ST-CRD200
                END-WRITE
           END-PERFORM.

           MOVE SEQ-CR200             TO SEQ-CR201.
           MOVE COD-COMPL-CR20        TO COD-COMPL-CR201.
           MOVE "TRANSF.PORTADOR- DOCTO: XXXXXXXXXX - 01-XXXXXXXXXXXXXXX
      -    "X P/ 99-XXXXXXXXXXXXXXXX" TO ANOTACAO-CR201.
           MOVE NR-DOCTO-CR20         TO ANOTACAO-CR201(25: 11)
           MOVE PORTADOR-CR20         TO ANOTACAO-CR201(38: 4) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES           TO NOME-PORT
           END-READ
           MOVE NOME-PORT             TO ANOTACAO-CR201(43: 16)
           MOVE GS-ACP-PORTADOR       TO ANOTACAO-CR201(63: 4) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES           TO NOME-PORT
           END-READ
           MOVE NOME-PORT             TO ANOTACAO-CR201(68: 16)
           MOVE ZEROS                 TO ST-CRD201
           MOVE 1                     TO SUBSEQ-CR201
           PERFORM UNTIL ST-CRD201 = "10"
                WRITE REG-CRD201 INVALID KEY
                      ADD 1 TO SUBSEQ-CR201
                      CONTINUE
                NOT INVALID KEY
                      MOVE "10" TO ST-CRD201
                END-WRITE
           END-PERFORM.

       MOVER-DADOS-TIPO0 SECTION.
           MOVE 1                        TO SEQ-W

           INITIALIZE HEADER-ARQUIVO

           MOVE BOLITAU-AGENCIA         TO H-AGENCIA
           MOVE BOLITAU-CONTA           TO H-CONTA
           MOVE BOLITAU-DIG-CONTA       TO H-DAC

           MOVE EMPRESA-W                TO CODIGO-CA001
           READ CAD001 INVALID KEY
                MOVE "**********"        TO NOME-EMP-CA001
           END-READ

           MOVE NOME-EMP-CA001           TO H-NOME-EMPRESA

           STRING WS-DIA-CPU
                  WS-MES-CPU
                  WS-ANO-CPU(3:2)      INTO H-DATA-ARQUIVO

           MOVE HEADER-ARQUIVO           TO DADOS-REMES
           MOVE X"0D0A"                  TO PULA-REMES

           WRITE REG-REMES INVALID KEY
              MOVE "ERRO DE GRAVAÇÃO...LOTE-ARQUIVO"
                                         TO MENSAGEM
              MOVE "C"                   TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           MOVE ZEROS                    TO VALOR-TOTAL.

       MOVER-DADOS-TIPO1 SECTION.
      *>SEQUENCIA NOSSO NUMERO (BANCO)

           MOVE COD-COMPL-CR20         TO COD-COMPL-CG10
                                          COD-COMPL-CG11
           READ CGD010 INVALID KEY
                INITIALIZE REG-CGD010
           END-READ

           READ CGD011 INVALID KEY
                INITIALIZE REG-CGD011
           END-READ

           MOVE "S"                      TO OK

           IF GS-CPF = 1
              IF TIPO-PESSOA-CG11 = "J"
                 MOVE  CPF1-CG11 TO   LINK-CGC
                 CALL   "DIGCGC"      USING   LINKA-CGC
                 CANCEL "DIGCGC"
                 IF LINK-CGC-CONF       =       1
                    MOVE "N"                TO OK
                    MOVE SPACES             TO MENSAGEM
                    EVALUATE CLASS-CLIENTE-CR20
                        WHEN 0  STRING "Cliente com o CNPJ Inválido"
                                       " | "
                                       "Contrato" " | "
                                        CLIENTE-CR20(1:4) "-"
                                        CLIENTE-CR20(5:4) INTO MENSAGEM

                        WHEN 1  STRING "Cliente com o CNPJ Inválido"
                                       " | "
                                       "Comum" " | "
                                        CLIENTE-CR20(1:4) "-"
                                        CLIENTE-CR20(5:4) INTO MENSAGEM
                    END-EVALUATE
                    MOVE MENSAGEM TO GS-LINDET
                    MOVE "INSERE-LIST2" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
              ELSE
                 MOVE  CPF1-CG11 TO   LINK-CPF
                 CALL   "DIGCPF"      USING   LINKA-CPF
                 CANCEL "DIGCPF"
                 IF LINK-CPF-CONF       =       1
                    MOVE "N"                TO OK
                    MOVE SPACES             TO MENSAGEM
                    EVALUATE CLASS-CLIENTE-CR20
                        WHEN 0  STRING "Cliente com o CPF Inválido"
                                       " | "
                                       "Contrato" " | "
                                        CLIENTE-CR20(1:4) "-"
                                        CLIENTE-CR20(5:4) INTO MENSAGEM

                        WHEN 1  STRING "Cliente com o CPF Inválido"
                                       " | "
                                       "Comum" " | "
                                        CLIENTE-CR20(1:4) "-"
                                        CLIENTE-CR20(5:4) INTO MENSAGEM
                    END-EVALUATE
                    MOVE MENSAGEM TO GS-LINDET
                    MOVE "INSERE-LIST2" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
              END-IF
           END-IF

           IF GS-CEP = 1
              IF CEP1-CG11 < 10000000
                 MOVE "N"                TO OK
                 MOVE SPACES             TO MENSAGEM
                 EVALUATE CLASS-CLIENTE-CR20
                     WHEN 0  STRING "Cliente com o CEP Inválido" " | "
                                    "Contrato" " | "
                                     CLIENTE-CR20(1:4) "-"
                                     CLIENTE-CR20(5:4) INTO MENSAGEM

                     WHEN 1  STRING "Cliente com o CEP Inválido" " | "
                                    "Comum" " | "
                                     CLIENTE-CR20(1:4) "-"
                                     CLIENTE-CR20(5:4) INTO MENSAGEM
                 END-EVALUATE
                 MOVE MENSAGEM TO GS-LINDET
                 MOVE "INSERE-LIST2" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-IF
           END-IF

           IF GS-ENDERECO = 1
              IF ENDERECO1-CG11 = SPACES OR BAIRRO1-CG11 = SPACES
                 MOVE "N"                TO OK
                 MOVE SPACES             TO MENSAGEM
                 EVALUATE CLASS-CLIENTE-CR20
                     WHEN 0  STRING "Cliente com o ENDEREÇO ou BAIRRO In
      -                             "válido" " | "
                                    "Contrato" " | "
                                     CLIENTE-CR20(1:4) "-"
                                     CLIENTE-CR20(5:4) INTO MENSAGEM

                     WHEN 1  STRING "Cliente com o ENDEREÇO ou BAIRRO In
      -                             "válido" " | "
                                    "Comum" " | "
                                     CLIENTE-CR20(1:4) "-"
                                     CLIENTE-CR20(5:4) INTO MENSAGEM
                 END-EVALUATE
                 MOVE MENSAGEM TO GS-LINDET
                 MOVE "INSERE-LIST2" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-IF
           END-IF

           IF OK = "S"
              ADD 1                         TO GS-QTDE-TITULO
              ADD VALOR-TOT-CR20            TO GS-VALOR-TOTAL


              ADD 1                         TO SEQ-W
              MOVE DATA-VENCTO-CR20         TO WS-DATA-DESCONTO
              IF BOLITAU-DIAS-DESCONTO > 0
                 PERFORM CALCULAR-DATA-DESCONTO
              ELSE
                 INITIALIZE WS-DATA-DESCONTO
              END-IF

              INITIALIZE DETALHE-ARQUIVO
              MOVE 1                        TO D-TIPO-ARQUIVO
              MOVE 2                        TO D-TIPO-INSCRICAO
              MOVE BOLITAU-CNPJ             TO D-INSC-EMPRESA
              MOVE BOLITAU-AGENCIA          TO D-AGENCIA
              MOVE "00"                     TO D-ZERO
              MOVE BOLITAU-CONTA            TO D-CONTA-CORRENTE
              MOVE BOLITAU-DIG-CONTA        TO D-DAC
              MOVE "341"                    TO D-BANCO
              MOVE ZEROS                    TO D-CODIGO-ALEGACAO
              MOVE "X"                      TO D-USO-EMPRESA(1: 1)
              MOVE COD-COMPL-CR20           TO D-USO-EMPRESA(2: 9)
              MOVE SEQ-CR20                 TO D-USO-EMPRESA(11: 05)
              MOVE FUNCTION NUMVAL(SEQUENCIA-W)
                                            TO D-NOSSO-NUMERO
              ADD 1                         TO SEQUENCIA-W
              MOVE ZEROS                    TO D-QTDE-MOEDA
              MOVE BOLITAU-CARTEIRA         TO D-NUM-CARTEIRA
              MOVE BOLITAU-COD-CARTEIRA     TO D-CARTEIRA
              MOVE 01                       TO D-CODIGO-OCORRENCIA
              MOVE NR-DOCTO-CR20            TO D-NUMERO-DOCUMENTO
              STRING DATA-VENCTO-CR20(7:2)
                     DATA-VENCTO-CR20(5:2)
                     DATA-VENCTO-CR20(3:2)
                                         INTO D-VENCIMENTO
              MOVE VALOR-TOT-CR20          TO D-VALOR-TITULO
              ADD VALOR-W                  TO VALOR-TOTAL
              MOVE ZEROS                   TO D-AGENCIA-COBRADORA
              MOVE BOLITAU-ESPECIE         TO D-ESPECIE
              MOVE "A"                     TO D-ACEITE
              STRING DATA-EMISSAO-CR20(1:4)
                     DATA-EMISSAO-CR20(7:2)
                                         INTO D-DATA-EMISSAO

              MOVE FUNCTION NUMVAL(BOLITAU-INSTRUCAO-1) TO WS-INT
              MOVE WS-INT                               TO D-INSTRUCAO-1
              MOVE FUNCTION NUMVAL(BOLITAU-INSTRUCAO-2) TO WS-INT
              MOVE WS-INT                               TO D-INSTRUCAO-2

              COMPUTE D-JUROS-1-DIA ROUNDED = (VALOR-TOT-CR20 *
                                            (BOLITAU-JUROS-1-DIA / 100))

              IF D-INSTRUCAO-1 = 93 OR 94 OR D-INSTRUCAO-2 = 93 OR 94
                 IF BOLITAU-MULTA > 0
                    COMPUTE MASC-VALOR ROUNDED = VALOR-TOT-CR20 *
                                                 BOLITAU-MULTA / 100
                    MOVE SPACES               TO D-SACADOR-AVALISTA
                    STRING "APOS VENCTO MULTA DE R$ " MASC-VALOR
                      INTO D-SACADOR-AVALISTA
                 ELSE
                    MOVE SPACES            TO D-SACADOR-AVALISTA
                 END-IF
              ELSE
                 MOVE SPACES               TO D-SACADOR-AVALISTA
              END-IF
              MOVE BOLITAU-INSTRUCAO1      TO D-SACADOR-AVALISTA

              STRING DESCONTO-DIA
                     DESCONTO-MES
                     DESCONTO-ANO(3:2)  INTO D-DESCONTO-ATE

              COMPUTE D-VALOR-DESCONTO ROUNDED = (VALOR-TOT-CR20 *
                                               (BOLITAU-DESCONTO / 100))

              MOVE ZEROS                  TO D-VALOR-IOF
              MOVE ZEROS                  TO D-ABATIMENTO

              IF TIPO-PESSOA-CG11 = "J"
                 MOVE 02                    TO D-CODIGO-INSCRICAO
                 MOVE CPF1-CG11             TO D-NUMERO-INSCRICAO
              ELSE
                 MOVE 01                    TO D-CODIGO-INSCRICAO
                 MOVE CPF1-CG11(3: 14)      TO D-NUMERO-INSCRICAO
              END-IF
              MOVE COMPRADOR-CG10           TO D-NOME-SACADO
              MOVE ENDERECO1-CG11           TO D-LOGRADOURO
              MOVE BAIRRO1-CG11             TO D-BAIRRO
              MOVE CEP1-CG11                TO D-CEP

              MOVE CIDADE1-CG11             TO CIDADE
              READ CAD010 INVALID KEY
                   MOVE SPACES              TO NOME-CID UF-CID
              END-READ
              MOVE NOME-CID                 TO D-CIDADE
              MOVE UF-CID                   TO D-ESTADO
              STRING DATA-VENCTO-CR20(7:2)
                     DATA-VENCTO-CR20(5:2)
                     DATA-VENCTO-CR20(3:2)
                                        INTO D-DATA-MORA

              MOVE BOLITAU-QTDE-DIAS      TO D-PRAZO
              MOVE SEQ-W                  TO D-NUMERO-REMESSA
              MOVE DETALHE-ARQUIVO        TO DADOS-REMES
              MOVE X"0D0A"                TO PULA-REMES

              WRITE REG-REMES INVALID KEY
                 MOVE "ERRO DE GRAVAÇÃO...LOTE-ARQUIVO"
                                         TO MENSAGEM
                 MOVE "C"                TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              END-WRITE.

       CALCULAR-DATA-DESCONTO SECTION.
           INITIALIZE IND
           PERFORM UNTIL IND = BOLITAU-DIAS-DESCONTO
               ADD 1 TO IND

               SUBTRACT 1 FROM DESCONTO-DIA
               IF DESCONTO-DIA = 0
                  SUBTRACT 1 FROM DESCONTO-MES
                  IF DESCONTO-MES = 0
                     MOVE 12          TO DESCONTO-MES
                     SUBTRACT 1 FROM DESCONTO-ANO
                  END-IF
                  MOVE TAB-MES(DESCONTO-MES) TO DESCONTO-DIA
               END-IF
           END-PERFORM.
       CALCULAR-DATA-DESCONTO-FIM.
           EXIT.

       MOVER-DADOS-TIPO4 SECTION.
           INITIALIZE TRAILER-ARQUIVO
           MOVE 9                     TO T-TIPO-ARQUIVO
           ADD  1                     TO SEQ-W
           MOVE SEQ-W                 TO T-NUMERO-REMESSA

           MOVE TRAILER-ARQUIVO       TO DADOS-REMES
           MOVE X"0D0A"               TO PULA-REMES
           WRITE REG-REMES INVALID KEY
              MOVE "ERRO DE GRAVAÇÃO...TRAILER-ARQUIVO"
                                      TO MENSAGEM
              MOVE "C"                TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           END-WRITE.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           OPEN INPUT REMES.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO ST-REM.
           PERFORM UNTIL ST-REM = "10"
                READ REMES AT END
                     MOVE "10" TO ST-REM
                NOT AT END
                     MOVE REG-REMES        TO DETALHE-ARQUIVO
                     MOVE D-NUMERO-REMESSA TO GS-SEQ
                     MOVE "REFRESH-DATA"   TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM

                     MOVE REG-REMES         TO GS-LINDET
                     MOVE "INSERE-LIST" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMES.
       CABECALHO SECTION.
           MOVE DATA-DIA TO EMISSAO-REL.
           IF LIN = ZEROS
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB02.
           MOVE 4 TO LIN.
       IMPRIME-RELATORIO SECTION.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK
           OPEN INPUT REMES
           MOVE ZEROS TO SEQ-WK
           MOVE ZEROS TO ST-REM
           PERFORM UNTIL ST-REM = "10"
             READ REMES AT END
                  MOVE "10" TO ST-REM
             NOT AT END
                  MOVE REG-REMES (1: 1) TO TIPO-W
                  EVALUATE TIPO-W
                     WHEN 1 MOVE REG-REMES           TO DETALHE-ARQUIVO
                            ADD 1                    TO SEQ-WK
                            MOVE D-NOME-SACADO       TO NOME-WK
                            MOVE D-LOGRADOURO        TO ENDERECO-WK
                            MOVE D-BAIRRO            TO BAIRRO-WK
                            MOVE D-CIDADE            TO CIDADE-WK
                            MOVE D-ESTADO            TO UF-WK
                            MOVE D-CEP               TO CEP-WK
                            MOVE D-NUMERO-DOCUMENTO  TO DOCTO-WK
                            MOVE D-VALOR-TITULO      TO VALOR-WK
                            WRITE REG-WORK
                            END-WRITE
                  END-EVALUATE
             END-READ
           END-PERFORM.

           COPY CONDENSA.

           CLOSE       WORK
           OPEN INPUT  WORK
           MOVE ZEROS  TO LIN
           PERFORM CABECALHO
           MOVE SPACES TO NOME-WK
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      MOVE NOME-WK           TO NOME-REL
                      MOVE ENDERECO-WK       TO ENDERECO-REL
                      MOVE BAIRRO-WK         TO BAIRRO-REL
                      MOVE CIDADE-WK         TO CIDADE-REL
                      MOVE UF-WK             TO UF-REL
                      MOVE CEP-WK(1: 5)      TO CEP-REL
                      MOVE CEP-WK(6: 3)      TO SUFIXO-REL
                      MOVE DOCTO-WK          TO DOCTO-REL
                      MOVE VALOR-WK          TO VALOR-REL
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                 END-READ
           END-PERFORM.
           MOVE VALOR-TOTAL        TO VALOR-TOTAL-REL
           MOVE QTDE-TIT-T2        TO QTDE-TIT-TOTAL-REL
           WRITE REG-RELAT FROM LINDET1 AFTER 3
           CLOSE REMES WORK
           DELETE FILE WORK

           COPY DESCONDENSA.
       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR     TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*******"  TO NOME-PORT
           END-READ
           MOVE NOME-PORT       TO GS-DESCR-PORTADOR.

       POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W POP-UP
           CANCEL "CAP018T"
           MOVE POP-UP(1: 30) TO GS-DESCR-PORTADOR
           MOVE POP-UP(33: 4) TO GS-PORTADOR.

      *---------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W GS-EXIT-FLG.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "ITAU"    TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           open i-o acesso

           move 1 to acesso-codigo
           read acesso not invalid key
                delete acesso invalid key
                    move "Erro de Exclusão...ACESSO" to mensagem
                    move "C" to tipo-msg
                    perform exibir-mensagem
                end-delete
           end-read

           close    acesso


           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "ITAU"              to logacess-programa
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

           CLOSE CRD020 CGD010 CGD011 CAD010 CAD018 CAD001 CGD001.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
