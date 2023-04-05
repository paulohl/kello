       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAIXA.
       AUTHOR.        ALFREDO SAVIOLLI NETO.
      *GERA ARQUIVO XXXXXXXX.REM P/ CAIXA
       DATE-WRITTEN.  23-07-2012.
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

          $set IDXFORMAT"4" FILETYPE"4"
          SELECT CRD020x ASSIGN TO PATH-CRD020x
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD020x
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR20x = COD-COMPL-CR20x SEQ-CR20x
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR20x
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CR20x = COD-COMPL-CR20x
                        DATA-VENCTO-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR20x = SITUACAO-CR20x
                       DATA-VENCTO-CR20x COD-COMPL-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR20x = SITUACAO-CR20x
                        DATA-MOVTO-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-CR20x = SITUACAO-CR20x
                        CLIENTE-CR20x DATA-VENCTO-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-CR20x = DATA-RCTO-CR20x
                       SEQ-CAIXA-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT7-CR20x = NR-DOCTO-CR20x
                       DATA-RCTO-CR20x
                       SEQ-CAIXA-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT8-CR20x = OUTRO-DOCTO-CR20x
                       DATA-RCTO-CR20x
                       SEQ-CAIXA-CR20x WITH DUPLICATES.
           $set IDXFORMAT"0" FILETYPE"0"

           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX011.
           COPY CRPX200.
           COPY CRPX201.
           COPY BOLCAIX.SEL.
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

           select sacados assign       to arquivo-sacados
                          organization is      sequential
                          access mode  is      sequential
                          file status  is      fs-sacados.

           select auxsac  assign       to arquivo-auxsac
                          organization  is       indexed
                          access mode   is       dynamic
                          record key    is auxsac-codigo
                          file status   is     fs-auxsac.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CAPW010.
       COPY CAPW018.
       COPY CRPW020.

      * Arquivo de Movimento de contas a receber
       FD  CRD020x.
       01  REG-CRD020x.
           05  DATA-MOVTO-CR20x             PIC 9(8).
           05  COD-COMPL-CR20x.
               10  CLASS-CLIENTE-CR20x      PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR20x            PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR20x                    PIC 9(5).
           05  PORTADOR-CR20x                   PIC 9999.
           05  CARTEIRA-CR20x                   PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR20x               PIC 99.
           05  NR-DOCTO-CR20x                   PIC X(10).
           05  OUTRO-DOCTO-CR20x                PIC X(25).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR20x                 PIC 9.
      *    TIPO-DOCTO = 0-BOLETO     1-DUPL/PROMIS.     2-ORG.EVENTO
      *                 3-DEBITO AUTOMATICO 4-CARTAO CREDITO
           05  DATA-EMISSAO-CR20x               PIC 9(8).
           05  DATA-VENCTO-CR20x                PIC 9(8).
      *    DATA-VENCTO-CR20 - AAAAMMDD
           05  DESCRICAO-CR20x                  PIC X(30).
           05  SITUACAO-CR20x                   PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA 4-CANCELADA  5-DESCONTADA
      *               1-PARCIAL
           05  TIPO-MOEDA-CR20x                 PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR20x.
               10  NR-PARC-CR20x                PIC 99.
               10  TOT-PARC-CR20x               PIC 99.
           05  CODREDUZ-APUR-CR20x              PIC 9(5).
           05  VALOR-TOT-CR20x                  PIC 9(8)V99.
           05  JURO-RCTO-CR20x                  PIC 9(6)V99.
           05  MULTA-RCTO-CR20x                 PIC 9(6)V99.
           05  DESCONTO-CR20x                   PIC 9(6)V99.
           05  DATA-RCTO-CR20x                  PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR20x                  PIC 9(8)V99.
           05  CONTABILIZADO-CR20x              PIC 9.
           05  VENDEDOR-CR20x                   PIC 9(6).
           05  RESPONSAVEL-CR20x                PIC X(5).
           05  DIGITADOR-CR20x                  PIC X(5).
           05  SEQ-CAIXA-CR20x                  PIC 9(3).
           05  NR-NOTA-FISCAL-CR20x             PIC X(10).
           05  DATA-NTA-FISCAL-CR20x            PIC 9(8).
           05  FORMA-PAGTO-CR20x                PIC X(10).
           05  DCR-MEM-CR20x                    PIC X(15).
           05  CARTAO-CRED-CR20x                PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR20x      PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR20x      PIC 9(03)V99.
           05  VALOR-SALDO-CR20x                PIC 9(08)V99.
           05  FILLER                           PIC X(08).

       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CRPW200.
       COPY CRPW201.
       COPY BOLCAIX.FD.
       COPY LOGACESS.FD.

       fd remes label record is omitted.
       01 reg-remes.
           05  dados-remes      pic x(449).
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

       FD SACADOS.
       01 REG-SACADOS.
          05 SAC-CODIGO         PIC X(15).
          05 SAC-RAZAO-SOCIAL   PIC X(40).
          05 SAC-FANTASIA       PIC X(40).
          05 SAC-COD-GRUPO      PIC X(10).
          05 SAC-TIPO           PIC X(01).
          05 SAC-CPFCGC         PIC X(20).
          05 SAC-ENDERECO       PIC X(40).
          05 SAC-BAIRRO         PIC X(15).
          05 SAC-CIDADE         PIC X(15).
          05 SAC-UF             PIC X(02).
          05 SAC-CEP            PIC 9(08).
          05 SAC-FONE           PIC X(20).
          05 SAC-VALOR          PIC 9(15).
          05 SAC-DIA            PIC 9(02).
          05 SAC-SERIE          PIC 9(10).
          05 SAC-PULA           PIC X(002).

       FD AUXSAC.
       01 REG-AUXSAC.
          05 AUXSAC-CODIGO      PIC 9(09).

       WORKING-STORAGE SECTION.
           COPY "CAIXA.CPB".
           COPY "CAIXA.CPY".
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
           05 ST-CRD020x             PIC XX     VALUE SPACES.
           05 ST-CGD001              PIC XX     VALUE SPACES.
           05 ST-CGD010              PIC XX     VALUE SPACES.
           05 ST-CGD011              PIC XX     VALUE SPACES.
           05 ST-CRD200              PIC XX     VALUE SPACES.
           05 ST-CRD201              PIC XX     VALUE SPACES.
           05 FS-LOGACESS            PIC XX     VALUE SPACES.
           05 FS-BOLCAIX             PIC XX     VALUE SPACES.
           05 FS-ACESSO              PIC XX     VALUE SPACES.
           05 FS-SACADOS             PIC XX     VALUE SPACES.
           05 FS-AUXSAC              PIC XX     VALUE SPACES.
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
           05 IND2                   PIC 9(03)    VALUE ZEROS.
           05 PASSAR-STRING-1        PIC X(65).
           05 MASC-VALOR             PIC ZZ9,99   VALUE ZEROS.
           05 MOVTO-INI-INV          PIC 9(8)     VALUE ZEROS.
           05 MOVTO-FIM-INV          PIC 9(8)     VALUE ZEROS.
           05 QTDE-TIT-T2            PIC 9(8).
           05 AUX-TIPO               PIC 9(1)     VALUE ZEROS.
           05 AUX-CARTEIRA           PIC 9(1)     VALUE ZEROS.
           05 MASC-NUMERO            PIC ZZ.ZZZ.ZZ9 VALUE ZEROS.
           05 CNPJ                   PIC 9(14)    VALUE ZEROS.
           05 CPF                    PIC 9(11)    VALUE ZEROS.
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


       01  linka-cgc.
           05  link-cgc             pic 9(14).
           05  link-cgc-r redefines link-cgc.
               10  link-cgc-num     pic 9(12).
               10  link-cgc-dig1    pic 9(01).
               10  link-cgc-dig2    pic 9(01).
           05  link-cgc-conf        pic 9(01).

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

       01 DETALHE-ARQUIVO.
          05 D-CODIGO              PIC X(15).
          05 D-SEU-NUMERO          PIC X(15).
          05 D-NOSSO-NUMERO        PIC X(11).
          05 D-CARTEIRA            PIC 9(04).
          05 D-ACEITE              PIC X(01).
          05 D-ESPECIE             PIC 9(02).
          05 D-INSTRUCAO-COB       PIC 9(01).
          05 D-PRAZO-PROTESTO      PIC 9(04).
          05 D-OPCAO-VECTO         PIC 9(01).
          05 D-VECTO               PIC 9(08).
          05 D-EMISSAO             PIC 9(08).
          05 D-COD-MOEDA           PIC 9(02).
          05 D-VALOR-TITULO        PIC 9(13)V99.
          05 D-VALOR-JUROS         PIC 9(13)V99.
          05 D-DATA-DESCONTO       PIC 9(08).
          05 D-VALOR-DESCONTO      PIC 9(13)V99.
          05 D-VALOR-ABATIMEN      PIC 9(13)V99.
          05 D-DATA-MULTA          PIC 9(08).
          05 D-VALOR-MULTA         PIC 9(13)V99.
          05 D-TIPO-BLOQUETO       PIC 9(01).
          05 D-COMPENSACAO         PIC X(01).
          05 D-FILLER              PIC X(03).
          05 D-AVALISTA            PIC X(20).
          05 D-TIPO-PES            PIC X(01).
          05 D-CPF-CNPJ            PIC X(20).
          05 D-MENSAGEM1           PIC X(40).
          05 D-MENSAGEM2           PIC X(40).
          05 D-MENSAGEM3           PIC X(40).
          05 D-MENSAGEM4           PIC X(40).
          05 D-MENSAGEM5           PIC X(40).
          05 D-MENSAGEM6           PIC X(40).


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
       01 aux-valor                    pic 9(06) value zeros.
       01 aux-numero2                  pic 9(06) value zeros.
       01 resto                        pic 9(02) value zeros.
       01 parcela                      pic 9(03) value zeros.

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

       01 TABELA-NUMERO             PIC 9(10).
       01 REDEFINES TABELA-NUMERO OCCURS 10 TIMES.
          05 TAB-NUMERO             PIC 9(01).

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

           COPY IMPRESSORA.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

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
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020x.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "AUXSAC" TO ARQ-REC.  MOVE EMPRESA-REF TO ARQUIVO-AUXSAC
           MOVE "BOLCAIX"  TO ARQ-REC.  MOVE EMPRESA-REF
                                                      TO ARQUIVO-BOLCAIX
           MOVE "ACESSO"   TO ARQ-REC.  MOVE EMPRESA-REF
                                                      TO ARQUIVO-ACESSO
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CGD010 CGD011 CAD010 CAD018 CAD001 CGD001
           OPEN I-O   BOLCAIX
           CLOSE      BOLCAIX
           OPEN I-O   BOLCAIX CRD020
           CLOSE      CRD020 CRD020X
           OPEN INPUT CRD020 CRD020X

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
           move "CAIXA"             to logacess-programa
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

           move 2                      to acesso-codigo
           read acesso invalid key
                move 2                 to acesso-codigo
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

                   MOVE BOLCAIX-NOSSO-NUM       TO MASC-NUMERO
                   MOVE SPACES                  TO MENSAGEM
                   STRING "Preste Atenção na Seqüência Mostrada" x"0da0"
                          "Seqüência -> " masc-numero
                     INTO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM

               WHEN GS-IMPRIMIR-RELATORIO-TRUE
                   copy impressora.chama.
                   if lnk-mapeamento <> spaces
                      PERFORM IMPRIME-RELATORIO
                   end-if
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
               WHEN GS-SACADOS-TRUE
                    PERFORM GERAR-SACADO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       GERAR-SACADO SECTION.
           MOVE SPACES TO ARQUIVO-SACADOS
           STRING "\PROGRAMA\KELLO\TEMP\SACADOCX.TXT" INTO
                          ARQUIVO-SACADOS

           OPEN I-O    AUXSAC
           CLOSE       AUXSAC
           OPEN INPUT  AUXSAC

           OPEN OUTPUT SACADOS

           INITIALIZE REG-AUXSAC
           START AUXSAC KEY IS NOT LESS AUXSAC-CODIGO INVALID KEY
                 MOVE "10" TO FS-AUXSAC.

           PERFORM UNTIL FS-AUXSAC = "10"
                 READ AUXSAC NEXT WITH IGNORE LOCK AT END
                      MOVE "10" TO FS-AUXSAC
                 NOT AT END
                      MOVE AUXSAC-CODIGO   TO COD-COMPL-CG10
                      READ CGD010 NOT INVALID KEY
                           MOVE COD-COMPL-CG10 TO COD-COMPL-CG11
                           READ CGD011 NOT INVALID KEY
                                MOVE COD-COMPL-CG11 TO SAC-CODIGO
                                MOVE COMPRADOR-CG10 TO SAC-RAZAO-SOCIAL
                                MOVE COMPRADOR-CG10 TO SAC-FANTASIA
                                MOVE SPACES         TO SAC-COD-GRUPO
                                MOVE SPACES         TO SAC-CPFCGC
                                IF TIPO-PESSOA-CG11 = "J"
                                   MOVE "J"         TO SAC-TIPO
                                   MOVE CPF1-CG11   TO CNPJ
                                   MOVE CNPJ        TO SAC-CPFCGC
                                ELSE
                                   MOVE "F"         TO SAC-TIPO
                                   MOVE CPF1-CG11   TO CPF
                                   MOVE CPF         TO SAC-CPFCGC
                                END-IF
                                MOVE ENDERECO1-CG11 TO SAC-ENDERECO
                                MOVE BAIRRO1-CG11   TO SAC-BAIRRO
                                MOVE CIDADE1-CG11   TO CIDADE
                                READ CAD010 INVALID KEY
                                     INITIALIZE REG-CAD010
                                END-READ
                                MOVE NOME-CID       TO SAC-CIDADE
                                MOVE UF-CID         TO SAC-UF
                                MOVE CEP1-CG11      TO SAC-CEP
                                MOVE FONE1-CG11     TO SAC-FONE
                                MOVE ZEROS          TO SAC-VALOR
                                MOVE ZEROS          TO SAC-DIA
                                MOVE ZEROS          TO SAC-SERIE
                                MOVE X"0D0A"        TO SAC-PULA
                                WRITE REG-SACADOS
                           END-READ
                      END-READ
                 END-READ
           END-PERFORM

           CLOSE SACADOS AUXSAC.
       GERAR-SACADO-FIM.
           EXIT.

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
           MOVE 1                       TO BOLCAIX-CODIGO
           READ BOLCAIX INVALID KEY
                MOVE "Não Existe a Parâmetrização" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
                INITIALIZE REG-BOLCAIX.

           PERFORM MOVER-DADOS.

       MOVER-DADOS SECTION.
           MOVE BOLCAIX-NUM-BANCO       TO GS-ACP-I-CODBAN
           MOVE BOLCAIX-NOME-BANCO      TO GS-ACP-I-NOMEBAN
           MOVE BOLCAIX-AGENCIA         TO GS-ACP-I-AGENCIA
           MOVE BOLCAIX-DIG-AGENCIA     TO GS-ACP-I-DIG-AGENCIA
           MOVE BOLCAIX-CONTA           TO GS-ACP-I-CONTA
           MOVE BOLCAIX-DIG-CONTA       TO GS-ACP-I-DIG-CONTA
           MOVE BOLCAIX-CNPJ            TO GS-ACP-I-CNPJ
           MOVE BOLCAIX-NOSSO-NUM       TO GS-ACP-I-NOSSO-NUMERO
           MOVE BOLCAIX-QTDE-DIAS       TO GS-ACP-I-DIAS-INSTRUCAO
           MOVE BOLCAIX-JUROS-1-DIA     TO GS-ACP-I-JUROS-1-DIA
           MOVE BOLCAIX-DESCONTO        TO GS-ACP-I-DESCONTO
           MOVE BOLCAIX-MULTA           TO GS-ACP-I-MULTA
           MOVE BOLCAIX-DIAS-DESCONTO   TO GS-ACP-I-DIAS-DESCONTO

           INITIALIZE GS-ACP-I-CARTEIRA
           EVALUATE BOLCAIX-CARTEIRA
              WHEN "11" MOVE "11 - COBRANÇA SIMPLES" TO
                                                       GS-ACP-I-CARTEIRA
              WHEN "12" MOVE "12 - COBRANÇA RÁPIDA"  TO
                                                       GS-ACP-I-CARTEIRA
              WHEN "14" MOVE "14 - COBRANÇA SEM REGISTRO" TO
                                                       GS-ACP-I-CARTEIRA
              WHEN "41" MOVE "41 - COBRANÇA DESCONTADA"   TO
                                                       GS-ACP-I-CARTEIRA
           END-EVALUATE

           INITIALIZE GS-ACP-I-ESPECIE
           EVALUATE BOLCAIX-ESPECIE
              when "01"
                 MOVE "01 - CHEQUE"                TO GS-ACP-I-ESPECIE
              when "02"
                 MOVE "02 - DUPLICATA MERCANTIL"   TO GS-ACP-I-ESPECIE
              when "03"
                 MOVE "03 - DUPLICATA MERCANTIL PARA INDICAÇÃO"
                                                   TO GS-ACP-I-ESPECIE
              when "04"
                 MOVE "04 - DUPLICATA DE SERVIÇO"  TO GS-ACP-I-ESPECIE
              when "05"
                 MOVE "05 - DUPLICATA DE SERVIÇO PARA INDICAÇÃO"
                                                   TO GS-ACP-I-ESPECIE
              when "06"
                 MOVE "06 - DUPLICATA RURAL"       TO GS-ACP-I-ESPECIE
              when "07"
                 MOVE "07 - LETRA DE CÂMBIO"       TO GS-ACP-I-ESPECIE
              when "08"
                 MOVE "08 - NOTA DE CRÉDITO COMERCIAL"
                                                   TO GS-ACP-I-ESPECIE
              when "09"
                 MOVE "09 - NOTA DE CRÉDITO A EXPORTAÇÃO"
                                                   TO GS-ACP-I-ESPECIE
              when "10"
                 MOVE "10 - NOTA DE CRÉDITO INDÚSTRIAL"
                                                    TO GS-ACP-I-ESPECIE
              when "11"
                 MOVE "11 - NOTA DE CRÉDITO RURAL"  TO GS-ACP-I-ESPECIE
              when "12"
                 MOVE "12 - NOTA PROMISSÓRIA"       TO GS-ACP-I-ESPECIE
              when "13"
                 MOVE "13 - NOTA PROMISSÓRIA RURAL" TO GS-ACP-I-ESPECIE
              when "14"
                 MOVE "14 - TRIPLICATA MERCANTIL"   TO GS-ACP-I-ESPECIE
              when "15"
                 MOVE "15 - TRIPLICATA DE SERVIÇO"  TO GS-ACP-I-ESPECIE
              when "16"
                 MOVE "16 - NOTA DE SEGURO"         TO GS-ACP-I-ESPECIE
              when "17"
                 MOVE "17 - RECIBO"                 TO GS-ACP-I-ESPECIE
              when "18"
                 MOVE "18 - FATURA"                 TO GS-ACP-I-ESPECIE
              when "19"
                 MOVE "19 - NOTA DE DÉBITO"         TO GS-ACP-I-ESPECIE
              when "20"
                 MOVE "20 - APÓLICE DE SEGURO"      TO GS-ACP-I-ESPECIE
              when "21"
                 MOVE "21 - MENSALIDADE ESCOLAR"    TO GS-ACP-I-ESPECIE
              when "22"
                 MOVE "22 - PARCELA DE CONSÓRCIO"   TO GS-ACP-I-ESPECIE
              when "99"
                 MOVE "99 - OUTROS"                 TO GS-ACP-I-ESPECIE
           end-evaluate

           MOVE BOLCAIX-INSTRUCAO1      TO GS-ACP-I-INSTRUCAO1
           MOVE BOLCAIX-INSTRUCAO2      TO GS-ACP-I-INSTRUCAO2
           MOVE BOLCAIX-INSTRUCAO3      TO GS-ACP-I-INSTRUCAO3
           MOVE BOLCAIX-INSTRUCAO4      TO GS-ACP-I-INSTRUCAO4
           MOVE BOLCAIX-INSTRUCAO5      TO GS-ACP-I-INSTRUCAO5

           MOVE BOLCAIX-LOCAL1          TO GS-ACP-I-LOCAL1
           MOVE BOLCAIX-LOCAL2          TO GS-ACP-I-LOCAL2

           MOVE BOLCAIX-PORTADOR        TO GS-ACP-PORTADOR
                                           PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*******"          TO NOME-PORT
           END-READ
           MOVE NOME-PORT               TO GS-DESC-PORTADOR

           REFRESH-OBJECT WIN1.


       GRAVAR SECTION.
           INITIALIZE REG-BOLCAIX

           MOVE 1                       TO BOLCAIX-CODIGO
           MOVE GS-ACP-I-CODBAN         TO BOLCAIX-NUM-BANCO
           MOVE GS-ACP-I-NOMEBAN        TO BOLCAIX-NOME-BANCO
           MOVE GS-ACP-I-AGENCIA        TO BOLCAIX-AGENCIA
           MOVE GS-ACP-I-DIG-AGENCIA    TO BOLCAIX-DIG-AGENCIA
           MOVE GS-ACP-I-CONTA          TO BOLCAIX-CONTA
           MOVE GS-ACP-I-DIG-CONTA      TO BOLCAIX-DIG-CONTA
           MOVE SPACES                  TO BOLCAIX-EMPRESA
           MOVE GS-ACP-I-CNPJ           TO BOLCAIX-CNPJ
           MOVE GS-ACP-I-NOSSO-NUMERO   TO BOLCAIX-NOSSO-NUM
           MOVE GS-ACP-I-DIAS-INSTRUCAO TO BOLCAIX-QTDE-DIAS
           MOVE GS-ACP-I-JUROS-1-DIA    TO BOLCAIX-JUROS-1-DIA
           MOVE GS-ACP-I-DESCONTO       TO BOLCAIX-DESCONTO
           MOVE GS-ACP-I-MULTA          TO BOLCAIX-MULTA
           MOVE GS-ACP-I-DIAS-DESCONTO  TO BOLCAIX-DIAS-DESCONTO

           MOVE GS-ACP-I-CARTEIRA(1:2)  TO BOLCAIX-CARTEIRA

           MOVE GS-ACP-I-ESPECIE(1:2)   TO BOLCAIX-ESPECIE

           MOVE GS-ACP-I-INSTRUCAO1      TO BOLCAIX-INSTRUCAO1
           MOVE GS-ACP-I-INSTRUCAO2      TO BOLCAIX-INSTRUCAO2
           MOVE GS-ACP-I-INSTRUCAO3      TO BOLCAIX-INSTRUCAO3
           MOVE GS-ACP-I-INSTRUCAO4      TO BOLCAIX-INSTRUCAO4
           MOVE GS-ACP-I-INSTRUCAO5      TO BOLCAIX-INSTRUCAO5

           MOVE GS-ACP-I-LOCAL1          TO BOLCAIX-LOCAL1
           MOVE GS-ACP-I-LOCAL2          TO BOLCAIX-LOCAL2

           MOVE GS-ACP-PORTADOR          TO BOLCAIX-PORTADOR

           WRITE REG-BOLCAIX INVALID KEY
                 REWRITE REG-BOLCAIX INVALID KEY
                      MOVE "Erro de Regravação...BOLCAIX" TO MENSAGEM
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
           OPEN OUTPUT AUXSAC
           CLOSE       AUXSAC
           OPEN I-O    AUXSAC


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


           MOVE 1 TO BOLCAIX-CODIGO
           READ BOLCAIX INVALID KEY
                MOVE "Parâmetrização Não Realizada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                PERFORM MOVER-DADOS
                MOVE BOLCAIX-NOSSO-NUM   TO SEQUENCIA-W

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

                MOVE ZEROS          TO GS-SEQ
                MOVE "REFRESH-DATA" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                CLOSE REMES AUXSAC
                PERFORM CARREGA-LISTA
                REFRESH-OBJECT PRINCIPAL.

       FAZER-OUTRAS-COMPARACOES SECTION.
           IF GS-VENDEDOR = 0 OR VENDEDOR-CR20
              IF GS-PORTADOR = 0 OR PORTADOR-CR20
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
                     MOVE REG-REMES               TO DETALHE-ARQUIVO

      *D-SEU-NUMERO(01:9) => CODIGO COMPLETO DO CLIENTE
      *D-SEU-NUMERO(10:3) => SEQUENCIA SEQ-CR20
      *D-SEU-NUMERO(13:3) => PARCELA

                     MOVE D-CODIGO                TO COD-COMPL-CR20
                     MOVE FUNCTION NUMVAL(D-SEU-NUMERO(10:3))
                                                  TO SEQ-CR20
                     READ CRD020 INVALID KEY
                          STRING "Contas a Receber Não Encontrado"
      -                          X"0DA0"
                                 "D-CODIGO => " D-CODIGO X"0DA0"
                                 "D-SEU-NUMERO => " D-SEU-NUMERO
                            INTO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                     NOT INVALID KEY
                          PERFORM GRAVA-ANOTACAO
                          MOVE GS-ACP-PORTADOR TO
                               PORTADOR-CR20
                          IF GS-ACP-I-CARTEIRA(1:1) = "1"
                             MOVE D-NOSSO-NUMERO  TO
                                  OUTRO-DOCTO-CR20
                          END-IF
                          REWRITE REG-CRD020
                          END-REWRITE
                     END-READ
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
           MOVE 1 TO BOLCAIX-CODIGO
           READ BOLCAIX INVALID KEY
                MOVE "Parâmetrização Não Realizada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE SEQUENCIA-W TO BOLCAIX-NOSSO-NUM
                REWRITE REG-BOLCAIX INVALID KEY
                    MOVE "Erro de Regravação...BOLCAIX" TO MENSAGEM
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
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE WS-DATA-CPU    TO DATA-MOVTO-CR200
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
           MOVE ZEROS                    TO SEQ-W
                                            GS-QTDE-TITULO
                                            GS-VALOR-TOTAL
                                            VALOR-TOTAL.

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
              IF CEP1-CG11 < 1000000
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
              IF BOLCAIX-DIAS-DESCONTO > 0
                 PERFORM CALCULAR-DATA-DESCONTO
              ELSE
                 INITIALIZE WS-DATA-DESCONTO
              END-IF

              INITIALIZE DETALHE-ARQUIVO
              MOVE COD-COMPL-CR20                    TO D-CODIGO
              MOVE FUNCTION NUMVAL(GS-ACP-I-CARTEIRA(1:2))
                                                     TO D-CARTEIRA
              MOVE "N"                               TO D-ACEITE
              MOVE FUNCTION NUMVAL(GS-ACP-I-ESPECIE(1:2))
                                                     TO D-ESPECIE
              MOVE 1                                 TO D-INSTRUCAO-COB
              MOVE GS-ACP-I-DIAS-INSTRUCAO           TO D-PRAZO-PROTESTO
              MOVE 1                                 TO D-OPCAO-VECTO
              STRING DATA-VENCTO-CR20(7:2)
                     DATA-VENCTO-CR20(5:2)
                     DATA-VENCTO-CR20(1:4)         INTO D-VECTO
              MOVE DATA-EMISSAO-CR20                 TO D-EMISSAO
              MOVE 09                                TO D-COD-MOEDA
              MOVE VALOR-TOT-CR20                    TO D-VALOR-TITULO
              STRING DATA-VENCTO-CR20(7:2)
                     DATA-VENCTO-CR20(5:2)
                     DATA-VENCTO-CR20(1:4)         INTO D-DATA-DESCONTO
              MOVE ZEROS                             TO D-VALOR-DESCONTO
              MOVE ZEROS                             TO D-VALOR-ABATIMEN
              STRING DATA-VENCTO-CR20(7:2)
                     DATA-VENCTO-CR20(5:2)
                     DATA-VENCTO-CR20(1:4)         INTO D-DATA-MULTA
              MOVE ZEROS                             TO D-VALOR-MULTA
              MOVE 2                                 TO D-TIPO-BLOQUETO
              MOVE "S"                               TO D-COMPENSACAO
              MOVE "001"                             TO D-FILLER
              MOVE SPACES                            TO D-AVALISTA
              MOVE SPACES                            TO D-TIPO-PES
              MOVE SPACES                            TO D-CPF-CNPJ
              MOVE GS-ACP-I-INSTRUCAO1               TO D-MENSAGEM1
              MOVE GS-ACP-I-INSTRUCAO2               TO D-MENSAGEM2
              MOVE GS-ACP-I-INSTRUCAO3               TO D-MENSAGEM3
              MOVE GS-ACP-I-INSTRUCAO4               TO D-MENSAGEM4
              MOVE GS-ACP-I-INSTRUCAO5               TO D-MENSAGEM5
              MOVE SPACES                            TO D-MENSAGEM6
              COMPUTE D-VALOR-JUROS ROUNDED = (VALOR-TOT-CR20 *
                                            (BOLCAIX-JUROS-1-DIA / 100))

              MOVE SPACES                            TO D-SEU-NUMERO
              PERFORM GERAR-SEU-NUMERO

      *       MOVE SEQ-CR20                          TO D-SEU-NUMERO

              ADD  1                                 TO SEQUENCIA-W
              MOVE SEQUENCIA-W                       TO TABELA-NUMERO
              IF D-CARTEIRA = 12
                 MOVE 9                              TO TAB-NUMERO(1)
              END-IF

              MOVE ZEROS                             TO IND
                                                        AUX-VALOR
              MOVE 11                                TO IND2
              MOVE 1                                 TO IND
              PERFORM UNTIL IND = 9
                  COMPUTE IND2 = IND2 - 1
                  ADD 1                              TO IND
                  COMPUTE AUX-VALOR = AUX-VALOR +
                                     (TAB-NUMERO(IND2) *  IND)
              END-PERFORM

              COMPUTE AUX-VALOR = AUX-VALOR + (9 * 3)

              COMPUTE AUX-NUMERO2 = AUX-VALOR / 11

              COMPUTE RESTO = AUX-VALOR - (AUX-NUMERO2 * 11)

              IF RESTO = 0 OR 1
                 MOVE 0             TO RESTO
              ELSE
                 COMPUTE RESTO = 11 - RESTO
              END-IF

              STRING TABELA-NUMERO RESTO(2:1) INTO D-NOSSO-NUMERO

              MOVE DETALHE-ARQUIVO        TO DADOS-REMES
              MOVE X"0D0A"                TO PULA-REMES

              WRITE REG-REMES INVALID KEY
                 MOVE "ERRO DE GRAVAÇÃO...LOTE-ARQUIVO"
                                         TO MENSAGEM
                 MOVE "C"                TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              END-WRITE

              MOVE COD-COMPL-CG10        TO AUXSAC-CODIGO
              WRITE REG-AUXSAC.

       CALCULAR-DATA-DESCONTO SECTION.
           INITIALIZE IND
           PERFORM UNTIL IND = BOLCAIX-DIAS-DESCONTO
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

       GERAR-SEU-NUMERO SECTION.
           INITIALIZE D-SEU-NUMERO

      *D-SEU-NUMERO(01:9) => CODIGO COMPLETO DO CLIENTE
      *D-SEU-NUMERO(10:3) => SEQUENCIA SEQ-CR20
      *D-SEU-NUMERO(13:3) => PARCELA

           INITIALIZE REG-CRD020X
                      PARCELA
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR20x
           START CRD020X KEY IS NOT LESS CHAVE-CR20X INVALID KEY
                 MOVE "10" TO ST-CRD020X.

           PERFORM UNTIL ST-CRD020X = "10"
                 READ CRD020X NEXT WITH IGNORE LOCK AT END
                      MOVE "10" TO ST-CRD020X
                 NOT AT END
                      IF COD-COMPL-CR20 <> COD-COMPL-CR20x
                         MOVE "10" TO ST-CRD020X
                      ELSE
                         IF DATA-VENCTO-CR20x NOT > DATA-VENCTO-CR20
                            ADD 1 TO PARCELA
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM
           STRING COD-COMPL-CR20 SEQ-CR20(3:3) PARCELA
             INTO D-SEU-NUMERO.
       GERAR-SEU-NUMERO-FIM.
           EXIT.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           OPEN INPUT REMES.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO ST-REM GS-SEQ.
           PERFORM UNTIL ST-REM = "10"
                READ REMES AT END
                     MOVE "10" TO ST-REM
                NOT AT END
                     MOVE REG-REMES        TO DETALHE-ARQUIVO
                     ADD 1                 TO GS-SEQ
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
                            MOVE D-CODIGO            TO COD-COMPL-CG10
                            READ CGD010 INVALID KEY
                                 INITIALIZE REG-CGD010
                            END-READ
                            MOVE COD-COMPL-CG10      TO COD-COMPL-CG11
                            READ CGD011 INVALID KEY
                                 INITIALIZE REG-CGD011
                            END-READ
                            MOVE COMPRADOR-CG10      TO NOME-WK
                            MOVE ENDERECO1-CG11      TO ENDERECO-WK
                            MOVE BAIRRO1-CG11        TO BAIRRO-WK
                            MOVE CIDADE1-CG11        TO CIDADE
                            READ CAD010 INVALID KEY
                                 INITIALIZE REG-CAD010
                            END-READ
                            MOVE NOME-CID            TO CIDADE-WK
                            MOVE UF-CID              TO UF-WK
                            MOVE CEP1-CG11           TO CEP-WK
                            MOVE D-SEU-NUMERO        TO DOCTO-WK
                            MOVE D-VALOR-TITULO      TO VALOR-WK
                            WRITE REG-WORK
                            END-WRITE
                  END-EVALUATE
             END-READ
           END-PERFORM.

           copy condensa.

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
           DELETE FILE WORK.

           copy descondensa.
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
           MOVE "CAIXA"     TO DS-SET-NAME
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

           move 2 to acesso-codigo
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
           move "CAIXA"             to logacess-programa
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

           CLOSE CRD020 CGD010 CGD011 CAD010 CAD018 CAD001 CGD001
                 CRD020X
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
