       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP022.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 09/12/2009
      *FUNÇÃO: Movimento de contas a receber (programação financeira)

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.
           COPY CAPX030.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX020.
           COPY CXPX020.
           COPY CRPX001.
           COPY CRPX020.
           COPY CRPX021.
           COPY CRPX022.
           COPY CRPX024.
           COPY CRPX099.
           COPY CAPX018.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGX001.
           COPY LOGX002.
           COPY LOGX003.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.
       COPY CAPW030.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW020.
       COPY CXPW020.
       COPY CAPW018.
       COPY CRPW001.
       COPY CRPW020.
       COPY CRPW021.
       COPY CRPW022.
       COPY CRPW024.
       COPY CRPW099.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGW001.
       COPY LOGW002.
       COPY LOGW003.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP022.CPB".
           COPY "CRP022.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CAD030             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD021             PIC XX       VALUE SPACES.
           05  ST-CRD022             PIC XX       VALUE SPACES.
           05  ST-CRD024             PIC XX       VALUE SPACES.
           05  ST-CRD099             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ST-LOG003             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
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
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  LOTE-W                PIC 9(2)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATAWI.
               10  ANO-WI            PIC 9(4).
               10  MES-WI            PIC 99.
               10  DIA-WI            PIC 99.
           05  DATA-WI REDEFINES DATAWI PIC 9(8).
           05  DATAWII.
               10  ANO-WII           PIC 9(4).
               10  MES-WII           PIC 99.
               10  DIA-WII           PIC 99.
           05  DATA-WII REDEFINES DATAWII PIC 9(8).
      * DATA-WII - Encontrar proxima data caso a data de vencto da conta
      * permanente seja invalida, por exemplo 30/02/1998
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  CODIGO-E              PIC Z.ZZ.ZZ.ZZ.

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
           05  NUMERO                PIC 9(09)    VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  QUANTIDADE            PIC 9(01)    VALUE ZEROS.
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(76)   VALUE
           "CONFERENCIA DO MOVIMENTO DO CONTAS A RECEBER".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(130)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "C   CLIENTE SEQUEN  DOCUMENTO  T OUTRO-DOCTO     DESCRICAO
      -    "                  PORT CAR SITUACAO APU  MOEDA   RESP.  DIGI
      -    "T SEQ.CX C".
       01  LINDET.
           05  CLASS-CLIENTE-REL   PIC 9       VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  CLIENTE-REL         PIC ZZZZ.ZZZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  SEQ-REL             PIC ZZ.ZZZ.
           05  FILLER              PIC XX      VALUE SPACES.
           05  DOCUMENTO-REL       PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  TIPO-DOCTO-REL      PIC 9       VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  OUTRO-DOCTO-REL     PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  DESCRICAO-REL       PIC X(28)   VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  PORTADOR-REL        PIC 9999    VALUE ZEROS.
           05  FILLER              PIC X      VALUE SPACES.
           05  CARTEIRA-REL        PIC 9       VALUE ZEROS.
           05  FILLER              PIC XXX     VALUE SPACES.
           05  SITUACAO-REL        PIC 9       VALUE ZEROS.
           05  FILLER              PIC X       VALUE "-".
           05  DESC-SITUACAO-REL   PIC X(6)    VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  APURACAO-REL        PIC ZZZ.
           05  FILLER              PIC XX      VALUE SPACES.
           05  MOEDA-REL           PIC XXXXX   VALUE ZEROS.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  RESPONSAVEL-REL     PIC X(5)    VALUE SPACES.
           05  FILLER              PIC XX      VALUE SPACES.
           05  DIGITADOR-REL       PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(4)    VALUE SPACES.
           05  SEQ-CAIXA-REL       PIC ZZZ     BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  CONTABILIZADO-REL   PIC 9       VALUE ZEROS.
       01  CAB05.
           05  FILLER              PIC X(130)  VALUE
           "S.TI NTA-FISCAL DTA-N.FISC PARC. EMISSAO    VENCIMENTO   VAL
      -    "OR-TOTAL DATA-PAGTO  VLR-JUROS  VLR-MULTA   DESCONTO   VLR-L
      -    "IQUIDO".
       01  LINDET1.
           05  SITUACAO-TIT-REL    PIC 9       VALUE ZEROS.
           05  FILLER              PIC X(4)    VALUE SPACES.
           05  NTA-FISCAL-REL      PIC X(10)   VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  DATA-NTA-FISCAL-REL PIC ZZ/ZZ/ZZZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  PARCELA-REL         PIC 99/99   VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  DATA-EMISSAO-REL    PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  VENCTO-REL          PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR-TOTAL-REL     PIC ZZ.ZZZ.ZZZ,ZZ VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  DATA-RCTO-REL       PIC ZZ/ZZ/ZZZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  VLR-JUROS-REL       PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  VLR-MULTA-REL       PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  DESCONTO-REL        PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  VLR-LIQUIDO-REL     PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.

       01  CAB02A.
           05  FILLER              PIC X(63)   VALUE
           "CONFERENCIA DO MOVIMENTO DO CONTAS A RECEBER".
           05  HORA-RELA           PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-RELA        PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03A.
           05  FILLER              PIC X(80)  VALUE ALL "=".

      *-----------------------------------------------------------------
      *           IMPRESSAO PROGRAMAÇÃO FINANCEIRA ALFREDO
      *-----------------------------------------------------------------
       01 DET-01.
          05 FILLER                        PIC X(26)
             VALUE "PROGRAMACAO FINANCEIRA N. ".
          05 DET-NUMERO                    PIC ZZ.ZZZ.ZZ9.
          05 FILLER                        PIC X(04).
          05 FILLER                        PIC X(09)
             VALUE "DATA...: ".
          05 DET-DIA                       PIC 99/.
          05 DET-MES                       PIC 99/.
          05 DET-ANO                       PIC 9999.
          05 FILLER                        PIC X(02).
          05 DET-HORA                      PIC 9(02).
          05 FILLER                        PIC X(01) VALUE ":".
          05 DET-MINU                      PIC 9(02).


       01 DET-02.
          05 FILLER                        PIC X(13)
             VALUE "DATA.......: ".
          05 DET-EMISSAO                   PIC 99/99/9999.
          05 FILLER                        PIC X(03).
          05 FILLER                        PIC X(29).
          05 FILLER                        PIC X(03).
          05 FILLER                        PIC X(09)
             VALUE "VECTO..: ".
          05 DET-VENCTO                    PIC 99/99/9999.

       01 DET-02a.
          05 FILLER                        PIC X(13)
             VALUE "N. DOCTO...: ".
          05 DET-NUMDOC                    PIC X(20).


       01 DET-03.
          05 FILLER                        PIC X(13)
             VALUE "FAVORECIDO.: ".
          05 DET-FAVORECIDO                PIC X(45).
          05 FILLER                        PIC X(09)
             VALUE "VALOR..: ".
          05 DET-VALOR                     PIC ZZZ.ZZ9,99.

       01 DET-04.
          05 FILLER                        PIC X(13)
             VALUE "VL. EXTENSO: ".
          05 DET-EXTENSO                   PIC X(64).

       01 DET-05.
          05 FILLER                        PIC X(13)
             VALUE "REFERENTE..: ".
          05 DET-REFERENTE                 PIC X(64).

       01 DET-06.
          05 FILLER                        PIC X(13)
             VALUE "TIPO DOCTO.: ".
          05 DET-TIPODOC                   PIC X(30).

       01 DET-07.
          05 FILLER                        PIC X(13)
             VALUE "BANCO......: ".
          05 DET-BANCO                     PIC X(40).

       01 DET-08.
          05 FILLER                        PIC X(13)
             VALUE "AGENCIA....: ".
          05 DET-AGENCIA                   PIC X(30).
          05 FILLER                        PIC X(01).
          05 FILLER                        PIC X(18)
             VALUE "CONTA CORRENTE..: ".
          05 DET-CONTA-CORRENTE            PIC X(20).


       01 DET-10.
          05 FILLER                        PIC X(13)
             VALUE "VISTO EMISSOR".
          05 FILLER                        PIC X(08).
          05 FILLER                        PIC X(15)
             VALUE "VISTO APROVACAO".
          05 FILLER                        PIC X(08).
          05 FILLER                        PIC X(17)
             VALUE "VISTO PROGRAMACAO".

       01 DET-11.
          05 FILLER                        PIC X(04).
          05 DET-RESPONSAVEL               PIC X(13).

       01  link-extenso.
           05  link-descricao.
               10  link-descr      pic x(01)         occurs 200  times.
           05  link-valor          pic 9(12)v99.
           05  link-valor-r        redefines         link-valor.
               10  link-vlr-cruz   pic 9(03)         occurs   4  times.
               10  link-vlr-cent   pic 9(02).

       01  LINDETA.
           05  LINDETA-REL         PIC X(80)   VALUE SPACES.

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

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE HORA-REL TO HORA-RELA
           MOVE EMISSAO-REL TO EMISSAO-RELA.
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "CAD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD030.
           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD001.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD021" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD021.
           MOVE "CRD022" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD022.
           MOVE "CRD024" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD024.
           MOVE "CRD099" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD099.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "LOG001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOG002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOG003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG003.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN I-O   CRD020 CRD021 CRD022 CRD024 LOG001 LOG002 LOG003
           CLOSE      CRD024 CRD022
           OPEN I-O   CRD024 CRD022
           OPEN INPUT CAD018 CGD001 CXD020 CAD002 CRD001 CGD010 CGD020
                      CAD030.
           OPEN I-O CRD099.
           IF ST-CRD099 = "35"
              CLOSE CRD099      OPEN OUTPUT CRD099
              CLOSE CRD099      OPEN I-O CRD099.
           CLOSE CRD099.
           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O CRD020
           END-IF.
           IF ST-CRD021 = "35"
              CLOSE CRD021      OPEN OUTPUT CRD021
              CLOSE CRD021      OPEN I-O CRD021
           END-IF.
           IF ST-CRD022 = "35"
              CLOSE CRD022      OPEN OUTPUT CRD022
              CLOSE CRD022      OPEN I-O CRD022
           END-IF.
           IF ST-CRD024 = "35"
              CLOSE CRD024      OPEN OUTPUT CRD024
              CLOSE CRD024      OPEN I-O CRD024
           END-IF.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD030 <> "00"
              MOVE "ERRO ABERTURA CAD030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD021 <> "00"
              MOVE "ERRO ABERTURA CRD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD022 <> "00"
              MOVE "ERRO ABERTURA CRD022: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD022 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD024 <> "00"
              MOVE "ERRO ABERTURA CRD024: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD024 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG003 <> "00"
              MOVE "ERRO ABERTURA LOG003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE ALL "9" TO CLASS-CLIENTE-CR24
           MOVE ALL "9" TO CLIENTE-CR24
           MOVE ALL "9" TO SEQ-CR24
           READ CRD024 INVALID KEY
               CLOSE      CRD024
               OPEN I-O   CRD024
               MOVE 0 TO NUMERO-PROGRAMACAO-CR24
               WRITE REG-CRD024
               CLOSE      CRD024
               OPEN INPUT CRD024.


      *    MOVE 1 TO COD-USUARIO-W.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP022"            to logacess-programa
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

           CLOSE      CRD020 CRD021 CRD022 CRD024 LOG001 LOG002 LOG003
           OPEN INPUT CRD020 CRD021 CRD022 CRD024 LOG001 LOG002 LOG003


           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   IF GS-PARCELA = 1
                      PERFORM GRAVA-PARCELAS
                   ELSE
                    PERFORM SALVAR-DADOS
                    IF GS-TIPO-GRAVACAO = 1
                       PERFORM REGRAVA-DADOS
                       PERFORM GRAVA-ANOTACAO
                    ELSE
                       PERFORM GRAVA-DADOS
                       PERFORM GRAVA-ANOTACAO-PRE-DEFINIDA
                    END-IF
                   END-IF
                   IF GS-ACP-BANCO > 0
                      CLOSE      CRD022
                      OPEN I-O   CRD022
                      MOVE GS-CLASSIFICACAO(1:1) TO CLASS-CLIENTE-CR22
                      MOVE GS-COD-CLIENTE        TO CLIENTE-CR22
                      MOVE SEQ-CR21              TO SEQ-CR22
                      READ CRD022 INVALID KEY
                         MOVE SPACES TO OBS-CR22
                         STRING "BANCO.: " GS-ACP-BANCO " AGENCIA
      -                  ".: " GS-ACP-AGENCIA " CONTA.: "
                         GS-ACP-CONTA INTO OBS-CR22
                         WRITE REG-CRD022 INVALID KEY
                               MOVE "CRD022"  TO
                                    GS-MENSAGEM-ERRO(15: 07)
                               MOVE ST-CRD022 TO
                                    GS-MENSAGEM-ERRO(23: 02)
                               PERFORM ERRO-GRAVACAO
                         END-WRITE
                      NOT INVALID KEY
                         MOVE SPACES TO OBS-CR22
                         STRING "BANCO.: " GS-ACP-BANCO " AGENCIA
      -                  ".: " GS-ACP-AGENCIA " CONTA.: "
                         GS-ACP-CONTA INTO OBS-CR22
                         REWRITE REG-CRD022 INVALID KEY
                            MOVE "CPD022"  TO
                                 GS-MENSAGEM-ERRO(15: 07)
                            MOVE ST-CRD022 TO
                                 GS-MENSAGEM-ERRO(23: 02)
                            PERFORM ERRO-GRAVACAO
                         END-REWRITE
                      END-READ
                      CLOSE      CRD022
                      OPEN INPUT CRD022
                   END-IF

                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   MOVE USUARIO-W TO DIGITADOR-CR20
                   MOVE 3 TO SITUACAO-CR20
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CANCELA-FLG-TRUE
                    MOVE USUARIO-W TO DIGITADOR-CR20
                    MOVE 4 TO SITUACAO-CR20
                    PERFORM CANCELA
               WHEN GS-REVERTE-CANCELADO-TRUE
                    PERFORM REVERTER-CANCELADO
               WHEN GS-REVERTE-BAIXADO-TRUE
                    PERFORM REVERTER-BAIXADO
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-IMPRIME-TELA-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-TELA
                    END-IF
               WHEN GS-LE-CONTA-TRUE
                    PERFORM LER-CONTA
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 1) TO CLASS-CLIENTE-CR20
                   MOVE GS-LINDET(3: 8) TO CLIENTE-CR20
                   MOVE GS-LINDET(12: 5) TO SEQ-CR20
                   PERFORM CARREGAR-DADOS
               WHEN GS-DIVIDE-PARCELA-TRUE
                   PERFORM DIVIDE-PARCELAS
               WHEN GS-VERIF-TOT-PARC-TRUE
                   PERFORM VERIFICA-TOTAL-PARCELA
               WHEN GS-LE-CLIENTE-TRUE
                   PERFORM LE-CLIENTE
               WHEN GS-LE-PORTADOR-TRUE
                   PERFORM LE-PORTADOR
               WHEN GS-LE-SITUACAO-TIT-TRUE
                   PERFORM LE-SITUACAO-TIT
               WHEN GS-LE-COD-APURACAO-TRUE
                   PERFORM LE-COD-APURACAO
               WHEN GS-LE-VENDEDOR-TRUE
                   PERFORM LE-VENDEDOR
               WHEN GS-CHAMAR-APURACAO-TRUE
                   PERFORM CHAMAR-APURACAO
               WHEN GS-CARREGA-DATA-TRUE
                   PERFORM CARREGA-DATA
               WHEN GS-EMISSAO-VENCTO-TRUE
                   PERFORM INVERTE-EMIS-VENCTO
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-LE-CARTAO-TRUE
                    PERFORM LER-CARTAO
               WHEN GS-POPUP-CARTAO-TRUE
                    PERFORM POPUP-CARTAO
               WHEN GS-SELECIONA-IMPRESSAO-TRUE
                    PERFORM SELECIONAR-IMPRESSAO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       SELECIONAR-IMPRESSAO SECTION.
           IF GS-LINDET(100:1) = "X"
              MOVE SPACES TO GS-LINDET(100:1)
           ELSE
              MOVE "X" TO GS-LINDET(100:1).

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".

       LER-CONTA SECTION.
           IF GS-ACP-TIPO = 3
              MOVE GS-ACP-BANCO    TO CODIGO-CAD30
              READ CAD030 INVALID KEY
                  MOVE "Dados do Banco Inválido" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM.
       LER-CARTAO SECTION.
           MOVE GS-ACP-CARTAO TO CODIGO-CG20
           READ CGD020 INVALID KEY
                MOVE SPACES TO NOME-CG20.
           MOVE NOME-CG20 TO GS-DESC-CARTAO.

       POPUP-CARTAO SECTION.
           CALL   "CGP020T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CGP020T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-CARTAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-ACP-CARTAO.

       CHAMAR-APURACAO SECTION.
           CALL   "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CXP020T"
           MOVE PASSAR-STRING-1(52: 5) TO GS-COD-APURACAO
           PERFORM LE-COD-APURACAO.
       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-CLIENTE
             WHEN 2 PERFORM CARREGA-POP-UP-VENDEDOR
             WHEN 3 CALL   "CAP018T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "CAP018T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-PORTADOR
                    MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR
             WHEN 4 PERFORM CARREGA-POP-UP-APURACAO
      *             CALL "CXP020T" USING PASSAR-PARAMETROS
      *             CANCEL "CXP020T"
      *             MOVE PASSAR-STRING-1(52: 3) TO GS-COD-APURACAO
             WHEN 5 CALL   "CRP001T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "CRP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-SITUACAO-TIT
                    MOVE PASSAR-STRING-1(33: 2) TO GS-SITUACAO-TIT
             WHEN 7 CALL   "CAP020T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "CAP020T"
                    MOVE PASSAR-STRING-1(59: 4) TO GS-ACP-BANCO.
       CARREGA-POP-UP-CLIENTE SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO COMPRADOR-CG10.
           START CGD010 KEY IS NOT < COMPRADOR-CG10 INVALID KEY
                 MOVE "10" TO ST-CGD010.
           PERFORM UNTIL ST-CGD010 = "10"
                 READ CGD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CGD010
                 NOT AT END
                      MOVE COMPRADOR-CG10(1: I) TO INICIAL-A-COMPARAR
                      IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                         MOVE "10" TO ST-CGD010
                      ELSE
                         MOVE COMPRADOR-CG10  TO GS-LINDET1(1: 32)
                         MOVE CODIGO-CG10     TO GS-LINDET1(33: 08)
                         MOVE CLASSIF-CG10    TO GS-LINDET1(43: 1)
                         MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-LINDET1(I: 1) TO LETRA
               IF LETRA = SPACES
                  MOVE 1 TO SAIR-W
                  SUBTRACT 1 FROM I
               ELSE
                  MOVE GS-LINDET1(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.
       CARREGA-POP-UP-VENDEDOR SECTION.
           MOVE SPACES TO NOME-CG01.
           MOVE GS-LINDET1(1: 1) TO NOME-CG01 LETRA.
           MOVE "CLEAR-LIST-BOX-VEND" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
                 READ CGD001 NEXT RECORD AT END
                      MOVE "10" TO ST-CGD001
                 NOT AT END
                      MOVE NOME-CG01     TO LETRA1
                      IF LETRA1 NOT = LETRA
                         MOVE "10" TO ST-CGD001
                      ELSE
                         IF T-VEND-CG01 = 1
                            MOVE NOME-CG01     TO GS-LINDET1(1: 32)
                            MOVE CODIGO-CG01   TO GS-LINDET1(33: 06)
                            MOVE "INSERE-POP-UP-VENDEDOR"
                              TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           IF GS-OPCAO-POP-UP = 4
              PERFORM ITEM-SELECIONADO-APURACAO
           ELSE
            IF GS-OPCAO-POP-UP = 2
                MOVE GS-LINDET1(33: 6) TO GS-VENDEDOR
                MOVE GS-LINDET1(1: 30) TO GS-DESCR-VENDEDOR
            ELSE MOVE GS-LINDET1(33: 8) TO GS-COD-CLIENTE
                 MOVE GS-LINDET1(43: 1) TO CLASSIF-W
                 EVALUATE CLASSIF-W
                    WHEN 0 MOVE "0-Contrato"       TO GS-CLASSIFICACAO
                    WHEN 1 MOVE "1-Comum"          TO GS-CLASSIFICACAO
                    WHEN 9 MOVE "9-Unificado"      TO GS-CLASSIFICACAO
                 END-EVALUATE
                 MOVE GS-LINDET1(1: 30) TO GS-DESCR-CLIENTE.

       ITEM-SELECIONADO-APURACAO SECTION.
           MOVE GS-LINDET1(52: 5)TO GS-COD-APURACAO.
           PERFORM LE-COD-APURACAO.
           MOVE DESCRICAO-CX20 TO GS-DESCR-APURACAO.
       CARREGA-POP-UP-APURACAO SECTION.
           MOVE "CLEAR-LIST-BOX1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO CODIGO-COMPL-CX20.
           START CXD020 KEY IS NOT < CODIGO-COMPL-CX20 INVALID KEY
                 MOVE "10" TO ST-CXD020.
           PERFORM UNTIL ST-CXD020 = "10"
                 READ CXD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CXD020
                 NOT AT END
                      MOVE SPACES TO GS-LINDET1
                      MOVE CODIGO-COMPL-CX20 TO CODIGO-E
                      EVALUATE GRAU-CX20
                        WHEN 1 PERFORM GRAU-1
                        WHEN 2 PERFORM GRAU-2
                        WHEN 3 PERFORM GRAU-3
                        WHEN 4 PERFORM GRAU-4
                      END-EVALUATE
                      MOVE "INSERE-POP-UP-APUR" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.
       GRAU-1 SECTION.
           MOVE CODIGO-E          TO GS-LINDET1(1: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET1(12: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET1(52: 05).
       GRAU-2 SECTION.
           MOVE CODIGO-E          TO GS-LINDET1(4: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET1(15: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET1(52: 05).
       GRAU-3 SECTION.
           MOVE CODIGO-E          TO GS-LINDET1(7: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET1(18: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET1(52: 05).
       GRAU-4 SECTION.
           MOVE CODIGO-E          TO GS-LINDET1(10: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET1(21: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET1(52: 05).

       INVERTE-EMIS-VENCTO SECTION.
           MOVE GS-DATA-EMISSAO TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           CANCEL "GRIDAT2".
           MOVE DATA-INV TO GS-EMISSAO-INV.
           MOVE GS-DATA-VENCTO TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           CANCEL "GRIDAT2".
           MOVE DATA-INV TO GS-VENCTO-INV.
       CARREGA-DATA SECTION.
           MOVE DATA-MOVTO-W TO GS-DATA-MOVTO.
           MOVE "CARREGAR-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       EXCLUI SECTION.
           CLOSE    LOG003 CRD024
           OPEN I-O CRD099 LOG003 CRD024
           MOVE REG-CRD020 TO REG-CRD099

           MOVE USUARIO-W  TO USUARIO-EXCLUSAO-CR99
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
           DATA-EXCLUSAO-CR99
           ACCEPT HORA-EXCLUSAO-CR99 FROM TIME

           WRITE REG-CRD099 NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "I"         TO LOG3-OPERACAO
                 MOVE "CRD099"    TO LOG3-ARQUIVO
                 MOVE "CRP022"    TO LOG3-PROGRAMA
                 MOVE REG-CRD099  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE.

           MOVE CLASS-CLIENTE-CR20  TO CLASS-CLIENTE-CR24
           MOVE CLIENTE-CR20        TO CLIENTE-CR24
           MOVE SEQ-CR20            TO SEQ-CR24
           READ CRD024 NOT INVALID KEY
                DELETE CRD024 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "E"         TO LOG3-OPERACAO
                       MOVE "CRD024"    TO LOG3-ARQUIVO
                       MOVE "CRP022"    TO LOG3-PROGRAMA
                       MOVE REG-CRD024  TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE.

           DELETE CRD020 NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "E"         TO LOG3-OPERACAO
                 MOVE "CRD020"    TO LOG3-ARQUIVO
                 MOVE "CRP022"    TO LOG3-PROGRAMA
                 MOVE REG-CRD020  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE.
           CLOSE      CRD099 LOG003 CRD024
           OPEN INPUT LOG003 CRD024.
           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
       CANCELA SECTION.
           CLOSE    CRD020 LOG003
           OPEN I-O CRD020 LOG003
           REWRITE REG-CRD020 NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "A"         TO LOG3-OPERACAO
                 MOVE "CRD020"    TO LOG3-ARQUIVO
                 MOVE "CRP022"    TO LOG3-PROGRAMA
                 MOVE REG-CRD020  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE.

           CLOSE      CRD020 LOG003
           OPEN INPUT CRD020 LOG003

           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
       REVERTER-CANCELADO SECTION.
           CLOSE    CRD020 LOG003
           OPEN I-O CRD020 LOG003

           MOVE USUARIO-W TO DIGITADOR-CR20
           MOVE 0         TO SITUACAO-CR20
           REWRITE REG-CRD020 NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "A"         TO LOG3-OPERACAO
                 MOVE "CRD020"    TO LOG3-ARQUIVO
                 MOVE "CRP022"    TO LOG3-PROGRAMA
                 MOVE REG-CRD020  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE.
           CLOSE      CRD020 LOG003
           OPEN INPUT CRD020 LOG003
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       REVERTER-BAIXADO SECTION.
           CLOSE    CRD020 LOG003
           OPEN I-O CRD020 LOG003
           MOVE USUARIO-W      TO DIGITADOR-CR20
           MOVE 0              TO SITUACAO-CR20
           MOVE ZEROS          TO JURO-RCTO-CR20
                                  MULTA-RCTO-CR20
                                  DESCONTO-CR20
                                  DATA-RCTO-CR20
                                  VALOR-LIQ-CR20.
           MOVE VALOR-TOT-CR20 TO VALOR-SALDO-CR20
           REWRITE REG-CRD020 NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "A"         TO LOG3-OPERACAO
                 MOVE "CRD020"    TO LOG3-ARQUIVO
                 MOVE "CRP022"    TO LOG3-PROGRAMA
                 MOVE REG-CRD020  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE.
           CLOSE      CRD020 LOG003
           OPEN INPUT CRD020 LOG003
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR            TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE "********"        TO NOME-CG01.
           MOVE NOME-CG01              TO GS-DESCR-VENDEDOR.
       LE-CLIENTE SECTION.
           MOVE GS-CLASSIFICACAO(1: 1) TO CLASSIF-CG10
           MOVE GS-COD-CLIENTE         TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE "********"        TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10         TO GS-DESCR-CLIENTE.
       LE-PORTADOR SECTION.
           MOVE GS-PORTADOR            TO PORTADOR.
           READ CAD018 INVALID KEY
                MOVE "******"          TO NOME-PORT.
           MOVE NOME-PORT              TO GS-DESCR-PORTADOR.
       LE-COD-APURACAO SECTION.
           MOVE GS-COD-APURACAO        TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY
                MOVE "*****"           TO DESCRICAO-CX20
                MOVE ZEROS             TO TIPO-CONTA-CX20.
           MOVE DESCRICAO-CX20         TO GS-DESCR-APURACAO.
           IF TIPO-CONTA-CX20 = 0
              MOVE 0                   TO GS-TIPO-CONTA-APUR
           ELSE
              MOVE 1                   TO GS-TIPO-CONTA-APUR.
       LE-SITUACAO-TIT SECTION.
           MOVE GS-SITUACAO-TIT TO CODIGO-CR01.
           READ CRD001 INVALID KEY MOVE "*********" TO
                   SITUACAO-TIT-CR01.
           MOVE SITUACAO-TIT-CR01  TO GS-DESCR-SITUACAO-TIT.
       CARREGAR-DADOS SECTION.
           START CRD020 KEY IS = CHAVE-CR20 INVALID KEY
                 CONTINUE.
           READ CRD020 INVALID KEY
                 INITIALIZE REG-CRD020.

           MOVE CLASS-CLIENTE-CR20          TO CLASS-CLIENTE-CR24
           MOVE CLIENTE-CR20                TO CLIENTE-CR24
           MOVE SEQ-CR20                    TO SEQ-CR24
           READ CRD024 INVALID KEY
                INITIALIZE REG-CRD024.

           MOVE BANCO-CR24                  TO GS-ACP-BANCO
           MOVE AGENCIA-CR24                TO GS-ACP-AGENCIA
           MOVE CONTA-CR24                  TO GS-ACP-CONTA
           MOVE TIPO-CR24                   TO GS-ACP-TIPO
           MOVE LOTE-CR20                   TO GS-LOTE

           MOVE DATA-MOVTO-W                TO GS-DATA-MOVTO
           EVALUATE CLASS-CLIENTE-CR20
             WHEN 0 MOVE "0-Contrato"       TO GS-CLASSIFICACAO
             WHEN 1 MOVE "1-Comum   "       TO GS-CLASSIFICACAO
             WHEN 9 MOVE "9-Unificado"      TO GS-CLASSIFICACAO
           END-EVALUATE
           MOVE CLASS-CLIENTE-CR20          TO CLASSIF-CG10
           MOVE CLIENTE-CR20                TO GS-COD-CLIENTE
                                               CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE "*****"                TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10              TO GS-DESCR-CLIENTE
           MOVE PORTADOR-CR20               TO GS-PORTADOR PORTADOR
           READ CAD018 INVALID KEY
                MOVE "******"               TO NOME-PORT.

           MOVE NOME-PORT                   TO GS-DESCR-PORTADOR.
           EVALUATE CARTEIRA-CR20
             WHEN 1 MOVE "1-Simples "       TO GS-CARTEIRA
             WHEN 2 MOVE "2-Caução  "       TO GS-CARTEIRA
             WHEN 3 MOVE "3-Desconto"       TO GS-CARTEIRA
           END-EVALUATE
           MOVE CARTAO-CRED-CR20            TO GS-ACP-CARTAO
                                               CODIGO-CG20
           READ CGD020 INVALID KEY
               MOVE SPACES                  TO NOME-CG20
           END-READ
           MOVE NOME-CG20                   TO GS-DESC-CARTAO

           MOVE NR-DOCTO-CR20               TO GS-NR-DOCTO
           MOVE OUTRO-DOCTO-CR20            TO GS-OUTRO-DOCTO
           MOVE NR-NOTA-FISCAL-CR20         TO GS-NR-NOTA-FISCAL
           MOVE DATA-NTA-FISCAL-CR20        TO GS-DATA-NTA-FISCAL
           MOVE DATA-EMISSAO-CR20           TO GS-DATA-EMISSAO
           MOVE DATA-VENCTO-CR20            TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV                    TO GS-DATA-VENCTO
           MOVE DESCRICAO-CR20              TO GS-DESCRICAO
           MOVE DIGITADOR-CR20              TO GS-DIGITADOR
           MOVE TIPO-MOEDA-CR20             TO GS-TIPO-MOEDA
           EVALUATE TIPO-MOEDA-CR20
             WHEN 0 MOVE "-Real"            TO GS-TIPO-MOEDA(2: 6)
             WHEN 1 MOVE "-Dolar"           TO GS-TIPO-MOEDA(2: 5)
           END-EVALUATE
           MOVE SITUACAO-TIT-CR20           TO GS-SITUACAO-TIT
                                               CODIGO-CR01
           READ CRD001 INVALID KEY
                MOVE "********"             TO SITUACAO-TIT-CR01
           END-READ
           MOVE SITUACAO-TIT-CR01           TO GS-DESCR-SITUACAO-TIT
           MOVE CODREDUZ-APUR-CR20          TO GS-COD-APURACAO
                                               CODIGO-REDUZ-CX20
           READ CXD020 INVALID KEY
                MOVE "*****"                TO DESCRICAO-CX20.

           MOVE DESCRICAO-CX20              TO GS-DESCR-APURACAO
           MOVE TIPO-DOCTO-CR20             TO GS-TIPO-DOCTOW
           EVALUATE TIPO-DOCTO-CR20
             WHEN 0 MOVE "0-Boleto           " TO GS-TIPO-DOCTO(1: 19)
             WHEN 1 MOVE "1-Dupl/Promis      " TO GS-TIPO-DOCTO(1: 19)
             WHEN 2 MOVE "2-Org.Evento       " TO GS-TIPO-DOCTO(1: 19)
             WHEN 3 MOVE "3-Debito Automatico" TO GS-TIPO-DOCTO(1: 19)
             WHEN 4 MOVE "4-Cartao de Credito" TO GS-TIPO-DOCTO(1: 19)
           END-EVALUATE
           MOVE VALOR-TOT-CR20              TO GS-VALOR-TOTAL
           MOVE VENDEDOR-CR20               TO GS-VENDEDOR
                                               CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE "*****"                TO NOME-CG01
           END-READ
           MOVE NOME-CG01                   TO GS-DESCR-VENDEDOR
           MOVE RESPONSAVEL-CR20            TO GS-RESPONSAVEL
           MOVE SITUACAO-CR20               TO GS-SITUACAO
           MOVE SITUACAO-TIT-CR20           TO GS-SITUACAO-TIT
                                               CODIGO-CR01.
           READ CRD001 INVALID KEY
                MOVE "********"             TO SITUACAO-TIT-CR01.

           MOVE SITUACAO-TIT-CR01           TO GS-DESCR-SITUACAO-TIT.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO"  TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1             TO ERRO-W.
       LIMPAR-DADOS SECTION.
           MOVE GS-DATA-MOVTO TO DATA-MOVTO-W
           MOVE GS-LOTE       TO LOTE-W
           INITIALIZE REG-CRD020
           INITIALIZE GS-DATA-BLOCK
           MOVE DATA-MOVTO-W  TO GS-DATA-MOVTO
           MOVE LOTE-W        TO GS-LOTE.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS         TO GS-PARCELA
                                 GS-GRAVA-OBS.
       DIVIDE-PARCELAS SECTION.
           COMPUTE VLR-PARCELA = GS-VALOR-TOTAL / GS-QT-PARCELA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
               MOVE ZEROS TO GS-VALOR(I) GS-NR(I)
                             GS-VENCTO(I)
           END-PERFORM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GS-QT-PARCELA
               MOVE VLR-PARCELA       TO GS-VALOR(I)
               MOVE I                 TO GS-NR(I)
           END-PERFORM.
           COMPUTE GS-VALOR(1) = (GS-VALOR-TOTAL - (
                   GS-QT-PARCELA * VLR-PARCELA)) + VLR-PARCELA.
           MOVE GS-DATA-VENCTO     TO GS-VENCTO(1).
           PERFORM INVERTE-EMIS-VENCTO.
           MOVE GS-VENCTO-INV TO DATA-WI.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > GS-QT-PARCELA
                   ADD 1 TO MES-WI
                   IF MES-WI > 12
                      MOVE 1 TO MES-WI  ADD 1 TO ANO-WI
                   END-IF
                   MOVE 2         TO GRTIME-TYPE
                   MOVE 7         TO GRTIME-FUNCTION
                   MOVE DATA-WI   TO GRTIME-DATE DATA-WII DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV  TO GS-VENCTO(I)
                   CALL "GRTIME" USING PARAMETROS-GRTIME
                   CANCEL "GRTIME"
                   IF GRTIME-DATE-FINAL = ZEROS
                      MOVE 1      TO DIA-WII
                      ADD 1       TO MES-WII
                      IF MES-WII = 13 MOVE 01 TO MES-WII
                                      ADD 1   TO ANO-WII
                      END-IF
                      MOVE DATA-WII TO DATA-INV
                      CALL "GRIDAT1" USING DATA-INV
                      MOVE DATA-INV TO GS-VENCTO(I)
                   END-IF
           END-PERFORM.
       VERIFICA-TOTAL-PARCELA SECTION.
           MOVE ZEROS TO VLR-PARCELA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GS-QT-PARCELA
              ADD GS-VALOR(I) TO VLR-PARCELA
           END-PERFORM.
           MOVE VLR-PARCELA  TO GS-VLR-TOT-PARCELA.
       SALVAR-DADOS SECTION.
           MOVE DATA-MOVTO-I      TO DATA-MOVTO-CR20.
           IF GS-CLASSIFICACAO = SPACES MOVE "0" TO CLASS-CLIENTE-CR20
           ELSE MOVE GS-CLASSIFICACAO(1: 1)  TO CLASS-CLIENTE-CR20.
           MOVE GS-COD-CLIENTE        TO CLIENTE-CR20
           MOVE GS-PORTADOR       TO PORTADOR-CR20.
           IF GS-CARTEIRA = SPACES
              MOVE "0" TO CARTEIRA-CR20
           ELSE
              MOVE GS-CARTEIRA(1: 1) TO CARTEIRA-CR20.

           IF GS-TIPO-DOCTO = SPACES
              MOVE "0" TO TIPO-DOCTO-CR20
           ELSE
              MOVE GS-TIPO-DOCTO(1: 1) TO TIPO-DOCTO-CR20.

           IF GS-SITUACAO-TIT = SPACES
              MOVE "00" TO SITUACAO-TIT-CR20
           ELSE
              MOVE GS-SITUACAO-TIT(1: 2) TO SITUACAO-TIT-CR20.

           MOVE GS-LOTE            TO LOTE-CR20
           MOVE GS-NR-DOCTO        TO NR-DOCTO-CR20
           MOVE GS-OUTRO-DOCTO     TO OUTRO-DOCTO-CR20
           MOVE GS-DATA-EMISSAO    TO DATA-EMISSAO-CR20
           MOVE GS-VENCTO-INV      TO DATA-VENCTO-CR20
           MOVE GS-DESCRICAO       TO DESCRICAO-CR20

           IF GS-TIPO-MOEDA = SPACES
              MOVE "0"                 TO TIPO-MOEDA-CR20
           ELSE
              MOVE GS-TIPO-MOEDA(1: 1) TO TIPO-MOEDA-CR20
           END-IF

           MOVE GS-COD-APURACAO    TO CODREDUZ-APUR-CR20
           MOVE GS-RESPONSAVEL     TO RESPONSAVEL-CR20
           MOVE PARAMETROS-W(1: 5) TO DIGITADOR-CR20
           MOVE 0101               TO NR-PARCELA-CR20
           MOVE GS-VALOR-TOTAL     TO VALOR-TOT-CR20
           MOVE GS-VALOR-TOTAL     TO VALOR-SALDO-CR20
           MOVE GS-VENDEDOR        TO VENDEDOR-CR20
           MOVE GS-NR-NOTA-FISCAL  TO NR-NOTA-FISCAL-CR20.
           MOVE GS-DATA-NTA-FISCAL TO DATA-NTA-FISCAL-CR20

           MOVE ALL "9" TO CLASS-CLIENTE-CR24
           MOVE ALL "9" TO CLIENTE-CR24
           MOVE ALL "9" TO SEQ-CR24
           READ CRD024 INVALID KEY
               MOVE 0                      TO NUMERO-PROGRAMACAO-CR24
               WRITE REG-CRD024.

           MOVE NUMERO-PROGRAMACAO-CR24    TO NUMERO
           ADD  1                          TO NUMERO

           MOVE CLASS-CLIENTE-CR20         TO CLASS-CLIENTE-CR24
           MOVE CLIENTE-CR20               TO CLIENTE-CR24
           MOVE SEQ-CR20                   TO SEQ-CR24
           MOVE GS-ACP-BANCO               TO BANCO-CR24
           MOVE GS-ACP-CONTA               TO CONTA-CR24
           MOVE GS-ACP-AGENCIA             TO AGENCIA-CR24
           MOVE GS-ACP-TIPO                TO TIPO-CR24
           MOVE NUMERO                     TO NUMERO-PROGRAMACAO-CR24.
           .

       GRAVA-PARCELAS SECTION.
           MOVE ZEROS TO K.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
               IF GS-NR(I) = ZEROS
                  MOVE 24 TO I
               ELSE
                  PERFORM SALVAR-DADOS
                  MOVE GS-NR(I)      TO NR-PARCELA-CR20
                  MOVE SPACES TO NR-DOCTO-CR20
                  STRING GS-NR-DOCTO GS-NR(I) DELIMITED BY " "
                  INTO NR-DOCTO-CR20
      *           IF GS-NR(I) > 9
      *              MOVE GS-NR-DOCTO(2: 4) TO NR-DOCTO-CR20(1: 4)
      *              MOVE GS-NR(I)          TO NR-DOCTO-CR20(5: 2)
      *           ELSE
      *              MOVE GS-NR-DOCTO(1: 5) TO NR-DOCTO-CR20(1: 5)
      *              MOVE GS-NR(I)(2: 1)    TO NR-DOCTO-CR20(6: 1)
      *           END-IF
                  MOVE GS-QT-PARCELA TO TOT-PARC-CR20
                  MOVE GS-VENCTO(I)  TO DATA-INV
                  CALL "GRIDAT2" USING DATA-INV
                  MOVE DATA-INV          TO DATA-VENCTO-CR20
                  MOVE GS-VALOR(I)       TO VALOR-TOT-CR20
                  MOVE GS-VALOR(I)       TO VALOR-SALDO-CR20
                  PERFORM GRAVA-DADOS
               END-IF
           END-PERFORM.
       GRAVA-DADOS SECTION.
           CLOSE    CRD020 LOG003 CRD024
           OPEN I-O CRD020 LOG003 CRD024

           MOVE ZEROS TO SITUACAO-CR20
                         VALOR-LIQ-CR20
                         JURO-RCTO-CR20
                         MULTA-RCTO-CR20
                         DESCONTO-CR20
                         CONTABILIZADO-CR20
                         SEQ-CAIXA-CR20
                         DATA-RCTO-CR20

           MOVE GS-ACP-CARTAO          TO CARTAO-CRED-CR20
                                          CODIGO-CG20
           READ CGD020 INVALID KEY
               INITIALIZE REG-CGD020
           END-READ
           MOVE TAXA-CREDITO-CG20      TO TAXA-ADMINIST-CREDITO-CR20
           MOVE TAXA-PARCELA-CG20      TO TAXA-ADMINIST-PARCELA-CR20
           move VALOR-TOT-CR20         TO VALOR-SALDO-CR20

           MOVE GS-CLASSIFICACAO(1: 1) TO CLASS-CLIENTE-CR21
           MOVE GS-COD-CLIENTE         TO CLIENTE-CR21
           PERFORM ATUALIZA-SEQ-CRD021
           MOVE SEQ-CR21               TO SEQ-CR20
           MOVE ZEROS TO ST-CRD020
           PERFORM UNTIL ST-CRD020 = "10"
                WRITE REG-CRD020 INVALID KEY
                       PERFORM ATUALIZA-SEQ-CRD021
                       MOVE SEQ-CR21 TO SEQ-CR20
                    NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "I"         TO LOG3-OPERACAO
                       MOVE "CRD020"    TO LOG3-ARQUIVO
                       MOVE "CRP022"    TO LOG3-PROGRAMA
                       MOVE REG-CRD020  TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE
                       MOVE "10" TO ST-CRD020
                END-WRITE
           END-PERFORM.

           MOVE CLASS-CLIENTE-CR21      TO CLASS-CLIENTE-CR24
           MOVE CLIENTE-CR21            TO CLIENTE-CR24
           MOVE SEQ-CR21                TO SEQ-CR24

           WRITE REG-CRD024 INVALID KEY
                 MOVE "CRD024"  TO GS-MENSAGEM-ERRO(15: 07)
                 MOVE ST-CRD024 TO GS-MENSAGEM-ERRO(23: 02)
                 PERFORM ERRO-GRAVACAO
           NOT INVALID KEY
                 MOVE ALL "9" TO CLASS-CLIENTE-CR24
                 MOVE ALL "9" TO CLIENTE-CR24
                 MOVE ALL "9" TO SEQ-CR24
                 READ CRD024 NOT INVALID KEY
                     MOVE NUMERO  TO NUMERO-PROGRAMACAO-CR24
                     REWRITE REG-CRD024
                     END-REWRITE
                 END-READ
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "I"         TO LOG3-OPERACAO
                 MOVE "CRD024"    TO LOG3-ARQUIVO
                 MOVE "CRP022"    TO LOG3-PROGRAMA
                 MOVE REG-CRD024  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE
           END-WRITE

           CLOSE      CRD020 LOG003 CRD024
           OPEN INPUT CRD020 LOG003 CRD024

           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           CLOSE    CRD020 LOG003 CRD024
           OPEN I-O CRD020 LOG003 CRD024

           MOVE GS-ACP-CARTAO      TO CARTAO-CRED-CR20 CODIGO-CG20
           READ CGD020 INVALID KEY
               INITIALIZE REG-CGD020
           END-READ
           MOVE TAXA-CREDITO-CG20  TO TAXA-ADMINIST-CREDITO-CR20
           MOVE TAXA-PARCELA-CG20  TO TAXA-ADMINIST-PARCELA-CR20

           REWRITE REG-CRD020 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG3-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG3-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG3-HORAS
                   MOVE "A"         TO LOG3-OPERACAO
                   MOVE "CRD020"    TO LOG3-ARQUIVO
                   MOVE "CRP022"    TO LOG3-PROGRAMA
                   MOVE REG-CRD020  TO LOG3-REGISTRO
                   WRITE REG-LOG003
                   END-WRITE
                   CONTINUE.

           MOVE CLASS-CLIENTE-CR20 TO CLASS-CLIENTE-CR24
           MOVE CLIENTE-CR20       TO CLIENTE-CR24
           MOVE SEQ-CR20           TO SEQ-CR24
           MOVE GS-ACP-TIPO        TO TIPO-CR24
           MOVE GS-ACP-BANCO       TO BANCO-CR24
           MOVE GS-ACP-CONTA       TO CONTA-CR24
           MOVE GS-ACP-AGENCIA     TO AGENCIA-CR24

           WRITE REG-CRD024 INVALID KEY
               REWRITE REG-CRD024 INVALID KEY
                  MOVE "CRD024"  TO GS-MENSAGEM-ERRO(15: 07)
                  MOVE ST-CRD024 TO GS-MENSAGEM-ERRO(23: 02)
                  PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                  MOVE USUARIO-W   TO LOG3-USUARIO
                  MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                  MOVE WS-DATA-CPU TO LOG3-DATA
                  ACCEPT WS-HORA-SYS FROM TIME
                  MOVE WS-HORA-SYS TO LOG3-HORAS
                  MOVE "A"         TO LOG3-OPERACAO
                  MOVE "CRD024"    TO LOG3-ARQUIVO
                  MOVE "CRP022"    TO LOG3-PROGRAMA
                  MOVE REG-CRD024  TO LOG3-REGISTRO
                  WRITE REG-LOG003
                  END-WRITE
               END-REWRITE
           NOT INVALID KEY
               MOVE ALL "9" TO CLASS-CLIENTE-CR24
               MOVE ALL "9" TO CLIENTE-CR24
               MOVE ALL "9" TO SEQ-CR24
               READ CRD024 NOT INVALID KEY
                   MOVE NUMERO              TO NUMERO-PROGRAMACAO-CR24
                   REWRITE REG-CRD024
                   END-REWRITE
               END-READ
               MOVE USUARIO-W   TO LOG3-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG3-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG3-HORAS
               MOVE "I"         TO LOG3-OPERACAO
               MOVE "CRD024"    TO LOG3-ARQUIVO
               MOVE "CRP022"    TO LOG3-PROGRAMA
               MOVE REG-CRD024  TO LOG3-REGISTRO
               WRITE REG-LOG003
               END-WRITE
           END-WRITE.

           CLOSE      CRD020 LOG003 CRD024
           OPEN INPUT CRD020 LOG003 CRD024

           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ATUALIZA-SEQ-CRD021 SECTION.
           CLOSE    CRD021 LOG001
           OPEN I-O CRD021 LOG001

           READ CRD021 INVALID KEY
                MOVE 1 TO SEQ-CR21
                WRITE REG-CRD021 INVALID KEY
                        MOVE "CRD021"    TO GS-MENSAGEM-ERRO(15: 7)
                        MOVE "ST-CRD021" TO GS-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO
                NOT INVALID KEY
                        MOVE USUARIO-W   TO LOG1-USUARIO
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        MOVE WS-DATA-CPU TO LOG1-DATA
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE WS-HORA-SYS TO LOG1-HORAS
                        MOVE "I"         TO LOG1-OPERACAO
                        MOVE "CRD021"    TO LOG1-ARQUIVO
                        MOVE "CRP022"    TO LOG1-PROGRAMA
                        MOVE REG-CRD021  TO LOG1-REGISTRO
                        WRITE REG-LOG001
                        END-WRITE
                END-WRITE
              NOT INVALID KEY
                  ADD 1 TO SEQ-CR21
                  REWRITE REG-CRD021 INVALID KEY
                        MOVE "CRD021"    TO GS-MENSAGEM-ERRO(15: 7)
                        MOVE "ST-CRD021" TO GS-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO
                  NOT INVALID KEY
                        MOVE USUARIO-W   TO LOG1-USUARIO
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        MOVE WS-DATA-CPU TO LOG1-DATA
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE WS-HORA-SYS TO LOG1-HORAS
                        MOVE "A"         TO LOG1-OPERACAO
                        MOVE "CRD021"    TO LOG1-ARQUIVO
                        MOVE "CRP022"    TO LOG1-PROGRAMA
                        MOVE REG-CRD021  TO LOG1-REGISTRO
                        WRITE REG-LOG001
                        END-WRITE
                  END-REWRITE
           END-READ

           CLOSE      CRD021 LOG001
           OPEN INPUT CRD021 LOG001.

       GRAVA-ANOTACAO-PRE-DEFINIDA SECTION.
           OPEN I-O CRD200 CRD201.
           IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
                                CLOSE CRD200  OPEN I-O CRD200.
           IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
                                CLOSE CRD201  OPEN I-O CRD201.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
           END-PERFORM.
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ        TO SEQ-CR200
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200.
           MOVE ZEROS          TO DATA-RETORNO-CR200
           MOVE USUARIO-W      TO USUARIO-CR200
           MOVE DATA-DIA-I     TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200       TO SEQ-CR201.
           MOVE COD-COMPL-CR200 TO COD-COMPL-CR201.
           MOVE 1               TO SUBSEQ-CR201.
           MOVE SPACES          TO ANOTACAO-CR201
           STRING "DUPLICATA Nº. " NR-DOCTO-CR20 " INCLUIDA ATRAVES DO M
      -    "OVIMENTO DE DUPLICATAS" INTO ANOTACAO-CR201
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.

           CLOSE CRD200 CRD201.

       GRAVA-ANOTACAO SECTION.
           CLOSE    LOG001 LOG002
           OPEN I-O CRD200 CRD201 LOG001 LOG002.
           IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
                                CLOSE CRD200  OPEN I-O CRD200.
           IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
                                CLOSE CRD201  OPEN I-O CRD201.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
           END-PERFORM.
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ      TO SEQ-CR200.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200.
           MOVE ZEROS        TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE DATA-DIA-I   TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                    ADD 1 TO SEQ-CR200
                    CONTINUE
              NOT INVALID KEY
                    MOVE USUARIO-W   TO LOG1-USUARIO
                    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                    MOVE WS-DATA-CPU TO LOG1-DATA
                    ACCEPT WS-HORA-SYS FROM TIME
                    MOVE WS-HORA-SYS TO LOG1-HORAS
                    MOVE "I"         TO LOG1-OPERACAO
                    MOVE "CRD200"    TO LOG1-ARQUIVO
                    MOVE "CRP022"    TO LOG1-PROGRAMA
                    MOVE REG-CRD200  TO LOG1-REGISTRO
                    WRITE REG-LOG001
                    END-WRITE
                    MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200      TO SEQ-CR201.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR201.
           MOVE ZEROS          TO SUBSEQ-CR201.
           MOVE "ALTERACAO EFETUADA NO TITULO            - MOTIVO: "
                  TO ANOTACAO-CR201(1: 80)
           MOVE NR-DOCTO-CR20  TO ANOTACAO-CR201(30: 10).
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
             NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG2-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "I"         TO LOG2-OPERACAO
                   MOVE "CRD201"    TO LOG2-ARQUIVO
                   MOVE "CRP022"    TO LOG2-PROGRAMA
                   MOVE REG-CRD201  TO LOG2-REGISTRO
                   WRITE REG-LOG002
                   END-WRITE
                   MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.

           PERFORM VARYING COL1 FROM 1 BY 80 UNTIL COL1 > 640
              MOVE GS-ANOTACAO(COL1: 80) TO ANOTACAO-W
              MOVE ANOTACAO-W TO ANOTACAO-CR201
              IF ANOTACAO-W <> SPACES
                 ADD 1 TO SUBSEQ-CR201
                 WRITE REG-CRD201 INVALID KEY
                       ADD 1 TO SUBSEQ-CR201
                       WRITE REG-CRD201 NOT INVALID KEY
                             MOVE USUARIO-W   TO LOG2-USUARIO
                             MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                             MOVE WS-DATA-CPU TO LOG2-DATA
                             ACCEPT WS-HORA-SYS FROM TIME
                             MOVE WS-HORA-SYS TO LOG2-HORAS
                             MOVE "I"         TO LOG2-OPERACAO
                             MOVE "CRD201"    TO LOG2-ARQUIVO
                             MOVE "CRP022"    TO LOG2-PROGRAMA
                             MOVE REG-CRD201  TO LOG2-REGISTRO
                             WRITE REG-LOG002
                             END-WRITE
                       END-WRITE
                 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "I"         TO LOG2-OPERACAO
                       MOVE "CRD201"    TO LOG2-ARQUIVO
                       MOVE "CRP022"    TO LOG2-PROGRAMA
                       MOVE REG-CRD201  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
                 END-WRITE
              END-IF
           END-PERFORM.
           CLOSE      CRD200 CRD201 LOG001 LOG002
           OPEN INPUT LOG001 LOG002.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           CLOSE    CRD020 LOG003
           OPEN I-O CRD020 LOG003
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-DATA-MOVTO TO DATA-INV DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO DATA-MOVTO-CR20 DATA-MOVTO-I.
           START CRD020 KEY IS NOT < DATA-MOVTO-CR20
                    INVALID KEY MOVE "10" TO ST-CRD020.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-CRD020 = "10"
              READ CRD020 NEXT RECORD AT END MOVE "10" TO ST-CRD020
              NOT AT END
               IF CLASS-CLIENTE-CR20 NOT NUMERIC OR
                  CLIENTE-CR20 NOT NUMERIC
                    DELETE CRD020 NOT INVALID KEY
                           MOVE USUARIO-W   TO LOG3-USUARIO
                           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                           MOVE WS-DATA-CPU TO LOG3-DATA
                           ACCEPT WS-HORA-SYS FROM TIME
                           MOVE WS-HORA-SYS TO LOG3-HORAS
                           MOVE "E"         TO LOG3-OPERACAO
                           MOVE "CRD020"    TO LOG3-ARQUIVO
                           MOVE "CRP022"    TO LOG3-PROGRAMA
                           MOVE REG-CRD020  TO LOG3-REGISTRO
                           WRITE REG-LOG003
                           END-WRITE
                    END-DELETE
      *          OPÇÃO FOI INCLUÍDA P/ EXCLUSÃO DE LIXO NO ARQUIVO
               ELSE
                IF DATA-MOVTO-CR20 NOT = DATA-MOVTO-I
                                   MOVE "10" TO ST-CRD020
                ELSE
      *          IF SITUACAO-CR20 = 3 OR SITUACAO-CR20 = 4
      *                CONTINUE
      *          ELSE
                    IF RESPONSAVEL-CR20 = USUARIO-W  or "CPD"
                       PERFORM MOVER-DADOS-LISTA
                       MOVE "INSERE-LIST" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                    END-IF
      *          END-IF
                END-IF
               END-IF
              END-READ
           END-PERFORM
           CLOSE      CRD020 LOG003
           OPEN INPUT CRD020 LOG003.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES TO GS-LINDET
           MOVE CLASS-CLIENTE-CR20 TO GS-LINDET(1: 1) CLASSIF-CG10
           MOVE CLIENTE-CR20       TO CLIENTE-E CODIGO-CG10
           MOVE CLIENTE-CR20       TO GS-LINDET(03: 09)
           MOVE SEQ-CR20           TO SEQ-E
           READ CGD010 INVALID KEY
                   MOVE "************" TO COMPRADOR-CG10.
           MOVE SEQ-CR20           TO GS-LINDET(12: 06)
           MOVE COMPRADOR-CG10(1: 10)   TO GS-LINDET(18: 11)
           MOVE DESCRICAO-CR20     TO GS-LINDET(29: 31)
           MOVE PORTADOR-CR20      TO GS-LINDET(60: 05)
           MOVE SITUACAO-CR20      TO GS-LINDET(65: 02)
           MOVE CODREDUZ-APUR-CR20 TO GS-LINDET(67: 04)
           MOVE RESPONSAVEL-CR20   TO GS-LINDET(71: 06)
           MOVE DATA-VENCTO-CR20   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(77: 11)
           MOVE VALOR-TOT-CR20     TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(88: 10).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP022" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           OPEN OUTPUT RELAT

      *    IF IMPRESSORA-W = 01
      *       WRITE REG-RELAT FROM COND-HP BEFORE 0
      *    ELSE
      *       WRITE REG-RELAT FROM COND-EP BEFORE 0.


           MOVE 0 TO QUANTIDADE

           MOVE 1 TO GS-CONT
           MOVE SPACES TO GS-LINDET
           MOVE "LER-LB1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
               IF GS-LINDET(100:1) = "X"
                  ADD 1 TO QUANTIDADE
                  EVALUATE QUANTIDADE
                      WHEN 1 PERFORM IMPRIMIR-PROGRAMACAO
                      WHEN 2 PERFORM IMPRIMIR-PROGRAMACAO
                      WHEN 3 PERFORM IMPRIMIR-PROGRAMACAO
                             MOVE 0 TO QUANTIDADE
                  END-EVALUATE
               END-IF
               ADD 1 TO GS-CONT
               MOVE SPACES TO GS-LINDET
               MOVE "LER-LB1" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

      *    MOVE SPACES TO REG-RELAT.
      *    IF IMPRESSORA-W = 01
      *       WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
      *    ELSE
      *       WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.

       IMPRIMIR-PROGRAMACAO SECTION.

           MOVE GS-LINDET(1: 1) TO CLASS-CLIENTE-CR20
           MOVE GS-LINDET(3: 8) TO CLIENTE-CR20
           MOVE GS-LINDET(12: 5) TO SEQ-CR20
           PERFORM CARREGAR-DADOS

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS

           MOVE WS-DIA-CPU            TO DET-DIA
           MOVE WS-MES-CPU            TO DET-MES
           MOVE WS-ANO-CPU            TO DET-ANO

           ACCEPT WS-HORA-SYS FROM TIME
           MOVE WS-HO-SYS             TO DET-HORA
           MOVE WS-MI-SYS             TO DET-MINU

           MOVE NUMERO-PROGRAMACAO-CR24 TO DET-NUMERO

           MOVE GS-DATA-EMISSAO  TO DET-EMISSAO
           MOVE GS-DATA-VENCTO   TO DET-VENCTO
           MOVE GS-NR-DOCTO      TO DET-NUMDOC
           MOVE GS-DESCR-CLIENTE TO DET-FAVORECIDO
           MOVE GS-VALOR-TOTAL   TO DET-VALOR

           MOVE GS-VALOR-TOTAL   TO LINK-VALOR

           CALL "EXTENSO" USING LINK-EXTENSO
           CANCEL "EXTENSO"

           MOVE LINK-DESCRICAO      TO DET-EXTENSO

           PERFORM LER-CONTA

           MOVE GS-DESCRICAO        TO DET-REFERENTE
           MOVE GS-ACP-BANCO        TO CODIGO-CAD30
           READ CAD030 INVALID KEY
               MOVE SPACES TO DESCRICAO-CAD30
           END-READ
           MOVE SPACES TO DET-BANCO
           STRING GS-ACP-BANCO " - " DESCRICAO-CAD30 INTO DET-BANCO
           MOVE GS-ACP-AGENCIA       TO DET-AGENCIA
           MOVE GS-ACP-CONTA         TO DET-CONTA-CORRENTE

           MOVE GS-TIPO-DOCTO        TO DET-TIPODOC

           MOVE GS-RESPONSAVEL  TO DET-RESPONSAVEL

           MOVE SPACES     TO LINDET
           WRITE REG-RELAT FROM LINDET
           WRITE REG-RELAT FROM LINDET
           WRITE REG-RELAT FROM DET-01
           MOVE SPACES     TO LINDET
           WRITE REG-RELAT FROM LINDET
           WRITE REG-RELAT FROM DET-02
           WRITE REG-RELAT FROM DET-02a
           WRITE REG-RELAT FROM DET-03
           WRITE REG-RELAT FROM DET-04
           WRITE REG-RELAT FROM DET-05
           MOVE SPACES     TO LINDET
           WRITE REG-RELAT FROM LINDET


           MOVE GS-COD-APURACAO     TO CODIGO-REDUZ-CX20
           READ CXD020 INVALID KEY
                MOVE ZEROS          TO CODIGO-COMPL-CX20
                MOVE "*****"        TO DESCRICAO-CX20
           END-READ

           STRING "CONTABIL.: " CODIGO-COMPL-CX20(1:1) "."
                                  CODIGO-COMPL-CX20(2:2) "."
                                  CODIGO-COMPL-CX20(4:2) "."
                                  CODIGO-COMPL-CX20(6:2) " => REDUZ.: "
                                  CODIGO-REDUZ-CX20
                                  " => " DESCRICAO-CX20 INTO LINDET
           WRITE REG-RELAT FROM LINDET


           MOVE SPACES     TO LINDET
           WRITE REG-RELAT FROM LINDET
           WRITE REG-RELAT FROM DET-06
           MOVE SPACES     TO LINDET
           WRITE REG-RELAT FROM LINDET
           WRITE REG-RELAT FROM DET-07
           WRITE REG-RELAT FROM DET-08
           MOVE SPACES     TO LINDET
           WRITE REG-RELAT FROM LINDET
           WRITE REG-RELAT FROM LINDET
           WRITE REG-RELAT FROM DET-10
           WRITE REG-RELAT FROM DET-11

           MOVE SPACES     TO LINDET
      *    WRITE REG-RELAT FROM LINDET
           WRITE REG-RELAT FROM LINDET.
       IMPRIMIR-PROGRAMACAO-FIM.
           EXIT.


       CABECALHO SECTION.
           ADD 1      TO PAG-W
           MOVE PAG-W TO PAG-REL

           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 2
           WRITE REG-RELAT FROM CAB03
           WRITE REG-RELAT FROM CAB04
           WRITE REG-RELAT FROM CAB05
           WRITE REG-RELAT FROM CAB03
           MOVE 5 TO LIN.

       IMPRIME-TELA SECTION.
           OPEN OUTPUT RELAT
           WRITE REG-RELAT FROM CAB02A AFTER 0
           WRITE REG-RELAT FROM CAB03A

           MOVE SPACES TO LINDETA-REL
           MOVE "DATA MOVTO...: "   TO LINDETA-REL(1: 15)
           MOVE GS-DATA-MOVTO       TO DATA-E
           MOVE DATA-E              TO LINDETA-REL(16: 31)
           MOVE "CLASSIFICACAO.: "  TO LINDETA-REL(50: 16)
           MOVE GS-CLASSIFICACAO    TO LINDETA-REL(66: 15)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO LINDETA-REL.
           MOVE "CLIENTE......: "   TO LINDETA-REL(1: 15)
           MOVE GS-COD-CLIENTE      TO LINDETA-REL(16: 10)
           MOVE GS-DESCR-CLIENTE    TO LINDETA-REL(26: 30)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO LINDETA-REL.
           MOVE "NR.DOCTO.....: "   TO LINDETA-REL(1: 15)
           MOVE GS-NR-DOCTO         TO LINDETA-REL(16: 31)
           MOVE "DATA EMISSAO..: "  TO LINDETA-REL(50: 16)
           MOVE GS-DATA-EMISSAO     TO DATA-E
           MOVE DATA-E              TO LINDETA-REL(66: 15)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO LINDETA-REL.
           MOVE "DESCRICAO....: "   TO LINDETA-REL(1: 15)
           MOVE GS-DESCRICAO        TO LINDETA-REL(16: 31)
           MOVE "TIPO DOCTO....: "  TO LINDETA-REL(50: 16)
           MOVE GS-TIPO-DOCTO       TO LINDETA-REL(66: 15)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO LINDETA-REL.
           MOVE "VENDEDOR.....: "   TO LINDETA-REL(1: 15)
           MOVE GS-VENDEDOR         TO LINDETA-REL(16: 7)
           MOVE GS-DESCR-VENDEDOR   TO LINDETA-REL(23: 20)
           MOVE "TIPO MOEDA....: "  TO LINDETA-REL(50: 16)
           MOVE GS-TIPO-MOEDA       TO LINDETA-REL(66: 20)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO LINDETA-REL
           MOVE "PORTADOR.....: "   TO LINDETA-REL(1: 15)
           MOVE GS-PORTADOR         TO LINDETA-REL(16: 4)
           MOVE GS-DESCR-PORTADOR   TO LINDETA-REL(22: 20)
           MOVE "OUTRO DOCTO...: "  TO LINDETA-REL(50: 16)
           MOVE GS-OUTRO-DOCTO      TO LINDETA-REL(66: 20)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO LINDETA-REL
           MOVE "SITUACAO.....: "     TO LINDETA-REL(1: 15)
           MOVE GS-SITUACAO-TIT       TO LINDETA-REL(16: 4)
           MOVE GS-DESCR-SITUACAO-TIT TO LINDETA-REL(20: 20)
           MOVE "RESPONSAVEL...: "    TO LINDETA-REL(50: 16)
           MOVE GS-RESPONSAVEL        TO LINDETA-REL(66: 20)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO LINDETA-REL
           MOVE "COD.APURACAO.: "   TO LINDETA-REL(1: 15)
           MOVE GS-COD-APURACAO     TO LINDETA-REL(16: 6)
           MOVE GS-DESCR-APURACAO   TO LINDETA-REL(22: 20)
           MOVE "CARTEIRA......: "  TO LINDETA-REL(52: 16)
           MOVE GS-CARTEIRA         TO LINDETA-REL(68: 20)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO LINDETA-REL
           MOVE "DATA VENCTO..: "   TO LINDETA-REL(1: 15)
           MOVE GS-DATA-VENCTO      TO DATA-E
           MOVE DATA-E              TO LINDETA-REL(16: 31)
           MOVE "VALOR TOTAL...: "  TO LINDETA-REL(50: 16)
           MOVE GS-VALOR-TOTAL      TO VALOR-E
           MOVE VALOR-E             TO LINDETA-REL(66: 20)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO LINDETA-REL
           MOVE "NR.NT.FISCAL.: "   TO LINDETA-REL(1: 15)
           MOVE GS-NR-NOTA-FISCAL   TO LINDETA-REL(16: 31)
           MOVE "DATA NT.FISCAL: "  TO LINDETA-REL(50: 16)
           MOVE GS-DATA-NTA-FISCAL  TO DATA-E
           MOVE DATA-E              TO LINDETA-REL(66: 20)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO LINDETA-REL
           MOVE "DIGITADOR....: "   TO LINDETA-REL(1: 15)
           MOVE GS-DIGITADOR        TO LINDETA-REL(16: 31)
           WRITE REG-RELAT FROM LINDETA AFTER 2

           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.
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
           move "CRP022"            to logacess-programa
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
           CLOSE CAD018 CRD001 CRD020 CRD021 CRD022 CGD001 CGD010 CGD020
                 CXD020 CAD002 LOG001 LOG002 LOG003
                 CRD024 CAD030.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
