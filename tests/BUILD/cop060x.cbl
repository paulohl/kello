       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP060X.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 28/07/1999.
      *FUNÇÃO: Movimento de Eventos  - SUBPROGRAMA

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CAPX010.
           COPY COPX003.
           COPY COPX040.
           COPY COPX049.
           COPY COPX041.
           COPY COPX060.
           COPY COPX060C.
           COPY COPX061.
           COPY CGPX010.
           COPY CGPX012.
           COPY IEPX011.
           COPY PARX001.
           COPY LOGX002.
           COPY LOGX004.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW010.
       COPY COPW003.
       COPY COPW040.
       COPY COPW049.
       COPY COPW041.
       COPY COPW060.
       COPY COPW060C.
       COPY COPW061.
       COPY CGPW010.
       COPY CGPW012.
       COPY IEPW011.
       COPY PARW001.
       COPY LOGW002.
       COPY LOGW004.
           COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP060X.CPB".
           COPY "COP060X.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD012             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD049             PIC XX       VALUE SPACES.
           05  ST-COD041             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-COD060C            PIC XX       VALUE SPACES.
           05  ST-COD061             PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ST-LOG004             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
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
           05  DT-PREV-REAL-W        PIC 9(1)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
           05  WS-STATUS-ANALISE     PIC 9(02) VALUE ZEROS.
           05  WS-STATUS-REVENDIDO   PIC 9(02) VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  I                     PIC 99       VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  AUX-CONTRATO          PIC 9(04) VALUE ZEROS.
           05  AUX-ITEM              PIC 9(03) VALUE ZEROS.

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
           "CONFERENCIA DO MOVIMENTO DE EVENTO".
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
           "CONT IT  EVENTO                PART T DT-REALIZ. DT-SOLICIT
      -    "F V C B F A COMI HORARIO    LOCAL                      ".
       01  CAB05.
           05  FILLER              PIC X(130)  VALUE
           "ENDERECO                       REFERENCIA
      -    "  PESSOA-CONTATO                 UNIFORME             ORGANI
      -    "ZADOR   ".
       01  CAB06.
           05  FILLER              PIC X(130)  VALUE
           "OBSERVACAO
      -    "                     DATA-CANC. HORA  NR-PLAN. NR-REP.".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.
      * ------------- IMPRESSÃO INDIVIDUAL ----------------------
       01  LINDET01.
           05  FILLER              PIC X(77)  VALUE
           "MOVIMENTO DE EVENTO INDIVIDUAL ".
           05  FILLER              PIC X(14)  VALUE "DATA EMISSAO: ".
           05  EMISSAO1-REL        PIC 99/99/9999.
       01  LINDET02.
           05  FILLER              PIC X(101) VALUE ALL "=".
       01  LINDET03.
           05  FILLER              PIC X(15)  VALUE "CONTRATO/ITEM: ".
           05  CONTRATO-REL        PIC Z(4)   BLANK WHEN ZEROS.
           05  FILLER              PIC X      VALUE "/".
           05  ITEM-REL            PIC Z(3)   BLANK WHEN ZEROS.
           05  FILLER              PIC X(33)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "COD.EVENTO:    ".
           05  EVENTO-REL          PIC ZZZ    BLANK WHEN ZEROS.
           05  FILLER              PIC X      VALUE SPACES.
           05  NOME-EVENTO-REL     PIC X(20)  VALUE SPACES.
       01  LINDET04.
           05  FILLER              PIC X(15)  VALUE "QTDE PARTICIP: ".
           05  QTDE-PARTIC-REL     PIC Z(4)   BLANK WHEN ZEROS.
           05  FILLER              PIC X(37)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "ORGANIZADOR:   ".
           05  ORGANIZADOR-REL     PIC X(15)  VALUE SPACES.
       01  LINDET05.
           05  FILLER              PIC X(15)  VALUE "DATA REALIZAC: ".
           05  DATA-REALIZ-REL     PIC ZZ/ZZ/ZZZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(31)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "DATA SOLICIT.: ".
           05  DATA-SOLICIT-REL    PIC ZZ/ZZ/ZZZZ BLANK WHEN ZEROS.
       01  LINDET06.
           05  FILLER              PIC X(15)  VALUE "SOLICITANTE..: ".
           05  SOLICITANTE-REL     PIC Z(4)   BLANK WHEN ZEROS.
           05  FILLER              PIC X      VALUE SPACES.
           05  NOME-SOLICIT-REL    PIC X(36)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "QTDE TELAO...: ".
           05  QTDE-TELAO-REL      PIC Z      BLANK WHEN ZEROS.
       01  LINDET07.
           05  FILLER              PIC X(15)  VALUE "FOTO.........: ".
           05  FOTO-REL            PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "VIDEO........: ".
           05  VIDEO-REL           PIC X(30)  VALUE SPACES.
       01  LINDET08.
           05  FILLER              PIC X(15)  VALUE "BECA.........: ".
           05  BECA-REL            PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "CLIP.........: ".
           05  CLIP-REL            PIC X(30)  VALUE SPACES.
       01  LINDET09.
           05  FILLER              PIC X(15)  VALUE "FAX..........: ".
           05  FAX-REL             PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "APROVACAO....: ".
           05  APROVACAO-REL       PIC X(30)  VALUE SPACES.
       01  LINDET10.
           05  FILLER              PIC X(15)  VALUE "NR.PLANEJAM..: ".
           05  NR-PLANEJ-REL       PIC ZZZZ.ZZZZ.
           05  FILLER              PIC X(32)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "NR.REL.REPORT: ".
           05  NR-REL-REPORT-REL   PIC ZZZ.ZZZ.
       01  LINDET11.
           05  FILLER              PIC X(15)  VALUE "OBSERVACAO...: ".
           05  OBS-REL             PIC X(80)  VALUE SPACES.
       01  LINDET12.
           05  FILLER              PIC X(15)  VALUE "LOCAL........: ".
           05  LOCAL-REL           PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "HORARIO......: ".
           05  HORARIO-REL         PIC X(30)  VALUE SPACES.
       01  LINDET13.
           05  FILLER              PIC X(15)  VALUE "ENDERECO.....: ".
           05  ENDERECO-REL        PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "PONTO REFEREN: ".
           05  PONTO-REFER-REL     PIC X(30)  VALUE SPACES.
       01  LINDET14.
           05  FILLER              PIC X(15)  VALUE "PESSOA CONTAT: ".
           05  PESSOA-CONT-REL     PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "UNIFORME.....: ".
           05  UNIFORME-REL        PIC X(30)  VALUE SPACES.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 WS-DATA-SYS.
          05 WS-DATA-CPU               PIC 9(08).
          05 FILLER REDEFINES WS-DATA-CPU.
             10 WS-ANO-CPU             PIC 9(04).
             10 WS-MES-CPU             PIC 9(02).
             10 WS-DIA-CPU             PIC 9(02).
          05 FILLER                    PIC X(13).

       01 AUX-DATA                     PIC 9(08).
       01 FILLER REDEFINES AUX-DATA.
          05 AUX-ANO                   PIC 9(04).
          05 AUX-MES                   PIC 9(02).
          05 AUX-DIA                   PIC 9(02).

       01 WS-HORA-SYS.
          05 WS-HO-SYS                 PIC 9(02).
          05 WS-MI-SYS                 PIC 9(02).
          05 WS-SE-SYS                 PIC 9(02).
          05 WS-MS-SYS                 PIC 9(02).

       LINKAGE SECTION.
           COPY "PARAMETR".

       01  STRING-1               PIC X(65) VALUE SPACES.
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD012.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD049" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD049.
           MOVE "COD041" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD041.
           MOVE "COD060" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "COD060C" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD060C.
           MOVE "COD061" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD061.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "PAR001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "LOG002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOG004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG004.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS
           OPEN I-O   COD060 COD060C COD061 LOG002 COD049
           CLOSE      COD060 COD060C COD061 LOG002 COD049
           OPEN INPUT COD060 COD060C COD061 PAR001 CAD010 COD049

           OPEN INPUT IED011 COD003 CGD010 COD040 COD041 CGD012 CAD004.
           IF ST-COD060 = "35"
              CLOSE COD060      OPEN OUTPUT COD060
              CLOSE COD060      OPEN I-O COD060
           END-IF.
           IF ST-COD060C = "35"
              CLOSE COD060C     OPEN OUTPUT COD060C
              CLOSE COD060C     OPEN I-O    COD060C
           END-IF.
           IF ST-COD061 = "35"
              CLOSE COD061      OPEN OUTPUT COD061
              CLOSE COD061      OPEN I-O COD061
           END-IF.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD012 <> "00"
              MOVE "ERRO ABERTURA CGD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD049 <> "00"
              MOVE "ERRO ABERTURA COD049: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD049 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD041 <> "00"
              MOVE "ERRO ABERTURA COD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD041 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060C <> "00"
              MOVE "ERRO ABERTURA COD060C: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060C TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD061 <> "00"
              MOVE "ERRO ABERTURA COD061: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD061 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE 1 TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                MOVE "Parametrização do Brinde Não Cadastrada"
                TO GS-MENSAGEM-ERRO
                PERFORM CARREGA-MENSAGEM-ERRO
           NOT INVALID KEY
                IF STATUS-REVENDIDO-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-REVENDIDO-PAR001
                END-IF
                MOVE STATUS-REVENDIDO-PAR001 TO WS-STATUS-REVENDIDO
                IF STATUS-ANALISE-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-ANALISE-PAR001
                END-IF
                MOVE STATUS-ANALISE-PAR001   TO WS-STATUS-ANALISE
           END-READ

           CLOSE PAR001

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "COP060X"           to logacess-programa
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

           INITIALIZE NR-PLANEJ-CO60

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1
                      PERFORM REGRAVA-DADOS
                   ELSE
                      PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   PERFORM CARREGAR-CURSOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
                   PERFORM VALIDAR-SENHA
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4)  TO NR-CONTRATO-CO60
                                            GS-CONTRATO
                   MOVE GS-LINDET(6: 3)  TO ITEM-CO60
                   PERFORM VERIFICA-CONTRATO
                   PERFORM VALIDAR-SENHA
                   PERFORM CARREGAR-DADOS
                   PERFORM CARREGAR-CURSOS
               WHEN GS-LE-EVENTO-TRUE
                   PERFORM LE-EVENTO
               WHEN GS-LE-COMISSAO-TRUE
                   PERFORM LE-COMISSAO
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-CHAMADA-SUBPROG-TRUE
                    PERFORM CHAMADA-SUBPROGRAMA
               WHEN GS-IMPRIME-TELA-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-TELA
                    END-IF
               WHEN GS-VERIFICAR-CONTRATO-TRUE
                    PERFORM VERIFICA-CONTRATO
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LE-CIDADE
               WHEN GS-POPUP-CIDADE-TRUE
                    PERFORM POPUP-CIDADE
               WHEN GS-VALIDAR-DTREALIZACAO-TRUE
                    PERFORM VALIDAR-DTREALIZACAO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VALIDAR-DTREALIZACAO SECTION.
           MOVE GS-DATA-REALIZACAO(1:2) TO AUX-DIA
           MOVE GS-DATA-REALIZACAO(3:2) TO AUX-MES
           MOVE GS-DATA-REALIZACAO(5:4) TO AUX-ANO

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS

           IF WS-DATA-CPU > AUX-DATA
              MOVE "Data de realização antecede a data atual?"
                TO MENSAGEM
              MOVE "Q" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              IF RESP-MSG = "S"
                 MOVE 0 TO GS-FLAG-CRITICA
              END-IF
           ELSE
              IF AUX-DATA = 0
                 MOVE "Data de realização não informada" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM.

       LE-CIDADE SECTION.
           MOVE GS-CIDADE-EVENTO   TO CIDADE
           READ CAD010 INVALID KEY
                MOVE "****"        TO NOME-CID
                MOVE "**"          TO UF-CID
           END-READ
           MOVE SPACES               TO GS-DESC-CIDADE-EVENTO
           STRING NOME-CID " - " UF-CID INTO GS-DESC-CIDADE-EVENTO.

       POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE-EVENTO
           PERFORM LE-CIDADE.

       VALIDAR-SENHA SECTION.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           MOVE "SENHA13"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITA-BOTOES" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               ENABLE-OBJECT PB1
               ENABLE-OBJECT PB3.

       VERIFICA-CONTRATO SECTION.
           ENABLE-OBJECT PB1
           ENABLE-OBJECT PB3.

           MOVE GS-CONTRATO            TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE "Número do Contrato Não Cadastrado no (COP040)"
                  TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                EVALUATE STATUS-CO40
                    WHEN WS-STATUS-REVENDIDO
                         MOVE "Contrato com STATUS Revendido" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                         DISABLE-OBJECT PB1
                         DISABLE-OBJECT PB3
                    WHEN WS-STATUS-ANALISE
                         MOVE "Contrato com STATUS Analise" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                         DISABLE-OBJECT PB1
                         DISABLE-OBJECT PB3
                    WHEN OTHER
                         MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO49
                         READ COD049 INVALID KEY
                              MOVE 0 TO GS-FLAG-CRITICA
                              IF GS-DATA-SOLICIT = 0
                                 MOVE FUNCTION CURRENT-DATE
                                   TO WS-DATA-SYS
                                 STRING WS-DIA-CPU
                                        WS-MES-CPU
                                        WS-ANO-CPU
                                   INTO GS-DATA-SOLICIT
                              END-IF
                              REFRESH-OBJECT PRINCIPAL
                         NOT INVALID KEY
                              IF CANCELADO-CO49 = 1
                                 MOVE "Contrato ENCERRADO" to mensagem
                                 move "C" to tipo-msg
                                 perform exibir-mensagem
                              ELSE
                                 MOVE 0 TO GS-FLAG-CRITICA
                                 MOVE 0 TO GS-FLAG-CRITICA
                                 IF GS-DATA-SOLICIT = 0
                                    MOVE FUNCTION CURRENT-DATE
                                      TO WS-DATA-SYS
                                    STRING WS-DIA-CPU
                                           WS-MES-CPU
                                           WS-ANO-CPU
                                      INTO GS-DATA-SOLICIT
                                 END-IF
                                 REFRESH-OBJECT PRINCIPAL
                              END-IF
                         END-READ
                END-EVALUATE.

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004

           MOVE "SENHA13"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITA-BOTOES" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               ENABLE-OBJECT PB1
               ENABLE-OBJECT PB3.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMADA-SUBPROGRAMA SECTION.
           MOVE STRING-1(1: 4) TO GS-CONTRATO.
           MOVE STRING-1(5: 3) TO GS-NR-ITEM
           MOVE STRING-1(9: 2) TO IMPRESSORA-W
           MOVE STRING-1(11: 3) TO COD-USUARIO-W.


           IF GS-CONTRATO <> ZEROS  PERFORM CARREGA-ULTIMOS.
           MOVE STRING-1(1: 4) TO GS-CONTRATO.
           MOVE STRING-1(5: 3) TO GS-NR-ITEM
           IF GS-CONTRATO <> ZEROS PERFORM CARREGAR-CURSOS.
           MOVE STRING-1(1: 4)    TO NR-CONTRATO-CO60
           MOVE STRING-1(5: 3)    TO ITEM-CO60
           IF ITEM-CO60 <> ZEROS  PERFORM CARREGAR-DADOS
                                  MOVE 1 TO GS-TIPO-GRAVACAO.

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004

           MOVE "SENHA13"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITA-BOTOES" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               ENABLE-OBJECT PB1
               ENABLE-OBJECT PB3.

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 2 PERFORM CARREGA-POP-UP-EVENTO
             WHEN 3 PERFORM CARREGA-POP-UP-COMISSAO
           END-EVALUATE.
       CARREGA-POP-UP-COMISSAO SECTION.
      *    comisao vem do cadastro do contrato
           MOVE GS-CONTRATO TO CODIGO-CG12(1: 4)
           MOVE ZEROS       TO CODIGO-CG12(5: 4)
           START CGD012 KEY IS NOT < CODIGO-CG12 INVALID KEY
              MOVE "10" TO ST-CGD012.
           PERFORM UNTIL ST-CGD012 = "10"
             READ CGD012 NEXT RECORD AT END MOVE "10" TO ST-CGD012
               NOT AT END
                  MOVE CODIGO-CG12(1: 4)  TO CONTRATO-W
                  IF CONTRATO-W <> GS-CONTRATO
                     MOVE "10" TO ST-CGD012
                  ELSE
                     MOVE 0              TO CLASSIF-CG10
                     MOVE CODIGO-CG12    TO CODIGO-CG10
                     READ CGD010 INVALID KEY
                          MOVE SPACES TO COMPRADOR-CG10
                     END-READ
                     MOVE COMPRADOR-CG10 TO GS-LINDET1(1: 31)
                     MOVE CODIGO-CG10    TO GS-LINDET1(32: 8)

                     MOVE "INSERE-POP-UP-SOLICIT" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
             END-READ
           END-PERFORM.
       CARREGA-POP-UP-EVENTO SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-CO03.
           START COD003 KEY IS NOT < NOME-CO03 INVALID KEY
                 MOVE "10" TO ST-COD003.
           PERFORM UNTIL ST-COD003 = "10"
              READ COD003 NEXT RECORD AT END MOVE "10" TO ST-COD003
               NOT AT END
                MOVE NOME-CO03(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-COD003
                ELSE
                  MOVE NOME-CO03       TO GS-LINDET1(1: 32)
                  MOVE CODIGO-CO03     TO GS-LINDET1(33: 05)
                  MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-LINDET1(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE GS-LINDET1(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.
       ITEM-SELECIONADO SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 2
               MOVE GS-LINDET1(33: 5) TO GS-COD-EVENTO
               MOVE GS-LINDET1(1: 30) TO GS-NOME-EVENTO
             WHEN 3
               MOVE GS-LINDET1(32: 8)  TO GS-SOLICITANTE
               MOVE GS-LINDET1(1: 30)  TO GS-NOME-SOLICITANTE
           END-EVALUATE.
       LE-COMISSAO SECTION.
           MOVE 0                  TO CLASSIF-CG10.
           MOVE GS-CONTRATO        TO CODIGO-CG10(1: 4)
           MOVE GS-SOLICITANTE      TO CODIGO-CG10(5: 4)
           READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10     TO GS-NOME-SOLICITANTE.
       LE-EVENTO SECTION.
           MOVE GS-COD-EVENTO          TO CODIGO-CO03.
           READ COD003 INVALID KEY MOVE "********" TO NOME-CO03.
           MOVE NOME-CO03          TO GS-NOME-EVENTO.

       CARREGAR-DADOS SECTION.
           START COD060 KEY IS = CHAVE-CO60 INVALID KEY CONTINUE.
           READ COD060 INVALID KEY INITIALIZE REG-COD060.
           MOVE NR-CONTRATO-CO60     TO  GS-CONTRATO
           MOVE ITEM-CO60            TO  GS-NR-ITEM
           MOVE CODEVENTO-CO60       TO  GS-COD-EVENTO CODIGO-CO03
           READ COD003 INVALID KEY MOVE "*******" TO NOME-CO03.
           MOVE NOME-CO03            TO  GS-NOME-EVENTO
           MOVE QT-PARTICIPANTE-CO60 TO  GS-NR-PARTICIPANTES
           MOVE QT-TELAO-CO60        TO  GS-QTDE-TELAO
           MOVE DATAREALIZA-CO60     TO  DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO  GS-DATA-REALIZACAO
           MOVE DATA-SOLIC-CO60      TO  GS-DATA-SOLICIT
           EVALUATE FOTO-CO60
             WHEN 0 MOVE "0-Não"     TO GS-FOTO
             WHEN 1 MOVE "1-Sim"     TO GS-FOTO
           END-EVALUATE.
           EVALUATE VIDEO-CO60
             WHEN 0 MOVE "0-Não"     TO GS-VIDEO
             WHEN 1 MOVE "1-Sim"     TO GS-VIDEO
           END-EVALUATE.
           EVALUATE BECA-CO60
             WHEN 0 MOVE "0-Não"     TO GS-BECA
             WHEN 1 MOVE "1-Sim"     TO GS-BECA
           END-EVALUATE.
           EVALUATE CLIP-CO60
             WHEN 0 MOVE "0-Não"     TO GS-CLIP
             WHEN 1 MOVE "1-Sim"     TO GS-CLIP
           END-EVALUATE.
           EVALUATE FAX-CO60
             WHEN 0 MOVE "0-Não"     TO GS-FAX
             WHEN 1 MOVE "1-Sim"     TO GS-FAX
           END-EVALUATE.
           EVALUATE APROVACAO-CO60
             WHEN 0 MOVE "0-Não"     TO GS-APROVACAO
             WHEN 1 MOVE "1-Sim"     TO GS-APROVACAO
           END-EVALUATE.
           MOVE 0                    TO CLASSIF-CG10
           MOVE NR-CONTRATO-CO60     TO CODIGO-CG10(1: 4).
           MOVE COD-COMISSAO-CO60    TO GS-SOLICITANTE CODIGO-CG10(5: 4)
           READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10       TO GS-NOME-SOLICITANTE.
           MOVE HORARIO-CO60         TO GS-HORARIO.
           MOVE LOCAL-CO60           TO GS-LOCAL
           MOVE ENDERECO-CO60        TO GS-ENDERECO
           MOVE REFERENCIA-CO60      TO GS-PONTO-REFERENCIA
           MOVE PESSOA-CONTATO-CO60  TO GS-PESSOA-CONTATO
           MOVE UNIFORME-CO60        TO GS-UNIFORME
           MOVE ORGANIZADOR-CO60     TO GS-ORGANIZADOR
           MOVE OBSERVACAO-CO60      TO GS-OBSERVACAO
           MOVE NR-PLANEJ-CO60       TO GS-NR-PLANEJAMENTO
           MOVE NR-REL-REPOR-CO60    TO GS-NR-REL-REPORT
           MOVE DATA-CANCELAM-CO60   TO GS-DATA-CANCELAMENTO
           MOVE HORA-CANCELAM-CO60   TO GS-HORA-CANCELAMENTO
           MOVE DT-PREV-REAL-CO60    TO GS-DT-PREV-REAL
           EVALUATE GS-DT-PREV-REAL
               WHEN 1 MOVE "DATA-PREVISTA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 2 MOVE "DATA-REALIZADA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM.
           MOVE CIDADE-CO60          TO CIDADE
           READ CAD010 INVALID KEY
                INITIALIZE NOME-CID
                           CIDADE
                           UF-CID
           END-READ
           MOVE CIDADE               TO GS-CIDADE-EVENTO
           MOVE SPACES               TO GS-DESC-CIDADE-EVENTO
           STRING NOME-CID " - " UF-CID INTO GS-DESC-CIDADE-EVENTO


           MOVE NR-CONTRATO-CO60       TO NR-CONTRATO-CO60C
           MOVE ITEM-CO60              TO ITEM-CO60C
           READ COD060C INVALID KEY
                MOVE SPACES            TO GS-USUARIO-CADASTRO
                MOVE ZEROS             TO GS-DATA-CADASTRO
           NOT INVALID KEY
                MOVE USUARIO-CAD-CO60C TO GS-USUARIO-CADASTRO
                MOVE DATA-CAD-CO60C    TO GS-DATA-CADASTRO
           END-READ

      *    initialize gs-usuario-cadastro
      *               gs-data-cadastro
      *
      *    open input log004
      *
      *    initialize reg-log004
      *    move "COD060" to log4-arquivo
      *    start log004 key is not less log4-ch-arquivo invalid key
      *          move "10" to st-log004
      *    end-start
      *
      *    perform until st-log004 = "10"
      *          read log004 next at end
      *               move "10" to st-log004
      *          not at end
      *               if log4-arquivo <> "COD060"
      *                  move "10" to st-log004
      *               else
      *                  move function numval(log4-registro(1:4)) to
      *                                       aux-contrato
      *                  move function numval(log4-registro(5:3)) to
      *                                       aux-item
      *                  if aux-contrato = gs-contrato and
      *                     aux-item     = gs-nr-item
      *                     move log4-usuario to gs-usuario-cadastro
      *                     string log4-dia log4-mes log4-ano into
      *                                          gs-data-cadastro
      *                     move "10" to st-log004
      *                  end-if
      *               end-if
      *          end-read
      *    end-perform
      *
      *    close log004

           IF NR-PLANEJ-CO60 > 0
              MOVE "Este Evento Não pode ser ALTERADO a Data de Realizaç
      -       "ão, pois está relacionado a um PLANEJAMENTO" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              MOVE "DESABILITA-BOTOES2" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              disable-object pb1
              disable-object pb3
           ELSE
              MOVE "HABILITA-BOTOES" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              ENABLE-OBJECT PB1
              ENABLE-OBJECT PB3.

       exibir-mensagem section.
           move 1 to gs-flag-critica
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.


       CARREGAR-CURSOS SECTION.
           MOVE GS-CONTRATO          TO NR-CONTRATO-CO41.
           MOVE ZEROS                TO CURSO-CO41
           MOVE SPACES               TO TURMA-CO41.
           START COD041 KEY IS NOT < CHAVE-CO41 INVALID KEY
                 MOVE "10" TO ST-COD041.
           MOVE ZEROS TO GS-CONT-TURMA.
           PERFORM UNTIL ST-COD041 = "10"
             READ COD041 NEXT RECORD AT END MOVE "10" TO ST-COD041
               NOT AT END
                 IF NR-CONTRATO-CO41 <> GS-CONTRATO
                    MOVE "10" TO ST-COD041
                 ELSE
                    MOVE CURSO-CO41   TO CODIGO-IE11
                                         GS-LINDET-TURMA(1: 3)
                    READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11
                    END-READ
                    MOVE NOME-IE11    TO GS-LINDET-TURMA(5: 31)
                    MOVE TURMA-CO41   TO GS-LINDET-TURMA(36: 3)
                    MOVE NR-PREV-FORM-CO41 TO GS-LINDET-TURMA(39: 4)
                    MOVE GS-CONTRATO      TO NR-CONTRATO-CO61
                    MOVE GS-NR-ITEM       TO ITEM-CO61
                    MOVE TURMA-CO41       TO TURMA-CO61
                    MOVE CURSO-CO41       TO CURSO-CO61
                    READ COD061 INVALID KEY MOVE 0 TO GS-ESTADO-SELECAO
                     NOT INVALID KEY MOVE 1 TO GS-ESTADO-SELECAO
                    END-READ
                    MOVE "INSERE-LIST-TURMA" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
             END-READ
           END-PERFORM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           MOVE GS-DT-PREV-REAL TO DT-PREV-REAL-W
           INITIALIZE REG-COD060
           INITIALIZE GS-DATA-BLOCK
           MOVE DT-PREV-REAL-W  TO GS-DT-PREV-REAL

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO GS-DATA-SOLICIT


           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           INITIALIZE REG-COD060.
           MOVE GS-CONTRATO                 TO NR-CONTRATO-CO60
           MOVE GS-NR-ITEM                  TO ITEM-CO60
           MOVE GS-COD-EVENTO               TO CODEVENTO-CO60
           MOVE GS-NR-PARTICIPANTES         TO QT-PARTICIPANTE-CO60
           MOVE GS-QTDE-TELAO               TO QT-TELAO-CO60
           MOVE GS-DATA-REALIZACAO          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                    TO DATAREALIZA-CO60
           MOVE GS-DATA-SOLICIT             TO DATA-SOLIC-CO60
           MOVE GS-FOTO(1: 1)               TO FOTO-CO60
           MOVE GS-VIDEO(1: 1)              TO VIDEO-CO60
           MOVE GS-BECA(1: 1)               TO BECA-CO60
           MOVE GS-CLIP(1: 1)               TO CLIP-CO60
           MOVE GS-FAX(1: 1)                TO FAX-CO60
           MOVE GS-APROVACAO(1: 1)          TO APROVACAO-CO60
           MOVE GS-SOLICITANTE              TO COD-COMISSAO-CO60
           MOVE GS-HORARIO                  TO HORARIO-CO60
           MOVE GS-LOCAL                    TO LOCAL-CO60
           MOVE GS-ENDERECO                 TO ENDERECO-CO60
           MOVE GS-PONTO-REFERENCIA         TO REFERENCIA-CO60
           MOVE GS-PESSOA-CONTATO           TO PESSOA-CONTATO-CO60
           MOVE GS-UNIFORME                 TO UNIFORME-CO60
           MOVE GS-ORGANIZADOR              TO ORGANIZADOR-CO60
           MOVE GS-OBSERVACAO               TO OBSERVACAO-CO60
           MOVE GS-NR-PLANEJAMENTO          TO NR-PLANEJ-CO60
           MOVE GS-NR-REL-REPORT            TO NR-REL-REPOR-CO60
           MOVE GS-DATA-CANCELAMENTO        TO DATA-CANCELAM-CO60
           MOVE GS-HORA-CANCELAMENTO        TO HORA-CANCELAM-CO60
           MOVE GS-DT-PREV-REAL             TO DT-PREV-REAL-CO60
           MOVE GS-CIDADE-EVENTO            TO CIDADE-CO60

           PERFORM SALVAR-DADOS-CURSO.
       APAGAR-CURSOS SECTION.
           CLOSE    COD061
           OPEN I-O COD061
      *    deletar todos os cursos/turmas da chave = contrato e item
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO61.
           MOVE GS-NR-ITEM   TO ITEM-CO61.
           MOVE ZEROS        TO CURSO-CO61.
           MOVE SPACES       TO TURMA-CO61.
           START COD061 KEY IS NOT < CHAVE-CO61 INVALID KEY
                 MOVE "10" TO ST-COD061.
           PERFORM UNTIL ST-COD061 = "10"
                 READ COD061 NEXT RECORD AT END
                      MOVE "10" TO ST-COD061
                 NOT AT END
                      IF NR-CONTRATO-CO61 <> GS-CONTRATO OR
                         ITEM-CO61 <> GS-NR-ITEM
                         MOVE "10" TO ST-COD061
                      ELSE
                         DELETE COD061 NOT INVALID KEY
                             MOVE "E"      TO LOG2-OPERACAO
                             MOVE "COD061" TO LOG2-ARQUIVO
                             PERFORM GRAVAR-LOG002
                         END-DELETE
                      END-IF
                 END-READ
           END-PERFORM
           CLOSE      COD061
           OPEN INPUT COD061.

       GRAVAR-LOG002 SECTION.
           OPEN I-O LOG002

           MOVE FUNCTION CURRENT-DATE  TO WS-DATA-SYS
           ACCEPT WS-HORA-SYS FROM TIME

           MOVE USUARIO-W              TO LOG2-USUARIO
           MOVE WS-DATA-CPU            TO LOG2-DATA
           MOVE WS-HORA-SYS            TO LOG2-HORAS
           MOVE "COP060"               TO LOG2-PROGRAMA
           MOVE REG-COD061             TO LOG2-REGISTRO

           WRITE REG-LOG002 INVALID KEY
                 MOVE "Erro de Gravação...LOG002" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
           END-WRITE

           CLOSE    LOG002.

       SALVAR-DADOS-CURSO SECTION.
           PERFORM APAGAR-CURSOS.
      *    gravar cursos/turmas selecionados
           CLOSE    COD061
           OPEN I-O COD061
           MOVE ZEROS TO GS-POSICAO-SELECAO GS-CONT-TURMA GS-SAIR-LOOP.
           MOVE "TURMA-SELECIONADA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM UNTIL GS-SAIR-LOOP = 1
                   MOVE GS-CONTRATO            TO NR-CONTRATO-CO61
                   MOVE GS-NR-ITEM             TO ITEM-CO61
                   MOVE GS-LINDET-TURMA(1: 3)  TO CURSO-CO61
                   MOVE GS-LINDET-TURMA(36: 2) TO TURMA-CO61
                   WRITE REG-COD061 NOT INVALID KEY
                         MOVE "I"      TO LOG2-OPERACAO
                         MOVE "COD061" TO LOG2-ARQUIVO
                         PERFORM GRAVAR-LOG002
                   END-WRITE
                   ADD 1 TO GS-CONT-TURMA
                   MOVE "TURMA-SELECIONADA" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM
           CLOSE      COD061
           OPEN INPUT COD061.


       GRAVA-DADOS SECTION.
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS

           CLOSE      COD060 COD060C
           OPEN I-O   COD060 COD060C
           MOVE ZEROS TO ST-COD060
           PERFORM UNTIL ST-COD060 = "10"

               WRITE REG-COD060 INVALID KEY
                     ADD 1 TO ITEM-CO60
               NOT INVALID KEY
                     MOVE "I"      TO LOG4-OPERACAO
                     MOVE "COD060" TO LOG4-ARQUIVO
                     PERFORM GRAVAR-LOG004
                     MOVE "10"     TO ST-COD060
                     MOVE NR-CONTRATO-CO60 TO NR-CONTRATO-CO60C
                     MOVE ITEM-CO60        TO ITEM-CO60C
                     MOVE USUARIO-W        TO USUARIO-CAD-CO60C
                     STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU
                       INTO DATA-CAD-CO60C
                     WRITE REG-COD060C
                     END-WRITE
               END-WRITE
           END-PERFORM
           CLOSE      COD060 COD060C
           OPEN INPUT COD060 COD060C
           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       GRAVAR-LOG004 SECTION.
           OPEN I-O LOG004

           MOVE FUNCTION CURRENT-DATE  TO WS-DATA-SYS
           ACCEPT WS-HORA-SYS FROM TIME

           MOVE USUARIO-W              TO LOG4-USUARIO
           MOVE WS-DATA-CPU            TO LOG4-DATA
           MOVE WS-HORA-SYS            TO LOG4-HORAS
           MOVE "COP060"               TO LOG4-PROGRAMA
           MOVE REG-COD060             TO LOG4-REGISTRO

           WRITE REG-LOG004 INVALID KEY
               MOVE "Erro de Gravação...LOG004" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           END-WRITE

           CLOSE    LOG004.

       REGRAVA-DADOS SECTION.
           CLOSE      COD060
           OPEN I-O   COD060
           REWRITE REG-COD060 INVALID KEY
                 MOVE "Erro Regravacao COD060" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD060 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE "A"      TO LOG4-OPERACAO
                 MOVE "COD060" TO LOG4-ARQUIVO
                 PERFORM GRAVAR-LOG004
           END-REWRITE
           CLOSE      COD060
           OPEN INPUT COD060
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       EXCLUI SECTION.
           CLOSE      COD060
           OPEN I-O   COD060
           DELETE COD060 NOT INVALID KEY
                  MOVE "E"      TO LOG4-OPERACAO
                  MOVE "COD060" TO LOG4-ARQUIVO
                  PERFORM GRAVAR-LOG004
           END-DELETE
           CLOSE      COD060
           OPEN INPUT COD060
           PERFORM APAGAR-CURSOS.
           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.


       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-CONTRATO     TO NR-CONTRATO-CO60.
           MOVE ZEROS           TO ITEM-CO60 GS-NR-ITEM.
           START COD060 KEY IS NOT < CHAVE-CO60
                    INVALID KEY MOVE "10" TO ST-COD060.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-COD060 = "10"
              READ COD060 NEXT RECORD AT END MOVE "10" TO ST-COD060
              NOT AT END
                IF NR-CONTRATO-CO60 <> GS-CONTRATO
                   MOVE "10" TO ST-COD060
                ELSE
                   PERFORM MOVER-DADOS-LISTA
                   MOVE ITEM-CO60     TO GS-NR-ITEM
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
           ADD 1 TO GS-NR-ITEM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES               TO GS-LINDET
           MOVE NR-CONTRATO-CO60     TO GS-LINDET(1: 5)
           MOVE ITEM-CO60            TO GS-LINDET(6: 4)
           MOVE CODEVENTO-CO60       TO CODIGO-CO03
           READ COD003 INVALID KEY MOVE "*******" TO NOME-CO03.
           MOVE NOME-CO03            TO GS-LINDET(10: 24)
           MOVE QT-PARTICIPANTE-CO60 TO GS-LINDET(34: 5)
           MOVE DATAREALIZA-CO60     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO DATA-E
           MOVE DATA-E               TO GS-LINDET(39: 11)
           MOVE DATA-SOLIC-CO60      TO DATA-E
           MOVE DATA-E               TO GS-LINDET(50: 11)
           EVALUATE FOTO-CO60
             WHEN 0 MOVE "N"         TO GS-LINDET(61: 3)
             WHEN 1 MOVE "S"         TO GS-LINDET(61: 3)
           END-EVALUATE.
           EVALUATE VIDEO-CO60
             WHEN 0 MOVE "N"         TO GS-LINDET(64: 3)
             WHEN 1 MOVE "S"         TO GS-LINDET(64: 3)
           END-EVALUATE.
           EVALUATE BECA-CO60
             WHEN 0 MOVE "N"         TO GS-LINDET(67: 3)
             WHEN 1 MOVE "S"         TO GS-LINDET(67: 3)
           END-EVALUATE.
           EVALUATE CLIP-CO60
             WHEN 0 MOVE "N"         TO GS-LINDET(70: 3)
             WHEN 1 MOVE "S"         TO GS-LINDET(70: 3)
           END-EVALUATE.
           EVALUATE FAX-CO60
             WHEN 0 MOVE " N"        TO GS-LINDET(73: 4)
             WHEN 1 MOVE " S"        TO GS-LINDET(73: 4)
           END-EVALUATE.
           EVALUATE APROVACAO-CO60
             WHEN 0 MOVE "N"         TO GS-LINDET(78: 3)
             WHEN 1 MOVE "S"         TO GS-LINDET(78: 3)
           END-EVALUATE.
           MOVE QT-TELAO-CO60        TO GS-LINDET(81: 4)
           MOVE ORGANIZADOR-CO60     TO GS-LINDET(84: 15).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP060X" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-CONTRATO    TO NR-CONTRATO-CO60.
           MOVE GS-NR-ITEM     TO ITEM-CO60.
           START COD060 KEY IS = CHAVE-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-COD060 = "10"
             READ COD060 NEXT RECORD AT END MOVE "10" TO ST-COD060
              NOT AT END
                IF NR-CONTRATO-CO60 <> GS-CONTRATO
                         MOVE "10" TO ST-COD060
                ELSE
                  PERFORM MOVER-DADOS-REL
                END-IF
             END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       MOVER-DADOS-REL SECTION.
           ADD 4 TO LIN
           IF LIN > 60 PERFORM CABECALHO.
           MOVE NR-CONTRATO-CO60     TO LINDET-REL(1: 5)
           MOVE ITEM-CO60            TO LINDET-REL(6: 4)
           MOVE CODEVENTO-CO60       TO CODIGO-CO03
           READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03.
           MOVE NOME-CO03            TO LINDET-REL(9: 22)
           MOVE QT-PARTICIPANTE-CO60 TO LINDET-REL(31: 5)
           MOVE QT-TELAO-CO60        TO LINDET-REL(35: 2)
           MOVE DATAREALIZA-CO60     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO LINDET-REL(37: 11)
           MOVE DATA-SOLIC-CO60      TO LINDET-REL(48: 11)
           MOVE FOTO-CO60            TO LINDET-REL(59: 2)
           MOVE VIDEO-CO60           TO LINDET-REL(61: 2)
           MOVE CLIP-CO60            TO LINDET-REL(63: 2)
           MOVE BECA-CO60            TO LINDET-REL(65: 2)
           MOVE FAX-CO60             TO LINDET-REL(67: 2)
           MOVE APROVACAO-CO60       TO LINDET-REL(69: 2)
           MOVE COD-COMISSAO-CO60    TO LINDET-REL(71: 5)
           MOVE HORARIO-CO60         TO LINDET-REL(76: 11)
           MOVE LOCAL-CO60           TO LINDET-REL(87: 30)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES TO LINDET-REL
           MOVE ENDERECO-CO60(1: 30) TO LINDET-REL(01: 31)
           MOVE REFERENCIA-CO60      TO LINDET-REL(32: 31)
           MOVE PESSOA-CONTATO-CO60  TO LINDET-REL(63: 31)
           MOVE UNIFORME-CO60        TO LINDET-REL(94: 21)
           MOVE ORGANIZADOR-CO60     TO LINDET-REL(115: 15)
           WRITE REG-RELAT FROM LINDET


           MOVE SPACES TO LINDET-REL
           MOVE OBSERVACAO-CO60      TO LINDET-REL(01: 81)
           MOVE DATA-CANCELAM-CO60   TO LINDET-REL(82: 11)
           MOVE HORA-CANCELAM-CO60   TO LINDET-REL(93: 6).
           MOVE NR-PLANEJ-CO60       TO LINDET-REL(100: 9)
           MOVE NR-REL-REPOR-CO60    TO LINDET-REL(109: 7)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES TO LINDET-REL
           WRITE REG-RELAT FROM LINDET.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB05.
           WRITE REG-RELAT FROM CAB06.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.
      *-----------------------------------------------------------
       IMPRIME-TELA SECTION.

           COPY CONDENSA.

           MOVE GS-CONTRATO          TO CONTRATO-REL
           MOVE GS-NR-ITEM           TO ITEM-REL
           MOVE GS-COD-EVENTO        TO EVENTO-REL
           MOVE GS-NOME-EVENTO       TO NOME-EVENTO-REL
           MOVE GS-NR-PARTICIPANTES  TO QTDE-PARTIC-REL
           MOVE GS-ORGANIZADOR       TO ORGANIZADOR-REL
           MOVE GS-DATA-REALIZACAO   TO DATA-REALIZ-REL
           MOVE GS-DATA-SOLICIT      TO DATA-SOLICIT-REL
           MOVE GS-SOLICITANTE       TO SOLICITANTE-REL
           MOVE GS-NOME-SOLICITANTE  TO NOME-SOLICIT-REL
           MOVE GS-QTDE-TELAO        TO QTDE-TELAO-REL
           MOVE GS-FOTO              TO FOTO-REL
           MOVE GS-VIDEO             TO VIDEO-REL
           MOVE GS-BECA              TO BECA-REL
           MOVE GS-CLIP              TO CLIP-REL
           MOVE GS-FAX               TO FAX-REL
           MOVE GS-APROVACAO         TO APROVACAO-REL
           MOVE GS-NR-PLANEJAMENTO   TO NR-PLANEJ-REL
           MOVE GS-NR-REL-REPORT     TO NR-REL-REPORT-REL
           MOVE GS-OBSERVACAO        TO OBS-REL
           MOVE GS-LOCAL             TO LOCAL-REL
           MOVE GS-HORARIO           TO HORARIO-REL
           MOVE GS-ENDERECO          TO ENDERECO-REL
           MOVE GS-PONTO-REFERENCIA  TO PONTO-REFER-REL
           MOVE GS-PESSOA-CONTATO    TO PESSOA-CONT-REL
           MOVE GS-UNIFORME          TO UNIFORME-REL
           WRITE REG-RELAT FROM LINDET01.
           WRITE REG-RELAT FROM LINDET02.
           WRITE REG-RELAT FROM LINDET03.
           WRITE REG-RELAT FROM LINDET04.
           WRITE REG-RELAT FROM LINDET05.
           WRITE REG-RELAT FROM LINDET06.
           WRITE REG-RELAT FROM LINDET07.
           WRITE REG-RELAT FROM LINDET08.
           WRITE REG-RELAT FROM LINDET09.
           WRITE REG-RELAT FROM LINDET10.
           WRITE REG-RELAT FROM LINDET11.
           WRITE REG-RELAT FROM LINDET12.
           WRITE REG-RELAT FROM LINDET13.
           WRITE REG-RELAT FROM LINDET14.

           COPY DESCONDENSA.


      *-----------------------------------------------------------

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD003 COD060 COD061 IED011 CGD010 CGD012 COD041
                 CAD004 COD040 CAD010 COD049 COD060C.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "COP050X"           to logacess-programa
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

           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
