       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP010.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 02/06/1999.
      *FUNÇÃO: Movimento de CHEQUES

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.
           COPY CAPX004.
           COPY CGPX001.
           COPY CGPX010.
           COPY CXPX020.
           COPY CAPX018.
           COPY CRPX001.
           COPY CHPX010.
           COPY CHPX011.
           COPY CHPX012.
           COPY CHPX013.
           COPY CHPX099.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.
       COPY CAPW004.
       COPY CGPW001.
       COPY CGPW010.
       COPY CXPW020.
       COPY CAPW018.
       COPY CRPW001.
       COPY CHPW010.
       COPY CHPW011.
       COPY CHPW012.
       COPY CHPW013.
       COPY CHPW099.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CHP010.CPB".
           COPY "CHP010.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD011             PIC XX       VALUE SPACES.
           05  ST-CHD012             PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CHD099             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
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
           05  GS-VALOR-SALDO        PIC 9(09)V99 VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  CODIGO-E              PIC Z.ZZ.ZZ.ZZ.

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
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
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  SEQ-ALTERACAO         PIC 9(4)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  CLASSIFICACAO-W       PIC X(10)    VALUE SPACES.
           05  COD-CLIENTE-W         PIC 9(8)     VALUE ZEROS.
           05  DESCR-CLIENTE-W       PIC X(30)    VALUE SPACES.
           05  BANCO-W               PIC 9(4)     VALUE ZEROS.
           05  NR-CHEQUE             PIC 9(07)    VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem                 pic x(200).
       01 tipo-msg                 pic x(01).
       01 resp-msg                 pic x(01).

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(76)   VALUE
           "CONFERENCIA DO MOVIMENTO DE CHEQUES".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(132)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(132)  VALUE
           "C   CLIENTE NR-CHEQ OUTRO-DOCTO      BCO AGENC NOME
      -    "                  CIDADE        PORT CAR SITUACAO APU    DIG
      -    "IT SEQ.CX".
       01  LINDET.
           05  CLASS-CLIENTE-REL   PIC 9       VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  CLIENTE-REL         PIC ZZZZ.ZZZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  NR-CHEQUE-REL       PIC X(7)    VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  OUTRO-DOCTO-REL     PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  BANCO-REL           PIC Z999    VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  AGENCIA-REL         PIC ZZ999   VALUE ZEROS.
           05  FILLER              PIC X       VALUE "-".
      *    05  DV-AGENCIA-REL      PIC X       VALUE SPACES.
      *    05  FILLER              PIC X       VALUE SPACES.
           05  NOME-REL            PIC X(30)   VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  CIDADE-REL          PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  PORTADOR-REL        PIC 9999    VALUE ZEROS.
           05  FILLER              PIC X      VALUE SPACES.
           05  CARTEIRA-REL        PIC 9       VALUE ZEROS.
           05  FILLER              PIC XXX     VALUE SPACES.
           05  SITUACAO-REL        PIC 9       VALUE ZEROS.
           05  FILLER              PIC X       VALUE "-".
           05  DESC-SITUACAO-REL   PIC X(6)    VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  APURACAO-REL        PIC ZZZZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  DIGITADOR-REL       PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(4)    VALUE SPACES.
           05  SEQ-CAIXA-REL       PIC ZZZ     BLANK WHEN ZEROS.
       01  CAB05.
           05  FILLER              PIC X(130)  VALUE
           "CTA-CORR COM S.TI NTA-FISCAL DTA-N.FISC VENCIMENTO   VALOR-T
      -    "OTAL OR SEQ.".
       01  LINDET1.
           05  CONTA-CORR-REL      PIC 9(8)    VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  COMPENSACAO-REL     PIC 999     VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  SITUACAO-TIT-REL    PIC 9       VALUE ZEROS.
           05  FILLER              PIC X(4)    VALUE SPACES.
           05  NTA-FISCAL-REL      PIC X(10)   VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  DATA-NTA-FISCAL-REL PIC ZZ/ZZ/ZZZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  VENCTO-REL          PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR-TOTAL-REL     PIC ZZ.ZZZ.ZZZ,ZZ VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  ORIGEM-REL          PIC ZZ.ZZZ.ZZZ,ZZ VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  SEQ-REL             PIC ZZ.ZZZ.ZZZ,ZZ VALUE ZEROS.

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

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD001.
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD011.
           MOVE "CHD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD012.
           MOVE "CHD013" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "CHD099" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD099.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN I-O   CHD010 CHD011 CHD013 CHD099
           CLOSE      CHD010 CHD011 CHD013 CHD099
           OPEN INPUT CHD010 CHD011 CHD013 CHD099 CAD004.

           OPEN INPUT CAD018 CGD001 CXD020 CAD002 CGD010 CRD001 CHD012.
           OPEN I-O CHD099.
           IF ST-CHD099 = "35"
              CLOSE CHD099      OPEN OUTPUT CHD099
              CLOSE CHD099      OPEN I-O CHD099.
           CLOSE CHD099.
           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O CHD010
           END-IF.
           IF ST-CHD011 = "35"
              CLOSE CHD011      OPEN OUTPUT CHD011
              CLOSE CHD011      OPEN I-O CHD011
           END-IF.
           IF ST-CHD013 = "35"
              CLOSE CHD013      OPEN OUTPUT CHD013
              CLOSE CHD013      OPEN I-O CHD013
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
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD011 <> "00"
              MOVE "ERRO ABERTURA CHD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD012 <> "00"
              MOVE "ERRO ABERTURA CHD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CHP010"            to logacess-programa
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
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   MOVE USUARIO-W TO DIGITADOR-CH10
                   MOVE 3 TO SITUACAO-CH10
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
      *        WHEN GS-CANCELA-FLG-TRUE
      *             MOVE USUARIO-W TO DIGITADOR-CH10
      *             MOVE 4 TO SITUACAO-CH10
      *             PERFORM CANCELA-DEVOLV-PROBLEM
      *        WHEN GS-DEVOLVIDO-FLG-TRUE
      *             MOVE USUARIO-W TO DIGITADOR-CH10
      *             IF SITUACAO-CH10 = 5
      *                MOVE DATA-MOVTO-CH10  TO DATA-MOVTO-CH13
      *                MOVE SEQ-CH10         TO SEQ-CH13
      *                READ CHD013 INVALID KEY CONTINUE
      *                  NOT INVALID KEY DELETE CHD013
      *                END-READ
      *             ELSE MOVE 5 TO SITUACAO-CH10
      *             END-IF
      *             PERFORM CANCELA-DEVOLV-PROBLEM
               WHEN GS-PROBLEMATICO-FLG-TRUE
                    MOVE USUARIO-W TO DIGITADOR-CH10
                    IF SITUACAO-CH10 = 6
                       MOVE 0 TO SITUACAO-CH10
                    ELSE MOVE 6 TO SITUACAO-CH10
                    END-IF
                    PERFORM CANCELA-DEVOLV-PROBLEM
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE DATA-MOVTO-I     TO DATA-MOVTO-CH10
                   MOVE GS-LINDET(12: 4) TO SEQ-CH10 SEQ-ALTERACAO
                   PERFORM CARREGAR-DADOS
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
               WHEN GS-VALIDA-CPF-TRUE
                    PERFORM VALIDA-CPF
               WHEN GS-LE-ORIGEM-TRUE
                    PERFORM LE-ORIGEM
               WHEN GS-DIVIDE-PARCELA-TRUE
                   PERFORM DIVIDE-PARCELAS
               WHEN GS-VERIF-TOT-PARC-TRUE
                   PERFORM VERIFICA-TOTAL-PARCELA
               WHEN GS-VERIF-SENHA-TRUE
                    MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
                    MOVE "SENHA60"          TO PROGRAMA-CA004
                    READ CAD004 INVALID KEY
                         MOVE "S" TO GS-OK
                    NOT INVALID KEY
                         MOVE "N" TO GS-OK
                    END-READ
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

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
       VALIDA-CPF SECTION.
           IF GS-CPF = ZEROS
              MOVE 0 TO GS-CPF-CORRETO
           ELSE
             MOVE GS-CPF TO GRDCIC-CIC
             CALL "GRDCIC" USING PARAMETROS-GRDCIC
             CANCEL "GRDCIC"
             IF GRDCIC-RETORNO = "00"
                MOVE ZEROS TO GS-CPF-CORRETO
             ELSE MOVE 1 TO GS-CPF-CORRETO.
      *    0-CORRETO  1-INCORRETO
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
             WHEN 4 CALL   "CXP020T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CXP020T"
                    MOVE PASSAR-STRING-1(52: 5) TO GS-COD-APURACAO
                    PERFORM LE-COD-APURACAO
                    MOVE DESCRICAO-CX20 TO GS-DESCR-APURACAO
             WHEN 5 CALL   "CRP001T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "CRP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-SITUACAO-TIT
                    MOVE PASSAR-STRING-1(33: 2) TO GS-SITUACAO-TIT
             WHEN 6 CALL   "CHP012T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "CHP012T"
                    MOVE PASSAR-STRING-1(1: 10) TO GS-DESCR-ORIGEM
                    MOVE PASSAR-STRING-1(33: 2) TO GS-ORIGEM
           END-EVALUATE.
       CARREGA-POP-UP-CLIENTE SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO COMPRADOR-CG10.
           START CGD010 KEY IS NOT < COMPRADOR-CG10 INVALID KEY
                 MOVE "10" TO ST-CGD010.
           PERFORM UNTIL ST-CGD010 = "10"
              READ CGD010 NEXT RECORD AT END MOVE "10" TO ST-CGD010
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
       CARREGA-POP-UP-VENDEDOR SECTION.
           INITIALIZE REG-CGD001
           MOVE GS-LINDET1(1: 1) TO NOME-CG01 LETRA.
           MOVE "CLEAR-LIST-BOX-VEND" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE SPACES TO NOME-CG01.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD001
              NOT AT END
                  MOVE NOME-CG01(1:1)  TO LETRA1
                  IF LETRA1 NOT = LETRA
                     MOVE "10" TO ST-CGD001
                  ELSE
                     IF T-VEND-CG01 = 1
                        MOVE NOME-CG01     TO GS-LINDET1(1: 32)
                        MOVE CODIGO-CG01   TO GS-LINDET1(33: 06)
                        MOVE "INSERE-POP-UP-VENDEDOR" TO DS-PROCEDURE
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
                    WHEN 0 MOVE "0-Contrato"        TO GS-CLASSIFICACAO
                    WHEN 1 MOVE "1-Comum"           TO GS-CLASSIFICACAO
                    WHEN 9 MOVE "9-Unificado"       TO GS-CLASSIFICACAO
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
              START CXD020 KEY IS NOT < CODIGO-COMPL-CX20
                    INVALID KEY MOVE "10" TO ST-CXD020.
           PERFORM UNTIL ST-CXD020 = "10"
              READ CXD020 NEXT RECORD AT END MOVE "10" TO ST-CXD020
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
           CLOSE    CHD010 CHD013
           OPEN I-O CHD099 CHD010 CHD013

           MOVE REG-CHD010 TO REG-CHD099
           MOVE USUARIO-W  TO USUARIO-EXCLUSAO-CH99
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
           DATA-EXCLUSAO-CH99
           ACCEPT HORA-EXCLUSAO-CH99 FROM TIME
           WRITE REG-CHD099.
           DELETE CHD010.
           MOVE DATA-MOVTO-CH10    TO DATA-MOVTO-CH13
           MOVE SEQ-CH10           TO SEQ-CH13
           READ CHD013 INVALID KEY CONTINUE
              NOT INVALID KEY
                  DELETE CHD013
           END-READ

           CLOSE      CHD099 CHD010 CHD013
           OPEN INPUT CHD010 CHD013.
           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
       CANCELA-DEVOLV-PROBLEM SECTION.
           CLOSE      CHD010
           OPEN I-O   CHD010
           REWRITE REG-CHD010.
           CLOSE      CHD010
           OPEN INPUT CHD010
      *    IF SITUACAO-CH10 = 5 PERFORM ANOTACAO-DEVOLVIDO.
           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
      *ANOTACAO-DEVOLVIDO SECTION.
      *    OPEN I-O CRD200 CRD201.
      *    IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
      *                         CLOSE CRD200  OPEN I-O CRD200.
      *    IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
      *                         CLOSE CRD201  OPEN I-O CRD201.
      *    IF ST-CRD200 <> "00"
      *       MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF ST-CRD201 <> "00"
      *       MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE COD-COMPL-CH10 TO COD-COMPL-CR200
      *    MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
      *    START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
      *          MOVE "10" TO ST-CRD200.
      *    PERFORM UNTIL ST-CRD200 = "10"
      *      READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
      *        NOT AT END
      *          IF COD-COMPL-CR200 <> COD-COMPL-CH10
      *                       MOVE "10" TO ST-CRD200
      *          ELSE MOVE SEQ-CR200 TO ULT-SEQ
      *               CONTINUE
      *      END-READ
      *    END-PERFORM.
      *    ADD 1 TO ULT-SEQ.
      *    MOVE ULT-SEQ      TO SEQ-CR200
      *    MOVE COD-COMPL-CH10 TO COD-COMPL-CR200.
      *    MOVE ZEROS        TO DATA-RETORNO-CR200
      *    MOVE USUARIO-W    TO USUARIO-CR200
      *    MOVE DATA-DIA-I   TO DATA-MOVTO-CR200
      *    MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200
      *
      *    MOVE ZEROS TO ST-CRD200.
      *    PERFORM UNTIL ST-CRD200 = "10"
      *       WRITE REG-CRD200 INVALID KEY
      *          ADD 1 TO SEQ-CR200
      *          CONTINUE
      *        NOT INVALID KEY MOVE "10" TO ST-CRD200
      *    END-PERFORM.
      *
      *    MOVE SEQ-CR200       TO SEQ-CR201.
      *    MOVE COD-COMPL-CR200 TO COD-COMPL-CR201.
      *    MOVE 1               TO SUBSEQ-CR201.
      *    MOVE "A T E N C A O - CHEQUE DEVOLVIDO NR: "
      *           TO ANOTACAO-CR201(1: 37)
      *    MOVE NR-CHEQUE-CH10  TO ANOTACAO-CR201(38: 08)
      *    MOVE NOME-CH10       TO ANOTACAO-CR201(50: 30)
      *    MOVE ZEROS TO ST-CRD201.
      *    PERFORM UNTIL ST-CRD201 = "10"
      *      WRITE REG-CRD201 INVALID KEY
      *            ADD 1 TO SUBSEQ-CR201
      *            CONTINUE
      *        NOT INVALID KEY MOVE "10" TO ST-CRD201
      *      END-WRITE
      *    END-PERFORM.
      *    ADD 1 TO SUBSEQ-CR201
      *    MOVE SPACES           TO ANOTACAO-CR201
      *    MOVE "MOVTO: "        TO ANOTACAO-CR201(1: 07)
      *    MOVE DATA-MOVTO-CH10  TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV         TO DATA-E
      *    MOVE DATA-E           TO ANOTACAO-CR201(08: 11)
      *    MOVE "VENCTO:"        TO ANOTACAO-CR201(23: 08)
      *    MOVE DATA-VENCTO-CH10 TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV         TO DATA-E
      *    MOVE DATA-E           TO ANOTACAO-CR201(31: 11)
      *    MOVE "VALOR: "        TO ANOTACAO-CR201(42: 7)
      *    MOVE VALOR-CH10       TO VALOR-E
      *    MOVE VALOR-E          TO ANOTACAO-CR201(49: 13)
      *    MOVE ZEROS TO ST-CRD201.
      *    PERFORM UNTIL ST-CRD201 = "10"
      *      WRITE REG-CRD201 INVALID KEY
      *            ADD 1 TO SUBSEQ-CR201
      *            CONTINUE
      *        NOT INVALID KEY MOVE "10" TO ST-CRD201
      *      END-WRITE
      *    END-PERFORM.
      *    CLOSE CRD200 CRD201.

       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01      TO GS-DESCR-VENDEDOR.
       LE-CLIENTE SECTION.
           MOVE GS-CLASSIFICACAO(1: 1) TO CLASSIF-CG10.
           MOVE GS-COD-CLIENTE   TO CODIGO-CG10.
           READ CGD010 INVALID KEY MOVE "********" TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10    TO GS-DESCR-CLIENTE.
       LE-PORTADOR SECTION.
           MOVE GS-PORTADOR    TO PORTADOR.
           READ CAD018 INVALID KEY MOVE "******" TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-PORTADOR.
       LE-COD-APURACAO SECTION.
           MOVE GS-COD-APURACAO TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY MOVE "*****" TO DESCRICAO-CX20
                                   MOVE ZEROS TO TIPO-CONTA-CX20.
           MOVE DESCRICAO-CX20    TO GS-DESCR-APURACAO.
           IF TIPO-CONTA-CX20 = 0
               MOVE 0 TO GS-TIPO-CONTA-APUR
           ELSE MOVE 1 TO GS-TIPO-CONTA-APUR.
       LE-SITUACAO-TIT SECTION.
           MOVE GS-SITUACAO-TIT TO CODIGO-CR01.
           READ CRD001 INVALID KEY MOVE "*********" TO
                   SITUACAO-TIT-CR01.
           MOVE SITUACAO-TIT-CR01  TO GS-DESCR-SITUACAO-TIT.
       LE-ORIGEM SECTION.
           MOVE GS-ORIGEM TO CODIGO-CH12.
           READ CHD012 INVALID KEY MOVE SPACES TO DESCR-ORIGEM-CH12.
           MOVE DESCR-ORIGEM-CH12 TO GS-DESCR-ORIGEM.
       CARREGAR-DADOS SECTION.
           START CHD010 KEY IS = CHAVE-CH10 INVALID KEY CONTINUE.
           READ CHD010 INVALID KEY INITIALIZE REG-CHD010.
           MOVE DATA-MOVTO-W       TO GS-DATA-MOVTO.
           EVALUATE CLASS-CLIENTE-CH10
             WHEN 0 MOVE "0-Contrato"       TO GS-CLASSIFICACAO
             WHEN 1 MOVE "1-Comum   "       TO GS-CLASSIFICACAO
             WHEN 9 MOVE "9-Unificado"      TO GS-CLASSIFICACAO
           END-EVALUATE
           MOVE CLASS-CLIENTE-CH10 TO CLASSIF-CG10.
           MOVE CLIENTE-CH10       TO GS-COD-CLIENTE CODIGO-CG10.
           READ CGD010 INVALID KEY MOVE "*****" TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10     TO GS-DESCR-CLIENTE.
           MOVE PORTADOR-CH10      TO GS-PORTADOR PORTADOR.
           READ CAD018 INVALID KEY MOVE "******" TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-PORTADOR.
           EVALUATE CARTEIRA-CH10
             WHEN 1 MOVE "1-Simples " TO GS-CARTEIRA
             WHEN 2 MOVE "2-Caução  " TO GS-CARTEIRA
             WHEN 3 MOVE "3-Desconto" TO GS-CARTEIRA
           END-EVALUATE
           MOVE NR-CHEQUE-CH10        TO GS-NR-CHEQUE.
           MOVE OUTRO-DOCTO-CH10      TO GS-OUTRO-DOCTO.
           MOVE NR-NOTA-FISCAL-CH10   TO GS-NR-NOTA-FISCAL.
           MOVE DATA-NTA-FISCAL-CH10  TO GS-DATA-NTA-FISCAL.
           MOVE DATA-VENCTO-CH10      TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV              TO GS-DATA-VENCTO.
           MOVE NOME-CH10             TO GS-NOME
           MOVE LOTE-CH10             TO GS-LOTE
           MOVE CIDADE-CH10           TO GS-CIDADE
           MOVE CPF-CH10              TO GS-CPF
           MOVE BANCO-CH10            TO GS-BANCO
           MOVE AGENCIA-CH10          TO GS-AGENCIA
           MOVE DV-AGENCIA-CH10       TO GS-DV-AGENCIA
           MOVE DIGITADOR-CH10        TO GS-DIGITADOR.
           MOVE SITUACAO-TIT-CH10  TO GS-SITUACAO-TIT CODIGO-CR01.
           READ CRD001 INVALID KEY MOVE "********"  TO
                   SITUACAO-TIT-CR01.
           MOVE SITUACAO-TIT-CR01      TO GS-DESCR-SITUACAO-TIT.
           MOVE CODREDUZ-APUR-CH10 TO GS-COD-APURACAO
                                      CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY MOVE "*****" TO DESCRICAO-CX20.
           MOVE DESCRICAO-CX20     TO GS-DESCR-APURACAO.
           MOVE VALOR-CH10     TO GS-VALOR-TOTAL.
           MOVE VENDEDOR-CH10      TO GS-VENDEDOR CODIGO-CG01
           READ CGD001 INVALID KEY MOVE "*****" TO NOME-CG01
           END-READ
           MOVE NOME-CG01          TO GS-DESCR-VENDEDOR
           MOVE SITUACAO-CH10      TO GS-SITUACAO.
           MOVE SITUACAO-TIT-CH10   TO GS-SITUACAO-TIT CODIGO-CR01.
           READ CRD001 INVALID KEY MOVE "********"  TO
                  SITUACAO-TIT-CR01.
           MOVE SITUACAO-TIT-CR01   TO GS-DESCR-SITUACAO-TIT.
           MOVE ORIGEM-CH10         TO GS-ORIGEM CODIGO-CH12.
           READ CHD012 INVALID KEY MOVE SPACES TO DESCR-ORIGEM-CH12.
           MOVE DESCR-ORIGEM-CH12   TO GS-DESCR-ORIGEM.
           MOVE VALOR-SALDO-CH10    TO GS-VALOR-SALDO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           MOVE GS-DATA-MOVTO    TO DATA-MOVTO-W
           MOVE GS-CLASSIFICACAO TO CLASSIFICACAO-W
           MOVE GS-COD-CLIENTE   TO COD-CLIENTE-W
           MOVE GS-DESCR-CLIENTE TO DESCR-CLIENTE-W
           MOVE GS-BANCO         TO BANCO-W
           INITIALIZE REG-CHD010
           INITIALIZE GS-DATA-BLOCK
                      GS-VALOR-SALDO
           MOVE DATA-MOVTO-W     TO GS-DATA-MOVTO.
           MOVE COD-CLIENTE-W    TO GS-COD-CLIENTE
           MOVE DESCR-CLIENTE-W  TO GS-DESCR-CLIENTE
           MOVE BANCO-W          TO GS-BANCO
           MOVE CLASSIFICACAO-W  TO GS-CLASSIFICACAO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

       GRAVA-PARCELAS SECTION.
           MOVE ZEROS TO K.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
               IF GS-NR(I) = ZEROS
                  MOVE 24 TO I
               ELSE
                  PERFORM SALVAR-DADOS
                  MOVE FUNCTION NUMVAL(GS-NR-CHEQUE) TO NR-CHEQUE
                  ADD GS-NR(I)                       TO NR-CHEQUE
                  COMPUTE NR-CHEQUE = NR-CHEQUE - 1

                  IF NR-CHEQUE > 999999
                     MOVE NR-CHEQUE            TO NR-CHEQUE-CH10
                  ELSE
                     IF NR-CHEQUE > 99999
                        MOVE NR-CHEQUE(2:6)    TO NR-CHEQUE-CH10
                     ELSE
                        IF NR-CHEQUE > 9999
                           MOVE NR-CHEQUE(3:5) TO NR-CHEQUE-CH10
                        ELSE
                           IF NR-CHEQUE > 999
                              MOVE NR-CHEQUE(4:4) TO NR-CHEQUE-CH10
                           ELSE
                              IF NR-CHEQUE > 99
                                 MOVE NR-CHEQUE(5:3) TO NR-CHEQUE-CH10
                              ELSE
                                 IF NR-CHEQUE > 9
                                    MOVE NR-CHEQUE(6:2) TO
                                         NR-CHEQUE-CH10
                                 ELSE
                                    MOVE NR-CHEQUE(7:1) TO
                                         NR-CHEQUE-CH10
                                 END-IF
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                  END-IF

                  MOVE GS-VENCTO(I)  TO DATA-INV
                  CALL "GRIDAT2" USING DATA-INV
                  MOVE DATA-INV          TO DATA-VENCTO-CH10
                  MOVE GS-VALOR(I)       TO VALOR-CH10
                  MOVE GS-VALOR(I)       TO VALOR-SALDO-CH10
                  PERFORM GRAVA-DADOS
               END-IF
           END-PERFORM.

       SALVAR-DADOS SECTION.
           INITIALIZE REG-CHD010.
           IF DATA-MOVTO-I = 0
              MOVE GS-DATA-MOVTO TO DATA-INV DATA-MOVTO-W
              CALL "GRIDAT2" USING DATA-INV
              MOVE DATA-INV          TO DATA-MOVTO-CH10
              DATA-MOVTO-I.

           MOVE DATA-MOVTO-I      TO DATA-MOVTO-CH10.

           IF GS-CLASSIFICACAO = SPACES
              MOVE "0" TO CLASS-CLIENTE-CH10
           ELSE
              MOVE GS-CLASSIFICACAO(1: 1)  TO CLASS-CLIENTE-CH10.

           MOVE GS-COD-CLIENTE    TO CLIENTE-CH10
           MOVE GS-PORTADOR       TO PORTADOR-CH10.
           IF GS-CARTEIRA = SPACES
              MOVE "0" TO CARTEIRA-CH10
           ELSE
              MOVE GS-CARTEIRA(1: 1) TO CARTEIRA-CH10.

           IF GS-SITUACAO-TIT = SPACES
              MOVE "00" TO SITUACAO-TIT-CH10
           ELSE
              MOVE GS-SITUACAO-TIT(1: 2) TO SITUACAO-TIT-CH10.

           MOVE GS-NR-CHEQUE       TO NR-CHEQUE-CH10
           MOVE GS-OUTRO-DOCTO     TO OUTRO-DOCTO-CH10
           MOVE GS-VENCTO-INV      TO DATA-VENCTO-CH10
           MOVE GS-NOME            TO NOME-CH10
           MOVE GS-LOTE            TO LOTE-CH10
           MOVE GS-BANCO           TO BANCO-CH10
           MOVE GS-AGENCIA         TO AGENCIA-CH10
           MOVE GS-DV-AGENCIA      TO DV-AGENCIA-CH10
           MOVE GS-ORIGEM          TO ORIGEM-CH10
           MOVE GS-COD-APURACAO    TO CODREDUZ-APUR-CH10
           MOVE GS-CIDADE          TO CIDADE-CH10
           MOVE PARAMETROS-W(1: 5) TO DIGITADOR-CH10
           MOVE GS-VALOR-TOTAL     TO VALOR-CH10
           IF GS-TIPO-GRAVACAO = 1
              IF GS-VALOR-TOTAL <> GS-VALOR-SALDO
                 MOVE "Valor Informado Diferente do Valor do Saldo do Ch
      -               "eque Deseja Atualiza-lo ?" TO MENSAGEM
                 MOVE "Q" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
                 IF RESP-MSG = "S"
                    MOVE GS-VALOR-TOTAL TO VALOR-SALDO-CH10
                 ELSE
                    MOVE GS-VALOR-SALDO TO VALOR-SALDO-CH10
                 END-IF
              ELSE
                 MOVE GS-VALOR-TOTAL TO VALOR-SALDO-CH10
              END-IF
           ELSE
               MOVE GS-VALOR-TOTAL TO VALOR-SALDO-CH10
           END-IF
           MOVE GS-VENDEDOR        TO VENDEDOR-CH10
           MOVE GS-NR-NOTA-FISCAL  TO NR-NOTA-FISCAL-CH10
           MOVE GS-DATA-NTA-FISCAL TO DATA-NTA-FISCAL-CH10.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       GRAVA-DADOS SECTION.
           PERFORM ACHA-SEQ-CHD011.
           CLOSE      CHD010
           OPEN I-O   CHD010
           MOVE DATA-MOVTO-I      TO DATA-MOVTO-CH10.
           MOVE SEQ-CH11          TO SEQ-CH10.
           MOVE ZEROS TO ST-CHD010.
           PERFORM UNTIL ST-CHD010 = "10"
                WRITE REG-CHD010 INVALID KEY
                    PERFORM ACHA-SEQ-CHD011
                    MOVE SEQ-CH11 TO SEQ-CH10
                NOT INVALID KEY
                    MOVE "10" TO ST-CHD010.
           CLOSE      CHD010
           OPEN INPUT CHD010
           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ACHA-SEQ-CHD011 SECTION.
           CLOSE CHD011
           OPEN I-O CHD011
           MOVE DATA-MOVTO-I TO DATA-MOVTO-CH11.
           READ CHD011 INVALID KEY
                MOVE 1 TO SEQ-CH11
                WRITE REG-CHD011
                END-WRITE
              NOT INVALID KEY
                ADD 1 TO SEQ-CH11
                REWRITE REG-CHD011
           END-READ.
           CLOSE CHD011
           OPEN INPUT CHD011.
       REGRAVA-DADOS SECTION.
           CLOSE      CHD010
           OPEN I-O   CHD010
           MOVE SEQ-ALTERACAO TO SEQ-CH10.
           REWRITE REG-CHD010 INVALID KEY
                 MOVE "Erro Regravacao CHD010" TO GS-MENSAGEM-ERRO
                 MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE "00" TO ST-CHD010
           END-REWRITE
           IF ST-CHD010 <> "00"
              MOVE "Erro Regravacao CHD010" TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(24: 5)
              MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.
           CLOSE      CHD010
           OPEN INPUT CHD010
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
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
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END
                  MOVE "10" TO ST-CRD200
             NOT AT END
                  IF COD-COMPL-CR200 <> COD-COMPL-CH10
                     MOVE "10" TO ST-CRD200
                 ELSE
                     MOVE SEQ-CR200 TO ULT-SEQ
                     CONTINUE
             END-READ
           END-PERFORM.
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ        TO SEQ-CR200
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200.
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
           STRING "CHEQUE Nº. " NR-CHEQUE-CH10 " INCLUIDO ATRAVES DO MOV
      -    "IMENTO DE CHEQUES" INTO ANOTACAO-CR201
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
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
               NOT AT END
                 IF COD-COMPL-CR200 <> COD-COMPL-CH10
                              MOVE "10" TO ST-CRD200
                 ELSE MOVE SEQ-CR200 TO ULT-SEQ
                      CONTINUE
             END-READ
           END-PERFORM.
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ      TO SEQ-CR200
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200.
           MOVE ZEROS        TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE DATA-DIA-I   TO DATA-MOVTO-CR200
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
           MOVE "ALTERACAO EFETUADA NO CHEQUE            - MOTIVO: "
                  TO ANOTACAO-CR201(1: 80)
           MOVE NR-CHEQUE-CH10  TO ANOTACAO-CR201(30: 10).
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
             NOT INVALID KEY
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
                       WRITE REG-CRD201
                       END-WRITE
                 END-WRITE
              END-IF
           END-PERFORM.
           CLOSE CRD200 CRD201.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-DATA-MOVTO TO DATA-INV DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO DATA-MOVTO-CH10 DATA-MOVTO-I.
           MOVE ZEROS             TO SEQ-CH10.
           START CHD010 KEY IS NOT < CHAVE-CH10
                    INVALID KEY MOVE "10" TO ST-CHD010.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-CHD010 = "10"
              READ CHD010 NEXT RECORD AT END
                   MOVE "10" TO ST-CHD010
              NOT AT END
                IF DATA-MOVTO-CH10 NOT = DATA-MOVTO-I
                                   MOVE "10" TO ST-CHD010
                ELSE
      *          IF SITUACAO-CH10 = 3 OR SITUACAO-CH10 = 4
      *                CONTINUE
      *          ELSE
                      PERFORM MOVER-DADOS-LISTA
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
      *          END-IF
                END-IF
              END-READ
           END-PERFORM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE CLASS-CLIENTE-CH10 TO GS-LINDET(1: 1) CLASSIF-CG10
           MOVE CLIENTE-CH10       TO CLIENTE-E CODIGO-CG10
           MOVE CLIENTE-CH10       TO GS-LINDET(03: 09)
           MOVE SEQ-CH10           TO GS-LINDET(12: 06)
           MOVE BANCO-CH10         TO GS-LINDET(18: 5)
           MOVE AGENCIA-CH10       TO GS-LINDET(23: 6)
           MOVE NOME-CH10          TO GS-LINDET(29: 31)
           MOVE PORTADOR-CH10      TO GS-LINDET(60: 05)
           MOVE SITUACAO-CH10      TO GS-LINDET(65: 02)
           MOVE CODREDUZ-APUR-CH10 TO GS-LINDET(67: 06)
           MOVE DIGITADOR-CH10     TO GS-LINDET(73: 06)
           MOVE DATA-VENCTO-CH10   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(79: 11)
           MOVE VALOR-CH10     TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(90: 10).
           MOVE NR-CHEQUE-CH10     TO GS-LINDET(101: 7).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CHP010" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           COPY CONDENSA.
           MOVE ZEROS TO PAG-W.
           MOVE DATA-MOVTO-I TO DATA-MOVTO-CH10.
           MOVE ZEROS        TO SEQ-CH10.
           START CHD010 KEY IS = CHAVE-CH10 INVALID KEY
                 MOVE "10" TO ST-CHD010.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CHD010 = "10"
             READ CHD010 NEXT RECORD AT END MOVE "10" TO ST-CHD010
              NOT AT END
                IF DATA-MOVTO-CH10 NOT = DATA-MOVTO-I
                   MOVE "10" TO ST-CHD010
                ELSE
                MOVE CLASS-CLIENTE-CH10    TO CLASS-CLIENTE-REL
                MOVE CLIENTE-CH10          TO CLIENTE-REL
                MOVE SEQ-CH10              TO SEQ-REL
                MOVE CARTEIRA-CH10         TO CARTEIRA-REL
                MOVE NR-CHEQUE-CH10        TO NR-CHEQUE-REL
                MOVE OUTRO-DOCTO-CH10      TO OUTRO-DOCTO-REL
                MOVE BANCO-CH10            TO BANCO-REL
                MOVE AGENCIA-CH10          TO AGENCIA-REL
                MOVE NOME-CH10             TO NOME-REL
                MOVE COMPENSACAO-CH10      TO COMPENSACAO-REL
                MOVE ORIGEM-CH10           TO ORIGEM-REL
                MOVE PORTADOR-CH10         TO PORTADOR-REL
                MOVE SITUACAO-TIT-CH10     TO SITUACAO-TIT-REL
                MOVE SITUACAO-CH10         TO SITUACAO-REL
                EVALUATE SITUACAO-CH10
                 WHEN 0 MOVE "0K"          TO DESC-SITUACAO-REL
                 WHEN 1 MOVE "      "      TO DESC-SITUACAO-REL
                 WHEN 2 MOVE "PAGA"        TO DESC-SITUACAO-REL
                 WHEN 3 MOVE "ESTORN"      TO DESC-SITUACAO-REL
                 WHEN 4 MOVE "CANCEL"      TO DESC-SITUACAO-REL
                 WHEN 5 MOVE "DEVOLV"      TO DESC-SITUACAO-REL
                 WHEN 6 MOVE "PROBL."      TO DESC-SITUACAO-REL
                END-EVALUATE
                MOVE CODREDUZ-APUR-CH10    TO APURACAO-REL
                MOVE DIGITADOR-CH10        TO DIGITADOR-REL
                MOVE SEQ-CAIXA-CH10        TO SEQ-CAIXA-REL
                MOVE DATA-VENCTO-CH10      TO DATA-INV
                CALL "GRIDAT1" USING DATA-INV
                MOVE DATA-INV              TO VENCTO-REL
                MOVE NR-NOTA-FISCAL-CH10   TO NTA-FISCAL-REL
                MOVE DATA-NTA-FISCAL-CH10  TO DATA-NTA-FISCAL-REL
                MOVE VALOR-CH10            TO VALOR-TOTAL-REL

                WRITE REG-RELAT FROM LINDET
                WRITE REG-RELAT FROM LINDET1
                ADD 2 TO LIN
                IF LIN > 56 PERFORM CABECALHO
                END-IF
             END-READ
           END-PERFORM.

           COPY DESCONDENSA.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB05.
           WRITE REG-RELAT FROM CAB03.
           MOVE 5 TO LIN.

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
           move "CHP010"            to logacess-programa
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

           close chd013
           CLOSE CAD018 CHD010 CHD011 CHD012 CHD099 CGD001 CGD010
                 CRD001 CXD020 CAD002 CAD004.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
