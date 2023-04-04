       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP051.
      *DATA: 12/04/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: CONTAS A RECEBER EM ABERTO
      *FUNÇÃO: Listar todos os títulos que estiverem dentro do intervalo
      *        de vencimento. As ordens serão: Vencto, Portador, cliente
      *        e VCTO/vcto. Altera portador atraves de senha.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX011.
           COPY CGPX020.
           COPY CRPX020.
           COPY CRPX020B.
           COPY CAPX004.
           COPY CAPX010.
           COPY CAPX018.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = CLASSIF-WK CLIENTE-WK SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK = VENCTO-WK
                            VALOR-BRUTO-WK
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK =
                     PORTADOR-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NOME-CLIEN-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CH-CONTRATO-WK = CLASSIF-WK
                                                           CONTRATO-WK
                                                           ALBUM-WK
                                                           VENCTO-WK
                  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK = VENCTO-WK
                  VALOR-LIQUIDO-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.
      *                 "CRP051.TXT"
      *                 ORGANIZATION IS LINE SEQUENTIAL
      *                 ACCESS MODE IS SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CGPW020.
       COPY CAPW004.
       COPY CAPW010.
       COPY CAPW018.
       COPY CRPW020.
       COPY CRPW020B.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  CLASSIF-WK          PIC 9.
           05  CLIENTE-WK.
               10  CONTRATO-WK     PIC 9(4).
               10  ALBUM-WK        PIC 9(4).
           05  SEQ-WK              PIC 9(5).
           05  NOME-CLIEN-WK       PIC X(15).
           05  CIDADE-WK           PIC X(13).
           05  UF-WK               PIC XX.
           05  NOSSO-NR-WK         PIC X(15).
           05  PORTADOR-WK         PIC 9(4).
           05  VENCTO-WK           PIC 9(8).
           05  VALOR-BRUTO-WK      PIC 9(8)V99.
           05  VALOR-LIQUIDO-WK    PIC 9(8)V99.
           05  TIPO-WK             PIC X(4).
           05  RESPONSAVEL-WK      PIC X(5).
           05  PARCELA-WK          PIC 9(02).
           05  QT-PARCELA-WK       PIC 9(02).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP051.CPB".
           COPY "CRP051.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  NOME-CLIEN-ANT        PIC X(20)    VALUE SPACES.
           05  PORTADOR-ANT          PIC X(10)    VALUE SPACES.
           05  CIDADE-ANT            PIC X(13)    VALUE SPACES.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  COD-COMPL-W           PIC 9(9)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  VENCTO-ANT            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-ANT           PIC 9(1)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  TOTAL-LIQUIDO-W       PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-BRUTO-W         PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  CLIENTE-W             PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
      *    variaveis p/ calcular o prazo-medio
           05  TOTAL-GERAL-PM        PIC 9(11)V99  VALUE ZEROS.
           05  PM-W                  PIC 999V99   VALUE ZEROS.
           05  PASSAR-STRING         PIC X(30)    VALUE SPACES.
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  AUX-TIPO              PIC 9(01)    VALUE ZEROS.
           05  TOTAL-ITENS           PIC 9(05)    VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 STRING-1            pic x(65).
       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 DATA-AUX.
          05 AUX-ANO                 PIC 9(04).
          05 AUX-MES                 PIC 9(02).
          05 AUX-DIA                 PIC 9(02).

       01  CAB01.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RELACAO DE CONTAS A RECEBER-ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
           05  FILLER              PIC X(06) VALUE "TIPO: ".
           05  TIPO-REL            PIC X(19).

       01  CAB03.
           05  FILLER              PIC X(132)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(134)  VALUE
           "NOME-CLIENTE    CIDADE        UF NR-BANCO        CONT     PO
      -    "  CART DATA-VECTO PARC. VALOR-LIQ.     TOTAL-LIQ VALOR-BRU
      -    "  TOTAL-BRU".


       01  LINDET.
           05  LINDET-REL          PIC X(132)  VALUE SPACES.

       01  LINTOT1.
           05  FILLER              PIC X(12)   VALUE "QT-TITULOS: ".
           05  QTDE-TITULO-REL     PIC ZZZZ.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(10) VALUE "LÍQUID.".
           05  FILLER              PIC X(13)   VALUE "TOT-PERIODO: ".
           05  TOTAL-PERIODO-REL   PIC ZZ.ZZZ.ZZZ,ZZ.
           05  FILLER              PIC X(04)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE "TOT-VENC: ".
           05  TOTAL-VENCIDO-REL   PIC ZZ.ZZZ.ZZZ,ZZ.
           05  FILLER              PIC X(04)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "TOT-A-VENC: ".
           05  TOTAL-AVENCER-REL   PIC ZZ.ZZZ.ZZZ,ZZ.

       01  LINTOT2.
           05  FILLER              PIC X(12)   VALUE "PRAZO MED.: ".
           05  PM-REL              PIC ZZZ,ZZ.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE "BRUTO".
           05  FILLER              PIC X(13)   VALUE "TOT-PERIODO: ".
           05  TOTAL-PERIODO-REL2  PIC ZZ.ZZZ.ZZZ,ZZ.
           05  FILLER              PIC X(04)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE "TOT-VENC: ".
           05  TOTAL-VENCIDO-REL2  PIC ZZ.ZZZ.ZZZ,ZZ.
           05  FILLER              PIC X(04)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "TOT-A-VENC: ".
           05  TOTAL-AVENCER-REL2  PIC ZZ.ZZZ.ZZZ,ZZ.

       01  CAB02A.
           05  FILLER              PIC X(09)   VALUE "CLIENTE: ".
           05  CLASSIF-REL         PIC 9       VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  CLIENTE-REL         PIC ZZZZ.ZZZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-CLIENTE-REL    PIC X(30)   VALUE ZEROS.

       01  CAB06.
           05  FILLER              PIC X(110)  VALUE
           "CONTATOS EFETUADOS                      ".

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

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.

           DISPLAY DATA-DIA-I STOP " "
           DISPLAY DATA-DIA-I STOP " "
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD020B" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020B.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CRD200"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CAD018 CAD010 CGD010 CGD011 CAD004 CGD020 CGD001
           OPEN I-O CRD020 CRD020B
           OPEN I-O CRD200 CRD201.
           IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
                                CLOSE CRD200  OPEN I-O CRD200.
           IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
                                CLOSE CRD201  OPEN I-O CRD201.
           CLOSE      CRD020 CRD200 CRD201 CRD020B
           OPEN INPUT CRD020 CRD200 CRD201 CRD020B

           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020B <> "00"
              MOVE "ERRO ABERTURA CRD020B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP051"            to logacess-programa
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

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT    WORK
           CLOSE          WORK
           OPEN I-O       WORK

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-VENCTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM CHAMA-ALTERACAO
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM POPUP-PORTADOR
               when gs-popup-contrato-true
                    PERFORM POPUP-CONTRATO
               WHEN GS-TRANSF-PORTADOR-TRUE
                    PERFORM TRANSFERE-PORTADOR
               WHEN GS-MARCA-ITEM-TRUE
                    PERFORM MARCAR-ITEM
               WHEN GS-BAIXAR-ITEM-TRUE
                    PERFORM BAIXAR-ITENS
               WHEN GS-VERIFICAR-SENHA-TRUE
                    PERFORM VERIFICAR-SENHA
               WHEN GS-LE-CARTAO-TRUE
                    PERFORM LER-CARTAO
               WHEN GS-POPUP-CARTAO-TRUE
                    PERFORM POPUP-CARTAO
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM CHAMAR-POPUP-VENDEDOR
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LE-VENDEDOR
               WHEN GS-CADASTRO-TRUE
                    PERFORM CARREGA-CADASTRO
               WHEN GS-GRAVA-ANOTACAO-TRUE
                    PERFORM GRAVA-ANOTACOES
               WHEN GS-LISTA-ANOTACAO-TRUE
                    PERFORM CARREGA-LISTA-ANOTACAO
               WHEN GS-PRINTER-FLG-AGEN-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIMIR-CONTATO
                    END-IF
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       GRAVA-ANOTACOES SECTION.
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

           MOVE GS-LINDET(137: 9) TO COD-COMPL-W
           MOVE COD-COMPL-W       TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
               NOT AT END
                 IF COD-COMPL-CR200 <> COD-COMPL-W
                              MOVE "10" TO ST-CRD200
                 ELSE MOVE SEQ-CR200 TO ULT-SEQ
                      CONTINUE
             END-READ
           END-PERFORM.
           ADD 1 TO ULT-SEQ.
           COPY "CBDATA1.CPY".
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           MOVE ULT-SEQ      TO SEQ-CR200.
           MOVE COD-COMPL-W  TO COD-COMPL-CR200.
           MOVE GS-DATA-AGENDADA TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE DATA-DIA-I   TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200
      *    ATUALIZA A LISTA DE ANOTACAO
           MOVE SPACES           TO GS-LINDET1
           MOVE DATA-MOVTO-CR200 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO GS-LINDET1(1: 15)
           MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
           MOVE ":"                    TO HORA-E(3: 1)
           MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
           MOVE HORA-E           TO GS-LINDET1(16: 10)
           MOVE USUARIO-CR200    TO GS-LINDET1(26: 11)
           MOVE SEQ-CR200        TO GS-LINDET1(36: 10)
           MOVE "DATA AGENDADA: " TO GS-LINDET1(50: 15)
           MOVE DATA-RETORNO-CR200 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV                                                                                      v
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET1(65: 10)
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200      TO SEQ-CR201.
           MOVE COD-COMPL-W    TO COD-COMPL-CR201.
           MOVE ZEROS          TO SUBSEQ-CR201.

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
                 MOVE SPACES            TO GS-LINDET1
                 MOVE ANOTACAO-W        TO GS-LINDET1(16: 80)
                 MOVE "INSERE-LIST1"    TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-IF
           END-PERFORM.
           MOVE SPACES TO GS-LINDET1.
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE CRD200 CRD201.

       CARREGA-LISTA-ANOTACAO SECTION.
           OPEN INPUT CRD200 CRD201.
           IF ST-CRD200 = "35"
              CLOSE CRD200       OPEN OUTPUT CRD200  CLOSE CRD200
              OPEN INPUT CRD200.
           IF ST-CRD201 = "35"
              CLOSE CRD201       OPEN OUTPUT CRD201  CLOSE CRD201
              OPEN INPUT CRD201.
           MOVE GS-LINDET(137: 9) TO COD-COMPL-W
           MOVE COD-COMPL-W       TO COD-COMPL-CR200.
           MOVE ZEROS             TO SEQ-CR200.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
               MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
                NOT AT END
                  MOVE SPACES TO GS-LINDET1
                  IF COD-COMPL-CR200 <> COD-COMPL-W
                     MOVE "10" TO ST-CRD200
                  ELSE
                    MOVE DATA-MOVTO-CR200 TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV         TO DATA-E
                    MOVE DATA-E           TO GS-LINDET1(1: 15)
                    MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
                    MOVE ":"                    TO HORA-E(3: 1)
                    MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
                    MOVE HORA-E           TO GS-LINDET1(16: 10)
                    MOVE USUARIO-CR200    TO GS-LINDET1(26: 11)
                    MOVE SEQ-CR200        TO GS-LINDET1(36: 10)
                    MOVE "DATA AGENDADA: " TO GS-LINDET1(50: 15)
                    MOVE DATA-RETORNO-CR200 TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV           TO DATA-E
                    MOVE DATA-E             TO GS-LINDET1(65: 10)
                    MOVE "INSERE-LIST1" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                    PERFORM CARREGA-CRD201
              END-READ
           END-PERFORM.
           CLOSE CRD200 CRD201.

       CARREGA-CRD201 SECTION.
           MOVE COD-COMPL-W      TO COD-COMPL-CR201.
           MOVE SEQ-CR200        TO SEQ-CR201.
           MOVE ZEROS            TO SUBSEQ-CR201.
           START CRD201 KEY IS NOT < CHAVE-CR201 INVALID KEY
                 MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
              READ CRD201 NEXT RECORD AT END MOVE "10" TO ST-CRD201
                   NOT AT END
                     IF COD-COMPL-CR201 <> COD-COMPL-W OR
                        SEQ-CR201 <> SEQ-CR200
                          MOVE "10" TO ST-CRD201
                     ELSE
                        MOVE SPACES TO GS-LINDET1
                        MOVE ANOTACAO-CR201 TO GS-LINDET1(16: 80)
                        MOVE "INSERE-LIST1" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-LINDET1.
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


       CARREGA-CADASTRO SECTION.
           MOVE GS-LINDET(137: 9) TO STRING-1
           CALL   "CGP010A" USING PARAMETROS-W STRING-1
           CANCEL "CGP010A".

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POPUP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-VENDEDOR.

       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR        TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE "****"        TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-VENDEDOR.

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


       VERIFICAR-SENHA SECTION.
           MOVE "SENHA17"     TO PROGRAMA-CA004.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
                MOVE "Baixa Não Realizada, Usuário sem Permissão" TO
                MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM 140-EXIBIR-MENSAGEM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       BAIXAR-ITENS SECTION.
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE "Você tem certeza da Baixa que vai ser Realizada ?" TO
           MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM 140-EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE 0 TO GS-FLAG-CRITICA
              MOVE 0 TO GS-CONT
              PERFORM UNTIL GS-CONT = TOTAL-ITENS
                  ADD 1 TO GS-CONT
                  MOVE "LER-ITENS" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  IF GS-LINDET(1:1) = "B"
                     PERFORM BAIXA-TITULO
                  END-IF
              END-PERFORM
              MOVE "Baixa dos Títulos Realizada" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM
              MOVE 0 TO GS-FLAG-CRITICA
              PERFORM GRAVA-WORK
              PERFORM ZERA-VARIAVEIS
              PERFORM CARREGA-LISTA.

       BAIXA-TITULO SECTION.
           CLOSE    CRD020 CRD200 CRD201 CRD020B
           OPEN I-O CRD020 CRD200 CRD201 CRD020B
           MOVE GS-LINDET(137: 9) TO COD-COMPL-CR20
           MOVE GS-LINDET(146: 5) TO SEQ-CR20
           READ CRD020 INVALID KEY
                MOVE "Título Não Encontrado" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM 140-EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE ZEROS            TO JURO-RCTO-CR20
                MOVE ZEROS            TO MULTA-RCTO-CR20
                IF TOT-PARC-CR20 > 1
                   COMPUTE DESCONTO-CR20 = VALOR-TOT-CR20 *
                                        TAXA-ADMINIST-PARCELA-CR20 / 100
                   COMPUTE VALOR-LIQ-CR20 = VALOR-TOT-CR20 -
                  (VALOR-TOT-CR20 * TAXA-ADMINIST-PARCELA-CR20 / 100)
                ELSE
                   COMPUTE DESCONTO-CR20 = VALOR-TOT-CR20 *
                           TAXA-ADMINIST-CREDITO-CR20 / 100
                   COMPUTE VALOR-LIQ-CR20 = VALOR-TOT-CR20 -
                  (VALOR-TOT-CR20 * TAXA-ADMINIST-CREDITO-CR20 / 100)

                END-IF
                MOVE "6-Rec.Opera."   TO FORMA-PAGTO-CR20
                MOVE SPACES           TO DCR-MEM-CR20
                MOVE GS-DATA-RECEBIMENTO(1:2) TO AUX-DIA
                MOVE GS-DATA-RECEBIMENTO(3:2) TO AUX-MES
                MOVE GS-DATA-RECEBIMENTO(5:4) TO AUX-ANO
                MOVE DATA-AUX                 TO DATA-RCTO-CR20
                MOVE 2                        TO SITUACAO-CR20
                MOVE ZEROS                    TO VALOR-SALDO-CR20
                REWRITE REG-CRD020 NOT INVALID KEY
                     MOVE CLASS-CLIENTE-CR20 TO CLASS-CLIENTE-CR20B
                     MOVE CLIENTE-CR20       TO CLIENTE-CR20B
                     MOVE SEQ-CR20           TO SEQ-CR20B
                     MOVE ZEROS              TO JURO-RCTO-CR20B
                     MOVE ZEROS              TO MULTA-RCTO-CR20B
                     MOVE VALOR-TOT-CR20     TO VALOR-BAIXA-CR20B
                     MOVE VALOR-TOT-CR20     TO VALOR-TOT-CR20B
                     MOVE ZEROS              TO DESCONTO-CR20B
                     MOVE DATA-RCTO-CR20     TO DATA-RCTO-CR20B
                     MOVE VALOR-LIQ-CR20     TO VALOR-LIQ-CR20B
                     MOVE ZEROS              TO SEQ-CAIXA-CR20B
                     MOVE FORMA-PAGTO-CR20   TO FORMA-PAGTO-CR20B
                     MOVE SPACES             TO DCR-MEM-CR20B
                     WRITE REG-CRD020B
                     END-WRITE
                END-REWRITE
                PERFORM GRAVAR-ANOTACAO.

           CLOSE      CRD020 CRD200 CRD201 CRD020B
           OPEN INPUT CRD020 CRD200 CRD201 CRD020B.


       GRAVAR-ANOTACAO SECTION.
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
                 MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200      TO SEQ-CR201.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR201.
           MOVE "BAIXADO NA DT DE VENCTO CONF. CREDITO REPASSADO PELA OP
      -    "ERADORA NA DT DE RECTO"
                                    TO ANOTACAO-CR201.
           MOVE ZEROS TO ST-CRD201.
           MOVE 1              TO SUBSEQ-CR201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
               ADD 1 TO SUBSEQ-CR201
               CONTINUE
              NOT INVALID KEY
                MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.


       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1      to gs-flag-critica.


       MARCAR-ITEM SECTION.
           IF GS-LINDET = SPACES
              MOVE ZEROS TO GS-LINDET
           ELSE
              MOVE GS-TIPO(1:1) TO AUX-TIPO
              IF AUX-TIPO = 4
                 IF GS-LINDET(1:1) = "B"
                    MOVE SPACES    TO GS-LINDET(1:1)
                 ELSE
                    MOVE "B"       TO GS-LINDET(1:1)
                 END-IF
              ELSE
                 MOVE SPACES       TO GS-LINDET(1:1).

       POPUP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "COP040T".
           MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO.

       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR TO PORTADOR.
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT TO GS-DESC-PORTADOR.
       POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP018T".
           MOVE PASSAR-PARAMETROS(1: 30) TO GS-DESC-PORTADOR
           MOVE PASSAR-PARAMETROS(1: 30) TO GS-DESC-PORTADOR-T
           MOVE PASSAR-PARAMETROS(33: 4) TO GS-PORTADOR
           MOVE PASSAR-PARAMETROS(33: 4) TO GS-PORTADOR-T.
       TRANSFERE-PORTADOR SECTION.
           CLOSE    CRD020 CRD200 CRD201
           OPEN I-O CRD020 CRD200 CRD201

           MOVE ZEROS TO CLASSIF-WK CLIENTE-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      MOVE CLIENTE-WK   TO GS-EXIBE-TRANSF
                      MOVE "REFRESH-WIN5" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      MOVE GS-DESC-PORTADOR-T TO PORTADOR-WK
                      REWRITE REG-WORK
                      END-REWRITE
                      MOVE CLASSIF-WK    TO CLASS-CLIENTE-CR20
                      MOVE CLIENTE-WK    TO CLIENTE-CR20
                      MOVE SEQ-WK        TO SEQ-CR20
                      READ CRD020 INVALID KEY
                           CONTINUE
                      NOT INVALID KEY
                      PERFORM GRAVA-ANOTACAO
                      MOVE GS-PORTADOR-T TO PORTADOR-CR20
                      REWRITE REG-CRD020
                      END-REWRITE
                  END-READ
             END-READ
           END-PERFORM
           CLOSE      CRD020 CRD200 CRD201
           OPEN INPUT CRD020 CRD200 CRD201
           MOVE "UNSHOW-WIN5" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
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
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200      TO SEQ-CR201.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR201.
           MOVE "TRANSF.PORTADOR- DOCTO: XXXXXXXXXX - 01-XXXXXXXXXXXXXXX
      -    "X P/ 99-XXXXXXXXXXXXXXXX" TO ANOTACAO-CR201.
           MOVE NR-DOCTO-CR20       TO ANOTACAO-CR201(25: 11)
           MOVE PORTADOR-CR20       TO ANOTACAO-CR201(38: 4) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT           TO ANOTACAO-CR201(43: 16)
           MOVE GS-PORTADOR-T       TO ANOTACAO-CR201(63: 4) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT           TO ANOTACAO-CR201(69: 16)
           MOVE ZEROS TO ST-CRD201.
           MOVE 1              TO SUBSEQ-CR201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
               ADD 1 TO SUBSEQ-CR201
               CONTINUE
              NOT INVALID KEY
                MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

      *    ACCEPT VARIA-W FROM TIME.
           CLOSE          WORK
           OPEN OUTPUT    WORK
           CLOSE          WORK
           OPEN I-O       WORK

           MOVE GS-TIPO(1:1)   TO AUX-TIPO

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-VENCTO-INI  TO DATA-INV
                                  VENCTO-INI-ANT
                                  VENCTO-INI-REL

           CALL "GRIDAT2"   USING DATA-INV
           MOVE DATA-INV       TO VENCTO-INI
           MOVE GS-VENCTO-FIM  TO DATA-INV
                                  VENCTO-FIM-ANT
                                  VENCTO-FIM-REL

           MOVE GS-TIPO        TO TIPO-REL
           CALL "GRIDAT2"   USING DATA-INV
           MOVE DATA-INV       TO VENCTO-FIM

      *Pendente total
           INITIALIZE REG-CRD020
           MOVE VENCTO-INI     TO DATA-VENCTO-CR20

           IF GS-TIPO-CONTRATO = 1
              MOVE GS-CONTRATO TO CLIENTE-CR20(1: 4)
              MOVE ZEROS       TO CLIENTE-CR20(5: 4)
           ELSE
              MOVE ZEROS       TO CLIENTE-CR20
           END-IF

           MOVE ZEROS          TO SITUACAO-CR20
                                  CLASS-CLIENTE-CR20.

           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                  MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                  READ CRD020 NEXT RECORD AT END
                       MOVE "10" TO ST-CRD020
                   NOT AT END
                       IF SITUACAO-CR20    <> 0 OR
                          DATA-VENCTO-CR20  > VENCTO-FIM
                          MOVE "10" TO ST-CRD020
                       ELSE
                          IF GS-VENDEDOR = 0 OR VENDEDOR-CR20
                             IF GS-PORTADOR <> 00
                                IF GS-PORTADOR <> PORTADOR-CR20
                                   CONTINUE
                                ELSE
                                   IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                                      IF GS-ACP-CARTAO = 0 OR
                                         CARTAO-CRED-CR20
                                         PERFORM CONT-GRAVA-WORK
                                      END-IF
                                   END-IF
                                END-IF
                             ELSE
                                IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                                   IF GS-ACP-CARTAO = 0 OR
                                      CARTAO-CRED-CR20
                                      PERFORM CONT-GRAVA-WORK
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                  END-READ
           END-PERFORM

      *Baixa Parcial
           INITIALIZE REG-CRD020
           MOVE VENCTO-INI     TO DATA-VENCTO-CR20

           IF GS-TIPO-CONTRATO = 1
              MOVE GS-CONTRATO TO CLIENTE-CR20(1: 4)
              MOVE ZEROS       TO CLIENTE-CR20(5: 4)
           ELSE
              MOVE ZEROS       TO CLIENTE-CR20
           END-IF

           MOVE ZEROS          TO CLASS-CLIENTE-CR20
           MOVE 1              TO SITUACAO-CR20

           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                  MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                  READ CRD020 NEXT RECORD AT END
                       MOVE "10" TO ST-CRD020
                   NOT AT END
                       IF SITUACAO-CR20    <> 1 OR
                          DATA-VENCTO-CR20  > VENCTO-FIM
                          MOVE "10" TO ST-CRD020
                       ELSE
                          IF GS-VENDEDOR = 0 OR VENDEDOR-CR20
                             IF GS-PORTADOR <> 00
                                IF GS-PORTADOR <> PORTADOR-CR20
                                   CONTINUE
                                ELSE
                                   IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                                      IF GS-ACP-CARTAO = 0 OR
                                         CARTAO-CRED-CR20
                                         PERFORM CONT-GRAVA-WORK
                                      END-IF
                                   END-IF
                                END-IF
                             ELSE
                                IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                                   IF GS-ACP-CARTAO = 0 OR
                                      CARTAO-CRED-CR20
                                      PERFORM CONT-GRAVA-WORK
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                  END-READ
           END-PERFORM


           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CONT-GRAVA-WORK SECTION.
           IF DATA-VENCTO-CR20 < VENCTO-INI OR
              DATA-VENCTO-CR20 > VENCTO-FIM
              CONTINUE
           ELSE
             IF GS-TIPO-CONTRATO = 1
                MOVE CLIENTE-CR20 TO CLIENTE-WK
                IF CONTRATO-WK NOT = GS-CONTRATO
                   CONTINUE
                ELSE
                   PERFORM MOVER-DADOS-WORK
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM

                   WRITE REG-WORK
                   END-WRITE
                END-IF
             ELSE
                   PERFORM MOVER-DADOS-WORK
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM

                   WRITE REG-WORK
                   END-WRITE

           END-IF.

       MOVER-DADOS-WORK SECTION.
           MOVE DATA-VENCTO-CR20     TO VENCTO-WK
                                        GS-EXIBE-VENCTO
           MOVE CLASS-CLIENTE-CR20   TO CLASSIF-WK
                                        CLASSIF-CG10
                                        CLASSIF-CG11.
           MOVE CLIENTE-CR20         TO CLIENTE-WK
                                        CODIGO-CG10
                                        CODIGO-CG11.
           READ CGD010 INVALID KEY
                MOVE "*******"       TO COMPRADOR-CG10.

           MOVE COMPRADOR-CG10       TO NOME-CLIEN-WK
           READ CGD011 INVALID KEY
                MOVE ZEROS           TO CIDADE1-CG11.

           MOVE CIDADE1-CG11         TO CIDADE.
           READ CAD010 INVALID KEY
                MOVE "********"      TO NOME-CID
                MOVE "**"            TO UF-CID.

           MOVE NOME-CID             TO CIDADE-WK
           MOVE UF-CID               TO UF-WK
           MOVE SEQ-CR20             TO SEQ-WK
           MOVE PORTADOR-CR20        TO PORTADOR-WK
           MOVE NR-PARC-CR20         TO PARCELA-WK
           MOVE TOT-PARC-CR20        TO QT-PARCELA-WK
      *    READ CAD018 INVALID KEY MOVE "******" TO NOME-PORT.
      *    MOVE NOME-PORT           TO PORTADOR-WK.
           MOVE OUTRO-DOCTO-CR20    TO NOSSO-NR-WK.
           MOVE VALOR-SALDO-CR20    TO VALOR-BRUTO-WK
           IF TIPO-DOCTO-CR20 = 4
              IF TOT-PARC-CR20 > 1
                 COMPUTE VALOR-LIQUIDO-WK = VALOR-SALDO-CR20 -
                 (VALOR-SALDO-CR20 * TAXA-ADMINIST-PARCELA-CR20 / 100)
              ELSE
                 COMPUTE VALOR-LIQUIDO-WK = VALOR-SALDO-CR20 -
                 (VALOR-SALDO-CR20 * TAXA-ADMINIST-CREDITO-CR20 / 100)
              END-IF
           ELSE
              MOVE VALOR-SALDO-CR20   TO VALOR-LIQUIDO-WK
           END-IF
           EVALUATE TIPO-DOCTO-CR20
               WHEN 0 MOVE "BOLE"  TO TIPO-WK
               WHEN 1 MOVE "NTAP"  TO TIPO-WK
               WHEN 2 MOVE "ORGE"  TO TIPO-WK
               WHEN 3 MOVE "DEBT"  TO TIPO-WK
               WHEN 4 MOVE "CART"  TO TIPO-WK
           END-EVALUATE.

       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOTAL-ITENS
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO GS-TOTAL-PERIODO-LIQUIDO
                         GS-TOTAL-PERIODO-BRUTO
                         GS-TOTAL-VENCIDO-LIQUIDO
                         GS-TOTAL-VENCIDO-BRUTO
                         GS-TOTAL-AVENCER-LIQUIDO
                         GS-TOTAL-AVENCER-BRUTO
                         GS-QTDE-TITULOS
                         GS-PM TOTAL-GERAL-PM
                         TOTAL-LIQUIDO-W
                         TOTAL-BRUTO-W.

           MOVE ALL "-" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE
       "  NOME-CLIENTE  CIDADE        UF NR-BANCO        CONT     PO  TI
      -"PO DATA-VECTO PARC.  VALOR LIQ     TOTAL LIQ  VALOR BRU     TOTA
      -"L BRU REFERÊNCIA CRD020" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ALL "-" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           COMPUTE GS-PM = TOTAL-GERAL-PM / GS-TOTAL-PERIODO-LIQUIDO

           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "VENCTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK
                START WORK KEY IS NOT < VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CLIENTE" TO GS-DESCR-ORDEM
                MOVE SPACES TO NOME-CLIEN-WK
                START WORK KEY IS NOT < NOME-CLIEN-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "PORTADOR/CART" TO GS-DESCR-ORDEM
                MOVE ZEROS TO PORTADOR-WK
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "VCTO/V_BRUTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK VALOR-BRUTO-WK
                START WORK KEY IS NOT < ALT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "CIDADE    " TO GS-DESCR-ORDEM
                MOVE SPACES TO CIDADE-WK
                START WORK KEY IS NOT < CIDADE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "CONTRATO  " TO GS-DESCR-ORDEM
                MOVE ZEROS  TO CLASSIF-WK
                MOVE ZEROS  TO CONTRATO-WK
                MOVE ZEROS  TO ALBUM-WK
                MOVE ZEROS  TO VENCTO-WK
                START WORK KEY IS NOT < CH-CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 7
                MOVE "VCTO/V_LIQ." TO GS-DESCR-ORDEM
                MOVE ZEROS  TO VENCTO-WK VALOR-LIQUIDO-WK
                START WORK KEY IS NOT < ALT2-WK INVALID KEY
                      MOVE "10" TO ST-WORK

           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF NOME-CLIEN-ANT  NOT = ZEROS
                 IF NOME-CLIEN-ANT NOT = NOME-CLIEN-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF PORTADOR-ANT NOT = ZEROS
                 IF PORTADOR-ANT NOT = PORTADOR-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
             WHEN 5
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA
             WHEN 6
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK OR
                    CLASSIF-ANT  NOT = CLASSIF-WK
                    PERFORM TOTALIZA
             WHEN 7
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           ADD 1                  TO TOTAL-ITENS
           MOVE SPACES            TO GS-LINDET(1:2)
           MOVE NOME-CLIEN-WK     TO GS-LINDET(03: 13)
           MOVE CIDADE-WK         TO GS-LINDET(17: 14)
           MOVE UF-WK             TO GS-LINDET(31: 3)
           MOVE NOSSO-NR-WK       TO GS-LINDET(34: 16)
           MOVE CLIENTE-WK        TO GS-LINDET(50: 9)
           MOVE PORTADOR-WK       TO GS-LINDET(59: 4)
           MOVE TIPO-WK           TO GS-LINDET(65: 5)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           IF VENCTO-WK > DATA-DIA-I
              MOVE DATA-DIA-I     TO GRDIAS-AAMMDD-INICIAL
              MOVE VENCTO-WK      TO GRDIAS-AAMMDD-FINAL
           ELSE
              MOVE DATA-DIA-I     TO GRDIAS-AAMMDD-FINAL
              MOVE VENCTO-WK      TO GRDIAS-AAMMDD-INICIAL
           END-IF
           CALL "GRDIAS1" USING PARAMETROS-GRDIAS

           COMPUTE TOTAL-GERAL-PM = (GRDIAS-NUM-DIAS * VALOR-LIQUIDO-WK)
                                     + TOTAL-GERAL-PM

           MOVE DATA-E            TO GS-LINDET(70: 11)
           MOVE PARCELA-WK        TO GS-LINDET(81:2)
           MOVE "/"               TO GS-LINDET(83:1)
           MOVE QT-PARCELA-WK     TO GS-LINDET(84:2)
           MOVE VALOR-LIQUIDO-WK  TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(87: 11)

           ADD VALOR-LIQUIDO-WK   TO TOTAL-LIQUIDO-W
                                     GS-TOTAL-PERIODO-LIQUIDO.

           ADD 1                  TO GS-QTDE-TITULOS
           MOVE TOTAL-LIQUIDO-W   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(98: 13)

           IF VENCTO-WK < DATA-DIA-I
              ADD VALOR-LIQUIDO-WK  TO GS-TOTAL-VENCIDO-LIQUIDO
              ADD VALOR-BRUTO-WK    TO GS-TOTAL-VENCIDO-BRUTO
           ELSE
              ADD VALOR-LIQUIDO-WK  TO GS-TOTAL-AVENCER-LIQUIDO
              ADD VALOR-LIQUIDO-WK  TO GS-TOTAL-AVENCER-BRUTO.

           MOVE VALOR-BRUTO-WK      TO VALOR-E1
           MOVE VALOR-E1            TO GS-LINDET(112:11)

           ADD VALOR-BRUTO-WK       TO TOTAL-BRUTO-W
                                       GS-TOTAL-PERIODO-BRUTO

           MOVE TOTAL-BRUTO-W       TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET(123:13)

           MOVE CLASSIF-WK        TO GS-LINDET(137: 1)
           MOVE CLIENTE-WK        TO GS-LINDET(138: 8)
           MOVE SEQ-WK            TO GS-LINDET(146: 5).

       ZERA-VARIAVEIS SECTION.
           MOVE SPACES TO NOME-CLIEN-ANT CIDADE-ANT.
           MOVE ZEROS  TO VENCTO-ANT     TOTAL-LIQUIDO-W  TOTAL-BRUTO-W
                          PORTADOR-ANT   CONTRATO-ANT     CLASSIF-ANT.
       MOVER-CHAVE-ANT SECTION.
           MOVE NOME-CLIEN-WK     TO NOME-CLIEN-ANT.
           MOVE VENCTO-WK         TO VENCTO-ANT.
           MOVE PORTADOR-WK       TO PORTADOR-ANT.
           MOVE CIDADE-WK         TO CIDADE-ANT.
           MOVE CONTRATO-WK       TO CONTRATO-ANT.
           MOVE CLASSIF-WK        TO CLASSIF-ANT.
       TOTALIZA SECTION.
           ADD 1 TO TOTAL-ITENS
           MOVE ZEROS TO TOTAL-LIQUIDO-W TOTAL-BRUTO-W.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CHAMA-ALTERACAO SECTION.
           IF GS-LINDET = SPACES MOVE ZEROS TO GS-LINDET.
           MOVE GS-LINDET(137: 9) TO PASSAR-STRING(1: 9)
                                     COD-COMPL-CR20.
           MOVE GS-LINDET(146: 5) TO PASSAR-STRING(10: 5) SEQ-CR20.
           MOVE USUARIO-W         TO PASSAR-STRING(20: 5)
           MOVE "CRP020" TO PROGRAMA-CA004.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
                CALL   "CRP020B" USING PARAMETROS-W PASSAR-STRING
                CANCEL "CRP020B"
              NOT INVALID KEY
                CALL   "CRP020A" USING PARAMETROS-W PASSAR-STRING
                CANCEL "CRP020A".
           READ CRD020.
           MOVE CLASS-CLIENTE-CR20 TO CLASSIF-WK.
           MOVE CLIENTE-CR20       TO CLIENTE-WK.
           MOVE SEQ-CR20           TO SEQ-WK.
           READ WORK.
           IF SITUACAO-CR20 = 4
              DELETE WORK
           ELSE
              PERFORM MOVER-DADOS-WORK
              REWRITE REG-WORK.

           PERFORM ZERA-VARIAVEIS
           PERFORM CARREGA-LISTA.

      *    PERFORM MOVER-DADOS.
      *    MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.


      *    Deveria ser regravado o arquivo work, pois pode ter havido
      *    alguma alteração no arquivo em que diferencie a classificação
      *    por exemplo, mudar a data de vencto fora do intervalo soli-
      *    citado pelo usuário. Mas foi solicitado que não regrave.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP051" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W
                         TOTAL-LIQUIDO-W
                         TOTAL-BRUTO-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           MOVE GS-QTDE-TITULOS   TO QTDE-TITULO-REL.
           MOVE GS-TOTAL-PERIODO-LIQUIDO  TO TOTAL-PERIODO-REL.
           MOVE GS-TOTAL-VENCIDO-LIQUIDO  TO TOTAL-VENCIDO-REL.
           MOVE GS-TOTAL-AVENCER-LIQUIDO  TO TOTAL-AVENCER-REL.
           MOVE GS-TOTAL-PERIODO-BRUTO    TO TOTAL-PERIODO-REL2.
           MOVE GS-TOTAL-VENCIDO-BRUTO    TO TOTAL-VENCIDO-REL2.
           MOVE GS-TOTAL-AVENCER-BRUTO    TO TOTAL-AVENCER-REL2.
           MOVE GS-PM             TO PM-REL
           WRITE REG-RELAT FROM LINTOT1 AFTER 2.
           WRITE REG-RELAT FROM LINTOT2

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF NOME-CLIEN-ANT NOT = ZEROS
                 IF NOME-CLIEN-ANT NOT = NOME-CLIEN-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF PORTADOR-ANT NOT = ZEROS
                 IF PORTADOR-ANT NOT = PORTADOR-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA-REL
             WHEN 6
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK OR
                    CLASSIF-ANT  NOT = CLASSIF-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.

           MOVE NOME-CLIEN-WK     TO LINDET-REL(01: 16)
           MOVE CIDADE-WK         TO LINDET-REL(17: 14)
           MOVE UF-WK             TO LINDET-REL(31: 3)
           MOVE NOSSO-NR-WK       TO LINDET-REL(34: 16)
           MOVE CLIENTE-WK        TO LINDET-REL(50: 9)
           MOVE PORTADOR-WK       TO LINDET-REL(59: 4)
           MOVE TIPO-WK           TO LINDET-REL(65: 5)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(70: 11)
           MOVE PARCELA-WK        TO LINDET-REL(81:2)
           MOVE "/"               TO LINDET-REL(83:1)
           MOVE QT-PARCELA-WK     TO LINDET-REL(84:2)
           MOVE VALOR-LIQUIDO-WK  TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(87: 11)
           ADD VALOR-LIQUIDO-WK   TO TOTAL-LIQUIDO-W.
           MOVE TOTAL-LIQUIDO-W   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(98: 13)
           MOVE VALOR-BRUTO-WK    TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(111:13)
           ADD VALOR-BRUTO-WK     TO TOTAL-BRUTO-W
           MOVE TOTAL-BRUTO-W     TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(124:13)

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE ZEROS TO TOTAL-LIQUIDO-W.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.

       IMPRIMIR-CONTATO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           OPEN INPUT CRD200 CRD201.
           PERFORM CABECALHO-AGENDA.
           MOVE COD-COMPL-W       TO COD-COMPL-CR200.
           MOVE ZEROS             TO SEQ-CR200.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
                 READ CRD200 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD200
                 NOT AT END
                     IF COD-COMPL-CR200 <> COD-COMPL-W
                        MOVE "10" TO ST-CRD200
                     ELSE
                        MOVE SPACES           TO LINDET-REL
                        MOVE SEQ-CR200        TO SEQ-CR201
                                                 LINDET-REL(1: 5)
                        MOVE COD-COMPL-CR200  TO COD-COMPL-CR201
                        MOVE DATA-MOVTO-CR200 TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV         TO DATA-E
                        MOVE DATA-E           TO LINDET-REL(7: 15)
                        MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
                        MOVE ":"                    TO HORA-E(3: 1)
                        MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
                        MOVE HORA-E           TO LINDET-REL(22: 7)
                        MOVE USUARIO-CR200    TO LINDET-REL(29: 10)
                        WRITE REG-RELAT FROM LINDET AFTER 2
                        ADD 2 TO LIN
                        IF LIN > 57
                           PERFORM CABECALHO-AGENDA
                        END-IF
                        PERFORM IMPRIME-TEXTO
                     END-IF
                 END-READ
           END-PERFORM.
           CLOSE CRD200 CRD201.

           COPY DESCONDENSA.

       IMPRIME-TEXTO SECTION.
           MOVE ZEROS TO SUBSEQ-CR201.
           START CRD201 KEY IS NOT < CHAVE-CR201 INVALID KEY
                 MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
              READ CRD201 NEXT RECORD AT END MOVE "10" TO ST-CRD201
                NOT AT END
                    IF COD-COMPL-CR201 <> COD-COMPL-CR200
                       MOVE "10" TO ST-CRD201
                    ELSE
                       MOVE SPACES TO LINDET-REL
                       MOVE ANOTACAO-CR201   TO LINDET-REL
                       WRITE REG-RELAT FROM LINDET-REL
                       ADD 1 TO LIN
                       IF LIN > 57 PERFORM CABECALHO
                       END-IF
                    END-IF
              END-READ
           END-PERFORM.

       CABECALHO-AGENDA SECTION.
           MOVE COD-COMPL-W(1:1) TO CLASSIF-REL
           MOVE COD-COMPL-W(2:8) TO CLIENTE-REL
           MOVE COD-COMPL-W      TO COD-COMPL-CG10
           READ CGD010 INVALID KEY
                MOVE "*****"     TO COMPRADOR-CG10
           END-READ
           MOVE COMPRADOR-CG10   TO NOME-CLIENTE-REL.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02A AFTER 2.
           WRITE REG-RELAT FROM CAB06 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.

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
           move "CRP051"            to logacess-programa
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

           CLOSE CRD020 CAD018 CGD010 CAD010 CGD011 CAD004 CGD020
                 CRD020B.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
