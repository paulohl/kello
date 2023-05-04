       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP020C.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 03/07/2000.
      *FUNÇÃO: TABELA DE CUSTO DOS PRODUTOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX004.
           COPY COPX040.
           COPY MTPX020CU.
           COPY LOGX004.
           COPY LOGACESS.SEL.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW004.
           COPY COPW040.
           COPY MTPW020CU.
           COPY LOGW004.
           COPY LOGACESS.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "MTP020C.CPB".
           COPY "MTP020C.CPY".
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
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-MTD020CU           PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-LOG004             PIC XX       VALUE SPACES.
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
           05  DATAMOV-W             PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  IDENTIFICACAO-W       PIC X(30)    VALUE SPACES.
           05  QT-FITA-W             PIC 9        VALUE ZEROS.
           05  QT-DVD-W              PIC 9        VALUE ZEROS.
           05  QT-PFITA-W            PIC 9        VALUE ZEROS.
           05  VISITA-W              PIC 9(3)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  TOTAL-AVULSA          PIC 9(09).
           05  TOTAL-FOTOS           PIC 9(09).
           05  TOTAL-CLIENTES        PIC 9(09).
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  LIBERADO              PIC 9(01)    VALUE ZEROS.

           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
      *    COPY "PARAMETR".
           COPY IMPRESSORA.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(48)   VALUE
           "MOVIMENTO PLANILHA MONTAGEM ALBUM".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(5)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "ALB. ES EN FOLH FOTO PO FI PF DVD FC MO PD BK NOME-CLIENTE
      -    "        MES/ANO V".

       01  LINDET.
           05  LINDET-REL          PIC X(90)  VALUE SPACES.

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

       01 masc-qtde                    PIC Z.ZZZ.ZZ9 BLANK WHEN ZEROS.

       01 lnktabela.
          02 lnkobjetoscol object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas
                           pic 9(09) comp-5 value zeros occurs 99 times.

       01 indice           pic 9(02).

       01 wssize           pic 9(09) comp-5 value zeros.
       01 wsIndice         pic 9(09) comp-5 value zeros.

       77 wsTexto          pic x(255) value spaces.
       77 wsItem           pic 9(009) comp-5 value zeros.
       77 UMITEM           object reference.
       77 UMOBJETO         object reference.

       01 lnkusu.
          copy usuario.cpy.

       LINKAGE SECTION.
           COPY "PARAMETR".

       01 LNK-CONTRATO                 PIC 9(04).

       PROCEDURE DIVISION USING PARAMETROS-W LNK-CONTRATO.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
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
           MOVE "CAD004"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-CAD004.
           MOVE "COD040"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-COD040.
           MOVE "MTD020CU"         TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-MTD020CU.
           MOVE "LOG004"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-LOG004.
           MOVE "LOGACESS"         TO ARQ-REC.
           MOVE EMPRESA-REF        TO ARQUIVO-LOGACESS

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario

           OPEN I-O   LOG004 MTD020CU
           CLOSE      MTD020CU
           OPEN I-O   MTD020CU

           OPEN INPUT COD040 CAD004.

           IF ST-MTD020CU = "35"
              CLOSE MTD020CU     OPEN OUTPUT MTD020CU
              CLOSE MTD020CU     OPEN I-O MTD020CU
           END-IF.
           IF ST-LOG004 = "35"
              CLOSE LOG004      OPEN OUTPUT LOG004
              CLOSE LOG004      OPEN I-O LOG004
           END-IF.
           IF ST-LOG004 <> "00"
              MOVE "ERRO ABERTURA LOG004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020CU <> "00"
              MOVE "ERRO ABERTURA MTD020CU: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020CU TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      LOG004 MTD020CU
           OPEN INPUT MTD020CU

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP020C"           to logacess-programa
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

           MOVE LNK-CONTRATO       TO GS-CONTRATO
                                      NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                INITIALIZE REG-COD040.

           MOVE IDENTIFICACAO-CO40           TO GS-IDENTIFICACAO

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
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   SET-FOCUS D-ESTOJO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop"

          move function current-date to ws-data-sys

          MOVE "SENHA68"         TO PROGRAMA-CA004
          MOVE COD-USUARIO-W     TO COD-USUARIO-CA004
          READ CAD004 INVALID KEY
               DISABLE-OBJECT PB1
               DISABLE-OBJECT PB3
          NOT INVALID KEY
               ENABLE-OBJECT PB1
               ENABLE-OBJECT PB3
          END-READ.

      *----------------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    MTD020CU
           OPEN I-O LOG004 MTD020CU

           MOVE GS-CONTRATO   TO CONTRATO-MTG-CU
           READ MTD020CU INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE MTD020CU NOT INVALID KEY
                       MOVE USUARIO-W     TO LOG4-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU   TO LOG4-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS   TO LOG4-HORAS
                       MOVE "E"           TO LOG4-OPERACAO
                       MOVE "MTD020CU"    TO LOG4-ARQUIVO
                       MOVE "MTP020C"     TO LOG4-PROGRAMA
                       MOVE REG-MTD020CU  TO LOG4-REGISTRO
                       WRITE REG-LOG004
                       END-WRITE
                END-DELETE
           END-READ

           CLOSE      LOG004 MTD020CU
           OPEN INPUT MTD020CU

           PERFORM LIMPAR-DADOS.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           MOVE GS-CONTRATO      TO CONTRATO-W
           INITIALIZE REG-MTD020CU
           INITIALIZE GS-DATA-BLOCK
           MOVE CONTRATO-W       TO GS-CONTRATO
           MOVE IDENTIFICACAO-W  TO GS-IDENTIFICACAO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO            TO CONTRATO-MTG-CU
           MOVE GS-CUSTO-ESTOJO        TO CUSTO-ESTOJO-MTG-CU
           MOVE GS-CUSTO-ENCADER       TO CUSTO-ENCADER-MTG-CU
           MOVE GS-CUSTO-FOLHA         TO CUSTO-FOLHA-MTG-CU
           MOVE GS-CUSTO-FOTO          TO CUSTO-FOTO-MTG-CU
           MOVE GS-CUSTO-FITA          TO CUSTO-FITA-MTG-CU
           MOVE GS-CUSTO-DVD           TO CUSTO-DVD-MTG-CU
           MOVE GS-CUSTO-POSTER        TO CUSTO-POSTER-MTG-CU
           MOVE GS-CUSTO-PORTA-FITA    TO CUSTO-PORTA-FITA-MTG-CU
           MOVE GS-CUSTO-FOTO-CD       TO CUSTO-FOTO-CD-MTG-CU
           MOVE GS-CUSTO-MOLDURA       TO CUSTO-MOLDURA-MTG-CU
           MOVE GS-CUSTO-PORTA-DVD     TO CUSTO-PORTA-DVD-MTG-CU
           MOVE GS-CUSTO-BOOK          TO CUSTO-BOOK-MTG-CU
           MOVE GS-CUSTO-PORTA-RETRATO TO CUSTO-PORTA-RETRATO-MTG-CU
           MOVE GS-CUSTO-PENDRIVE      TO CUSTO-PENDRIVE-MTG-CU
           MOVE GS-CUSTO-VIDEO-HD      TO CUSTO-VIDEO-HD-MTG-CU
           MOVE GS-CUSTO-REVISTA       TO CUSTO-REVISTA-MTG-CU
           MOVE GS-CUSTO-CALENDARIO    TO CUSTO-CALENDARIO-MTG-CU.

       GRAVA-DADOS SECTION.
           CLOSE    MTD020CU
           OPEN I-O LOG004 MTD020CU
           MOVE SPACES TO MENSAGEM

           WRITE REG-MTD020CU INVALID KEY
                 MOVE SPACES                       TO MENSAGEM
                 STRING "Erro ao Gravar o MTD020CU" X"0A"
                        "Status => " ST-MTD020CU INTO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                 MOVE USUARIO-W     TO LOG4-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU   TO LOG4-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS   TO LOG4-HORAS
                 MOVE "I"           TO LOG4-OPERACAO
                 MOVE "MTD020CU"    TO LOG4-ARQUIVO
                 MOVE "MTP020C"     TO LOG4-PROGRAMA
                 MOVE REG-MTD020CU  TO LOG4-REGISTRO
                 WRITE REG-LOG004
                 END-WRITE
           END-WRITE

           CLOSE      LOG004 MTD020CU
           OPEN INPUT MTD020CU.

       REGRAVA-DADOS SECTION.
           CLOSE    MTD020CU
           OPEN I-O LOG004 MTD020CU
           REWRITE REG-MTD020CU INVALID KEY
                 MOVE "Erro Regravacao MTD020CU" TO GS-MENSAGEM-ERRO
                 MOVE ST-MTD020CU TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE USUARIO-W    TO LOG4-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU  TO LOG4-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS  TO LOG4-HORAS
                 MOVE "A"          TO LOG4-OPERACAO
                 MOVE "MTD020CU"   TO LOG4-ARQUIVO
                 MOVE "MTP020C"    TO LOG4-PROGRAMA
                 MOVE REG-MTD020CU TO LOG4-REGISTRO
                 WRITE REG-LOG004
                 END-WRITE
           END-REWRITE

           CLOSE      LOG004 MTD020CU
           OPEN INPUT MTD020CU.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CARREGA-ULTIMOS SECTION.
           MOVE GS-CONTRATO          TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE SPACES          TO IDENTIFICACAO-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40   TO GS-IDENTIFICACAO.

           MOVE GS-CONTRATO          TO CONTRATO-MTG-CU
           READ MTD020CU INVALID KEY
                INITIALIZE REG-MTD020CU
                MOVE 0 TO GS-TIPO-GRAVACAO
           NOT INVALID KEY
                MOVE 1 TO GS-TIPO-GRAVACAO
           END-READ

           MOVE CUSTO-ESTOJO-MTG-CU        TO GS-CUSTO-ESTOJO
           MOVE CUSTO-ENCADER-MTG-CU       TO GS-CUSTO-ENCADER
           MOVE CUSTO-FOLHA-MTG-CU         TO GS-CUSTO-FOLHA
           MOVE CUSTO-FOTO-MTG-CU          TO GS-CUSTO-FOTO
           MOVE CUSTO-FITA-MTG-CU          TO GS-CUSTO-FITA
           MOVE CUSTO-DVD-MTG-CU           TO GS-CUSTO-DVD
           MOVE CUSTO-POSTER-MTG-CU        TO GS-CUSTO-POSTER
           MOVE CUSTO-PORTA-FITA-MTG-CU    TO GS-CUSTO-PORTA-FITA
           MOVE CUSTO-FOTO-CD-MTG-CU       TO GS-CUSTO-FOTO-CD
           MOVE CUSTO-MOLDURA-MTG-CU       TO GS-CUSTO-MOLDURA
           MOVE CUSTO-PORTA-DVD-MTG-CU     TO GS-CUSTO-PORTA-DVD
           MOVE CUSTO-BOOK-MTG-CU          TO GS-CUSTO-BOOK
           MOVE CUSTO-PORTA-RETRATO-MTG-CU TO GS-CUSTO-PORTA-RETRATO
           MOVE CUSTO-PENDRIVE-MTG-CU      TO GS-CUSTO-PENDRIVE
           MOVE CUSTO-VIDEO-HD-MTG-CU      TO GS-CUSTO-VIDEO-HD
           MOVE CUSTO-REVISTA-MTG-CU       TO GS-CUSTO-REVISTA
           MOVE CUSTO-CALENDARIO-MTG-CU    TO GS-CUSTO-CALENDARIO

           REFRESH-OBJECT PRINCIPAL.

      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP020C"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       EXIBIR-MENSAGEM SECTION.
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
           CLOSE COD040 CAD004 MTD020CU.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP020C"           to logacess-programa
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
