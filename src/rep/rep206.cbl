       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP206.
      *DATA: 08/05/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: ESTATISTICA DE REPORTAGEM POR ORDEM SOLICITADA
      *        Listar todos as reportagens que estiverem
      *        dentro do intervalo de reportagem solicitado.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX004.
           COPY CAPX012.
           COPY COPX001.
           COPY COPX003.
           COPY COPX040.
           COPY COPX041.
           COPY REPX100.
           COPY REPX101.
           COPY REPX206.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS ORDEM-WK.

           SELECT EVENTO ASSIGN TO ARQUIVO-EVENTO
                         ORGANIZATION IS INDEXED
                         ACCESS MODE IS DYNAMIC
                         STATUS IS ST-EVENTO
                         RECORD KEY IS EVE-CHAVE.

           SELECT PARTIC ASSIGN TO ARQUIVO-PARTIC
                         ORGANIZATION IS INDEXED
                         ACCESS MODE IS DYNAMIC
                         STATUS IS ST-PARTIC
                         RECORD KEY IS PARTIC-CHAVE.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW004.
       COPY CAPW012.
       COPY COPW001.
       COPY COPW003.
       COPY COPW040.
       COPY COPW041.
       COPY REPW100.
       COPY REPW101.
       COPY REPW206.

       FD  WORK.
       01  REG-WORK.
           05  ORDEM-WK            PIC X(10).
           05  QT-RELAT-WK         PIC 9(5).
           05  FORMANDO-WK         PIC 9(6).
           05  PARTICIPANTE-WK     PIC 9(6).
           05  QT-CONTRATO-WK      PIC 9(6).
           05  QT-EQUIPE-WK        PIC 9(6)V9.
           05  QT-EVENTO-WK        PIC 9(5).
           05  QT-FILME-WK         PIC 9(6)V99.
           05  QT-FITA-WK          PIC 9(6)V99.
           05  VLR-REPORT-WK       PIC 9(8)V99.
           05  VLR-DESPESA-WK      PIC 9(8)V99.

       FD EVENTO.
       01 REG-EVENTO.
          05 EVE-CHAVE.
             10 EVE-ORDEM          PIC X(10).
             10 EVE-CONTR          PIC 9(04).
             10 EVE-CODIGO         PIC 9(05).

       FD PARTIC.
       01 REG-PARTIC.
          05 PARTIC-CHAVE.
             10 PARTIC-DOCTO       PIC 9(06).
          05 PARTIC-QTDE           PIC 9(06).
          05 PARTIC-EVENTO         PIC 9(06).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "REP206.CPB".
           COPY "REP206.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-RED206             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD041             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-EVENTO             PIC XX       VALUE SPACES.
           05  ST-PARTIC             PIC XX       VALUE SPACES.
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
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  DATA-W                PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  CIDADE-W              PIC X(10)    VALUE SPACES.
           05  REGIAO-W              PIC X(10)    VALUE SPACES.
           05  QTDE-E                PIC ZZ.ZZZ.
           05  QTDE-E1               PIC ZZZ.ZZZ.
           05  TOT-PARTICIPANTE      PIC 9(6)     VALUE ZEROS.
           05  TOT-FORMANDO          PIC 9(6)     VALUE ZEROS.
           05  TOT-EVENTO            PIC 9(5)     VALUE ZEROS.
           05  TOT-CONTRATO          PIC 9(6)     VALUE ZEROS.
           05  EVENTO-ANT            PIC 9(5)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  DESPESA-W             PIC 9(8)V99  VALUE ZEROS.

           05  TOT-GER-RELATORIO     PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-CONTRATO      PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-FORMANDO      PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-EVENTO        PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-PARTICIPANTE  PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-EQUIPE        PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-FITA          PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-FILME         PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-VLR-REPORT    PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-VLR-DESPESA   PIC 9(8)V99  VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(40).
           05  ACHEI                 PIC X(01)    VALUE SPACES.

           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.

           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).

           05  QT-EQUIPE             PIC 9(6)V9.
           05  QT-EVENTO             PIC 9(5).
           05  QT-FILME              PIC 9(6)V99.
           05  QT-FITA               PIC 9(6)V99.
           05  VLR-REPORT            PIC 9(8)V99.
           05  VLR-DESPESA           PIC 9(8)V99.

           05  AUX-EVENTO            PIC 9(05) VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "ESTATISTICA DE REPORTAGEM   ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB02A.
           05  FILLER              PIC X(08)   VALUE "CIDADE: ".
           05  CIDADE-REL          PIC ZZZZ    BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-CID-REL        PIC X(30)   VALUE ZEROS.
       01  CAB02B.
           05  FILLER              PIC X(08)   VALUE "REGIAO: ".
           05  REGIAO-REL          PIC ZZZZ    BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-REG-REL        PIC X(30)   VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(111)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(111)  VALUE
           "ORDEM      QT.REL QT.CONT QT.FORM Q.EVEN QT.PART QT.EQUI QT-
      -    "FITA QT-FILM   VLR-REPORT. VLR-DESPES     VLR-TOTAL".
       01  LINDET.
           05  LINDET-REL          PIC X(111)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(111)  VALUE
           "Q.RELAT QT.CONT QT.FORM QT.EVE  QT.PART  QT.EQUIPE  QT.FITA
      -    " QT-FILME    TOT-REPORT   TOT-DESPESA   TOTAL-GERAL".
       01  LINTOT.
           05  LINTOT-REL          PIC X(110)  VALUE SPACES.

           copy impressora.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD041"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD041.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           MOVE "RED206"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED206.
           ACCEPT VARIA-W FROM TIME.

           MOVE SPACES    TO ARQUIVO-EVENTO
           STRING "EVENTO-" USUARIO-W INTO ARQUIVO-EVENTO

           MOVE SPACES    TO ARQUIVO-PARTIC
           STRING "PARTIC-" USUARIO-W INTO ARQUIVO-PARTIC

           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN I-O   RED206
           CLOSE      RED206
           OPEN INPUT RED206

           OPEN INPUT CAD010 CAD012 COD003 COD040 RED100 RED101
                      COD001 COD041.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD041 <> "00"
              MOVE "ERRO ABERTURA COD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD041 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED101 <> "00"
              MOVE "ERRO ABERTURA RED101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED206 <> "00"
              MOVE "ERRO ABERTURA RED206: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED206 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM VERIFICAR-SENHA-STATUS
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-CIDADE-TRUE
                    PERFORM CHAMAR-POPUP-CIDADE
               WHEN GS-LE-CIDADE-TRUE
                   PERFORM LE-CIDADE
               WHEN GS-POPUP-REGIAO-TRUE
                    PERFORM CHAMAR-POPUP-REGIAO
               WHEN GS-LE-REGIAO-TRUE
                   PERFORM LE-REGIAO
               WHEN GS-LE-STATUS-TRUE
                    PERFORM LE-STATUS
               WHEN GS-CHAMAR-POP-STATUS-TRUE
                    PERFORM CHAMAR-POPUP-STATUS
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR
               WHEN GS-CARREGAR-STATUS-TRUE
                    PERFORM CARREGAR-STATUS
               WHEN GS-GRAVA-STATUS-TRUE
                    PERFORM GRAVA-STATUS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB12
           NOT INVALID KEY
               ENABLE-OBJECT PB12.

           CLOSE CAD004.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GRAVA-STATUS SECTION.
           CLOSE    RED206
           OPEN I-O RED206

           INITIALIZE REG-RED206
           START RED206 KEY IS NOT LESS CODIGO-RED206 INVALID KEY
                MOVE "10" TO ST-RED206.
           PERFORM UNTIL ST-RED206 = "10"
                READ RED206 NEXT AT END
                     MOVE "10" TO ST-RED206
                NOT AT END
                     DELETE RED206 INVALID KEY
                         MOVE "Erro de Exclusão...RED206" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                     END-DELETE
                END-READ
           END-PERFORM

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-RED206
               WRITE REG-RED206
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      RED206
           OPEN INPUT RED206.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-RED206
           START RED206 KEY IS NOT LESS CODIGO-RED206 INVALID KEY
               MOVE "10" TO ST-RED206.

           PERFORM UNTIL ST-RED206 = "10"
               READ RED206 NEXT AT END
                    MOVE "10" TO ST-RED206
               NOT AT END
                    MOVE CODIGO-RED206 TO CODIGO-CO01
                    READ COD001 NOT INVALID KEY
                         MOVE "S"              TO ACHEI
                         MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                         MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                         MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                    END-READ
               END-READ
           END-PERFORM


           IF ACHEI = "N"
              CLOSE      RED206
              OPEN I-O   RED206
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO CODIGO-RED206
                        WRITE REG-RED206

                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM
              CLOSE      RED206
              OPEN INPUT RED206.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.

       INCLUIR SECTION.
           MOVE "Você Deseja Incluir o Status?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE GS-STATUS        TO GS-LINHA-STATUS(1:2)
              MOVE GS-DESC-STATUS   TO GS-LINHA-STATUS(4:30)
              MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       LE-STATUS SECTION.
           MOVE GS-STATUS              TO CODIGO-CO01
           READ COD001 INVALID KEY
                MOVE SPACES            TO STATUS-CO01
           END-READ
           MOVE STATUS-CO01            TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       CHAMAR-POPUP-STATUS SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP001T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       VERIFICAR-IGUAL SECTION.
           MOVE 0   TO GS-FLAG-CRITICA
           MOVE "N" TO ACHEI
           MOVE 1   TO GS-CONT
           MOVE SPACES TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               IF GS-LINHA-STATUS(1:2) = GS-STATUS
                  MOVE "S" TO ACHEI
                  EXIT PERFORM
               ELSE
                  ADD 1 TO GS-CONT
                  MOVE SPACES TO GS-LINHA-STATUS
                  MOVE "LER-LINHA" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               END-IF
           END-PERFORM

           IF ACHEI = "S"
              MOVE "Status já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CHAMAR-POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-CID.
       LE-CIDADE SECTION.
           MOVE GS-CIDADE  TO CIDADE.
           READ CAD010 INVALID KEY MOVE "****" TO NOME-CID.
           MOVE NOME-CID           TO GS-NOME-CID.
       CHAMAR-POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-REGIAO.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-REG.
       LE-REGIAO SECTION.
           MOVE GS-REGIAO  TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE "****" TO NOME-REG.
           MOVE NOME-REG           TO GS-NOME-REG.
       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
           MOVE ZEROS TO TOT-GER-FILME TOT-GER-FORMANDO
                 TOT-GER-VLR-DESPESA TOT-GER-EQUIPE
                 TOT-GER-PARTICIPANTE TOT-GER-VLR-REPORT TOT-GER-FITA.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           IF GS-TIPO-DATA = 1
              PERFORM POR-INTERVALO-DATA
           ELSE
              PERFORM POR-MES-ANO.

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       POR-INTERVALO-DATA SECTION.
           MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-ANT
                                     VECTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-ANT
                                     VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-FIM.

           MOVE VECTO-INI         TO DATA-MOV-R100.
           START RED100 KEY IS NOT < DATA-MOV-R100 INVALID KEY
              MOVE "10" TO ST-RED100.
           PERFORM UNTIL ST-RED100 = "10"
              READ RED100 NEXT RECORD AT END
                   MOVE "10" TO ST-RED100
              NOT AT END
                   IF DATA-MOV-R100 > VECTO-FIM
                      MOVE "10" TO ST-RED100
                   ELSE
                      PERFORM VERIFICA-TOT-FORM-PART-CONT
                   END-IF
              END-READ
           END-PERFORM.

       POR-MES-ANO SECTION.
           INITIALIZE REG-COD040
           MOVE GS-MES-INI     TO MESANO-W
           MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
           MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I       TO MESANO-INI
           MOVE GS-MES-FIM     TO MESANO-W
           MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
           MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I       TO MESANO-FIM


           MOVE MESANO-INI     TO MESANO-PREV-CO40
           MOVE ZEROS          TO NR-CONTRATO-CO40
           START COD040 KEY IS NOT < ALT1-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
                 READ COD040 NEXT RECORD AT END
                      MOVE "10" TO ST-COD040
                 NOT AT END
                      IF MESANO-PREV-CO40 > MESANO-FIM
                         MOVE "10" TO ST-COD040
                      ELSE
                         MOVE NR-CONTRATO-CO40 TO GS-EXIBE-MOVTO
                         MOVE "TELA-AGUARDA1"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM

                         PERFORM PESQUISAR-STATUS
                         IF ACHEI = "S"
                            MOVE ZEROS TO TOT-PARTICIPANTE
                                          TOT-FORMANDO

                            INITIALIZE REG-COD041
                            MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO41
                            START COD041 KEY IS NOT LESS CHAVE-CO41
                                                             INVALID KEY
                                  MOVE "10" TO ST-COD041
                            END-START
                            PERFORM UNTIL ST-COD041 = "10"
                                  READ COD041 NEXT AT END
                                       MOVE "10" TO ST-COD041
                                  NOT AT END
                                       IF NR-CONTRATO-CO40 <>
                                          NR-CONTRATO-CO41
                                          MOVE "10" TO ST-COD041
                                       ELSE
                                          ADD NR-PREV-FORM-CO41 TO
                                              TOT-FORMANDO
                                       END-IF
                                  END-READ
                            END-PERFORM

                            OPEN OUTPUT EVENTO PARTIC
                            CLOSE       EVENTO PARTIC
                            OPEN I-O    EVENTO PARTIC

                            INITIALIZE REG-RED101
                                       TOT-PARTICIPANTE
                                       TOT-EVENTO

                            MOVE NR-CONTRATO-CO40 TO CONTRATO-R101
                            START RED101 KEY IS NOT < CONTRATO-R101
                                                             INVALID KEY
                                 MOVE "10" TO ST-RED101
                            END-START
                            PERFORM UNTIL ST-RED101 = "10"
                                 READ RED101 NEXT AT END
                                      MOVE "10" TO ST-RED101
                                 NOT AT END
                                      IF NR-CONTRATO-CO40 <>
                                         CONTRATO-R101
                                         MOVE "10" TO ST-RED101
                                      ELSE
                                         MOVE DOCTO-R101    TO
                                              PARTIC-DOCTO
                                         READ PARTIC INVALID KEY
                                              MOVE ZEROS TO PARTIC-QTDE
                                              MOVE 1 TO PARTIC-EVENTO
                                              WRITE REG-PARTIC
                                              END-WRITE
                                         END-READ
                                      END-IF
                                 END-READ
                            END-PERFORM

                            INITIALIZE REG-PARTIC
                                       AUX-EVENTO
                            START PARTIC KEY IS NOT LESS PARTIC-DOCTO
                                                             INVALID KEY
                               MOVE "10" TO ST-PARTIC
                            END-START
                            PERFORM UNTIL ST-PARTIC = "10"
                               READ PARTIC NEXT AT END
                                    MOVE "10" TO ST-PARTIC
                               NOT AT END
                                    INITIALIZE REG-RED101
                                               TOT-EVENTO
                                    MOVE PARTIC-DOCTO TO DOCTO-R101
                                    START RED101 KEY IS NOT LESS
                                                   ALT1-R101 INVALID KEY
                                         MOVE "10" TO ST-RED101
                                    END-START
                                    PERFORM UNTIL ST-RED101 = "10"
                                         READ RED101 NEXT AT END
                                              MOVE "10" TO ST-RED101
                                         NOT AT END
                                              IF PARTIC-DOCTO <>
                                                 DOCTO-R101
                                                 MOVE "10" TO ST-RED101
                                              ELSE
                                                 IF AUX-EVENTO <>
                                                    EVENTO-R101
                                                    ADD 1 TO TOT-EVENTO
                                                    MOVE TOT-EVENTO TO
                                                    PARTIC-EVENTO
                                                    MOVE EVENTO-R101 TO
                                                         AUX-EVENTO
                                                    REWRITE REG-PARTIC
                                                 END-IF
                                                 MOVE DOCTO-R101    TO
                                                      PARTIC-DOCTO
                                                 READ PARTIC INVALID KEY
                                                      MOVE ZEROS
                                                        TO PARTIC-QTDE
                                                      ADD QT-PARTIC-R101
                                                        TO PARTIC-QTDE
                                                      WRITE REG-PARTIC
                                                      END-WRITE
                                                 NOT INVALID KEY
                                                      ADD QT-PARTIC-R101
                                                       TO PARTIC-QTDE
                                                      REWRITE REG-PARTIC
                                                      END-REWRITE
                                                 END-READ
                                              END-IF
                                         END-READ
                                    END-PERFORM
                               END-READ
                            END-PERFORM

                            MOVE 1 TO TOT-CONTRATO


                            INITIALIZE REG-RED101
                                       TOT-EVENTO
                            MOVE NR-CONTRATO-CO40 TO CONTRATO-R101
                            START RED101 KEY IS NOT < CONTRATO-R101
                                                             INVALID KEY
                                 MOVE "10" TO ST-RED101
                            END-START
                            PERFORM UNTIL ST-RED101 = "10"
                                 READ RED101 NEXT AT END
                                      MOVE "10" TO ST-RED101
                                 NOT AT END
                                      IF NR-CONTRATO-CO40 <>
                                         CONTRATO-R101
                                         MOVE "10" TO ST-RED101
                                      ELSE
                                         MOVE CIDADE-CO40 TO CIDADE
                                         READ CAD010 INVALID KEY
                                              MOVE SPACE  TO NOME-CID
                                         END-READ
                                         MOVE REGIAO-CID  TO CODIGO-REG
                                         READ CAD012 INVALID KEY
                                              MOVE SPACES TO NOME-REG
                                         END-READ
                                         MOVE NOME-CID TO CIDADE-W
                                         MOVE NOME-REG TO REGIAO-W

                                         MOVE DOCTO-R101 TO DOCTO-R100
                                         READ RED100 INVALID KEY
                                              INITIALIZE REG-RED100
                                         END-READ

                                         MOVE DOCTO-R101    TO
                                              PARTIC-DOCTO
                                         READ PARTIC INVALID KEY
                                              MOVE 1 TO TOT-PARTICIPANTE
                                         NOT INVALID KEY
                                              MOVE PARTIC-QTDE TO
                                               TOT-PARTICIPANTE
                                         END-READ


                                         EVALUATE GS-TIPO-REL
                                           WHEN 1
                                            MOVE DATA-MOV-R100 TO DATA-W
                                            MOVE DATA-W(1:6) TO ORDEM-WK
                                           WHEN 2
                                            MOVE CIDADE-W    TO ORDEM-WK
                                           WHEN 3
                                            MOVE REGIAO-W    TO ORDEM-WK
                                           WHEN 4
                                            MOVE UF-CID      TO ORDEM-WK
                                           WHEN 5
                                            MOVE CONTRATO-R101
                                                             TO ORDEM-WK
                                         END-EVALUATE

                                         MOVE ORDEM-WK      TO EVE-ORDEM
                                         MOVE CONTRATO-R101 TO EVE-CONTR
                                         MOVE EVENTO-R101  TO EVE-CODIGO
                                         READ EVENTO INVALID KEY
                                              MOVE 1       TO TOT-EVENTO
                                              WRITE REG-EVENTO
                                              END-WRITE
                                         END-READ

                                         COMPUTE QT-EQUIPE ROUNDED =
                                          (QTDE-PESSOAS-R100 *
                                           QT-PARTIC-R101) /
                                           TOT-PARTICIPANTE

                                         COMPUTE QT-FILME ROUNDED =
                                          (TOT-FILME-REPORT-R100 *
                                           QT-PARTIC-R101) /
                                           TOT-PARTICIPANTE

                                         COMPUTE QT-FITA ROUNDED =
                                          (TOT-FITA-REPORT-R100 *
                                           QT-PARTIC-R101) /
                                          TOT-PARTICIPANTE

                                         COMPUTE QT-EQUIPE ROUNDED =
                                          (QTDE-PESSOAS-R100 *
                                           QT-PARTIC-R101) /
                                           TOT-PARTICIPANTE

                                         COMPUTE VLR-REPORT ROUNDED =
                                                (VLR-TOT-REPORT-R100
                                                * QT-PARTIC-R101) /
                                                  TOT-PARTICIPANTE

                                         COMPUTE DESPESA-W =
                                           VLR-COMB-R100      +
                                           VLR-HOSP-R100      +
                                           VLR-REFEICAO-R100  +
                                           VLR-PASSAGEM-R100  +
                                           VLR-MAT-R100       +
                                           VLR-DESPESA-REPORT-R100 +
                                           VLR-ALUGUEL-R100   +
                                           VLR-OUTROS-R100

                                         COMPUTE VLR-DESPESA ROUNDED =
                                            (DESPESA-W *
                                             QT-PARTIC-R101) /
                                             TOT-PARTICIPANTE

      *                                  IF NR-CONTRATO-CO40 = 3519
      *                                     DISPLAY "DATA-R100 = "
      *                                          AT 0101
      *                                     DISPLAY  DATA-MOV-R100
      *                                          AT 0120
      *
      *                                     DISPLAY "QT-EQUIPE = "
      *                                          AT 0201
      *                                     DISPLAY QT-EQUIPE
      *                                          AT 0220
      *
      *                                     DISPLAY "QT-FILME = "
      *                                          AT 0301
      *                                     DISPLAY QT-FILME
      *                                          AT 0320
      *
      *                                     DISPLAY "QT-FITA = "
      *                                          AT 0401
      *                                     DISPLAY QT-FITA
      *                                          AT 0420
      *
      *                                     DISPLAY "QT-EQUIPE = "
      *                                          AT 0501
      *                                     DISPLAY QT-EQUIPE
      *                                          AT 0520
      *
      *                                     DISPLAY "VLR-REPORT = "
      *                                          AT 0601
      *                                     DISPLAY VLR-REPORT
      *                                          AT 0620
      *
      *                                     DISPLAY "VLR-DESPESA = "
      *                                          AT 0701
      *                                     DISPLAY VLR-DESPESA
      *                                          AT 0720
      *
      *                                     DISPLAY "PARTIC-EVENTO = "
      *                                          AT 0801
      *                                     DISPLAY PARTIC-EVENTO
      *                                          AT 0820
      *
      *                                     DISPLAY "TOT-PARTICIPANTE ="
      *                                          AT 0901
      *                                     DISPLAY TOT-PARTICIPANTE
      *                                          AT 0920
      *
      *                                     MOVE EVENTO-R101 TO
      *                                          CODIGO-CO03
      *                                     READ COD003 INVALID KEY
      *                                          MOVE "****" TO
      *                                          NOME-CO03
      *                                     END-READ
      *
      *                                     DISPLAY "EVENTO-R101 = "
      *                                          AT 1001
      *                                     DISPLAY NOME-CO03
      *                                          AT 1020
      *
      *                                     DISPLAY "VLR-REPORT-R100 = "
      *                                          AT 1101
      *                                     DISPLAY VLR-TOT-REPORT-R100
      *                                          AT 1120
      *
      *                                     DISPLAY "DESPESA-W = "
      *                                          AT 1201
      *                                     DISPLAY DESPESA-W
      *                                          AT 1220
      *
      *                                     DISPLAY "QT-PARTIC-R101 = "
      *                                          AT 1301
      *                                     DISPLAY QT-PARTIC-R101
      *                                          AT 1320
      *
      *                                     STOP " "
      *
      *                                  END-IF

                                         READ WORK INVALID KEY
                                            MOVE TOT-FORMANDO
                                              TO FORMANDO-WK
                                            MOVE TOT-EVENTO
                                              TO QT-EVENTO-WK
                                            MOVE QT-PARTIC-R101
                                              TO PARTICIPANTE-WK
                                            MOVE TOT-CONTRATO
                                              TO QT-CONTRATO-WK
                                            MOVE 1
                                              TO QT-RELAT-WK

                                            MOVE QT-EQUIPE TO
                                                 QT-EQUIPE-WK

                                            MOVE QT-FILME TO
                                                 QT-FILME-WK

                                            MOVE QT-FITA TO
                                                 QT-FITA-WK

                                            MOVE VLR-REPORT TO
                                                 VLR-REPORT-WK

                                            MOVE VLR-DESPESA TO
                                                 VLR-DESPESA-WK

                                            WRITE REG-WORK
                                            END-WRITE
                                          NOT INVALID KEY
                                            ADD TOT-EVENTO
                                             TO QT-EVENTO-WK
                                            ADD QT-PARTIC-R101
                                             TO PARTICIPANTE-WK
                                            ADD TOT-CONTRATO
                                             TO QT-CONTRATO-WK
                                            ADD 1
                                             TO QT-RELAT-WK

                                            ADD  QT-EQUIPE TO
                                                 QT-EQUIPE-WK

                                            ADD  QT-FILME TO
                                                 QT-FILME-WK

                                            ADD  QT-FITA TO
                                                 QT-FITA-WK

                                            ADD  VLR-REPORT TO
                                                 VLR-REPORT-WK

                                            ADD  VLR-DESPESA TO
                                                 VLR-DESPESA-WK

      *                                     IF NR-CONTRATO-CO40 = 3519
      *                                        DISPLAY "QT-FILME-WK = "
      *                                             AT 2001
      *                                        DISPLAY QT-FILME-WK
      *                                             AT 2020
      *                                        STOP " "
      *                                     END-IF


                                            REWRITE REG-WORK
                                            END-REWRITE
                                         END-READ

                                         MOVE 0 TO TOT-CONTRATO
                                      END-IF
                                 END-READ
                            END-PERFORM
                            CLOSE       EVENTO PARTIC
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

       VERIFICA-TOT-FORM-PART-CONT SECTION.
           PERFORM VERIFICA-QTDE-EVENTO.

           MOVE ZEROS               TO TOT-PARTICIPANTE
                                       TOT-CONTRATO
                                       CONTRATO-ANT
                                       TOT-FORMANDO

           MOVE SPACES              TO CIDADE-W
                                       REGIAO-W

           MOVE DOCTO-R100          TO DOCTO-R101

           MOVE ZEROS               TO CONTRATO-R101
                                       EVENTO-R101

           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
               MOVE "10" TO ST-RED101.

           PERFORM UNTIL ST-RED101 = "10"
               READ RED101 NEXT RECORD AT END
                    MOVE "10" TO ST-RED101
               NOT AT END
                    IF DOCTO-R101 <> DOCTO-R100
                       MOVE "10" TO ST-RED101
                    ELSE
                       IF CONTRATO-R101 <> CONTRATO-ANT
                          MOVE CONTRATO-R101    TO CONTRATO-ANT
                          MOVE CONTRATO-R101    TO NR-CONTRATO-CO40
                          READ COD040 INVALID KEY
                               MOVE ZEROS       TO QTDE-FORM-CO40
                          END-READ
                          PERFORM PESQUISAR-STATUS
                          IF ACHEI = "S"
                             ADD QT-PARTIC-R101    TO TOT-PARTICIPANTE
                             ADD 1                 TO TOT-CONTRATO
                             ADD QTDE-FORM-CO40    TO TOT-FORMANDO
                          END-IF
                       END-IF
                    END-IF
               END-READ
           END-PERFORM

           MOVE ZEROS               TO CONTRATO-ANT
           MOVE DOCTO-R100          TO DOCTO-R101
           MOVE ZEROS               TO CONTRATO-R101
                                       EVENTO-R101

           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
               MOVE "10" TO ST-RED101.

           PERFORM UNTIL ST-RED101 = "10"
               READ RED101 NEXT RECORD AT END
                    MOVE "10" TO ST-RED101
               NOT AT END
                    IF DOCTO-R101 <> DOCTO-R100
                       MOVE "10" TO ST-RED101
                    ELSE
                       IF CONTRATO-R101 <> CONTRATO-ANT
                          MOVE CONTRATO-R101    TO CONTRATO-ANT
                          MOVE CONTRATO-R101    TO NR-CONTRATO-CO40
                          READ COD040 INVALID KEY
                               MOVE ZEROS       TO QTDE-FORM-CO40
                          END-READ
                          PERFORM PESQUISAR-STATUS
                          IF ACHEI = "S"
                             MOVE CIDADE-CO40      TO CIDADE
                             READ CAD010 INVALID KEY
                                  MOVE SPACE       TO NOME-CID
                             END-READ
                             MOVE REGIAO-CID       TO CODIGO-REG
                             READ CAD012 INVALID KEY
                                  MOVE SPACES      TO NOME-REG
                             END-READ
                             MOVE NOME-CID         TO CIDADE-W
                             MOVE NOME-REG         TO REGIAO-W

                             EVALUATE GS-TIPO-REL
                               WHEN 1 MOVE DATA-MOV-R100 TO DATA-W
                                      MOVE DATA-W(1: 6)  TO ORDEM-WK
                               WHEN 2 MOVE CIDADE-W      TO ORDEM-WK
                               WHEN 3 MOVE REGIAO-W      TO ORDEM-WK
                               WHEN 4 MOVE UF-CID        TO ORDEM-WK
                               WHEN 5 MOVE CONTRATO-R101 TO ORDEM-WK
                             END-EVALUATE

                             READ WORK INVALID KEY
                                MOVE TOT-FORMANDO     TO FORMANDO-WK
                                MOVE TOT-EVENTO       TO QT-EVENTO-WK
                                MOVE TOT-PARTICIPANTE TO PARTICIPANTE-WK
                                MOVE TOT-CONTRATO      TO QT-CONTRATO-WK
                                MOVE 1                 TO QT-RELAT-WK
      *                         MOVE QTDE-PESSOAS-R100 TO QT-EQUIPE-WK

                                COMPUTE QT-EQUIPE-WK ROUNDED =
                                          (QTDE-PESSOAS-R100 *
                                   QT-PARTIC-R101) / TOT-PARTICIPANTE

                                COMPUTE QT-FILME-WK ROUNDED =
                                     (TOT-FILME-REPORT-R100
                                    * QT-PARTIC-R101) / TOT-PARTICIPANTE
                                COMPUTE QT-FITA-WK ROUNDED =
                                     (TOT-FITA-REPORT-R100
                                    * QT-PARTIC-R101) / TOT-PARTICIPANTE
                                COMPUTE QT-EQUIPE-WK ROUNDED =
                                     (QTDE-PESSOAS-R100
                                    * QT-PARTIC-R101) / TOT-PARTICIPANTE
                                COMPUTE VLR-REPORT-WK ROUNDED =
                                     (VLR-TOT-REPORT-R100
                                    * QT-PARTIC-R101) / TOT-PARTICIPANTE
                                COMPUTE DESPESA-W =
                                         VLR-COMB-R100           +
                                         VLR-HOSP-R100           +
                                         VLR-REFEICAO-R100       +
                                         VLR-PASSAGEM-R100       +
                                         VLR-MAT-R100            +
                                         VLR-DESPESA-REPORT-R100 +
                                         VLR-ALUGUEL-R100        +
                                         VLR-OUTROS-R100

                                COMPUTE VLR-DESPESA-WK ROUNDED =
                                       (DESPESA-W * QT-PARTIC-R101) /
                                            TOT-PARTICIPANTE

      *                         MOVE TOT-FILME-REPORT-R100
      *                                                TO QT-FILME-WK
      *                         MOVE TOT-FITA-REPORT-R100 TO QT-FITA-WK
      *                         MOVE VLR-TOT-REPORT-R100
      *                                                TO VLR-REPORT-WK
      *                         MOVE VLR-DESPESA-REPORT-R100
      *                                                TO VLR-DESPESA-WK
                                WRITE REG-WORK
                                END-WRITE
                              NOT INVALID KEY
                                ADD  TOT-FORMANDO     TO FORMANDO-WK
                                ADD  TOT-EVENTO       TO QT-EVENTO-WK
                                ADD  TOT-PARTICIPANTE TO PARTICIPANTE-WK
                                ADD  TOT-CONTRATO     TO QT-CONTRATO-WK
                                ADD  1                TO QT-RELAT-WK
      *                         ADD  QTDE-PESSOAS-R100 TO QT-EQUIPE-WK

                                COMPUTE QT-EQUIPE-WK ROUNDED =
                                           QT-EQUIPE-WK +
                                         ((QTDE-PESSOAS-R100 *
                                   QT-PARTIC-R101) / TOT-PARTICIPANTE)

                                COMPUTE QT-FILME-WK ROUNDED =
                                     QT-FILME-WK +
                                   ((TOT-FILME-REPORT-R100
                                   * QT-PARTIC-R101) / TOT-PARTICIPANTE)
                                COMPUTE QT-FITA-WK ROUNDED = QT-FITA-WK
                                  + ((TOT-FITA-REPORT-R100
                                   * QT-PARTIC-R101) / TOT-PARTICIPANTE)
                                COMPUTE VLR-REPORT-WK ROUNDED =
                                        VLR-REPORT-WK +
                                    ((VLR-TOT-REPORT-R100
                                   * QT-PARTIC-R101) / TOT-PARTICIPANTE)
                                COMPUTE DESPESA-W =
                                         VLR-COMB-R100           +
                                         VLR-HOSP-R100           +
                                         VLR-REFEICAO-R100       +
                                         VLR-PASSAGEM-R100       +
                                         VLR-MAT-R100            +
                                         VLR-DESPESA-REPORT-R100 +
                                         VLR-ALUGUEL-R100        +
                                         VLR-OUTROS-R100

                                COMPUTE VLR-DESPESA-WK ROUNDED =
                                        VLR-DESPESA-WK +
                                       ((DESPESA-W * QT-PARTIC-R101) /
                                            TOT-PARTICIPANTE)

      *                         ADD  TOT-FILME-REPORT-R100
      *                                               TO QT-FILME-WK
      *                         ADD  TOT-FITA-REPORT-R100  TO QT-FITA-WK
      *                         ADD  VLR-TOT-REPORT-R100
      *                                               TO VLR-REPORT-WK
      *                         ADD  VLR-DESPESA-REPORT-R100
      *                                               TO VLR-DESPESA-WK
                                REWRITE REG-WORK
                                END-REWRITE
                             END-READ
                          END-IF
                       END-IF
                    END-IF
               END-READ
           END-PERFORM.

       PESQUISAR-STATUS SECTION.
           MOVE "N" TO ACHEI

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES OR ACHEI = "S"
               IF GS-LINHA-STATUS(1:2) = STATUS-CO40
                  MOVE "S" TO ACHEI
               END-IF
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       VERIFICA-QTDE-EVENTO SECTION.
           MOVE ZEROS               TO TOT-EVENTO EVENTO-ANT
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE ZEROS               TO EVENTO-R101.
           START RED101 KEY IS NOT < ALT1-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END
                  MOVE "10" TO ST-RED101
             NOT AT END
                  IF DOCTO-R101 <> DOCTO-R100
                     MOVE "10" TO ST-RED101
                  ELSE
                     MOVE CONTRATO-R101    TO NR-CONTRATO-CO40
                     READ COD040 INVALID KEY
                          INITIALIZE REG-COD040
                     END-READ
                     PERFORM PESQUISAR-STATUS
                     IF ACHEI = "S"
                        IF EVENTO-R101 <> EVENTO-ANT
                           MOVE EVENTO-R101      TO EVENTO-ANT
                           ADD 1                 TO TOT-EVENTO
                        END-IF
                     END-IF
                  END-IF
             END-READ
           END-PERFORM.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO TOT-GER-FORMANDO  TOT-GER-EVENTO
                      TOT-GER-PARTICIPANTE TOT-GER-CONTRATO
                      TOT-GER-RELATORIO TOT-GER-EQUIPE
                      TOT-GER-FILME TOT-GER-FITA
                      TOT-GER-VLR-REPORT TOT-GER-VLR-DESPESA.

           MOVE SPACES TO ORDEM-WK.
           START WORK KEY IS NOT < ORDEM-WK INVALID KEY
              MOVE "10" TO ST-WORK.

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA.

       MOVER-DADOS-LINDET SECTION.
           MOVE SPACES            TO GS-LINDET.
           IF GS-TIPO-REL = 1
              MOVE ORDEM-WK(1: 4) TO GS-LINDET(4: 4)
              MOVE "/"            TO GS-LINDET(3: 1)
              MOVE ORDEM-WK(5: 2) TO GS-LINDET(1: 2)
           ELSE
              MOVE ORDEM-WK       TO GS-LINDET(1: 11)
           END-IF
           MOVE QT-RELAT-WK        TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(12: 7)
           MOVE QT-CONTRATO-WK     TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(19: 8)
           MOVE FORMANDO-WK        TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(27: 8)
           MOVE QT-EVENTO-WK          TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(35: 7)
           MOVE PARTICIPANTE-WK    TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(42: 8)
           MOVE QT-EQUIPE-WK       TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(50: 8)
           MOVE QT-FITA-WK         TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(58: 9)
           MOVE QT-FILME-WK        TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(67: 8)
           MOVE VLR-REPORT-WK      TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(74: 14)
           MOVE VLR-DESPESA-WK     TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(87: 11)
           ADD VLR-REPORT-WK VLR-DESPESA-WK GIVING VALOR-E
           MOVE VALOR-E            TO GS-LINDET(98: 13).
           ADD FORMANDO-WK         TO TOT-GER-FORMANDO
           ADD QT-EVENTO-WK           TO TOT-GER-EVENTO
           ADD PARTICIPANTE-WK     TO TOT-GER-PARTICIPANTE
           ADD QT-CONTRATO-WK      TO TOT-GER-CONTRATO
           ADD QT-RELAT-WK         TO TOT-GER-RELATORIO
           ADD QT-EQUIPE-WK        TO TOT-GER-EQUIPE
           ADD QT-FILME-WK         TO TOT-GER-FILME
           ADD QT-FITA-WK          TO TOT-GER-FITA
           ADD VLR-REPORT-WK       TO TOT-GER-VLR-REPORT
           ADD VLR-DESPESA-WK      TO TOT-GER-VLR-DESPESA.

       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE TOT-GER-RELATORIO    TO QTDE-E1
           MOVE QTDE-E1              TO GS-LINTOT(2: 8)
           MOVE TOT-GER-CONTRATO     TO QTDE-E1
           MOVE QTDE-E1              TO GS-LINTOT(10: 8)
           MOVE TOT-GER-FORMANDO     TO QTDE-E1
           MOVE QTDE-E1              TO GS-LINTOT(18: 8)
           MOVE TOT-GER-EVENTO       TO QTDE-E1
           MOVE QTDE-E1              TO GS-LINTOT(25: 8)
           MOVE TOT-GER-PARTICIPANTE TO QTDE-E1
           MOVE QTDE-E1              TO GS-LINTOT(34: 12)
           MOVE TOT-GER-EQUIPE       TO QTDE-E1
           MOVE QTDE-E1              TO GS-LINTOT(45: 08)
           MOVE TOT-GER-FITA         TO QTDE-E1
           MOVE QTDE-E1              TO GS-LINTOT(54: 8)
           MOVE TOT-GER-FILME        TO QTDE-E1
           MOVE QTDE-E1              TO GS-LINTOT(64: 8)
           MOVE TOT-GER-VLR-REPORT   TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(72: 14)
           MOVE TOT-GER-VLR-DESPESA  TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(86: 14)

           COMPUTE VALOR-E ROUNDED = TOT-GER-VLR-REPORT +
                                     TOT-GER-VLR-DESPESA
           MOVE VALOR-E              TO GS-LINTOT(100: 13).

           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP206" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO ORDEM-WK.
           START WORK KEY IS NOT < ORDEM-WK INVALID KEY
               MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           copy descondensa.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           IF GS-TIPO-REL = 1
              MOVE ORDEM-WK(1: 4) TO LINDET-REL(4: 4)
              MOVE "/"            TO LINDET-REL(3: 1)
              MOVE ORDEM-WK(5: 2) TO LINDET-REL(1: 2)
           ELSE
              MOVE ORDEM-WK       TO LINDET-REL(1: 11)
           END-IF
           MOVE QT-RELAT-WK        TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(12: 7)
           MOVE QT-CONTRATO-WK     TO QTDE-E1
           MOVE QTDE-E1            TO LINDET-REL(19: 8)
           MOVE FORMANDO-WK        TO QTDE-E1
           MOVE QTDE-E1            TO LINDET-REL(27: 8)
           MOVE QT-EVENTO-WK       TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(35: 7)
           MOVE PARTICIPANTE-WK    TO QTDE-E1
           MOVE QTDE-E1            TO LINDET-REL(42: 8)
           MOVE QT-EQUIPE-WK       TO QTDE-E1
           MOVE QTDE-E1            TO LINDET-REL(50: 8)
           MOVE QT-FITA-WK         TO QTDE-E1
           MOVE QTDE-E1            TO LINDET-REL(58: 8)
           MOVE QT-FILME-WK        TO QTDE-E1
           MOVE QTDE-E1            TO LINDET-REL(66: 8)
           MOVE VLR-REPORT-WK      TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(74: 14)
           MOVE VLR-DESPESA-WK     TO VALOR-E1
           MOVE VALOR-E1           TO LINDET-REL(88: 11)
           ADD VLR-REPORT-WK VLR-DESPESA-WK GIVING VALOR-E
           MOVE VALOR-E            TO LINDET-REL(99: 13).

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.


       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE TOT-GER-RELATORIO    TO QTDE-E1
           MOVE QTDE-E1              TO LINTOT-REL(1: 8)
           MOVE TOT-GER-CONTRATO     TO QTDE-E1
           MOVE QTDE-E1              TO LINTOT-REL(09: 8)
           MOVE TOT-GER-FORMANDO     TO QTDE-E1
           MOVE QTDE-E1              TO LINTOT-REL(17: 8)
           MOVE TOT-GER-EVENTO       TO QTDE-E
           MOVE QTDE-E               TO LINTOT-REL(25: 7)
           MOVE TOT-GER-PARTICIPANTE TO QTDE-E1
           MOVE QTDE-E1              TO LINTOT-REL(32: 09)
           MOVE TOT-GER-EQUIPE       TO QTDE-E1
           MOVE QTDE-E1              TO LINTOT-REL(41: 11)
           MOVE TOT-GER-FITA         TO QTDE-E1
           MOVE QTDE-E1              TO LINTOT-REL(52: 9)
           MOVE TOT-GER-FILME        TO QTDE-E1
           MOVE QTDE-E1              TO LINTOT-REL(61: 10)
           MOVE TOT-GER-VLR-REPORT   TO VALOR-E
           MOVE VALOR-E              TO LINTOT-REL(71: 14)
           MOVE TOT-GER-VLR-DESPESA  TO VALOR-E
           MOVE VALOR-E              TO LINTOT-REL(85: 14)
           ADD TOT-GER-VLR-REPORT TO TOT-GER-VLR-DESPESA GIVING VALOR-E
           MOVE VALOR-E              TO LINTOT-REL(99: 13).

           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.

           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
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
           CLOSE CAD010 CAD012 COD003 COD040 RED100 RED101 WORK
                 COD001 RED206 COD041
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
