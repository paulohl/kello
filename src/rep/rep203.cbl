       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP203.
      *DATA: 02/05/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RESUMO DE EQUIPE DE REPORTAGEM POR FUNCAO
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
           COPY CGPX001.
           COPY CGPX005.
           COPY REPX002.
           COPY REPX100.
           COPY REPX103.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = FUNCIONARIO-WK FUNCAO-WK
                  ALTERNATE RECORD KEY IS ALT-WK =
                     NOME-FUNC-WK FUNCAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS VLR-REPORT-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FUNCAO-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CGPW005.
       COPY REPW002.
       COPY REPW100.
       COPY REPW103.

       FD  WORK.
       01  REG-WORK.
           05  FUNCIONARIO-WK      PIC 9(6).
           05  NOME-FUNC-WK        PIC X(30).
           05  PADRAO-WK           PIC X.
           05  FUNCAO-WK           PIC X(8).
           05  QT-FILM-FIT-WK      PIC 9(4).
           05  QT-REPORT-WK        PIC 99V99.
           05  VLR-REPORT-WK       PIC 9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "REP203.CPB".
           COPY "REP203.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD005             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED103             PIC XX       VALUE SPACES.
           05  ST-RED002             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
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
           05  VALOR-E               PIC ZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  VALOR-ACUM            PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-ACUM            PIC 9(8)V99  VALUE ZEROS.
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZ.ZZZ.ZZZ.
           05  QTDE-E1               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  TOT-FIT-FILME         PIC 9(6)     VALUE ZEROS.
           05  TOT-REPORT            PIC 9(6)V99  VALUE ZEROS.
           05  TOT-VLR-REPORT        PIC 9(8)V99  VALUE ZEROS.
           05  TOT-FUNCIONARIO       PIC 9(4)     VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
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
           "ESTATISTICA REPORTAGEM-FUNCAO ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB02A.
           05  FILLER              PIC X(08)   VALUE "FUNCAO: ".
           05  FUNCAO-REL          PIC ZZ      BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-FUNCAO-REL     PIC X(20)   VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "CODIGO FUNCIONARIO                    P FUNCAO     PRODUCAO
      -    "  QT.REPORTAGEM   VLR-REPORTAGEM ".


       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(100)  VALUE
           "QTDE-FUNCION.       PRODUCAO    QTDE-REPORT   VLR-TOTAL-REPO
      -    "RT".
       01  LINTOT.
           05  LINTOT-REL          PIC X(100)  VALUE SPACES.

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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD005.
           MOVE "RED002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED002.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED103.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CGD001 CGD005 RED002 RED100 RED103.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD005 <> "00"
              MOVE "ERRO ABERTURA CGD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED002 <> "00"
              MOVE "ERRO ABERTURA RED002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED103 <> "00"
              MOVE "ERRO ABERTURA RED103: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED103 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-POPUP-FUNCAO-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-FUNCAO-TRUE
                   PERFORM LE-FUNCAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CHAMAR-POPUP SECTION.
           CALL   "REP002T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "REP002T".
           MOVE PASSAR-STRING-1(22: 2) TO GS-FUNCAO.
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESCR-FUNCAO.
       LE-FUNCAO SECTION.
           MOVE GS-FUNCAO      TO CODIGO-RE02.
           READ RED002 INVALID KEY MOVE "****" TO DESCRICAO-RE02.
           MOVE DESCRICAO-RE02     TO GS-DESCR-FUNCAO.
       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
           MOVE ZEROS TO TOT-FIT-FILME TOT-REPORT TOT-FUNCIONARIO
                         TOT-VLR-REPORT.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
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
           MOVE ZEROS TO ULT-SEQ.
           PERFORM UNTIL ST-RED100 = "10"
             READ RED100 NEXT RECORD AT END MOVE "10" TO ST-RED100
              NOT AT END
               IF DATA-MOV-R100 > VECTO-FIM MOVE "10" TO ST-RED100
               ELSE
                PERFORM VERIFICA-CODIGO
               END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       VERIFICA-CODIGO SECTION.
           MOVE DOCTO-R100          TO DOCTO-R103
           MOVE ZEROS               TO CODIGO-R103
           MOVE ZEROS               TO FUNCAO-R103
           INITIALIZE REG-WORK.
           START RED103 KEY IS NOT < CHAVE-R103 INVALID KEY
                MOVE "10" TO ST-RED103
           END-START
           PERFORM UNTIL ST-RED103 = "10"
             READ RED103 NEXT RECORD AT END MOVE "10" TO ST-RED103
              NOT AT END
               IF DOCTO-R103 <> DOCTO-R100
                    MOVE "10" TO ST-RED103
               ELSE
                IF FUNCAO-R103 <> GS-FUNCAO CONTINUE
                ELSE
                 MOVE CODIGO-R103         TO FUNCIONARIO-WK
                 MOVE FUNCAO-R103         TO CODIGO-RE02
                 READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02
                 END-READ
                 READ WORK INVALID KEY
                      PERFORM MOVER-DADOS-WORK
                      MOVE QT-FILMES-R103        TO QT-FILM-FIT-WK
                      MOVE QT-REPORT-R103        TO QT-REPORT-WK
                      MOVE VLR-REPORT-R103       TO VLR-REPORT-WK
                      ADD 1                      TO TOT-FUNCIONARIO
                      WRITE REG-WORK
                      END-WRITE
                   NOT INVALID KEY
                     PERFORM MOVER-DADOS-WORK
                     ADD QT-FILMES-R103          TO QT-FILM-FIT-WK
                     ADD QT-REPORT-R103          TO QT-REPORT-WK
                     ADD VLR-REPORT-R103         TO VLR-REPORT-WK
                     REWRITE REG-WORK
                     END-REWRITE
                 END-READ
                 ADD QT-FILMES-R103       TO TOT-FIT-FILME
                 ADD QT-REPORT-R103       TO TOT-REPORT
                 ADD VLR-REPORT-R103      TO TOT-VLR-REPORT
                END-IF
               END-IF
             END-READ
           END-PERFORM.
       MOVER-DADOS-WORK SECTION.
           MOVE DESCRICAO-RE02           TO FUNCAO-WK
           MOVE CODIGO-R103              TO FUNCIONARIO-WK CODIGO-CG01
                                            CODIGO-CG05
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
           END-READ.
           MOVE NOME-CG01                TO NOME-FUNC-WK
           READ CGD005 INVALID KEY MOVE SPACES TO PADRAO-CG05.
           MOVE PADRAO-CG05              TO PADRAO-WK.


       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
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
           MOVE FUNCIONARIO-WK    TO GS-LINDET(01: 7)
           MOVE NOME-FUNC-WK      TO GS-LINDET(8: 31)
           MOVE PADRAO-WK         TO GS-LINDET(39: 2)
           MOVE FUNCAO-WK         TO GS-LINDET(41: 9)
           MOVE QT-FILM-FIT-WK    TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(48: 13)
           MOVE QT-REPORT-WK      TO QTDE-E1
           MOVE QTDE-E1           TO GS-LINDET(63: 15)
           MOVE VLR-REPORT-WK     TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(82: 16).
       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "FUNCIONARIO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "FUNCAO     " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < FUNCAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "VLR-REPORT." TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < VLR-REPORT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE TOT-FUNCIONARIO   TO GS-LINTOT(01: 14)
           MOVE TOT-FIT-FILME     TO QTDE-E
           MOVE QTDE-E            TO GS-LINTOT(18: 13)
           MOVE TOT-REPORT        TO QTDE-E1
           MOVE QTDE-E1           TO GS-LINTOT(34: 13)
           MOVE TOT-VLR-REPORT    TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(55: 13)
           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP203" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
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
           MOVE FUNCIONARIO-WK    TO LINDET-REL(01: 7)
           MOVE NOME-FUNC-WK      TO LINDET-REL(8: 31)
           MOVE PADRAO-WK         TO LINDET-REL(39: 2)
           MOVE FUNCAO-WK         TO LINDET-REL(41: 9)
           MOVE QT-FILM-FIT-WK    TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(48: 13)
           MOVE QT-REPORT-WK      TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(63: 15)
           MOVE VLR-REPORT-WK     TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(82: 16)

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.


       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE TOT-FUNCIONARIO   TO QTDE-E
           MOVE QTDE-E            TO LINTOT-REL(01: 14)
           MOVE TOT-FIT-FILME     TO QTDE-E
           MOVE QTDE-E            TO LINTOT-REL(18: 13)
           MOVE TOT-REPORT        TO QTDE-E1
           MOVE QTDE-E1           TO LINTOT-REL(31: 13)
           MOVE TOT-VLR-REPORT    TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(52: 13)

           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           MOVE GS-FUNCAO           TO FUNCAO-REL
           MOVE GS-DESCR-FUNCAO     TO NOME-FUNCAO-REL.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
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
           CLOSE CGD001 CGD005 RED002 RED100 RED103 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
