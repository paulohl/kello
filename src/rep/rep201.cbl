       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP201.
      *DATA: 14/04/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Extrato de REPORTAGEM
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
           COPY CGPX001.
           COPY COPX040.
           COPY IEPX010.
           COPY REPX002.
           COPY REPX100.
           COPY REPX101.
           COPY REPX103.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS DATA-REPORT-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DOCUMENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FUNCAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CONTRATO-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT EXCEL ASSIGN TO ARQUIVO-EXCEL
                        ORGANIZATION IS SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CGPW001.
       COPY COPW040.
       COPY IEPW010.
       COPY REPW002.
       COPY REPW100.
       COPY REPW101.
       COPY REPW103.

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(5).
           05  DOCUMENTO-WK        PIC 9(6).
           05  CONTRATO-WK         PIC 9(4).
           05  INSTITUICAO-WK      PIC X(15).
           05  DATA-REPORT-WK      PIC 9(8).
           05  CURSO-WK            PIC X(12).
           05  CIDADE-WK           PIC X(13).
           05  FUNCAO-WK           PIC X(3).
           05  QT-FILM-FIT-WK      PIC 9(4).
           05  QT-REPORT-WK        PIC 99V99.
           05  VALOR-REPORT-WK     PIC 9(8)V99.
           05  ANOMES-WK           PIC 9(6).
           05  LCTO-CTA-CORR-WK    PIC 9.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       FD EXCEL.
       01 REG-EXCEL.
          05 EXCEL-DATA            PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-DOCTO           PIC X(06).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CONTRATO        PIC X(04).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-INSTITUICAO     PIC X(15).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CIDADE          PIC X(13).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CURSO           PIC X(12).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FUNCAO          PIC X(03).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-PRODUCAO        PIC X(06).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-QT-REPORT       PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-VALOR           PIC X(13).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-MESANO          PIC X(07).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CONTA           PIC X(01).
          05 FILLER                PIC X(02) VALUE X"0DA0".


       WORKING-STORAGE SECTION.
           COPY "REP201.CPB".
           COPY "REP201.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-RED103             PIC XX       VALUE SPACES.
           05  ST-RED002             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
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
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  MESANO-INV            PIC 9(6)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E.
               10  MES-E             PIC 99.
               10  TRACO-W           PIC X        VALUE "/".
               10  ANO-E             PIC 9999.
           05  VALOR-E               PIC ZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  VALOR-ACUM            PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-ACUM            PIC 9(8)V99  VALUE ZEROS.
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZ.ZZZ.
           05  QTDE-E1               PIC Z.ZZZ,ZZ.
           05  TOT-FIT-FILME         PIC 9(6)     VALUE ZEROS.
           05  TOT-REPORT            PIC 9(6)V99  VALUE ZEROS.
           05  TOT-VLR-REPORT        PIC 9(8)V99  VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
           05  MASC-DATA             PIC 99/99/9999.
           05  MASC-MESANO           PIC 99/9999.
           05  MASC-QT-REPORT        PIC Z9,99.
           05  MASC-VALOR            PIC ZZ.ZZZ.ZZ9,99.
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
           "EXTRATO DE REPORTAGEM       ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB06.
           05  FILLER              PIC X(41)   VALUE
           "EXTRATO DE REPORTAGEM       ORDEM: ".
           05  ORDEM1-REL          PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  MESANO-INI-REL      PIC 99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MESANO-FIM-REL      PIC 99/9999.

       01  CAB02A.
           05  FILLER              PIC X(12)   VALUE "FORNECEDOR: ".
           05  FORNECEDOR-REL      PIC ZZZ.ZZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-FORNEC-REL     PIC X(30)   VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-REPOR  DOCTO CONT INSTITUICAO     CIDADE        CURSO
      -    "         FUN  PROD.  QT-REPOR    VLR-REPORT ".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(100)  VALUE
           "QTDE-REPORT         PRODUCAO    QTDE-REPORT   VLR-TOTAL-REPO
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
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "IED010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "RED002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED002.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           MOVE "RED103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED103.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CAD010 CGD001 COD040 IED010 RED002 RED100 RED101
                      RED103.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED002 <> "00"
              MOVE "ERRO ABERTURA RED002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED101 <> "00"
              MOVE "ERRO ABERTURA RED101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-POPUP-FORNEC-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-FORNEC-TRUE
                   PERFORM LE-FORNEC
               WHEN GS-EXCEL-TRUE
                   PERFORM GERAR-EXCEL
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GERAR-EXCEL SECTION.
           MOVE SPACES TO ARQUIVO-EXCEL
           IF GS-TIPO-REL = 1
              STRING "\ARQUIVOS\REP-" GS-FORNECEDOR
              INTO ARQUIVO-EXCEL
           ELSE
              STRING "\ARQUIVOS\REP-" GS-MESANO-INI "-" GS-MESANO-FIM
              INTO ARQUIVO-EXCEL.

           OPEN OUTPUT EXCEL

           MOVE "*  DATA  *"       TO EXCEL-DATA
           MOVE "DOCTO"            TO EXCEL-DOCTO
           MOVE "CONT"             TO EXCEL-CONTRATO
           MOVE "INSTITUICAO"      TO EXCEL-INSTITUICAO
           MOVE "CIDADE"           TO EXCEL-CIDADE
           MOVE "CURSO"            TO EXCEL-CURSO
           MOVE "FUN"              TO EXCEL-FUNCAO
           MOVE "PROD."            TO EXCEL-PRODUCAO
           MOVE "QT-REPORT"        TO EXCEL-QT-REPORT
           MOVE "VLR-REPORT"       TO EXCEL-VALOR
           MOVE "MESANO"           TO EXCEL-MESANO
           MOVE "L"                TO EXCEL-CONTA
           WRITE REG-EXCEL

           PERFORM ORDEM.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   MOVE DATA-REPORT-WK    TO DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV          TO MASC-DATA
                   MOVE MASC-DATA         TO EXCEL-DATA
                   MOVE DOCUMENTO-WK      TO EXCEL-DOCTO
                   MOVE CONTRATO-WK       TO EXCEL-CONTRATO
                   MOVE INSTITUICAO-WK    TO EXCEL-INSTITUICAO
                   MOVE CIDADE-WK         TO EXCEL-CIDADE
                   MOVE CURSO-WK          TO EXCEL-CURSO
                   MOVE FUNCAO-WK         TO EXCEL-FUNCAO
                   MOVE QT-FILM-FIT-WK    TO EXCEL-PRODUCAO
                   MOVE QT-REPORT-WK      TO MASC-QT-REPORT
                   MOVE MASC-QT-REPORT    TO EXCEL-QT-REPORT
                   MOVE VALOR-REPORT-WK   TO MASC-VALOR
                   MOVE MASC-VALOR        TO EXCEL-VALOR
                   STRING ANOMES-WK(5:2) "/" ANOMES-WK(1:4)
                   INTO EXCEL-MESANO
                   MOVE LCTO-CTA-CORR-WK  TO EXCEL-CONTA
                   WRITE REG-EXCEL
              END-READ
           END-PERFORM.

           MOVE "TOTAL =>"        TO EXCEL-DATA
           MOVE ULT-SEQ           TO EXCEL-DOCTO
           MOVE SPACES            TO EXCEL-CONTRATO
                                     EXCEL-INSTITUICAO
                                     EXCEL-CIDADE
                                     EXCEL-CURSO
                                     EXCEL-FUNCAO
                                     EXCEL-MESANO
                                     EXCEL-CONTA
           MOVE TOT-FIT-FILME     TO EXCEL-PRODUCAO
           MOVE TOT-REPORT        TO MASC-QT-REPORT
           MOVE MASC-QT-REPORT    TO EXCEL-QT-REPORT
           MOVE TOT-VLR-REPORT    TO MASC-VALOR
           MOVE MASC-VALOR        TO EXCEL-VALOR
           WRITE REG-EXCEL


      *    PERFORM TOTALIZA.
           CLOSE EXCEL.

       CHAMAR-POPUP SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-FORNECEDOR.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-FORNECEDOR.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-FORNEC SECTION.
           MOVE GS-FORNECEDOR  TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "****" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-FORNECEDOR.
       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
           MOVE ZEROS TO TOT-FIT-FILME TOT-REPORT
                         TOT-VLR-REPORT.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           IF GS-TIPO-REL = 1

              MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-ANT
                                     VECTO-INI-REL
              CALL "GRIDAT2" USING DATA-INV
              MOVE DATA-INV    TO VECTO-INI
              MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-ANT
                                     VECTO-FIM-REL
              CALL "GRIDAT2" USING DATA-INV
              MOVE DATA-INV    TO VECTO-FIM

              MOVE VECTO-INI         TO DATA-MOV-R100
              START RED100 KEY IS NOT < DATA-MOV-R100 INVALID KEY
                    MOVE "10" TO ST-RED100
              END-START

           ELSE
              MOVE GS-MESANO-INI     TO MESANO-INV MESANO-INI-REL
              MOVE MESANO-INV(1: 2)  TO MESANO-INI(5: 2)
              MOVE MESANO-INV(3: 4)  TO MESANO-INI(1: 4)
              MOVE MESANO-INI        TO ANOMES-R100

              MOVE GS-MESANO-FIM     TO MESANO-INV MESANO-FIM-REL
              MOVE MESANO-INV(1: 2)  TO MESANO-FIM(5: 2)
              MOVE MESANO-INV(3: 4)  TO MESANO-FIM(1: 4)

              START RED100 KEY IS NOT < ANOMES-R100 INVALID KEY
                    MOVE "10" TO ST-RED100
              END-START.
           MOVE ZEROS TO ULT-SEQ.
           PERFORM UNTIL ST-RED100 = "10"
             READ RED100 NEXT RECORD AT END MOVE "10" TO ST-RED100
              NOT AT END
               IF GS-TIPO-REL = 1
                  IF DATA-MOV-R100 > VECTO-FIM MOVE "10" TO ST-RED100
                  ELSE
                     PERFORM VERIFICA-CODIGO
                  END-IF
               ELSE
                  IF ANOMES-R100 > MESANO-INI MOVE "10" TO ST-RED100
                  ELSE
                     PERFORM VERIFICA-CODIGO
                  END-IF
               END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       VERIFICA-CODIGO SECTION.
           MOVE DOCTO-R100          TO DOCTO-R103
           MOVE GS-FORNECEDOR       TO CODIGO-R103
           MOVE ZEROS               TO FUNCAO-R103
           INITIALIZE REG-WORK.
           START RED103 KEY IS NOT < CHAVE-R103 INVALID KEY
                MOVE "10" TO ST-RED103
           END-START
           PERFORM UNTIL ST-RED103 = "10"
             READ RED103 NEXT RECORD AT END MOVE "10" TO ST-RED103
              NOT AT END
                IF DOCTO-R103 <> DOCTO-R100
                   OR CODIGO-R103 <> GS-FORNECEDOR
                     MOVE "10" TO ST-RED103
                ELSE
                 ADD 1                    TO ULT-SEQ
                 MOVE ULT-SEQ             TO SEQ-WK
                 MOVE DOCTO-R103          TO DOCUMENTO-WK
                 MOVE DATA-MOV-R100       TO DATA-REPORT-WK
                                            GS-EXIBE-MOVTO
                 MOVE LCTO-CTA-CORR-R100  TO LCTO-CTA-CORR-WK
                 MOVE ANOMES-R100         TO ANOMES-WK

                 MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM

                 MOVE DOCTO-R100          TO DOCUMENTO-WK DOCTO-R101
                 MOVE ZEROS TO CONTRATO-R101 EVENTO-R101
                 START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                   MOVE ZEROS TO CONTRATO-R101
                  NOT INVALID KEY
                    READ RED101 NEXT RECORD AT END CONTINUE
                     NOT AT END
                     IF DOCTO-R101 <> DOCTO-R100
                       MOVE ZEROS TO CONTRATO-R101
                     END-IF
                    END-READ
                 END-START

                 MOVE CONTRATO-R101       TO CONTRATO-WK
                                             NR-CONTRATO-CO40
                 READ COD040 INVALID KEY INITIALIZE REG-COD040
                 END-READ
                 MOVE INSTITUICAO-CO40    TO CODIGO-IE10
                 READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10
                 END-READ
                 MOVE NOME-IE10           TO INSTITUICAO-WK
                 MOVE CIDADE-CO40         TO CIDADE
                 READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
                 END-READ
                 MOVE NOME-CID            TO CIDADE-WK
                 MOVE IDENTIFICACAO-CO40  TO CURSO-WK
                 MOVE FUNCAO-R103         TO CODIGO-RE02
                 READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02
                 END-READ
                 MOVE DESCRICAO-RE02      TO FUNCAO-WK
                 MOVE QT-FILMES-R103      TO QT-FILM-FIT-WK
                 MOVE QT-REPORT-R103      TO QT-REPORT-WK
                 MOVE VLR-REPORT-R103     TO VALOR-REPORT-WK
                 ADD QT-FILMES-R103       TO TOT-FIT-FILME
                 ADD QT-REPORT-R103       TO TOT-REPORT
                 ADD VLR-REPORT-R103      TO TOT-VLR-REPORT

                 WRITE REG-WORK
                 END-WRITE
             END-READ
           END-PERFORM.

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
           MOVE DATA-REPORT-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(01: 11)
           MOVE DOCUMENTO-WK      TO GS-LINDET(12: 07)
           MOVE CONTRATO-WK       TO GS-LINDET(19: 5)
           MOVE INSTITUICAO-WK    TO GS-LINDET(24: 16)
           MOVE CIDADE-WK         TO GS-LINDET(40: 14)
           MOVE CURSO-WK          TO GS-LINDET(54: 13)
           MOVE FUNCAO-WK         TO GS-LINDET(67: 5)
           MOVE QT-FILM-FIT-WK    TO GS-LINDET(72: 7)
           MOVE QT-REPORT-WK      TO QTDE-E1
           MOVE QTDE-E1           TO GS-LINDET(79: 9)
           MOVE VALOR-REPORT-WK   TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(88: 14)
           MOVE ANOMES-WK         TO MESANO-INV
           MOVE MESANO-INV(1: 4)  TO ANO-E
           MOVE MESANO-INV(5: 2)  TO MES-E
           MOVE MESANO-E          TO GS-LINDET(102: 8)
           MOVE LCTO-CTA-CORR-WK  TO GS-LINDET(110: 1).

       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "DTA-REPORT" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DATA-REPORT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "DOCUMENTO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DOCUMENTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "CIDADE" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CIDADE-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "FUNCAO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < FUNCAO-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE ULT-SEQ           TO GS-LINTOT(01: 14)
           MOVE TOT-FIT-FILME     TO QTDE-E
           MOVE QTDE-E            TO GS-LINTOT(18: 12)
           MOVE TOT-REPORT        TO QTDE-E1
           MOVE QTDE-E1           TO GS-LINTOT(35: 13)
           MOVE TOT-VLR-REPORT    TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(51: 13)
           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP201" TO DS-SET-NAME
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
           MOVE DATA-REPORT-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01: 11)
           MOVE DOCUMENTO-WK      TO LINDET-REL(12: 7)
           MOVE CONTRATO-WK       TO LINDET-REL(19: 5)
           MOVE INSTITUICAO-WK    TO LINDET-REL(24: 16)
           MOVE CIDADE-WK         TO LINDET-REL(40: 14)
           MOVE CURSO-WK          TO LINDET-REL(54: 13)
           MOVE FUNCAO-WK         TO LINDET-REL(67: 5)
           MOVE QT-FILM-FIT-WK    TO LINDET-REL(72: 7)
           MOVE QT-REPORT-WK      TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(79: 9)
           MOVE VALOR-REPORT-WK   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(89: 13)
           MOVE ANOMES-WK         TO MESANO-INV
           MOVE MESANO-INV(1: 4)  TO ANO-E
           MOVE MESANO-INV(5: 2)  TO MES-E
           MOVE MESANO-E          TO LINDET-REL(102: 8)
           MOVE LCTO-CTA-CORR-WK  TO LINDET-REL(110: 1).

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.


       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE ULT-SEQ           TO LINTOT-REL(01: 14)
           MOVE TOT-FIT-FILME     TO QTDE-E
           MOVE QTDE-E            TO LINTOT-REL(18: 13)
           MOVE TOT-REPORT        TO QTDE-E1
           MOVE QTDE-E1           TO LINTOT-REL(35: 13)
           MOVE TOT-VLR-REPORT    TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(51: 13)

           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           MOVE GS-FORNECEDOR  TO FORNECEDOR-REL.
           MOVE GS-NOME-FORNECEDOR TO NOME-FORNEC-REL.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL ORDEM1-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           IF GS-TIPO-REL = 1
              WRITE REG-RELAT FROM CAB02
           ELSE
              WRITE REG-RELAT FROM CAB06
           END-IF
           WRITE REG-RELAT FROM CAB02A AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 7 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 CAD010 COD040 IED010 RED002 RED100 RED101
                 RED103 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

