       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP206.
      *DATA: 29/09/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELATORIO DE VENDAS CONTABIL
      *        QTDE FOTOS = QFOTOS-REC + QABERTURA-REC
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY RCPX100.
           COPY RCPX101.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CONTRATO-WK.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT EXCEL ASSIGN TO ARQUIVO-EXCEL
                        ORGANIZATION IS SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL
                        FILE STATUS IS FS-EXCEL.

       DATA DIVISION.
       FILE SECTION.
       COPY RCPW100.
       COPY RCPW101.

       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  ALBUM-WK            PIC 9(5).
           05  FOTOS-WK            PIC 9(7).
           05  FITAS-WK            PIC 9(5).
           05  FOLHAS-WK           PIC 9(7).
           05  POSTER-WK           PIC 9(5).
           05  PFITA-WK            PIC 9(5).
           05  DVD-WK              PIC 9(5).
           05  PDVD-WK             PIC 9(5).
           05  FOTO-CD-WK          PIC 9(5).
           05  MOLDURA-WK          pic 9(5).
           05  VLR-VISTA-WK        PIC 9(9)V99.
           05  VLR-PRAZO-WK        PIC 9(9)V99.
           05  VLR-DUPLICATA-WK    PIC 9(9)V99.
           05  VLR-ANTECIPADO-WK   PIC 9(9)V99.
           05  VLR-DEBITO-WK       PIC 9(9)V99.
           05  VLR-CARTAO-WK       PIC 9(9)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       FD EXCEL.
       01 REG-EXCEL.
          05 EXCEL-1               PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-2               PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-3               PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-4               PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-5               PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-6               PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-7               PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-8               PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-9               PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-10              PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-11              PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-12              PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-13              PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-14              PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-15              PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-16              PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-17              PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 FILLER                PIC X(02) VALUE X"0DA0".

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP206.CPB".
           COPY "RCP206.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  FS-EXCEL              PIC XX       VALUE SPACES.
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
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  DATA-W                PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC ZZZ.ZZ9,99.
           05  VALOR-E1              PIC ZZ.ZZ9,99.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  TOTVLRVISTA-W         PIC 9(11)V99 VALUE ZEROS.
           05  TOTVLRPRAZO-W         PIC 9(11)V99 VALUE ZEROS.
           05  TOTVLRDUPLIC-W        PIC 9(11)V99 VALUE ZEROS.
           05  TOTVLRANTEC-W         PIC 9(11)V99 VALUE ZEROS.
           05  TOTVLRDEBITO-W        PIC 9(11)V99 VALUE ZEROS.
           05  TOTVLRCARTAO-W        PIC 9(11)V99 VALUE ZEROS.

           05  ALBUM-E               PIC 9999.9999.
           05  QTDE-E                PIC ZZZZ9.
           05  QTDE-E1               PIC ZZ9.
           05  QTDE1-E               PIC ZZZZ.ZZ9.

           05  TOT-ALBUM             PIC 9(3)     VALUE ZEROS.
           05  TOT-FOLHA             PIC 9(5)     VALUE ZEROS.
           05  TOT-FOTO              PIC 9(5)     VALUE ZEROS.
           05  TOT-FITA              PIC 9(4)     VALUE ZEROS.
           05  TOT-DVD               PIC 9(4)     VALUE ZEROS.
           05  TOT-PFITA             PIC 9(3)     VALUE ZEROS.
           05  TOT-POSTER            PIC 9(3)     VALUE ZEROS.
           05  TOT-PDVD              PIC 9(3)     VALUE ZEROS.
           05  TOT-FOTO-CD           PIC 9(3)     VALUE ZEROS.
           05  TOT-MOLDURA           PIC 9(3)     VALUE ZEROS.
           05  TOT-VLR-VISTA         PIC 9(10)V99 VALUE ZEROS.
           05  TOT-VLR-ANTECIPADO    PIC 9(10)V99 VALUE ZEROS.
           05  TOT-VLR-PRAZO         PIC 9(10)V99 VALUE ZEROS.
           05  TOT-VLR-DUPLICATA     PIC 9(10)V99 VALUE ZEROS.
           05  TOT-VLR-DEBITO        PIC 9(10)V99 VALUE ZEROS.
           05  TOT-VLR-CARTAO        PIC 9(10)V99 VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(40).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(96)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(106)   VALUE
           "RELATORIO DE VENDAS CONTABIL             ".
           05  DESCRICAO-REL       PIC X(11).
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
           05  FILLER              PIC X(03).
           05  FILLER              PIC X(10) VALUE "CONTRATO: ".
           05  CONTRATO-REL        PIC 9(04) VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(130) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
                "CONT ALBUM FOTOS FITAS FOLHAS  POST P.FIT DVD P.DVD FOT
      -    "O-CD MOLD.  VLR-VISTA   VLR-PRAZO VLR-ANTEC VLR-BOL/DP VLR-D
      -    "EBIT VLR-CARTAO".

       01  LINDET.
           05  LINDET-REL          PIC X(130) VALUE SPACES.

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
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT RCD100 RCD101.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
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
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
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
           STRING "\ARQUIVOS\RCP206-" GS-VECTO-INI "-"
                                      GS-VECTO-FIM
           INTO ARQUIVO-EXCEL.
           OPEN OUTPUT EXCEL

           MOVE "CONT"                TO EXCEL-1
           MOVE "ALBUM"               TO EXCEL-2
           MOVE "FOTOS"               TO EXCEL-3
           MOVE "FITAS"               TO EXCEL-4
           MOVE "FOLHAS"              TO EXCEL-5
           MOVE "POST"                TO EXCEL-6
           MOVE "P.FIT"               TO EXCEL-7
           MOVE "DVD"                 TO EXCEL-8
           MOVE "P.DVD"               TO EXCEL-9
           MOVE "FOTO-CD"             TO EXCEL-10
           MOVE "MOLD."               TO EXCEL-11
           MOVE "VLR-VISTA"           TO EXCEL-12
           MOVE "VLR-PRAZO"           TO EXCEL-13
           MOVE "VLR-ANTEC"           TO EXCEL-14
           MOVE "VLR-BOL/DP"          TO EXCEL-15
           MOVE "VLR-DEBIT"           TO EXCEL-16
           MOVE "VLR-CARTAO"          TO EXCEL-17

           WRITE REG-EXCEL

           MOVE ZEROS TO CONTRATO-WK
           START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
              MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   MOVE CONTRATO-WK        TO EXCEL-1
                   MOVE ALBUM-WK           TO QTDE-E
                   MOVE QTDE-E             TO EXCEL-2
                   MOVE FOTOS-WK           TO QTDE-E
                   MOVE QTDE-E             TO EXCEL-3
                   MOVE FITAS-WK           TO QTDE-E
                   MOVE QTDE-E             TO EXCEL-4
                   MOVE FOLHAS-WK          TO QTDE-E
                   MOVE QTDE-E             TO EXCEL-5
                   MOVE POSTER-WK          TO QTDE-E
                   MOVE QTDE-E             TO EXCEL-6
                   MOVE PFITA-WK           TO QTDE-E
                   MOVE QTDE-E             TO EXCEL-7
                   MOVE DVD-WK             TO QTDE-E1
                   MOVE QTDE-E1            TO EXCEL-8
                   MOVE PDVD-WK            TO QTDE-E1
                   MOVE QTDE-E1            TO EXCEL-9
                   MOVE FOTO-CD-WK         TO QTDE-E1
                   MOVE QTDE-E1            TO EXCEL-10
                   MOVE MOLDURA-WK         TO QTDE-E1
                   MOVE QTDE-E1            TO EXCEL-11

                   MOVE VLR-VISTA-WK       TO VALOR-E
                   MOVE VALOR-E            TO EXCEL-12
                   MOVE VLR-PRAZO-WK       TO VALOR-E
                   MOVE VALOR-E            TO EXCEL-13
                   MOVE VLR-ANTECIPADO-WK  TO VALOR-E1
                   MOVE VALOR-E1           TO EXCEL-14
                   MOVE VLR-DUPLICATA-WK   TO VALOR-E1
                   MOVE VALOR-E1           TO EXCEL-15
                   MOVE VLR-DEBITO-WK      TO VALOR-E1
                   MOVE VALOR-E1           TO EXCEL-16
                   MOVE VLR-CARTAO-WK      TO VALOR-E1
                   MOVE VALOR-E1           TO EXCEL-17

                   WRITE REG-EXCEL
              END-READ
           END-PERFORM.

           MOVE SPACES                     TO EXCEL-1
                                              EXCEL-2
                                              EXCEL-3
                                              EXCEL-4
                                              EXCEL-5
                                              EXCEL-6
                                              EXCEL-7
                                              EXCEL-8
                                              EXCEL-9
                                              EXCEL-10
                                              EXCEL-11
                                              EXCEL-12
                                              EXCEL-13
                                              EXCEL-14
                                              EXCEL-15
                                              EXCEL-16
                                              EXCEL-17

           WRITE REG-EXCEL

           MOVE SPACES             TO EXCEL-1
           MOVE TOT-ALBUM          TO QTDE-E
           MOVE QTDE-E             TO EXCEL-2
           MOVE TOT-FOTO           TO QTDE-E
           MOVE QTDE-E             TO EXCEL-3
           MOVE TOT-FITA           TO QTDE-E
           MOVE QTDE-E             TO EXCEL-4
           MOVE TOT-FOLHA          TO QTDE-E
           MOVE QTDE-E             TO EXCEL-5
           MOVE TOT-POSTER         TO QTDE-E
           MOVE QTDE-E             TO EXCEL-6
           MOVE TOT-PFITA          TO QTDE-E
           MOVE QTDE-E             TO EXCEL-7
           MOVE TOT-DVD            TO QTDE-E1
           MOVE QTDE-E1            TO EXCEL-8
           MOVE TOT-PDVD           TO QTDE-E1
           MOVE QTDE-E1            TO EXCEL-9
           MOVE TOT-FOTO-CD        TO QTDE-E1
           MOVE QTDE-E1            TO EXCEL-10
           MOVE TOT-MOLDURA        TO QTDE-E1
           MOVE QTDE-E1            TO EXCEL-11

           MOVE TOT-VLR-VISTA      TO VALOR-E
           MOVE VALOR-E            TO EXCEL-12
           MOVE TOT-VLR-PRAZO      TO VALOR-E
           MOVE VALOR-E            TO EXCEL-13
           MOVE TOT-VLR-ANTECIPADO TO VALOR-E1
           MOVE VALOR-E1           TO EXCEL-14
           MOVE TOT-VLR-DUPLICATA  TO VALOR-E1
           MOVE VALOR-E1           TO EXCEL-15
           MOVE TOT-VLR-DEBITO     TO VALOR-E1
           MOVE VALOR-E1           TO EXCEL-16
           MOVE TOT-VLR-CARTAO     TO VALOR-E1
           MOVE VALOR-E1           TO EXCEL-17

           WRITE REG-EXCEL

           CLOSE EXCEL.
       GERAR-EXCEL-FIM.
           EXIT.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
      *--------------------------------------------------------------
       GRAVA-WORK SECTION.
           CLOSE WORK.  OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-FIM.
           MOVE GS-CONTRATO  TO CONTRATO-REL
           EVALUATE GS-OPCAO
               WHEN 1 MOVE "INT.MOVTO: " TO DESCRICAO-REL
               WHEN 2 MOVE "INT.VENDA: " TO DESCRICAO-REL
           END-EVALUATE

           INITIALIZE REG-RCD100

           IF GS-CONTRATO > 0
              STRING GS-CONTRATO "0000"  INTO ALBUM-REC
              START RCD100 KEY IS NOT < ALBUM-REC INVALID KEY
                    MOVE "10" TO ST-RCD100
              END-START
           ELSE
             EVALUATE GS-OPCAO
                 WHEN 1 MOVE VECTO-INI    TO DATA-MOVTO-REC
                        MOVE ZEROS        TO ALBUM-REC
                        START RCD100 KEY IS NOT < ALT-REC INVALID KEY
                              MOVE "10" TO ST-RCD100
                        END-START
                 WHEN 2 MOVE VECTO-INI    TO DATAVEN-REC
                        MOVE ZEROS        TO ALBUM-REC
                        START RCD100 KEY IS NOT < DATAVEN-REC
                                                            INVALID KEY
                              MOVE "10" TO ST-RCD100
                        END-START
             END-EVALUATE
           END-IF

           PERFORM UNTIL ST-RCD100 = "10"
              READ RCD100 NEXT RECORD AT END
                   MOVE "10" TO ST-RCD100
              NOT AT END
                  IF GS-CONTRATO > 0
                     IF GS-CONTRATO <> ALBUM-REC(1:4)
                        MOVE "10" TO ST-RCD100
                     ELSE
                        IF GS-OPCAO = 1
                           IF (VECTO-FIM = 0 AND VECTO-INI = 0)
                                  MOVE DATA-MOVTO-REC TO GS-EXIBE-MOVTO
                                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                                  PERFORM GRAVA-DADOS-WORK
                           ELSE
                               IF (DATA-MOVTO-REC NOT < VECTO-INI AND
                                   DATA-MOVTO-REC NOT > VECTO-FIM)
                                   MOVE DATA-MOVTO-REC TO GS-EXIBE-MOVTO
                                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                   PERFORM CALL-DIALOG-SYSTEM
                                    PERFORM GRAVA-DADOS-WORK
                               END-IF
                           END-IF
                        ELSE
                           IF (VECTO-FIM = 0 AND VECTO-INI = 0)
                                  MOVE DATAVEN-REC TO GS-EXIBE-MOVTO
                                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                                  PERFORM GRAVA-DADOS-WORK
                           ELSE
                               IF (DATAVEN-REC NOT < VECTO-INI AND
                                   DATAVEN-REC NOT > VECTO-FIM)
                                      MOVE DATAVEN-REC TO GS-EXIBE-MOVTO
                                    MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                      PERFORM CALL-DIALOG-SYSTEM
                                      PERFORM GRAVA-DADOS-WORK
                               END-IF
                           END-IF
                        END-IF
                     END-IF
                  ELSE
                     EVALUATE GS-OPCAO
                         WHEN 1 MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
                                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                PERFORM CALL-DIALOG-SYSTEM
                                IF DATA-MOVTO-REC > VECTO-FIM
                                   MOVE "10" TO ST-RCD100
                                ELSE
                                      PERFORM GRAVA-DADOS-WORK
                                END-IF
                         WHEN 2 MOVE DATAVEN-REC     TO GS-EXIBE-MOVTO
                                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                PERFORM CALL-DIALOG-SYSTEM
                                IF DATAVEN-REC > VECTO-FIM
                                   MOVE "10" TO ST-RCD100
                                ELSE
                                     PERFORM GRAVA-DADOS-WORK
                                END-IF
                     END-EVALUATE
                  END-IF
              END-READ
           END-PERFORM
      *             MOVE DATA-MOVTO-REC TO GS-EXIBE-MOVTO
      *             MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
      *             PERFORM CALL-DIALOG-SYSTEM
      *             PERFORM GRAVA-DADOS-WORK
      *          END-IF
      *      END-READ
      *    END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       GRAVA-DADOS-WORK SECTION.
           MOVE ALBUM-REC(1: 4)    TO CONTRATO-WK
           READ WORK INVALID KEY
               MOVE QENCADER-REC   TO ALBUM-WK
               COMPUTE FOTOS-WK = QFOTOS-REC + QABERTURA-REC
               MOVE QFITAS-REC     TO FITAS-WK
               MOVE QDVD-REC       TO DVD-WK
               MOVE QFOLHAS-REC    TO FOLHAS-WK
               MOVE QPOSTER-REC    TO POSTER-WK
               MOVE QPFITA-REC     TO PFITA-WK
               MOVE QPORTA-DVD-REC TO PDVD-WK
               MOVE QFOTO-CD-REC   TO FOTO-CD-WK
               MOVE QMOLDURA-REC   TO MOLDURA-WK
               MOVE ALBUM-REC      TO ALBUM-REC1
               PERFORM GRAVA-VALORES
               MOVE TOTVLRVISTA-W  TO VLR-VISTA-WK
               MOVE TOTVLRPRAZO-W  TO VLR-PRAZO-WK
               MOVE TOTVLRDUPLIC-W TO VLR-DUPLICATA-WK
               MOVE TOTVLRANTEC-W  TO VLR-ANTECIPADO-WK
               MOVE TOTVLRDEBITO-W TO VLR-DEBITO-WK
               MOVE TOTVLRCARTAO-W TO VLR-CARTAO-WK
               WRITE REG-WORK
               END-WRITE
             NOT INVALID KEY
               ADD QENCADER-REC   TO ALBUM-WK
               ADD QFOTOS-REC     TO FOTOS-WK
               ADD QDVD-REC       TO DVD-WK
               ADD QABERTURA-REC  TO FOTOS-WK
               ADD QFITAS-REC     TO FITAS-WK
               ADD QFOLHAS-REC    TO FOLHAS-WK
               ADD QPOSTER-REC    TO POSTER-WK
               ADD QPFITA-REC     TO PFITA-WK
               ADD QPORTA-DVD-REC TO PDVD-WK
               ADD QFOTO-CD-REC   TO FOTO-CD-WK
               ADD QMOLDURA-REC   TO MOLDURA-WK
               ADD ALBUM-REC      TO ALBUM-REC1
               PERFORM GRAVA-VALORES
               ADD TOTVLRVISTA-W  TO VLR-VISTA-WK
               ADD TOTVLRPRAZO-W  TO VLR-PRAZO-WK
               ADD TOTVLRDUPLIC-W TO VLR-DUPLICATA-WK
               ADD TOTVLRANTEC-W  TO VLR-ANTECIPADO-WK
               ADD TOTVLRDEBITO-W TO VLR-DEBITO-WK
               ADD TOTVLRCARTAO-W TO VLR-CARTAO-WK
               REWRITE REG-WORK
               END-REWRITE
           END-READ.
       GRAVA-VALORES SECTION.
           MOVE ALBUM-REC  TO ALBUM-REC1.
           START RCD101 KEY IS NOT < ALBUM-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101.
           MOVE ZEROS TO TOTVLRVISTA-W TOTVLRDUPLIC-W TOTVLRPRAZO-W
                         TOTVLRANTEC-W TOTVLRDEBITO-W TOTVLRCARTAO-W.
           PERFORM UNTIL ST-RCD101 = "10"
             READ RCD101 NEXT RECORD AT END
                  MOVE "10" TO ST-RCD101
             NOT AT END
               IF ALBUM-REC1 <> ALBUM-REC
                  MOVE "10" TO ST-RCD101
               ELSE
                 EVALUATE TIPO-REC1
                   WHEN 2 ADD VALOR-REC1   TO TOTVLRVISTA-W
                   WHEN 3 ADD VALOR-REC1   TO TOTVLRANTEC-W
                   WHEN 4 ADD VALOR-REC1   TO TOTVLRDUPLIC-W
                   WHEN 5 ADD VALOR-REC1   TO TOTVLRDEBITO-W
                   WHEN 6 ADD VALOR-REC1   TO TOTVLRCARTAO-W
                   WHEN OTHER
                     IF VENCTO-REC1 NOT > DATA-MOVTO-REC
                        ADD VALOR-REC1 TO TOTVLRVISTA-W
                     ELSE
                        ADD VALOR-REC1 TO TOTVLRPRAZO-W
                     END-IF
                 END-EVALUATE
               END-IF
             END-READ
           END-PERFORM.

      *-------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-ALBUM TOT-FOLHA TOT-PFITA TOT-FOTO
                         TOT-FITA TOT-POSTER TOT-VLR-VISTA
                         TOT-VLR-PRAZO TOT-VLR-DUPLICATA
                         TOT-VLR-ANTECIPADO TOT-DVD
                         TOT-VLR-DEBITO TOT-VLR-CARTAO
                         TOT-PDVD TOT-FOTO-CD TOT-MOLDURA.

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO GS-LINDET.

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "---------------" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE "CONT ALBUM FOTOS FITAS FOLHAS  POST P.FIT DVD P.DVD FOT
      -    "O-CD MOLD.  VLR-VISTA   VLR-PRAZO VLR-ANTEC VLR-BOL/DP VLR-D
      -    "EBIT VLR-CARTAO"
           TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "---------------" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE SPACES TO GS-LINDET

           MOVE ZEROS TO CONTRATO-WK
           START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
              MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-GER.

       MOVER-DADOS-LINDET SECTION.
           MOVE SPACES             TO GS-LINDET.
           MOVE CONTRATO-WK        TO GS-LINDET(1: 5)
           MOVE ALBUM-WK           TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(6: 6)
           MOVE FOTOS-WK           TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(12: 5)
           MOVE FITAS-WK           TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(18: 6)
           MOVE FOLHAS-WK          TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(25: 6)
           MOVE POSTER-WK          TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(31: 6)
           MOVE PFITA-WK           TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(37: 6)
           MOVE DVD-WK             TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(43: 4)
           MOVE PDVD-WK            TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(49: 4)
           MOVE FOTO-CD-WK         TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(57: 4)
           MOVE MOLDURA-WK         TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(63: 4)

           MOVE VLR-VISTA-WK       TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(67: 11)
           MOVE VLR-PRAZO-WK       TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(79: 11)
           MOVE VLR-ANTECIPADO-WK  TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(90: 10).
           MOVE VLR-DUPLICATA-WK   TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(101: 10)
           MOVE VLR-DEBITO-WK      TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(111: 10)
           MOVE VLR-CARTAO-WK      TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(122: 10)

           ADD ALBUM-WK            TO TOT-ALBUM
           ADD FOTOS-WK            TO TOT-FOTO
           ADD FITAS-WK            TO TOT-FITA
           ADD DVD-WK              TO TOT-DVD
           ADD FOLHAS-WK           TO TOT-FOLHA
           ADD POSTER-WK           TO TOT-POSTER
           ADD PFITA-WK            TO TOT-PFITA
           ADD PDVD-WK             TO TOT-PDVD
           ADD FOTO-CD-WK          TO TOT-FOTO-CD
           ADD MOLDURA-WK          TO TOT-MOLDURA
           ADD VLR-VISTA-WK        TO TOT-VLR-VISTA
           ADD VLR-PRAZO-WK        TO TOT-VLR-PRAZO
           ADD VLR-DUPLICATA-WK    TO TOT-VLR-DUPLICATA
           ADD VLR-ANTECIPADO-WK   TO TOT-VLR-ANTECIPADO
           ADD VLR-DEBITO-WK       TO TOT-VLR-DEBITO
           ADD VLR-CARTAO-WK       TO TOT-VLR-CARTAO.

       TOTALIZA-GER SECTION.
           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "---------------" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO GS-LINDET.

           MOVE TOT-ALBUM          TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(6: 6)
           MOVE TOT-FOTO           TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(12: 5)
           MOVE TOT-FITA           TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(18: 6)
           MOVE TOT-FOLHA          TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(25: 6)
           MOVE TOT-POSTER         TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(31: 6)
           MOVE TOT-PFITA          TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(37: 6)
           MOVE TOT-DVD            TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(43: 4)
           MOVE TOT-PDVD           TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(49: 4)
           MOVE TOT-FOTO-CD        TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(57: 4)
           MOVE TOT-MOLDURA        TO QTDE-E1
           MOVE QTDE-E1            TO GS-LINDET(63: 4)

           MOVE TOT-VLR-VISTA      TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(67: 11)
           MOVE TOT-VLR-PRAZO      TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(79: 11)
           MOVE TOT-VLR-ANTECIPADO TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(90: 10).
           MOVE TOT-VLR-DUPLICATA  TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(101: 10)
           MOVE TOT-VLR-DEBITO     TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(111: 10)
           MOVE TOT-VLR-CARTAO     TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(122: 10)



           MOVE "INSERE-LIST"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP206" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *----------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE ZEROS TO CONTRATO-WK
           START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
               MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                PERFORM MOVER-DADOS-RELATORIO
                WRITE REG-RELAT FROM LINDET
                ADD 1 TO LIN
                IF LIN > 56 PERFORM CABECALHO
                END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-GER-REL.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES             TO LINDET-REL.
           MOVE CONTRATO-WK        TO LINDET-REL(1: 5)
           MOVE ALBUM-WK           TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(6: 7)
           MOVE FOTOS-WK           TO QTDE1-E
           MOVE QTDE1-E            TO LINDET-REL(13: 9)
           MOVE FITAS-WK           TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(22: 7)
           MOVE FOLHAS-WK          TO QTDE1-E
           MOVE QTDE1-E            TO LINDET-REL(29: 9)
           MOVE POSTER-WK          TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(38: 7)
           MOVE PFITA-WK           TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(45: 7)
           MOVE VLR-VISTA-WK       TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(52: 15)
           MOVE VLR-PRAZO-WK       TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(67: 15)
           MOVE VLR-DUPLICATA-WK   TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(82: 15)
           MOVE VLR-ANTECIPADO-WK  TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(97: 13).

       TOTALIZA-GER-REL SECTION.
           MOVE SPACES             TO LINDET-REL.

           MOVE TOT-ALBUM          TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(6: 7)
           MOVE TOT-FOTO           TO QTDE1-E
           MOVE QTDE1-E            TO LINDET-REL(13: 9)
           MOVE TOT-FITA           TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(22: 7)
           MOVE TOT-FOLHA          TO QTDE1-E
           MOVE QTDE1-E            TO LINDET-REL(29: 9)
           MOVE TOT-POSTER         TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(38: 7)
           MOVE TOT-PFITA          TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(45: 7)
           MOVE TOT-VLR-VISTA      TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(52: 15)
           MOVE TOT-VLR-PRAZO      TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(67: 15)
           MOVE TOT-VLR-DUPLICATA  TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(82: 15)
           MOVE TOT-VLR-ANTECIPADO TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(97: 13).

           WRITE REG-RELAT FROM LINDET AFTER 2.
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
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE RCD100 RCD101 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
