       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP209.
      *DATA: 01/11/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELATORIO DE REPORTAGEM ANALÍTICO  - POR CONTRATO
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX012.
           COPY COPX003.
           COPY COPX040.
           COPY REPX100.
           COPY REPX101.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS DATA-REPORT-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DOCUMENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK = EVENTO-WK CIDADE-WK
                                   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CONTRATO-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT WORK1 ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK1
                  RECORD KEY IS DOCTO-WK1.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW012.
       COPY COPW003.
       COPY COPW040.
       COPY REPW100.
       COPY REPW101.

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(5).
           05  DOCUMENTO-WK        PIC 9(6).
           05  CONTRATO-WK         PIC 9(4).
           05  DATA-REPORT-WK      PIC 9(8).
           05  DATA-VENDA-WK       PIC 9(8).
           05  FORMANDO-WK         PIC 9(4).
           05  PARTICIPANTE-WK     PIC 9(4).
           05  QT-EQUIPE-WK        PIC 99V9.
           05  EVENTO-WK           PIC X(10).
           05  CIDADE-WK           PIC X(11).
           05  PADRAO-WK           PIC X.
           05  QT-FILME-WK         PIC 9(3)V99.
           05  QT-FITA-WK          PIC 9(3)V99.
           05  VLR-REPORT-WK       PIC 9(8)V99.
           05  VLR-DESPESA-WK      PIC 9(8)V99.
           05  PM-WK               PIC 9(4)V99.
           05  JUROS-WK            PIC 9(8)V99.
           05  CUSTO-CORRIG-WK     PIC 9(8)V99.
       FD  WORK1.
       01  REG-WORK1.
           05  DOCTO-WK1         PIC 9(6).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(140).
       WORKING-STORAGE SECTION.
           COPY "REP209.CPB".
           COPY "REP209.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
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
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  DESPESA-W             PIC 9(8)V99  VALUE ZEROS.
      *    GRAVA-W CONTROLA SE GRAVA P/ WORK - ORDEM CIDADE / REGIAO
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZ,ZZ.
           05  QTDE-E1               PIC Z.ZZZ,ZZ.
           05  QTDE-E2               PIC ZZ.ZZZ,ZZ.
           05  QTDE-E3               PIC ZZZ.ZZZ.
           05  QTDE-E4               PIC ZZ,Z.
           05  TOT-PARTICIPANTE      PIC 9(6)     VALUE ZEROS.
      *    CALCULO PM E JUROS
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  PM-E                  PIC ZZZ,ZZ   BLANK WHEN ZEROS.

           05  TOT-GER-FITA          PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-FILME         PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-EQUIPE        PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-FORMANDO      PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-PARTICIPANTE  PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-VLR-REPORT    PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-VLR-JUROS     PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-VLR-CORRIG    PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-VLR-DESPESA   PIC 9(8)V99  VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(40).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(95)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RELATORIO DE REPORTAGEM     ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(45)   VALUE SPACES.
      *    05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
      *    05  VECTO-INI-REL       PIC 99/99/9999.
      *    05  FILLER              PIC X(3)    VALUE ' a '.
      *    05  VECTO-FIM-REL       PIC 99/99/9999.
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
       01  CAB02C.
           05  FILLER              PIC X(10)   VALUE "CONTRATO: ".
           05  CONTRATO-REL        PIC ZZZZ    BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  DESC-CONTRATO-REL   PIC X(30)   VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(140)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(140)  VALUE
           "DATA-REPOR  DOCTO CONT CIDADE      P FORM EVENTO     PART EQ
      -    "U. Q-FITA Q-FILM VLR-REPORT VLR-DESPES  VLR-TOTAL      JUROS
      -    "   P.M. VLR-CORRIG".
       01  LINDET.
           05  LINDET-REL          PIC X(140)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(122)  VALUE
           "QT.FORM QT.PART QT.EQUIPE   QT.FITA  QT-FILME VLR-TOT-REPOR
      -    "TOT-DESPES   TOTAL-GERAL      JUROS VLR-CORRIGIDO".

       01  LINTOT.
           05  LINTOT-REL          PIC X(122)  VALUE SPACES.

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
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           ACCEPT VARIA-W1 FROM TIME.
           add 100 to varia-w1
           OPEN OUTPUT WORK1  CLOSE WORK1  OPEN I-O WORK1.

           OPEN INPUT CAD010 CAD012 COD003 COD040 RED100 RED101.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED101 <> "00"
              MOVE "ERRO ABERTURA RED101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM GRAVA-WORK1
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM CHAMAR-POPUP-CONTRATO
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
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
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE "****" TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO.
       CHAMAR-POPUP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP040T".
           MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO.
           PERFORM LE-CONTRATO.
       GRAVA-WORK1 SECTION.
           CLOSE WORK1  OPEN OUTPUT WORK1  CLOSE WORK1  OPEN I-O WORK1.
           MOVE GS-CONTRATO    TO CONTRATO-R101.
           START RED101 KEY IS NOT < CONTRATO-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
                 READ RED101 NEXT RECORD AT END
                      MOVE "10" TO ST-RED101
                 NOT AT END
                     IF GS-CONTRATO <> CONTRATO-R101
                        MOVE "10" TO ST-RED101
                     ELSE
                        MOVE DOCTO-R101  TO DOCTO-WK1
                        READ WORK1 INVALID KEY WRITE REG-WORK1
                        END-READ
                     END-IF
                 END-READ
           END-PERFORM.
       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
           MOVE ZEROS TO TOT-GER-FILME TOT-GER-FORMANDO
                 TOT-GER-VLR-DESPESA TOT-GER-EQUIPE
                 TOT-GER-PARTICIPANTE TOT-GER-VLR-REPORT TOT-GER-FITA
                 TOT-GER-VLR-JUROS TOT-GER-VLR-CORRIG.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

      *    MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-ANT
      *                              VECTO-INI-REL.
      *    CALL "GRIDAT2" USING DATA-INV.
      *    MOVE DATA-INV    TO VECTO-INI.
      *    MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-ANT
      *                              VECTO-FIM-REL.
      *    CALL "GRIDAT2" USING DATA-INV.
      *    MOVE DATA-INV    TO VECTO-FIM.
      *
      *
      *    MOVE VECTO-INI         TO DATA-MOV-R100.
      *    START RED100 KEY IS NOT < DATA-MOV-R100 INVALID KEY
      *           MOVE "10" TO ST-RED100.
           MOVE ZEROS TO DOCTO-WK1.
           START WORK1 KEY IS NOT < DOCTO-WK1 INVALID KEY
                       MOVE "10" TO ST-WORK1.
           MOVE ZEROS TO ULT-SEQ.
           PERFORM UNTIL ST-WORK1 = "10"
             READ WORK1 NEXT RECORD AT END MOVE "10" TO ST-WORK1
              NOT AT END
               MOVE DOCTO-WK1   TO DOCTO-R100
               READ RED100 INVALID KEY INITIALIZE REG-RED100
               END-READ
               PERFORM VERIFICA-CONT-EVENTO
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       VERIFICA-CONT-EVENTO SECTION.
           PERFORM VERIFICA-TOTAL-PARTICIPANTE
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE GS-CONTRATO         TO CONTRATO-R101.
           MOVE ZEROS               TO EVENTO-R101
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END
                  MOVE "10" TO ST-RED101
             NOT AT END
                IF DOCTO-R101 <> DOCTO-R100 OR
                   CONTRATO-R101 <> GS-CONTRATO
                     MOVE "10" TO ST-RED101
                ELSE
                 INITIALIZE REG-WORK
                 MOVE CONTRATO-R101         TO CONTRATO-WK
                                               NR-CONTRATO-CO40
                 READ COD040 INVALID KEY
                      INITIALIZE REG-COD040
                 END-READ
                 MOVE DATA-PREV-VENDA-CO40  TO DATA-INV
                 CALL "GRIDAT2" USING DATA-INV
                 MOVE DATA-INV              TO GRTIME-DATE-FINAL
                 MOVE DATA-MOV-R100         TO GRTIME-DATE
                 IF GRTIME-DATE > GRTIME-DATE-FINAL
                    MOVE GRTIME-DATE-FINAL  TO GRTIME-DATE
                 END-IF

                 MOVE CIDADE-CO40           TO CIDADE
                 MOVE PADRAO-CO40           TO PADRAO-WK
                 READ CAD010 INVALID KEY
                      MOVE SPACES           TO NOME-CID
                 END-READ
                 MOVE NOME-CID              TO CIDADE-WK
                 ADD 1                      TO ULT-SEQ
                 MOVE ULT-SEQ               TO SEQ-WK
                 MOVE DOCTO-R100            TO DOCUMENTO-WK
                 MOVE DATA-MOV-R100         TO DATA-REPORT-WK
                                               GS-EXIBE-MOVTO
                 MOVE "TELA-AGUARDA1"       TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM

                 MOVE QT-PARTIC-R101        TO PARTICIPANTE-WK
                 MOVE QTDE-FORM-CO40        TO FORMANDO-WK
                 MOVE EVENTO-R101           TO CODIGO-CO03
                 READ COD003 INVALID KEY
                      MOVE SPACES           TO NOME-CO03
                 END-READ
                 MOVE NOME-CO03             TO EVENTO-WK
                 COMPUTE QT-FILME-WK ROUNDED = (TOT-FILME-REPORT-R100
                                 * QT-PARTIC-R101) / TOT-PARTICIPANTE
                 COMPUTE QT-FITA-WK ROUNDED = (TOT-FITA-REPORT-R100
                          * QT-PARTIC-R101) / TOT-PARTICIPANTE

                 COMPUTE QT-EQUIPE-WK ROUNDED = (QTDE-PESSOAS-R100 *
                                   QT-PARTIC-R101) / TOT-PARTICIPANTE

                 COMPUTE VLR-REPORT-WK ROUNDED = (VLR-TOT-REPORT-R100 *
                                   QT-PARTIC-R101) / TOT-PARTICIPANTE

                 COMPUTE DESPESA-W = VLR-COMB-R100           +
                                     VLR-HOSP-R100           +
                                     VLR-REFEICAO-R100       +
                                     VLR-PASSAGEM-R100       +
                                     VLR-MAT-R100            +
                                     VLR-DESPESA-REPORT-R100 +
                                     VLR-ALUGUEL-R100        +
                                     VLR-OUTROS-R100

                 COMPUTE VLR-DESPESA-WK ROUNDED =
                     (DESPESA-W * QT-PARTIC-R101) / TOT-PARTICIPANTE

      *          IF DOCTO-R100 = 44185
      *             DISPLAY "DESPESA-W = " DESPESA-W STOP " "
      *
      *             DISPLAY "VLR-TOT-REPORT-R100 = "
      *             VLR-TOT-REPORT-R100 STOP " "
      *
      *             DISPLAY "TOT-PARTICIPANTE = "
      *             TOT-PARTICIPANTE STOP " "
      *
      *             DISPLAY "QT-PARTIC-R101 = "
      *             QT-PARTIC-R101 STOP " "
      *
      *             DISPLAY "VLR-REPORT-WK = "
      *             VLR-REPORT-WK STOP " "
      *             DISPLAY "VLR-DESPESA-WK = "
      *             VLR-DESPESA-WK STOP " "
      *          END-IF


      *          ADD QT-FILME-WK        TO TOT-GER-FILME
      *          ADD QT-FITA-WK         TO TOT-GER-FITA
      *          ADD QT-EQUIPE-WK       TO TOT-GER-EQUIPE
      *          ADD FORMANDO-WK        TO TOT-GER-FORMANDO
      *          ADD PARTICIPANTE-WK    TO TOT-GER-PARTICIPANTE
      *          ADD VLR-REPORT-WK      TO TOT-GER-VLR-REPORT
      *          ADD VLR-DESPESA-WK     TO TOT-GER-VLR-DESPESA
                 MOVE 2 TO GRTIME-TYPE
                 MOVE 3 TO GRTIME-FUNCTION
                 CALL "GRTIME" USING PARAMETROS-GRTIME
                 COMPUTE PM-WK = GRTIME-DAYS-FINAL / 30
                 MOVE PM-WK       TO PRAZO-MEDIO
                 MOVE 1           TO TAXA-ACUMULADA
                 PERFORM VARYING CONT FROM 1 BY 1 UNTIL
                            CONT > PRAZO-MEDIO
                     COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                              ((GS-TAXA / 100) + 1)
                 END-PERFORM

                 COMPUTE JUROS-WK = (VLR-REPORT-WK + VLR-DESPESA-WK) *
                                    (TAXA-ACUMULADA - 1)
                 COMPUTE CUSTO-CORRIG-WK = VLR-REPORT-WK +
                        VLR-DESPESA-WK + JUROS-WK
      *          ADD JUROS-WK           TO TOT-GER-VLR-JUROS
      *          ADD CUSTO-CORRIG-WK    TO TOT-GER-VLR-CORRIG
                 WRITE REG-WORK
                 END-WRITE

             END-READ
           END-PERFORM.

       VERIFICA-TOTAL-PARTICIPANTE SECTION.
           MOVE ZEROS               TO TOT-PARTICIPANTE.
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE ZEROS               TO CONTRATO-R101 EVENTO-R101.
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END MOVE "10" TO ST-RED101
               NOT AT END
                IF DOCTO-R101 <> DOCTO-R100
                     MOVE "10" TO ST-RED101
                ELSE
                  ADD QT-PARTIC-R101   TO TOT-PARTICIPANTE
                END-IF
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
                IF CONTRATO-WK <> GS-CONTRATO CONTINUE
                ELSE
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM

                   ADD QT-FILME-WK        TO TOT-GER-FILME
                   ADD QT-FITA-WK         TO TOT-GER-FITA
                   ADD QT-EQUIPE-WK       TO TOT-GER-EQUIPE
                   ADD FORMANDO-WK        TO TOT-GER-FORMANDO
                   ADD PARTICIPANTE-WK    TO TOT-GER-PARTICIPANTE
                   ADD VLR-REPORT-WK      TO TOT-GER-VLR-REPORT
                   ADD VLR-DESPESA-WK     TO TOT-GER-VLR-DESPESA

                   ADD JUROS-WK           TO TOT-GER-VLR-JUROS
                   ADD CUSTO-CORRIG-WK    TO TOT-GER-VLR-CORRIG

                END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA.

       MOVER-DADOS-LINDET SECTION.
           MOVE DATA-REPORT-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(01: 11)
           MOVE DOCUMENTO-WK      TO GS-LINDET(12: 7)
           MOVE CONTRATO-WK       TO GS-LINDET(19: 5)
           MOVE CIDADE-WK         TO GS-LINDET(24: 12)
           MOVE PADRAO-WK         TO GS-LINDET(36: 2)
           MOVE FORMANDO-WK       TO GS-LINDET(38: 05)
           MOVE EVENTO-WK         TO GS-LINDET(43: 11)
           MOVE PARTICIPANTE-WK   TO GS-LINDET(54: 5)
           MOVE QT-EQUIPE-WK      TO QTDE-E4
           MOVE QTDE-E4           TO GS-LINDET(59: 5)
           MOVE QT-FITA-WK        TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(64: 7)
           MOVE QT-FILME-WK       TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(71: 7)
           MOVE VLR-REPORT-WK     TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(78: 11)
           MOVE VLR-DESPESA-WK    TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(89: 11).
           ADD VLR-REPORT-WK VLR-DESPESA-WK GIVING VALOR-E
           MOVE VALOR-E           TO GS-LINDET(100: 10).
           MOVE JUROS-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(115: 12)
           MOVE PM-WK             TO PM-E
           MOVE PM-E              TO GS-LINDET(127: 7)
           MOVE CUSTO-CORRIG-WK     TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(134: 13).
       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "DTA-REPORT" TO GS-DESCR-ORDEM ORDEM-REL
                START WORK KEY IS NOT < DATA-REPORT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "DOCUMENTO" TO GS-DESCR-ORDEM ORDEM-REL
                START WORK KEY IS NOT < DOCUMENTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CONTRATO" TO GS-DESCR-ORDEM ORDEM-REL
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "CIDADE" TO GS-DESCR-ORDEM ORDEM-REL
                START WORK KEY IS NOT < CIDADE-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "EVENTO" TO GS-DESCR-ORDEM ORDEM-REL
                START WORK KEY IS NOT < ALT1-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE TOT-GER-FORMANDO     TO QTDE-E3
           MOVE QTDE-E3              TO GS-LINTOT(1: 8)
           MOVE TOT-GER-PARTICIPANTE TO QTDE-E3
           MOVE QTDE-E3              TO GS-LINTOT(9: 8)
           MOVE TOT-GER-EQUIPE       TO QTDE-E2
           MOVE QTDE-E2              TO GS-LINTOT(17: 10)
           MOVE TOT-GER-FITA         TO QTDE-E2
           MOVE QTDE-E2              TO GS-LINTOT(27: 10)
           MOVE TOT-GER-FILME        TO QTDE-E2
           MOVE QTDE-E2              TO GS-LINTOT(37: 10)
           MOVE TOT-GER-VLR-REPORT   TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINTOT(47: 14)
           MOVE TOT-GER-VLR-DESPESA  TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(61: 11)
           ADD TOT-GER-VLR-REPORT TO TOT-GER-VLR-DESPESA GIVING VALOR-E1
           MOVE VALOR-E1             TO GS-LINTOT(72: 14).
           MOVE TOT-GER-VLR-JUROS    TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(86: 11)
           MOVE TOT-GER-VLR-CORRIG   TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINTOT(97: 13).

           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP209" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.

           MOVE ZEROS TO PAG-W.

           copy condensa.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF CONTRATO-WK <> GS-CONTRATO CONTINUE
                ELSE
                   PERFORM MOVER-DADOS-RELATORIO
                END-IF
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
           MOVE CIDADE-WK         TO LINDET-REL(24: 12)
           MOVE PADRAO-WK         TO LINDET-REL(36: 2)
           MOVE FORMANDO-WK       TO LINDET-REL(38: 05)
           MOVE EVENTO-WK         TO LINDET-REL(43: 11)
           MOVE PARTICIPANTE-WK   TO LINDET-REL(54: 5)
           MOVE QT-EQUIPE-WK      TO QTDE-E4
           MOVE QTDE-E4           TO LINDET-REL(59: 5)
           MOVE QT-FITA-WK        TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(64: 7)
           MOVE QT-FILME-WK       TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(71: 7)
           MOVE VLR-REPORT-WK     TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(78: 11)
           MOVE VLR-DESPESA-WK    TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(89: 11).
           ADD VLR-REPORT-WK TO VLR-DESPESA-WK GIVING VALOR-E
           MOVE VALOR-E           TO LINDET-REL(100: 10).
           MOVE JUROS-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(111: 11)
           MOVE PM-WK             TO PM-E
           MOVE PM-E              TO LINDET-REL(122: 7)
           MOVE CUSTO-CORRIG-WK   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(129: 10)

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.


       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
      *    MOVE TOT-GER-FORMANDO     TO QTDE-E3
      *    MOVE QTDE-E3              TO LINTOT-REL(1: 10)
      *    MOVE TOT-GER-PARTICIPANTE TO QTDE-E3
      *    MOVE QTDE-E3              TO LINTOT-REL(11: 10)
      *    MOVE TOT-GER-EQUIPE       TO QTDE-E2
      *    MOVE QTDE-E2              TO LINTOT-REL(21: 10)
      *    MOVE TOT-GER-FITA         TO QTDE-E2
      *    MOVE QTDE-E2              TO LINTOT-REL(31: 10)
      *    MOVE TOT-GER-FILME        TO QTDE-E2
      *    MOVE QTDE-E2              TO LINTOT-REL(41: 13)
      *    MOVE TOT-GER-VLR-REPORT   TO VALOR-E1
      *    MOVE VALOR-E1             TO LINTOT-REL(54: 17)
      *    MOVE TOT-GER-VLR-DESPESA  TO VALOR-E1
      *    MOVE VALOR-E1             TO LINTOT-REL(71: 14)
      *    ADD TOT-GER-VLR-REPORT TO TOT-GER-VLR-DESPESA GIVING VALOR-E1
      *    MOVE VALOR-E1             TO LINTOT-REL(85: 13).
      *    MOVE TOT-GER-VLR-JUROS    TO VALOR-E
      *    MOVE VALOR-E              TO LINTOT-REL(98: 11)
      *    MOVE TOT-GER-VLR-CORRIG   TO VALOR-E1
      *    MOVE VALOR-E1             TO LINTOT-REL(109: 13).

           MOVE GS-LINTOT TO LINTOT-REL

           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           MOVE 2 TO LIN.

           EVALUATE GS-TIPO-REL
             WHEN 1 CONTINUE
             WHEN 2 MOVE GS-CIDADE      TO CIDADE-REL
                    MOVE GS-NOME-CID    TO NOME-CID-REL
                    WRITE REG-RELAT FROM CAB02A AFTER 2
                    ADD 2 TO LIN
             WHEN 3 MOVE GS-REGIAO      TO REGIAO-REL
                    MOVE GS-NOME-REG    TO NOME-REG-REL
                    WRITE REG-RELAT FROM CAB02B AFTER 2
                    ADD 2 TO LIN
             WHEN 4 MOVE GS-CONTRATO    TO CONTRATO-REL
                    MOVE GS-DESC-CONTRATO TO DESC-CONTRATO-REL
                    WRITE REG-RELAT FROM CAB02C AFTER 2
                    ADD 2 TO LIN
           END-EVALUATE
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           ADD 3 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 CAD012 COD003 COD040 RED100 RED101 WORK WORK1.
           DELETE FILE WORK WORK1.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
