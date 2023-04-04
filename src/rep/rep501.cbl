       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP501.
       DATE-WRITTEN. 15/03/2000.
       AUTHOR. MARELI AMÂNCIO VOLPATO
      *PROGRAMA: DEFINICAO QUALITATIVA DA EQUIPE
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
           COPY CGPX002.
           COPY CGPX005.
           COPY COPX003.
           COPY COPX040.
           COPY COPX060.
           COPY REPX002.
           COPY REPX009.
           COPY REPX501.
           COPY REPX502.
      *    SELECT WORK ASSIGN TO VARIA-W
      *           ORGANIZATION IS INDEXED
      *           ACCESS MODE IS DYNAMIC
      *           STATUS IS ST-WORK
      *           RECORD KEY IS SEQ-WK
      *           ALTERNATE RECORD KEY IS ALT-WK = CIDADE-WK CONTRATO-WK
      *                      WITH DUPLICATES.

      *    SELECT WORK1 ASSIGN TO VARIA-W1
      *           ORGANIZATION IS INDEXED
      *           ACCESS MODE IS DYNAMIC
      *           STATUS IS ST-WORK1
      *           RECORD KEY IS NOME-CIDADE-WK1.

           SELECT WORK2 ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS CODIGO-WK2
                  ALTERNATE RECORD KEY IS NOME-WK2 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK2 =
                         CIDADE-WK2 NOME-WK2 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK2 = NOME-FUNCAO-WK2
                         NOME-WK2 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-WK2 = PONTOS-WK2
                         NOME-WK2 WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CGPW001.
       COPY CGPW002.
       COPY CGPW005.
       COPY COPW003.
       COPY COPW040.
       COPY COPW060.
       COPY REPW002.
       COPY REPW009.
       COPY REPW501.
       COPY REPW502.

      *FD  WORK.
      *01  REG-WORK.
      *    05  SEQ-WK              PIC 9(3).
      *    05  DATA-WK             PIC 9(8).
      *    05  CONTRATO-WK         PIC 9(4).
      *    05  CIDADE-WK           PIC 9(4).
      *    05  FORM-WK             PIC 9(4).
      *    05  PADRAO-WK           PIC X.
      *    05  TELAO-WK            PIC 9.
      *    05  STUDIO-WK           PIC 9.
      *    05  VIDEO-WK            PIC 9.
      *    05  EVENTO-PRE-WK       PIC 9.
      *    1-EVENTO  2-PRE-EVENTO
      *FD  WORK1.
      *01  REG-WORK1.
      *    05  CIDADE-WK1          PIC 9(4).
      *    05  NOME-CIDADE-WK1     PIC X(13).
      *    05  FOTOG-WK1           PIC 9(3).
      *    05  COORD-WK1           PIC 9(3).
      *    05  CINEG-WK1           PIC 9(3).
      *    05  AUXIL-WK1           PIC 9(3).
      *    05  TELAO-WK1           PIC 9(2).
      *    05  STUDIO-WK1          PIC 9(2).
      *    05  VEICULO-WK1         PIC 9(2).
       FD  WORK2.
       01  REG-WORK2.
           05  CODIGO-WK2          PIC 9(6).
           05  NOME-WK2            PIC X(30).
           05  FUNCAO-WK2          PIC 9(2).
           05  NOME-FUNCAO-WK2     PIC X(6).
           05  QT-SELECAO-WK2      PIC 9.
           05  CIDADE-WK2          PIC X(13).
           05  PONTOS-WK2          PIC 9(3).
           05  PADRAO-WK2          PIC X.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(230).
       WORKING-STORAGE SECTION.
           COPY "REP501.CPB".
           COPY "REP501.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD002             PIC XX       VALUE SPACES.
           05  ST-CGD005             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-RED002             PIC XX       VALUE SPACES.
           05  ST-RED009             PIC XX       VALUE SPACES.
           05  ST-RED501             PIC XX       VALUE SPACES.
           05  ST-RED502             PIC XX       VALUE SPACES.
      *    05  ST-WORK               PIC XX       VALUE SPACES.
      *    05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
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
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  CODIGO-W              PIC 9(6)     VALUE ZEROS.
           05  COORD-W               PIC 9(3)     VALUE ZEROS.
           05  FOTOG-W               PIC 9(3)     VALUE ZEROS.
           05  CINEG-W               PIC 9(3)     VALUE ZEROS.
           05  AUXIL-W               PIC 9(3)     VALUE ZEROS.
           05  TELAO-W               PIC 9(3)     VALUE ZEROS.
           05  STUDIO-W              PIC 9(3)     VALUE ZEROS.
           05  VEICULO-W             PIC 9(3)     VALUE ZEROS.
           05  CIDADE-SELEC          PIC 9(4)     VALUE ZEROS.
           05  COORD-ULT             PIC 9(3)     VALUE ZEROS.
           05  FOTOG-ULT             PIC 9(3)     VALUE ZEROS.
           05  CINEG-ULT             PIC 9(3)     VALUE ZEROS.
           05  AUXIL-ULT             PIC 9(3)     VALUE ZEROS.
           05  TELAO-ULT             PIC 9(3)     VALUE ZEROS.
           05  STUDIO-ULT            PIC 9(3)     VALUE ZEROS.
           05  VEICULO-ULT           PIC 9(3)     VALUE ZEROS.
           05  CIDADE-ANT            PIC 9(4)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
      *    CIDADE-ANT - CONTROLA QUAL A ULTIMA CIDADE UTILIZADA

      *    VARIAVEIS P/ CALCULAR A QTDE DE FOTOG/CINEG/FORM
           05  QTDE-W                PIC 9(8)     VALUE ZEROS.
           05  QTDE-E                PIC ZZ.ZZZ.ZZZ.
           05  QT-EQ-E               PIC ZZZ      BLANK WHEN ZEROS.
           05  LINDET-W              PIC X(60)    VALUE SPACES.
           05  OPCAO-SEMANA           PIC 9        VALUE ZEROS.
           05  DATA-E                 PIC ZZ/ZZ/ZZZZ.
           05  I                      PIC 999      VALUE ZEROS.

           05  DATA-SEG               PIC 9(8)     VALUE ZEROS.
           05  DATA-DOM               PIC 9(8)     VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(38)  VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(78)   VALUE
           "DEFINICAO QUANTITATIVA DE EQUIPE".
       01  CAB03.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(78)   VALUE ALL "=".
       01  TRACO.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(78)   VALUE ALL "-".
       01  CAB04.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  CABECALHO-REL       PIC X(78)   VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(78)   VALUE
           "CIDADE        COO FOT CIN AUX TEL STU VEI".
       01  LINDET.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  LINDET-REL          PIC X(78)   VALUE SPACES.

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
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD002.
           MOVE "CGD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD005.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "RED002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED002.
           MOVE "RED009"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED009.
           MOVE "RED501"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED501.
           MOVE "RED502"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED502.
      *    ACCEPT VARIA-W FROM TIME.
      *    OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.

           OPEN INPUT CAD010 COD003 COD040 COD060 RED009 CGD001 CGD005
                      RED002 CGD002 RED502.

      *    ACCEPT VARIA-W1 FROM TIME.
      *    OPEN OUTPUT WORK1. CLOSE WORK1. OPEN I-O WORK1.

           OPEN I-O RED501.
           IF ST-RED501 = "35"
             CLOSE RED501      OPEN OUTPUT RED501    CLOSE RED501
             OPEN I-O RED501.

           ACCEPT VARIA-W2 FROM TIME.
           OPEN OUTPUT WORK2. CLOSE WORK2. OPEN I-O WORK2.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD002 <> "00"
              MOVE "ERRO ABERTURA CGD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD005 <> "00"
              MOVE "ERRO ABERTURA CGD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED002 <> "00"
              MOVE "ERRO ABERTURA RED002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED009 <> "00"
              MOVE "ERRO ABERTURA RED009: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED009 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED501 <> "00"
              MOVE "ERRO ABERTURA RED501: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED501 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED502 <> "00"
              MOVE "ERRO ABERTURA RED502: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED502 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W IMPRESSORA-W
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
                    MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
      *             PERFORM GRAVA-WORK
      *             PERFORM GRAVA-WORK1
                    PERFORM GRAVA-WORK2
                    PERFORM CARREGA-LISTA
                    PERFORM CARREGA-EQUIPE
               WHEN GS-CARREGA-EQUIPE-TRUE
                    PERFORM CARREGA-EQUIPE
               WHEN GS-VERIFICA-DATA-TRUE
                    PERFORM VERIFICA-DATA
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM EQUIPE-SELECIONADA
               WHEN GS-CIDADE-SELECION-TRUE
                    PERFORM CIDADE-SELECIONADA
               WHEN GS-LE-FUNCAO-TRUE
                    PERFORM LER-FUNCAO
               WHEN GS-POPUP-FUNCAO-TRUE
                    PERFORM POPUP-FUNCAO
               WHEN GS-EXCLUI-EQUIP-SEL-TRUE
                    PERFORM EXCLUI-EQUIPE-SELECIONADA
               WHEN GS-VERIF-CID-EQU-TRUE
                    PERFORM VERIFICA-CIDADE-EQUIPE
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
      *-----------------------------------------------------------
       VERIFICA-DATA SECTION.
           MOVE GS-DATA   TO GRTIME-DATE.
           MOVE 1         TO GRTIME-TYPE.
           MOVE 8         TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           IF GRTIME-WEEK-NUM <> 2
              MOVE "ERRO-DATA-SEMANA" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
             PERFORM VERIFICA-DIA-SEMANA.
       VERIFICA-DIA-SEMANA SECTION.
      *    memoriza os 7 dias da semana(iniciando pela segunda)
           MOVE GS-DATA   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV  TO DATA-SEG

           MOVE DATA-SEG   TO GRTIME-DATE
           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL TO DATA-DOM.

       CHAMA-GRTIME-ADAY SECTION.
           MOVE 2         TO GRTIME-TYPE
           MOVE 1         TO GRTIME-FUNCTION
           MOVE 6         TO GRTIME-DAYS
           CALL "GRTIME"  USING PARAMETROS-GRTIME
           CANCEL "GRTIME".
      *------------------------------------------------------------
       LER-FUNCAO SECTION.
           MOVE GS-FUNCAO  TO CODIGO-RE02.
           READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02.
           MOVE DESCRICAO-RE02  TO GS-DESC-FUNCAO.
       POPUP-FUNCAO SECTION.
           CALL   "REP002T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "REP002T".
           MOVE PASSAR-STRING-1(22: 2) TO GS-FUNCAO
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-FUNCAO.
      *----------------------------------------------------------
      *GRAVA-WORK SECTION.
      *    MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE ZEROS TO SEQ-WK.
      *    MOVE DATA-SEG TO DATAREALIZA-CO60.
      *    START COD060 KEY IS NOT < DATAREALIZA-CO60 INVALID KEY
      *          MOVE "10" TO ST-COD060.
      *    PERFORM UNTIL ST-COD060 = "10"
      *      READ COD060 NEXT RECORD AT END MOVE "10" TO ST-COD060
      *        NOT AT END
      *          MOVE DATAREALIZA-CO60 TO GS-EXIBE-VENCTO
      *          MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
      *          PERFORM CALL-DIALOG-SYSTEM
      *          IF DATAREALIZA-CO60 > DATA-DOM MOVE "10" TO ST-COD060
      *          ELSE
      *             MOVE DATAREALIZA-CO60 TO DATA-WK
      *             MOVE NR-CONTRATO-CO60 TO NR-CONTRATO-CO40
      *                                      CONTRATO-WK
      *             READ COD040 INVALID KEY MOVE ZEROS TO CIDADE-CO40
      *             END-READ
      *             MOVE CIDADE-CO40      TO CIDADE-WK
      *             MOVE QT-PARTICIPANTE-CO60 TO FORM-WK
      *             MOVE PADRAO-CO40      TO PADRAO-WK
      *             IF VIDEO-CO60 = 1
      *                  MOVE 1           TO VIDEO-WK
      *             ELSE MOVE ZEROS       TO VIDEO-WK
      *             END-IF
      *             MOVE QT-TELAO-CO60    TO TELAO-WK
      *             MOVE ZEROS            TO STUDIO-WK
      *             MOVE CODEVENTO-CO60   TO CODIGO-CO03
      *             READ COD003 INVALID KEY MOVE 1 TO EVENTO-PRE-CO03
      *             END-READ
      *             MOVE EVENTO-PRE-CO03  TO EVENTO-PRE-WK
      *             ADD 1 TO SEQ-WK
      *             WRITE REG-WORK
      *             END-WRITE
      *          END-IF
      *      END-READ
      *    END-PERFORM.

      *GRAVA-WORK1 SECTION.
      *    SE MESMA CIDADE E CONTRATO CONSIDERE A QTDE MAIOR
      *    SE MESMA CIDADE E CONTRATOS DIFERENTES SOMA-SE AS QTDES
      *    MOVE ZEROS TO COORD-ULT FOTOG-ULT CINEG-ULT AUXIL-ULT
      *                  TELAO-ULT STUDIO-ULT VEICULO-ULT.
      *    MOVE ZEROS TO CIDADE-WK CONTRATO-WK.
      *
      *    MOVE ZEROS TO CIDADE-ANT CONTRATO-ANT.
      *    START WORK KEY IS NOT < ALT-WK INVALID KEY
      *          MOVE "10" TO ST-WORK.
      *    PERFORM UNTIL ST-WORK = "10"
      *      READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
      *        NOT AT END
      *         MOVE CIDADE-WK TO GS-EXIBE-VENCTO
      *         MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
      *         PERFORM CALL-DIALOG-SYSTEM
      *         IF CIDADE-WK = CIDADE-ANT AND CONTRATO-WK = CONTRATO-ANT
      *            PERFORM VERIFICA-QTDE-EQUIPE
      *            IF COORD-W > COORD-ULT MOVE COORD-W TO COORD-ULT
      *            END-IF
      *            IF FOTOG-W > FOTOG-ULT MOVE FOTOG-W TO FOTOG-ULT
      *            END-IF
      *            IF CINEG-W > CINEG-ULT MOVE CINEG-W TO CINEG-ULT
      *            END-IF
      *            IF AUXIL-W > AUXIL-ULT MOVE AUXIL-W TO AUXIL-ULT
      *            END-IF
      *            IF TELAO-WK > TELAO-ULT MOVE TELAO-WK TO TELAO-ULT
      *            END-IF
      *            IF STUDIO-WK > STUDIO-ULT
      *               MOVE STUDIO-WK TO STUDIO-ULT
      *            END-IF
      *         ELSE PERFORM CONT-GRAVA-WORK1
      *              MOVE CIDADE-WK     TO CIDADE-ANT
      *              MOVE CONTRATO-WK   TO CONTRATO-ANT
      *         END-IF
      *      END-READ
      *    END-PERFORM.
      *CONT-GRAVA-WORK1 SECTION.
      *    MOVE CIDADE-WK  TO CIDADE CIDADE-WK1
      *    READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
      *    MOVE NOME-CID   TO NOME-CIDADE-WK1
      *    READ WORK1
      *      INVALID KEY
      *         MOVE COORD-ULT   TO COORD-WK1
      *         MOVE FOTOG-ULT   TO FOTOG-WK1
      *         MOVE CINEG-ULT   TO CINEG-WK1
      *         MOVE AUXIL-ULT   TO AUXIL-WK1
      *         MOVE TELAO-ULT   TO TELAO-WK1
      *         MOVE STUDIO-ULT  TO STUDIO-WK1
      *         MOVE ZEROS       TO VEICULO-WK1
      *         WRITE REG-WORK1
      *         END-WRITE
      *      NOT INVALID KEY
      *         ADD COORD-ULT   TO COORD-WK1
      *         ADD FOTOG-ULT   TO FOTOG-WK1
      *         ADD CINEG-ULT   TO CINEG-WK1
      *         ADD AUXIL-ULT   TO AUXIL-WK1
      *         ADD TELAO-ULT   TO TELAO-WK1
      *         ADD STUDIO-ULT  TO STUDIO-WK1
      *         ADD ZEROS       TO VEICULO-WK1
      *         REWRITE REG-WORK1
      *         END-REWRITE
      *    END-READ.
      *    MOVE ZEROS TO COORD-ULT CINEG-ULT FOTOG-ULT TELAO-ULT
      *                  STUDIO-ULT AUXIL-ULT.
      *VERIFICA-QTDE-EQUIPE SECTION.
      *    MOVE ZEROS TO FOTOG-W CINEG-W COORD-W AUXIL-W.
      *    MOVE PADRAO-WK      TO PADRAO-RE09.
      *    START RED009 KEY IS NOT < PADRAO-RE09 INVALID KEY
      *          MOVE "10" TO ST-RED009.
      *    PERFORM UNTIL ST-RED009 = "10"
      *      READ RED009 NEXT RECORD AT END MOVE "10" TO ST-RED009
      *        NOT AT END
      *          IF PADRAO-WK <> PADRAO-WK MOVE "10" TO ST-RED009
      *          ELSE
      *             IF FORM-WK NOT < FORM-MIN-RE09 AND
      *                FORM-WK NOT > FORM-MAX-RE09
      *                  IF EVENTO-PRE-WK = 1
      **                     IGUAL A EVENTO
      *                     MOVE FOTOG-EVE-RE09   TO FOTOG-W
      *                     MOVE COORD-EVE-RE09   TO COORD-W
      *                     MOVE CINEG-EVE-RE09   TO CINEG-W
      *                     MOVE AUXIL-EVE-RE09   TO AUXIL-W
      *                     MOVE STUDIO-EVE-RE09  TO STUDIO-W
      *                  ELSE
      **                     IGUAL A PRE-EVENTO
      *                     MOVE FOTOG-PRE-RE09   TO FOTOG-W
      *                     MOVE COORD-PRE-RE09   TO COORD-W
      *                     MOVE CINEG-PRE-RE09   TO CINEG-W
      *                     MOVE AUXIL-PRE-RE09   TO AUXIL-W
      *                     MOVE STUDIO-PRE-RE09  TO STUDIO-W
      *                  END-IF
      *             END-IF
      *          END-IF
      *      END-READ
      *    END-PERFORM.
       GRAVA-WORK2 SECTION.
      *    GRAVACAO DE EQUIPE A SER ESCALADA CONFORME FUNCAO
           MOVE ZEROS TO CODIGO-CG01.
           START CGD001 KEY IS NOT < CODIGO-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
             READ CGD001 NEXT RECORD AT END MOVE "10" TO ST-CGD001
               NOT AT END
                 IF T-FOTOG-CG01 = ZEROS AND
                    T-CINEG-CG01 = ZEROS CONTINUE
                 ELSE
                  MOVE CODIGO-CG01      TO GS-EXIBE-VENCTO
                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  MOVE CODIGO-CG01       TO CODIGO-WK2 CODIGO-CG05
                                            CODIGO-RE501
                  READ CGD005 INVALID KEY MOVE ZEROS TO FUNCAO-CG05
                  END-READ
                  MOVE FUNCAO-CG05       TO FUNCAO-WK2 CODIGO-RE02
                  READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02
                  END-READ
                  MOVE DESCRICAO-RE02    TO NOME-FUNCAO-WK2
                  MOVE ZEROS             TO PONTOS-WK2
                  MOVE NOME-CG01         TO NOME-WK2
                  MOVE CODIGO-CG01       TO CODIGO-RE501 CODIGO-CG02
                  READ CGD002 INVALID KEY MOVE ZEROS TO CIDADE1-CG02
                  END-READ
                  MOVE PADRAO-CG05       TO PADRAO-WK2
                  MOVE CIDADE1-CG02      TO CIDADE
                  READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
                  END-READ
                  MOVE NOME-CID          TO CIDADE-WK2
                  MOVE DATA-SEG          TO DATA-SEG-RE501
                  START RED501 KEY IS NOT < ALT-RE501 INVALID KEY
                        MOVE "10" TO ST-RED501
                  END-START
                  MOVE ZEROS TO QT-SELECAO-WK2
                  PERFORM UNTIL ST-RED501 = "10"
                    READ RED501 NEXT RECORD AT END
                         MOVE "10" TO ST-RED501
                     NOT AT END
                      IF CODIGO-RE501 <> CODIGO-CG01
                         MOVE "10" TO ST-RED501
                      ELSE
                         ADD 1 TO QT-SELECAO-WK2
                      END-IF
                    END-READ
                  END-PERFORM
                  WRITE REG-WORK2
                  END-WRITE
                 END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------------
       CIDADE-SELECIONADA SECTION.
           MOVE GS-LINDET(50: 4)    TO CIDADE-SELEC CIDADE.
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID           TO GS-CIDADE.
           PERFORM CARREGA-EQUIPE-SELECIONADA.

       VERIFICA-CIDADE-EQUIPE SECTION.
      * VERIFICAR SE PESSOA JÁ ESTA SELECIONADA P/ OUTRA CIDADE
           MOVE GS-LINDET(65: 6) TO CODIGO-RE501 CODIGO-W
           MOVE DATA-SEG         TO DATA-SEG-RE501
           START RED501 KEY IS NOT < ALT-RE501 INVALID KEY
                   MOVE 0 TO GS-CID-EQU-DUPL
             NOT INVALID KEY
              READ RED501 NEXT RECORD
              END-READ
               IF CODIGO-RE501 = CODIGO-W AND DATA-SEG-RE501 = DATA-SEG
                  MOVE CIDADE-RE501 TO CIDADE
                  READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
                  END-READ
                  MOVE "Pessoa já cadastrada em " TO GS-MENSAGEM
                  MOVE NOME-CID                   TO GS-MENSAGEM(25: 14)
                  MOVE "-"                        TO GS-MENSAGEM(40: 1)
                  MOVE "CONTINUAR? "              TO GS-MENSAGEM(41: 12)
                  MOVE 1 TO GS-CID-EQU-DUPL
               ELSE MOVE 0 TO GS-CID-EQU-DUPL
               END-IF
           END-START.
      *----------------------------------------------------------
       CARREGA-EQUIPE SECTION.
      * EQUIPE DISPONIVEL A SER ESCALADA
           MOVE ZEROS TO GS-CONT
           MOVE SPACES TO GS-LINDET.
           MOVE "CLEAR-LIST-EQUIPE" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM ORDEM-EQUIPE.
           PERFORM UNTIL ST-WORK2 = "10"
             READ WORK2 NEXT RECORD AT END MOVE "10" TO ST-WORK2
               NOT AT END
                   MOVE SPACES            TO GS-LINDET
                   MOVE NOME-WK2          TO GS-LINDET(1: 31)
                   MOVE QT-SELECAO-WK2    TO GS-LINDET(32: 2)
                   MOVE NOME-FUNCAO-WK2   TO GS-LINDET(34: 7)
                   MOVE PONTOS-WK2        TO GS-LINDET(41: 4)
                   MOVE CIDADE-WK2        TO GS-LINDET(45: 8)
                   MOVE PADRAO-WK2        TO GS-LINDET(55: 1)
                   MOVE CODIGO-WK2        TO GS-LINDET(65: 6)
                   MOVE "INSERE-LIST-EQUIPE" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
             END-READ
           END-PERFORM.
       ORDEM-EQUIPE SECTION.
           INITIALIZE REG-WORK2.
           EVALUATE GS-ORDEM
            WHEN 1 START WORK2 KEY IS NOT < NOME-WK2 INVALID KEY
                      MOVE "10" TO ST-WORK2
                   END-START
            WHEN 2 START WORK2 KEY IS NOT < ALT2-WK2 INVALID KEY
                      MOVE "10" TO ST-WORK2
                   END-START
            WHEN 3 START WORK2 KEY IS NOT < ALT3-WK2 INVALID KEY
                      MOVE "10" TO ST-WORK2
                   END-START
            WHEN 4 START WORK2 KEY IS NOT < ALT1-WK2 INVALID KEY
                      MOVE "10" TO ST-WORK2
                   END-START
           END-EVALUATE.
       CARREGA-EQUIPE-SELECIONADA SECTION.
           MOVE ZEROS TO GS-CONT1
           MOVE "CLEAR-LIST-EQUIPE-SEL" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS  TO GS-CONT1.
           MOVE CIDADE-SELEC TO CIDADE-RE501.
           MOVE DATA-SEG     TO DATA-SEG-RE501.
           MOVE ZEROS  TO FUNCAO-RE501 CODIGO-RE501.
           START RED501 KEY IS NOT < CHAVE-RE501 INVALID KEY
                 MOVE "10" TO ST-RED501.
           PERFORM UNTIL ST-RED501 = "10"
             READ RED501 NEXT RECORD AT END MOVE "10" TO ST-RED501
               NOT AT END
                 IF CIDADE-RE501 <> CIDADE-SELEC OR
                    DATA-SEG-RE501 <> DATA-SEG MOVE "10" TO ST-RED501
                 ELSE
                  MOVE SPACES            TO GS-LINDET1
                  MOVE CODIGO-RE501      TO CODIGO-CG01
                  READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                  END-READ
                  MOVE NOME-CG01         TO GS-LINDET1(1: 30)
                  MOVE CODIGO-CG01       TO GS-LINDET1(33: 6)
                  MOVE FUNCAO-RE501      TO GS-LINDET1(41: 2)
                                            CODIGO-RE02
                  READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02
                  END-READ
                  MOVE "-"               TO GS-LINDET1(43: 1)
                  MOVE DESCRICAO-RE02(1: 3) TO GS-LINDET1(44: 3)
                  MOVE "INSERE-LIST-SELECIONADA" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                 END-IF
             END-READ
           END-PERFORM.

       EQUIPE-SELECIONADA SECTION.
      *  INCLUSAO
           MOVE GS-LINDET(1: 30) TO GS-LINDET1(1: 31)
           MOVE GS-LINDET(65: 6) TO GS-LINDET1(33: 6)
           MOVE GS-FUNCAO        TO GS-LINDET1(41: 2)
           MOVE "-"               TO GS-LINDET1(43: 1)
           MOVE GS-DESC-FUNCAO(1: 3) TO GS-LINDET1(44: 3)
           MOVE GS-LINDET(65: 6) TO CODIGO-RE501 CODIGO-WK2
           READ WORK2 INVALID KEY CONTINUE
             NOT INVALID KEY
               ADD 1 TO QT-SELECAO-WK2
               REWRITE REG-WORK2
               END-REWRITE
               MOVE QT-SELECAO-WK2   TO GS-LINDET(32: 2)
           END-READ.
           MOVE CIDADE-SELEC     TO CIDADE-RE501
           MOVE GS-FUNCAO        TO FUNCAO-RE501
           MOVE DATA-SEG         TO DATA-SEG-RE501
           MOVE GS-LINDET(55: 1) TO PADRAO-RE501
           WRITE REG-RED501
           END-WRITE
           MOVE "INSERE-LIST-SELECIONADA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       EXCLUI-EQUIPE-SELECIONADA SECTION.
           MOVE GS-LINDET1(33: 6)  TO CODIGO-RE501 CODIGO-WK2.
           READ WORK2 INVALID KEY CONTINUE
             NOT INVALID KEY SUBTRACT 1 FROM QT-SELECAO-WK2
                             REWRITE REG-WORK2
                             END-REWRITE
           END-READ.
           MOVE GS-LINDET1(41: 2)  TO FUNCAO-RE501.
           MOVE CIDADE-SELEC       TO CIDADE-RE501.
           MOVE DATA-SEG           TO DATA-SEG-RE501.
           READ RED501 INVALID KEY CONTINUE
             NOT INVALID KEY DELETE RED501.
      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
      *  LISTA DO CRONOGRAMA
           MOVE ZEROS TO COORD-W FOTOG-W CINEG-W AUXIL-W STUDIO-W
                         TELAO-W VEICULO-W.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE DATA-SEG TO DATA-RE502
           MOVE SPACES TO NOME-CIDADE-RE502
           START RED502 KEY IS NOT < ALT1-RE502 INVALID KEY
                 MOVE "10" TO ST-RED502.
           PERFORM UNTIL ST-RED502 = "10"
             READ RED502 NEXT RECORD AT END MOVE "10" TO ST-RED502
               NOT AT END
                IF DATA-RE502 <> DATA-SEG MOVE "10" TO ST-RED502
                ELSE
                 MOVE NOME-CIDADE-RE502     TO GS-LINDET(1: 15)
                 MOVE COORD-RE502           TO QT-EQ-E
                 MOVE QT-EQ-E               TO GS-LINDET(16: 4)
                 ADD COORD-RE502            TO COORD-W
                 MOVE FOTOG-RE502           TO QT-EQ-E
                 MOVE QT-EQ-E               TO GS-LINDET(20: 4)
                 ADD FOTOG-RE502            TO FOTOG-W
                 MOVE CINEG-RE502           TO QT-EQ-E
                 MOVE QT-EQ-E               TO GS-LINDET(24: 4)
                 ADD CINEG-RE502            TO CINEG-W
                 MOVE AUXIL-RE502           TO QT-EQ-E
                 MOVE QT-EQ-E               TO GS-LINDET(28: 4)
                 ADD AUXIL-RE502            TO AUXIL-W
                 MOVE TELAO-RE502           TO QT-EQ-E
                 MOVE QT-EQ-E               TO GS-LINDET(32: 4)
                 ADD TELAO-RE502            TO TELAO-W
                 MOVE STUDIO-RE502          TO QT-EQ-E
                 MOVE QT-EQ-E               TO GS-LINDET(36: 4)
                 ADD STUDIO-RE502           TO STUDIO-W
                 MOVE VEICULO-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E               TO GS-LINDET(40: 4)
                 ADD VEICULO-RE502          TO VEICULO-W
                 MOVE CIDADE-RE502          TO GS-LINDET(50: 4)

                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                END-IF
             END-READ
           END-PERFORM.
           PERFORM TOTALIZA.

       TOTALIZA SECTION.
           MOVE "TOTAL         "    TO GS-LINTOT(1: 15)
           MOVE COORD-W             TO GS-LINTOT(16: 4)
           MOVE FOTOG-W             TO GS-LINTOT(20: 4)
           MOVE CINEG-W             TO GS-LINTOT(24: 4)
           MOVE AUXIL-W             TO GS-LINTOT(28: 4)
           MOVE TELAO-W             TO GS-LINTOT(32: 4)
           MOVE STUDIO-W            TO GS-LINTOT(36: 4)
           MOVE VEICULO-W           TO GS-LINTOT(40: 4)

           MOVE "INSERE-LINTOT" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP501" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           IF GS-IMPR-CRON-TRUE
              PERFORM IMPRIME-CRONOGRAMA.

           IF GS-IMPR-EQU-NOM-TRUE
              PERFORM IMPRIME-EQUIPE-NOME.

           IF GS-IMPR-EQU-CID-TRUE
              PERFORM IMPRIME-EQUIPE-CIDADE.

           copy descondensa.

       IMPRIME-CRONOGRAMA SECTION.
           MOVE ZEROS TO PAG-W LIN.
           PERFORM CABECALHO.
           MOVE "CRONOGRAMA DA CIDADE         "  TO CABECALHO-REL.
           WRITE REG-RELAT FROM CAB04 AFTER 2.
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           ADD 5 TO LIN.
           MOVE SPACES TO LINDET-REL.
           MOVE SPACES TO NOME-CIDADE-RE502
           MOVE DATA-SEG  TO DATA-RE502
           START RED502 KEY IS NOT < ALT1-RE502 INVALID KEY
                 MOVE "10" TO ST-RED502.
           PERFORM UNTIL ST-RED502 = "10"
             READ RED502 NEXT RECORD AT END MOVE "10" TO ST-RED502
               NOT AT END
                IF DATA-RE502 <> DATA-SEG MOVE "10" TO ST-RED502
                ELSE
                 MOVE NOME-CIDADE-RE502   TO LINDET-REL(1: 15)
                 MOVE COORD-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E             TO LINDET-REL(16: 4)
                 MOVE FOTOG-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E             TO LINDET-REL(20: 4)
                 MOVE CINEG-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E             TO LINDET-REL(24: 4)
                 MOVE AUXIL-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E             TO LINDET-REL(28: 4)
                 MOVE TELAO-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E             TO LINDET-REL(32: 4)
                 MOVE STUDIO-RE502        TO QT-EQ-E
                 MOVE QT-EQ-E             TO LINDET-REL(36: 4)
                 MOVE VEICULO-RE502       TO QT-EQ-E
                 MOVE QT-EQ-E             TO LINDET-REL(40: 4)
                 WRITE REG-RELAT FROM LINDET
                 ADD 1 TO LIN
                 IF LIN > 56 PERFORM CABECALHO
                 END-IF
                END-IF
             END-READ
           END-PERFORM.
           WRITE REG-RELAT FROM TRACO.
           MOVE SPACES              TO LINDET-REL
           MOVE "TOTAL         "    TO LINDET-REL(1: 15)
           MOVE COORD-W             TO LINDET-REL(16: 4)
           MOVE FOTOG-W             TO LINDET-REL(20: 4)
           MOVE CINEG-W             TO LINDET-REL(24: 4)
           MOVE AUXIL-W             TO LINDET-REL(28: 4)
           MOVE TELAO-W             TO LINDET-REL(32: 4)
           MOVE STUDIO-W            TO LINDET-REL(36: 4)
           MOVE VEICULO-W           TO LINDET-REL(40: 4)
           WRITE REG-RELAT FROM LINDET.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
       IMPRIME-EQUIPE-NOME SECTION.
           MOVE ZEROS TO PAG-W LIN.
           PERFORM CABECALHO.
           MOVE "EQUIPE SELECIONA POR PESSOAL  "  TO CABECALHO-REL.
           WRITE REG-RELAT FROM CAB04 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           ADD 3 TO LIN.

           MOVE SPACES         TO LINDET-REL
           MOVE ZEROS          TO CODIGO-RE501
           MOVE DATA-SEG       TO DATA-SEG-RE501.
           START RED501 KEY IS NOT < ALT1-RE501 INVALID KEY
                 MOVE "10" TO ST-RED501.
           PERFORM UNTIL ST-RED501 = "10"
             READ RED501 NEXT RECORD AT END MOVE "10" TO ST-RED501
               NOT AT END
                 IF DATA-SEG-RE501 > DATA-SEG MOVE "10" TO ST-RED501
                 ELSE
                   MOVE CODIGO-RE501    TO LINDET-REL(1: 7) CODIGO-CG01
                   READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01       TO LINDET-REL(8: 33)
                   MOVE FUNCAO-RE501    TO LINDET-REL(41: 2) CODIGO-RE02
                   MOVE "-"             TO LINDET-REL(43: 1)
                   READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02
                   END-READ
                   MOVE DESCRICAO-RE02  TO LINDET-REL(44: 3)
                   MOVE CIDADE-RE501    TO CIDADE LINDET-REL(49: 5)
                   READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
                   END-READ
                   MOVE NOME-CID        TO LINDET-REL(54: 13)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.


           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.

       IMPRIME-EQUIPE-CIDADE SECTION.
           MOVE ZEROS TO PAG-W LIN.
           PERFORM CABECALHO.
           MOVE "EQUIPE SELECIONA POR CIDADE   "  TO CABECALHO-REL.
           WRITE REG-RELAT FROM CAB04 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           ADD 3 TO LIN.

           MOVE SPACES         TO LINDET-REL
           MOVE ZEROS          TO CIDADE-RE501
           MOVE DATA-SEG       TO DATA-SEG-RE501.
           START RED501 KEY IS NOT < ALT2-RE501 INVALID KEY
                 MOVE "10" TO ST-RED501.
           PERFORM UNTIL ST-RED501 = "10"
             READ RED501 NEXT RECORD AT END MOVE "10" TO ST-RED501
               NOT AT END
                 IF DATA-SEG-RE501 > DATA-SEG MOVE "10" TO ST-RED501
                 ELSE
                   MOVE CIDADE-RE501    TO CIDADE LINDET-REL(1: 5)
                   READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
                   END-READ
                   MOVE NOME-CID        TO LINDET-REL(6: 13)
                   MOVE CODIGO-RE501    TO LINDET-REL(18: 7) CODIGO-CG01
                   READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01       TO LINDET-REL(25: 33)
                   MOVE FUNCAO-RE501    TO LINDET-REL(58: 2) CODIGO-RE02
                   MOVE "-"             TO LINDET-REL(60: 1)
                   READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02
                   END-READ
                   MOVE DESCRICAO-RE02  TO LINDET-REL(61: 3)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.


           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD003 COD040 COD060 RED009 WORK2 RED502
                 CGD001 CGD005 RED501 RED002 CGD002.
      *    CLOSE WORK WORK1.
           DELETE FILE WORK2.
      *    DELETE FILE WORK WORK1.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
