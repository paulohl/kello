       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP301A.
      *DATA: 06/09/2003
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RELAÇÃO DE FITAS DE VÍDEO
      *FUNÇÃO: Listar todos as FITAS EXISTENTES NO INTERVALO SOLICITADO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CGPX001.
           COPY COPX003.
           COPY COPX040.
           COPY VIPX020.
           COPY VIPX021.
           COPY VIPX022.
           COPY VIPX100.
           COPY VIPX101.
           COPY VIPX102.
           COPY VIPX103.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS WK-SEQ
                  ALTERNATE RECORD KEY IS ALT1-WK = WK-CONTRATO
                               WK-NR-FITA WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK = WK-EVENTO
                               WK-NR-FITA WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-WK = WK-NR-FITA
                                          WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-WK = WK-CINEGRAFISTA
                               WK-NR-FITA WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT5-WK = WK-AVALIACAO
                               WK-NR-FITA WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-WK = WK-MOMENTO
                               WK-NR-FITA WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT7-WK = WK-PROBLEMA
                               WK-NR-FITA WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CGPW001.
       COPY COPW003.
       COPY COPW040.
       COPY VIPW020.
       COPY VIPW021.
       COPY VIPW022.
       COPY VIPW100.
       COPY VIPW101.
       COPY VIPW102.
       COPY VIPW103.

       FD  WORK.
       01  REG-WORK.
           05  WK-SEQ              PIC 9(4).
           05  WK-CONTRATO         PIC 9(04).
           05  WK-CURSO            PIC X(10).
           05  WK-CIDADE           PIC X(10).
           05  WK-EVENTO           PIC X(10).
           05  WK-NR-FITA          PIC 9(5).
           05  WK-CINEGRAFISTA     PIC X(10).
           05  WK-AVALIACAO        PIC X(10).
           05  WK-MOMENTO          PIC X(20).
           05  WK-PROBLEMA         PIC X(25).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "VIP301A.CPB".
           COPY "VIP301A.CPY".
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
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-VID020             PIC XX       VALUE SPACES.
           05  ST-VID021             PIC XX       VALUE SPACES.
           05  ST-VID022             PIC XX       VALUE SPACES.
           05  ST-VID100             PIC XX       VALUE SPACES.
           05  ST-VID101             PIC XX       VALUE SPACES.
           05  ST-VID102             PIC XX       VALUE SPACES.
           05  ST-VID103             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.

           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
      *    TOTALIZA VARIAVEIS
           05  QTDE-E                PIC ZZZZ.ZZZ.
           05  FORM-E                PIC ZZZZ.
           05  SEQ-W                 PIC 9(04)    VALUE ZEROS.
           05  FLAG-CONTROL          PIC 9(01)    VALUE ZEROS.
           05  I                     PIC 9(3)     VALUE ZEROS.
           05  W                     PIC 9(3)     VALUE ZEROS.
           05  MAIOR-SEQ             PIC 9(3)     VALUE ZEROS.
           05  NR-FITA-W             PIC 9(5)     VALUE ZEROS.
           05  OBS-W                 PIC X(100)   VALUE SPACES.
           05  SEQUENCIAL OCCURS 100 TIMES.
               10  DATA-MOVTO        PIC 9(8) VALUE ZEROS.
               10  SEQ               PIC 9(3) VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(34)   VALUE
           "AVALIACAO DE CINEGRAFISTA ".
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "CONT CURSO      CIDADE     EVENTO     N.FIT CINEGRAF. AVALIA
      -    "C MOMENTO              PROBLEMA               ".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.


           copy impressora.cpy.

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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "VID020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-VID020.
           MOVE "VID021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-VID021.
           MOVE "VID022"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-VID022.
           MOVE "VID100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-VID100.
           MOVE "VID101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-VID101.
           MOVE "VID102"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-VID102.
           MOVE "VID103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-VID103.
           OPEN INPUT CGD001 COD003 COD040 CAD010 VID020 VID021 VID022
                      VID100 VID101 VID102 VID103.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-VID020 <> "00"
              MOVE "ERRO ABERTURA VID020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID021 <> "00"
              MOVE "ERRO ABERTURA VID021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID022 <> "00"
              MOVE "ERRO ABERTURA VID022: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID022 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID100 <> "00"
              MOVE "ERRO ABERTURA VID100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID101 <> "00"
              MOVE "ERRO ABERTURA VID101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID102 <> "00"
              MOVE "ERRO ABERTURA VID102: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID102 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID103 <> "00"
              MOVE "ERRO ABERTURA VID103: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID103 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.
      *----------------------------------------------------------
       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-CARREGA-SEQUENCIA-TRUE
                    PERFORM CARREGA-SEQUENCIA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-LER-CADASTRO-TRUE
                    PERFORM LER-CADASTRO
               WHEN GS-POPUP-TRUE
                    PERFORM LEITURA-POPUP
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

      *--------------------------------------------------------------
       LER-CADASTRO SECTION.
           EVALUATE GS-OPCAO-LER
             WHEN 1 MOVE GS-CINEGRAFISTA   TO CODIGO-CG01
                    READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                    END-READ
                    MOVE NOME-CG01         TO GS-NOME-CINEGRAF
             WHEN 2 MOVE GS-CONTRATO       TO NR-CONTRATO-CO40
                    READ COD040 INVALID KEY
                         MOVE SPACES TO IDENTIFICACAO-CO40
                    END-READ
                    MOVE IDENTIFICACAO-CO40 TO GS-DESCR-CONTRATO
           END-EVALUATE.
       LEITURA-POPUP SECTION.
           EVALUATE GS-OPCAO-POPUP
             WHEN 1 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-CINEGRAF
                    MOVE PASSAR-STRING-1(33: 6) TO GS-CINEGRAFISTA
             WHEN 2 CALL   "COP040T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO
           END-EVALUATE.
      *-------------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

      *------------------------------------------------------------
       CARREGA-SEQUENCIA SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT    WORK
           CLOSE          WORK
           OPEN I-O       WORK
           MOVE ZEROS         TO SEQ-W

           MOVE GS-DATA-INI   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV      TO DATA-INI
           MOVE GS-DATA-FIM   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV      TO DATA-FIM
           EVALUATE GS-ORDEM1
             WHEN 1 MOVE GS-CINEGRAFISTA  TO CINEGRAFISTA-V100
                    MOVE ZEROS        TO DATA-EVENTO-V100
                    START VID100 KEY IS NOT < ALT2-V100 INVALID KEY
                        MOVE "10" TO ST-VID100
                    END-START
             WHEN 2 MOVE GS-CONTRATO  TO CONTRATO-V100
                    MOVE ZEROS        TO DATA-EVENTO-V100
                    START VID100 KEY IS NOT < ALT1-V100 INVALID KEY
                        MOVE "10" TO ST-VID100
                    END-START
             WHEN 3 IF GS-CINEGRAFISTA > 0
                       MOVE GS-CINEGRAFISTA TO CINEGRAFISTA-V100
                       MOVE DATA-INI        TO DATA-EVENTO-V100
                       START VID100 KEY IS NOT < ALT2-V100 INVALID KEY
                             MOVE "10" TO ST-VID100
                       END-START
                    ELSE
                       MOVE DATA-INI        TO DATA-EVENTO-V100
                       START VID100 KEY IS NOT < ALT3-V100 INVALID KEY
                             MOVE "10" TO ST-VID100
                       END-START
                    END-IF
           END-EVALUATE.

           PERFORM UNTIL ST-VID100 = "10"
             READ VID100 NEXT RECORD AT END MOVE "10" TO ST-VID100
               NOT AT END
                 EVALUATE GS-ORDEM1
                   WHEN 1 IF CINEGRAFISTA-V100 <> GS-CINEGRAFISTA
                             MOVE "10" TO ST-VID100
                          ELSE
                             IF GS-CONTRATO = 0 OR GS-CONTRATO =
                                CONTRATO-V100
                                PERFORM GRAVAR-WORK
                             END-IF
                          END-IF
                   WHEN 2 IF CONTRATO-V100 <> GS-CONTRATO
                             MOVE "10" TO ST-VID100
                          ELSE PERFORM GRAVAR-WORK
                          END-IF
                   WHEN 3 IF DATA-EVENTO-V100 > DATA-FIM
                             MOVE "10" TO ST-VID100
                          ELSE
                             IF GS-CINEGRAFISTA = 0 OR GS-CINEGRAFISTA =
                                CINEGRAFISTA-V100
                                PERFORM GRAVAR-WORK
                             END-IF
                          END-IF
                 END-EVALUATE
             END-READ
           END-PERFORM.

           PERFORM CARREGA-LISTA.
       GRAVAR-WORK SECTION.
           INITIALIZE REG-WORK

           MOVE CONTRATO-V100     TO WK-CONTRATO
                                     NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE ZEROS        TO CIDADE-CO40.
           MOVE CURSO-V100        TO WK-CURSO
           MOVE CIDADE-CO40       TO CIDADE

           READ CAD010 INVALID KEY
                MOVE SPACES       TO NOME-CID.
           MOVE NOME-CID          TO WK-CIDADE

           MOVE EVENTO-V100       TO CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE SPACES       TO NOME-CO03.
           MOVE NOME-CO03         TO WK-EVENTO

           MOVE CINEGRAFISTA-V100 TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE SPACES       TO NOME-CG01.

           MOVE NOME-CG01         TO WK-CINEGRAFISTA

           MOVE NR-FITA-V100      TO WK-NR-FITA
                                     NR-FITA-V101

           READ VID101 INVALID KEY MOVE ZEROS TO AVALIACAO-GERAL-V101.

           EVALUATE AVALIACAO-GERAL-V101
             WHEN 1 MOVE "PESSIMA" TO WK-AVALIACAO
             WHEN 2 MOVE "RUIM   " TO WK-AVALIACAO
             WHEN 3 MOVE "REGULAR" TO WK-AVALIACAO
             WHEN 4 MOVE "BOA    " TO WK-AVALIACAO
             WHEN 5 MOVE "OTIMA  " TO WK-AVALIACAO
             WHEN OTHER
                    MOVE SPACES    TO WK-AVALIACAO
           END-EVALUATE

           INITIALIZE REG-VID102
           MOVE CONTRATO-V100     TO CONTRATO-V102
           MOVE NR-FITA-V100      TO NR-FITA-V102
           MOVE ZEROS             TO SEQ-V102.
           MOVE ZEROS             TO FLAG-CONTROL
           START VID102 KEY IS NOT < CHAVE-V102 INVALID KEY
                 MOVE "10" TO ST-VID102.
           PERFORM UNTIL ST-VID102 = "10"
                 READ VID102 NEXT RECORD AT END
                      MOVE "10" TO ST-VID102
                 NOT AT END
                     IF CONTRATO-V102 <> CONTRATO-V100 OR
                        NR-FITA-V102  <> NR-FITA-V100
                        MOVE "10" TO ST-VID102
                     ELSE
                       MOVE 1               TO FLAG-CONTROL
                       MOVE MOMENTO-V102    TO CODIGO-V22
                       READ VID022 INVALID KEY
                            MOVE SPACES TO DESCRICAO-V22
                       END-READ
                       MOVE DESCRICAO-V22   TO WK-MOMENTO
                       MOVE PROBLEMA-V102   TO CODIGO-V21
                       READ VID021 INVALID KEY
                            MOVE SPACES TO DESCRICAO-V21
                       END-READ
                       MOVE DESCRICAO-V21   TO WK-PROBLEMA

                       PERFORM GRAVAR-REGISTRO
                     END-IF
                 END-READ
           END-PERFORM.
           IF FLAG-CONTROL EQUAL ZEROS
              PERFORM GRAVAR-REGISTRO.

       GRAVAR-REGISTRO SECTION.
           ADD 1 TO SEQ-W
           MOVE SEQ-W                   TO WK-SEQ
           WRITE REG-WORK.

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


       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "CONTRATO      " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "EVENTO        " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT2-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "NR-FITA       " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT3-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "CINEGRAFISTA  " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT4-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "AVALIACAO     " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT5-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "MOMENTO       " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT6-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 7
                MOVE "PROBLEMA      " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT7-WK INVALID KEY
                      MOVE "10" TO ST-WORK

           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           MOVE WK-CONTRATO            TO GS-LINDET(1: 5)
           MOVE WK-CURSO               TO GS-LINDET(6: 11)
           MOVE WK-CIDADE              TO GS-LINDET(17: 11)
           MOVE WK-EVENTO              TO GS-LINDET(28: 11)
           MOVE WK-NR-FITA             TO GS-LINDET(39: 6)
           MOVE WK-CINEGRAFISTA        TO GS-LINDET(45: 11)
           MOVE WK-AVALIACAO           TO GS-LINDET(56: 8)
           MOVE WK-MOMENTO             TO GS-LINDET(64: 21)
           MOVE WK-PROBLEMA            TO GS-LINDET(85: 25).


      *MOVER-DADOS-OBS SECTION.
      *    MOVE 1 TO W.
      *    MOVE NR-FITA-V100         TO NR-FITA-V103.
      *    MOVE ZEROS                TO SEQ-V103.
      *    START VID103 KEY IS NOT < CHAVE-V103 INVALID KEY
      *          MOVE "10" TO ST-VID103.
      *    PERFORM UNTIL ST-VID103 = "10"
      *      READ VID103 NEXT RECORD AT END MOVE "10" TO ST-VID103
      *        NOT AT END
      *          IF NR-FITA-V103 <> NR-FITA-V100 MOVE "10" TO ST-VID103
      *          ELSE
      *            IF W < 600
      *               MOVE OBS-V103     TO GS-OBS(W: 100)
      *               ADD 100 TO W
      *            END-IF
      *          END-IF
      *      END-READ
      *    END-PERFORM.
      *-----------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "VIP301A" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                      PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.

           copy descondensa.

       MOVER-DADOS-RELATORIO SECTION.
           PERFORM MOVER-DADOS-LINDET
           MOVE GS-LINDET TO LINDET-REL

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
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
           CLOSE CAD010 CGD001 COD003 COD040 VID020 VID021 VID022
                 VID100 VID101 VID102 VID103.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
