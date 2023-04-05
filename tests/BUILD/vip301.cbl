       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP301.
      *DATA: 28/08/2000
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
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "VIP301.CPB".
           COPY "VIP301.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
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
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
      *    TOTALIZA VARIAVEIS
           05  QTDE-E                PIC ZZZZ.ZZZ.
           05  FORM-E                PIC ZZZZ.
           05  I                     PIC 9(5)     VALUE ZEROS.
           05  W                     PIC 9(5)     VALUE ZEROS.
           05  MAIOR-SEQ             PIC 9(5)     VALUE ZEROS.
           05  NR-FITA-W             PIC 9(5)     VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  OBS-W                 PIC X(100)   VALUE SPACES.
           05  SEQUENCIAL OCCURS 10000 TIMES.
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
           "CONT CURSO      CIDADE     EVENTO     DATA-EVENT N.FIT CINEG
      -    "RAF.  FILMADORA  REVISOR    AVALIAC DATA-REVIS".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

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
               WHEN GS-LE-ANTERIOR-TRUE
                    PERFORM MOVER-ANTERIOR
               WHEN GS-LE-PROXIMO-TRUE
                    PERFORM MOVER-PROXIMO
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
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10000
             IF DATA-MOVTO(I) = ZEROS MOVE 10000 TO I
             ELSE
                MOVE ZEROS TO DATA-MOVTO(I) SEQ(I)
             END-IF
           END-PERFORM.

           MOVE ZEROS TO I MAIOR-SEQ.
           MOVE GS-DATA-INI   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV      TO DATA-INI
           MOVE GS-DATA-FIM   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV      TO DATA-FIM
           EVALUATE GS-ORDEM
             WHEN 1 MOVE GS-CINEGRAFISTA  TO CINEGRAFISTA-V100
                    MOVE ZEROS            TO DATA-EVENTO-V100
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
               READ VID100 NEXT RECORD AT END
                    MOVE "10" TO ST-VID100
               NOT AT END
                    EVALUATE GS-ORDEM
                      WHEN 1 IF CINEGRAFISTA-V100 <> GS-CINEGRAFISTA
                                MOVE "10" TO ST-VID100
                             ELSE
                                IF GS-CONTRATO = 0 OR CONTRATO-V100
                                   PERFORM MOVER-DADOS-SEQ
                                END-IF
                             END-IF
                      WHEN 2 IF CONTRATO-V100 <> GS-CONTRATO
                                MOVE "10" TO ST-VID100
                             ELSE
                                PERFORM MOVER-DADOS-SEQ
                             END-IF
                      WHEN 3 IF DATA-EVENTO-V100 > DATA-FIM
                                MOVE "10" TO ST-VID100
                             ELSE
                                IF GS-CINEGRAFISTA = 0 OR
                                   CINEGRAFISTA-V100
                                   PERFORM MOVER-DADOS-SEQ
                                END-IF
                             END-IF
                    END-EVALUATE
               END-READ
           END-PERFORM.
           MOVE ZEROS TO I.
           PERFORM VERIFICA-SEQ.
           PERFORM MOVER-PROXIMO.
       VERIFICA-SEQ SECTION.
           EVALUATE MAIOR-SEQ
             WHEN ZEROS
              MOVE "DESABILITA-ANTERIOR" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              MOVE "DESABILITA-PROXIMO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
             WHEN 1
                   MOVE "DESABILITA-ANTERIOR" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   MOVE "DESABILITA-PROXIMO" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM

             WHEN I
                   MOVE "DESABILITA-PROXIMO" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   MOVE "HABILITA-ANTERIOR" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
             WHEN OTHER
                   MOVE "HABILITA-PROXIMO" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   MOVE "HABILITA-ANTERIOR" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
           END-EVALUATE.
           IF I = 1 MOVE "DESABILITA-ANTERIOR" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-SEQ SECTION.
           ADD 1 TO I
           IF I > 10000 CONTINUE
           ELSE
             MOVE DATA-MOVTO-V100  TO DATA-MOVTO(I)
             MOVE SEQ-V100         TO SEQ(I)
           END-IF.
           MOVE I TO MAIOR-SEQ.

       MOVER-PROXIMO SECTION.
           ADD 1 TO I
           MOVE DATA-MOVTO(I)    TO DATA-MOVTO-V100
           MOVE SEQ(I)           TO SEQ-V100
           READ VID100 INVALID KEY
                INITIALIZE REG-VID100
           END-READ
           PERFORM MOVER-DADOS-LISTA.
           PERFORM VERIFICA-SEQ.
       MOVER-ANTERIOR SECTION.
           SUBTRACT 1 FROM I.
           MOVE DATA-MOVTO(I)    TO DATA-MOVTO-V100.
           MOVE SEQ(I)           TO SEQ-V100.
           READ VID100 INVALID KEY
                INITIALIZE REG-VID100.
           PERFORM MOVER-DADOS-LISTA.
           PERFORM VERIFICA-SEQ.

       MOVER-DADOS-LISTA SECTION.
           MOVE CONTRATO-V100      TO GS-LINDET-CAB(1: 5)
                                      NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE ZEROS         TO CIDADE-CO40.
           MOVE CURSO-V100         TO GS-LINDET-CAB(6: 10)
           READ CAD010 INVALID KEY
                MOVE SPACES        TO NOME-CID.
           MOVE NOME-CID           TO GS-LINDET-CAB(17: 10)
           MOVE EVENTO-V100        TO CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE SPACES        TO NOME-CO03.
           MOVE NOME-CO03          TO GS-LINDET-CAB(28: 10)
           MOVE DATA-EVENTO-V100   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET-CAB(39: 11)
           MOVE NR-FITA-V100       TO GS-LINDET-CAB(50: 6)
           MOVE CINEGRAFISTA-V100  TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE SPACES        TO NOME-CG01.
           MOVE NOME-CG01          TO GS-LINDET-CAB(56: 10)
           MOVE FILMADORA-V100     TO CODIGO-V20.
           READ VID020 INVALID KEY
                MOVE SPACES        TO DESCRICAO-V20.
           MOVE DESCRICAO-V20      TO GS-LINDET-CAB(67: 10)

           MOVE NR-FITA-V100       TO NR-FITA-V101
           READ VID101 INVALID KEY
                INITIALIZE REG-VID101.
           MOVE REVISOR-V101       TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE SPACES        TO NOME-CG01.
           MOVE NOME-CG01          TO GS-LINDET-CAB(78: 10)
           EVALUATE AVALIACAO-GERAL-V101
             WHEN 1 MOVE "PESSIMA" TO GS-LINDET-CAB(89: 8)
             WHEN 2 MOVE "RUIM   " TO GS-LINDET-CAB(89: 8)
             WHEN 3 MOVE "REGULAR" TO GS-LINDET-CAB(89: 8)
             WHEN 4 MOVE "BOA    " TO GS-LINDET-CAB(89: 8)
             WHEN 5 MOVE "OTIMA  " TO GS-LINDET-CAB(89: 8)
           END-EVALUATE
           MOVE DATA-REVISAO-V101  TO DATA-E
           MOVE DATA-E             TO GS-LINDET-CAB(97: 11)

           MOVE "CLEAR-LIST-BOX"   TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE NR-FITA-V100       TO NR-FITA-V102
           MOVE ZEROS              TO SEQ-V102.
           START VID102 KEY IS NOT < CHAVE-V102 INVALID KEY
                 MOVE "10" TO ST-VID102.
           PERFORM UNTIL ST-VID102 = "10"
                 READ VID102 NEXT RECORD AT END
                      MOVE "10" TO ST-VID102
                 NOT AT END
                     IF NR-FITA-V102 <> NR-FITA-V100
                        MOVE "10" TO ST-VID102
                     ELSE
                       MOVE MOMENTO-V102    TO CODIGO-V22
                       READ VID022 INVALID KEY
                            MOVE SPACES     TO DESCRICAO-V22
                       END-READ
                       MOVE DESCRICAO-V22   TO GS-LINDET(1: 31)
                       MOVE PROBLEMA-V102   TO CODIGO-V21
                       READ VID021 INVALID KEY
                            MOVE SPACES     TO DESCRICAO-V21
                       END-READ
                       MOVE DESCRICAO-V21   TO GS-LINDET(32: 30)
                       MOVE "INSERE-LIST"   TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                     END-IF
                 END-READ
           END-PERFORM.

           MOVE "CLEAR-OBSERVACAO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO W.
           MOVE CONTRATO-V100        TO CONTRATO-V103
           MOVE NR-FITA-V100         TO NR-FITA-V103.
           MOVE ZEROS                TO SEQ-V103.
           START VID103 KEY IS NOT < CHAVE-V103 INVALID KEY
                 MOVE "10" TO ST-VID103.
           PERFORM UNTIL ST-VID103 = "10"
             READ VID103 NEXT RECORD AT END
                  MOVE "10" TO ST-VID103
             NOT AT END
                  IF CONTRATO-V100 <> CONTRATO-V103 OR
                     NR-FITA-V103  <> NR-FITA-V100
                     MOVE "10"           TO ST-VID103
                  ELSE
                    IF W < 600
                       MOVE OBS-V103     TO GS-OBS(W: 100)
                       ADD 100 TO W
                    END-IF
                  END-IF
             END-READ
           END-PERFORM.
      *-----------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "VIP301" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           WRITE REG-RELAT FROM CAB04 AFTER 3.
           WRITE REG-RELAT FROM CAB03.
           MOVE GS-LINDET-CAB  TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.
           MOVE "OBSERVACAO" TO LINDET-REL
           WRITE REG-RELAT FROM LINDET AFTER 2
           WRITE REG-RELAT FROM CAB03.
           ADD 8 TO LIN.

           PERFORM VARYING W FROM 1 BY 100 UNTIL W > 600

              MOVE GS-OBS(W: 100)  TO LINDET-REL OBS-W
              IF OBS-W = SPACES MOVE 600 TO W
                 CONTINUE
              ELSE
                 WRITE REG-RELAT FROM LINDET
                 ADD 1 TO LIN
              END-IF
           END-PERFORM.

           MOVE GS-LINDET-CAB(1: 5)   TO CONTRATO-V102
                                         CONTRATO-W
           MOVE GS-LINDET-CAB(50: 5)  TO NR-FITA-V102
                                         NR-FITA-W.
           MOVE ZEROS                 TO SEQ-V102.
           START VID102 KEY IS NOT < CHAVE-V102 INVALID KEY
                 MOVE "10" TO ST-VID102.

           MOVE SPACES TO LINDET-REL.
           MOVE "PROBLEMA"   TO GS-LINDET(1: 31)
           MOVE "MOMENTO"    TO GS-LINDET(32: 30)
           WRITE REG-RELAT FROM LINDET
           WRITE REG-RELAT FROM CAB03.
           ADD 2 TO LIN.

           PERFORM UNTIL ST-VID102 = "10"
             READ VID102 NEXT RECORD AT END
                  MOVE "10" TO ST-VID102
             NOT AT END
                  IF CONTRATO-V102  <> CONTRATO-W OR
                     NR-FITA-V102   <> NR-FITA-W
                      MOVE "10"            TO ST-VID102
                  ELSE
                      MOVE MOMENTO-V102    TO CODIGO-V22
                      READ VID022 INVALID KEY
                           MOVE SPACES     TO DESCRICAO-V22
                      END-READ
                      MOVE DESCRICAO-V22   TO LINDET-REL(1: 31)
                      MOVE PROBLEMA-V102   TO CODIGO-V21
                      READ VID021 INVALID KEY
                           MOVE SPACES     TO DESCRICAO-V21
                      END-READ
                      MOVE DESCRICAO-V21   TO LINDET-REL(32: 30)
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                  END-IF
             END-READ
           END-PERFORM.

           copy descondensa.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.
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
