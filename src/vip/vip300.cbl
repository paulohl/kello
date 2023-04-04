       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP300.
      *DATA: 06/06/2000
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
           COPY CGPX001.
           COPY COPX003.
           COPY COPX040.
           COPY VIPX100.
           COPY VIPX106.
           COPY VIPX130.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS DATA-MOVTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS IDENTIFICADOR-WK
                                                        WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NR-CONTRATO-WK DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-EVENTO-WK DUPLICATES
                  ALTERNATE RECORD KEY IS EVENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                           CINEGRAFISTA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LOCALIZACAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS SEQ-ENT-WK  WITH DUPLICATES.

           SELECT RELAT ASSIGN TO ARQUIVO-IMPRESSAO
                        ORGANIZATION IS LINE SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.

           SELECT EXCEL ASSIGN TO ARQUIVO-EXCEL
                        ORGANIZATION IS SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.



       DATA DIVISION.
       FILE SECTION.
       COPY COPW003.
       COPY COPW040.
       COPY CGPW001.
       COPY VIPW100.
       COPY VIPW106.
       COPY VIPW130.
       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(4).
           05  SEQ-ENT-WK          PIC 9(3).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  NR-CONTRATO-WK      PIC 9(4).
           05  IDENTIFICADOR-WK    PIC 9(9).
           05  DATA-EVENTO-WK      PIC 9(8).
           05  EVENTO-WK           PIC X(15).
           05  CINEGRAFISTA-WK     PIC X(15).
           05  LOCALIZACAO-WK      PIC X(5).
           05  LOTE-WK             PIC 9(2).
           05  QTDE-ARQ-WK         PIC 9(03).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       FD EXCEL.
       01 REG-EXCEL.
          05 EXCEL-DATA            PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-SEQUENCIA       PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FITA            PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CONTRATO        PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-DATA-EVENTO     PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-EVENTO          PIC X(15).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CINEGRAFISTA    PIC X(15).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-LOCALIZACAO     PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-LOTE            PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-ARQ             PIC X(10).
          05 FILLER                PIC X(02) VALUE X"0DA0".

       WORKING-STORAGE SECTION.
           COPY "VIP300.CPB".
           COPY "VIP300.CPY".
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
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-VID100             PIC XX       VALUE SPACES.
           05  ST-VID106             PIC XX       VALUE SPACES.
           05  ST-VID130             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  COMPACTA              PIC X(01)    VALUE SPACES.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  IDENTIFICADOR-W       PIC 9(9)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  MASC-QTDE             PIC ZZZ.
      *    CONTROLE DE QUEBRA
           05  DATA-MOVTO-ANT        PIC 9(8)     VALUE ZEROS.
           05  NR-CONTRATO-ANT       PIC 9(4)     VALUE ZEROS.
           05  DATA-EVENTO-ANT       PIC 9(8)     VALUE ZEROS.
           05  EVENTO-ANT            PIC X(15)    VALUE SPACES.
           05  CINEGRAFISTA-ANT      PIC X(15)    VALUE SPACES.
           05  LOCALIZACAO-ANT       PIC X(5)     VALUE SPACES.
           05  AVALIACAO-ANT         PIC X(4)     VALUE SPACES.
      *    TOTALIZA VARIAVEIS
           05  QTDE-E                PIC ZZZZ.ZZZ.
           05  FORM-E                PIC ZZZZ.
           05  LIN                   PIC 9(02)    VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  MASC-NUMERO           PIC ZZZZZZZZ9.
           05  MES-FIM               PIC 9(06)    VALUE ZEROS.
           05  MES-INI               PIC 9(06)    VALUE ZEROS.
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
           "RELACAO DE FITAS DE VIDEO-ORDEM: ".
           05  ORDEM-REL           PIC X(27)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  ORDEM-SOLIC-REL     PIC X(39)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(112)  VALUE
           "DATA-MOVTO SEQ NR.FITA   CONT DATA-EVENT EVENTO          CIN
      -    "EGRAFISTA    LOCAL LOTE ARQ OBSERVACAO      ".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(50)   VALUE
            "___________________________ _____/_____/_____".
           05  FILLER              PIC X(50)   VALUE
            "___________________________ _____/_____/_____".
       01  CAB06.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(50)   VALUE
            "     D.O.              DATA     ".
           05  FILLER              PIC X(50)   VALUE
            "    DEPTO-VIDEO        DATA     ".

       02 status-code           PIC X(2) COMP-5.



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
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "VID100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-VID100.
           MOVE "VID106"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-VID106.
           MOVE "VID130"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-VID130.
           OPEN INPUT CGD001 COD003 VID100 VID106 COD040
           OPEN I-O   VID130
           CLOSE      VID130
           OPEN INPUT VID130
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID100 <> "00"
              MOVE "ERRO ABERTURA VID100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID106 <> "00"
              MOVE "ERRO ABERTURA VID106: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID106 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID130 <> "00"
              MOVE "ERRO ABERTURA VID130: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID130 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           ACCEPT VARIA-W FROM TIME.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.


       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    CLOSE    WORK
                    OPEN I-O WORK
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    OPEN INPUT WORK
                    PERFORM CARREGA-LISTA
                    CLOSE      WORK
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    OPEN INPUT WORK
                    PERFORM CARREGA-LISTA
                    CLOSE      WORK
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LE-CONTRATO
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM POPUP-CONTRATO
               WHEN GS-EXCEL-TRUE
                    OPEN INPUT WORK
                    PERFORM GERAR-EXCEL
                    CLOSE      WORK
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

           IF GS-DATA-INI > 0
              STRING "\ARQUIVOS\VIDEO-" GS-DATA-INI "-" GS-DATA-FIM INTO
              ARQUIVO-EXCEL
           ELSE
              STRING "\ARQUIVOS\VIDEO-" GS-CONTRATO INTO ARQUIVO-EXCEL.


           OPEN OUTPUT EXCEL

           MOVE "DATA-MOVTO"           TO EXCEL-DATA
           MOVE "SEQ"                  TO EXCEL-SEQUENCIA
           MOVE "N.FIT"                TO EXCEL-FITA
           MOVE "CONT"                 TO EXCEL-CONTRATO
           MOVE "DATA-EVENT"           TO EXCEL-DATA-EVENTO
           MOVE "EVENTO"               TO EXCEL-EVENTO
           MOVE "CINEGRAFISTA"         TO EXCEL-CINEGRAFISTA
           MOVE "LOCAL"                TO EXCEL-LOCALIZACAO
           MOVE "LOTE"                 TO EXCEL-LOTE
           MOVE "QTDE ARQ"             TO EXCEL-ARQ
           WRITE REG-EXCEL

           PERFORM ORDEM.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   MOVE DATA-MOVTO-WK     TO DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV          TO DATA-E
                   MOVE DATA-E            TO EXCEL-DATA
                   MOVE SEQ-ENT-WK        TO MASC-NUMERO
                   MOVE MASC-NUMERO       TO EXCEL-SEQUENCIA
                   MOVE IDENTIFICADOR-WK  TO MASC-NUMERO
                   MOVE MASC-NUMERO       TO EXCEL-FITA
                   MOVE NR-CONTRATO-WK    TO EXCEL-CONTRATO
                   MOVE DATA-EVENTO-WK    TO DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV          TO DATA-E
                   MOVE DATA-E            TO EXCEL-DATA-EVENTO
                   MOVE EVENTO-WK         TO EXCEL-EVENTO
                   MOVE CINEGRAFISTA-WK   TO EXCEL-CINEGRAFISTA
                   MOVE LOCALIZACAO-WK    TO EXCEL-LOCALIZACAO
                   MOVE LOTE-WK           TO EXCEL-LOTE
                   MOVE QTDE-ARQ-WK       TO MASC-QTDE
                   MOVE MASC-QTDE         TO EXCEL-ARQ
                   WRITE REG-EXCEL
              END-READ
           END-PERFORM.

           CLOSE EXCEL.

       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE SPACES TO IDENTIFICACAO-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO.

       POPUP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "COP040T"
           MOVE PASSAR-STRING-1(22: 11)  TO GS-DESC-CONTRATO
           MOVE PASSAR-STRING-1(52: 4)   TO GS-CONTRATO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA"         TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-DATA-INI            TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV               TO DATA-INI


           MOVE GS-DATA-FIM            TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV               TO DATA-FIM

           STRING GS-MES-INI(3:4) GS-MES-INI(1:2) INTO
                  MES-INI

           STRING GS-MES-FIM(3:4) GS-MES-FIM(1:2) INTO
                  MES-FIM

           EVALUATE GS-TIPO-DATA
               WHEN 3      PERFORM LER-PELO-VID130
               WHEN OTHER  PERFORM LER-PELO-VID100
           END-EVALUATE

           CLOSE WORK

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM
           EXIT.

       LER-PELO-VID130 SECTION.
           MOVE ZEROS                  TO SEQ-WK
           INITIALIZE REG-VID130
           MOVE DATA-INI               TO DATA-V130
           MOVE GS-LOTE                TO LOTE-V130
           START VID130 KEY IS NOT LESS CHAVE-V130 INVALID KEY
                MOVE "10" TO ST-VID130.

           PERFORM UNTIL ST-VID130 = "10"
                READ VID130 NEXT AT END
                     MOVE "10" TO ST-VID130
                NOT AT END
                     IF DATA-INI <> DATA-V130
                        MOVE "10" TO ST-VID130
                     ELSE
                        IF GS-LOTE > 0 AND GS-LOTE <> LOTE-V130
                           MOVE "10" TO ST-VID130
                        ELSE
                           INITIALIZE REG-VID100 IDENTIFICADOR-W
                           MOVE NR-FITAS-V130 TO NR-FITAS-V100
                           START VID100 KEY IS NOT LESS ALT-V100
                                                             INVALID KEY
                                MOVE "10" TO ST-VID100
                           END-START
                           READ VID100 NEXT RECORD
                           END-READ
                           IF NR-FITAS-V130 = NR-FITAS-V100
                              MOVE IDENTIFICADOR-V100 TO IDENTIFICADOR-W
                           END-IF
                           INITIALIZE REG-VID100
                           MOVE IDENTIFICADOR-W TO IDENTIFICADOR-V100
                           START VID100 KEY IS NOT LESS ALT4-V100
                                                             INVALID KEY
                                 MOVE "10" TO ST-VID100
                           END-START
                           PERFORM UNTIL ST-VID100 = "10"
                                READ VID100 NEXT AT END
                                     MOVE "10" TO ST-VID100
                                NOT AT END
                                     IF IDENTIFICADOR-W <>
                                        IDENTIFICADOR-V100
                                        MOVE "10" TO ST-VID100
                                     ELSE
                                        IF GS-CONTRATO = 0 OR
                                           CONTRATO-V100
                                           IF GS-MES-INI = 0
                                              PERFORM MOVER-DADOS-WORK
                                           ELSE
                                              PERFORM VERIFICAR-MESANO
                                           END-IF
                                        END-IF
                                     END-IF
                                END-READ
                           END-PERFORM
                        END-IF
                     END-IF
                END-READ
           END-PERFORM.

       VERIFICAR-MESANO SECTION.
           MOVE CONTRATO-V100          TO NR-CONTRATO-CO40
           READ COD040 NOT INVALID KEY
                IF MESANO-PREV-CO40 NOT < MES-INI AND
                   MESANO-PREV-CO40 NOT > MES-FIM
                   PERFORM MOVER-DADOS-WORK.

       LER-PELO-VID100 SECTION.
           MOVE ZEROS                  TO SEQ-WK
           INITIALIZE REG-VID100
           IF GS-MES-INI > 0
              INITIALIZE REG-COD040
              STRING GS-MES-INI(3:4) GS-MES-INI(1:2) INTO
                     MESANO-PREV-CO40
              START COD040 KEY IS NOT LESS ALT1-CO40 INVALID KEY
                    MOVE "10" TO ST-COD040
              END-START
              PERFORM UNTIL ST-COD040 = "10"
                    READ COD040 NEXT AT END
                         MOVE "10" TO ST-COD040
                    NOT AT END
                         IF MESANO-PREV-CO40 > MES-FIM
                            MOVE "10" TO ST-COD040
                         ELSE
                            IF GS-CONTRATO = 0 OR NR-CONTRATO-CO40
                               INITIALIZE REG-VID100
                               MOVE NR-CONTRATO-CO40 TO CONTRATO-V100
                               START VID100 KEY IS NOT LESS ALT1-V100
                                                             INVALID KEY
                                    MOVE "10" TO ST-VID100
                               END-START
                               PERFORM UNTIL ST-VID100 = "10"
                                    READ VID100 NEXT AT END
                                         MOVE "10" TO ST-VID100
                                    NOT AT END
                                         IF NR-CONTRATO-CO40 <>
                                            CONTRATO-V100
                                            MOVE "10" TO ST-VID100
                                         ELSE
                                            EVALUATE GS-TIPO-DATA
                                              WHEN 1
                                                IF DATA-INI = 0 OR
                                                  (DATA-MOVTO-V100 NOT <
                                                   DATA-INI AND
                                                   DATA-MOVTO-V100 NOT >
                                                   DATA-FIM)
                                                   PERFORM
                                                        MOVER-DADOS-WORK
                                                END-IF
                                              WHEN 2
                                                IF DATA-INI = 0 OR
                                                 (DATA-EVENTO-V100 NOT <
                                                   DATA-INI AND
                                                  DATA-EVENTO-V100 NOT >
                                                   DATA-FIM)
                                                  PERFORM
                                                        MOVER-DADOS-WORK
                                                END-IF
                                            END-EVALUATE
                                         END-IF
                                    END-READ
                               END-PERFORM
                            END-IF
                         END-IF
                    END-READ
              END-PERFORM
           ELSE
              IF GS-DATA-INI > 0
                 MOVE GS-DATA-INI            TO DATA-INV DATA-E
                 MOVE DATA-E                 TO ORDEM-SOLIC-REL(17: 11)
                 MOVE "a "                   TO ORDEM-SOLIC-REL(28: 2)
                 CALL "GRIDAT2" USING DATA-INV
                 MOVE DATA-INV               TO DATA-INI
                 MOVE GS-DATA-FIM            TO DATA-INV DATA-E
                 MOVE DATA-E                 TO ORDEM-SOLIC-REL(30: 10)
                 CALL "GRIDAT2" USING DATA-INV
                 MOVE DATA-INV               TO DATA-FIM

                 EVALUATE GS-TIPO-DATA
      *             DATA MOVIMENTO
                    WHEN 1 MOVE "INT.DATA MOVTO: "
                                             TO ORDEM-SOLIC-REL(1: 16)
                        MOVE DATA-INI        TO DATA-MOVTO-V100
                        START VID100 KEY IS NOT < CHAVE-V100 INVALID KEY
                              MOVE "10" TO ST-VID100
                        END-START
      *             DATA EVENTO
                    WHEN 2 MOVE "INT.DT EVENTO.: "
                                             TO ORDEM-SOLIC-REL(1: 16)
                        MOVE DATA-INI        TO DATA-EVENTO-V100
                        START VID100 KEY IS NOT < ALT3-V100 INVALID KEY
                              MOVE "10" TO ST-VID100
                        END-START
                 END-EVALUATE
                 MOVE ZEROS                     TO SEQ-WK
              ELSE
                 MOVE GS-CONTRATO    TO CONTRATO-V100
                 MOVE ZEROS          TO DATA-EVENTO-V100
                 START VID100 KEY IS NOT < ALT1-V100 INVALID KEY
                     MOVE "10" TO ST-VID100
                 END-START
              END-IF

              PERFORM UNTIL ST-VID100 = "10"
                  READ VID100 NEXT RECORD AT END
                      MOVE "10" TO ST-VID100
                  NOT AT END
                      IF GS-DATA-INI > 0
                         EVALUATE GS-TIPO-DATA
                            WHEN 1
                                 IF DATA-MOVTO-V100 > DATA-FIM
                                    MOVE "10" TO ST-VID100
                                 ELSE
                                    IF GS-CONTRATO = 0 OR CONTRATO-V100
                                       PERFORM MOVER-DADOS-WORK
                                    END-IF
                                 END-IF
                            WHEN 2
                                 IF DATA-EVENTO-V100 > DATA-FIM
                                    MOVE "10" TO ST-VID100
                                 ELSE
                                    IF GS-CONTRATO = 0 OR CONTRATO-V100
                                       PERFORM MOVER-DADOS-WORK
                                    END-IF
                                 END-IF
                         END-EVALUATE
                      ELSE
                         IF GS-CONTRATO <> CONTRATO-V100
                            MOVE "10" TO ST-VID100
                         ELSE
                            PERFORM MOVER-DADOS-WORK
                         END-IF
                      END-IF
                  END-READ
              END-PERFORM
           END-IF.

       MOVER-DADOS-WORK SECTION.
           IF GS-LOCALIZACAO = SPACES OR LOCALIZACAO-V100
              ADD 1                     TO SEQ-WK
              MOVE DATA-MOVTO-V100      TO DATA-MOVTO-WK
                                           GS-EXIBE-VENCTO
              MOVE "TELA-AGUARDA1"      TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              MOVE IDENTIFICADOR-V100   TO IDENTIFICADOR-WK
              MOVE SEQ-V100             TO SEQ-ENT-WK
              MOVE CONTRATO-V100        TO NR-CONTRATO-WK
              MOVE DATA-EVENTO-V100     TO DATA-EVENTO-WK
              MOVE EVENTO-V100          TO CODIGO-CO03
              READ COD003 INVALID KEY
                   MOVE SPACES          TO NOME-CO03
              END-READ
              MOVE NOME-CO03            TO EVENTO-WK
              MOVE CINEGRAFISTA-V100    TO CODIGO-CG01
              READ CGD001 INVALID KEY
                   MOVE SPACES          TO NOME-CG01
              END-READ
              MOVE NOME-CG01            TO CINEGRAFISTA-WK
              MOVE LOCALIZACAO-V100     TO LOCALIZACAO-WK
              IF GS-TIPO-DATA = 3
                 MOVE LOTE-V130         TO LOTE-WK
              ELSE
                 MOVE ZEROS             TO LOTE-WK
              END-IF

              IF QTDE-ARQUIVOS-V100 IS NOT NUMERIC
                 MOVE 0 TO QTDE-ARQUIVOS-V100
              END-IF
              MOVE QTDE-ARQUIVOS-V100   TO QTDE-ARQ-WK

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

           MOVE SPACES         TO GS-LINDET

      *    INITIALIZE REG-WORK
      *    START WORK KEY IS NOT LESS SEQ-WK INVALID KEY
      *        MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    PERFORM MOVER-DADOS-LINDET
               END-READ
           END-PERFORM.
       SAIR2.
           exit.
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE "REFRESH-DATA" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.

       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "DATA-MOVTO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DATA-MOVTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "NR-FITA  " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < IDENTIFICADOR-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < NR-CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "DATA-EVENT" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DATA-EVENTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "EVENTO    " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < EVENTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "CINEGRAF. " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CINEGRAFISTA-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 7
                MOVE "LOCALIZ.  " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < LOCALIZACAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.

       MOVER-DADOS-LINDET SECTION.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS SECTION.
           MOVE DATA-MOVTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(1: 11)
           MOVE SEQ-ENT-WK        TO GS-LINDET(12: 4)
           MOVE IDENTIFICADOR-WK  TO GS-LINDET(16:10)
           MOVE NR-CONTRATO-WK    TO GS-LINDET(26: 5)
           MOVE DATA-EVENTO-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(31: 11)
           MOVE EVENTO-WK         TO GS-LINDET(42: 16)
           MOVE CINEGRAFISTA-WK   TO GS-LINDET(58: 16)
           MOVE LOCALIZACAO-WK    TO GS-LINDET(74: 6)
           MOVE LOTE-WK           TO GS-LINDET(82:2)
           MOVE QTDE-ARQ-WK       TO MASC-QTDE
           MOVE MASC-QTDE         TO GS-LINDET(85:3).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "VIP300" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------------
       IMPRIME-RELATORIO SECTION.

           MOVE ZEROS TO PAG-W.

      *    COPY "COND-IMP".

           MOVE "S" TO COMPACTA

           MOVE SPACES TO ARQUIVO-IMPRESSAO

           evaluate gs-tipo
               when 1 string "\ARQUIVOS\VID-" GS-DATA-INI "-"
               GS-DATA-FIM INTO ARQUIVO-IMPRESSAO
               when 2 string "\ARQUIVOS\VID-" GS-CONTRATO INTO
                                                       ARQUIVO-IMPRESSAO
           end-evaluate

           OPEN OUTPUT RELAT.

           PERFORM ORDEM.

           IF ST-WORK = "10"
              GO TO SAIR3.

           MOVE ZEROS TO LIN.

           PERFORM CABECALHO.

       READ-WORK2.
           MOVE SPACES TO LINDET-REL
           READ WORK NEXT RECORD AT END
               GO TO SAIR3.

           IF ST-WORK <> "00" AND "02"
              GO TO READ-WORK2.

           PERFORM MOVER-DADOS-RELATORIO

           GO TO READ-WORK2.

       SAIR3.
           IF LIN > 53
              PERFORM CABECALHO.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT
           WRITE REG-RELAT
           WRITE REG-RELAT FROM CAB05
      *    AFTER 3.
           WRITE REG-RELAT FROM CAB06.
           MOVE SPACES TO REG-RELAT.

           MOVE "SALTAR PAGINA" TO REG-RELAT
           WRITE REG-RELAT.

      *    WRITE REG-RELAT AFTER PAGE.

           CLOSE RELAT.
      *    COPY "DESC-IMP".

           IF GS-IMPRIMIR = "S"
              MOVE "S" TO COMPACTA
              CALL "PRP102" USING PARAMETROS-W ARQUIVO-IMPRESSAO
                                  COMPACTA IMPRESSORA-W
              CANCEL "PRP102"
              call "CBL_DELETE_FILE" using     ARQUIVO-IMPRESSAO
                                     returning status-code
              if status-code <> "0000"
                 move "Erro na Exclusão do Arquivo" to mensagem
                move "C" to tipo-msg
                perform exibir-mensagem.


       MOVER-DADOS-RELATORIO SECTION.
           PERFORM MOVER-DADOS
           MOVE GS-LINDET TO LINDET-REL

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 53
               PERFORM CABECALHO.

       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              MOVE "INICIO" TO REG-RELAT
              WRITE REG-RELAT
      *       WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              MOVE "SALTAR PAGINA" TO REG-RELAT
              WRITE REG-RELAT.
      *       FROM CAB01 AFTER PAGE.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT
           WRITE REG-RELAT FROM CAB02.
      *    AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.

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
           CLOSE COD003 COD040 VID100 VID106 CGD001 VID130.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
