       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP113.
      *DATA: 17-11-2005
      *AUTOR: ALFREDO SAVIOLLI NETO
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
           COPY IEPX011.
           COPY MTPX019.
           COPY MTPX025.
           COPY COPX008.
           COPY COPX002.
           COPY COPX040.
           COPY COPX050.
           COPY COPX051.
           COPY COPX060.
           COPY REPX100.
           COPY REPX101.
           COPY RCPX100.
           COPY RCPX101.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS ALT-WK = CONTRATO-WK
                  ALTERNATE RECORD KEY IS ALT1-WK =
                       CURSO-WK CONTRATO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT5-WK =
                       CIDADE-WK CONTRATO-WK WITH DUPLICATES.

           SELECT WORK2 ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS CHAVE-WK2 = NOME-CURSO-WK2.


           SELECT RELAT ASSIGN TO PRINTER
                        ORGANIZATION IS LINE SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL
                        FILE STATUS IS FS-RELAT.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CGPW001.
       COPY IEPW011.
       COPY MTPW019.
       COPY MTPW025.
       COPY COPW008.
       COPY COPW002.
       COPY COPW040.
       COPY COPW050.
       COPY COPW051.
       COPY COPW060.
       COPY REPW100.
       COPY REPW101.
       COPY RCPW100.
       COPY RCPW101.
       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  CIDADE-WK           PIC X(13).
           05  CURSO-WK            PIC X(30).
           05  VENDA-CONTRATO-WK   PIC 9(12)V99.
           05  VALOR-COMISSAO-WK   PIC 9(12)V99.
           05  BRINDES-WK          PIC 9(12)V99.
           05  TOTAL-EVENTOS-WK    PIC 9(12)V99.
           05  TOTAL-ADM-WK        PIC 9(12)V99.
           05  MATERIAL-WK         PIC 9(12)V99.

       FD  WORK2.
       01  REG-WORK2.
           05 COD-CURSO-WK2             PIC 999.
           05 NOME-CURSO-WK2            PIC X(12).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY "COP113.CPB".
           COPY "COP113.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD025             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD008             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-COD051             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  FS-RELAT              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  AUX-MESANO            PIC 9(06)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  QTDE-E                PIC ZZZ.ZZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  TOT-FORMANDO          PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
           05  COMPACTA              PIC X(01)    VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  CUSTO-PREVISTO        PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-W               PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-W               PIC 9(8)V99  VALUE ZEROS.
           05  CONTRATO-REL          PIC 9(4)    VALUE ZEROS.
           05  AUX-DATA              PIC 9(08).
           05  MESANO-INI            PIC 9(06)   VALUE ZEROS.
           05  MESANO-FIM            PIC 9(06)   VALUE ZEROS.
           05  PERCENTUAL            PIC S9(3)V99 VALUE ZEROS.
           05  QTDE-E2               PIC ZZZ,ZZ-  BLANK WHEN ZEROS.
           05  MASC-VALOR-12         PIC Z.ZZZ.ZZ9,99 VALUE ZEROS.
           05  MASC-VALOR-10         PIC ZZZ.ZZ9,99   VALUE ZEROS.
           05  MASC-VALOR-8          PIC   Z.ZZ9,99   VALUE ZEROS.
           05  MASC-VALOR-6          PIC     ZZ9,99   VALUE ZEROS.
           05  MASC-VALOR-5          PIC      Z9,99   VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(44)   VALUE
           "RELATORIO ESTATISTICO DE FORMANDOS - ORDEM: ".
           05  ORDEM-REL           PIC X(20)   VALUE SPACES.
           05  FILLER              PIC X(09) VALUE "MES/ANO: ".
           05  MESANO-REL1         PIC 99/9999.
           05  FILLER              PIC X(03) VALUE " A ".
           05  MESANO-REL2         PIC 99/9999.

       01  CAB03.
           05  FILLER              PIC X(126)  VALUE ALL "=".
       01  CAB04.
           05  DET-CAB04           PIC X(126).

       01  LINDET.
           05  LINDET-REL          PIC X(128)  VALUE SPACES.
       01  LINTOT.
           05  LINTOT-REL          PIC X(128)  VALUE SPACES.

       01 status-code           PIC X(2) COMP-5.

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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD025"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD025.
           MOVE "COD008"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD008.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD050"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "COD051"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD051.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           COMPUTE VARIA-W2 = VARIA-W + 10

           OPEN INPUT CAD010 IED011 MTD019 COD040 COD008 MTD025 COD060
                      RED101 CGD001 RCD100 RCD101 RED100 COD002 COD050
                      COD051.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD025 <> "00"
              MOVE "ERRO ABERTURA MTD025: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD025 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD008 <> "00"
              MOVE "ERRO ABERTURA COD008: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD008 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050  "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD050 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD051 <> "00"
              MOVE "ERRO ABERTURA COD051: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD051 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED101 <> "00"
              MOVE "ERRO ABERTURA RED101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-CIDADE-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-CIDADE-TRUE
                   PERFORM LE-CIDADE
               WHEN GS-LE-CONSULTOR-TRUE
                   PERFORM LE-CONSULTOR
               WHEN GS-POPUP-CONSULTOR-TRUE
                   PERFORM POPUP-CONSULTOR
               WHEN GS-CARREGA-CURSOS-TRUE
                    PERFORM CARREGAR-CURSOS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGAR-CURSOS SECTION.
           OPEN OUTPUT WORK2
           CLOSE       WORK2
           OPEN I-O    WORK2

           INITIALIZE REG-IED011
           START IED011 KEY IS NOT < CODIGO-IE11 INVALID KEY
                  MOVE "10" TO ST-IED011.

           PERFORM UNTIL ST-IED011 = "10"
             READ IED011 NEXT RECORD AT END
                  MOVE "10" TO ST-IED011
             NOT AT END
                  MOVE NOME-REDUZ-IE11   TO NOME-CURSO-WK2
                  MOVE CODIGO-IE11       TO COD-CURSO-WK2
                  WRITE REG-WORK2
                  END-WRITE
             END-READ

           END-PERFORM

           MOVE "TODOS"           TO NOME-CURSO-WK2
           MOVE 0                 TO COD-CURSO-WK2
           WRITE REG-WORK2



           MOVE 20 TO GS-CONT

           MOVE "LIMPAR-SB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK GS-CONT
           MOVE SPACES TO NOME-CURSO-WK2

           START WORK2 KEY IS NOT LESS CHAVE-WK2 INVALID KEY
               MOVE "10" TO ST-WORK2.

           PERFORM UNTIL ST-WORK2 = "10"
               READ WORK2 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK2
               NOT AT END
                   ADD  1              TO GS-CONT
                   MOVE SPACES         TO GS-CURSO
                   MOVE COD-CURSO-WK2  TO GS-CURSO(16:3)
                   MOVE NOME-CURSO-WK2 TO GS-CURSO(1:15)
                   MOVE "INSERIR-SB"  TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM

           CLOSE WORK2.


       CHAMAR-POPUP SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE.
           PERFORM LE-CIDADE.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-CIDADE SECTION.
           MOVE GS-CIDADE  TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-CID UF-CID NOME-COMPL-CID.
           MOVE NOME-CID TO GS-DESC-CIDADE.
       LE-CONSULTOR SECTION.
           MOVE GS-CONSULTOR TO CODIGO-CG01
           READ CGD001 INVALID KEY
               MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01 TO GS-DESC-CONSULTOR.
       POPUP-CONSULTOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-CONSULTOR
           PERFORM LE-CONSULTOR.

      *------------------------------------------------------------
       GRAVA-WORK SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           INITIALIZE REG-COD040

           STRING GS-MESANO-INI(3:4) GS-MESANO-INI(1:2)
                  INTO MESANO-PREV-CO40

           MOVE   MESANO-PREV-CO40   TO MESANO-INI

           STRING GS-MESANO-FIM(3:4) GS-MESANO-FIM(1:2)
                  INTO MESANO-FIM

           IF GS-CURSO = SPACES
              MOVE "000" TO GS-CURSO(16:3).

           START COD040 KEY IS NOT LESS ALT2-CO40 INVALID KEY
                MOVE "10" TO ST-COD040
           END-START
           PERFORM UNTIL ST-COD040 = "10"
               READ COD040 NEXT AT END
                   MOVE "10" TO ST-COD040
               NOT AT END
                   IF MESANO-FIM > 0 AND MESANO-PREV-CO40 > MESANO-FIM
                      MOVE "10" TO ST-COD040
                   ELSE
                      IF GS-CIDADE = 0 OR GS-CIDADE = CIDADE-CO40
                         IF GS-CONSULTOR = 0 OR GS-CONSULTOR =
                            REPRESENTANTE-CO40
                            IF GS-CURSO(16:3) = "000"
                               PERFORM MOVER-DADOS
                            ELSE
                               INITIALIZE REG-MTD019
                               MOVE NR-CONTRATO-CO40 TO CONTRATO-MT19
                               START MTD019 KEY IS NOT LESS ALBUMMT19
                                                             INVALID KEY
                                     MOVE "10" TO ST-MTD019
                               END-START
                               PERFORM UNTIL ST-MTD019 = "10"
                                   READ MTD019 NEXT AT END
                                        MOVE "10" TO ST-MTD019
                                   NOT AT END
                                     IF NR-CONTRATO-CO40 = CONTRATO-MT19
                                        IF GS-CURSO(16:3) = CURSO-MT19
                                           PERFORM MOVER-DADOS
                                        END-IF
                                     END-IF
                                     MOVE "10" TO ST-MTD019
                                   END-READ
                               END-PERFORM
                            END-IF
                         END-IF
                      END-IF
                   END-IF
               END-READ
           END-PERFORM
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS SECTION.
           MOVE CIDADE-CO40         TO CIDADE
           READ CAD010 INVALID KEY
               MOVE ZEROS           TO REGIAO-CID.

           IF GS-REGIAO = 0 OR GS-REGIAO = REGIAO-CID
               INITIALIZE REG-WORK
               MOVE NR-CONTRATO-CO40    TO GS-EXIBE-MOVTO
               MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM

               MOVE NR-CONTRATO-CO40    TO CONTRATO-WK
               MOVE CIDADE-CO40         TO CIDADE
               READ CAD010 INVALID KEY
                    MOVE SPACES TO NOME-CID
               END-READ
               MOVE NOME-CID            TO CIDADE-WK

               INITIALIZE REG-RCD100 VALOR-COMISSAO-WK
               STRING NR-CONTRATO-CO40 "0000" INTO ALBUM-REC
               START RCD100 KEY IS NOT LESS ALBUM-REC INVALID KEY
                   MOVE "10" TO ST-RCD100
               END-START
               PERFORM UNTIL ST-RCD100 = "10"
                   READ RCD100 NEXT AT END
                       MOVE "10" TO ST-RCD100
                   NOT AT END
                       IF NR-CONTRATO-CO40 <> ALBUM-REC(1:4)
                          MOVE "10" TO ST-RCD100
                       ELSE
                          ADD TOTAL-REC TO VENDA-CONTRATO-WK
                          INITIALIZE REG-RCD101
                          MOVE ALBUM-REC  TO ALBUM-REC1
                          START RCD101 KEY IS NOT LESS CHAVE-REC1
                                                             INVALID KEY
                               MOVE "10" TO ST-RCD101
                          END-START
                          PERFORM UNTIL ST-RCD101 = "10"
                               READ RCD101 NEXT AT END
                                   MOVE "10" TO ST-RCD101
                               NOT AT END
                                   IF ALBUM-REC <> ALBUM-REC1
                                      MOVE "10" TO ST-RCD101
                                   ELSE
                                      COMPUTE VALOR-COMISSAO-WK =
                                              VALOR-COMISSAO-WK +
                                          (VALOR-REC1 * COMIS-PARC-REC1)
                                   END-IF
                               END-READ
                          END-PERFORM
                       END-IF
                   END-READ
               END-PERFORM

               INITIALIZE REG-RED101 TOTAL-EVENTOS-WK
               MOVE NR-CONTRATO-CO40             TO CONTRATO-R101
               START RED101 KEY IS NOT LESS CONTRATO-R101 INVALID KEY
                   MOVE "10" TO ST-RED101
               END-START
               PERFORM UNTIL ST-RED101 = "10"
                   READ RED101 NEXT AT END
                       MOVE "10" TO ST-RED101
                   NOT AT END
                       IF NR-CONTRATO-CO40 <> CONTRATO-R101
                          MOVE "10" TO ST-RED101
                       ELSE
                          MOVE DOCTO-R101  TO DOCTO-R100
                          READ RCD100 INVALID KEY
                               INITIALIZE REG-RCD100
                          END-READ
                          ADD VLR-TOT-REPORT-R100 TO TOTAL-EVENTOS-WK
                          MOVE "10" TO ST-RED101
                       END-IF
                   END-READ
               END-PERFORM

               MOVE NR-CONTRATO-CO40   TO NR-CONTRATO-CO50
               MOVE ZEROS              TO ITEM-CO50
               START COD050 KEY IS NOT < CHAVE-CO50 INVALID KEY
                     MOVE "10" TO ST-COD050
               END-START
               PERFORM UNTIL ST-COD050 = "10"
                  READ COD050 NEXT RECORD AT END
                       MOVE "10" TO ST-COD050
                  NOT AT END
                     IF NR-CONTRATO-CO50 <> NR-CONTRATO-CO40
                         MOVE "10" TO ST-COD050
                     ELSE
                         IF SUSP-PREV-DEF-CO50 <> 2
                            MOVE CODBRINDE-CO50 TO CODIGO-CO02
                            READ COD002 INVALID KEY
                                 MOVE ZEROS TO VALOR-CO02
                            END-READ

                            MOVE REG-COD050 TO REG-COD051

                            READ COD051 NOT INVALID KEY
                                IF CUSTO-UNIT-CO51 <> ZEROS
                                  MOVE CUSTO-UNIT-CO51 TO CUSTO-PREVISTO
                                ELSE
                                  MOVE VALOR-CO02 TO CUSTO-PREVISTO
                                END-IF
                                IF REALIZADO-CO51 <> 1
                                   IF MULT-FORM-CO02 = 2
                                      COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                                      QTDE-POR-FORM-CO51
                                   ELSE
                                      COMPUTE CUSTO-W =
                                             (QTDE-POR-FORM-CO51
                                      * QTDE-FORM-CO51) * CUSTO-PREVISTO
                                   END-IF
                                ELSE
                                   MOVE VALOR-PAGO-CO51 TO CUSTO-W
                                END-IF

                                ADD CUSTO-W TO BRINDES-WK
                                ADD JUROS-W TO BRINDES-WK
                            END-READ
                         END-IF
                     END-IF
                  END-READ
               END-PERFORM

               WRITE REG-WORK.


       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-FORMANDO.

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES              TO GS-LINDET
           MOVE "CONT."             TO GS-LINDET(1:6)
           MOVE "CIDADE"            TO GS-LINDET(7:13)
           MOVE "        VENDA"     TO GS-LINDET(21:13)
           MOVE "                    CUSTOS" TO GS-LINDET(34:28)
           MOVE " TOTAL"            TO GS-LINDET(83:7)
           MOVE "  RESULT"          TO GS-LINDET(90:8)
           MOVE "   %"              TO GS-LINDET(98:6)
           MOVE "   FOTOS   "       TO GS-LINDET(105:13)
           MOVE "  ALBUNS"          TO GS-LINDET(118:8)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES              TO GS-LINDET
           MOVE ALL "-"             TO GS-LINDET(34:46)
      *    MOVE ALL "-"             TO GS-LINDET(68:7)
           MOVE SPACES              TO GS-LINDET(81:6)
           MOVE ALL "-"             TO GS-LINDET(105:10)
           MOVE ALL "-"             TO GS-LINDET(118:10)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES            TO GS-LINDET
           MOVE SPACES            TO GS-LINDET(1:6)
           MOVE SPACES            TO GS-LINDET(7:13)
           MOVE SPACES            TO GS-LINDET(21:13)
           MOVE "CONT.*"          TO GS-LINDET(34:7)
           MOVE "  MATERIAL"      TO GS-LINDET(41:11)
           MOVE "  EVENTOS*"      TO GS-LINDET(52:11)
           MOVE "  VENDAS"        TO GS-LINDET(63:9)
           MOVE "     ADM"        TO GS-LINDET(72:9)
           MOVE "  CUSTOS"        TO GS-LINDET(81:9)
           MOVE SPACES            TO GS-LINDET(90:7)
           MOVE SPACES            TO GS-LINDET(97:6)
           MOVE "  PROD"          TO GS-LINDET(103:7)
           MOVE "ESTOQ"           TO GS-LINDET(110:6)
           MOVE "  PROD"          TO GS-LINDET(116:7)
           MOVE "ESTOQ"           TO GS-LINDET(123:5)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ALL "-"           TO GS-LINDET(1:127)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


      *    MOVE SPACES            TO GS-LINDET
      *    MOVE "2057"            TO GS-LINDET(1:6)
      *    MOVE "BELO HORIZON"    TO GS-LINDET(7:13)
      *    MOVE "144.087,99"      TO GS-LINDET(21:11)
      *    MOVE "12.019"          TO GS-LINDET(32:7)
      *    MOVE "  17.246"        TO GS-LINDET(39:9)
      *    MOVE "  14.886"        TO GS-LINDET(48:9)
      *    MOVE "15.731"          TO GS-LINDET(57:7)
      *    MOVE "11.022"          TO GS-LINDET(64:7)
      *    MOVE "30.884"          TO GS-LINDET(71:7)
      *    MOVE "13.203"          TO GS-LINDET(78:7)
      *    MOVE "42,75"           TO GS-LINDET(85:6)
      *    MOVE "16.057"          TO GS-LINDET(91:7)
      *    MOVE " 1587"           TO GS-LINDET(98:5)
      *    MOVE " 169"            TO GS-LINDET(104:4)
      *    MOVE "118"             TO GS-LINDET(111:5)
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *
      *
      *    MOVE ALL "-"           TO GS-LINDET(1:113)
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *
      *    MOVE SPACES            TO GS-LINDET
      *    MOVE "TOTAL"           TO GS-LINDET(1:6)
      *    MOVE SPACES            TO GS-LINDET(7:13)
      *    MOVE "144.087,99"      TO GS-LINDET(21:11)
      *    MOVE "12.019"          TO GS-LINDET(32:7)
      *    MOVE "  17.246"        TO GS-LINDET(39:9)
      *    MOVE "  14.886"        TO GS-LINDET(48:9)
      *    MOVE "15.731"          TO GS-LINDET(57:7)
      *    MOVE "11.022"          TO GS-LINDET(64:7)
      *    MOVE "30.884"          TO GS-LINDET(71:7)
      *    MOVE "13.203"          TO GS-LINDET(78:7)
      *    MOVE "42,75"           TO GS-LINDET(85:6)
      *    MOVE "16.057"          TO GS-LINDET(91:7)
      *    MOVE " 1587"           TO GS-LINDET(98:5)
      *    MOVE " 169"            TO GS-LINDET(104:4)
      *    MOVE "118"             TO GS-LINDET(111:5)
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.

           INITIALIZE REG-WORK

           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       MOVER-DADOS-LINDET SECTION.

           MOVE SPACES            TO GS-LINDET
           MOVE CONTRATO-WK       TO GS-LINDET(1:6)
           MOVE CIDADE-WK         TO GS-LINDET(7:13)
           MOVE VENDA-CONTRATO-WK TO MASC-VALOR-12
           MOVE MASC-VALOR-12     TO GS-LINDET(21:13)
           MOVE BRINDES-WK        TO MASC-VALOR-6
           MOVE MASC-VALOR-6      TO GS-LINDET(34:7)
           MOVE MATERIAL-WK       TO MASC-VALOR-10
           MOVE MASC-VALOR-10     TO GS-LINDET(41:11)
           MOVE TOTAL-EVENTOS-WK  TO MASC-VALOR-10
           MOVE MASC-VALOR-10     TO GS-LINDET(52:11)
           MOVE VALOR-COMISSAO-WK TO MASC-VALOR-8
           MOVE MASC-VALOR-8      TO GS-LINDET(63:9)
           MOVE TOTAL-ADM-WK      TO MASC-VALOR-8
           MOVE MASC-VALOR-8      TO GS-LINDET(72:9)
           COMPUTE MASC-VALOR-8 = BRINDES-WK + MATERIAL-WK +
                   TOTAL-EVENTOS-WK + VALOR-COMISSAO-WK + TOTAL-ADM-WK
           MOVE MASC-VALOR-8      TO GS-LINDET(81:9)
           COMPUTE MASC-VALOR-8 = VENDA-CONTRATO-WK -
                   (BRINDES-WK + MATERIAL-WK +
                    TOTAL-EVENTOS-WK + VALOR-COMISSAO-WK + TOTAL-ADM-WK)
           MOVE MASC-VALOR-8      TO GS-LINDET(90:9)
           COMPUTE MASC-VALOR-5 = (VENDA-CONTRATO-WK -
                   (BRINDES-WK + MATERIAL-WK +
                   TOTAL-EVENTOS-WK + VALOR-COMISSAO-WK + TOTAL-ADM-WK))
                   / (BRINDES-WK + MATERIAL-WK +
                    TOTAL-EVENTOS-WK + VALOR-COMISSAO-WK + TOTAL-ADM-WK)
           MOVE MASC-VALOR-5      TO GS-LINDET(99:6).

      *                              GS-LINDET(91:7)
      *                              GS-LINDET(98:5)
      *                              GS-LINDET(104:4)
      *                              GS-LINDET(111:5)







       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 1 MOVE "CURSO        " TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 2 MOVE "CONSULTOR    " TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 3 MOVE "CONTRATO     " TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 4 MOVE "CIDADE       " TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 5 MOVE "VALOR        " TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 6 MOVE "REGIÃO       " TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT-WK INVALID KEY
                          MOVE "10" TO ST-WORK
           END-EVALUATE.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP113" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.

           MOVE SPACES TO REG-RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE CONTRATO-WK       TO LINDET-REL(3:8)
           MOVE CURSO-WK          TO LINDET-REL(10:19)
           MOVE CIDADE-WK         TO LINDET-REL(30:13)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       CABECALHO SECTION.
           MOVE SPACES            TO DET-CAB04
           MOVE "CONTRATO"        TO DET-CAB04(1:8)
           MOVE "CURSO"           TO DET-CAB04(10:20)
           MOVE "CIDADE"          TO DET-CAB04(30:13)
           MOVE "CONSULTOR"       TO DET-CAB04(44:20)
           MOVE "FORM"            TO DET-CAB04(65:7)
           MOVE "FECHOU"          TO DET-CAB04(72:7)
           MOVE "COLAÇÃO"         TO DET-CAB04(79:7)
           MOVE "%RED."           TO DET-CAB04(89:6)
           MOVE "REL.FORM."       TO DET-CAB04(95:7)
           MOVE "%RED."           TO DET-CAB04(105:7)
           MOVE "REL.ATIV."       TO DET-CAB04(111:7)
           MOVE "%RED."           TO DET-CAB04(121:5)

           MOVE GS-MESANO-INI  TO MESANO-REL1
           MOVE GS-MESANO-FIM  TO MESANO-REL2

           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.

           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 1
           WRITE REG-RELAT FROM CAB03 AFTER 1
           WRITE REG-RELAT FROM CAB04 AFTER 1.
           WRITE REG-RELAT FROM CAB03 AFTER 1.
           MOVE 5 TO LIN.

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
           CLOSE CAD010 COD040 IED011 MTD019 WORK MTD025 COD008 COD060
                 RED101 CGD001 RCD100 RCD101 COD002 COD050 COD051
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
