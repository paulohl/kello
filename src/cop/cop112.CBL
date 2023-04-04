       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP112.
      *DATA: 29-09-2005
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
           COPY CAPX004.
           COPY CGPX001.
           COPY IEPX011.
           COPY MTPX019.
           COPY MTPX025.
           COPY COPX008.
           COPY COPX040.
           COPY COPX060.
           COPY REPX101.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS ALT-WK = MESANO-WK CONTRATO-WK
                  ALTERNATE RECORD KEY IS ALT1-WK = MESANO-WK
                       CURSO-WK CONTRATO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK = MESANO-WK
                       CONSULTOR-WK CONTRATO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-WK = MESANO-WK
                       NR-COLACAO-WK CONTRATO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-WK = MESANO-WK
                       NR-ATIVIDADES-WK CONTRATO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT5-WK = MESANO-WK
                       CIDADE-WK CONTRATO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-WK = MESANO-WK
                       NR-INICIAL-WK CONTRATO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT7-WK = MESANO-WK
                       REL-FORM-WK CONTRATO-WK WITH DUPLICATES.

           SELECT WORK2 ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS CHAVE-WK2 = NOME-CURSO-WK2.


           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA
                        ORGANIZATION IS LINE SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL
                        FILE STATUS IS FS-RELAT.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW004.
       COPY CGPW001.
       COPY IEPW011.
       COPY MTPW019.
       COPY MTPW025.
       COPY COPW008.
       COPY COPW040.
       COPY COPW060.
       COPY REPW101.
       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  CIDADE-WK           PIC X(13).
           05  CURSO-WK            PIC X(30).
           05  CONSULTOR-WK        PIC X(30).
           05  MESANO-WK           PIC 9(6).
           05  NR-INICIAL-WK       PIC 9(5).
           05  NR-COLACAO-WK       PIC 9(5).
           05  NR-ATIVIDADES-WK    PIC 9(5).
           05  REL-FORM-WK         PIC 9(5).

       FD  WORK2.
       01  REG-WORK2.
           05 COD-CURSO-WK2             PIC 999.
           05 NOME-CURSO-WK2            PIC X(12).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP112.CPB".
           COPY "COP112.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD025             PIC XX       VALUE SPACES.
           05  ST-COD008             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
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
           05  CONTRATO-REL          PIC 9(4)    VALUE ZEROS.
           05  AUX-DATA              PIC 9(08).
           05  MESANO-INI            PIC 9(06)    VALUE ZEROS.
           05  MESANO-FIM            PIC 9(06)    VALUE ZEROS.
           05  PERCENTUAL            PIC S9(3)V99 VALUE ZEROS.
           05  QTDE-E2               PIC ZZZ,ZZ-  BLANK WHEN ZEROS.
           05  TOTP-INICIAL          PIC 9(06)    VALUE ZEROS.
           05  TOTAL-INICIAL         PIC 9(06)    VALUE ZEROS.
           05  TOTP-COLACAO          PIC 9(06)    VALUE ZEROS.
           05  TOTAL-COLACAO         PIC 9(06)    VALUE ZEROS.
           05  TOTP-FORMANDOS        PIC 9(06)    VALUE ZEROS.
           05  TOTAL-FORMANDOS       PIC 9(06)    VALUE ZEROS.
           05  TOTP-ATIVIDADES       PIC 9(06)    VALUE ZEROS.
           05  TOTAL-ATIVIDADES      PIC 9(06)    VALUE ZEROS.

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
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD025"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD025.
           MOVE "COD008"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD008
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           COMPUTE VARIA-W2 = VARIA-W + 10

           OPEN INPUT CAD010 IED011 MTD019 COD040 COD008 MTD025 COD060
                      RED101 CGD001.
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
           IF ST-COD008 <> "00"
              MOVE "ERRO ABERTURA COD008: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD008 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED101 <> "00"
              MOVE "ERRO ABERTURA RED101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM VERIFICAR-SENHA-STATUS
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

       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB9
           NOT INVALID KEY
               ENABLE-OBJECT PB9.

           CLOSE CAD004.

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
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
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

           MOVE REPRESENTANTE-CO40  TO CODIGO-CG01
           READ CGD001 INVALID KEY
               MOVE "************"  TO CONSULTOR-WK
           NOT INVALID KEY
               MOVE NOME-CG01       TO CONSULTOR-WK
           END-READ

           MOVE NR-CONTRATO-CO40    TO GS-EXIBE-MOVTO
           MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE NR-CONTRATO-CO40    TO CONTRATO-WK
           MOVE CIDADE-CO40         TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-CID
           END-READ
           MOVE NOME-CID            TO CIDADE-WK
           INITIALIZE REG-MTD019 REL-FORM-WK CURSO-WK
           MOVE NR-CONTRATO-CO40 TO CONTRATO-MT19
           START MTD019 KEY IS NOT LESS ALBUMMT19
                                            INVALID KEY
              MOVE "10" TO ST-MTD019
           END-START
           PERFORM UNTIL ST-MTD019 = "10"
              READ MTD019 NEXT AT END
                  MOVE "10" TO ST-MTD019
              NOT AT END
                  IF NR-CONTRATO-CO40 <> CONTRATO-MT19
                     MOVE "10" TO ST-MTD019
                  ELSE
                      IF SEQ-MT19 > 0
                         ADD 1 TO REL-FORM-WK
                      END-IF
                      IF CURSO-MT19 > 0 AND CURSO-WK = SPACES
                         MOVE CURSO-MT19          TO CODIGO-IE11
                         READ IED011 INVALID KEY
                              MOVE SPACES TO NOME-IE11
                         END-READ
                         MOVE NOME-IE11          TO CURSO-WK
      *                  MOVE "10" TO ST-MTD019
                      END-IF
                  END-IF
              END-READ
           END-PERFORM
           INITIALIZE REG-COD060 NR-COLACAO-WK
           MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO60
           START COD060 KEY IS NOT LESS CHAVE-CO60
                                            INVALID KEY
              MOVE "10" TO ST-COD060
           END-START
           PERFORM UNTIL ST-COD060 = "10"
              READ COD060 NEXT AT END
                  MOVE "10" TO ST-COD060
              NOT AT END
                  IF NR-CONTRATO-CO40 <> NR-CONTRATO-CO60
                     MOVE "10" TO ST-COD060
                  ELSE
                     IF CODEVENTO-CO60 = 80
                        ADD QT-PARTICIPANTE-CO60 TO NR-COLACAO-WK
                     END-IF
                  END-IF
              END-READ
           END-PERFORM

           INITIALIZE REG-RED101 NR-ATIVIDADES-WK
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
                      IF EVENTO-R101 = 80
                         ADD QT-PARTIC-R101 TO NR-ATIVIDADES-WK
                      END-IF
                   END-IF
               END-READ
           END-PERFORM

           MOVE MESANO-PREV-CO40             TO MESANO-WK
           MOVE QTDE-FORM-INI-CO40           TO NR-INICIAL-WK
      *    MOVE QTDE-FORM-CO40               TO NR-COLACAO-WK

           WRITE REG-WORK.

       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-FORMANDO
                         TOTP-INICIAL
                         TOTAL-INICIAL
                         TOTP-COLACAO
                         TOTAL-COLACAO
                         TOTP-FORMANDOS
                         TOTAL-FORMANDOS
                         TOTP-ATIVIDADES
                         TOTAL-ATIVIDADES.

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES            TO GS-LINDET
           MOVE "CONTRATO"        TO GS-LINDET(1:8)
           MOVE "CURSO"           TO GS-LINDET(10:20)
           MOVE "CIDADE"          TO GS-LINDET(30:13)
           MOVE "CONSULTOR"       TO GS-LINDET(44:20)
           MOVE "FORM"            TO GS-LINDET(65:7)
           MOVE "FECHOU"          TO GS-LINDET(72:7)
           MOVE "COLACAO"         TO GS-LINDET(79:7)
           MOVE "%RED."           TO GS-LINDET(89:6)
           MOVE "REL.FORM."       TO GS-LINDET(95:7)
           MOVE "%RED."           TO GS-LINDET(105:7)
           MOVE "REL.ATIV."       TO GS-LINDET(111:7)
           MOVE "%RED."           TO GS-LINDET(121:5)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ALL "-"           TO GS-LINDET(1:125)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS TO AUX-MESANO

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
           END-PERFORM

           MOVE SPACES            TO GS-LINDET
           MOVE "T O T A L . . ." TO GS-LINDET(1:15)
           MOVE TOTP-INICIAL      TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(71:7)
           MOVE TOTP-COLACAO      TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(79:7)
           COMPUTE PERCENTUAL =((TOTP-COLACAO / TOTP-INICIAL) - 1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO GS-LINDET(87:7)

           MOVE TOTP-FORMANDOS    TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(95:7)

           COMPUTE PERCENTUAL =((TOTP-FORMANDOS/ TOTP-INICIAL) -
           1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO GS-LINDET(103:7)

           MOVE TOTP-ATIVIDADES  TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(111:7)
           COMPUTE PERCENTUAL =((TOTP-ATIVIDADES / TOTP-INICIAL) - 1)
                                     * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO GS-LINDET(119:7)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES            TO GS-LINDET
           MOVE "T O T A L   G E R A L . . ." TO GS-LINDET(1:35)
           MOVE TOTAL-INICIAL      TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(71:7)
           MOVE TOTAL-COLACAO      TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(79:7)
           COMPUTE PERCENTUAL =((TOTAL-COLACAO / TOTAL-INICIAL) - 1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO GS-LINDET(87:7)

           MOVE TOTAL-FORMANDOS    TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(95:7)

           COMPUTE PERCENTUAL =((TOTAL-FORMANDOS/ TOTAL-INICIAL) -
           1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO GS-LINDET(103:7)

           MOVE TOTAL-ATIVIDADES  TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(111:7)
           COMPUTE PERCENTUAL =((TOTAL-ATIVIDADES / TOTAL-INICIAL) - 1)
                                     * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO GS-LINDET(119:7)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-LINDET SECTION.
           IF AUX-MESANO <> MESANO-WK
              IF AUX-MESANO <> 0
                 MOVE SPACES            TO GS-LINDET
                 MOVE "T O T A L . . ." TO GS-LINDET(1:15)
                 MOVE TOTP-INICIAL      TO QTDE-E
                 MOVE QTDE-E            TO GS-LINDET(71:7)
                 MOVE TOTP-COLACAO      TO QTDE-E
                 MOVE QTDE-E            TO GS-LINDET(79:7)
                 COMPUTE PERCENTUAL =((TOTP-COLACAO / TOTP-INICIAL) - 1)
                                      * 100
                 MOVE PERCENTUAL        TO QTDE-E2
                 MOVE QTDE-E2           TO GS-LINDET(87:7)

                 MOVE TOTP-FORMANDOS    TO QTDE-E
                 MOVE QTDE-E            TO GS-LINDET(95:7)

                 COMPUTE PERCENTUAL =((TOTP-FORMANDOS/ TOTP-INICIAL) -
                 1)
                                      * 100
                 MOVE PERCENTUAL        TO QTDE-E2
                 MOVE QTDE-E2           TO GS-LINDET(103:7)

                 MOVE TOTP-ATIVIDADES  TO QTDE-E
                 MOVE QTDE-E            TO GS-LINDET(111:7)
                COMPUTE PERCENTUAL =((TOTP-ATIVIDADES / TOTP-INICIAL) -
                1)
                                     * 100
                 MOVE PERCENTUAL        TO QTDE-E2
                 MOVE QTDE-E2           TO GS-LINDET(119:7)
                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM

                 MOVE SPACES TO GS-LINDET
                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 MOVE ZEROS TO TOTP-INICIAL
                               TOTP-COLACAO
                               TOTP-FORMANDOS
                               TOTP-ATIVIDADES
              END-IF
              MOVE MESANO-WK TO AUX-MESANO.

           MOVE SPACES            TO GS-LINDET
           MOVE CONTRATO-WK       TO GS-LINDET(3:8)
           MOVE CURSO-WK          TO GS-LINDET(10:19)
           MOVE CIDADE-WK         TO GS-LINDET(30:13)
           MOVE CONSULTOR-WK      TO GS-LINDET(44:20)
           STRING MESANO-WK(5:2) "/" MESANO-WK(3:2) INTO GS-LINDET(65:7)
           ADD NR-INICIAL-WK      TO TOTP-INICIAL
           ADD NR-INICIAL-WK      TO TOTAL-INICIAL
           MOVE NR-INICIAL-WK     TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(71:7)
           ADD  NR-COLACAO-WK     TO TOTP-COLACAO
           ADD  NR-COLACAO-WK     TO TOTAL-COLACAO
           MOVE NR-COLACAO-WK     TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(79:7)
           COMPUTE PERCENTUAL =((NR-COLACAO-WK / NR-INICIAL-WK) - 1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO GS-LINDET(87:7)

           ADD REL-FORM-WK        TO TOTP-FORMANDOS
           ADD REL-FORM-WK        TO TOTAL-FORMANDOS

           MOVE REL-FORM-WK       TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(95:7)
           COMPUTE PERCENTUAL =((REL-FORM-WK / NR-INICIAL-WK) - 1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO GS-LINDET(103:7)

           ADD NR-ATIVIDADES-WK   TO TOTP-ATIVIDADES
           ADD NR-ATIVIDADES-WK   TO TOTAL-ATIVIDADES

           MOVE NR-ATIVIDADES-WK  TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(111:7)
           COMPUTE PERCENTUAL =((NR-ATIVIDADES-WK / NR-INICIAL-WK) - 1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO GS-LINDET(119:7).

       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 0 MOVE "CONTRATO     " TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 1 MOVE "CURSO" TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT1-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 2 MOVE "CONSULTOR" TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT2-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 3 MOVE "COLAÇÃO" TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT3-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 4 MOVE "RELAT.ATIVIDADE" TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT4-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 5 MOVE "CIDADE" TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT5-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 6 MOVE "FECHOU" TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT6-WK INVALID KEY
                          MOVE "10" TO ST-WORK
             WHEN 7 MOVE "RELAT.FORMANDOS" TO GS-DESCR-ORDEM
                    START WORK KEY IS NOT < ALT7-WK INVALID KEY
                          MOVE "10" TO ST-WORK
           END-EVALUATE.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP112" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO TOT-FORMANDO
                         TOTP-INICIAL
                         TOTAL-INICIAL
                         TOTP-COLACAO
                         TOTAL-COLACAO
                         TOTP-FORMANDOS
                         TOTAL-FORMANDOS
                         TOTP-ATIVIDADES
                         TOTAL-ATIVIDADES.

           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.

           MOVE SPACES            TO LINDET-REL
           MOVE "T O T A L . . ." TO LINDET-REL(1:15)
           MOVE TOTP-INICIAL      TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(71:7)
           MOVE TOTP-COLACAO      TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(79:7)
           COMPUTE PERCENTUAL =((TOTP-COLACAO / TOTP-INICIAL) - 1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO LINDET-REL(87:7)

           MOVE TOTP-FORMANDOS    TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(95:7)

           COMPUTE PERCENTUAL =((TOTP-FORMANDOS/ TOTP-INICIAL) -
           1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO LINDET-REL(103:7)

           MOVE TOTP-ATIVIDADES  TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(111:7)
           COMPUTE PERCENTUAL =((TOTP-ATIVIDADES / TOTP-INICIAL) - 1)
                                     * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO LINDET-REL(119:7)

           ADD 1 TO LIN
           WRITE REG-RELAT FROM LINDET
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           ADD 1 TO LIN
           MOVE SPACES TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           MOVE SPACES            TO LINDET-REL
           MOVE "T O T A L   G E R A L . . ." TO LINDET-REL(1:35)
           MOVE TOTAL-INICIAL      TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(71:7)
           MOVE TOTAL-COLACAO      TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(79:7)
           COMPUTE PERCENTUAL =((TOTAL-COLACAO / TOTAL-INICIAL) - 1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO LINDET-REL(87:7)

           MOVE TOTAL-FORMANDOS    TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(95:7)

           COMPUTE PERCENTUAL =((TOTAL-FORMANDOS/ TOTAL-INICIAL) -
           1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO LINDET-REL(103:7)

           MOVE TOTAL-ATIVIDADES  TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(111:7)
           COMPUTE PERCENTUAL =((TOTAL-ATIVIDADES / TOTAL-INICIAL) - 1)
                                     * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO LINDET-REL(119:7)

           WRITE REG-RELAT FROM LINDET.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           IF AUX-MESANO <> MESANO-WK
              IF AUX-MESANO <> 0
                 MOVE SPACES            TO LINDET-REL
                 MOVE "T O T A L . . ." TO LINDET-REL(1:15)
                 MOVE TOTP-INICIAL      TO QTDE-E
                 MOVE QTDE-E            TO LINDET-REL(71:7)
                 MOVE TOTP-COLACAO      TO QTDE-E
                 MOVE QTDE-E            TO LINDET-REL(79:7)
                 COMPUTE PERCENTUAL =((TOTP-COLACAO / TOTP-INICIAL) - 1)
                                      * 100
                 MOVE PERCENTUAL        TO QTDE-E2
                 MOVE QTDE-E2           TO LINDET-REL(87:7)

                 MOVE TOTP-FORMANDOS    TO QTDE-E
                 MOVE QTDE-E            TO LINDET-REL(95:7)

                 COMPUTE PERCENTUAL =((TOTP-FORMANDOS/ TOTP-INICIAL) -
                 1)
                                      * 100
                 MOVE PERCENTUAL        TO QTDE-E2
                 MOVE QTDE-E2           TO LINDET-REL(103:7)

                 MOVE TOTP-ATIVIDADES  TO QTDE-E
                 MOVE QTDE-E            TO LINDET-REL(111:7)
                COMPUTE PERCENTUAL =((TOTP-ATIVIDADES / TOTP-INICIAL) -
                1)
                                     * 100
                 MOVE PERCENTUAL        TO QTDE-E2
                 MOVE QTDE-E2           TO LINDET-REL(119:7)
                 ADD 1 TO LIN

                 WRITE REG-RELAT FROM LINDET
                 IF LIN > 56
                    PERFORM CABECALHO
                 END-IF

                 ADD 1 TO LIN
                 MOVE SPACES TO LINDET-REL
                 IF LIN > 56
                    PERFORM CABECALHO
                 END-IF
                 WRITE REG-RELAT FROM LINDET

                 MOVE ZEROS TO TOTP-INICIAL
                               TOTP-COLACAO
                               TOTP-FORMANDOS
                               TOTP-ATIVIDADES
              END-IF
              MOVE MESANO-WK TO AUX-MESANO.

           MOVE SPACES            TO LINDET-REL
           MOVE CONTRATO-WK       TO LINDET-REL(3:8)
           MOVE CURSO-WK          TO LINDET-REL(10:19)
           MOVE CIDADE-WK         TO LINDET-REL(30:13)
           MOVE CONSULTOR-WK      TO LINDET-REL(44:20)
           STRING MESANO-WK(5:2) "/" MESANO-WK(3:2) INTO
                  LINDET-REL(65:7)

           ADD NR-INICIAL-WK      TO TOTP-INICIAL
           ADD NR-INICIAL-WK      TO TOTAL-INICIAL

           MOVE NR-INICIAL-WK     TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(71:7)

           ADD NR-COLACAO-WK      TO TOTP-COLACAO
           ADD NR-COLACAO-WK      TO TOTAL-COLACAO

           MOVE NR-COLACAO-WK     TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(79:7)

           COMPUTE PERCENTUAL =((NR-COLACAO-WK / NR-INICIAL-WK) - 1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO LINDET-REL(87:7)

           ADD REL-FORM-WK        TO TOTP-FORMANDOS
           ADD REL-FORM-WK        TO TOTAL-FORMANDOS

           MOVE REL-FORM-WK       TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(95:7)

           COMPUTE PERCENTUAL =((REL-FORM-WK / NR-INICIAL-WK) - 1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO LINDET-REL(103:7)

           ADD NR-ATIVIDADES-WK   TO TOTP-ATIVIDADES
           ADD NR-ATIVIDADES-WK   TO TOTAL-ATIVIDADES

           MOVE NR-ATIVIDADES-WK  TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(111:7)
           COMPUTE PERCENTUAL =((NR-ATIVIDADES-WK / NR-INICIAL-WK) - 1)
                                * 100
           MOVE PERCENTUAL        TO QTDE-E2
           MOVE QTDE-E2           TO LINDET-REL(119:7).
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
           MOVE "COLACAO"         TO DET-CAB04(79:7)
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

           WRITE REG-RELAT FROM CAB02 AFTER 2
           WRITE REG-RELAT FROM CAB03 AFTER 1
           WRITE REG-RELAT FROM CAB04 AFTER 1.
           WRITE REG-RELAT FROM CAB03 AFTER 1.
           MOVE 7 TO LIN.

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
                 RED101 CGD001
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
