       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP101.
      *DATA: 20/08/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Relação de Contrato
      *FUNÇÃO: Listar todos os contratos c/ status => 50 dentro
      *        do "mes/ano previsto" solicitado
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CAPX004.
           COPY COPX001.
           COPY COPX005.
           COPY COPX040.
           COPY COPX041.
           COPY COPX049.
           COPY COPX101.
           COPY IEPX010.
           COPY IEPX011.
           COPY CAPX010.
           COPY CAPX012.
           COPY MTPX019.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CONTRATO-WK
                  ALTERNATE RECORD KEY IS CURSO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS INSTITUICAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PREPOSTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS REPRESENT-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PADRAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MESANO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS STATUS-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CAPW004.
       COPY IEPW010.
       COPY IEPW011.
       COPY CAPW010.
       COPY CAPW012.
       COPY COPW001.
       COPY COPW005.
       COPY COPW040.
       COPY COPW041.
       COPY COPW049.
       COPY COPW101.
       COPY MTPW019.
       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  CURSO-WK            PIC X(15).
           05  INSTITUICAO-WK      PIC X(11).
           05  CIDADE-WK           PIC X(12).
           05  PREPOSTO-WK         PIC X(10).
           05  REPRESENT-WK        PIC X(13).
           05  QT-FORM-WK          PIC 9(4).
           05  PADRAO-WK           PIC X.
           05  QT-FOTOS-WK         PIC 9(5).
           05  MESANO-WK           PIC 9(6).
           05  COD-STATUS-WK       PIC 9(2).
           05  STATUS-WK           PIC X(13).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP101.CPB".
           COPY "COP101.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD041             PIC XX       VALUE SPACES.
           05  ST-COD049             PIC XX       VALUE SPACES.
           05  ST-COD101             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  OK                    PIC X(01)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(8)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  MESANO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  CURSO-ANT             PIC X(15)    VALUE SPACES.
           05  INSTITUICAO-ANT       PIC X(11)    VALUE SPACES.
           05  CIDADE-ANT            PIC X(12)    VALUE SPACES.
           05  PREPOSTO-ANT          PIC X(10)    VALUE SPACES.
           05  REPRESENT-ANT         PIC X(13)    VALUE SPACES.
           05  PADRAO-ANT            PIC X        VALUE SPACES.
           05  MESANO-ANT            PIC 9(6)     VALUE ZEROS.
           05  STATUS-ANT            PIC X(13)    VALUE SPACES.
           05  QT-CURSO              PIC 9        VALUE ZEROS.
           05  CURSO-W               PIC X(15)    VALUE SPACES.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.
           05  DATAINI               PIC 9(8)     VALUE ZEROS.
           05  DATAFIM               PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FORM            PIC 9(6)     VALUE ZEROS.
           05  TOTAL-FOTOS           PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FORM-G          PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FOTOS-G         PIC 9(9)     VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  CONTINUAR             PIC X(01)    VALUE SPACES.
           05  ACHEI                 PIC X(01)    VALUE SPACES.
           05  QTDE-NAO              PIC 9(03)    VALUE ZEROS.
           05  QTDE-REC              PIC 9(03)    VALUE ZEROS.

           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(29)   VALUE
           "RELACAO DE CONTRATO - ORDEM: ".
           05  ORDEM-REL           PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(09)   VALUE "MES/ANO: ".
           05  MESANO-INI-REL      PIC 99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MESANO-FIM-REL      PIC 99/9999.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(12)
               VALUE "ASSINATURA: ".
           05  DATAINI-REL         PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  DATAFIM-REL         PIC 99/99/9999.

       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(107)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(107)  VALUE
           "CONT CURSO           INSTITUICAO CIDADE       REGIAO     REP
      -    "RESENT     FORM PAD FOTOS MES/ANO STATUS       ".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(107)  VALUE SPACES.

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
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "IED010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD041"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD041.
           MOVE "COD049"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD049.
           MOVE "COD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD101.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           OPEN INPUT CAD010 CAD012 IED010 IED011 COD001 COD040
                      COD005 CGD001 MTD019 COD049 COD041
           OPEN I-O   COD101
           CLOSE      COD101
           OPEN INPUT COD101.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD005 <> "00"
              MOVE "ERRO ABERTURA COD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD041 <> "00"
              MOVE "ERRO ABERTURA COD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD041 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD049 <> "00"
              MOVE "ERRO ABERTURA COD049: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD049 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD101 <> "00"
              MOVE "ERRO ABERTURA COD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD101 TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-REGIAO-TRUE
                    PERFORM LE-REGIAO
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM CHAMAR-POPUP-REGIAO
               WHEN GS-LE-REPRES-TRUE
                    PERFORM LE-REPRES
               WHEN GS-CHAMAR-POP-REPRES-TRUE
                    PERFORM CHAMAR-POPUP-REPRES
               WHEN GS-LE-STATUS-TRUE
                    PERFORM LE-STATUS
               WHEN GS-CHAMAR-POP-STATUS-TRUE
                    PERFORM CHAMAR-POPUP-STATUS
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR
               WHEN GS-CARREGAR-STATUS-TRUE
                    PERFORM CARREGAR-STATUS
               WHEN GS-GRAVA-STATUS-TRUE
                    PERFORM GRAVA-STATUS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB12
           NOT INVALID KEY
               ENABLE-OBJECT PB12.

           CLOSE CAD004.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GRAVA-STATUS SECTION.
           CLOSE    COD101
           OPEN I-O COD101

           INITIALIZE REG-COD101
           START COD101 KEY IS NOT LESS CODIGO-COP101 INVALID KEY
                MOVE "10" TO ST-COD101.
           PERFORM UNTIL ST-COD101 = "10"
                READ COD101 NEXT AT END
                     MOVE "10" TO ST-COD101
                NOT AT END
                     DELETE COD101 INVALID KEY
                         MOVE "Erro de Exclusão...COD101" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                     END-DELETE
                END-READ
           END-PERFORM

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-COP101
               WRITE REG-COD101
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      COD101
           OPEN INPUT COD101.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-COD101
           START COD101 KEY IS NOT LESS CODIGO-COP101 INVALID KEY
               MOVE "10" TO ST-COD101.

           PERFORM UNTIL ST-COD101 = "10"
               READ COD101 NEXT AT END
                    MOVE "10" TO ST-COD101
               NOT AT END
                    MOVE CODIGO-COP101 TO CODIGO-CO01
                    READ COD001 NOT INVALID KEY
                         MOVE "S"              TO ACHEI
                         MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                         MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                         MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                    END-READ
               END-READ
           END-PERFORM

           IF ACHEI = "N"
              CLOSE      COD101
              OPEN I-O   COD101
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO CODIGO-COP101
                        WRITE REG-COD101

                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM
              CLOSE      COD101
              OPEN INPUT COD101.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.

       INCLUIR SECTION.
           MOVE "Você Deseja Incluir o Status?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE GS-STATUS        TO GS-LINHA-STATUS(1:2)
              MOVE GS-DESC-STATUS   TO GS-LINHA-STATUS(4:30)
              MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       LE-STATUS SECTION.
           MOVE GS-STATUS              TO CODIGO-CO01
           READ COD001 INVALID KEY
                MOVE SPACES            TO STATUS-CO01
           END-READ
           MOVE STATUS-CO01            TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       CHAMAR-POPUP-STATUS SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP001T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       VERIFICAR-IGUAL SECTION.
           MOVE 0   TO GS-FLAG-CRITICA
           MOVE "N" TO ACHEI
           MOVE 1   TO GS-CONT
           MOVE SPACES TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               IF GS-LINHA-STATUS(1:2) = GS-STATUS
                  MOVE "S" TO ACHEI
                  EXIT PERFORM
               ELSE
                  ADD 1 TO GS-CONT
                  MOVE SPACES TO GS-LINHA-STATUS
                  MOVE "LER-LINHA" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               END-IF
           END-PERFORM

           IF ACHEI = "S"
              MOVE "Status já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.


       LE-REPRES SECTION.
           MOVE GS-REPRESENTANTE       TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES            TO NOME-CG01
           END-READ
           MOVE NOME-CG01              TO GS-DESC-REPRESENTANTE.

       CHAMAR-POPUP-REPRES SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-REPRESENTANTE.
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-REPRESENTANTE.

       CHAMAR-POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2)  TO GS-REGIAO.
           MOVE PASSAR-STRING-1(1: 30)  TO GS-DESC-REGIAO.

       LE-REGIAO SECTION.
           MOVE GS-REGIAO       TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG.
           MOVE NOME-REG        TO GS-DESC-REGIAO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       VERIFICAR-MTD019 SECTION.
           INITIALIZE REG-MTD019
           MOVE "N" TO ACHEI
           MOVE NR-CONTRATO-CO40 TO CONTRATO-MT19
           START MTD019 KEY IS NOT LESS ALBUMMT19 INVALID KEY
               MOVE "10" TO ST-MTD019
               IF GS-NAO-DIGITADOS = 1
                  MOVE "S" TO CONTINUAR.

           PERFORM UNTIL ST-MTD019 = "10"
               READ MTD019 NEXT AT END
                   MOVE "10" TO ST-MTD019
               NOT AT END
                   IF NR-CONTRATO-CO40 <> CONTRATO-MT19
                      MOVE "10" TO ST-MTD019
                      IF GS-NAO-DIGITADOS = 1 AND ACHEI = "N"
                         MOVE "S" TO CONTINUAR
                      END-IF
                   ELSE
                      MOVE "S" TO ACHEI
                      IF GS-DIGITADOS = 1
                         MOVE "S" TO CONTINUAR
                         IF GS-NAO-DIGITADOS = 0
                            MOVE "10" TO ST-MTD019
                         END-IF
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME.

           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE GS-MESANO-INI      TO MESANO-INI-REL
           MOVE GS-MESANO-FIM      TO MESANO-FIM-REL
           MOVE GS-DATA-INI        TO DATAINI-REL
           MOVE GS-DATA-FIM        TO DATAFIM-REL

           INITIALIZE REG-COD040

           EVALUATE GS-OP-DATA
               WHEN 1 MOVE GS-MESANO-INI  TO MESANO-W
                                             MESANO-INI-ANT
                                             MESANO-INI-REL
                      MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
                      MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
                      MOVE MESANO-I       TO MESANO-INI


                      MOVE GS-MESANO-FIM  TO MESANO-W
                                             MESANO-FIM-REL
                      MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
                      MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
                      MOVE MESANO-I       TO MESANO-FIM

                      MOVE MESANO-INI     TO MESANO-PREV-CO40
                      MOVE ZEROS          TO NR-CONTRATO-CO40
                      START COD040 KEY IS NOT < ALT1-CO40 INVALID KEY
                           MOVE "10" TO ST-COD040
                      END-START
               WHEN 2 STRING GS-DATA-INI(5:4)
                             GS-DATA-INI(3:2)
                             GS-DATA-INI(1:2) INTO DATAINI
                      MOVE   DATAINI            TO ASSINATURA-CO40
                      MOVE   GS-DATA-INI        TO DATAINI-REL

                      STRING GS-DATA-FIM(5:4)
                             GS-DATA-FIM(3:2)
                             GS-DATA-FIM(1:2) INTO DATAFIM
                      MOVE   GS-DATA-FIM        TO DATAFIM-REL
                      START COD040 KEY IS NOT < ASSINATURA-CO40
                                                             INVALID KEY
                           MOVE "10" TO ST-COD040
                      END-START
           END-EVALUATE

           PERFORM UNTIL ST-COD040 = "10"
                READ COD040 NEXT RECORD AT END
                     MOVE "10" TO ST-COD040
                NOT AT END
                    IF GS-REPRESENTANTE = 0 OR REPRESENTANTE-CO40
                       PERFORM LER-TURMAS
                       IF OK = "S"
                          MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO49
                          READ COD049 INVALID KEY
                               INITIALIZE REG-COD049
                          END-READ
                          IF CANCELADO-CO49 IS NOT NUMERIC
                             MOVE 0 TO CANCELADO-CO49
                          END-IF
                          IF GS-ENCERRADOS = CANCELADO-CO49
                          IF GS-INATIVO = 0 OR GS-ATIVO = 0
      *                 IF STATUS-CO40 < 50 AND GS-INATIVO = ZEROS OR
      *                    STATUS-CO40 NOT < 50 AND GS-ATIVO = ZEROS
                             CONTINUE
                          ELSE
                             EVALUATE GS-OP-DATA
                               WHEN 1
                                    IF MESANO-PREV-CO40 > MESANO-FIM
                                       MOVE "10" TO ST-COD040
                                    END-IF
                               WHEN 2
                                    IF ASSINATURA-CO40 > DATAFIM
                                       MOVE "10" TO ST-COD040
                                    END-IF
                             END-EVALUATE
                             IF ST-COD040 <> "10"
                                MOVE "N" TO CONTINUAR
                                IF GS-NAO-DIGITADOS = 1 OR
                                   GS-DIGITADOS = 1
                                   PERFORM VERIFICAR-MTD019
                                ELSE
                                   MOVE "S" TO CONTINUAR
                                END-IF
                                IF CONTINUAR = "S"
                                   MOVE CIDADE-CO40 TO CIDADE
                                   READ CAD010 INVALID KEY
                                        MOVE SPACES TO NOME-CID
                                        MOVE 0      TO REGIAO-CID
                                        MOVE "**"   TO UF-CID
                                   END-READ
                                   MOVE NOME-CID    TO CIDADE-WK

                                   IF GS-UF = SPACES OR UF-CID
                                      IF GS-REGIAO = 0 OR REGIAO-CID
                                         PERFORM PESQUISAR-STATUS
                                         IF ACHEI = "S"
      *                                  MOVE STATUS-CO40
      *                                    TO CODIGO-COP101
      *                                  READ COD101 NOT INVALID KEY
                                              IF CANCELADO-CO49 = 1
                                                 MOVE 49 TO STATUS-CO40
                                              END-IF

                                              MOVE MESANO-PREV-CO40
                                                TO MESANO-WK
                                                   GS-EXIBE-VENCTO
                                              MOVE NR-CONTRATO-CO40
                                                TO CONTRATO-WK
                                              MOVE INSTITUICAO-CO40
                                                TO CODIGO-IE10
                                              READ IED010 INVALID KEY
                                                   MOVE SPACES
                                                     TO NOME-IE10
                                              END-READ
                                              MOVE NOME-IE10
                                                TO INSTITUICAO-WK

                                              MOVE PREPOSTO-CO49
                                                TO CODIGO-CG01
                                              READ CGD001 INVALID KEY
                                                   MOVE "********"
                                                     TO PREPOSTO-WK
                                              NOT INVALID KEY
                                                   MOVE NOME-CG01
                                                     TO PREPOSTO-WK
                                              END-READ

                                              MOVE STATUS-CO40
                                                TO COD-STATUS-WK
                                                   CODIGO-CO01
                                              READ COD001 INVALID KEY
                                                   MOVE SPACES TO
                                                        STATUS-CO01
                                              END-READ
                                              IF STATUS-CO40 = 49
                                                 MOVE "ENCERRADO"
                                                   TO STATUS-CO01
                                              END-IF
                                              MOVE STATUS-CO01
                                                TO STATUS-WK
                                              MOVE REPRESENTANTE-CO40
                                                TO CODIGO-CG01
                                              READ CGD001 INVALID KEY
                                                   MOVE SPACES TO
                                                        NOME-CG01
                                              END-READ
                                              MOVE NOME-CG01      TO
                                                   REPRESENT-WK
                                              MOVE QTDE-FORM-CO40
                                                TO QT-FORM-WK
                                              MOVE IDENTIFICACAO-CO40
                                                TO CURSO-WK
                                              MOVE PADRAO-CO40
                                                TO PADRAO-CO05
                                                   PADRAO-WK
                                              READ COD005 INVALID KEY
                                                   MOVE ZEROS TO
                                                        PREV-FOTOS-CO05
                                              END-READ
                                              COMPUTE QT-FOTOS-WK =
                                                      PREV-FOTOS-CO05 *
                                                      QT-FORM-WK
                                              MOVE "TELA-AGUARDA1" TO
                                                    DS-PROCEDURE
                                              PERFORM CALL-DIALOG-SYSTEM
                                              WRITE REG-WORK
                                         END-IF
                                      END-IF
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                          END-IF
                       END-IF
                    END-IF
                END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       LER-TURMAS SECTION.
           MOVE "N" TO OK
           INITIALIZE REG-COD041
                      QTDE-NAO
                      QTDE-REC
           MOVE NR-CONTRATO-CO40       TO NR-CONTRATO-CO41
           START COD041 KEY IS NOT LESS CHAVE-CO41 INVALID KEY
                MOVE "10" TO ST-COD041.

           PERFORM UNTIL ST-COD041 = "10"
                READ COD041 NEXT AT END
                     MOVE "10" TO ST-COD041
                NOT AT END
                     IF NR-CONTRATO-CO40 <> NR-CONTRATO-CO41
                        MOVE "10" TO ST-COD041
                     ELSE
                        IF CHECK-CONVITE-CO41 IS NOT NUMERIC
                           MOVE 0 TO CHECK-CONVITE-CO41
                        END-IF
                        EVALUATE CHECK-CONVITE-CO41
                           WHEN 0 ADD 1 TO QTDE-NAO
                           WHEN 1 ADD 1 TO QTDE-REC
                        END-EVALUATE
                     END-IF
                END-READ
           END-PERFORM

           IF GS-RECEBIDOS = 1
              IF QTDE-NAO = 0
                 MOVE "S" TO OK
              END-IF
           END-IF

           IF GS-NAO-RECEBIDOS = 1
              IF QTDE-NAO > 0
                 MOVE "S" TO OK
              END-IF
           END-IF.

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
                   READ WORK NEXT RECORD AT END
                        MOVE "10" TO ST-WORK
                   NOT AT END
                        IF GS-ATIVO-TRUE AND GS-INATIVO-TRUE
      *                 IF GS-ATIVO-TRUE AND COD-STATUS-WK NOT < 50 OR
      *                    GS-INATIVO-TRUE AND COD-STATUS-WK < 50
                           PERFORM MOVER-DADOS-LINDET
                        ELSE
                           CONTINUE
                        END-IF
                   END-READ
           END-PERFORM.
           PERFORM TOTALIZA
           MOVE "TOTAL GERAL: " TO GS-LINDET(1: 30)
           MOVE TOTAL-FORM-G   TO GS-LINDET(68: 8)
           MOVE TOTAL-FOTOS-G  TO GS-LINDET(77: 9)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE "REFRESH-DATA" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.

       PESQUISAR-STATUS SECTION.
           MOVE "N" TO ACHEI

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES OR ACHEI = "S"
               IF GS-LINHA-STATUS(1:2) = STATUS-CO40
                  MOVE "S" TO ACHEI
               END-IF
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO CONTRATO-WK
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CURSO" TO GS-DESCR-ORDEM
                MOVE SPACES TO CURSO-WK
                START WORK KEY IS NOT < CURSO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "INSTITUIÇÃO" TO GS-DESCR-ORDEM
                MOVE SPACES TO INSTITUICAO-WK
                START WORK KEY IS NOT < INSTITUICAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "CIDADE" TO GS-DESCR-ORDEM
                MOVE SPACES TO CIDADE-WK
                START WORK KEY IS NOT < CIDADE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "PREPOSTO" TO GS-DESCR-ORDEM
                MOVE SPACES TO PREPOSTO-WK
                START WORK KEY IS NOT < PREPOSTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "REPRESENTANTE" TO GS-DESCR-ORDEM
                MOVE SPACES TO REPRESENT-WK
                START WORK KEY IS NOT < REPRESENT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 7
                MOVE "PADRÃO" TO GS-DESCR-ORDEM
                MOVE SPACES TO PADRAO-WK
                START WORK KEY IS NOT < PADRAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 8
                MOVE "MES/ANO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO MESANO-WK
                START WORK KEY IS NOT < MESANO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 9
                MOVE "STATUS" TO GS-DESCR-ORDEM
                MOVE SPACES TO STATUS-WK
                START WORK KEY IS NOT < STATUS-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF CURSO-ANT  NOT = SPACES
                 IF CURSO-ANT NOT = CURSO-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF INSTITUICAO-ANT NOT = SPACES
                 IF INSTITUICAO-ANT NOT = INSTITUICAO-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA
             WHEN 5
              IF PREPOSTO-ANT NOT = SPACES
                 IF PREPOSTO-ANT NOT = PREPOSTO-WK
                    PERFORM TOTALIZA
             WHEN 6
              IF REPRESENT-ANT NOT = SPACES
                 IF REPRESENT-ANT NOT = REPRESENT-WK
                    PERFORM TOTALIZA
             WHEN 7
              IF PADRAO-ANT NOT = SPACES
                 IF PADRAO-ANT NOT = PADRAO-WK
                    PERFORM TOTALIZA
             WHEN 8
              IF MESANO-ANT NOT = ZEROS
                 IF MESANO-ANT NOT = MESANO-WK
                    PERFORM TOTALIZA
             WHEN 9
              IF STATUS-ANT NOT = SPACES
                 IF STATUS-ANT NOT = STATUS-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE CONTRATO-WK       TO GS-LINDET(1: 5)
           MOVE CURSO-WK          TO GS-LINDET(6: 16)
           MOVE INSTITUICAO-WK    TO GS-LINDET(22: 12)
           MOVE CIDADE-WK         TO GS-LINDET(34: 13)
           MOVE PREPOSTO-WK       TO GS-LINDET(47: 11)
           MOVE REPRESENT-WK      TO GS-LINDET(58: 14)
           MOVE QT-FORM-WK        TO GS-LINDET(72: 5)
           ADD QT-FORM-WK TO TOTAL-FORM.
           MOVE PADRAO-WK         TO GS-LINDET(77: 4)
           MOVE QT-FOTOS-WK       TO GS-LINDET(81: 6)
           ADD QT-FOTOS-WK TO TOTAL-FOTOS.
           MOVE MESANO-WK         TO MESANO-I
           MOVE MESANO-I(1: 4)    TO MESANO-W(3: 4)
           MOVE MESANO-I(5: 2)    TO MESANO-W(1: 2)
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO GS-LINDET(87: 8)
           MOVE STATUS-WK         TO GS-LINDET(95: 13).

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO CONTRATO-ANT MESANO-ANT TOTAL-FOTOS-G
                         TOTAL-FORM-G TOTAL-FORM TOTAL-FOTOS.
           MOVE SPACES TO PREPOSTO-ANT INSTITUICAO-ANT CURSO-ANT
                         CIDADE-ANT REPRESENT-ANT PADRAO-ANT STATUS-ANT.
       MOVER-CHAVE-ANT SECTION.
           MOVE CURSO-WK          TO CURSO-ANT.
           MOVE CIDADE-WK         TO CIDADE-ANT.
           MOVE PREPOSTO-WK       TO PREPOSTO-ANT.
           MOVE INSTITUICAO-WK    TO INSTITUICAO-ANT.
           MOVE CONTRATO-WK       TO CONTRATO-ANT
           MOVE MESANO-WK         TO MESANO-ANT
           MOVE STATUS-WK         TO STATUS-ANT.
           MOVE REPRESENT-WK      TO REPRESENT-ANT
           MOVE PADRAO-WK         TO PADRAO-ANT.
       TOTALIZA SECTION.
           MOVE SPACES        TO GS-LINDET
           MOVE TOTAL-FORM    TO GS-LINDET(70: 6)
           MOVE TOTAL-FOTOS   TO GS-LINDET(78: 8)
           ADD TOTAL-FORM  TO TOTAL-FORM-G
           ADD TOTAL-FOTOS TO TOTAL-FOTOS-G
           MOVE ZEROS TO TOTAL-FORM TOTAL-FOTOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP101" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-ATIVO-TRUE AND GS-INATIVO-TRUE
      *         IF GS-ATIVO-TRUE AND COD-STATUS-WK NOT < 50 OR
      *            GS-INATIVO-TRUE AND COD-STATUS-WK < 50
                      PERFORM MOVER-DADOS-RELATORIO
                ELSE CONTINUE
                END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL
           MOVE SPACES        TO LINDET-REL
           MOVE TOTAL-FORM-G  TO LINDET-REL(68: 8)
           MOVE TOTAL-FOTOS-G TO LINDET-REL(77: 9)
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.

           COPY DESCONDENSA.
       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF CURSO-ANT  NOT = SPACES
                 IF CURSO-ANT NOT = CURSO-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF INSTITUICAO-ANT NOT = SPACES
                 IF INSTITUICAO-ANT NOT = INSTITUICAO-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF PREPOSTO-ANT NOT = SPACES
                 IF PREPOSTO-ANT NOT = PREPOSTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 6
              IF REPRESENT-ANT NOT = SPACES
                 IF REPRESENT-ANT NOT = REPRESENT-WK
                    PERFORM TOTALIZA-REL
             WHEN 7
              IF PADRAO-ANT NOT = SPACES
                 IF PADRAO-ANT NOT = PADRAO-WK
                    PERFORM TOTALIZA-REL
             WHEN 8
              IF MESANO-ANT NOT = ZEROS
                 IF MESANO-ANT NOT = MESANO-WK
                    PERFORM TOTALIZA-REL
             WHEN 9
              IF STATUS-ANT NOT = SPACES
                 IF STATUS-ANT NOT = STATUS-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           MOVE CONTRATO-WK       TO LINDET-REL(1: 5)
           MOVE CURSO-WK          TO LINDET-REL(6: 16)
           MOVE INSTITUICAO-WK    TO LINDET-REL(22: 12)
           MOVE CIDADE-WK         TO LINDET-REL(34: 13)
           MOVE PREPOSTO-WK       TO LINDET-REL(47: 11)
           MOVE REPRESENT-WK      TO LINDET-REL(58: 14)
           MOVE QT-FORM-WK        TO LINDET-REL(72: 5)
           ADD QT-FORM-WK         TO TOTAL-FORM
           MOVE PADRAO-WK         TO LINDET-REL(77: 4)
           MOVE QT-FOTOS-WK       TO LINDET-REL(81: 6)
           ADD QT-FOTOS-WK        TO TOTAL-FOTOS
           MOVE MESANO-WK         TO MESANO-I
           MOVE MESANO-I(1: 4)    TO MESANO-W(3: 4)
           MOVE MESANO-I(5: 2)    TO MESANO-W(1: 2)
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO LINDET-REL(87: 8)
           MOVE STATUS-WK         TO LINDET-REL(95: 13).

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES        TO LINDET-REL
           MOVE TOTAL-FORM    TO LINDET-REL(70: 6)
           MOVE TOTAL-FOTOS   TO LINDET-REL(78: 8)
           MOVE ZEROS TO TOTAL-FORM TOTAL-FOTOS.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.

           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
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
           CLOSE COD001 COD040 IED010 IED011 CAD010 CAD012
                 CGD001 COD005 MTD019 COD049 COD101
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
