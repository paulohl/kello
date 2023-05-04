       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP140.
      *DATA: 19/03/2007
      *AUTOR: ALFREDO SAVIOLLI NETO
      *RELATÓRIO: Extrato de Conta de Resultado X Fornecedor
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CCPX100.
           COPY CXPX004.
           COPY CXPX020.
           COPY CXPX042.

           SELECT WORK ASSIGN       TO   VARIA-WK
                       ORGANIZATION IS    INDEXED
                       ACCESS MODE  IS    DYNAMIC
                       STATUS       IS    ST-WORK
                       RECORD KEY   IS   CHAVE-WK.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT EXCEL ASSIGN TO ARQUIVO-EXCEL
                        ORGANIZATION IS SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CCPW100.
       COPY CXPW004.
       COPY CXPW020.
       COPY CXPW042.

       FD  WORK.
       01  REG-WORK.
           05 CHAVE-WK.
              10 MES-WK                    PIC 9(02).
              10 CH-WK                     PIC X(20).
              10 FORNECEDOR-WK             PIC X(30).
           05 HISTORICO-WK                 PIC X(40).
           05 VALOR-WK                     PIC S9(09)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       FD EXCEL.
       01 REG-EXCEL.
          05 EXCEL-SEQ                 PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-DATA-MOVTO          PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-HISTORICO           PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-DOCUMENTO           PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-VALOR               PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-DB                  PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-CONTA-FORNECEDOR    PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-NOME-FORNECEDOR     PIC X(30).
          05 FILLER                    PIC X(02) VALUE X"0DA0".


       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP140.CPB".
           COPY "CCP140.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CXD004             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CXD042             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  VARIA-WK              PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MOVTO-INI             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  MESANO-SALDO-ANT      PIC 9(6)     VALUE ZEROS.
           05  SALDO-INICIAL         PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL           PIC S9(8)V99 VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65)    VALUE SPACES.
           05  SALDO-INTERVALO       PIC S9(8)V99 VALUE ZEROS.
           05  MASC-VALOR            PIC ZZZ.ZZZ.ZZ9,99-.
           05  WS-DTMOV-INI          PIC 9(08)    VALUE ZEROS.
           05  WS-DTMOV-FIM          PIC 9(08)    VALUE ZEROS.
           05  WS-DTVEN-INI          PIC 9(08)    VALUE ZEROS.
           05  WS-DTVEN-FIM          PIC 9(08)    VALUE ZEROS.
           05  AUX-VALOR             PIC S9(09)V99 VALUE ZEROS.

      *  MES/ANO LIMITE PARA CALCULA O SALDO ANTERIOR
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(24)   VALUE
           "EXTRATO DE APURACAO C/C ".
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV. MOVTO: ".
           05  MOVTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MOVTO-FIM-REL       PIC 99/99/9999.
           05  FILLER              PIC X(18) VALUE "  INTERV. VENCTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB021.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE
           "FORNECEDOR.: ".
           05  FORNEC-REL          PIC X(30).

       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(104)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(104)  VALUE
           "   DATA    FORNECEDOR                    HISTÓRICO
      -    "                     VALOR".
       01  CAB041.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(104)  VALUE
           "MES                FORNECEDOR                              V
      -    "ALOR".
       01  CAB05.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(17)   VALUE "CONTA RESULTADO:".
           05  CONTA-REDUZ-REL     PIC ZZZ.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  NOME-CONTA-REDUZ-REL PIC X(42)   VALUE SPACES.
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(104)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "SALDO INICIAL: ".
           05  SALDO-INI-REL       PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X(08)   VALUE SPACES.
           05  FILLER              PIC X(17)   VALUE "SALDO INTERVALO:".
           05  SALDO-INT-REL       PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X(09)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "SALDO FINAL: ".
           05  SALDO-FIM-REL       PIC ZZ.ZZZ.ZZZ,ZZ-.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CCP140-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CCP140-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CCP140-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CCP140-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CXD042"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD042.
           OPEN INPUT CCD100 CGD001 CXD004 CXD020 CXD042.

           ACCEPT VARIA-WK FROM TIME.
           OPEN OUTPUT WORK
           CLOSE       WORK

           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CCP140-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CCP140-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO CCP140-MENSAGEM-ERRO
              MOVE ST-CCD100 TO CCP140-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD004 <> "00"
              MOVE "ERRO ABERTURA CXD004: "  TO CCP140-MENSAGEM-ERRO
              MOVE ST-CXD004 TO CCP140-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CCP140-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CCP140-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD042 <> "00"
              MOVE "ERRO ABERTURA CXD042: "  TO CCP140-MENSAGEM-ERRO
              MOVE ST-CXD042 TO CCP140-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CCP140-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CCP140-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CCP140-LE-CONTA-REDUZ-TRUE
                    PERFORM LE-CONTA-REDUZ
               WHEN CCP140-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CCP140-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN CCP140-POPUP-CONTAREDUZ-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN CCP140-EXCEL-TRUE
                    PERFORM GERAR-EXCEL
               WHEN CCP140-LE-FORNE-TRUE
                    PERFORM LER-FORNECEDOR
               WHEN CCP140-POPUP-FORNE-TRUE
                    PERFORM POPUP-FORNE
               WHEN CCP140-POPUP-CONTAREDUZ-TRUE
                    PERFORM POPUP-CONTARED
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       POPUP-CONTARED SECTION.
           CALL   "CXP020T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CXP020T"
           MOVE PASSAR-STRING-1(52: 5) TO CCP140-CONTA-REDUZ
           PERFORM LE-CONTA-REDUZ.

       LER-FORNECEDOR SECTION.
           MOVE CCP140-FORNE   TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01      TO CCP140-NOME-FORNE.

       POPUP-FORNE SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(33: 6) TO CCP140-FORNE
           PERFORM LER-FORNECEDOR.


       GERAR-EXCEL SECTION.
           MOVE SPACES TO ARQUIVO-EXCEL
           STRING "\ARQUIVOS\CXP-" CCP140-MOVTO-INI "-" CCP140-MOVTO-FIM
           INTO ARQUIVO-EXCEL

           OPEN OUTPUT EXCEL

           MOVE "INTERV. MOVTO: "    TO EXCEL-SEQ
           MOVE MOVTO-INI-REL        TO EXCEL-DATA-MOVTO
           MOVE " a "                TO EXCEL-HISTORICO
           MOVE MOVTO-FIM-REL        TO EXCEL-DOCUMENTO
           MOVE "CONTA RESULTADO:"   TO EXCEL-VALOR
           MOVE CONTA-REDUZ-REL      TO EXCEL-DB
           MOVE NOME-CONTA-REDUZ-REL TO EXCEL-CONTA-FORNECEDOR
           MOVE SPACES               TO EXCEL-NOME-FORNECEDOR
           WRITE REG-EXCEL

           MOVE "SEQ."             TO EXCEL-SEQ
           MOVE "DATA-MOVTO"       TO EXCEL-DATA-MOVTO
           MOVE "HISTORICO"        TO EXCEL-HISTORICO
           MOVE "DOCUMENTO"        TO EXCEL-DOCUMENTO
           MOVE "VALOR-R$"         TO EXCEL-VALOR
           MOVE "D/C"              TO EXCEL-DB
           MOVE "CTA-FORN."        TO EXCEL-CONTA-FORNECEDOR
           MOVE "NOME FORN."       TO EXCEL-NOME-FORNECEDOR
           WRITE REG-EXCEL

           STRING CCP140-MOVTO-INI(5:4) CCP140-MOVTO-INI(3:2)
                  CCP140-MOVTO-INI(1:2) INTO WS-DTMOV-INI

           STRING CCP140-MOVTO-FIM(5:4) CCP140-MOVTO-FIM(3:2)
                  CCP140-MOVTO-FIM(1:2) INTO WS-DTMOV-FIM

           STRING CCP140-VENCTO-INI(5:4) CCP140-VENCTO-INI(3:2)
                  CCP140-VENCTO-INI(1:2) INTO WS-DTVEN-INI

           STRING CCP140-VENCTO-FIM(5:4) CCP140-VENCTO-FIM(3:2)
                  CCP140-VENCTO-FIM(1:2) INTO WS-DTVEN-FIM

           INITIALIZE REG-CCD100

           IF CCP140-FORNE > 0
              MOVE CCP140-FORNE    TO FORNEC-CC100
              START CCD100 KEY IS NOT LESS CHAVE-CC100 INVALID KEY
                   MOVE "10" TO ST-CCD100
              END-START
              PERFORM UNTIL ST-CCD100 = "10"
                   READ CCD100 NEXT AT END
                       MOVE "10" TO ST-CCD100
                   NOT AT END
                       IF CCP140-FORNE <> FORNEC-CC100
                          MOVE "10" TO ST-CCD100
                       ELSE
                          IF CCP140-CONTA-REDUZ = 0 OR
                             CCP140-CONTA-REDUZ = CODREDUZ-APUR-CC100
                             IF CCP140-MOVTO-INI = 0 OR
                               (DATA-MOVTO-CC100 NOT < WS-DTMOV-INI AND
                                DATA-MOVTO-CC100 NOT > WS-DTMOV-FIM)
                                IF CCP140-VENCTO-INI = 0 OR
                                  (DATA-VENCTO-CC100 NOT < WS-DTVEN-INI
                                   AND
                                   DATA-VENCTO-CC100 NOT > WS-DTVEN-FIM)
                                   IF SITUACAO-CC100 <> 2
                                      PERFORM MOVER-DADOS
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                   END-READ
              END-PERFORM
           ELSE
              IF WS-DTMOV-INI > 0
                 INITIALIZE REG-CCD100
                 MOVE WS-DTMOV-INI         TO DATA-MOVTO-CC100
                 START CCD100 KEY IS NOT LESS DATA-MOVTO-CC100 INVALID
                                                                   KEY
                    MOVE "10" TO ST-CCD100
                 END-START
                 PERFORM UNTIL ST-CCD100 = "10"
                    READ CCD100 NEXT AT END
                       MOVE "10" TO ST-CCD100
                    NOT AT END
                       IF DATA-MOVTO-CC100 > WS-DTMOV-FIM
                          MOVE "10" TO ST-CCD100
                       ELSE
                          IF CCP140-FORNE = 0 OR CCP140-FORNE =
                             FORNEC-CC100
                             IF CCP140-CONTA-REDUZ = 0 OR
                                CCP140-CONTA-REDUZ = CODREDUZ-APUR-CC100
                                IF CCP140-VENCTO-INI = 0 OR
                                  (DATA-VENCTO-CC100 NOT < WS-DTVEN-INI
                                   AND
                                   DATA-VENCTO-CC100 NOT > WS-DTVEN-FIM)
                                   IF SITUACAO-CC100 <> 2
                                      PERFORM MOVER-DADOS
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                    END-READ
                 END-PERFORM
              ELSE
                 IF CCP140-VENCTO-INI > 0
                    INITIALIZE REG-CCD100
                    MOVE ZEROS TO SITUACAO-CC100
                    MOVE WS-DTVEN-INI TO DATA-VENCTO-CC100
                    START CCD100 KEY IS NOT LESS ALT3-CC100 INVALID KEY
                       MOVE "10" TO ST-CCD100
                    END-START
                    PERFORM UNTIL ST-CCD100 = "10"
                       READ CCD100 NEXT AT END
                           MOVE "10" TO ST-CCD100
                       NOT AT END
                           IF SITUACAO-CC100 <> 0 OR
                              DATA-VENCTO-CC100 > WS-DTVEN-FIM
                              MOVE "10" TO ST-CCD100
                           ELSE
                              IF CCP140-FORNE = 0 OR CCP140-FORNE =
                                 FORNEC-CC100
                                 IF CCP140-MOVTO-INI = 0 OR
                                   (DATA-MOVTO-CC100 NOT < WS-DTMOV-INI
                                    AND
                                    DATA-MOVTO-CC100 NOT > WS-DTMOV-FIM)
                                    IF CCP140-CONTA-REDUZ = 0 OR
                                       CCP140-CONTA-REDUZ =
                                       CODREDUZ-APUR-CC100
                                       IF SITUACAO-CC100 <> 2
                                          PERFORM MOVER-DADOS
                                       END-IF
                                    END-IF
                                 END-IF
                              END-IF
                           END-IF
                       END-READ
                    END-PERFORM
                 ELSE
                    MOVE "Nenhuma Opção de Busca Informada" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                 END-IF
              END-IF
           END-IF.


           MOVE "SALDO INICIAL"    TO EXCEL-SEQ
           MOVE SALDO-INI-REL      TO EXCEL-DATA-MOVTO
           MOVE "SALDO INTERVALO"  TO EXCEL-HISTORICO
           MOVE SALDO-INT-REL      TO EXCEL-DOCUMENTO
           MOVE "SALDO FINAL"      TO EXCEL-VALOR
           MOVE SALDO-FIM-REL      TO EXCEL-DB
           MOVE SPACES             TO EXCEL-CONTA-FORNECEDOR
           MOVE SPACES             TO EXCEL-NOME-FORNECEDOR

           WRITE REG-EXCEL

           CLOSE EXCEL.

       MOVER-DADOS SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE SEQ-CC100         TO EXCEL-SEQ
           MOVE DATA-MOVTO-CC100  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO EXCEL-DATA-MOVTO
           MOVE DESCRICAO-CC100   TO EXCEL-HISTORICO
           MOVE NR-DOCTO-CC100    TO EXCEL-DOCUMENTO
           MOVE VALOR-CC100       TO VALOR-E
           MOVE VALOR-E           TO EXCEL-VALOR
           IF TIPO-LCTO-CC100 < 50
              MOVE "C"            TO EXCEL-DB
           ELSE
              MOVE "D"            TO EXCEL-DB
           END-IF
           MOVE FORNEC-CC100      TO CODIGO-CG01
                                     EXCEL-CONTA-FORNECEDOR
           READ CGD001 INVALID KEY
                MOVE SPACES       TO NOME-CG01
           END-READ
           MOVE NOME-CG01         TO EXCEL-NOME-FORNECEDOR
           WRITE REG-EXCEL.
       MOVER-DADOS-FIM.
           EXIT.

       CHAMAR-POPUP SECTION.
           CALL   "CXP020T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CXP020T".
           MOVE PASSAR-STRING-1(52: 5) TO CCP140-CONTA-REDUZ.
           PERFORM LE-CONTA-REDUZ.

       LE-CONTA-REDUZ SECTION.
           if ccp140-conta-reduz > 0
           MOVE COD-USUARIO-W      TO COD-USUARIO-CX004
           MOVE CCP140-CONTA-REDUZ TO PROGRAMA-CX004
           READ CXD004 INVALID KEY
                MOVE "Usuário Sem Permissão falar com o CPD para Acesso"
                TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENASGEM
           NOT INVALID KEY
                MOVE CCP140-CONTA-REDUZ TO CODIGO-REDUZ-CX20
                                           CONTA-REDUZ-REL
                READ CXD020 INVALID KEY
                     MOVE SPACES TO NOME-CG01
                END-READ
                MOVE DESCRICAO-CX20 TO CCP140-NOME-CONTA-REDUZ
                                       NOME-CONTA-REDUZ-REL.
       EXIBIR-MENASGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           MOVE    1 TO CCP140-FLAG-CRITICA
           MOVE SPACES TO MENSAGEM.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE CCP140-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CARREGA-LISTA SECTION.
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO CCP140-LINDET.

           STRING CCP140-MOVTO-INI(5:4) CCP140-MOVTO-INI(3:2)
                  CCP140-MOVTO-INI(1:2) INTO WS-DTMOV-INI

           STRING CCP140-MOVTO-FIM(5:4) CCP140-MOVTO-FIM(3:2)
                  CCP140-MOVTO-FIM(1:2) INTO WS-DTMOV-FIM

           STRING CCP140-VENCTO-INI(5:4) CCP140-VENCTO-INI(3:2)
                  CCP140-VENCTO-INI(1:2) INTO WS-DTVEN-INI

           STRING CCP140-VENCTO-FIM(5:4) CCP140-VENCTO-FIM(3:2)
                  CCP140-VENCTO-FIM(1:2) INTO WS-DTVEN-FIM

           INITIALIZE REG-CCD100

           IF CCP140-FORNE > 0
              MOVE CCP140-FORNE    TO FORNEC-CC100
              START CCD100 KEY IS NOT LESS CHAVE-CC100 INVALID KEY
                   MOVE "10" TO ST-CCD100
              END-START
              PERFORM UNTIL ST-CCD100 = "10"
                   READ CCD100 NEXT AT END
                       MOVE "10" TO ST-CCD100
                   NOT AT END
                       IF CCP140-FORNE <> FORNEC-CC100
                          MOVE "10" TO ST-CCD100
                       ELSE
                          IF CCP140-CONTA-REDUZ = 0 OR
                             CCP140-CONTA-REDUZ = CODREDUZ-APUR-CC100
                             IF CCP140-MOVTO-INI = 0 OR
                               (DATA-MOVTO-CC100 NOT < WS-DTMOV-INI AND
                                DATA-MOVTO-CC100 NOT > WS-DTMOV-FIM)
                                IF CCP140-VENCTO-INI = 0 OR
                                  (DATA-VENCTO-CC100 NOT < WS-DTVEN-INI
                                   AND
                                   DATA-VENCTO-CC100 NOT > WS-DTVEN-FIM)
                                   IF SITUACAO-CC100 <> 2
                                      PERFORM MOVER-DADOS2
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                   END-READ
              END-PERFORM
           ELSE
              IF WS-DTMOV-INI > 0
                 INITIALIZE REG-CCD100
                 MOVE WS-DTMOV-INI         TO DATA-MOVTO-CC100
                 START CCD100 KEY IS NOT LESS DATA-MOVTO-CC100 INVALID
                                                                   KEY
                    MOVE "10" TO ST-CCD100
                 END-START
                 PERFORM UNTIL ST-CCD100 = "10"
                    READ CCD100 NEXT AT END
                       MOVE "10" TO ST-CCD100
                    NOT AT END
                       IF DATA-MOVTO-CC100 > WS-DTMOV-FIM
                          MOVE "10" TO ST-CCD100
                       ELSE
                          IF CCP140-FORNE = 0 OR CCP140-FORNE =
                             FORNEC-CC100
                             IF CCP140-CONTA-REDUZ = 0 OR
                                CCP140-CONTA-REDUZ = CODREDUZ-APUR-CC100
                                IF CCP140-VENCTO-INI = 0 OR
                                  (DATA-VENCTO-CC100 NOT < WS-DTVEN-INI
                                   AND
                                   DATA-VENCTO-CC100 NOT > WS-DTVEN-FIM)
                                   IF SITUACAO-CC100 <> 2
                                      PERFORM MOVER-DADOS2
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                    END-READ
                 END-PERFORM
              ELSE
                 IF CCP140-VENCTO-INI > 0
                    INITIALIZE REG-CCD100
                    MOVE ZEROS TO SITUACAO-CC100
                    MOVE WS-DTVEN-INI TO DATA-VENCTO-CC100
                    START CCD100 KEY IS NOT LESS ALT3-CC100 INVALID KEY
                       MOVE "10" TO ST-CCD100
                    END-START
                    PERFORM UNTIL ST-CCD100 = "10"
                       READ CCD100 NEXT AT END
                           MOVE "10" TO ST-CCD100
                       NOT AT END
                           IF SITUACAO-CC100 <> 0 OR
                              DATA-VENCTO-CC100 > WS-DTVEN-FIM
                              MOVE "10" TO ST-CCD100
                           ELSE
                              IF CCP140-FORNE = 0 OR CCP140-FORNE =
                                 FORNEC-CC100
                                 IF CCP140-MOVTO-INI = 0 OR
                                   (DATA-MOVTO-CC100 NOT < WS-DTMOV-INI
                                    AND
                                    DATA-MOVTO-CC100 NOT > WS-DTMOV-FIM)
                                    IF CCP140-CONTA-REDUZ = 0 OR
                                       CCP140-CONTA-REDUZ =
                                       CODREDUZ-APUR-CC100
                                       IF SITUACAO-CC100 <> 2
                                          PERFORM MOVER-DADOS2
                                       END-IF
                                    END-IF
                                 END-IF
                              END-IF
                           END-IF
                       END-READ
                    END-PERFORM
                 ELSE
                    MOVE "Nenhuma Opção de Busca Informada" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                 END-IF
              END-IF
           END-IF.
           CLOSE WORK
           OPEN INPUT WORK

           INITIALIZE REG-WORK
                      SALDO-INTERVALO

           START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
               MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   MOVE SPACES TO CCP140-LINDET
                   EVALUATE CCP140-OPCAO
                       WHEN 1
                              STRING CH-WK(7:2) "/"
                                     CH-WK(5:2) "/"
                                     CH-WK(1:4) INTO CCP140-LINDET
                              MOVE FORNECEDOR-WK TO CCP140-LINDET(12:29)
                              MOVE HISTORICO-WK  TO CCP140-LINDET(42:30)
                              MOVE VALOR-WK      TO MASC-VALOR
                              MOVE MASC-VALOR    TO CCP140-LINDET(73:15)
      *"   DATA    FORNECEDOR                    HISTÓRICO
      *"                 VALOR"
                       WHEN 2
                              MOVE CH-WK         TO CCP140-LINDET
                              MOVE FORNECEDOR-WK TO CCP140-LINDET(20:30)
                              MOVE VALOR-WK      TO MASC-VALOR
                              MOVE MASC-VALOR    TO CCP140-LINDET(51:15)
      *                       "MÊS                           VALOR"
      *"MÊS                FORNECEDOR                              VALOR"
                   END-EVALUATE
                   ADD VALOR-WK TO SALDO-INTERVALO
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM

           IF CCP140-OPCAO = 1
              MOVE ALL "-" TO CCP140-LINDET(1:88)
           ELSE
              MOVE ALL "-" TO CCP140-LINDET(1:66)
           END-IF

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO CCP140-LINDET

           MOVE SALDO-INTERVALO TO MASC-VALOR

           IF CCP140-OPCAO = 1
              MOVE "TOTAL . . . " TO CCP140-LINDET
              MOVE MASC-VALOR     TO CCP140-LINDET(73:15)
           ELSE
              MOVE "TOTAL . . . " TO CCP140-LINDET
              MOVE MASC-VALOR     TO CCP140-LINDET(51:15)
           END-IF

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           IF CCP140-OPCAO = 1
              MOVE ALL "-" TO CCP140-LINDET(1:88)
           ELSE
              MOVE ALL "-" TO CCP140-LINDET(1:66)
           END-IF

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           CLOSE WORK.
       CARREGA-LISTA-FIM.
           EXIT.

       MOVER-DADOS2 SECTION.
           MOVE SPACES TO CHAVE-WK
           IF CCP140-MOVTO-INI > 0
              EVALUATE CCP140-OPCAO
                  WHEN 1 STRING DATA-MOVTO-CC100 SEQ-CC100
                           INTO CH-WK
                         MOVE DESCRICAO-CC100        TO HISTORICO-WK
                         MOVE DATA-MOVTO-CC100(5:2)  TO MES-WK
                  WHEN 2 EVALUATE DATA-MOVTO-CC100(5:2)
                              WHEN 01 STRING "JANEIRO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 02 STRING "FEVEREIRO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 03 STRING "MARÇO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 04 STRING "ABRIL/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 05 STRING "MAIO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 06 STRING "JUNHO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 07 STRING "JULHO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 08 STRING "AGOSTO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 09 STRING "SETEMBRO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 10 STRING "OUTUBRO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 11 STRING "NOVEMBRO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                              WHEN 12 STRING "DEZEMBRO/"
                                      DATA-MOVTO-CC100(1:4) INTO CH-WK
                         END-EVALUATE
                         MOVE SPACES                  TO HISTORICO-WK
                         MOVE DATA-MOVTO-CC100(5:2)   TO MES-WK
              END-EVALUATE
           ELSE
              EVALUATE CCP140-OPCAO
                  WHEN 1 STRING DATA-VENCTO-CC100 SEQ-CC100
                           INTO CH-WK
                         MOVE DESCRICAO-CC100        TO HISTORICO-WK
                         MOVE DATA-VENCTO-CC100(5:2)  TO MES-WK
                  WHEN 2 EVALUATE DATA-VENCTO-CC100(5:2)
                              WHEN 01 STRING "JANEIRO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 02 STRING "FEVEREIRO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 03 STRING "MARÇO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 04 STRING "ABRIL/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 05 STRING "MAIO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 06 STRING "JUNHO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 07 STRING "JULHO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 08 STRING "AGOSTO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 09 STRING "SETEMBRO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 10 STRING "OUTUBRO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 11 STRING "NOVEMBRO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                              WHEN 12 STRING "DEZEMBRO/"
                                      DATA-VENCTO-CC100(1:4) INTO CH-WK
                         END-EVALUATE
                         MOVE SPACES                  TO HISTORICO-WK
                         MOVE DATA-VENCTO-CC100(5:2)  TO MES-WK
              END-EVALUATE
           END-IF

           IF TIPO-LCTO-CC100 < 50                                      CREDITO
              MOVE VALOR-CC100 TO AUX-VALOR
           ELSE
              COMPUTE AUX-VALOR = VALOR-CC100 * (-1)                    DEBITO
           END-IF

           MOVE FORNEC-CC100   TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES    TO NOME-CG01
           END-READ
           MOVE NOME-CG01      TO FORNECEDOR-WK
           READ WORK INVALID KEY
               MOVE AUX-VALOR TO VALOR-WK
               WRITE REG-WORK INVALID KEY
                   MOVE "Erro de Gravação...WORK" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               END-WRITE
           NOT INVALID KEY
               ADD AUX-VALOR   TO VALOR-WK
               REWRITE REG-WORK INVALID KEY
                   MOVE "Erro de Regravação...WORK" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               END-REWRITE
           END-READ.
       MOVER-DADOS2-FIM.
           EXIT.

       CLEAR-FLAGS SECTION.
           INITIALIZE CCP140-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP140"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.


           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL

           OPEN INPUT WORK

           INITIALIZE REG-WORK
                      SALDO-INTERVALO

           START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
               MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   MOVE SPACES TO LINDET-REL
                   EVALUATE CCP140-OPCAO
                       WHEN 1
                              STRING CH-WK(7:2) "/"
                                     CH-WK(5:2) "/"
                                     CH-WK(1:4) INTO LINDET-REL
                              MOVE FORNECEDOR-WK TO LINDET-REL(12:29)
                              MOVE HISTORICO-WK  TO LINDET-REL(42:30)
                              MOVE VALOR-WK      TO MASC-VALOR
                              MOVE MASC-VALOR    TO LINDET-REL(73:15)
      *"   DATA    FORNECEDOR                    HISTÓRICO
      *"                 VALOR"
                       WHEN 2
                              MOVE CH-WK         TO LINDET-REL
                              MOVE FORNECEDOR-WK TO LINDET-REL(20:30)
                              MOVE VALOR-WK      TO MASC-VALOR
                              MOVE MASC-VALOR    TO LINDET-REL(51:15)
      *                       "MÊS                           VALOR"
      *"MÊS                FORNECEDOR                              VALOR"
                   END-EVALUATE
                   ADD VALOR-WK TO SALDO-INTERVALO

                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56
                      PERFORM CABECALHO
                   END-IF
               END-READ
           END-PERFORM

           IF CCP140-OPCAO = 1
              MOVE ALL "-" TO LINDET-REL(1:88)
           ELSE
              MOVE ALL "-" TO LINDET-REL(1:66)
           END-IF

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           MOVE SPACES TO CCP140-LINDET

           MOVE SALDO-INTERVALO TO MASC-VALOR

           IF CCP140-OPCAO = 1
              MOVE "TOTAL . . . " TO LINDET-REL
              MOVE MASC-VALOR     TO LINDET-REL(73:15)
           ELSE
              MOVE "TOTAL . . . " TO LINDET-REL
              MOVE MASC-VALOR     TO LINDET-REL(51:15)
           END-IF

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           IF CCP140-OPCAO = 1
              MOVE ALL "-" TO LINDET-REL(1:88)
           ELSE
              MOVE ALL "-" TO LINDET-REL(1:66)
           END-IF

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           CLOSE WORK.

           copy descondensa.

       IMPRIME-RELATORIO-FIM.
           EXIT.

       CABECALHO SECTION.
           MOVE CCP140-MOVTO-INI  TO MOVTO-INI-REL
           MOVE CCP140-MOVTO-FIM  TO MOVTO-FIM-REL
           MOVE CCP140-VENCTO-INI TO VENCTO-INI-REL
           MOVE CCP140-VENCTO-FIM TO VENCTO-FIM-REL
           MOVE CCP140-NOME-FORNE TO FORNEC-REL

           ADD  1     TO LIN
                         PAG-W

           MOVE PAG-W TO PG-REL.

           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE
           END-IF

           WRITE REG-RELAT FROM CAB02
           WRITE REG-RELAT FROM CAB021 AFTER 2
           WRITE REG-RELAT FROM CAB05  AFTER 2
           WRITE REG-RELAT FROM CAB03
           IF CCP140-OPCAO = 1
              WRITE REG-RELAT FROM CAB04
           ELSE
              WRITE REG-RELAT FROM CAB041
           END-IF
           WRITE REG-RELAT FROM CAB03
           MOVE 9 TO LIN.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CCP140-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CCD100 CGD001 CXD004 CXD020 CXD042
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

