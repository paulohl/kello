       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP205.
      *DATA: 04/05/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Estatisiticas das despesas de reportagem
      *        Listar todos as reportagens que estiverem
      *        dentro do intervalo de reportagem solicitado.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CAPX010.
           COPY CAPX012.
           COPY COPX001.
           COPY COPX040.
           COPY REPX002.
           COPY REPX100.
           COPY REPX101.
           COPY REPX103.
           COPY REPX205.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW010.
       COPY CAPW012.
       COPY COPW001.
       COPY COPW040.
       COPY REPW002.
       COPY REPW100.
       COPY REPW101.
       COPY REPW103.
       COPY REPW205.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "REP205.CPB".
           COPY "REP205.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-RED002             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-RED103             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-RED205             PIC XX       VALUE SPACES.
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
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
      *    GRAVA-W CONTROLA SE GRAVA P/ WORK - ORDEM CIDADE / REGIAO
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZ,ZZ.
           05  QTDE-E1               PIC Z.ZZZ,ZZ.
           05  QTDE-E2               PIC ZZ.ZZZ,ZZ.
           05  QTDE-E3               PIC ZZZ.ZZZ.
           05  TOT-PARTICIPANTE      PIC 9(6)     VALUE ZEROS.
           05  FORM-PREV-W           PIC 9(6)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.

           05  TOT-GER-FITA          PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-FILME         PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-EQUIPE        PIC 9(6)V99  VALUE ZEROS.
           05  TOT-GER-FORMANDO      PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-PARTICIPANTE  PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-VLR-REPORT    PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-VLR-DESPESA   PIC 9(8)V99  VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).

           05  ACHEI                 PIC X(01)    VALUE SPACES.

           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(35)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(42)   VALUE
           "ESTATISTICAS DAS DESPESAS    ".
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
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
           05  FILLER              PIC X(20)   VALUE "POR PERIODO".
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       02  LINHA-07.
           05 FILLER                         PIC  X(055) VALUE
              "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 FILLER                         PIC  X(025) VALUE
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
       02  LINHA-08.
           05 FILLER                         PIC  X(019) VALUE
              "³TOTAL REPORTAGEM: ".
           05 GER-REPORT-REL                 PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-REPORT-REL                PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(017) VALUE
              "%³TOTAL DESPESA: ".
           05 GER-DESPESA-REL                PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-DESPESA-REL               PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%³".
       02  LINHA-09.
           05 FILLER                         PIC  X(055) VALUE
              "³                                        ³             ".
           05 FILLER                         PIC  X(025) VALUE
              "                        ³".
       02  LINHA-10.
           05 FILLER                         PIC  X(006) VALUE "³COORD".
           05 FILLER                         PIC  X(013) VALUE
              "ENACAO.....: ".
           05 GER-COORD-REL                  PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-COORD-REL                 PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(017) VALUE
              "%³LOCACOES(KM).: ".
           05 GER-LOCAC-REL                  PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-LOCAC-REL                 PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%³".
       02  LINHA-11.
           05 FILLER                         PIC  X(019) VALUE
              "³FOTOGRAFO.......: ".
           05 GER-FOTOG-REL                  PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-FOTOG-REL                 PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(017) VALUE
              "%³COMBUSTIVEL..: ".
           05 GER-COMBUST-REL                PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-COMBUST-REL               PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%³".
       02  LINHA-12.
           05 FILLER                         PIC  X(019) VALUE
              "³CINEGRAFISTA....: ".
           05 GER-CINEG-REL                  PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-CINEG-REL                 PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(017) VALUE
              "%³HOSPEDAGEM...: ".
           05 GER-HOSPED-REL                 PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-HOSPED-REL                PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%³".
       02  LINHA-13.
           05 FILLER                         PIC  X(008) VALUE
              "³AUXILIA".
           05 FILLER                         PIC  X(001) VALUE "R".
           05 FILLER                         PIC  X(010) VALUE
              "........: ".
           05 GER-AUX-REL                    PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-AUX-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(017) VALUE
              "%³REFEICAO.....: ".
           05 GER-REFEIC-REL                 PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-REFEIC-REL                PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%³".
       02  LINHA-14.
           05 FILLER                         PIC  X(019) VALUE
              "³OUTROS..........: ".
           05 GER-OUT-REPORT-REL             PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-OUT-REPORT-REL            PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(017) VALUE
              "%³PASSAGEM.....: ".
           05 GER-PASSAG-REL                 PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-PASSAG-REL                PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%³".
       02  LINHA-15.
           05 FILLER                         PIC  X(055) VALUE
              "³                                        ³ALUGUEL......".
           05 FILLER                         PIC  X(002) VALUE ": ".
           05 GER-ALUG-REL                   PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-ALUG-REL                  PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%³".
       02  LINHA-16.
           05 FILLER                         PIC  X(019) VALUE
              "³CUSTO TOT/FORM..: ".
           05 GER-CUSTO-FORM-REL             PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(025) VALUE
              "         ³MATERIAL.....: ".
           05 GER-MAT-REL                    PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-MAT-REL                   PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%³".
       02  LINHA-17.
           05 FILLER                         PIC  X(013) VALUE
              "³CUSTO PESSOA".
           05 FILLER                         PIC  X(002) VALUE "/D".
           05 FILLER                         PIC  X(004) VALUE "IA: ".
           05 GER-CUSTO-PESS-DIA-REL         PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(025) VALUE
              "         ³OUTROS.......: ".
           05 GER-OUT-DESP-REL               PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 PERC-OUT-DESP-REL              PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%³".
       02  LINHA-18.
           05 FILLER                         PIC  X(055) VALUE
              "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 FILLER                         PIC  X(025) VALUE
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
       02  LINHA-19.
           05 FILLER                         PIC  X(002) VALUE "³N".
           05 FILLER                         PIC  X(001) VALUE "R".
           05 FILLER                         PIC  X(017) VALUE
              ".PESSOAS VIAGEM: ".
           05 GER-PESSOAS-REL                PIC  Z.ZZZ.ZZZ.
           05 FILLER                         PIC  X(032) VALUE
              "            ³QT.REAL FORMANDO.: ".
           05 GER-REAL-FORM-REL              PIC  Z.ZZZ.ZZZ.
           05 FILLER                         PIC  X(010) VALUE
              "         ³".
       02  LINHA-20.
           05 FILLER                         PIC  X(002) VALUE "³N".
           05 FILLER                         PIC  X(001) VALUE "R".
           05 FILLER                         PIC  X(017) VALUE
              ".VEICULOS......: ".
           05 GER-VEIC-REL                   PIC  Z.ZZZ.ZZZ.
           05 FILLER                         PIC  X(032) VALUE
              "            ³QT.PREV FORMANDO.: ".
           05 GER-PREV-FORM-REL              PIC  Z.ZZZ.ZZZ.
           05 FILLER                         PIC  X(010) VALUE
              "         ³".
       02  LINHA-21.
           05 FILLER                         PIC  X(002) VALUE "³N".
           05 FILLER                         PIC  X(001) VALUE "R".
           05 FILLER                         PIC  X(017) VALUE
              ".DIAS VIAGEM...: ".
           05 GER-DIAS-REL                   PIC  Z.ZZZ.ZZZ.
           05 FILLER                         PIC  X(032) VALUE
              "            ³FILMES PRODUZIDOS: ".
           05 GER-FILMES-REL                 PIC  Z.ZZZ.ZZZ.
           05 FILLER                         PIC  X(010) VALUE
              "         ³".
       02  LINHA-22.
           05 FILLER                         PIC  X(055) VALUE
              "³                                        ³FITAS PRODUZI".
           05 FILLER                         PIC  X(006) VALUE
              "DAS.: ".
           05 GER-FITAS-REL                  PIC  Z.ZZZ.ZZZ.
           05 FILLER                         PIC  X(010) VALUE
              "         ³".
       02  LINHA-23.
           05 FILLER                         PIC  X(055) VALUE
              "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 FILLER                         PIC  X(025) VALUE
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

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
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "RED002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED002.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           MOVE "RED103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED103.
           MOVE "RED205"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED205.

           OPEN INPUT CAD010 CAD012 COD040 RED100 RED101 RED103 COD001
                      RED002

           OPEN I-O   RED205
           CLOSE      RED205
           OPEN INPUT RED205
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED002 <> "00"
              MOVE "ERRO ABERTURA RED002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED101 <> "00"
              MOVE "ERRO ABERTURA RED101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED103 <> "00"
              MOVE "ERRO ABERTURA RED103: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED103 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED205 <> "00"
              MOVE "ERRO ABERTURA RED205: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED205 TO GS-MENSAGEM-ERRO(23: 02)
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
                   PERFORM VERIFICAR-SENHA-STATUS
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-QUADRO
                    end-if
               WHEN GS-CARREGAR-DADOS-TRUE
                    PERFORM INICIO-RED100
                    PERFORM CARREGAR-DADOS-TELA
               WHEN GS-POPUP-CIDADE-TRUE
                    PERFORM CHAMAR-POPUP-CIDADE
               WHEN GS-LE-CIDADE-TRUE
                   PERFORM LE-CIDADE
               WHEN GS-POPUP-REGIAO-TRUE
                    PERFORM CHAMAR-POPUP-REGIAO
               WHEN GS-LE-REGIAO-TRUE
                   PERFORM LE-REGIAO
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
           CLOSE    RED205
           OPEN I-O RED205

           INITIALIZE REG-RED205
           START RED205 KEY IS NOT LESS CODIGO-RED205 INVALID KEY
                MOVE "10" TO ST-RED205.
           PERFORM UNTIL ST-RED205 = "10"
                READ RED205 NEXT AT END
                     MOVE "10" TO ST-RED205
                NOT AT END
                     DELETE RED205 INVALID KEY
                         MOVE "Erro de Exclusão...RED205" TO MENSAGEM
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
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-RED205
               WRITE REG-RED205
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      RED205
           OPEN INPUT RED205.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-RED205
           START RED205 KEY IS NOT LESS CODIGO-RED205 INVALID KEY
               MOVE "10" TO ST-RED205.

           PERFORM UNTIL ST-RED205 = "10"
               READ RED205 NEXT AT END
                    MOVE "10" TO ST-RED205
               NOT AT END
                    MOVE CODIGO-RED205 TO CODIGO-CO01
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
              CLOSE      RED205
              OPEN I-O   RED205
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO CODIGO-RED205
                        WRITE REG-RED205

                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM
              CLOSE      RED205
              OPEN INPUT RED205.

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

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CHAMAR-POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-CID.
       LE-CIDADE SECTION.
           MOVE GS-CIDADE  TO CIDADE.
           READ CAD010 INVALID KEY MOVE "****" TO NOME-CID.
           MOVE NOME-CID           TO GS-NOME-CID.
       CHAMAR-POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-REGIAO.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-REG.
       LE-REGIAO SECTION.
           MOVE GS-REGIAO  TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE "****" TO NOME-REG.
           MOVE NOME-REG           TO GS-NOME-REG.


       INICIO-RED100 SECTION.
           INITIALIZE TOT-PARTICIPANTE
                      FORM-PREV-W
                      CONTRATO-ANT
                      TOT-GER-FITA
                      TOT-GER-FILME
                      TOT-GER-EQUIPE
                      TOT-GER-FORMANDO
                      TOT-GER-PARTICIPANTE
                      TOT-GER-VLR-REPORT
                      TOT-GER-VLR-DESPESA

                      GS-TOT-REPORT
                      GS-PER-TOT-REPORT
                      GS-TOT-DESPESA
                      GS-PER-TOT-DESPESA
                      GS-VLR-COORD
                      GS-PER-COORD
                      GS-VLR-CINEG
                      GS-PER-CINEG
                      GS-VLR-FOTOG
                      GS-PER-FOTOG
                      GS-VLR-AUXIL
                      GS-PER-AUXIL
                      GS-VLR-OUTROS
                      GS-PER-OUTROS
                      GS-VLR-LOCACAO
                      GS-PER-LOCACAO
                      GS-VLR-COMBUSTIVEL
                      GS-PER-COMBUSTIVEL
                      GS-VLR-HOSPEDAGEM
                      GS-PER-HOSPEDAGEM
                      GS-VLR-REFEICAO
                      GS-PER-REFEICAO
                      GS-VLR-PASSAGEM
                      GS-PER-PASSAGEM
                      GS-VLR-ALUGUEL
                      GS-PER-ALUGUEL
                      GS-VLR-MATERIAL
                      GS-PER-MATERIAL
                      GS-VLR-OUTROS-DESP
                      GS-PER-OUTROS-DESP
                      GS-CUSTO-POR-FORM
                      GS-CUSTO-POR-PESSOA
                      GS-NR-PESS-VIAGEM
                      GS-NR-VEICULOS
                      GS-NR-DIAS-VIAGEM
                      GS-QTDE-REAL-FORM
                      GS-QTDE-PREV-FORM
                      GS-QTDE-FILMES
                      GS-QTDE-FITAS

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-ANT
                                     VECTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-ANT
                                     VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-FIM.

           MOVE VECTO-INI         TO DATA-MOV-R100.
           START RED100 KEY IS NOT < DATA-MOV-R100 INVALID KEY
              MOVE "10" TO ST-RED100.
           PERFORM UNTIL ST-RED100 = "10"
              READ RED100 NEXT RECORD AT END
                   MOVE "10" TO ST-RED100
              NOT AT END
                   IF DATA-MOV-R100 > VECTO-FIM
                      MOVE "10" TO ST-RED100
                   ELSE
                      PERFORM VERIFICA-OPCAO-RELATORIO
                      IF GRAVA-W = 0
                         CONTINUE
                      ELSE
                         MOVE DOCTO-R100           TO GS-EXIBE-MOVTO
                         MOVE "TELA-AGUARDA1"      TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                         PERFORM GRAVA-REPORTAGEM
                         PERFORM VERIFICA-QTDE-FORM-PREVISTA
                         ADD FORM-PREV-W           TO GS-QTDE-PREV-FORM
                         ADD QTDE-PESSOAS-R100     TO GS-NR-PESS-VIAGEM
                         ADD QTDE-DIAS-R100        TO GS-NR-DIAS-VIAGEM
                         ADD QTDE-FORM-R100        TO GS-QTDE-REAL-FORM
                         ADD QTDE-VEICULOS-R100    TO GS-NR-VEICULOS
                         ADD TOT-FILME-REPORT-R100 TO GS-QTDE-FILMES
                         ADD TOT-FITA-REPORT-R100  TO GS-QTDE-FITAS
                         ADD VLR-COMB-R100         TO GS-VLR-COMBUSTIVEL
                         ADD VLR-HOSP-R100         TO GS-VLR-HOSPEDAGEM
                         ADD VLR-REFEICAO-R100     TO GS-VLR-REFEICAO
                         ADD VLR-PASSAGEM-R100     TO GS-VLR-PASSAGEM
                         ADD VLR-ALUGUEL-R100      TO GS-VLR-ALUGUEL
      *                  LOCACAO VEM DA FUNCAO 12 DA REPORTAGEM
                         ADD VLR-MAT-R100          TO GS-VLR-MATERIAL
                         ADD VLR-OUTROS-R100       TO GS-VLR-OUTROS-DESP
                         COMPUTE GS-TOT-DESPESA ROUNDED =
                                 GS-TOT-DESPESA    +
                                 VLR-COMB-R100     +
                                 VLR-HOSP-R100     +
                                 VLR-REFEICAO-R100 +
                                 VLR-PASSAGEM-R100 +
                                 VLR-MAT-R100      +
                                 VLR-OUTROS-R100   +
                                 VLR-ALUGUEL-R100
      *                          + VLR-DESPESA-REPORT-R100

                      END-IF
                   END-IF
              END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       GRAVA-REPORTAGEM SECTION.
           MOVE DOCTO-R100             TO DOCTO-R103.
           MOVE ZEROS                  TO CODIGO-R103 FUNCAO-R103.
           START RED103 KEY IS NOT < CHAVE-R103 INVALID KEY
                 MOVE "10" TO ST-RED103.                                        x
           PERFORM UNTIL ST-RED103 = "10"
                 READ RED103 NEXT RECORD AT END
                      MOVE "10" TO ST-RED103
                 NOT AT END
                      IF DOCTO-R103 <> DOCTO-R100
                         MOVE "10" TO ST-RED103
                      ELSE
                         MOVE FUNCAO-R103 TO CODIGO-RE02
                         READ RED002 INVALID KEY
                              INITIALIZE REG-RED002
                         END-READ
                         EVALUATE ACUMULAR-RE02
                              WHEN 1 ADD VLR-REPORT-R103 TO
                                         GS-VLR-COORD GS-TOT-REPORT
                              WHEN 2 ADD VLR-REPORT-R103 TO
                                         GS-VLR-FOTOG GS-TOT-REPORT
                              WHEN 3 ADD VLR-REPORT-R103 TO
                                         GS-VLR-CINEG GS-TOT-REPORT
                              WHEN 4 ADD VLR-REPORT-R103 TO
                                         GS-VLR-AUXIL GS-TOT-REPORT
                              WHEN 5 ADD VLR-REPORT-R103 TO
                                         GS-VLR-MATERIAL GS-TOT-DESPESA
                              WHEN 6 ADD VLR-REPORT-R103 TO
                                         GS-VLR-LOCACAO GS-TOT-DESPESA
                              WHEN 7 ADD VLR-REPORT-R103 TO
                                         GS-VLR-ALUGUEL GS-TOT-DESPESA
                              WHEN 8 ADD VLR-REPORT-R103 TO
                                         GS-VLR-OUTROS  GS-TOT-REPORT
                         END-EVALUATE
                      END-IF
                 END-READ
           END-PERFORM.

       VERIFICA-QTDE-FORM-PREVISTA SECTION.
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE ZEROS               TO CONTRATO-R101 EVENTO-R101
           MOVE ZEROS               TO FORM-PREV-W CONTRATO-ANT
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END
                  MOVE "10" TO ST-RED101
             NOT AT END
                IF DOCTO-R101 <> DOCTO-R100
                   MOVE "10" TO ST-RED101
                ELSE
                   MOVE CONTRATO-R101       TO NR-CONTRATO-CO40
                   IF CONTRATO-R101 <> CONTRATO-ANT
                      MOVE CONTRATO-R101 TO CONTRATO-ANT
                      READ COD040 INVALID KEY
                           INITIALIZE REG-COD040
                      END-READ
                      ADD QTDE-FORM-CO40       TO FORM-PREV-W
                   END-IF
                END-IF
              END-READ
            END-PERFORM.

       VERIFICA-OPCAO-RELATORIO SECTION.
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE ZEROS               TO CONTRATO-R101
                                       EVENTO-R101.

           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
                 READ RED101 NEXT RECORD AT END
                      MOVE "10" TO ST-RED101
                 NOT AT END
                      IF DOCTO-R101 <> DOCTO-R100
                         MOVE "10" TO ST-RED101
                      ELSE
                         MOVE CONTRATO-R101       TO NR-CONTRATO-CO40
                         READ COD040 INVALID KEY
                              INITIALIZE REG-COD040
                         END-READ
                         MOVE CIDADE-CO40         TO CIDADE
                         READ CAD010 INVALID KEY
                              INITIALIZE REG-CAD010
                         END-READ
                      END-IF
                 END-READ
           END-PERFORM.

           MOVE 0 TO GRAVA-W.
           EVALUATE GS-TIPO-REL
             WHEN 1 MOVE 1 TO GRAVA-W
             WHEN 2 IF CIDADE = GS-CIDADE MOVE 1 TO GRAVA-W
             WHEN 3 IF REGIAO-CID = GS-REGIAO MOVE 1 TO GRAVA-W
           END-EVALUATE

           IF GRAVA-W = 1
              PERFORM PESQUISAR-STATUS
              IF ACHEI = "S"
      *       MOVE STATUS-CO40 TO CODIGO-RED205
      *       READ RED205 INVALID KEY
                   MOVE 1      TO GRAVA-W.

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

       CARREGAR-DADOS-TELA SECTION.

           COMPUTE GS-CUSTO-POR-FORM ROUNDED =
                 (GS-TOT-REPORT + GS-TOT-DESPESA) / GS-QTDE-REAL-FORM.
           COMPUTE GS-CUSTO-POR-PESSOA =
                 (GS-TOT-REPORT + GS-TOT-DESPESA) / GS-NR-PESS-VIAGEM.

           MOVE 100 TO GS-PER-TOT-REPORT.
           COMPUTE GS-PER-COORD ROUNDED = (GS-VLR-COORD * 100)
                               / GS-TOT-REPORT.
           COMPUTE GS-PER-CINEG ROUNDED = (GS-VLR-CINEG * 100)
                               / GS-TOT-REPORT.
           COMPUTE GS-PER-FOTOG ROUNDED = (GS-VLR-FOTOG * 100)
                               / GS-TOT-REPORT.
           COMPUTE GS-PER-AUXIL ROUNDED = (GS-VLR-AUXIL * 100)
                               / GS-TOT-REPORT.
           COMPUTE GS-PER-OUTROS ROUNDED = (GS-VLR-OUTROS * 100) /
                                   GS-TOT-REPORT.
           MOVE 100  TO GS-PER-TOT-DESPESA
           COMPUTE GS-PER-LOCACAO ROUNDED = (GS-VLR-LOCACAO * 100) /
                                       GS-TOT-DESPESA
           COMPUTE GS-PER-COMBUSTIVEL ROUNDED =
                       (GS-VLR-COMBUSTIVEL * 100) / GS-TOT-DESPESA
           COMPUTE GS-PER-HOSPEDAGEM ROUNDED = (GS-VLR-HOSPEDAGEM * 100)
                                       / GS-TOT-DESPESA
           COMPUTE GS-PER-REFEICAO ROUNDED = (GS-VLR-REFEICAO * 100)
                                       / GS-TOT-DESPESA
           COMPUTE GS-PER-PASSAGEM ROUNDED = (GS-VLR-PASSAGEM * 100)
                                       / GS-TOT-DESPESA
           COMPUTE GS-PER-ALUGUEL ROUNDED = (GS-VLR-ALUGUEL * 100)
                                       / GS-TOT-DESPESA
           COMPUTE GS-PER-MATERIAL ROUNDED = (GS-VLR-MATERIAL * 100)
                                       / GS-TOT-DESPESA
           COMPUTE GS-PER-OUTROS-DESP ROUNDED =
                          (GS-VLR-OUTROS-DESP * 100) / GS-TOT-DESPESA.

       IMPRIME-QUADRO SECTION.
           copy condensa.

           PERFORM CABECALHO.
           MOVE GS-TOT-REPORT       TO GER-REPORT-REL
           MOVE 100                 TO PERC-REPORT-REL.
           MOVE GS-VLR-COORD        TO GER-COORD-REL
           MOVE GS-PER-COORD        TO PERC-COORD-REL.
           MOVE GS-VLR-FOTOG        TO GER-FOTOG-REL
           MOVE GS-PER-FOTOG        TO PERC-FOTOG-REL.
           MOVE GS-VLR-CINEG        TO GER-CINEG-REL
           MOVE GS-PER-CINEG        TO PERC-CINEG-REL.
           MOVE GS-VLR-AUXIL        TO GER-AUX-REL
           MOVE GS-PER-AUXIL        TO PERC-AUX-REL.
           MOVE GS-VLR-OUTROS       TO GER-OUT-REPORT-REL
           MOVE GS-PER-OUTROS       TO PERC-OUT-REPORT-REL.
           MOVE GS-TOT-DESPESA      TO GER-DESPESA-REL.
           MOVE 100                 TO PERC-DESPESA-REL.
           MOVE GS-VLR-LOCACAO      TO GER-LOCAC-REL
           MOVE GS-PER-LOCACAO      TO PERC-LOCAC-REL.
           MOVE GS-VLR-COMBUSTIVEL  TO GER-COMBUST-REL
           MOVE GS-PER-COMBUSTIVEL  TO PERC-COMBUST-REL.
           MOVE GS-VLR-HOSPEDAGEM   TO GER-HOSPED-REL
           MOVE GS-PER-HOSPEDAGEM   TO PERC-HOSPED-REL.
           MOVE GS-VLR-REFEICAO     TO GER-REFEIC-REL
           MOVE GS-PER-REFEICAO     TO PERC-REFEIC-REL.
           MOVE GS-VLR-PASSAGEM     TO GER-PASSAG-REL
           MOVE GS-PER-PASSAGEM     TO PERC-PASSAG-REL.
           MOVE GS-VLR-ALUGUEL      TO GER-ALUG-REL
           MOVE GS-PER-ALUGUEL      TO PERC-ALUG-REL.
           MOVE GS-VLR-MATERIAL     TO GER-MAT-REL
           MOVE GS-PER-MATERIAL     TO PERC-MAT-REL.
           MOVE GS-VLR-OUTROS-DESP  TO GER-OUT-DESP-REL
           MOVE GS-PER-OUTROS-DESP  TO PERC-OUT-DESP-REL.
           MOVE GS-NR-PESS-VIAGEM   TO GER-PESSOAS-REL
           MOVE GS-NR-VEICULOS      TO GER-VEIC-REL
           MOVE GS-NR-DIAS-VIAGEM   TO GER-DIAS-REL
           MOVE GS-QTDE-REAL-FORM   TO GER-REAL-FORM-REL
           MOVE GS-QTDE-PREV-FORM   TO GER-PREV-FORM-REL
           MOVE GS-QTDE-FILMES      TO GER-FILMES-REL
           MOVE GS-QTDE-FITAS       TO GER-FITAS-REL
           MOVE GS-CUSTO-POR-FORM   TO GER-CUSTO-FORM-REL
           MOVE GS-CUSTO-POR-PESSOA TO GER-CUSTO-PESS-DIA-REL.
           WRITE REG-RELAT FROM LINHA-07.
           WRITE REG-RELAT FROM LINHA-08.
           WRITE REG-RELAT FROM LINHA-09.
           WRITE REG-RELAT FROM LINHA-10.
           WRITE REG-RELAT FROM LINHA-11.
           WRITE REG-RELAT FROM LINHA-12.
           WRITE REG-RELAT FROM LINHA-13.
           WRITE REG-RELAT FROM LINHA-14.
           WRITE REG-RELAT FROM LINHA-15.
           WRITE REG-RELAT FROM LINHA-16.
           WRITE REG-RELAT FROM LINHA-17.
           WRITE REG-RELAT FROM LINHA-18.
           WRITE REG-RELAT FROM LINHA-19.
           WRITE REG-RELAT FROM LINHA-20.
           WRITE REG-RELAT FROM LINHA-21.
           WRITE REG-RELAT FROM LINHA-22.
           WRITE REG-RELAT FROM LINHA-23.

           copy descondensa.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           WRITE REG-RELAT FROM CAB01 AFTER 0
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB03.
           EVALUATE GS-TIPO-REL
             WHEN 1 WRITE REG-RELAT FROM CAB02C AFTER 2
             WHEN 2 MOVE GS-CIDADE      TO CIDADE-REL
                    MOVE GS-NOME-CID    TO NOME-CID-REL
                    WRITE REG-RELAT FROM CAB02A AFTER 2
             WHEN 3 MOVE GS-REGIAO      TO REGIAO-REL
                    MOVE GS-NOME-REG    TO NOME-REG-REL
                    WRITE REG-RELAT FROM CAB02B AFTER 2
           END-EVALUATE.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP205" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 CAD012 COD001 RED002
                 COD040 RED100 RED101 RED103 RED205.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
