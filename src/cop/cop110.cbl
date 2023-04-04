       COPY DSLANG.CPY.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP110.
       DATE-WRITTEN. 10/09/2001.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: Relatório 501 - COM DESPESAS FIXAS
      * LISTAR APENAS OS CONTRATOS ATIVOS, OU SEJA, STATUS => 50
      * FALTA ACRESCENTAR OS CRÉDITOS
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY COPX002.
           COPY COPX003.
           COPY COPX005.
           COPY COPX040.
           COPY COPX050.
           COPY COPX080.
           COPY COPX081.
           COPY IEPX010.
           COPY IEPX011.
           COPY CAPX010.
           COPY REPX100.
           COPY REPX101.
           SELECT WORK1 ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK1
                  RECORD KEY IS DOCTO-WK1.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY IEPW010.
       COPY IEPW011.
       COPY CAPW010.
       COPY COPW002.
       COPY COPW003.
       COPY COPW005.
       COPY COPW040.
       COPY COPW050.
       COPY COPW080.
       COPY COPW081.
       COPY REPW100.
       COPY REPW101.
       FD  WORK1.
       01  REG-WORK1.
           05  DOCTO-WK1         PIC 9(6).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP110.CPB".
           COPY "COP110.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-COD080             PIC XX       VALUE SPACES.
           05  ST-COD081             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(8)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E2              PIC ZZZ,ZZ    BLANK WHEN ZEROS.
           05  PERC-E                PIC ZZZ,ZZ    BLANK WHEN ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  PM-E                  PIC ZZZZ,ZZ.
           05  MESES-E               PIC ZZZZ,ZZ.
           05  TAXA-E                PIC ZZZ,ZZZZ.
           05  QTDE-E                PIC ZZZ.ZZZ,ZZ.
           05  QTDE-E1               PIC ZZZ.ZZZ.
           05  I                     PIC 99       VALUE ZEROS.

           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.

      *    VARIAVEIS P/ CALCULO DE PATROCINIO PAGOS E A PAGAR
           05  CUSTO-PREVISTO        PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-W               PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-W               PIC 9(8)V99  VALUE ZEROS.
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  TAXA-W                PIC 9(3)V99999 VALUE ZEROS.
           05  TAXA-JUROS-PM         PIC 9(4)V9999 VALUE ZEROS.
           05  TAXA-JUROS            PIC 9(4)V9999 VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  MESES-W               PIC 9(4)V99  VALUE ZEROS.
           05  MESES-P-FORM          PIC 9(3)     VALUE ZEROS.
           05  VLR-PAGO              PIC 9(8)V99  VALUE ZEROS.
           05  VLR-PAGAR             PIC 9(8)V99  VALUE ZEROS.
           05  VLR-GERAL             PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-E-JUROS         PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-GERAL           PIC 9(8)V99  VALUE ZEROS.
           05  VLR-POR-FORM          PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGO            PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGAR           PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-TOT-GERAL       PIC 9(8)V99  VALUE ZEROS.
      *    VARIAVEIS P/ CALCULAR 70% DE QTDE E VALORES DE FOTO/FITA/ALB
           05  QTDE-PERC70           PIC 9(6)V99  VALUE ZEROS.
           05  VALOR-W               PIC 9(8)V99  VALUE ZEROS.
           05  VALOR-W1              PIC 9(8)V99  VALUE ZEROS.
           05  FATURAMENTO-W         PIC 9(8)V99  VALUE ZEROS.

      *    VARIAVEIS P/ REPORTAGEM
           05  QT-FILME-REP          PIC 9(3)V99  VALUE ZEROS.
           05  QT-FITA-REP           PIC 9(3)V99  VALUE ZEROS.
           05  QT-EQUIPE-REP         PIC 9(3)V99  VALUE ZEROS.
           05  TOT-PARTICIPANTE      PIC 9(4)     VALUE ZEROS.
           05  DESPESA-W             PIC 9(8)V99  VALUE ZEROS.
           05  VLR-DESPESA-REP       PIC 9(8)V99  VALUE ZEROS.
           05  VLR-REPORT-REP        PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-REPORT          PIC 9(8)V99  VALUE ZEROS.
           05  QTDE-E-REP            PIC ZZZZ,ZZ.
           05  VLR-REPORTAGEM-RES    PIC 9(8)V99  VALUE ZEROS.
           05  DIAS-REPORTAGEM-RES   PIC 9(8)     VALUE ZEROS.
           05  JUROS-REPORTAGEM-RES  PIC 9(8)V99  VALUE ZEROS.
           05  TOT-FITA-RES          PIC 9(4)V99  VALUE ZEROS.
           05  TOT-FILME-RES         PIC 9(4)V99  VALUE ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ.
           05  PM-W                  PIC 9(3)V99  VALUE ZEROS.
           05  CUSTO-TOTAL           PIC 9(8)V99  VALUE ZEROS.
      *   VARIAVEIS P/ CALCULO DE DESPESAS
           05  VLR-DESPESA-RES       PIC 9(8)V99  VALUE ZEROS.
           05  VLR-DESPESA-FIXA-RES  PIC 9(8)V99  VALUE ZEROS.
           05  VLR-DESPESA-VENDA-RES PIC 9(8)V99  VALUE ZEROS.
           05  DIAS-DESPESA-RES      PIC 9(8)     VALUE ZEROS.
           05  JUROS-DESPESA-RES     PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-DESPESA         PIC 9(8)V99  VALUE ZEROS.

           05  RESULTADO-FINAL       PIC S9(8)V99 VALUE ZEROS.
           05  VALOR-ES              PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  TOTAL-CUSTO-DESPESA   PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(36)   VALUE
           "RELATORIO 501          ".
           05  FILLER              PIC X(20)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(110)   VALUE
           "CONT INSTITUIC. IDENTIFICACAO        CIDADE       FORM QT-FO
      -    "TOS PAD MES/ANO MESES P/FORM".
       01  CAB05.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "IT.   PATROCINIO                                 DATA-PAGTO
      -    "      VLR-PREVISTO  REALIZ     MESES".
       01  CAB06.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "DOCTO  DATA-REPOR EVENTO               QT-FILM QT-FITA VLR-D
      -    "ESPES VLR-REPORT    P.M.      JUROS     VLR-TOTAL".
       01  CAB07.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "DATA-PREV  DESCRICAO                             QTDE
      -    "   VALOR   MESES      JUROS         TOTAL".
       01  CAB08.
           05  FILLER             PIC X(2)     VALUE SPACES.
           05  FILLER             PIC X(30)    VALUE "R E S U M O".
       01  CAB09.
           05  FILLER             PIC X(2)     VALUE SPACES.
           05  FILLER             PIC X(30)    VALUE "= = = = = =".

       01  LINDET.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

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
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "IED010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "COD080"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD080.
           MOVE "COD081"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD081.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           OPEN INPUT CGD001 CAD010 IED011 COD040 COD005 COD050
                      COD002 COD080 COD081 IED010.
           ACCEPT VARIA-W1 FROM TIME.

           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD005 <> "00"
              MOVE "ERRO ABERTURA COD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD050 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD080 <> "00"
              MOVE "ERRO ABERTURA COD080: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD080 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD081 <> "00"
              MOVE "ERRO ABERTURA COD081: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD081 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    move 1 to cod-usuario-w.
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
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
                    PERFORM CARREGAR-DADOS-REPORTAGEM
                    PERFORM RESUMO-REPORTAGEM
                    PERFORM CARREGA-DESPESA-FIXA
               WHEN GS-CHAMA-PROGRAMA-TRUE
                    PERFORM CHAMA-PROGRAMA-DESPESA
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
       CHAMA-PROGRAMA-DESPESA SECTION.
           MOVE GS-NR-CONTRATO TO PASSAR-STRING-1
           CALL "COP081T" USING PASSAR-STRING-1
           CANCEL "COP081T".

      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.



      *    MOVER DADOS DO CONTRATO
           MOVE ZEROS           TO GS-CONT-PATROCINIO
           MOVE GS-NR-CONTRATO  TO NR-CONTRATO-CO40.

           READ COD040 INVALID KEY CONTINUE
             NOT INVALID KEY
               MOVE NR-CONTRATO-CO40     TO GS-CONTRATO
               MOVE IDENTIFICACAO-CO40   TO GS-CURSO
               MOVE INSTITUICAO-CO40     TO CODIGO-IE10
               READ IED010 INVALID KEY MOVE SPACES TO SIGLA-IE10
               END-READ
               MOVE SIGLA-IE10           TO GS-INSTITUICAO
               MOVE CIDADE-CO40          TO CIDADE
               READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
               END-READ
               MOVE NOME-CID             TO GS-CIDADE
               MOVE REPRESENTANTE-CO40   TO CODIGO-CG01
               READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
               END-READ
               MOVE SPACES               TO GS-LINDET
               MOVE NOME-CG01            TO GS-LINDET(7: 43)
               MOVE ASSINATURA-CO40      TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO DATA-E
               MOVE DATA-E               TO GS-LINDET(50: 14)
               MOVE VLR-COMISSAO-CO40    TO VALOR-E
               MOVE VALOR-E              TO GS-LINDET(66: 13)
               IF ASSINATURA-CO40 > DATA-DIA-I
                  MOVE VLR-COMISSAO-CO40   TO VLR-PAGAR
                  MOVE "Não"               TO GS-LINDET(84: 6)
               ELSE MOVE VLR-COMISSAO-CO40 TO VLR-PAGO
                  MOVE "Sim"               TO GS-LINDET(84: 6)
               END-IF
               MOVE ASSINATURA-CO40        TO GRDIAS-AAMMDD-INICIAL
               MOVE DATA-PREV-VENDA-CO40   TO DATA-INV
               CALL "GRIDAT2" USING DATA-INV
               MOVE DATA-INV               TO GRDIAS-AAMMDD-FINAL
               CALL "GRDIAS1" USING PARAMETROS-GRDIAS
               COMPUTE MESES-W = GRDIAS-NUM-DIAS / 30
               MOVE MESES-W                TO MESES-E MESES-P-FORM
               MOVE MESES-E                TO GS-LINDET(90: 8)
               COMPUTE VLR-GERAL = (VLR-COMISSAO-CO40 * MESES-W)
               MOVE "INSERE-LIST" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
               MOVE QTDE-FORM-CO40       TO GS-FORM
               MOVE PADRAO-CO40          TO GS-PADRAO PADRAO-CO05
               READ COD005 INVALID KEY MOVE ZEROS TO PREV-FOTOS-CO05
               END-READ
               COMPUTE GS-QT-FOTOS = PREV-FOTOS-CO05 * GS-FORM
               MOVE MESANO-PREV-CO40     TO MESANO-I
               MOVE MESANO-I(1: 4)       TO MESANO-W(3: 4)
               MOVE MESANO-I(5: 2)       TO MESANO-W(1: 2)
               MOVE MESANO-W             TO GS-MESANO
      *        CALCULA MESES PARA A FORMATURA
               MOVE DATA-DIA-I             TO GRDIAS-AAMMDD-INICIAL
               MOVE DATA-PREV-VENDA-CO40   TO DATA-INV
               CALL "GRIDAT2" USING DATA-INV
               MOVE DATA-INV               TO GRDIAS-AAMMDD-FINAL
               CALL "GRDIAS1" USING PARAMETROS-GRDIAS
               COMPUTE MESES-W = GRDIAS-NUM-DIAS / 30
               MOVE MESES-W                TO GS-MESES
           END-READ.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

      *    MOVER DADOS DO BRINDE
           MOVE ZEROS TO JUROS-PAGAR JUROS-PAGO

           MOVE GS-NR-CONTRATO    TO NR-CONTRATO-CO50
           MOVE ZEROS             TO ITEM-CO50 REALIZADO-CO50
                                     DATA-VENCTO-CO50.
           START COD050 KEY IS NOT < ALT1-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.
           PERFORM UNTIL ST-COD050 = "10"
             READ COD050 NEXT RECORD AT END MOVE "10" TO ST-COD050
               NOT AT END
                 IF NR-CONTRATO-CO50 <> GS-NR-CONTRATO
                    MOVE "10" TO ST-COD050
                 ELSE
                    IF SUSP-PREV-DEF-CO50 <> 2
                       MOVE SPACES            TO GS-LINDET
                       MOVE ITEM-CO50         TO GS-LINDET(1: 6)
                       MOVE CODBRINDE-CO50 TO CODIGO-CO02
                       READ COD002 INVALID KEY MOVE ZEROS TO VALOR-CO02
                                                          MULT-FORM-CO02
                       END-READ
                       MOVE NOME-CO02         TO GS-LINDET(7: 43)
                       IF DATA-PAGTO-CO50 > ZEROS
                          MOVE DATA-PAGTO-CO50  TO DATA-INV
                          CALL "GRIDAT1" USING DATA-INV
                          MOVE DATA-INV         TO DATA-E
                       ELSE
                          MOVE DATA-VENCTO-CO50 TO DATA-INV
                          CALL "GRIDAT1" USING DATA-INV
                          MOVE DATA-INV         TO DATA-E
                       END-IF
                       MOVE DATA-E            TO GS-LINDET(50: 14)
                       IF CUSTO-UNIT-CO50 <> ZEROS
                          MOVE CUSTO-UNIT-CO50 TO CUSTO-PREVISTO
                       ELSE
                          MOVE VALOR-CO02 TO CUSTO-PREVISTO
                       END-IF
                       IF REALIZADO-CO50 <> 1
                          IF MULT-FORM-CO02 = 2
                             COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                                      QTDE-POR-FORM-CO50
                          ELSE
                             COMPUTE CUSTO-W = (QTDE-POR-FORM-CO50 *
                                        QTDE-FORM-CO50) * CUSTO-PREVISTO
                          END-IF
                       ELSE
                          MOVE VALOR-PAGO-CO50 TO CUSTO-W
                       END-IF
                       MOVE CUSTO-W           TO VALOR-E
                       MOVE VALOR-E           TO GS-LINDET(66: 13)
                       PERFORM CALCULO-JUROS-BRINDE
                       IF REALIZADO-CO50 = 0
                          MOVE "Não"          TO GS-LINDET(84: 6)
                          ADD CUSTO-W TO VLR-PAGAR
                          ADD JUROS-W TO JUROS-PAGAR
                       ELSE
                          ADD CUSTO-W TO VLR-PAGO
                          MOVE "Sim"        TO GS-LINDET(84: 6)
                          ADD JUROS-W TO JUROS-PAGO
                       END-IF
                       COMPUTE MESES-W = DIAS-PRAZO-CO50 / 30
                       MOVE MESES-W            TO MESES-E
                       MOVE MESES-E            TO GS-LINDET(90: 8)
                       COMPUTE VLR-GERAL = (CUSTO-W * MESES-W) +
                                                               VLR-GERAL
                       MOVE "INSERE-LIST" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                    END-IF
                  END-IF
              END-READ
           END-PERFORM.
           COMPUTE PRAZO-MEDIO ROUNDED = VLR-GERAL / (VLR-PAGO +
                                         VLR-PAGAR)
           MOVE 1 TO TAXA-ACUMULADA.
           COMPUTE TAXA-W = (GS-TAXA / 100) + 1.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA * TAXA-W
           END-PERFORM.
           COMPUTE TAXA-JUROS-PM = (TAXA-ACUMULADA - 1) * 100
           COMPUTE TAXA-JUROS = TAXA-ACUMULADA - 1
           COMPUTE TOTAL-GERAL = VLR-PAGO + VLR-PAGAR
           COMPUTE JUROS-TOT-GERAL = JUROS-PAGO + JUROS-PAGAR
           COMPUTE TOTAL-E-JUROS = TOTAL-GERAL + JUROS-TOT-GERAL
           COMPUTE VLR-POR-FORM = TOTAL-E-JUROS / GS-FORM

      *    mover dados p/ janela-total
      *    MOVE "Total do patrocinio/prazo-médio...: " TO GS-LINTOT
      *    MOVE TOTAL-GERAL          TO VALOR-E
      *    MOVE VALOR-E              TO GS-LINTOT(55: 20)
      *    MOVE PRAZO-MEDIO          TO PM-E
      *    MOVE PM-E                 TO GS-LINTOT(86: 20)
      *    MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *    MOVE SPACES               TO GS-LINTOT
      *    MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM

      *    MOVE "Taxa mensal/Juros aplicado ao P.M.: " TO GS-LINTOT
      *    MOVE GS-TAXA              TO PERC-E
      *    MOVE PERC-E               TO GS-LINTOT(61: 6)
      *    MOVE "%"                  TO GS-LINTOT(67: 1)
      *    MOVE TAXA-JUROS-PM        TO TAXA-E
      *    MOVE TAXA-E               TO GS-LINTOT(85: 8)
      *    MOVE "%"                  TO GS-LINTOT(93: 1)
      *    MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *    MOVE SPACES               TO GS-LINTOT
      *    MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM


      *    FATURAMENTO - RESUMO
           MOVE ZEROS TO GS-CONT-LINTOT
           MOVE SPACES TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "(  % fotos): "      TO GS-LINTOT(1: 13)
           MOVE GS-IND-FOTO          TO GS-LINTOT(2: 2)
           MOVE "Preço Foto.: "      TO GS-LINTOT(36: 13)
           MOVE "Total......: "      TO GS-LINTOT(72: 13)
           COMPUTE QTDE-PERC70 = GS-QT-FOTOS * (GS-IND-FOTO / 100)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FOTO
           MOVE QTDE-PERC70          TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(14: 10)
           MOVE GS-PRECO-FOTO        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(49: 15)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FOTO
           MOVE VALOR-W              TO VALOR-E FATURAMENTO-W
           MOVE VALOR-E              TO GS-LINTOT(85: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE "(  % album): "      TO GS-LINTOT(1: 13)
           MOVE GS-IND-ALBUM          TO GS-LINTOT(2: 2)
           MOVE "Preço Album: "      TO GS-LINTOT(36: 13)
           MOVE "Total......: "      TO GS-LINTOT(72: 13)
           COMPUTE QTDE-PERC70 = GS-FORM * (GS-IND-ALBUM / 100)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-ALBUM
           MOVE QTDE-PERC70          TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(14: 10)
           MOVE GS-PRECO-ALBUM       TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(49: 15)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-ALBUM
           MOVE VALOR-W              TO VALOR-E
           ADD VALOR-W               TO FATURAMENTO-W
           MOVE VALOR-E              TO GS-LINTOT(85: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE "(  % Fita).: "      TO GS-LINTOT(1: 13)
           MOVE GS-IND-FITA          TO GS-LINTOT(2: 2)
           MOVE "Preço Fita.: "      TO GS-LINTOT(36: 13)
           MOVE "Total......: "      TO GS-LINTOT(72: 13)
           COMPUTE QTDE-PERC70 = GS-FORM * (GS-IND-FITA / 100)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FITA
           MOVE QTDE-PERC70          TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(14: 10)
           MOVE GS-PRECO-FITA        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(49: 15)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FITA
           MOVE VALOR-W              TO VALOR-E
           ADD VALOR-W               TO FATURAMENTO-W
           MOVE VALOR-E              TO GS-LINTOT(85: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE "Faturamento Previsto...................."
                TO GS-LINTOT
           MOVE FATURAMENTO-W        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(85: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

      *    RESUMO PATROCINIO
      *    MOVE "Total pago/Juros..................: " TO GS-LINTOT
      *    MOVE VLR-PAGO             TO VALOR-E
      *    MOVE VALOR-E              TO GS-LINTOT(55: 20)
      *    MOVE JUROS-PAGO           TO VALOR-E
      *    MOVE VALOR-E              TO GS-LINTOT(80: 13)
      *    MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *    MOVE SPACES               TO GS-LINTOT

      *    MOVE "Total a pagar/Juros...............: " TO GS-LINTOT
      *    MOVE VLR-PAGAR            TO VALOR-E
      *    MOVE VALOR-E              TO GS-LINTOT(55: 20)
      *    MOVE JUROS-PAGAR          TO VALOR-E
      *    MOVE VALOR-E              TO GS-LINTOT(80: 13)
      *    MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *    MOVE SPACES               TO GS-LINTOT

           MOVE "Total Patrocinio/Juros................................"
                 TO GS-LINTOT
           MOVE TOTAL-GERAL          TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(41: 13)
           MOVE JUROS-TOT-GERAL      TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(69: 13)
           MOVE TOTAL-E-JUROS        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(85: 13)
           MOVE TOTAL-E-JUROS        TO TOTAL-CUSTO-DESPESA
           MOVE "("                  TO GS-LINTOT(98: 1)
           MOVE VLR-POR-FORM         TO VALOR-E2
           MOVE VALOR-E2             TO GS-LINTOT(99: 6)
           MOVE ")"                  TO GS-LINTOT(105: 1)

           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT.

      *    MOVE "Total Geral + Juros...............: " TO GS-LINTOT
      *    MOVE TOTAL-E-JUROS        TO VALOR-E
      *    MOVE VALOR-E              TO GS-LINTOT(55: 20)
      *    MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *    MOVE SPACES               TO GS-LINTOT

      *    MOVE "Divisão por formando..............: " TO GS-LINTOT
      *    MOVE VLR-POR-FORM         TO VALOR-E
      *    MOVE VALOR-E              TO GS-LINTOT(55: 20)
      *    MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *    MOVE SPACES               TO GS-LINTOT
      *    MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.

      *-------------------------------------------------------
      *    despesas de reportagem
       CARREGAR-DADOS-REPORTAGEM SECTION.
           OPEN INPUT RED100 RED101 COD003.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           PERFORM GRAVA-DOCTO-REPORTAGEM.
           MOVE ZEROS TO VLR-REPORTAGEM-RES TOT-FITA-RES TOT-FILME-RES
                         DIAS-REPORTAGEM-RES JUROS-REPORTAGEM-RES.

      *    ZERA VARIAVEIS DO RESUMO
           MOVE ZEROS TO GS-CONT-REPORTAGEM.

           MOVE ZEROS TO DOCTO-WK1.
           START WORK1 KEY IS NOT < DOCTO-WK1 INVALID KEY
                       MOVE "10" TO ST-WORK1.
           PERFORM UNTIL ST-WORK1 = "10"
             READ WORK1 NEXT RECORD AT END MOVE "10" TO ST-WORK1
              NOT AT END
               MOVE DOCTO-WK1   TO DOCTO-R100
               READ RED100 INVALID KEY INITIALIZE REG-RED100
               END-READ
               PERFORM CALCULA-DIAS-REPORTAGEM
               PERFORM VERIFICA-CONT-EVENTO
             END-READ
           END-PERFORM.
           CLOSE RED100 RED101 COD003 WORK1.
           DELETE FILE WORK1.
       GRAVA-DOCTO-REPORTAGEM SECTION.
           OPEN OUTPUT WORK1  CLOSE WORK1  OPEN I-O WORK1.
           MOVE GS-CONTRATO    TO CONTRATO-R101.
           START RED101 KEY IS NOT < CONTRATO-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END MOVE "10" TO ST-RED101
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
       VERIFICA-CONT-EVENTO SECTION.
           PERFORM VERIFICA-TOTAL-PARTICIPANTE
           MOVE SPACES              TO GS-LINDET1
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE GS-CONTRATO         TO CONTRATO-R101.
           MOVE ZEROS               TO EVENTO-R101.
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END MOVE "10" TO ST-RED101
               NOT AT END
                IF DOCTO-R101 <> DOCTO-R100 OR
                   CONTRATO-R101 <> GS-CONTRATO
                     MOVE "10" TO ST-RED101
                ELSE
                 MOVE DOCTO-R101          TO GS-LINDET1(1: 7)
                 MOVE DATA-MOV-R100       TO DATA-INV
                 CALL "GRIDAT1" USING DATA-INV
                 MOVE DATA-INV            TO DATA-E GRTIME-DATE
                 MOVE DATA-E              TO GS-LINDET1(8: 11)
                 MOVE EVENTO-R101         TO CODIGO-CO03
                 READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03
                 END-READ
                 MOVE NOME-CO03           TO GS-LINDET1(19: 21)

                 COMPUTE QT-FILME-REP ROUNDED = (TOT-FILME-REPORT-R100
                                 * QT-PARTIC-R101) / TOT-PARTICIPANTE
                 ADD QT-FILME-REP         TO TOT-FILME-RES
                 MOVE QT-FILME-REP        TO QTDE-E-REP
                 MOVE QTDE-E-REP          TO GS-LINDET1(40: 8)
                 COMPUTE QT-FITA-REP ROUNDED = (TOT-FITA-REPORT-R100
                          * QT-PARTIC-R101) / TOT-PARTICIPANTE
                 ADD QT-FITA-REP          TO TOT-FITA-RES
                 MOVE QT-FITA-REP         TO QTDE-E-REP
                 MOVE QTDE-E-REP          TO GS-LINDET1(48: 8)
                 COMPUTE QT-EQUIPE-REP ROUNDED = (QTDE-PESSOAS-R100 *
                                   QT-PARTIC-R101) / TOT-PARTICIPANTE
      *          MOVE QT-EQUIPE-REP       TO QTDE-E
      *          MOVE QTDE-E              TO GS-LINDET(51: 8)
                 COMPUTE DESPESA-W = VLR-COMB-R100 + VLR-HOSP-R100 +
                   VLR-REFEICAO-R100 + VLR-PASSAGEM-R100 + VLR-MAT-R100
                   + VLR-DESPESA-REPORT-R100 +
                   VLR-ALUGUEL-R100 + VLR-OUTROS-R100
                 COMPUTE VLR-DESPESA-REP ROUNDED =
                     (DESPESA-W * QT-PARTIC-R101) / TOT-PARTICIPANTE
                 MOVE VLR-DESPESA-REP     TO VALOR-E1
                 MOVE VALOR-E1            TO GS-LINDET1(56: 11)
                 COMPUTE VLR-REPORT-REP ROUNDED = (VLR-TOT-REPORT-R100 *
                                   QT-PARTIC-R101) / TOT-PARTICIPANTE
                 COMPUTE VLR-REPORTAGEM-RES = VLR-REPORTAGEM-RES +
                     (VLR-REPORT-REP + VLR-DESPESA-REP)
                 MOVE VLR-REPORT-REP      TO VALOR-E1
                 MOVE VALOR-E1            TO GS-LINDET1(67: 11)

      *          CALCULA JUROS DE REPORTAGEM

                 COMPUTE MESES-W = GRTIME-DAYS-FINAL / 30
                 COMPUTE PM-W = GRTIME-DAYS-FINAL / 30
                 MOVE MESES-W     TO PRAZO-MEDIO
                 MOVE 1           TO TAXA-ACUMULADA
                 PERFORM VARYING CONT FROM 1 BY 1 UNTIL
                            CONT > PRAZO-MEDIO
                     COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                              ((GS-TAXA / 100) + 1)
                 END-PERFORM

                 COMPUTE JUROS-W = (VLR-REPORT-REP + VLR-DESPESA-REP) *
                                   (TAXA-ACUMULADA - 1)
                 ADD JUROS-W      TO JUROS-REPORTAGEM-RES

                 MOVE PM-W                TO PM-E
                 MOVE PM-E                TO GS-LINDET1(78: 8)
                 MOVE JUROS-W             TO VALOR-E1
                 MOVE VALOR-E1            TO GS-LINDET1(86: 11)

                 COMPUTE TOTAL-REPORT = VLR-REPORT-REP + VLR-DESPESA-REP
                                        + JUROS-W

                 MOVE TOTAL-REPORT        TO VALOR-E
                 MOVE VALOR-E             TO GS-LINDET1(97: 13)

                 MOVE "INSERE-LIST-REPORTAGEM" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                END-IF
             END-READ
           END-PERFORM.
       CALCULA-DIAS-REPORTAGEM SECTION.
           MOVE 1                    TO GRTIME-TYPE
           MOVE 3                    TO GRTIME-FUNCTION
           MOVE DATA-MOV-R100        TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO GRTIME-DATE
           MOVE DATA-PREV-VENDA-CO40 TO GRTIME-DATE-FINAL
           CALL "GRTIME" USING PARAMETROS-GRTIME.
      *    CANCEL "GRTIME"
           ADD GRTIME-DAYS-FINAL     TO DIAS-REPORTAGEM-RES.

       VERIFICA-TOTAL-PARTICIPANTE SECTION.
           MOVE ZEROS               TO TOT-PARTICIPANTE.
           MOVE DOCTO-R100          TO DOCTO-R101.
           MOVE ZEROS               TO CONTRATO-R101.
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
       RESUMO-REPORTAGEM SECTION.
           MOVE SPACES TO GS-LINTOT
           MOVE "Custo reportagens....................................."
                TO GS-LINTOT(1: 40)
           MOVE VLR-REPORTAGEM-RES  TO VALOR-E
           MOVE VALOR-E             TO GS-LINTOT(41: 16)
           COMPUTE PM-W = ((VLR-REPORTAGEM-RES * DIAS-REPORTAGEM-RES) /
                          VLR-REPORTAGEM-RES) / 30
           MOVE PM-W               TO PM-E
           MOVE PM-E               TO GS-LINTOT(61: 08)
           MOVE JUROS-REPORTAGEM-RES TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(69: 16)
           COMPUTE CUSTO-TOTAL = VLR-REPORTAGEM-RES +
                                 JUROS-REPORTAGEM-RES
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(85: 16)
           ADD CUSTO-TOTAL         TO TOTAL-CUSTO-DESPESA

           MOVE "INSERE-LINTOT" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

      *    MOVE SPACES TO GS-LINTOT
      *    MOVE "Quantia de filmes"  TO GS-LINTOT(1: 41)
      *    MOVE TOT-FILME-RES        TO QTDE-E-REP
      *    MOVE QTDE-E-REP           TO GS-LINTOT(48: 7)
      *    MOVE "INSERE-LINTOT" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.

      *    MOVE SPACES TO GS-LINTOT
      *    MOVE "Quantia de fitas"  TO GS-LINTOT(1: 41)
      *    MOVE TOT-FITA-RES        TO QTDE-E-REP
      *    MOVE QTDE-E-REP          TO GS-LINTOT(48: 7)
      *    MOVE "INSERE-LINTOT" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.

      *------------------------------------------------------
      *    DESPESAS FIXAS
       CARREGA-DESPESA-FIXA SECTION.
      *   DESPESA FIXA
           MOVE SPACES             TO GS-LINDET2
           MOVE ZEROS              TO GS-CONT-DESPESA
           MOVE ZEROS              TO VLR-DESPESA-FIXA-RES
                                      VLR-DESPESA-VENDA-RES.
           MOVE GS-NR-CONTRATO     TO NR-CONTRATO-CO81.
           READ COD081 INVALID KEY CONTINUE
             NOT INVALID KEY
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
                  EVALUATE I
                     WHEN 1 MOVE "01- Custo dos Filmes"
                                  TO GS-LINDET2(1: 46)
                     WHEN 2 MOVE "02- Custo das Fotos "
                                  TO GS-LINDET2(1: 46)
                     WHEN 3 MOVE "03- Custo do Álbum/Estojo"
                                  TO GS-LINDET2(1: 46)
                     WHEN 4 MOVE "04- Custo das Fitas S-VHS"
                                  TO GS-LINDET2(1: 46)
                     WHEN 5 MOVE "05- Custo das Fitas VHS"
                                  TO GS-LINDET2(1: 46)
                     WHEN 6 MOVE "06- Custo dos Estojos de Fitas"
                                  TO GS-LINDET2(1: 46)
                     WHEN 7 MOVE "07- Custo de Montagem"
                                  TO GS-LINDET2(1: 46)
                     WHEN 8 MOVE "08- Custo das Vendas(%)"
                                  TO GS-LINDET2(1: 46)
                  END-EVALUATE

                  MOVE QTDE-CO81(I)     TO QTDE-E1
                  MOVE QTDE-E1          TO GS-LINDET2(47: 10)
                  IF I = 8
                   COMPUTE VALOR-W1 = (QTDE-CO81(I)/100) * FATURAMENTO-W
                   MOVE VALOR-W1        TO VALOR-E
                   MOVE VALOR-W1        TO VALOR-W VLR-DESPESA-VENDA-RES
                  ELSE
                    MOVE VALOR-CO81(I)  TO VALOR-E
                    COMPUTE VALOR-W = QTDE-CO81(I) * VALOR-CO81(I)
                    ADD VALOR-W         TO VLR-DESPESA-FIXA-RES
                  END-IF
                  MOVE VALOR-E          TO GS-LINDET2(57: 14)
                  MOVE VALOR-W          TO VALOR-E
                  MOVE VALOR-E          TO GS-LINDET2(90: 13)
                  MOVE "INSERE-LIST-DESPESA"      TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM

              END-PERFORM
           END-READ.
           MOVE SPACES                     TO GS-LINDET2
           MOVE "INSERE-LIST-DESPESA"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

      *    OUTRAS DESPESAS
           MOVE ZEROS TO VLR-DESPESA-RES JUROS-DESPESA-RES
                         DIAS-DESPESA-RES.
           MOVE SPACES               TO GS-LINDET2
           MOVE GS-NR-CONTRATO       TO NR-CONTRATO-CO80.
           MOVE ZEROS                TO ITEM-CO80.
           START COD080 KEY IS NOT < CHAVE-CO80 INVALID KEY
                 MOVE "10" TO ST-COD080.
           PERFORM UNTIL ST-COD080 = "10"
             READ COD080 NEXT RECORD AT END MOVE "10" TO ST-COD080
               NOT AT END
                 IF NR-CONTRATO-CO80 <> GS-NR-CONTRATO
                    MOVE "10" TO ST-COD080
                 ELSE
                    MOVE DATA-PREV-CO80   TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV         TO DATA-E
                    MOVE DATA-E           TO GS-LINDET2(1: 11)
                    MOVE DESCRICAO-CO80   TO GS-LINDET2(12: 35)
                    MOVE QTDE-PREV-CO80   TO QTDE-E1
                    MOVE QTDE-E1          TO GS-LINDET2(47: 10)
                    MOVE VALOR-PREV-CO80  TO VALOR-E
                    MOVE VALOR-E          TO GS-LINDET2(57: 14)
                    ADD VALOR-PREV-CO80      TO VLR-DESPESA-RES

      *             CALCULA JUROS DE DESPESAS FIXAS
                    MOVE 1                    TO GRTIME-TYPE
                    MOVE 3                    TO GRTIME-FUNCTION
                    MOVE DATA-PREV-CO80       TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV             TO GRTIME-DATE
                    MOVE DATA-PREV-VENDA-CO40 TO GRTIME-DATE-FINAL
                    CALL "GRTIME" USING PARAMETROS-GRTIME
      *             CANCEL "GRTIME"
                    ADD GRTIME-DAYS-FINAL     TO DIAS-DESPESA-RES

                    COMPUTE MESES-W = GRTIME-DAYS-FINAL / 30
                    COMPUTE PM-W = GRTIME-DAYS-FINAL / 30
                    MOVE MESES-W     TO PRAZO-MEDIO
                    MOVE 1           TO TAXA-ACUMULADA
                    PERFORM VARYING CONT FROM 1 BY 1 UNTIL
                               CONT > PRAZO-MEDIO
                        COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                                 ((GS-TAXA / 100) + 1)
                    END-PERFORM

                    COMPUTE JUROS-W = VALOR-PREV-CO80 *
                                      (TAXA-ACUMULADA - 1)
                    ADD JUROS-W      TO JUROS-DESPESA-RES

                    MOVE PM-W                TO PM-E
                    MOVE PM-E                TO GS-LINDET2(71: 8)
                    MOVE JUROS-W             TO VALOR-E1
                    MOVE VALOR-E1            TO GS-LINDET2(79: 11)

                    COMPUTE TOTAL-DESPESA = VALOR-PREV-CO80 + JUROS-W

                    MOVE TOTAL-DESPESA       TO VALOR-E
                    MOVE VALOR-E             TO GS-LINDET2(90: 13)

                    MOVE "INSERE-LIST-DESPESA"      TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
             END-READ
           END-PERFORM.

      *    RESUMO DAS DESPESAS
           MOVE SPACES TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "Custo de Outras Despesas Gerais" TO GS-LINTOT(1: 41)
           MOVE VLR-DESPESA-RES     TO VALOR-E
           MOVE VALOR-E             TO GS-LINTOT(41: 16)
           COMPUTE PM-W = ((VLR-DESPESA-RES * DIAS-DESPESA-RES) /
                          VLR-DESPESA-RES) / 30
           MOVE PM-W               TO PM-E
           MOVE PM-E               TO GS-LINTOT(61: 08)
           MOVE JUROS-DESPESA-RES  TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(69: 16)
           COMPUTE CUSTO-TOTAL = VLR-DESPESA-RES +
                                 JUROS-DESPESA-RES
           MOVE CUSTO-TOTAL        TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(85: 16)
           ADD CUSTO-TOTAL         TO TOTAL-CUSTO-DESPESA

           MOVE "INSERE-LINTOT" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

      *    RESUMO DESPESA FIXA
           MOVE SPACES                     TO GS-LINTOT
           MOVE "Custo Gerais de Produção" TO GS-LINTOT(1: 41)
           MOVE VLR-DESPESA-FIXA-RES     TO VALOR-E
           MOVE VALOR-E                  TO GS-LINTOT(85: 16)
           ADD VLR-DESPESA-FIXA-RES      TO TOTAL-CUSTO-DESPESA

           MOVE "INSERE-LINTOT" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES                     TO GS-LINTOT
           MOVE "Custo das Vendas        " TO GS-LINTOT(1: 41)
           MOVE VLR-DESPESA-VENDA-RES      TO VALOR-E
           MOVE VALOR-E                    TO GS-LINTOT(85: 16)
           ADD VLR-DESPESA-VENDA-RES       TO TOTAL-CUSTO-DESPESA

           MOVE "INSERE-LINTOT" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


      *    TOTALIZACAO DOS CUSTOS E DEPESAS
           MOVE SPACES TO GS-LINTOT
           MOVE "INSERE-LINTOT" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "Total Geral de Custos/Despesas........................"
                TO GS-LINTOT(1: 41)
           MOVE TOTAL-CUSTO-DESPESA TO VALOR-E
           MOVE VALOR-E             TO GS-LINTOT(85: 16)
           MOVE "INSERE-LINTOT" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.



      *    TOTALIZACAO = FATURAMENTO - DESPESAS+CUSTO
           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "Resultado Final.........................."
                 TO GS-LINTOT
           COMPUTE RESULTADO-FINAL = FATURAMENTO-W -
                                     TOTAL-CUSTO-DESPESA
           MOVE RESULTADO-FINAL      TO VALOR-ES
           MOVE VALOR-ES             TO GS-LINTOT(85: 16)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO VLR-PAGO VLR-PAGAR CUSTO-W
                         CUSTO-PREVISTO VLR-GERAL JUROS-PAGO
                         JUROS-PAGAR TOTAL-GERAL JUROS-TOT-GERAL
                         TOTAL-E-JUROS VLR-POR-FORM.

       CALCULO-JUROS-BRINDE SECTION.
           COMPUTE MESES-W = DIAS-PRAZO-CO50 / 30.
           MOVE MESES-W TO PRAZO-MEDIO
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-W * (TAXA-ACUMULADA - 1).
       FIM-CARREGA-LISTA SECTION.
      *--------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.

           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL.
           MOVE GS-CONTRATO          TO LINDET-REL(1: 5)
           MOVE GS-INSTITUICAO       TO LINDET-REL(6: 11)
           MOVE GS-CURSO             TO LINDET-REL(17: 21)
           MOVE GS-CIDADE            TO LINDET-REL(38: 13)
           MOVE GS-FORM              TO LINDET-REL(51: 5)
           MOVE GS-QT-FOTOS          TO LINDET-REL(56: 9)
           MOVE GS-PADRAO            TO LINDET-REL(65: 4)
           MOVE GS-MESANO            TO MESANO-E
           MOVE MESANO-E             TO LINDET-REL(69: 8)
           MOVE GS-MESES             TO LINDET-REL(77: 14)
           WRITE REG-RELAT FROM CAB04
           WRITE REG-RELAT FROM CAB03
           WRITE REG-RELAT FROM LINDET

           WRITE REG-RELAT FROM CAB05 AFTER 2
           WRITE REG-RELAT FROM CAB03
           ADD 6 TO LIN
      *    MOVER DADOS PARA IMPRESSORA DOS PATROCINIO
           MOVE ZEROS TO GS-CONT-IMP.
           PERFORM UNTIL GS-CONT-IMP = GS-CONT-PATROCINIO
             MOVE "CARREGA-LINHA-PATROCINIO" TO DS-PROCEDURE
             PERFORM CALL-DIALOG-SYSTEM
             MOVE GS-LINHA-IMP    TO LINDET-REL
             WRITE REG-RELAT FROM LINDET
             ADD 1 TO LIN
             IF LIN > 56 PERFORM CABECALHO
             END-IF
           END-PERFORM.


      *    MOVER DADOS PARA IMPRESSORA DA REPORTAGEM
           WRITE REG-RELAT FROM CAB06 AFTER 2
           WRITE REG-RELAT FROM CAB03
           ADD 3 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

           MOVE ZEROS TO GS-CONT-IMP.
           PERFORM UNTIL GS-CONT-IMP = GS-CONT-REPORTAGEM
             MOVE "CARREGA-LINHA-REPORTAGEM" TO DS-PROCEDURE
             PERFORM CALL-DIALOG-SYSTEM
             MOVE GS-LINHA-IMP    TO LINDET-REL
             WRITE REG-RELAT FROM LINDET
             ADD 1 TO LIN
             IF LIN > 56 PERFORM CABECALHO
             END-IF
           END-PERFORM.

      *    MOVER DADOS PARA IMPRESSORA DAS DESPESAS
           WRITE REG-RELAT FROM CAB07 AFTER 2
           WRITE REG-RELAT FROM CAB03
           ADD 3 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

           MOVE ZEROS TO GS-CONT-IMP.
           PERFORM UNTIL GS-CONT-IMP = GS-CONT-DESPESA
             MOVE "CARREGA-LINHA-DESPESA" TO DS-PROCEDURE
             PERFORM CALL-DIALOG-SYSTEM
             MOVE GS-LINHA-IMP    TO LINDET-REL
             WRITE REG-RELAT FROM LINDET
             ADD 1 TO LIN
             IF LIN > 56 PERFORM CABECALHO
             END-IF
           END-PERFORM.

      *    MOVER DADOS PARA IMPRESSORA DO RESUMO
           WRITE REG-RELAT FROM CAB08 AFTER 2
           WRITE REG-RELAT FROM CAB09
           MOVE SPACES TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           ADD 4 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

           MOVE ZEROS TO GS-CONT-IMP.
           PERFORM UNTIL GS-CONT-IMP = GS-CONT-LINTOT
             MOVE "CARREGA-LINHA-RESUMO" TO DS-PROCEDURE
             PERFORM CALL-DIALOG-SYSTEM
             MOVE GS-LINHA-IMP    TO LINDET-REL
             WRITE REG-RELAT FROM LINDET
             ADD 1 TO LIN
             IF LIN > 56 PERFORM CABECALHO
             END-IF
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
           MOVE 5 TO LIN.
      *-------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP110" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 COD050 IED011 CAD010 CGD001
                 COD080 COD081 COD005 COD002 IED010.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
