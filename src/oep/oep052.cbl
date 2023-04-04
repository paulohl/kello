       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OEP052.
       DATE-WRITTEN. 28/02/2000.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: Relatório 501 DE ORGANIZAÇÃO DE EVENTOS
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
           COPY COPX005.
           COPY COPX040.
           COPY OEPX010.
           COPY OEPX020.
           COPY IEPX010.
           COPY IEPX011.
           COPY CAPX010.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY IEPW010.
       COPY IEPW011.
       COPY CAPW010.
       COPY COPW002.
       COPY COPW005.
       COPY COPW040.
       COPY OEPW010.
       COPY OEPW020.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "OEP052.CPB".
           COPY "OEP052.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-OED010             PIC XX       VALUE SPACES.
           05  ST-OED020             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(8)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PERC-E                PIC ZZZ,ZZ    BLANK WHEN ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  PM-E                  PIC ZZZZ,ZZ.
           05  MESES-E               PIC ZZZZ,ZZ.
           05  TAXA-E                PIC ZZZ,ZZZZ.
           05  QTDE-E                PIC ZZZ.ZZZ,ZZ.

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
           05  FATURAMENTO-W         PIC 9(8)V99  VALUE ZEROS.


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
           "IT. PATROCINIO           DATA-PAGTO  VLR-PREVISTO REALIZ MES
      -    "ES".
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
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "IED010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "OED010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-OED010.
           MOVE "OED020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-OED020.
           OPEN INPUT CGD001 CAD010 IED011 COD040 COD005 OED010 OED020
                      COD002 IED010.
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
           IF ST-OED010 <> "00"
              MOVE "ERRO ABERTURA OED020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-OED020 <> "00"
              MOVE "ERRO ABERTURA OED020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED020 TO GS-MENSAGEM-ERRO(23: 02)
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
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
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
      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO VLR-PAGO VLR-PAGAR VLR-GERAL.
      *    MOVER DADOS DO CONTRATO
           MOVE GS-NR-CONTRATO  TO NR-CONTRATO-CO40.

           READ COD040 INVALID KEY CONTINUE
             NOT INVALID KEY
               MOVE NR-CONTRATO-CO40     TO GS-CONTRATO NR-CONTRATO-OE10
               READ OED010 INVALID KEY INITIALIZE REG-OED010
               END-READ
               MOVE IDENTIFICACAO-CO40   TO GS-CURSO
               MOVE INSTITUICAO-CO40     TO CODIGO-IE10
               READ IED010 INVALID KEY MOVE SPACES TO SIGLA-IE10
               END-READ
               MOVE SIGLA-IE10           TO GS-INSTITUICAO
               MOVE CIDADE-CO40          TO CIDADE
               READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
               END-READ
               MOVE NOME-CID             TO GS-CIDADE
      *        MOVE REPRESENTANTE-CO40   TO CODIGO-CG01
      *        READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
      *        END-READ
      *        MOVE SPACES               TO GS-LINDET
      *        MOVE NOME-CG01            TO GS-LINDET(7: 43)
      *        MOVE ASSINATURA-CO40      TO DATA-INV
      *        CALL "GRIDAT1" USING DATA-INV
      *        MOVE DATA-INV             TO DATA-E
      *        MOVE DATA-E               TO GS-LINDET(50: 14)
      *        MOVE VLR-COMISSAO-CO40    TO VALOR-E
      *        MOVE VALOR-E              TO GS-LINDET(66: 13)
      *        IF ASSINATURA-CO40 > DATA-DIA-I
      *           MOVE VLR-COMISSAO-CO40   TO VLR-PAGAR
      *           MOVE "Não"               TO GS-LINDET(84: 6)
      *        ELSE MOVE VLR-COMISSAO-CO40 TO VLR-PAGO
      *           MOVE "Sim"               TO GS-LINDET(84: 6)
      *        END-IF
               MOVE ASSINATURA-OE10        TO GRDIAS-AAMMDD-INICIAL
               MOVE DATA-ULT-EVENTO-OE10   TO GRDIAS-AAMMDD-FINAL
               CALL "GRDIAS1" USING PARAMETROS-GRDIAS
               COMPUTE MESES-W = GRDIAS-NUM-DIAS / 30
               MOVE MESES-W                TO MESES-E MESES-P-FORM
                                              GS-MESES
      *        MOVE MESES-E                TO GS-LINDET(90: 8)
      *        COMPUTE VLR-GERAL = (VLR-COMISSAO-CO40 * MESES-W)
      *        MOVE "INSERE-LIST" TO DS-PROCEDURE
      *        PERFORM CALL-DIALOG-SYSTEM
               MOVE TOTAL-ALUNO-OE10     TO GS-FORM
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
           MOVE GS-NR-CONTRATO    TO NR-CONTRATO-OE20
           MOVE ZEROS             TO ITEM-OE20 REALIZADO-OE20
                                     DATA-VENCTO-OE20.
           START OED020 KEY IS NOT < ALT1-OE20 INVALID KEY
                 MOVE "10" TO ST-OED020.
           PERFORM UNTIL ST-OED020 = "10"
             READ OED020 NEXT RECORD AT END MOVE "10" TO ST-OED020
               NOT AT END
                 IF NR-CONTRATO-OE20 <> GS-NR-CONTRATO
                    MOVE "10" TO ST-OED020
                 ELSE
                     MOVE ITEM-OE20         TO GS-LINDET(1: 6)
                     MOVE CODBRINDE-OE20 TO CODIGO-CO02
                     READ COD002 INVALID KEY MOVE ZEROS TO VALOR-CO02
                                                      MULT-FORM-CO02
                     END-READ
                     MOVE NOME-CO02         TO GS-LINDET(7: 43)
                     IF DATA-PAGTO-OE20 = ZEROS
                        MOVE DATA-VENCTO-OE20 TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV         TO DATA-E
                     ELSE MOVE DATA-PAGTO-OE20 TO DATA-E
                     END-IF
                     MOVE DATA-E            TO GS-LINDET(50: 14)
                     IF CUSTO-UNIT-OE20 <> ZEROS
                        MOVE CUSTO-UNIT-OE20 TO CUSTO-PREVISTO
                     ELSE MOVE VALOR-CO02 TO CUSTO-PREVISTO
                     END-IF
                     IF REALIZADO-OE20 <> 1
                        IF MULT-FORM-CO02 = 2
                           COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                             QTDE-POR-FORM-OE20
                        ELSE COMPUTE CUSTO-W = (QTDE-POR-FORM-OE20 *
                            QTDE-FORM-OE20) * CUSTO-PREVISTO
                        END-IF
                     ELSE MOVE VALOR-PAGO-OE20 TO CUSTO-W
                     END-IF
                     MOVE CUSTO-W           TO VALOR-E
                     MOVE VALOR-E           TO GS-LINDET(66: 13)
                     PERFORM CALCULO-JUROS-BRINDE-OE
                     IF REALIZADO-OE20 = 0
                        MOVE "Não"          TO GS-LINDET(84: 6)
                        ADD CUSTO-W TO VLR-PAGAR
                        ADD JUROS-W TO JUROS-PAGAR
                     ELSE ADD CUSTO-W TO VLR-PAGO
                           MOVE "Sim"        TO GS-LINDET(84: 6)
                           ADD JUROS-W TO JUROS-PAGO
                     END-IF
                     COMPUTE MESES-W = DIAS-PRAZO-OE20 / 30
                     MOVE MESES-W            TO MESES-E
                     MOVE MESES-E            TO GS-LINDET(90: 6)
                     COMPUTE VLR-GERAL = (CUSTO-W * MESES-W) + VLR-GERAL
                     MOVE "INSERE-LIST" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
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
           COMPUTE TOTAL-GERAL = VLR-PAGO + VLR-PAGAR
           COMPUTE JUROS-TOT-GERAL = JUROS-PAGO + JUROS-PAGAR
           COMPUTE TOTAL-E-JUROS = TOTAL-GERAL + JUROS-TOT-GERAL
           COMPUTE VLR-POR-FORM = TOTAL-E-JUROS / GS-FORM

      *    mover dados p/ janela-total
           MOVE ZEROS TO GS-CONT-LINTOT
           MOVE "Total do patrocinio/prazo-médio...: " TO GS-LINTOT
           MOVE TOTAL-GERAL          TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE PRAZO-MEDIO          TO PM-E
           MOVE PM-E                 TO GS-LINTOT(86: 20)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "Taxa mensal/Juros aplicado ao P.M.: " TO GS-LINTOT
           MOVE GS-TAXA              TO PERC-E
           MOVE PERC-E               TO GS-LINTOT(61: 6)
           MOVE "%"                  TO GS-LINTOT(67: 1)
           MOVE TAXA-JUROS-PM        TO TAXA-E
           MOVE TAXA-E               TO GS-LINTOT(85: 8)
           MOVE "%"                  TO GS-LINTOT(93: 1)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "Total pago/Juros..................: " TO GS-LINTOT
           MOVE VLR-PAGO             TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE JUROS-PAGO           TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(80: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE "Total a pagar/Juros...............: " TO GS-LINTOT
           MOVE VLR-PAGAR            TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE JUROS-PAGAR          TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(80: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE "Total Geral/Juros.................: " TO GS-LINTOT
           MOVE TOTAL-GERAL          TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE JUROS-TOT-GERAL      TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(80: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE "Total Geral + Juros...............: " TO GS-LINTOT
           MOVE TOTAL-E-JUROS        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE "Divisão por formando..............: " TO GS-LINTOT
           MOVE VLR-POR-FORM         TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO VLR-PAGO VLR-PAGAR CUSTO-W
                         CUSTO-PREVISTO VLR-GERAL JUROS-PAGO
                         JUROS-PAGAR TOTAL-GERAL JUROS-TOT-GERAL
                         TOTAL-E-JUROS VLR-POR-FORM.
       CALCULO-JUROS-BRINDE-OE SECTION.
           COMPUTE MESES-W = DIAS-PRAZO-OE20 / 30.
           MOVE MESES-W TO PRAZO-MEDIO
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-W * (TAXA-ACUMULADA - 1).
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

           MOVE GS-NR-CONTRATO  TO NR-CONTRATO-CO40.

           READ COD040 INVALID KEY CONTINUE
             NOT INVALID KEY
               MOVE SPACES               TO LINDET-REL
               MOVE REPRESENTANTE-CO40   TO CODIGO-CG01
               READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
               END-READ
               MOVE NOME-CG01            TO LINDET-REL(5: 21)
               MOVE ASSINATURA-CO40      TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO DATA-E
               MOVE DATA-E               TO LINDET-REL(26: 11)
               MOVE VLR-COMISSAO-CO40    TO VALOR-E
               MOVE VALOR-E              TO LINDET-REL(37: 14)
               IF ASSINATURA-CO40 > DATA-DIA-I
                  MOVE "Não"             TO LINDET-REL(51: 7)
               ELSE MOVE VLR-COMISSAO-CO40 TO VLR-PAGO
                  MOVE "Sim"             TO LINDET-REL(51: 7)
               END-IF
               MOVE MESES-P-FORM         TO LINDET-REL(58: 5)
               WRITE REG-RELAT FROM LINDET
               ADD 1 TO LIN
           END-READ.

      *    MOVER DADOS DO BRINDE
           MOVE GS-NR-CONTRATO    TO NR-CONTRATO-OE20
           MOVE ZEROS             TO ITEM-OE20 DATA-VENCTO-OE20
                                     REALIZADO-OE20.
           START OED020 KEY IS NOT < ALT1-OE20 INVALID KEY
                 MOVE "10" TO ST-OED020.
           PERFORM UNTIL ST-OED020 = "10"
             READ OED020 NEXT RECORD AT END MOVE "10" TO ST-OED020
               NOT AT END
                 IF NR-CONTRATO-OE20 <> GS-NR-CONTRATO
                    MOVE "10" TO ST-OED020
                 ELSE
                     MOVE ITEM-OE20         TO LINDET-REL(1: 4)
                     MOVE CODBRINDE-OE20 TO CODIGO-CO02
                     READ COD002 INVALID KEY MOVE ZEROS TO VALOR-CO02
                     END-READ
                     MOVE NOME-CO02         TO LINDET-REL(5: 21)
                     IF DATA-PAGTO-OE20 = ZEROS
                        MOVE DATA-VENCTO-OE20 TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV         TO DATA-E
                     ELSE MOVE DATA-PAGTO-OE20 TO DATA-E
                     END-IF
                     MOVE DATA-E            TO LINDET-REL(26: 11)
                     IF CUSTO-UNIT-OE20 <> ZEROS
                        MOVE CUSTO-UNIT-OE20 TO CUSTO-PREVISTO
                     ELSE MOVE VALOR-CO02 TO CUSTO-PREVISTO
                     END-IF
                     IF REALIZADO-OE20 <> 1
                        IF MULT-FORM-CO02 = 2
                           COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                             QTDE-POR-FORM-OE20
                        ELSE COMPUTE CUSTO-W = (QTDE-POR-FORM-OE20 *
                            QTDE-FORM-OE20) * CUSTO-PREVISTO
                        END-IF
                     ELSE MOVE VALOR-PAGO-OE20 TO CUSTO-W
                     END-IF
                     MOVE CUSTO-W           TO VALOR-E
                     MOVE VALOR-E           TO LINDET-REL(37: 14)
                     IF REALIZADO-OE20 = 0
                        MOVE "Não"          TO LINDET-REL(51: 7)
                     ELSE ADD CUSTO-W TO VLR-PAGO
                           MOVE "Sim"        TO LINDET-REL(51: 7)
                     END-IF
                     COMPUTE MESES-W = DIAS-PRAZO-OE20 / 30
                     MOVE MESES-W            TO MESES-E
                     MOVE MESES-E            TO LINDET-REL(58: 5)
                     WRITE REG-RELAT FROM LINDET
                     ADD 1 TO LIN
                     IF LIN > 56 PERFORM CABECALHO
                  END-IF
              END-READ
           END-PERFORM.

      *    TOTALIZA
           ADD 4 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
           MOVE "Total do patrocinio/prazo-médio...: " TO LINDET-REL
           MOVE TOTAL-GERAL          TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           MOVE PRAZO-MEDIO          TO PM-E
           MOVE PM-E                 TO LINDET-REL(86: 20)
           WRITE REG-RELAT FROM CAB03
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL

           MOVE "Taxa mensal/Juros aplicado ao P.M.: " TO LINDET-REL
           MOVE GS-TAXA              TO PERC-E
           MOVE PERC-E               TO LINDET-REL(61: 6)
           MOVE "%"                  TO LINDET-REL(67: 1)
           MOVE TAXA-JUROS-PM        TO TAXA-E
           MOVE TAXA-E               TO LINDET-REL(85: 8)
           MOVE "%"                  TO LINDET-REL(93: 1)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE SPACES               TO LINDET-REL

           ADD 4 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
           MOVE "Total pago/Juros..................: " TO LINDET-REL
           MOVE VLR-PAGO             TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           MOVE JUROS-PAGO           TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(80: 13)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE SPACES TO LINDET-REL

           MOVE "Total a pagar/Juros...............: " TO LINDET-REL
           MOVE VLR-PAGAR            TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           MOVE JUROS-PAGAR          TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(80: 13)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL

           MOVE "Total Geral/Juros.................: " TO LINDET-REL
           MOVE TOTAL-GERAL          TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           MOVE JUROS-TOT-GERAL      TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(80: 13)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES TO LINDET-REL

           ADD 3 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
           MOVE "Total Geral + Juros...............: " TO LINDET-REL
           MOVE TOTAL-E-JUROS        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE SPACES TO LINDET-REL

           MOVE "Divisão por formando..............: " TO LINDET-REL
           MOVE VLR-POR-FORM         TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES TO LINDET-REL

           COPY DESCONDENSA.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
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
           MOVE "OEP052" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 OED010 OED020 IED011 CAD010 CGD001
                 COD005 COD002 IED010.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
