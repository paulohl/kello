       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP043.
       DATE-WRITTEN. 15/09/1999.
       AUTHOR. MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Relatório 501 DO CONTRATO SIMULADO
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
           COPY COPX002.
           COPY COPX042.
           COPY COPX043.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY COPW042.
       COPY COPW043.
       COPY COPW002.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP043.CPB".
           COPY "COP043.CPY".
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
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD042             PIC XX       VALUE SPACES.
           05  ST-COD043             PIC XX       VALUE SPACES.
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
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(8)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(8)     VALUE ZEROS.
           05  DATAWI.
               10  ANO-WI          PIC 9(4).
               10  MES-WI          PIC 9(2).
               10  DIA-WI          PIC 9(2).
           05  DATA-WI REDEFINES DATAWI PIC 9(8).
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PERC-E                PIC ZZZ,ZZ    BLANK WHEN ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  PM-E                  PIC ZZZZ,ZZ.
           05  MESES-E               PIC ZZZZ.
           05  TAXA-E                PIC ZZZ,ZZZZ.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ.
           05  QTDE-E                PIC ZZZ.ZZZ,ZZ.
           05  DATA-PREV-VENDA-W     PIC 9(8)     VALUE ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.

      *    VARIAVEIS P/ CALCULO DE PATROCINIO PAGOS E A PAGAR
           05  CUSTO-TOTAL           PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-PREVISTO        PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-W               PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-W               PIC 9(8)V99  VALUE ZEROS.
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  TAXA-ACUMULADA1       PIC 9(3)V9(8) VALUE ZEROS.
           05  TAXA-W                PIC 9(3)V99999 VALUE ZEROS.
           05  TAXA-JUROS-PM         PIC 9(4)V9999 VALUE ZEROS.
           05  TAXA-JUROS-PM1        PIC 9(4)V9999 VALUE ZEROS.
           05  TAXA-JUROS            PIC 9(4)V9999 VALUE ZEROS.
           05  TAXA-JUROS1           PIC 9(4)V9999 VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  PRAZO-MEDIO1          PIC 9(4)     VALUE ZEROS.
           05  MESES-W               PIC 9(3)     VALUE ZEROS.
           05  MESES-P-FORM          PIC 9(3)     VALUE ZEROS.
           05  VLR-PAGAR             PIC 9(8)V99  VALUE ZEROS.
           05  VLR-PAGAR1            PIC 9(8)V99  VALUE ZEROS.
           05  VLR-GERAL             PIC 9(8)V99  VALUE ZEROS.
           05  VLR-GERAL1            PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-E-JUROS         PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-E-JUROS1        PIC 9(8)V99  VALUE ZEROS.
           05  VLR-GERAL-CD          PIC 9(8)V99  VALUE ZEROS.
           05  VLR-POR-FORM          PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGAR           PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGAR1          PIC 9(8)V99  VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.

      *    VARIAVEIS P/ CALCULAR 70% DE QTDE E VALORES DE FOTO/FITA/ALB
           05  QTDE-PERC70           PIC 9(6)V99  VALUE ZEROS.
           05  VALOR-W               PIC 9(8)V99  VALUE ZEROS.
           05  FATURAMENTO-W         PIC 9(8)V99  VALUE ZEROS.


           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 WS-DATA-COMP.
          05 WS-ANO-INI            PIC 9(04).
          05 WS-MES-INI            PIC 9(02).

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
           05  FILLER              PIC X(70)   VALUE
           "SIMULACAO DO CONTRATO - RELATORIO 501          ".
       01  CAB03.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(110)   VALUE
           "SEQUEN INSTITUIC. IDENTIFICACAO        CIDADE       FORM QT-
      -    "FOTOS PAD MES/ANO MESES P/FORM".
       01  CAB05.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "IT. PATROCINIO           DATA-PAGTO  VLR-PREVISTO REALIZ MES
      -    "ES".
       01  LINDET.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

       01 PERC-VENDA               PIC 9(02) VALUE ZEROS.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).


       LINKAGE SECTION.
           COPY "PARAMETR".

       01  STRING-1               PIC X(65) VALUE SPACES.

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE STRING-1(12:2) TO PERC-VENDA
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
           MOVE "COD002"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-COD002.
           MOVE "COD042"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-COD042.
           MOVE "COD043"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-COD043.
           OPEN I-O    COD002 COD042 COD043.
           CLOSE       COD002 COD042 COD043.
           OPEN INPUT  COD002 COD042 COD043.
           IF ST-COD002 <> "00"
              MOVE SPACES TO GS-MENSAGEM-ERRO
              STRING "ERRO ABERTURA COD002: " ST-COD002 X"0DA0"
                      PATH-COD002 INTO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD042 <> "00"
              MOVE "ERRO ABERTURA COD042: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD042 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD043 <> "00"
              MOVE "ERRO ABERTURA COD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD043 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF COD-USUARIO-W NOT NUMERIC
      *       MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
      *       PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-CARREGA-SEQUENCIA-TRUE
                    PERFORM CARREGA-SEQUENCIA
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CRITICAR-FLG-TRUE
                    PERFORM CRITICAR-DATA
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CRITICAR-DATA SECTION.
           IF GS-PRE-DATA = ZEROS
              MOVE GS-SEQ   TO SEQ-CO42
              INITIALIZE JUROS-PAGAR

              READ COD042 INVALID KEY
                   CONTINUE
              NOT INVALID KEY
                 MOVE ASSINATURA-CO42      TO DATA-INV
                 CALL "GRIDAT2" USING DATA-INV
                 MOVE DATA-INV             TO DATA-WI
                 IF MES-WI = 12 MOVE 1 TO DIA-WI
                           MOVE 1 TO MES-WI
                           ADD 1 TO ANO-WI
                 ELSE MOVE 1 TO DIA-WI
                       ADD 1 TO MES-WI
                 END-IF
                 MOVE DATA-WI              TO GRDIAS-AAMMDD-INICIAL

                 MOVE MESANO-CO42(1: 4)    TO ANO-WI
                 MOVE MESANO-CO42(5: 2)    TO MES-WI
                 MOVE ZEROS                TO DIA-WI
                 IF MES-WI = 11 MOVE 01 TO MES-WI
                                ADD 1   TO ANO-WI
                                MOVE 31 TO DIA-WI
                 ELSE IF MES-WI = 12 MOVE 02 TO MES-WI
                                     ADD 1   TO ANO-WI
                                     MOVE 28 TO DIA-WI
                      ELSE
                        ADD 2 TO MES-WI
                        IF MES-WI = 03 OR 05 OR 07 OR 08 OR 10 OR 12
                           MOVE 31 TO DIA-WI
                        ELSE MOVE 30 TO DIA-WI
                        END-IF
                      END-IF
                 END-IF
                 STRING DIA-WI MES-WI ANO-WI INTO GS-PRE-DATA.

           MOVE GS-SEQ   TO SEQ-CO42
           READ COD042 INVALID KEY
                INITIALIZE REG-COD042.

           MOVE GS-PRE-DATA(3:2) TO WS-MES-INI
           MOVE GS-PRE-DATA(5:4) TO WS-ANO-INI

           IF MESANO-CO42 > WS-DATA-COMP
              MOVE "Data informada superior ao Mês/Ano Previsto" TO
                          MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       EXIBIR-MENSAGEM SECTION.
           move    1      to gs-flag-critica
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
      *--------------------------------------------------------------
       CARREGA-SEQUENCIA SECTION.
           MOVE STRING-1(1: 6)   TO GS-SEQ.
           MOVE STRING-1(8: 2)   TO IMPRESSORA-W.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
      *    MOVER DADOS DO CONTRATO
           MOVE GS-SEQ           TO SEQ-CO42.

           INITIALIZE JUROS-PAGAR

           READ COD042 INVALID KEY
                CONTINUE
           NOT INVALID KEY
               MOVE IDENTIFICACAO-CO42   TO GS-CURSO
               MOVE INSTITUICAO-CO42     TO GS-INSTITUICAO
               MOVE CIDADE-CO42          TO GS-CIDADE
               MOVE SPACES               TO GS-LINDET
               MOVE REPRESENTANTE-CO42   TO GS-LINDET(4: 43)
               MOVE VLR-COMISSAO-CO42    TO VALOR-E1 CUSTO-W
                                            CUSTO-PREVISTO
               MOVE VALOR-E1             TO GS-LINDET(30: 11)
               MOVE VALOR-E1             TO GS-LINDET(41: 11)

      *        MOVE ASSINATURA-CO42      TO DATA-E
      *        MOVE DATA-E               TO GS-LINDET(52: 14)
      *        MOVE ASSINATURA-CO42      TO DATA-INV
      *        CALL "GRIDAT2" USING DATA-INV
      *        MOVE DATA-INV             TO DATA-WI
      *        IF MES-WI = 12 MOVE 1 TO DIA-WI
      *                  MOVE 1 TO MES-WI
      *                  ADD 1 TO ANO-WI
      *        ELSE MOVE 1 TO DIA-WI
      *              ADD 1 TO MES-WI
      *        END-IF
      *        MOVE DATA-WI              TO GRDIAS-AAMMDD-INICIAL
      *
      *        MOVE MESANO-CO42(1: 4)    TO ANO-WI
      *        MOVE MESANO-CO42(5: 2)    TO MES-WI
      *        MOVE ZEROS                TO DIA-WI
      *        IF MES-WI = 11 MOVE 01 TO MES-WI
      *                       ADD 1   TO ANO-WI
      *                       MOVE 31 TO DIA-WI
      *        ELSE IF MES-WI = 12 MOVE 02 TO MES-WI
      *                            ADD 1   TO ANO-WI
      *                            MOVE 28 TO DIA-WI
      *             ELSE
      *               ADD 2 TO MES-WI
      *               IF MES-WI = 03 OR 05 OR 07 OR 08 OR 10 OR 12
      *                  MOVE 31 TO DIA-WI
      *               ELSE MOVE 30 TO DIA-WI
      *               END-IF
      *             END-IF
      *        END-IF
      *        MOVE DATA-WI TO GRDIAS-AAMMDD-FINAL
      *                        DATA-PREV-VENDA-W

               STRING GS-PRE-DATA(5:4) GS-PRE-DATA(3:2) GS-PRE-DATA(1:2)
                 INTO GRDIAS-AAMMDD-FINAL
               MOVE GRDIAS-AAMMDD-FINAL    TO DATA-PREV-VENDA-W

               CALL "GRDIAS1" USING PARAMETROS-GRDIAS
               MOVE GRDIAS-NUM-DIAS        TO GS-LINDET(73: 5)
               PERFORM CALCULO-JUROS-BRINDE
               ADD  JUROS-W                TO JUROS-PAGAR
               MOVE JUROS-W                TO VALOR-E1
               MOVE VALOR-E1               TO GS-LINDET(78: 11)
               COMPUTE CUSTO-TOTAL = CUSTO-W + JUROS-W
               MOVE CUSTO-TOTAL            TO VALOR-E1
               ADD CUSTO-W                 TO VLR-PAGAR
               MOVE VALOR-E1               TO GS-LINDET(89: 11)
               COMPUTE MESES-W = GRDIAS-NUM-DIAS / 30
               MOVE MESES-W                TO MESES-E MESES-P-FORM
               MOVE MESES-E                TO GS-LINDET(100: 8)
               COMPUTE VLR-GERAL = (VLR-COMISSAO-CO42 * MESES-W)
               MOVE "INSERE-LIST" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
               MOVE QT-FORM-CO42         TO GS-FORM
               MOVE PADRAO-CO42          TO GS-PADRAO
               MOVE QT-FOTOS-CO42        TO GS-QT-FOTOS
               MOVE MESANO-CO42          TO MESANO-I
               MOVE MESANO-I(1: 4)       TO MESANO-W(3: 4)
               MOVE MESANO-I(5: 2)       TO MESANO-W(1: 2)
               MOVE MESANO-W             TO GS-MESANO
      *        CALCULA MESES PARA A FORMATURA
               MOVE DATA-DIA-I             TO GRDIAS-AAMMDD-INICIAL
               MOVE DATA-PREV-VENDA-W      TO GRDIAS-AAMMDD-FINAL
               CALL "GRDIAS1"           USING PARAMETROS-GRDIAS
               IF GRDIAS-AAMMDD-INICIAL > GRDIAS-AAMMDD-FINAL
                  MOVE 0 TO MESES-W
               ELSE
                 COMPUTE MESES-W = GRDIAS-NUM-DIAS / 30
               END-IF
               MOVE MESES-W                TO GS-MESES
           END-READ.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.



      *    MOVER DADOS DO BRINDE
           MOVE GS-SEQ            TO SEQ-CO43
           MOVE ZEROS             TO ITEM-CO43.
           START COD043 KEY IS NOT < CHAVE-CO43 INVALID KEY
                 MOVE "10" TO ST-COD043.
           PERFORM UNTIL ST-COD043 = "10"
             READ COD043 NEXT RECORD AT END MOVE "10" TO ST-COD043
               NOT AT END
                 IF SEQ-CO43 <> GS-SEQ
                    MOVE "10" TO ST-COD043
                 ELSE
                     MOVE SPACES       TO GS-LINDET
                     MOVE ITEM-CO43    TO GS-LINDET(1: 3)
                     MOVE BRINDE-CO43  TO CODIGO-CO02
                     READ COD002 INVALID KEY
                          MOVE SPACES TO NOME-CO02
                     END-READ
                     MOVE NOME-CO02    TO GS-LINDET(4: 14)
                     MOVE QTDE-BRINDE-CO43   TO GS-LINDET(19: 6)
                     IF MULT-FORM-CO02 = 1
                        MOVE GS-FORM   TO GS-LINDET(25: 5)
                     END-IF
                     MOVE CUSTO-PREVISTO-CO43 TO VALOR-E1 CUSTO-PREVISTO
                     MOVE VALOR-E1           TO GS-LINDET(30: 11)
                     IF MULT-FORM-CO02 = 2
                        COMPUTE CUSTO-W = CUSTO-PREVISTO-CO43 *
                                          QTDE-BRINDE-CO43
                     ELSE
                        COMPUTE CUSTO-W = (QTDE-BRINDE-CO43 *
                                GS-FORM) * CUSTO-PREVISTO-CO43
                     END-IF
                     MOVE CUSTO-W            TO VALOR-E1
                     MOVE VALOR-E1           TO GS-LINDET(41: 11)
                     MOVE DATA-PAGTO-CO43    TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV           TO DATA-E
                     MOVE DATA-E             TO GS-LINDET(52: 11)
                     IF DEB-CRED-CO43 = "D"
                        MOVE "D"             TO GS-LINDET(64: 2)
                     ELSE MOVE "C"           TO GS-LINDET(64: 2)
                     END-IF
                     MOVE DATA-PREV-VENDA-W TO GRDIAS-AAMMDD-FINAL
                     MOVE DATA-PAGTO-CO43   TO GRDIAS-AAMMDD-INICIAL
                     CALL "GRDIAS1" USING PARAMETROS-GRDIAS
                     MOVE GRDIAS-NUM-DIAS   TO GS-LINDET(73: 5)
                     PERFORM CALCULO-JUROS-BRINDE
                     ADD  JUROS-W                TO JUROS-PAGAR
                     MOVE JUROS-W            TO VALOR-E1
                     MOVE VALOR-E1           TO GS-LINDET(78: 11)
                     COMPUTE CUSTO-TOTAL = CUSTO-W + JUROS-W
                     MOVE CUSTO-TOTAL        TO VALOR-E1
                     MOVE VALOR-E1           TO GS-LINDET(89: 11)
                     MOVE MESES-W            TO MESES-E
                     MOVE MESES-E            TO GS-LINDET(100: 8)
                     IF DEB-CRED-CO43 = "D"
                       COMPUTE VLR-GERAL = (CUSTO-W * MESES-W) +
                               VLR-GERAL
                       ADD CUSTO-W           TO VLR-PAGAR
                     ELSE
                       COMPUTE VLR-GERAL1 = (CUSTO-W * MESES-W) +
                               VLR-GERAL1
                       ADD CUSTO-W           TO VLR-PAGAR1
                     END-IF

                     MOVE "INSERE-LIST" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-READ
           END-PERFORM.

           COMPUTE PRAZO-MEDIO ROUNDED = VLR-GERAL / VLR-PAGAR
           IF VLR-GERAL1 <> ZEROS AND VLR-PAGAR1 <> ZEROS
             COMPUTE PRAZO-MEDIO1 ROUNDED = VLR-GERAL1 / VLR-PAGAR1.
           MOVE 1 TO TAXA-ACUMULADA.
           COMPUTE TAXA-W = (GS-TAXA / 100) + 1.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA * TAXA-W
           END-PERFORM.
           MOVE 1 TO TAXA-ACUMULADA1.
           COMPUTE TAXA-W = (GS-TAXA / 100) + 1.

           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO1
               COMPUTE TAXA-ACUMULADA1 = TAXA-ACUMULADA1 * TAXA-W
           END-PERFORM.
           COMPUTE TAXA-JUROS-PM = (TAXA-ACUMULADA - 1) * 100
           COMPUTE TAXA-JUROS-PM1 = (TAXA-ACUMULADA1 - 1) * 100
           COMPUTE TAXA-JUROS = TAXA-ACUMULADA - 1
           COMPUTE TAXA-JUROS1 = TAXA-ACUMULADA1 - 1
      *    COMPUTE JUROS-PAGAR = VLR-PAGAR * TAXA-JUROS
           COMPUTE JUROS-PAGAR1 = VLR-PAGAR1 * TAXA-JUROS1
           COMPUTE TOTAL-E-JUROS = VLR-PAGAR + JUROS-PAGAR
           COMPUTE TOTAL-E-JUROS1 = VLR-PAGAR1 + JUROS-PAGAR1
           COMPUTE VLR-GERAL-CD = TOTAL-E-JUROS - TOTAL-E-JUROS1
           COMPUTE VLR-POR-FORM = VLR-GERAL-CD / GS-FORM.

      *    mover dados p/ janela-total
           MOVE ZEROS TO GS-CONT-LINTOT
           MOVE SPACES TO GS-LINTOT
           MOVE "Total Patrocinio/P-médio (debito).: " TO GS-LINTOT
           MOVE VLR-PAGAR            TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE PRAZO-MEDIO          TO PM-E
           MOVE PM-E                 TO GS-LINTOT(86: 20)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO GS-LINTOT

           MOVE "Total Patrocinio/P-médio (crédito): " TO GS-LINTOT
           MOVE VLR-PAGAR1           TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE PRAZO-MEDIO1         TO PM-E
           MOVE PM-E                 TO GS-LINTOT(86: 20)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "Tx.mensal/Jr.aplicado ao PM (DÉB).: " TO GS-LINTOT
           MOVE GS-TAXA              TO PERC-E
           MOVE PERC-E               TO GS-LINTOT(61: 6)
           MOVE "%"                  TO GS-LINTOT(67: 1)
           MOVE TAXA-JUROS-PM        TO TAXA-E
           MOVE TAXA-E               TO GS-LINTOT(85: 8)
           MOVE "%"                  TO GS-LINTOT(93: 1)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "Tx.mensal/Jr.aplicado ao PM (CRÉD): " TO GS-LINTOT
           MOVE GS-TAXA              TO PERC-E
           MOVE PERC-E               TO GS-LINTOT(61: 6)
           MOVE "%"                  TO GS-LINTOT(67: 1)
           MOVE TAXA-JUROS-PM1       TO TAXA-E
           MOVE TAXA-E               TO GS-LINTOT(85: 8)
           MOVE "%"                  TO GS-LINTOT(93: 1)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "Total Geral/Juros (Débito)........: " TO GS-LINTOT
           MOVE VLR-PAGAR            TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE JUROS-PAGAR          TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(80: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE "Total Geral/Juros (Crédito).......: " TO GS-LINTOT
           MOVE VLR-PAGAR1           TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE JUROS-PAGAR1         TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(80: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "Total Geral Capital + Juros (DÉB).: " TO GS-LINTOT
           MOVE TOTAL-E-JUROS        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE "Total Geral Capital + Juros (CRÉ).: " TO GS-LINTOT
           MOVE TOTAL-E-JUROS1       TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(55: 20)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "Total Geral ......................: " TO GS-LINTOT
           MOVE VLR-GERAL-CD         TO VALOR-E
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

           MOVE SPACES               TO GS-LINTOT(1:13)
           STRING "(" PERC-VENDA "% fotos): " INTO GS-LINTOT(1:13)
      *    MOVE "(70% fotos): "      TO GS-LINTOT(1: 13)
           MOVE PERC-VENDA           TO GS-LINTOT(2:2)
           MOVE "Preço Foto.: "      TO GS-LINTOT(40: 13)
           MOVE "Total......: "      TO GS-LINTOT(80: 13)
           COMPUTE QTDE-PERC70 = GS-QT-FOTOS * PERC-VENDA / 100
      *    0,7
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FOTO
           MOVE QTDE-PERC70          TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(14: 10)
           MOVE GS-PRECO-FOTO        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(53: 15)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FOTO
           MOVE VALOR-W              TO VALOR-E FATURAMENTO-W
           MOVE VALOR-E              TO GS-LINTOT(93: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE SPACES               TO GS-LINTOT(1:13)
           STRING "(" PERC-VENDA "% Estoj): " INTO GS-LINTOT(1:13)
      *    MOVE "(70% Estoj): "      TO GS-LINTOT(1: 13)
           MOVE PERC-VENDA           TO GS-LINTOT(2:2)
           MOVE "Preço Estoj: "      TO GS-LINTOT(40: 13)
           MOVE "Total......: "      TO GS-LINTOT(80: 13)
           COMPUTE QTDE-PERC70 = GS-FORM * PERC-VENDA / 100
      *    0,7
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-ALBUM
           MOVE QTDE-PERC70          TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(14: 10)
           MOVE GS-PRECO-ALBUM       TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(53: 15)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-ALBUM
           MOVE VALOR-W              TO VALOR-E
           ADD VALOR-W               TO FATURAMENTO-W
           MOVE VALOR-E              TO GS-LINTOT(93: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT

           MOVE SPACES               TO GS-LINTOT(1:13)
           STRING "(" PERC-VENDA "% DVD)..: " INTO GS-LINTOT(1:13)
      *    MOVE "(70% DVD)..: "      TO GS-LINTOT(1: 13)
           MOVE PERC-VENDA           TO GS-LINTOT(2:2)
           MOVE "Preço DVD..: "      TO GS-LINTOT(40: 13)
           MOVE "Total......: "      TO GS-LINTOT(80: 13)
           COMPUTE QTDE-PERC70 = GS-FORM * PERC-VENDA / 100
      *    0,7
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FITA
           MOVE QTDE-PERC70          TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(14: 10)
           MOVE GS-PRECO-FITA        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(53: 15)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FITA
           MOVE VALOR-W              TO VALOR-E
           ADD VALOR-W               TO FATURAMENTO-W
           MOVE VALOR-E              TO GS-LINTOT(93: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES               TO GS-LINTOT.
           MOVE "Faturamento Previsto. . ." TO GS-LINTOT
           MOVE FATURAMENTO-W        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(93: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CALCULO-JUROS-BRINDE SECTION.
           COMPUTE MESES-W = GRDIAS-NUM-DIAS / 30.
           COMPUTE PRAZO-MEDIO ROUNDED = (CUSTO-PREVISTO * MESES-W) /
                                  CUSTO-PREVISTO.
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-W * (TAXA-ACUMULADA - 1).


       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO VLR-PAGAR VLR-PAGAR1  CUSTO-W
                         CUSTO-PREVISTO VLR-GERAL VLR-GERAL1
                         JUROS-PAGAR JUROS-PAGAR1 VLR-GERAL-CD
                         TOTAL-E-JUROS TOTAL-E-JUROS1 VLR-POR-FORM.
      *--------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL.
           MOVE GS-SEQ               TO LINDET-REL(1: 7)
           MOVE GS-INSTITUICAO       TO LINDET-REL(8: 11)
           MOVE GS-CURSO             TO LINDET-REL(19: 21)
           MOVE GS-CIDADE            TO LINDET-REL(40: 13)
           MOVE GS-FORM              TO LINDET-REL(53: 5)
           MOVE GS-QT-FOTOS          TO LINDET-REL(58: 9)
           MOVE GS-PADRAO            TO LINDET-REL(67: 4)
           MOVE GS-MESANO            TO MESANO-E
           MOVE MESANO-E             TO LINDET-REL(71: 8)
           MOVE GS-MESES             TO LINDET-REL(79: 14)
           WRITE REG-RELAT FROM CAB04
           WRITE REG-RELAT FROM CAB03
           WRITE REG-RELAT FROM LINDET

           WRITE REG-RELAT FROM CAB05 AFTER 2
           WRITE REG-RELAT FROM CAB03
           ADD 6 TO LIN

           MOVE GS-SEQ   TO SEQ-CO42.

           READ COD042 INVALID KEY CONTINUE
             NOT INVALID KEY
               MOVE SPACES               TO LINDET-REL
               MOVE REPRESENTANTE-CO42   TO LINDET-REL(7: 43)
               MOVE ASSINATURA-CO42      TO DATA-E
               MOVE DATA-E               TO LINDET-REL(50: 14)
               MOVE VLR-COMISSAO-CO42    TO VALOR-E
               MOVE VALOR-E              TO LINDET-REL(66: 13)
               MOVE MESES-P-FORM           TO LINDET-REL(90: 8)
               WRITE REG-RELAT FROM LINDET
               ADD 1 TO LIN

           END-READ.

      *    MOVER DADOS DO BRINDE
           MOVE SEQ-CO42          TO SEQ-CO43
           MOVE ZEROS             TO ITEM-CO43.
           START COD043 KEY IS NOT < CHAVE-CO43 INVALID KEY
                 MOVE "10" TO ST-COD043.
           PERFORM UNTIL ST-COD043 = "10"
             READ COD043 NEXT RECORD AT END MOVE "10" TO ST-COD043
               NOT AT END
                 IF SEQ-CO43 <> GS-SEQ
                    MOVE "10" TO ST-COD043
                 ELSE
                     MOVE ITEM-CO43         TO LINDET-REL(1: 6)
                     MOVE BRINDE-CO43       TO CODIGO-CO02
                     READ COD002 INVALID KEY MOVE ZEROS TO VALOR-CO02
                     END-READ
                     MOVE NOME-CO02         TO LINDET-REL(7: 43)
                     MOVE DATA-PAGTO-CO43   TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV          TO DATA-E
                     MOVE DATA-E            TO LINDET-REL(50: 14)
                     IF MULT-FORM-CO02 = 1
                        COMPUTE CUSTO-PREVISTO =
                          (GS-FORM * QTDE-BRINDE-CO43) *
                          CUSTO-PREVISTO-CO43
                     ELSE COMPUTE CUSTO-PREVISTO = QTDE-BRINDE-CO43 *
                                                   CUSTO-PREVISTO-CO43
                     END-IF
                     MOVE CUSTO-PREVISTO    TO VALOR-E
                     MOVE VALOR-E           TO LINDET-REL(66: 14)
                     IF DEB-CRED-CO43 = "D"
                        MOVE "D"            TO LINDET-REL(81: 2)
                     ELSE MOVE "C"          TO LINDET-REL(81: 2)
                     END-IF

                     MOVE DATA-PREV-VENDA-W TO GRDIAS-AAMMDD-FINAL
                     MOVE DATA-PAGTO-CO43   TO GRDIAS-AAMMDD-INICIAL
                     CALL "GRDIAS1" USING PARAMETROS-GRDIAS
                     COMPUTE MESES-W = GRDIAS-NUM-DIAS / 30
                     MOVE MESES-W            TO MESES-E
                     MOVE MESES-E            TO LINDET-REL(90: 6)
                     WRITE REG-RELAT FROM LINDET
                     ADD 1 TO LIN
                     IF LIN > 60 PERFORM CABECALHO
                  END-IF
              END-READ
           END-PERFORM.

      *    TOTALIZAR IMPRESSORA
           ADD 3 TO LIN
           IF LIN > 60 PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE "Total Patrocinio/P-médio (debito).: " TO GS-LINDET
           MOVE VLR-PAGAR            TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           MOVE PRAZO-MEDIO          TO PM-E
           MOVE PM-E                 TO LINDET-REL(86: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES TO LINDET-REL

           MOVE "Total Patrocinio/P-médio (crédito): " TO LINDET-REL
           MOVE VLR-PAGAR1           TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           MOVE PRAZO-MEDIO1         TO PM-E
           MOVE PM-E                 TO LINDET-REL(86: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL
           WRITE REG-RELAT FROM LINDET

           ADD 3 TO LIN
           IF LIN > 60 PERFORM CABECALHO.
           MOVE "Tx.mensal/Jr.aplicado ao PM (DÉB).: " TO LINDET-REL
           MOVE GS-TAXA              TO PERC-E
           MOVE PERC-E               TO LINDET-REL(61: 6)
           MOVE "%"                  TO LINDET-REL(67: 1)
           MOVE TAXA-JUROS-PM        TO TAXA-E
           MOVE TAXA-E               TO LINDET-REL(85: 8)
           MOVE "%"                  TO LINDET-REL(93: 1)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES TO LINDET-REL

           MOVE "Tx.mensal/Jr.aplicado ao PM (CRÉD): " TO LINDET-REL
           MOVE GS-TAXA              TO PERC-E
           MOVE PERC-E               TO LINDET-REL(61: 6)
           MOVE "%"                  TO LINDET-REL(67: 1)
           MOVE TAXA-JUROS-PM1       TO TAXA-E
           MOVE TAXA-E               TO LINDET-REL(85: 8)
           MOVE "%"                  TO LINDET-REL(93: 1)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL
           WRITE REG-RELAT FROM LINDET

           ADD 3 TO LIN
           IF LIN > 60 PERFORM CABECALHO.
           MOVE "Total Geral/Juros (Débito)........: " TO LINDET-REL
           MOVE VLR-PAGAR            TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           MOVE JUROS-PAGAR          TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(80: 13)
           WRITE REG-RELAT FROM LINDET.
           MOVE SPACES               TO LINDET-REL

           MOVE "Total Geral/Juros (Crédito).......: " TO LINDET-REL
           MOVE VLR-PAGAR1           TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           MOVE JUROS-PAGAR1         TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(80: 13)
           MOVE "INSERE-LINTOT"      TO DS-PROCEDURE
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL
           WRITE REG-RELAT FROM LINDET.

           ADD 3 TO LIN
           IF LIN > 60 PERFORM CABECALHO.
           MOVE "Total Geral Capital + Juros (DÉB).: " TO LINDET-REL
           MOVE TOTAL-E-JUROS        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL

           MOVE "Total Geral Capital + Juros (CRÉ).: " TO LINDET-REL
           MOVE TOTAL-E-JUROS1       TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL
           WRITE REG-RELAT FROM LINDET

           ADD 3 TO LIN
           IF LIN > 60 PERFORM CABECALHO.
           MOVE "Total Geral ......................: " TO LINDET-REL
           MOVE VLR-GERAL-CD         TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL

           MOVE "Divisão por formando..............: " TO LINDET-REL
           MOVE VLR-POR-FORM         TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(55: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL
           WRITE REG-RELAT FROM LINDET

           ADD 4 TO LIN
           IF LIN > 60 PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL(1:13)
           STRING "(" PERC-VENDA "% fotos): " INTO LINDET-REL(1:13)
      *    MOVE "(70% fotos): "      TO LINDET-REL(1: 13)
           MOVE PERC-VENDA           TO GS-LINTOT(2:2)
           MOVE "Preço Foto.: "      TO LINDET-REL(40: 13)
           MOVE "Total......: "      TO LINDET-REL(80: 13)
           COMPUTE QTDE-PERC70 = GS-QT-FOTOS * PERC-VENDA / 100
      *    0,7
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FOTO
           MOVE QTDE-PERC70          TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(14: 10)
           MOVE GS-PRECO-FOTO        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(53: 15)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FOTO
           MOVE VALOR-W              TO VALOR-E FATURAMENTO-W
           MOVE VALOR-E              TO LINDET-REL(93: 13)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL

           MOVE SPACES               TO LINDET-REL(1:13)
           STRING "(" PERC-VENDA "% Estoj): " INTO LINDET-REL(1:13)
      *    MOVE "(70% Estoj): "      TO LINDET-REL(1: 13)
           MOVE PERC-VENDA           TO GS-LINTOT(2:2)
           MOVE "Preço Estoj: "      TO LINDET-REL(40: 13)
           MOVE "Total......: "      TO LINDET-REL(80: 13)
           COMPUTE QTDE-PERC70 = GS-FORM * PERC-VENDA / 100
      *    0,7
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-ALBUM
           MOVE QTDE-PERC70          TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(14: 10)
           MOVE GS-PRECO-ALBUM       TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(53: 15)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-ALBUM
           MOVE VALOR-W              TO VALOR-E
           ADD VALOR-W               TO FATURAMENTO-W
           MOVE VALOR-E              TO LINDET-REL(93: 13)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL

           MOVE SPACES               TO LINDET-REL(1:13)
           STRING "(" PERC-VENDA "% DVD)..: " INTO LINDET-REL(1:13)
      *    MOVE "(70% DVD)..: "      TO LINDET-REL(1: 13)
           MOVE PERC-VENDA           TO GS-LINTOT(2:2)
           MOVE "Preço DVD..: "      TO LINDET-REL(40: 13)
           MOVE "Total......: "      TO LINDET-REL(80: 13)
           COMPUTE QTDE-PERC70 = GS-FORM * PERC-VENDA / 100
      *    0,7
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FITA
           MOVE QTDE-PERC70          TO QTDE-E
           MOVE QTDE-E               TO LINDET-REL(14: 10)
           MOVE GS-PRECO-FITA        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(53: 15)
           COMPUTE VALOR-W = QTDE-PERC70 * GS-PRECO-FITA
           MOVE VALOR-W              TO VALOR-E
           ADD VALOR-W               TO FATURAMENTO-W
           MOVE VALOR-E              TO LINDET-REL(93: 13)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES               TO LINDET-REL
           WRITE REG-RELAT FROM LINDET

           MOVE "Faturamento Previsto. . ." TO LINDET-REL
           MOVE FATURAMENTO-W        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(93: 13)
           WRITE REG-RELAT FROM LINDET.

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
           MOVE "COP043" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD042 COD043 COD002.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
