       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP100.
      *DATA: 09/05/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: MOVIMENTO DE REPORTAGEM
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY COPX003.
           COPY COPX040.
           COPY COPX060.
           COPY CGPX001.
           COPY REPX002.
           COPY REPX100.
           COPY REPX101.
           COPY REPX103.
           COPY PRPX100.
           COPY PRPX101.
           COPY PRPX105.
           COPY PARX001.
           COPY LOGX001.
           COPY LOGX002.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY COPW003.
       COPY COPW040.
       COPY COPW060.
       COPY CGPW001.
       COPY REPW002.
       COPY REPW100.
       COPY REPW101.
       COPY REPW103.
       COPY PRPW100.
       COPY PRPW101.
       COPY PRPW105.
       COPY PARW001.
       COPY LOGW001.
       COPY LOGW002.

       FD  RELAT.
       01  REG-RELAT.
           05  FILLER              PIC X(100).
       WORKING-STORAGE SECTION.
           COPY "REP100.CPB".
           COPY "REP100.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-RED002             PIC XX       VALUE SPACES.
           05  ST-PRD100             PIC XX       VALUE SPACES.
           05  ST-PRD101             PIC XX       VALUE SPACES.
           05  ST-PRD105             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-RED103             PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
      *    GRAVAR-W = 0 JÁ ESTA GRAVADO   GRAVAR-W = 1 NÃO
           05  NR-PLAN-W             PIC 9(14)    VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  SEQ-EQ-W              PIC 9(3)     VALUE ZEROS.
           05  CODIGO-ANT            PIC 9(6)     VALUE ZEROS.
           05  FUNCAO-ANT            PIC 9(2)     VALUE ZEROS.
           05  ULT-DOCTO             PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ BLANK WHEN ZEROS.
           05  QTDE-E                PIC ZZ,ZZ    BLANK WHEN ZEROS.
           05  QTDE-E1               PIC Z,Z      BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
           05  WS-STATUS-REVENDIDO   PIC 9(02)    VALUE ZEROS.
           05  WS-STATUS-ANALISE     PIC 9(02)    VALUE ZEROS.
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
           05  FILLER              PIC X(41)   VALUE
           "RELATORIO DE REPORTAGEM            ".
           05  FILLER              PIC X(31)   VALUE SPACES.
       01  TRACO.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE
           "CONT DESCRICAO-CONTRATO   EVE   DESCRICAO-EVENTO     NR-PART
      -    "".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "SEQ CODIGO-DESCRICAO   FU-DESCR FIT-FIL-KM QT-PRO TIPO
      -    " Q.REP         VALOR".
       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU             PIC 9(04).
             10 WS-MES-CPU             PIC 9(02).
             10 WS-DIA-CPU             PIC 9(02).
          05 FILLER                    PIC X(13).

       01 WS-HORA-SYS.
          05 WS-HO-SYS                 PIC 9(02).
          05 WS-MI-SYS                 PIC 9(02).
          05 WS-SE-SYS                 PIC 9(02).
          05 WS-MS-SYS                 PIC 9(02).

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
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "RED002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED002.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           MOVE "RED103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED103.
           MOVE "PRD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD100.
           MOVE "PRD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD101.
           MOVE "PRD105"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD105.
           MOVE "PAR001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "LOG001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOG002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOG002.

           OPEN INPUT CAD010 COD003 COD040 COD060 CGD001 RED002 PRD100
                      PRD101 PRD105 PAR001

           OPEN I-O RED100 RED101 RED103.

           IF ST-RED100 = "35" CLOSE RED100  OPEN OUTPUT RED100
                               CLOSE RED100  OPEN I-O RED100.
           IF ST-RED101 = "35" CLOSE RED101  OPEN OUTPUT RED101
                               CLOSE RED101  OPEN I-O RED101.
           IF ST-RED103 = "35" CLOSE RED103  OPEN OUTPUT RED103
                               CLOSE RED103  OPEN I-O RED103.

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
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-PRD100 <> "00"
              MOVE "ERRO ABERTURA PRD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD101 <> "00"
              MOVE "ERRO ABERTURA PRD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD105 <> "00"
              MOVE "ERRO ABERTURA PRD105: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD105 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE 1 TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                MOVE "Parametrização do Brinde Não Cadastrada"
                TO GS-MENSAGEM-ERRO
                PERFORM CARREGA-MENSAGEM-ERRO
           NOT INVALID KEY
                IF STATUS-REVENDIDO-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-REVENDIDO-PAR001
                END-IF
                MOVE STATUS-REVENDIDO-PAR001 TO WS-STATUS-REVENDIDO
                IF STATUS-ANALISE-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-ANALISE-PAR001
                END-IF
                MOVE STATUS-ANALISE-PAR001   TO WS-STATUS-ANALISE
           END-READ

           CLOSE PAR001


           CLOSE      RED100 RED101 RED103
           OPEN INPUT RED100 RED101 RED103

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-VERIFICA-CODIGO-TRUE
                    PERFORM VERIFICA-CODIGO
               WHEN GS-VERIF-ULT-DOCTO-TRUE
                    PERFORM VERIFICA-ULTIMO-DOCTO
               WHEN GS-VERIFICA-DOCTO-TRUE
                    PERFORM VERIFICA-DOCTO
               WHEN GS-SAVE-REPORT-TRUE
                    PERFORM GRAVAR-DADOS-REPORTAGEM
               WHEN GS-PROXIMO-TRUE
                    PERFORM LER-PROXIMO
               WHEN GS-SAVE-EVENTO-TRUE
                    PERFORM GRAVAR-DADOS-EVENTO
               WHEN GS-SAVE-EQUIPE-TRUE
                    PERFORM GRAVAR-DADOS-EQUIPE
               WHEN GS-EXCLUI-REPORTAGEM-TRUE
                    PERFORM EXCLUI-REPORTAGEM
               WHEN GS-EXCLUI-EQUIPE-TRUE
                    PERFORM EXCLUI-EQUIPE
               WHEN GS-EXCLUI-EVENTO-TRUE
                    PERFORM EXCLUI-EVENTO
               WHEN GS-SELECAO-EQUIPE-TRUE
                    PERFORM SELECAO-EQUIPE
               WHEN GS-SELECAO-EVENTO-TRUE
                    PERFORM SELECAO-EVENTO
               WHEN GS-LE-EQUIPE-TRUE
                    PERFORM LER-EQUIPE
               WHEN GS-LE-EVENTO-TRUE
                    PERFORM LER-EVENTO
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LER-CONTRATO
               WHEN GS-LE-FUNCAO-TRUE
                    PERFORM LER-FUNCAO
               WHEN GS-POPUP-GERAL-TRUE
                    PERFORM CARREGAR-POPUP
               WHEN GS-NOVA-REPORT-TRUE
                    PERFORM NOVA-REPORTAGEM
           END-EVALUATE.

           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-PROXIMO SECTION.
           INITIALIZE REG-RED103 GS-SEQ-EQ
           MOVE GS-DOCTO           TO DOCTO-R103
           MOVE ALL "9"            TO SEQ-R103
           START RED103 KEY IS LESS THAN ALT1-R103 NOT INVALID KEY
                 READ RED103 PREVIOUS NOT AT END
                      IF GS-DOCTO = DOCTO-R103
                         MOVE SEQ-R103 TO GS-SEQ-EQ.

           ADD 1 TO GS-SEQ-EQ.

      *----------------------------------------------------
       LER-EQUIPE SECTION.
           MOVE GS-CODIGO-EQ   TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01   TO GS-DESC-EQ.
       LER-FUNCAO SECTION.
           MOVE GS-FUNCAO-EQ   TO CODIGO-RE02.
           READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02.
           MOVE DESCRICAO-RE02 TO GS-DESC-FUNCAO-EQ.
       LER-CONTRATO SECTION.
           MOVE GS-CONTRATO    TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40  TO GS-DESC-CONTRATO.
       LER-EVENTO SECTION.
           MOVE GS-EVENTO      TO CODIGO-CO03.
           READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03.
           MOVE NOME-CO03      TO GS-DESC-EVENTO.
       CARREGAR-POPUP SECTION.
           EVALUATE GS-TIPO-POPUP
             WHEN 1 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(52: 4)  TO GS-CONTRATO
                    MOVE PASSAR-STRING-1(22: 11) TO GS-DESC-CONTRATO
             WHEN 2 CALL   "COP003T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP003T"
                    MOVE PASSAR-STRING-1(33: 5) TO GS-EVENTO
                    MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-EVENTO
             WHEN 3 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(33: 6) TO GS-CODIGO-EQ
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-EQ
             WHEN 4 CALL   "REP002T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "REP002T"
                    MOVE PASSAR-STRING-1(22: 2) TO GS-FUNCAO-EQ
                    MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-FUNCAO-EQ
           END-EVALUATE.

      *----------------------------------------------------
       NOVA-REPORTAGEM SECTION.
           MOVE ULT-DOCTO    TO GS-DOCTO.
       VERIFICA-CODIGO SECTION.
      *    BUSCA DADOS APARTIR DO PLANEJAMENTO

           MOVE "Gerando arquivo"  TO GS-MENSAGEM
           MOVE "EXIBE-MENSAGEM"    TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE GS-NR-PLAN         TO NR-PLAN-W(1: 8)
           MOVE GS-ANO-PLAN        TO NR-PLAN-W(9: 4)
           MOVE GS-SEQ-PLAN        TO NR-PLAN-W(13: 2)
           MOVE "CLEAR-OBJECT-PRINCIPAL" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE NR-PLAN-W(1: 8)    TO GS-NR-PLAN
           MOVE NR-PLAN-W(9: 4)    TO GS-ANO-PLAN
           MOVE NR-PLAN-W(13: 2)   TO GS-SEQ-PLAN

      *    DADOS PLANEJAMENTO - EQUIPE
           MOVE ZEROS              TO CODIGO-PR100 FUNCAO-PR100.
           MOVE NR-PLAN-W          TO NR-PLAN-PR100.
           START PRD100 KEY IS NOT < ALT-PR100 INVALID KEY
                 MOVE "10" TO ST-PRD100.
           MOVE SPACES TO GS-LINDET1.
           MOVE ZEROS TO CODIGO-ANT FUNCAO-ANT GS-CONT-EQ SEQ-EQ-W.
           PERFORM UNTIL ST-PRD100 = "10"
            READ PRD100 NEXT RECORD AT END MOVE "10" TO ST-PRD100
              NOT AT END
                IF NR-PLAN-PR100 <> NR-PLAN-W MOVE "10" TO ST-PRD100
                ELSE
                  IF CODIGO-PR100 <> CODIGO-ANT OR
                     FUNCAO-PR100 <> FUNCAO-ANT
                     MOVE CODIGO-PR100   TO CODIGO-ANT
                     MOVE FUNCAO-PR100   TO FUNCAO-ANT
                     ADD 1               TO SEQ-EQ-W
                     MOVE SEQ-EQ-W       TO GS-LINDET1(1: 4)
                     MOVE CODIGO-PR100   TO GS-LINDET1(5: 7)
                                            CODIGO-CG01
                     READ CGD001 INVALID KEY
                          MOVE SPACE     TO NOME-CG01
                     END-READ
                     MOVE NOME-CG01      TO GS-LINDET1(12: 31)
                     MOVE FUNCAO-PR100   TO GS-LINDET1(43: 3)
                                            CODIGO-RE02
                     MOVE "-"            TO GS-LINDET1(46: 1)
                     READ RED002 INVALID KEY
                           MOVE SPACES   TO DESCRICAO-RE02
                     END-READ
                     MOVE DESCRICAO-RE02 TO GS-LINDET1(47: 11)
                     EVALUATE FUNCAO-PR100
                       WHEN 1     MOVE 1 TO GS-LINDET1(59: 13)
                       WHEN 2     MOVE 2 TO GS-LINDET1(59: 13)
                       WHEN 12    MOVE 3 TO GS-LINDET1(59: 13)
                       WHEN OTHER MOVE 0 TO GS-LINDET1(59: 13)
                     END-EVALUATE
                     MOVE 1             TO GS-LINDET1(72: 3)

                     MOVE "INSERE-LIST-EQUIPE" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
                END-IF
            END-READ
           END-PERFORM.

      *    DADOS PLANEJAMENTO - EVENTO
           MOVE NR-PLAN-W    TO NR-PLAN-PR101.
           MOVE ZEROS        TO SEQ-EVE-PR101.
           MOVE ZEROS        TO GS-CONT-EV.
           START PRD101 KEY IS NOT < CHAVE-PR101 INVALID KEY
                 MOVE "10"   TO ST-PRD101.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-PRD101 = "10"
             READ PRD101 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD101
             NOT AT END
                 IF NR-PLAN-PR101 <> NR-PLAN-W
                    MOVE "10"                 TO ST-PRD101
                 ELSE
                    MOVE CONTRATO-PR101       TO GS-LINDET(1: 5)
                                                 NR-CONTRATO-CO40
                                                 NR-CONTRATO-CO60
                    READ COD040 INVALID KEY
                         INITIALIZE REG-COD040
                    END-READ
                    MOVE IDENTIFICACAO-CO40   TO GS-LINDET(06: 31)
                    MOVE ITEM-PR101           TO ITEM-CO60
                    READ COD060 INVALID KEY
                         INITIALIZE REG-COD060
                    END-READ
                    MOVE CODEVENTO-CO60       TO CODIGO-CO03
                                                 GS-LINDET(37: 5)
                    READ COD003 INVALID KEY
                         MOVE SPACES          TO NOME-CO03
                    END-READ
                    MOVE NOME-CO03            TO GS-LINDET(43: 25)

                    IF STATUS-CO40 <> WS-STATUS-REVENDIDO AND
                       WS-STATUS-ANALISE
                       MOVE "INSERE-LIST-EVENTO" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                    END-IF
                 END-IF
             END-READ
           END-PERFORM.

           MOVE NR-PLAN-W          TO NR-PLAN-PR105.
           READ PRD105 INVALID KEY INITIALIZE REG-PRD105.
           MOVE PREV-VEIC-PR105    TO GS-VLR-COMBUSTIVEL
           MOVE PREV-HOSP-PR105    TO GS-VLR-HOSPEDAGEM
           MOVE PREV-REFEIC-PR105  TO GS-VLR-REFEICAO
           MOVE PREV-OUTROS-PR105  TO GS-VLR-OUTROS
           MOVE QT-VEICULO-PR105   TO GS-QT-VEICULO
           MOVE QT-DIAS-PR105      TO GS-QT-DIAS

           MOVE GS-NR-PLAN(1: 4)   TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID           TO GS-CIDADE
           MOVE GS-NR-PLAN(5: 4)   TO GRTIME-DATE(5: 4)
           MOVE GS-ANO-PLAN        TO GRTIME-DATE(1: 4).
           PERFORM VERIFICA-DATA.
           MOVE "UNSHOW-MENSAGEM"  TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM VERIFICA-ULTIMO-DOCTO.

       VERIFICA-ULTIMO-DOCTO SECTION.
      *    ACHAR NR-DOCTO
           MOVE ZEROS TO ULT-DOCTO
           MOVE all "9"  TO DOCTO-R100.
           START RED100 KEY IS LESS THAN DOCTO-R100 INVALID KEY
                 MOVE "10" TO ST-RED100.
           PERFORM UNTIL ST-RED100 = "10"
             READ RED100 NEXT RECORD AT END
                  MOVE "10" TO ST-RED100
             NOT AT END
                 MOVE DOCTO-R100  TO ULT-DOCTO
                  MOVE "10" TO ST-RED100
             END-READ
           END-PERFORM.
           ADD 1            TO ULT-DOCTO
           MOVE ULT-DOCTO   TO GS-DOCTO
           MOVE ZEROS       TO GS-LCTO-CTA-CORR.
       VERIFICA-DATA SECTION.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 2         TO GRTIME-TYPE.
           MOVE 8         TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           IF GRTIME-WEEK-NUM <> 2
              MOVE "ERRO-DATA-SEMANA" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
             PERFORM VERIFICA-DIA-SEMANA.
      *      verifica se o planejamento já está cadastrado
       VERIFICA-DIA-SEMANA SECTION.
      *    memoriza os 7 dias da semana(iniciando pela segunda)
           MOVE GRTIME-DATE   TO DATA-INI DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV      TO GS-DATA-INI

           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL   TO DATA-FIM DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV      TO GS-DATA-FIM.
       CHAMA-GRTIME-ADAY SECTION.
      *    VERIFICA ULTIMO DIA DA SEMANA SOLICITADA
           MOVE 2         TO GRTIME-TYPE
           MOVE 1         TO GRTIME-FUNCTION
           MOVE 6         TO GRTIME-DAYS
           CALL "GRTIME"  USING PARAMETROS-GRTIME
           CANCEL "GRTIME".
      *----------------------------------------------------------
       VERIFICA-DOCTO SECTION.
           MOVE GS-DOCTO      TO DOCTO-R100.
      *    MOVE "CLEAR-OBJECT-PRINCIPAL" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.
           MOVE DOCTO-R100    TO GS-DOCTO.
           READ RED100 INVALID KEY MOVE 1 TO GRAVA-W
                       MOVE "HABILITA-EVE-EQUI" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
      *                MOVE "DESABILITA-EVE-EQUI" TO DS-PROCEDURE
      *                PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY PERFORM MOVER-DADOS-REPORTAGEM
                       MOVE "HABILITA-EVE-EQUI" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       MOVE 0 TO GRAVA-W.

       MOVER-DADOS-REPORTAGEM SECTION.
           MOVE DATA-MOV-R100              TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                   TO GS-DATA-REPORT
           MOVE LCTO-CTA-CORR-R100         TO GS-LCTO-CTA-CORR
           MOVE ANOMES-R100                TO MESANO-I
           MOVE MESANO-I(5: 2)             TO MESANO-W(1: 2)
           MOVE MESANO-I(1: 4)             TO MESANO-W(3: 4)
           MOVE MESANO-W                   TO GS-MESANO
           MOVE QTDE-PESSOAS-R100          TO GS-QT-PESSOAS
           MOVE QTDE-VEICULOS-R100         TO GS-QT-VEICULO
           MOVE QTDE-DIAS-R100             TO GS-QT-DIAS
           MOVE QTDE-FORM-R100             TO GS-QT-FORM
           MOVE VLR-COMB-R100              TO GS-VLR-COMBUSTIVEL
           MOVE VLR-HOSP-R100              TO GS-VLR-HOSPEDAGEM
           MOVE VLR-REFEICAO-R100          TO GS-VLR-REFEICAO
           MOVE VLR-PASSAGEM-R100          TO GS-VLR-PASSAGEM
           MOVE VLR-ALUGUEL-R100           TO GS-VLR-ALUGUEL
           MOVE VLR-MAT-R100               TO GS-VLR-MATERIAL
           MOVE VLR-OUTROS-R100            TO GS-VLR-OUTROS
           MOVE VLR-TOT-REPORT-R100        TO GS-VLR-TOT-REPORT
           MOVE VLR-DESPESA-REPORT-R100    TO GS-VLR-DESPESA-REPORT
           MOVE TOT-FILME-REPORT-R100      TO GS-TOT-FILME-REPORT
           MOVE TOT-FITA-REPORT-R100       TO GS-TOT-FITA-REPORT.
           PERFORM INSERE-LISTA-EVENTO.
           PERFORM INSERE-LISTA-EQUIPE.

       INSERE-LISTA-EVENTO SECTION.
      *    DADOS REPORTAGEM - EVENTO

           MOVE "CLEAR-LIST-BOX-EVENTO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO GS-CONT-EV.
           MOVE GS-DOCTO       TO DOCTO-R101.
           MOVE ZEROS          TO CONTRATO-R101 EVENTO-R101.
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
                 READ RED101 NEXT RECORD AT END
                      MOVE "10" TO ST-RED101
                 NOT AT END
                      IF DOCTO-R101 <> GS-DOCTO
                         MOVE "10" TO ST-RED101
                      ELSE
                         PERFORM MOVER-LISTA-RED101-EVENTO
                         IF STATUS-CO40 <> WS-STATUS-REVENDIDO AND
                            WS-STATUS-ANALISE
                            MOVE "INSERE-LIST-EVENTO" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
       MOVER-LISTA-RED101-EVENTO SECTION.
           MOVE CONTRATO-R101     TO GS-LINDET(1: 5)
                                     NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                INITIALIZE REG-COD040
           END-READ
           MOVE IDENTIFICACAO-CO40 TO GS-LINDET(06: 31)
           MOVE EVENTO-R101        TO CODIGO-CO03
                                      GS-LINDET(37: 5)
           READ COD003 INVALID KEY
                MOVE SPACES        TO NOME-CO03
           END-READ
           MOVE NOME-CO03          TO GS-LINDET(43: 25)
           MOVE QT-PARTIC-R101     TO GS-LINDET(68: 5).

       INSERE-LISTA-EQUIPE SECTION.
      *    DADOS REPORTAGEM - EQUIPE
           MOVE "CLEAR-LIST-BOX-EQUIPE" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO GS-CONT-EQ.

           MOVE ZEROS              TO SEQ-R103.
           MOVE GS-DOCTO           TO DOCTO-R103.
           START RED103 KEY IS NOT < ALT1-R103 INVALID KEY
                 MOVE "10" TO ST-RED103.

           PERFORM UNTIL ST-RED103 = "10"
            READ RED103 NEXT RECORD AT END
                 MOVE "10" TO ST-RED103
            NOT AT END
                 IF DOCTO-R103 <> GS-DOCTO
                    MOVE "10" TO ST-RED103
                 ELSE
                    PERFORM MOVER-LISTA-RED103-EQUIPE
                    MOVE "INSERE-LIST-EQUIPE" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
            END-READ
           END-PERFORM.
       MOVER-LISTA-RED103-EQUIPE SECTION.
           MOVE SEQ-R103         TO GS-LINDET1(1: 4)
           MOVE CODIGO-R103      TO GS-LINDET1(5: 7) CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACE       TO NOME-CG01
           END-READ
           MOVE NOME-CG01        TO GS-LINDET1(12: 31)
           MOVE FUNCAO-R103      TO GS-LINDET1(43: 3) CODIGO-RE02
           MOVE "-"              TO GS-LINDET1(46: 1)
           READ RED002 INVALID KEY
                MOVE SPACES      TO DESCRICAO-RE02
           END-READ
           MOVE DESCRICAO-RE02   TO GS-LINDET1(47: 11)
           MOVE FITA-FILME-R103  TO GS-LINDET1(59: 3)
           MOVE QT-FILMES-R103   TO GS-LINDET1(62: 10)
           MOVE TIPO-REPORT-R103 TO GS-LINDET1(72: 3)
           MOVE QT-REPORT-R103   TO QTDE-E1
           MOVE QTDE-E1          TO GS-LINDET1(75: 8)
           MOVE VLR-REPORT-R103  TO VALOR-E
           MOVE VALOR-E          TO GS-LINDET1(83: 13).

       GRAVAR-DADOS-REPORTAGEM SECTION.
           CLOSE    RED100
           OPEN I-O RED100
           MOVE GS-DOCTO              TO DOCTO-R100.
           READ RED100 INVALID KEY
                MOVE 1                TO GRAVA-W
           NOT INVALID KEY
                MOVE 0                TO GRAVA-W.
           MOVE GS-DATA-REPORT        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATA-MOV-R100
           MOVE GS-LCTO-CTA-CORR      TO LCTO-CTA-CORR-R100
           MOVE GS-MESANO             TO MESANO-W
           MOVE MESANO-W(1: 2)        TO MESANO-I(5: 2)
           MOVE MESANO-W(3: 4)        TO MESANO-I(1: 4)
           MOVE MESANO-I              TO ANOMES-R100
           MOVE GS-QT-PESSOAS         TO QTDE-PESSOAS-R100
           MOVE GS-QT-VEICULO         TO QTDE-VEICULOS-R100
           MOVE GS-QT-DIAS            TO QTDE-DIAS-R100
           MOVE GS-QT-FORM            TO QTDE-FORM-R100
           MOVE GS-VLR-COMBUSTIVEL    TO VLR-COMB-R100.
           MOVE GS-VLR-HOSPEDAGEM     TO VLR-HOSP-R100
           MOVE GS-VLR-REFEICAO       TO VLR-REFEICAO-R100
           MOVE GS-VLR-PASSAGEM       TO VLR-PASSAGEM-R100
           MOVE GS-VLR-ALUGUEL        TO VLR-ALUGUEL-R100
           MOVE GS-VLR-MATERIAL       TO VLR-MAT-R100
           MOVE GS-VLR-OUTROS         TO VLR-OUTROS-R100
           MOVE GS-VLR-TOT-REPORT     TO VLR-TOT-REPORT-R100
           MOVE GS-VLR-DESPESA-REPORT TO VLR-DESPESA-REPORT-R100
           MOVE GS-TOT-FILME-REPORT   TO TOT-FILME-REPORT-R100
           MOVE GS-TOT-FITA-REPORT    TO TOT-FITA-REPORT-R100
           IF GRAVA-W = 1
              WRITE REG-RED100 INVALID KEY
                MOVE "ERRO GRAVACAO RED100: "  TO GS-MENSAGEM-ERRO
                MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
                PERFORM CARREGA-MENSAGEM-ERRO
              NOT INVALID KEY
                OPEN I-O LOG002
                MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                ACCEPT WS-HORA-SYS FROM TIME
                MOVE USUARIO-W TO LOG2-USUARIO
                MOVE WS-DATA-CPU TO LOG2-DATA
                MOVE WS-HORA-SYS TO LOG2-HORAS
                MOVE "I"         TO LOG2-OPERACAO
                MOVE "RED100"    TO LOG2-ARQUIVO
                MOVE "REP100"    TO LOG2-PROGRAMA
                MOVE REG-RED100  TO LOG2-REGISTRO
                WRITE REG-LOG002 INVALID KEY
                     MOVE "Erro de Gravação...LOG002" TO
                     MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                END-WRITE
                CLOSE LOG002
              END-WRITE
              ADD 1 TO ULT-DOCTO
           ELSE
              REWRITE REG-RED100 INVALID KEY
                MOVE "ERRO REGRAVACAO RED100: "  TO GS-MENSAGEM-ERRO
                MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
                PERFORM CARREGA-MENSAGEM-ERRO
              NOT INVALID KEY
                OPEN I-O LOG002
                MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                ACCEPT WS-HORA-SYS FROM TIME
                MOVE USUARIO-W TO LOG2-USUARIO
                MOVE WS-DATA-CPU TO LOG2-DATA
                MOVE WS-HORA-SYS TO LOG2-HORAS
                MOVE "A"         TO LOG2-OPERACAO
                MOVE "RED100"    TO LOG2-ARQUIVO
                MOVE "REP100"    TO LOG2-PROGRAMA
                MOVE REG-RED100  TO LOG2-REGISTRO
                WRITE REG-LOG002 INVALID KEY
                     MOVE "Erro de Gravação...LOG002" TO
                     MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                END-WRITE
                CLOSE LOG002
              END-REWRITE.
           CLOSE      RED100
           OPEN INPUT RED100.

       EXCLUI-REPORTAGEM SECTION.
           CLOSE    RED100 RED101 RED103
           OPEN I-O RED100 RED101 RED103
           MOVE GS-DOCTO             TO DOCTO-R100.
           READ RED100 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE RED100 NOT INVALID KEY
                       OPEN I-O LOG002
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE USUARIO-W TO LOG2-USUARIO
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "E"         TO LOG2-OPERACAO
                       MOVE "RED100"    TO LOG2-ARQUIVO
                       MOVE "REP100"    TO LOG2-PROGRAMA
                       MOVE REG-RED100  TO LOG2-REGISTRO
                       WRITE REG-LOG002 INVALID KEY
                            MOVE "Erro de Gravação...LOG002" TO
                            MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                       END-WRITE
                       CLOSE LOG002.

      *    EXCLUI-EVENTOS
           MOVE GS-DOCTO             TO DOCTO-R101.
           MOVE ZEROS                TO CONTRATO-R101 EVENTO-R101.
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
                 READ RED101 NEXT RECORD AT END
                      MOVE "10" TO ST-RED101
                 NOT AT END
                      IF DOCTO-R101 <> GS-DOCTO
                         MOVE "10" TO ST-RED101
                      ELSE
                         DELETE RED101 NOT INVALID KEY
                             OPEN I-O LOG001
                             MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                             ACCEPT WS-HORA-SYS FROM TIME
                             MOVE USUARIO-W TO LOG1-USUARIO
                             MOVE WS-DATA-CPU TO LOG1-DATA
                             MOVE WS-HORA-SYS TO LOG1-HORAS
                             MOVE "E"         TO LOG1-OPERACAO
                             MOVE "RED101"    TO LOG1-ARQUIVO
                             MOVE "REP100"    TO LOG1-PROGRAMA
                             MOVE REG-RED101  TO LOG1-REGISTRO
                             WRITE REG-LOG001 INVALID KEY
                                  MOVE "Erro de Gravação...LOG001" TO
                                  MENSAGEM
                                  MOVE "C" TO TIPO-MSG
                                  PERFORM EXIBIR-MENSAGEM
                             END-WRITE
                             CLOSE LOG001
                         END-DELETE
                      END-IF
                 END-READ
           END-PERFORM.

      *    EXCLUI EQUIPE
           MOVE GS-DOCTO             TO DOCTO-R103.
           MOVE ZEROS                TO SEQ-R103.
           START RED103 KEY IS NOT < ALT1-R103 INVALID KEY
                 MOVE "10" TO ST-RED103.
           PERFORM UNTIL ST-RED103 = "10"
             READ RED103 NEXT RECORD AT END
                  MOVE "10" TO ST-RED103
             NOT AT END
                IF DOCTO-R103 <> GS-DOCTO
                   MOVE "10" TO ST-RED103
                ELSE
                   DELETE RED103 NOT INVALID KEY
                          OPEN I-O LOG001
                          MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                          ACCEPT WS-HORA-SYS FROM TIME
                          MOVE USUARIO-W TO LOG1-USUARIO
                          MOVE WS-DATA-CPU TO LOG1-DATA
                          MOVE WS-HORA-SYS TO LOG1-HORAS
                          MOVE "E"         TO LOG1-OPERACAO
                          MOVE "RED103"    TO LOG1-ARQUIVO
                          MOVE "REP100"    TO LOG1-PROGRAMA
                          MOVE REG-RED103  TO LOG1-REGISTRO
                          WRITE REG-LOG001 INVALID KEY
                               MOVE "Erro de Gravação...LOG001" TO
                               MENSAGEM
                               MOVE "C" TO TIPO-MSG
                               PERFORM EXIBIR-MENSAGEM
                          END-WRITE
                          CLOSE LOG001
                   END-DELETE
             END-READ
           END-PERFORM.

           CLOSE      RED100 RED101 RED103
           OPEN INPUT RED100 RED101 RED103.

      *----------------------------------------------------------------
       GRAVAR-DADOS-EQUIPE SECTION.
           CLOSE    RED103
           OPEN I-O RED103

           MOVE GS-DOCTO             TO DOCTO-R103
           MOVE GS-CODIGO-EQ         TO CODIGO-R103
           MOVE GS-FUNCAO-EQ         TO FUNCAO-R103
           READ RED103 INVALID KEY
                MOVE GS-DOCTO             TO DOCTO-R103
                MOVE GS-SEQ-EQ            TO SEQ-R103
                                             GS-LINDET1(1: 4)
                START RED103 KEY IS = ALT1-R103 INVALID KEY
                      MOVE 1              TO GRAVA-W
                NOT INVALID KEY
                      MOVE 0              TO GRAVA-W
                END-START

                MOVE GS-CODIGO-EQ         TO CODIGO-R103
                                             GS-LINDET1(5: 7)
                                             CODIGO-CG01
                READ CGD001 INVALID KEY
                     MOVE SPACES TO NOME-CG01
                END-READ

                MOVE NOME-CG01            TO GS-LINDET1(12: 31)
                MOVE GS-FUNCAO-EQ         TO FUNCAO-R103
                                             GS-LINDET1(43: 3)
                                             CODIGO-RE02

                MOVE "-"                  TO GS-LINDET1(46: 1)
                READ RED002 INVALID KEY
                     MOVE SPACES          TO DESCRICAO-RE02
                END-READ

                MOVE DESCRICAO-RE02       TO GS-LINDET(47: 11)
                MOVE GS-FIT-FIL-KM(1: 1)  TO FITA-FILME-R103
                                             GS-LINDET1(59: 3)
                MOVE GS-QTDE-EQ           TO QT-FILMES-R103
                                             GS-LINDET1(62: 10)
                MOVE GS-TIPO-VLR-EQ(1: 1) TO TIPO-REPORT-R103
                                             GS-LINDET1(72: 3)
                MOVE GS-QTDE-REPORT       TO QT-REPORT-R103 QTDE-E1
                MOVE QTDE-E1              TO GS-LINDET1(75: 8)
                MOVE GS-VALOR-EQ          TO VLR-REPORT-R103 VALOR-E
                MOVE VALOR-E              TO GS-LINDET1(83: 13)
                IF GRAVA-W = 1
                   WRITE REG-RED103 INVALID KEY
                     MOVE "ERRO GRAVACAO RED103: "  TO GS-MENSAGEM-ERRO
                     MOVE ST-RED103 TO GS-MENSAGEM-ERRO(23: 02)
                     PERFORM CARREGA-MENSAGEM-ERRO
                   NOT INVALID KEY
                     OPEN I-O LOG001
                     MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                     ACCEPT WS-HORA-SYS FROM TIME
                     MOVE USUARIO-W TO LOG1-USUARIO
                     MOVE WS-DATA-CPU TO LOG1-DATA
                     MOVE WS-HORA-SYS TO LOG1-HORAS
                     MOVE "I"         TO LOG1-OPERACAO
                     MOVE "RED103"    TO LOG1-ARQUIVO
                     MOVE "REP100"    TO LOG1-PROGRAMA
                     MOVE REG-RED103  TO LOG1-REGISTRO
                     WRITE REG-LOG001 INVALID KEY
                          MOVE "Erro de Gravação...LOG001" TO
                          MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                     END-WRITE
                     CLOSE LOG001
                   END-WRITE
      *            PERFORM MOVER-LISTA-RED103-EQUIPE
      *            MOVE "INSERE-LIST-EQUIPE" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
                   PERFORM ATUALIZA-DADOS-EQU-RED100
                ELSE
                   REWRITE REG-RED103 INVALID KEY
                     MOVE "ERRO REGRAVACAO RED103: "
                       TO GS-MENSAGEM-ERRO
                     MOVE ST-RED103 TO GS-MENSAGEM-ERRO(23: 02)
                     PERFORM CARREGA-MENSAGEM-ERRO
                   NOT INVALID KEY
                     OPEN I-O LOG001
                     MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                     ACCEPT WS-HORA-SYS FROM TIME
                     MOVE USUARIO-W TO LOG1-USUARIO
                     MOVE WS-DATA-CPU TO LOG1-DATA
                     MOVE WS-HORA-SYS TO LOG1-HORAS
                     MOVE "A"         TO LOG1-OPERACAO
                     MOVE "RED103"    TO LOG1-ARQUIVO
                     MOVE "REP100"    TO LOG1-PROGRAMA
                     MOVE REG-RED103  TO LOG1-REGISTRO
                     WRITE REG-LOG001 INVALID KEY
                          MOVE "Erro de Gravação...LOG001" TO
                          MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                     END-WRITE
                     CLOSE LOG001
                   END-REWRITE
                   PERFORM REGRAVA-DADOS-EQU-RED100
                END-IF
      *         PERFORM MOVER-LISTA-RED103-EQUIPE
                EVALUATE GS-TIPO-EQUIPE
                  WHEN 0
                   MOVE "INSERE-LIST-EQUIPE" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   MOVE "CLEAR-OBJECT-EQUIPE" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                  WHEN 1
                      MOVE "ATUALIZA-EQUIPE" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                END-EVALUATE
           NOT INVALID KEY
                IF GS-SEQ-EQ <> SEQ-R103
                   MOVE "Equipe Já Cadastrada" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
                 ELSE
                   MOVE GS-DOCTO             TO DOCTO-R103
                   MOVE GS-SEQ-EQ            TO SEQ-R103
                                                GS-LINDET1(1: 4)
                   START RED103 KEY IS = ALT1-R103 INVALID KEY
                         MOVE 1              TO GRAVA-W
                   NOT INVALID KEY
                         MOVE 0              TO GRAVA-W
                   END-START

                   MOVE GS-CODIGO-EQ         TO CODIGO-R103
                                                GS-LINDET1(5: 7)
                                                CODIGO-CG01
                   READ CGD001 INVALID KEY
                        MOVE SPACES TO NOME-CG01
                   END-READ

                   MOVE NOME-CG01            TO GS-LINDET1(12: 31)
                   MOVE GS-FUNCAO-EQ         TO FUNCAO-R103
                                                GS-LINDET1(43: 3)
                                                CODIGO-RE02

                   MOVE "-"                  TO GS-LINDET1(46: 1)
                   READ RED002 INVALID KEY
                        MOVE SPACES          TO DESCRICAO-RE02
                   END-READ

                   MOVE DESCRICAO-RE02       TO GS-LINDET(47: 11)
                   MOVE GS-FIT-FIL-KM(1: 1)  TO FITA-FILME-R103
                                                GS-LINDET1(59: 3)
                   MOVE GS-QTDE-EQ           TO QT-FILMES-R103
                                                GS-LINDET1(62: 10)
                   MOVE GS-TIPO-VLR-EQ(1: 1) TO TIPO-REPORT-R103
                                                GS-LINDET1(72: 3)
                   MOVE GS-QTDE-REPORT       TO QT-REPORT-R103 QTDE-E1
                   MOVE QTDE-E1              TO GS-LINDET1(75: 8)
                   MOVE GS-VALOR-EQ          TO VLR-REPORT-R103 VALOR-E
                   MOVE VALOR-E              TO GS-LINDET1(83: 13)
                   IF GRAVA-W = 1
                      WRITE REG-RED103 INVALID KEY
                        MOVE "ERRO GRAVACAO RED103: "
                          TO GS-MENSAGEM-ERRO
                        MOVE ST-RED103 TO GS-MENSAGEM-ERRO(23: 02)
                        PERFORM CARREGA-MENSAGEM-ERRO
                      NOT INVALID KEY
                        OPEN I-O LOG001
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE USUARIO-W TO LOG1-USUARIO
                        MOVE WS-DATA-CPU TO LOG1-DATA
                        MOVE WS-HORA-SYS TO LOG1-HORAS
                        MOVE "I"         TO LOG1-OPERACAO
                        MOVE "RED103"    TO LOG1-ARQUIVO
                        MOVE "REP100"    TO LOG1-PROGRAMA
                        MOVE REG-RED103  TO LOG1-REGISTRO
                        WRITE REG-LOG001 INVALID KEY
                             MOVE "Erro de Gravação...LOG001" TO
                             MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                        END-WRITE
                        CLOSE LOG001
                      END-WRITE
      *               PERFORM MOVER-LISTA-RED103-EQUIPE
      *               MOVE "INSERE-LIST-EQUIPE" TO DS-PROCEDURE
      *               PERFORM CALL-DIALOG-SYSTEM
                      PERFORM ATUALIZA-DADOS-EQU-RED100
                   ELSE
                      REWRITE REG-RED103 INVALID KEY
                        MOVE "ERRO REGRAVACAO RED103: "
                          TO GS-MENSAGEM-ERRO
                        MOVE ST-RED103 TO GS-MENSAGEM-ERRO(23: 02)
                        PERFORM CARREGA-MENSAGEM-ERRO
                      NOT INVALID KEY
                        OPEN I-O LOG001
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE USUARIO-W TO LOG1-USUARIO
                        MOVE WS-DATA-CPU TO LOG1-DATA
                        MOVE WS-HORA-SYS TO LOG1-HORAS
                        MOVE "A"         TO LOG1-OPERACAO
                        MOVE "RED103"    TO LOG1-ARQUIVO
                        MOVE "REP100"    TO LOG1-PROGRAMA
                        MOVE REG-RED103  TO LOG1-REGISTRO
                        WRITE REG-LOG001 INVALID KEY
                             MOVE "Erro de Gravação...LOG001" TO
                             MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                        END-WRITE
                        CLOSE LOG001
                      END-REWRITE
                      PERFORM REGRAVA-DADOS-EQU-RED100
                   END-IF
      *            PERFORM MOVER-LISTA-RED103-EQUIPE
                   EVALUATE GS-TIPO-EQUIPE
                     WHEN 0
                      MOVE "INSERE-LIST-EQUIPE" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      MOVE "CLEAR-OBJECT-EQUIPE" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                     WHEN 1
                         MOVE "ATUALIZA-EQUIPE" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                   END-EVALUATE
                 END-IF
           END-READ.

           CLOSE      RED103
           OPEN INPUT RED103.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       EXCLUI-EQUIPE SECTION.
           CLOSE    RED103
           OPEN I-O RED103
           MOVE GS-DOCTO          TO DOCTO-R103.
           MOVE GS-SEQ-EQ         TO SEQ-R103.
           READ RED103 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE RED103 NOT INVALID KEY
                    OPEN I-O LOG001
                    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                    ACCEPT WS-HORA-SYS FROM TIME
                    MOVE USUARIO-W TO LOG1-USUARIO
                    MOVE WS-DATA-CPU TO LOG1-DATA
                    MOVE WS-HORA-SYS TO LOG1-HORAS
                    MOVE "E"         TO LOG1-OPERACAO
                    MOVE "RED103"    TO LOG1-ARQUIVO
                    MOVE "REP100"    TO LOG1-PROGRAMA
                    MOVE REG-RED103  TO LOG1-REGISTRO
                    WRITE REG-LOG001 INVALID KEY
                         MOVE "Erro de Gravação...LOG001" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                    END-WRITE
                    CLOSE LOG001
                END-DELETE
                PERFORM REGRAVA-DADOS-EQU-RED100.
           CLOSE      RED103
           OPEN INPUT RED103.

       ATUALIZA-DADOS-EQU-RED100 SECTION.
           CLOSE    RED100
           OPEN I-O RED100
           EVALUATE FITA-FILME-R103
      *      1-FILME  2-FITA 3-KM
             WHEN 1 ADD QT-FILMES-R103    TO TOT-FILME-REPORT-R100
             WHEN 2 ADD QT-FILMES-R103    TO TOT-FITA-REPORT-R100
             WHEN 3 CONTINUE
           END-EVALUATE
           EVALUATE TIPO-REPORT-R103
      *      1-REPORTAGEM   2-DESPESA
             WHEN 1 ADD VLR-REPORT-R103   TO VLR-TOT-REPORT-R100
             WHEN 2 ADD VLR-REPORT-R103   TO VLR-DESPESA-REPORT-R100
           END-EVALUATE
           REWRITE REG-RED100 NOT INVALID KEY
                   OPEN I-O LOG002
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE USUARIO-W TO LOG2-USUARIO
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "A"         TO LOG2-OPERACAO
                   MOVE "RED100"    TO LOG2-ARQUIVO
                   MOVE "REP100"    TO LOG2-PROGRAMA
                   MOVE REG-RED100  TO LOG2-REGISTRO
                   WRITE REG-LOG002 INVALID KEY
                        MOVE "Erro de Gravação...LOG002" TO
                        MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                   END-WRITE
                   CLOSE LOG002
           END-REWRITE
           MOVE VLR-TOT-REPORT-R100       TO GS-VLR-TOT-REPORT
           MOVE VLR-DESPESA-REPORT-R100   TO GS-VLR-DESPESA-REPORT
           MOVE TOT-FILME-REPORT-R100     TO GS-TOT-FILME-REPORT
           MOVE TOT-FITA-REPORT-R100      TO GS-TOT-FITA-REPORT.
           CLOSE      RED100
           OPEN INPUT RED100.

       REGRAVA-DADOS-EQU-RED100 SECTION.
           CLOSE    RED100
           OPEN I-O RED100
           MOVE GS-DOCTO   TO DOCTO-R103.
           MOVE ZEROS      TO CODIGO-R103 FUNCAO-R103.
           MOVE ZEROS TO VLR-TOT-REPORT-R100 VLR-DESPESA-REPORT-R100
                         TOT-FILME-REPORT-R100 TOT-FITA-REPORT-R100.
           START RED103 KEY IS NOT < CHAVE-R103 INVALID KEY
                 MOVE "10" TO ST-RED103.
           PERFORM UNTIL ST-RED103 = "10"
                 READ RED103 NEXT RECORD AT END
                      MOVE "10" TO ST-RED103
                 NOT AT END
                    IF DOCTO-R103 <> GS-DOCTO
                       MOVE "10" TO ST-RED103
                    ELSE
                       EVALUATE FITA-FILME-R103
      *                  1-FILME  2-FITA 3-KM
                         WHEN 1
                          ADD QT-FILMES-R103  TO TOT-FILME-REPORT-R100
                         WHEN 2
                          ADD QT-FILMES-R103  TO TOT-FITA-REPORT-R100
                         WHEN 3 CONTINUE
                       END-EVALUATE
                       EVALUATE TIPO-REPORT-R103
      *                  1-REPORTAGEM   2-DESPESA
                         WHEN 1
                          ADD VLR-REPORT-R103 TO VLR-TOT-REPORT-R100
                         WHEN 2
                          ADD VLR-REPORT-R103 TO VLR-DESPESA-REPORT-R100
                       END-EVALUATE
                       REWRITE REG-RED100 NOT INVALID KEY
                          OPEN I-O LOG002
                          MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                          ACCEPT WS-HORA-SYS FROM TIME
                          MOVE USUARIO-W TO LOG2-USUARIO
                          MOVE WS-DATA-CPU TO LOG2-DATA
                          MOVE WS-HORA-SYS TO LOG2-HORAS
                          MOVE "A"         TO LOG2-OPERACAO
                          MOVE "RED100"    TO LOG2-ARQUIVO
                          MOVE "REP100"    TO LOG2-PROGRAMA
                          MOVE REG-RED100  TO LOG2-REGISTRO
                          WRITE REG-LOG002 INVALID KEY
                               MOVE "Erro de Gravação...LOG002" TO
                               MENSAGEM
                               MOVE "C" TO TIPO-MSG
                               PERFORM EXIBIR-MENSAGEM
                          END-WRITE
                          CLOSE LOG002
                       END-REWRITE
                    END-IF
                 END-READ
           END-PERFORM.
           MOVE VLR-TOT-REPORT-R100       TO GS-VLR-TOT-REPORT
           MOVE VLR-DESPESA-REPORT-R100   TO GS-VLR-DESPESA-REPORT
           MOVE TOT-FILME-REPORT-R100     TO GS-TOT-FILME-REPORT
           MOVE TOT-FITA-REPORT-R100      TO GS-TOT-FITA-REPORT.

           CLOSE      RED100
           OPEN INPUT RED100.

       SELECAO-EQUIPE SECTION.
           MOVE GS-DOCTO        TO DOCTO-R103
           MOVE GS-LINDET1(1: 3) TO SEQ-R103.
           START RED103 KEY IS = ALT1-R103 INVALID KEY
                        INITIALIZE REG-RED103
                        PERFORM MOVER-LINDET1-EQUIPE
           NOT INVALID KEY
            READ RED103 NEXT RECORD
            END-READ
               MOVE SEQ-R103          TO GS-SEQ-EQ
               MOVE CODIGO-R103       TO GS-CODIGO-EQ CODIGO-CG01
               READ CGD001 INVALID KEY
                    MOVE SPACES TO NOME-CG01
               END-READ
               MOVE NOME-CG01         TO GS-DESC-EQ
               MOVE FUNCAO-R103       TO GS-FUNCAO-EQ CODIGO-RE02
               READ RED002 INVALID KEY
                    MOVE SPACES TO DESCRICAO-RE02
               END-READ
               MOVE DESCRICAO-RE02    TO GS-DESC-FUNCAO-EQ
               MOVE GS-LINDET1(59: 1) TO FITA-FILME-R103
               EVALUATE FITA-FILME-R103
                 WHEN 1 MOVE "1-Filme   " TO GS-FIT-FIL-KM
                 WHEN 2 MOVE "2-Fita    " TO GS-FIT-FIL-KM
                 WHEN 3 MOVE "3-Km      " TO GS-FIT-FIL-KM
                 WHEN 4 MOVE "4-Cd-Filme" TO GS-FIT-FIL-KM
                 WHEN OTHER MOVE "0-Nenhum" TO GS-FIT-FIL-KM
               END-EVALUATE
               MOVE GS-LINDET1(72: 1)    TO TIPO-REPORT-R103
               EVALUATE TIPO-REPORT-R103
                 WHEN 1 MOVE "1-Reportag" TO GS-TIPO-VLR-EQ
                 WHEN 2 MOVE "2-Despesa " TO GS-TIPO-VLR-EQ
               END-EVALUATE
               MOVE QT-FILMES-R103    TO GS-QTDE-EQ
               MOVE QT-REPORT-R103    TO GS-QTDE-REPORT
               MOVE VLR-REPORT-R103   TO GS-VALOR-EQ
           END-START.
       MOVER-LINDET1-EQUIPE SECTION.
           MOVE GS-LINDET1(1: 3)  TO GS-SEQ-EQ
           MOVE GS-LINDET1(5: 6)  TO GS-CODIGO-EQ
           MOVE GS-LINDET1(12: 30) TO GS-DESC-EQ
           MOVE GS-LINDET1(43: 2) TO GS-FUNCAO-EQ CODIGO-RE02
           READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02.
           MOVE DESCRICAO-RE02    TO GS-DESC-FUNCAO-EQ.
           MOVE GS-LINDET1(59: 1) TO FITA-FILME-R103.
           EVALUATE FITA-FILME-R103
             WHEN 1 MOVE "1-Filme   " TO GS-FIT-FIL-KM
             WHEN 2 MOVE "2-Fita    " TO GS-FIT-FIL-KM
             WHEN 3 MOVE "3-Km      " TO GS-FIT-FIL-KM
             WHEN 4 MOVE "4-Cd-Filme" TO GS-FIT-FIL-KM
           END-EVALUATE
           MOVE GS-LINDET1(72: 1)    TO TIPO-REPORT-R103
           EVALUATE TIPO-REPORT-R103
             WHEN 1 MOVE "1-Reportag" TO GS-TIPO-VLR-EQ
             WHEN 2 MOVE "2-Despesa " TO GS-TIPO-VLR-EQ
           END-EVALUATE.
           MOVE ZEROS TO GS-QTDE-EQ GS-QTDE-REPORT GS-VALOR-EQ.

      *----------------------------------------------------------------
       GRAVAR-DADOS-EVENTO SECTION.
           CLOSE    RED101
           OPEN I-O RED101
           MOVE GS-DOCTO        TO DOCTO-R101.
           MOVE GS-CONTRATO     TO CONTRATO-R101
           MOVE GS-EVENTO       TO EVENTO-R101
           READ RED101 INVALID KEY MOVE 1 TO GRAVA-W
              NOT INVALID KEY MOVE 0 TO GRAVA-W.
           MOVE GS-QTDE-PARTICIPANTE  TO QT-PARTIC-R101 GS-LINDET(66: 5)
           IF GRAVA-W = 1
              WRITE REG-RED101 INVALID KEY
                    MOVE "ERRO GRAVACAO RED101: "  TO GS-MENSAGEM-ERRO
                    MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
                    PERFORM CARREGA-MENSAGEM-ERRO
              NOT INVALID KEY
                    OPEN I-O LOG001
                    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                    ACCEPT WS-HORA-SYS FROM TIME
                    MOVE USUARIO-W TO LOG1-USUARIO
                    MOVE WS-DATA-CPU TO LOG1-DATA
                    MOVE WS-HORA-SYS TO LOG1-HORAS
                    MOVE "I"         TO LOG1-OPERACAO
                    MOVE "RED101"    TO LOG1-ARQUIVO
                    MOVE "REP100"    TO LOG1-PROGRAMA
                    MOVE REG-RED101  TO LOG1-REGISTRO
                    WRITE REG-LOG001 INVALID KEY
                         MOVE "Erro de Gravação...LOG001" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                    END-WRITE
                    CLOSE LOG001
              END-WRITE
           ELSE
              REWRITE REG-RED101 INVALID KEY
                    MOVE "ERRO REGRAVACAO RED101: "  TO GS-MENSAGEM-ERRO
                    MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
                    PERFORM CARREGA-MENSAGEM-ERRO
              NOT INVALID KEY
                    OPEN I-O LOG001
                    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                    ACCEPT WS-HORA-SYS FROM TIME
                    MOVE USUARIO-W TO LOG1-USUARIO
                    MOVE WS-DATA-CPU TO LOG1-DATA
                    MOVE WS-HORA-SYS TO LOG1-HORAS
                    MOVE "A"         TO LOG1-OPERACAO
                    MOVE "RED101"    TO LOG1-ARQUIVO
                    MOVE "REP100"    TO LOG1-PROGRAMA
                    MOVE REG-RED101  TO LOG1-REGISTRO
                    WRITE REG-LOG001 INVALID KEY
                         MOVE "Erro de Gravação...LOG001" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                    END-WRITE
                    CLOSE LOG001
              END-REWRITE
           END-IF
           PERFORM MOVER-LISTA-RED101-EVENTO.
           EVALUATE GS-TIPO-EVENTO
             WHEN 0
              MOVE "INSERE-LIST-EVENTO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              MOVE "CLEAR-OBJECT-EVENTO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
             WHEN 1
                 MOVE "ATUALIZA-EVENTO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           END-EVALUATE.

           CLOSE      RED101
           OPEN INPUT RED101.

       SELECAO-EVENTO SECTION.
           MOVE GS-DOCTO           TO DOCTO-R101.
           MOVE GS-LINDET(1: 4)    TO CONTRATO-R101.
           MOVE GS-LINDET(37: 5)   TO EVENTO-R101.
           READ RED101 INVALID KEY
                INITIALIZE REG-RED101.
           MOVE GS-LINDET(1: 4)    TO GS-CONTRATO
                                      NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE SPACES        TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO.
           MOVE GS-LINDET(37: 5)   TO GS-EVENTO CODIGO-CO03.
           READ COD003 INVALID KEY
                MOVE SPACES        TO NOME-CO03.
           MOVE NOME-CO03          TO GS-DESC-EVENTO.
           MOVE QT-PARTIC-R101     TO GS-QTDE-PARTICIPANTE.
       EXCLUI-EVENTO SECTION.
           CLOSE    RED101
           OPEN I-O RED101
           MOVE GS-DOCTO          TO DOCTO-R101.
           MOVE GS-CONTRATO       TO CONTRATO-R101.
           MOVE GS-EVENTO         TO EVENTO-R101.
           READ RED101 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE RED101 NOT INVALID KEY
                    OPEN I-O LOG001
                    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                    ACCEPT WS-HORA-SYS FROM TIME
                    MOVE USUARIO-W TO LOG1-USUARIO
                    MOVE WS-DATA-CPU TO LOG1-DATA
                    MOVE WS-HORA-SYS TO LOG1-HORAS
                    MOVE "E"         TO LOG1-OPERACAO
                    MOVE "RED101"    TO LOG1-ARQUIVO
                    MOVE "REP100"    TO LOG1-PROGRAMA
                    MOVE REG-RED101  TO LOG1-REGISTRO
                    WRITE REG-LOG001 INVALID KEY
                         MOVE "Erro de Gravação...LOG001" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                    END-WRITE
                    CLOSE LOG001
                END-DELETE
           END-READ
           CLOSE    RED101
           OPEN I-O RED101.

      *----------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

      *-----------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.

      *    imprime dados da reportagem
           MOVE SPACES              TO LINDET-REL
           MOVE "DADOS DE REPORTAGEM " TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES TO LINDET-REL
           MOVE "DOCTO.........: "  TO LINDET-REL(1: 16)
           MOVE "DATA MOVTO....: "  TO LINDET-REL(40: 16)
           MOVE GS-DOCTO            TO LINDET-REL(18: 20)
           MOVE GS-DATA-REPORT      TO DATA-E
           MOVE DATA-E              TO LINDET-REL(58: 20)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "QT.PESSOAS....: "  TO LINDET-REL(1: 16)
           MOVE "QT.VEICULO....: "  TO LINDET-REL(40: 16)
           MOVE GS-QT-PESSOAS       TO LINDET-REL(18: 20)
           MOVE GS-QT-VEICULO       TO LINDET-REL(58: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE "QT.DIAS.......: "  TO LINDET-REL(1: 16)
           MOVE "QT.REAL FORM..: "  TO LINDET-REL(40: 16)
           MOVE GS-QT-DIAS          TO QTDE-E
           MOVE QTDE-E              TO LINDET-REL(18: 20)
           MOVE GS-QT-FORM          TO LINDET-REL(58: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES              TO LINDET-REL.
           MOVE "LCTO.CTA.COR..: "  TO LINDET-REL(1: 16)
           MOVE GS-LCTO-CTA-CORR    TO LINDET-REL(18: 20)
           WRITE REG-RELAT FROM LINDET

           MOVE SPACES              TO LINDET-REL
           MOVE "DESPESAS "         TO LINDET-REL
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE SPACES TO LINDET-REL
           MOVE "VLR.HOSPEDAG..: "  TO LINDET-REL(40: 16)
           MOVE "VLR.COMBUST...: "  TO LINDET-REL(1: 16)
           MOVE GS-VLR-COMBUSTIVEL  TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(18: 20)
           MOVE GS-VLR-HOSPEDAGEM   TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(58: 20)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "VLR.PASSAG....: "  TO LINDET-REL(40: 16)
           MOVE "VLR.REFEICAO..: "  TO LINDET-REL(1: 16)
           MOVE GS-VLR-REFEICAO     TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(18: 20)
           MOVE GS-VLR-PASSAGEM     TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(58: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE "VLR.MATERIAL..: "  TO LINDET-REL(40: 16)
           MOVE "VLR.ALUGUEL...: "  TO LINDET-REL(1: 16)
           MOVE GS-VLR-ALUGUEL      TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(18: 20)
           MOVE GS-VLR-MATERIAL     TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(58: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES              TO LINDET-REL
           MOVE "VLR.OUTROS....: "  TO LINDET-REL( 1: 16)
           MOVE GS-VLR-OUTROS       TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(18: 20)
           WRITE REG-RELAT FROM LINDET

           MOVE SPACES              TO LINDET-REL
           MOVE "DADOS EQUIPE REPORTAGEM" TO LINDET-REL
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE SPACES TO LINDET-REL
           MOVE "VLR.TOT.REPORT: "  TO LINDET-REL(1: 16)
           MOVE "VLR.DESP.REPOR: "  TO LINDET-REL(40: 16)
           MOVE GS-VLR-TOT-REPORT   TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(18: 20)
           MOVE GS-VLR-DESPESA-REPORT TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(58: 20)
           WRITE REG-RELAT FROM LINDET
           MOVE "TOT.FILME.....: "  TO LINDET-REL(1: 16)
           MOVE "TOT.FITA......: "  TO LINDET-REL(40: 16)
           MOVE GS-TOT-FILME-REPORT TO LINDET-REL(18: 20)
           MOVE GS-TOT-FITA-REPORT  TO LINDET-REL(58: 20)
           WRITE REG-RELAT FROM LINDET.
           ADD 17 TO LIN.

           MOVE SPACES                    TO LINDET-REL
           MOVE "NUMERO DE PARTICIPANTES" TO LINDET-REL
           WRITE REG-RELAT FROM LINDET AFTER 2
           WRITE REG-RELAT FROM CAB03 AFTER 2
           WRITE REG-RELAT FROM TRACO
           MOVE ZEROS TO CONTRATO-R101 EVENTO-R101
           MOVE GS-DOCTO  TO DOCTO-R101.
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
                 MOVE "10" TO ST-RED101.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END
                  MOVE "10"                 TO ST-RED101
             NOT AT END
                 IF DOCTO-R101 <> GS-DOCTO
                    MOVE "10"               TO ST-RED101
                 ELSE
                    MOVE CONTRATO-R101      TO NR-CONTRATO-CO40
                    READ COD040 INVALID KEY
                         MOVE SPACES        TO IDENTIFICACAO-CO40
                    END-READ
                    MOVE CONTRATO-R101      TO LINDET-REL(1: 5)
                    MOVE IDENTIFICACAO-CO40 TO LINDET-REL(6: 20)
                    MOVE EVENTO-R101        TO LINDET-REL(27: 5)
                                               CODIGO-CO03
                    READ COD003 INVALID KEY
                         MOVE SPACES        TO NOME-CO03
                    END-READ
                    MOVE NOME-CO03          TO LINDET-REL(33: 21)
                    MOVE QT-PARTIC-R101     TO LINDET-REL(54: 6)
                    IF STATUS-CO40 <> WS-STATUS-REVENDIDO AND
                       WS-STATUS-ANALISE
                       WRITE REG-RELAT FROM LINDET
                       ADD 1 TO LIN
                       IF LIN > 56
                          PERFORM CABECALHO
                       END-IF
                    END-IF
                 END-IF
             END-READ
           END-PERFORM.

           ADD 5 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
           MOVE SPACES                  TO LINDET-REL
           MOVE "EQUIPE DE REPORTAGEM"  TO LINDET-REL
           WRITE REG-RELAT FROM LINDET AFTER 2
           WRITE REG-RELAT FROM CAB04
           WRITE REG-RELAT FROM TRACO
           ADD 5 TO LIN.

           MOVE GS-DOCTO        TO DOCTO-R103.
           MOVE ZEROS           TO SEQ-R103.
           START RED103 KEY IS NOT < ALT1-R103 INVALID KEY
                 MOVE "10" TO ST-RED103.
           PERFORM UNTIL ST-RED103 = "10"
             READ RED103 NEXT RECORD AT END MOVE "10" TO ST-RED103
               NOT AT END
                 IF DOCTO-R103 <> GS-DOCTO
                    MOVE "10"                 TO ST-RED103
                 ELSE
                   MOVE SEQ-R103              TO LINDET-REL(1: 4)
                   MOVE CODIGO-R103           TO LINDET-REL(5: 7)
                                                 CODIGO-CG01
                   READ CGD001 INVALID KEY
                        MOVE SPACES           TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01             TO LINDET-REL(12: 11)
                   MOVE FUNCAO-R103           TO LINDET-REL(24: 3)
                                                 CODIGO-RE02
                   READ RED002 INVALID KEY
                        MOVE SPACES           TO DESCRICAO-RE02
                   END-READ
                   MOVE DESCRICAO-RE02        TO LINDET-REL(27: 5)
                   EVALUATE FITA-FILME-R103
                     WHEN 0 MOVE "0-NENHUM  " TO LINDET-REL(33: 11)
                     WHEN 1 MOVE "1-FILME   " TO LINDET-REL(33: 11)
                     WHEN 2 MOVE "2-FITA    " TO LINDET-REL(33: 11)
                     WHEN 3 MOVE "3-KM      " TO LINDET-REL(33: 11)
                   END-EVALUATE
                   MOVE QT-FILMES-R103        TO LINDET-REL(44: 7)
                   EVALUATE TIPO-REPORT-R103
                     WHEN 1 MOVE "1-REPORT  " TO LINDET-REL(51: 11)
                     WHEN 2 MOVE "2-DESPESA " TO LINDET-REL(51: 11)
                   END-EVALUATE
                   MOVE QT-REPORT-R103        TO QTDE-E1
                   MOVE QTDE-E1               TO LINDET-REL(62: 6)
                   MOVE VLR-REPORT-R103       TO VALOR-E
                   MOVE VALOR-E               TO LINDET-REL(68: 13)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM TRACO.
           MOVE 4 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
      *--------------------------------------------------------------
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD003 COD040 COD060 CGD001 RED002
                 RED100 RED101 RED103 PRD100 PRD101 PRD105.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
