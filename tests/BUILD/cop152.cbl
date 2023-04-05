       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP152.
       DATE-WRITTEN. 15/12/1999.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: Relatório FINANCEIRO de compromisso contratual
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX002.
           COPY COPX004.
           COPY COPX040.
           COPY COPX050.
           COPY CAPX010.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = CONTRATO-WK ITEM-WK
                  ALTERNATE RECORD KEY IS CONTRATO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DESC-BRINDE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-VENCTO-WK
                                          WITH DUPLICATES.

           SELECT WORK1 ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK1
                  RECORD KEY IS CAMPANHA-WK1.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY COPW002.
       COPY COPW004.
       COPY COPW040.
       COPY COPW050.
       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  ITEM-WK             PIC 9(4).
           05  BRINDE-WK           PIC 9(3).
           05  DESC-BRINDE-WK      PIC X(20).
           05  DATA-VENCTO-WK      PIC 9(8).
           05  VLR-TOTAL-WK        PIC 9(8)V99.
           05  IDENTIFICACAO-WK    PIC X(20).
           05  CIDADE-WK           PIC X(13).
           05  MESANO-WK           PIC 9(6).
       FD  WORK1.
       01  REG-WORK1.
           05  CAMPANHA-WK1        PIC 9(2).
           05  VALOR-WK1           PIC 9(10)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP152.CPB".
           COPY "COP152.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD004             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC Z.ZZZ.ZZZ.ZZZ,ZZ.
           05  QTDE-E                PIC ZZ.ZZZ.ZZZ BLANK WHEN ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.
      *    VARIAVEIS P/ AUXILIAR NA TOTALIZACAO
           05  DESC-BRINDE-ANT       PIC X(20)    VALUE SPACES.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  DATA-VENCTO-ANT       PIC 9(8)     VALUE ZEROS.
           05  VLR-TOTAL-TOT         PIC 9(8)V99  VALUE ZEROS.
           05  VLR-TOTAL-TOTG        PIC 9(8)V99  VALUE ZEROS.
           05  CAMPANHA-W            PIC 9(8)     VALUE ZEROS.
           05  PERC-W                PIC 9(3)V99  VALUE ZEROS.
           05  PERC-E                PIC ZZZ,ZZ   BLANK WHEN ZEROS.
           05  QTDE-BRINDE           PIC 9(8)     VALUE ZEROS.
           05  CUSTO-PREVISTO        PIC 9(8)V99  VALUE ZEROS.
           05  VLR-TOTAL-CAMPANHA    PIC 9(10)v99 VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(64)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(52)   VALUE
           "RELAT.FINANCEIRO DE COMPR.CONTRATUAL PAGO - ORDEM: ".
           05  ORDEM-REL           PIC X(24)   VALUE SPACES.
           05  TIPO-DATA-REL       PIC X(12).
           05  DATA-VENCTO-INI     PIC ZZ/ZZ/ZZZZ.
           05  FILLER              PIC X(3)    VALUE " a ".
           05  DATA-VENCTO-FIM     PIC ZZ/ZZ/ZZZZ.

       01  CAB03.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(111)  VALUE ALL "=".
       01  CAB04A.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(111)  VALUE
           "CAMPANHA                              VALOR    PERC.(%)".
       01  CAB04.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(111)  VALUE
           "CONT IDENTIFICACAO         CIDADE         MES/ANO ITEM COMP.
      -    "CONTRATUAL      DATA-VECTO         VLR-TOTAL".
       01  LINDET.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  LINDET-REL          PIC X(111)  VALUE SPACES.

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
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD004.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD050.
           OPEN INPUT CAD010 COD040 COD050 COD002 COD004.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD004 <> "00"
              MOVE "ERRO ABERTURA COD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD050 TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM ZERA-VARIAVEIS
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
               WHEN GS-LE-BRINDE-TRUE
                    PERFORM LER-BRINDE
               WHEN GS-POPUP-BRINDE-TRUE
                    PERFORM POPUP-BRINDE
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
       LER-BRINDE SECTION.
           MOVE GS-CODIGO    TO CODIGO-CO02.
           READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02.
           MOVE NOME-CO02    TO GS-DESCRICAO.
       POPUP-BRINDE SECTION.
           CALL   "COP002T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "COP002T".
           MOVE PASSAR-STRING-1(1: 20)  TO GS-DESCRICAO.
           MOVE PASSAR-STRING-1(33: 3)  TO GS-CODIGO.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.

           IF ST-WORK1 NOT = "35" CLOSE WORK1   DELETE FILE WORK1.
           ACCEPT VARIA-W1 FROM TIME.
           OPEN OUTPUT WORK1.  CLOSE WORK1.  OPEN I-O WORK1.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO VLR-TOTAL-CAMPANHA.

           MOVE GS-VENCTO-INI  TO DATA-INV DATA-VENCTO-INI
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO VENCTO-INI
           MOVE GS-VENCTO-FIM  TO DATA-INV DATA-VENCTO-FIM
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO VENCTO-FIM


           IF GS-ACP-OPCAO = 1
              MOVE VENCTO-INI     TO DATA-VENCTO-CO50
              START COD050 KEY IS NOT < DATA-VENCTO-CO50 INVALID KEY
                    MOVE "10" TO ST-COD050
              END-START
           ELSE
              MOVE VENCTO-INI     TO DATA-PAGTO-CO50
              START COD050 KEY IS NOT < DATA-PAGTO-CO50 INVALID KEY
                    MOVE "10" TO ST-COD050
              END-START
           END-IF

           PERFORM UNTIL ST-COD050 = "10"
               READ COD050 NEXT RECORD AT END
                    MOVE "10" TO ST-COD050
               NOT AT END
                    IF SUSP-PREV-DEF-CO50 <> 2
                       MOVE DATA-VENCTO-CO50 TO GS-EXIBE-VENCTO
                       MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       IF (GS-ACP-OPCAO = 1 AND
                           DATA-VENCTO-CO50 > VENCTO-FIM) OR
                          (GS-ACP-OPCAO = 2 AND
                           DATA-PAGTO-CO50 > VENCTO-FIM)
                           MOVE "10" TO ST-COD050
                       ELSE
                          IF REALIZADO-CO50 = 1
                             PERFORM MOVER-DADOS-WORK
                          ELSE
                             CONTINUE
                          END-IF
                       END-IF
                    END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           MOVE NR-CONTRATO-CO50 TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE 0 TO STATUS-CO40.
           IF STATUS-CO40 < 50 CONTINUE
           ELSE
              MOVE ITEM-CO50          TO ITEM-WK
              MOVE NR-CONTRATO-CO50   TO CONTRATO-WK
              MOVE DATA-VENCTO-CO50   TO DATA-VENCTO-WK
              MOVE CODBRINDE-CO50     TO BRINDE-WK CODIGO-CO02
              READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02
              END-READ
              MOVE NOME-CO02          TO DESC-BRINDE-WK
              IF MULT-FORM-CO02 = 2
                 MOVE QTDE-POR-FORM-CO50 TO QTDE-BRINDE
              ELSE COMPUTE QTDE-BRINDE = QTDE-POR-FORM-CO50 *
                            QTDE-FORM-CO50
              END-IF
              MOVE MESANO-PREV-CO40   TO MESANO-I
              MOVE MESANO-I(1: 4)     TO MESANO-W(3: 4)
              MOVE MESANO-I(5: 2)     TO MESANO-W(1: 2)
              MOVE MESANO-W           TO MESANO-WK
              MOVE CIDADE-CO40        TO CIDADE
              READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
              END-READ
              MOVE NOME-CID           TO CIDADE-WK
              IF CUSTO-UNIT-CO50 <> ZEROS
                 MOVE CUSTO-UNIT-CO50 TO CUSTO-PREVISTO
              ELSE MOVE VALOR-CO02 TO CUSTO-PREVISTO
              END-IF
              COMPUTE VLR-TOTAL-WK = CUSTO-PREVISTO * QTDE-BRINDE
              MOVE IDENTIFICACAO-CO40 TO IDENTIFICACAO-WK

              WRITE REG-WORK
              END-WRITE
              PERFORM GRAVA-RESUMO
           END-IF.
       GRAVA-RESUMO SECTION.
           MOVE ZEROS            TO DATA-INI-CO04.
           MOVE MESANO-PREV-CO40 TO CAMPANHA-W(1: 6)
           MOVE 01               TO CAMPANHA-W(7: 2)
           START COD004 KEY IS NOT < DATA-INI-CO04 INVALID KEY
                 MOVE "10" TO ST-COD004.
           PERFORM UNTIL ST-COD004 = "10"
             READ COD004 NEXT RECORD AT END MOVE "10" TO ST-COD004
               NOT AT END
                 IF CAMPANHA-W NOT < DATA-INI-CO04 AND
                    CAMPANHA-W NOT > DATA-FIM-CO04
                    MOVE CODIGO-CO04    TO CAMPANHA-WK1
                    READ WORK1 INVALID KEY
                         MOVE VLR-TOTAL-WK  TO VALOR-WK1
                         WRITE REG-WORK1
                         END-WRITE
                      NOT INVALID KEY
                         ADD VLR-TOTAL-WK   TO VALOR-WK1
                         REWRITE REG-WORK1
                         END-REWRITE
                    END-READ
                    ADD VLR-TOTAL-WK   TO VLR-TOTAL-CAMPANHA
                    MOVE "10" TO ST-COD004
                 ELSE CONTINUE
             END-READ
           END-PERFORM.
      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           INITIALIZE REG-WORK.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA

      *    TOTALIZA GERAL
           MOVE SPACES         TO GS-LINDET
           MOVE "Total Geral..." TO GS-LINDET(1: 30)
           MOVE VLR-TOTAL-TOTG TO VALOR-E
           MOVE VALOR-E        TO GS-LINDET(90: 16)
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM CARREGAR-RESUMO.
       CARREGAR-RESUMO SECTION.
           MOVE "CLEAR-LIST-BOX2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO GS-LINDET1.
           INITIALIZE REG-WORK1.
      *    MOVE "REFRESH-DATA" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
           START WORK1 KEY IS NOT < CAMPANHA-WK1 INVALID KEY
                 MOVE "10" TO ST-WORK1.
           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END MOVE "10" TO ST-WORK1
              NOT AT END
                PERFORM MOVER-DADOS-RESUMO
              END-READ
           END-PERFORM.

      *    TOTALIZA GERAL
           MOVE SPACES             TO GS-LINDET1
           MOVE "INSERE-LIST2"     TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "Total Geral..."   TO GS-LINDET1(1: 30)
           MOVE VLR-TOTAL-CAMPANHA TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET1(28: 20)
           MOVE 100                TO PERC-E
           MOVE PERC-E             TO GS-LINDET1(48: 6)
           MOVE "INSERE-LIST2"     TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-RESUMO SECTION.
           MOVE CAMPANHA-WK1   TO CODIGO-CO04.
           READ COD004 INVALID KEY INITIALIZE REG-COD004.
           MOVE DATA-INI-CO04  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV       TO DATA-E.
           MOVE DATA-E         TO GS-LINDET1(1: 12)
           MOVE "a"            TO GS-LINDET1(13: 3)
           MOVE DATA-FIM-CO04  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV       TO DATA-E.
           MOVE DATA-E         TO GS-LINDET1(16: 12)
           MOVE VALOR-WK1      TO VALOR-E
           MOVE VALOR-E        TO GS-LINDET1(28: 20)
           COMPUTE PERC-W = (VALOR-WK1 / VLR-TOTAL-CAMPANHA) * 100.
           MOVE PERC-W         TO PERC-E
           MOVE PERC-E         TO GS-LINDET1(48: 6).
           MOVE "INSERE-LIST2"     TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "COMP.CONTRAT" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DESC-BRINDE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "DATA-VENCTO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DATA-VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF DESC-BRINDE-ANT NOT = SPACES
                 IF DESC-BRINDE-ANT NOT = DESC-BRINDE-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF DATA-VENCTO-ANT  NOT = ZEROS
                 IF DATA-VENCTO-ANT NOT = DATA-VENCTO-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
      *    INSERIR LINHA NA LIST BOX
           MOVE CONTRATO-WK       TO GS-LINDET(1: 5)
           MOVE IDENTIFICACAO-WK  TO GS-LINDET(6: 22)
           MOVE CIDADE-WK         TO GS-LINDET(28: 15)
           MOVE MESANO-WK         TO MESANO-E
           MOVE MESANO-E          TO GS-LINDET(43: 8)
           MOVE ITEM-WK           TO GS-LINDET(51: 5)
           MOVE DESC-BRINDE-WK    TO GS-LINDET(56: 21)
           MOVE DATA-VENCTO-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(77: 12)
           MOVE VLR-TOTAL-WK      TO VALOR-E
           ADD VLR-TOTAL-WK       TO VLR-TOTAL-TOT
           MOVE VALOR-E           TO GS-LINDET(90: 16)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-CHAVE-ANT SECTION.
           MOVE DESC-BRINDE-WK    TO DESC-BRINDE-ANT.
           MOVE DATA-VENCTO-WK    TO DATA-VENCTO-ANT.
           MOVE CONTRATO-WK       TO CONTRATO-ANT.
       TOTALIZA SECTION.
           MOVE SPACES        TO GS-LINDET
           MOVE "Total..."    TO GS-LINDET(1: 30)
           MOVE VLR-TOTAL-TOT TO VALOR-E
           MOVE VALOR-E       TO GS-LINDET(90: 16)
           ADD VLR-TOTAL-TOT  TO VLR-TOTAL-TOTG
           MOVE ZEROS TO VLR-TOTAL-TOT
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO VLR-TOTAL-TOT.

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO VLR-TOTAL-TOT VLR-TOTAL-TOTG.
           MOVE SPACES TO DESC-BRINDE-ANT.
           MOVE ZEROS TO DATA-VENCTO-ANT CONTRATO-ANT.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP152" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM IMPRIME-RESUMO.
           INITIALIZE REG-WORK.
           MOVE SPACES TO DESC-BRINDE-ANT.
           MOVE ZEROS TO DATA-VENCTO-ANT CONTRATO-ANT.
           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

      *    TOTALIZA GERAL
           MOVE SPACES           TO LINDET-REL
           MOVE "Total Geral..." TO LINDET-REL(1: 30)
           MOVE VLR-TOTAL-TOTG   TO VALOR-E
           MOVE VALOR-E          TO LINDET-REL(90: 16)
           WRITE REG-RELAT FROM LINDET

           COPY DESCONDENSA.

       IMPRIME-RESUMO SECTION.
           PERFORM CABECALHO-RESUMO.
           MOVE SPACES TO LINDET-REL.
           INITIALIZE REG-WORK1.
           START WORK1 KEY IS NOT < CAMPANHA-WK1 INVALID KEY
                 MOVE "10" TO ST-WORK1.
           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END MOVE "10" TO ST-WORK1
              NOT AT END
                PERFORM IMPRIMIR-DADOS-RESUMO
              END-READ
           END-PERFORM.

      *    TOTALIZA GERAL

           MOVE SPACES             TO REG-RELAT.
           WRITE REG-RELAT.

           MOVE "Total Geral..."   TO LINDET-REL(1: 30)
           MOVE VLR-TOTAL-CAMPANHA TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(28: 20)
           MOVE 100                TO PERC-E
           MOVE PERC-E             TO LINDET-REL(48: 6)
           WRITE REG-RELAT FROM LINDET.
           MOVE 55 TO LIN.
       IMPRIMIR-DADOS-RESUMO SECTION.
           MOVE CAMPANHA-WK1   TO CODIGO-CO04.
           READ COD004 INVALID KEY INITIALIZE REG-COD004.
           MOVE DATA-INI-CO04  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV       TO DATA-E.
           MOVE DATA-E         TO LINDET-REL(1: 12)
           MOVE "a"            TO LINDET-REL(13: 3)
           MOVE DATA-FIM-CO04  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV       TO DATA-E.
           MOVE DATA-E         TO LINDET-REL(16: 12)
           MOVE VALOR-WK1      TO VALOR-E
           MOVE VALOR-E        TO LINDET-REL(28: 20)
           COMPUTE PERC-W = (VALOR-WK1 / VLR-TOTAL-CAMPANHA) * 100.
           MOVE PERC-W         TO PERC-E
           MOVE PERC-E         TO LINDET-REL(48: 6).
           WRITE REG-RELAT FROM LINDET.


       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF DESC-BRINDE-ANT NOT = SPACES
                 IF DESC-BRINDE-ANT NOT = DESC-BRINDE-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF DATA-VENCTO-ANT  NOT = ZEROS
                 IF DATA-VENCTO-ANT NOT = DATA-VENCTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
      *    IMPRIMIR LINHA DETALHE
           MOVE CONTRATO-WK       TO LINDET-REL(1: 5)
           MOVE IDENTIFICACAO-WK  TO LINDET-REL(6: 22)
           MOVE CIDADE-WK         TO LINDET-REL(28: 15)
           MOVE MESANO-WK         TO MESANO-E
           MOVE MESANO-E          TO LINDET-REL(43: 8)
           MOVE ITEM-WK           TO LINDET-REL(51: 5)
           MOVE DESC-BRINDE-WK    TO LINDET-REL(56: 21)
           MOVE DATA-VENCTO-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(77: 12)
           MOVE VLR-TOTAL-WK      TO VALOR-E
           ADD VLR-TOTAL-WK       TO VLR-TOTAL-TOT
           MOVE VALOR-E           TO LINDET-REL(90: 16)

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 55 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES        TO LINDET-REL
           MOVE "Total..."    TO LINDET-REL(1: 30)
           MOVE VLR-TOTAL-TOT TO VALOR-E
           MOVE VALOR-E       TO LINDET-REL(90: 16)
           ADD VLR-TOTAL-TOT  TO VLR-TOTAL-TOTG
           MOVE ZEROS TO VLR-TOTAL-TOT
           WRITE REG-RELAT FROM LINDET

           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT
           ADD 2 TO LIN.
           IF LIN > 55 PERFORM CABECALHO.

       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM     TO ORDEM-REL
           IF GS-ACP-OPCAO = 1
              MOVE "INT.VENCTO: "  TO TIPO-DATA-REL
           ELSE
              MOVE "INT.PAGTO.: "  TO TIPO-DATA-REL
           END-IF
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CABECALHO-RESUMO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04A.
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
           CLOSE COD040 COD050 CAD010 COD002 COD004.
           CLOSE WORK WORK1.
           DELETE FILE WORK WORK1.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
