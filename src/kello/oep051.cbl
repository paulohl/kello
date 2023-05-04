       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OEP051.
      *DATA-WRITTEN. 17/01/2000.
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RESUMO DE ARRECADAÇÃO DE ORGANIZAÇÃO DE EVENTO -GERAL
      *FUNÇÃO: Listar todos os títulos que pertencerem ao contrato sele-
      *        cionado, e tipo-docto-cr20 = 2(org.evento).
      *        As ordens serão: CONTRATO, IDENTIF, CIDADE.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX040.
           COPY OEPX010.
           COPY CAPX010.
           COPY CRPX020.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CONTRATO-WK
                  ALTERNATE RECORD KEY IS IDENTIF-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY COPW040.
       COPY OEPW010.
       COPY CAPW010.
       COPY CRPW020.
       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  IDENTIF-WK          PIC X(11).
           05  CIDADE-WK           PIC X(10).
           05  DATA-ULT-EV-WK      PIC 9(8).
           05  VLR-TOTAL-WK        PIC 9(8)V99.
      *    05  PARC-TOTAL-WK       PIC 9(2).
           05  VLR-PAGO-WK         PIC 9(8)V99.
      *    05  PARC-PAGO-WK        PIC 9(2).
           05  VLR-ATRASADO-WK     PIC 9(8)V99.
      *    05  PARC-ATRASADO-WK    PIC 9(2).
           05  VLR-RESTANTE-WK     PIC 9(8)V99.
      *    05  PARC-RESTANTE-WK    PIC 9(2).
           05  JUROS-WK            PIC 9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "OEP051.CPB".
           COPY "OEP051.CPY".
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
           05  ST-OED010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
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
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VLR-GERAL-TOT         PIC 9(8)V99  VALUE ZEROS.
           05  PARC-GERAL-TOT        PIC 9(4)     VALUE ZEROS.
           05  VLR-PAGO-TOT          PIC 9(8)V99  VALUE ZEROS.
           05  PARC-PAGO-TOT         PIC 9(4)     VALUE ZEROS.
           05  VLR-ATRASADO-TOT      PIC 9(8)V99  VALUE ZEROS.
           05  PARC-ATRASADO-TOT     PIC 9(4)     VALUE ZEROS.
           05  VLR-RESTANTE-TOT      PIC 9(8)V99  VALUE ZEROS.
           05  PARC-RESTANTE-TOT     PIC 9(4)     VALUE ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  CLIENTE-W             PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(30)    VALUE SPACES.

      *    VARIAVEIS P/ O CALCULO DE JUROS
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  CUSTO-PREVISTO        PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-W               PIC 9(8)V99  VALUE ZEROS.
           05  MESES-W               PIC 9(3)     VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(60)   VALUE
           "RESUMO DE ARRECADACAO DE ORG-EVENTOS (GERAL)    - ORDEM: ".
           05  ORDEM-REL           PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(6)    VALUE "TIPO: ".
           05  TIPO-REL            PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE "TAXA:     ".
           05  TAXA-REL            PIC 9(4)    VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "CONT IDENTIFICAC CIDADE     DATA-ULT.       RESTANTE
      -    "  PAGO      ATRASADO         TOTAL         JUROS".
       01  LINDET.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Geral...: ".
           05  TOTAL-GERAL-REL     PIC x(20)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Atrasado: ".
           05  TOTAL-ATRASADO-REL  PIC X(20)   VALUE SPACES.
       01  LINTOT1.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Recebido: ".
           05  TOTAL-PAGO-REL      PIC x(20)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total a Receb.: ".
           05  TOTAL-RESTANTE-REL  PIC X(20)   VALUE SPACES.

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
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "OED010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-OED010.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           OPEN INPUT CAD010 COD040 OED010 CRD020.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-OED010 <> "00"
              MOVE "ERRO ABERTURA OED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W
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
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-VENCTO-ANT
                    PERFORM CRIA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
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
       CRIA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-OED010
           MOVE ZEROS TO NR-CONTRATO-OE10
           START OED010 KEY IS NOT < NR-CONTRATO-OE10 INVALID KEY
                 MOVE "10" TO ST-OED010.

           PERFORM UNTIL ST-OED010 = "10"
                 READ OED010 NEXT RECORD AT END
                      MOVE "10" TO ST-OED010
                 NOT AT END
                      PERFORM LEITURA-RECEBER
                 END-READ
           END-PERFORM

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LEITURA-RECEBER SECTION.
           INITIALIZE REG-CRD020
           MOVE 0                TO COD-COMPL-CR20(1: 1)
           MOVE NR-CONTRATO-OE10 TO COD-COMPL-CR20(2: 4)
           MOVE ZEROS            TO COD-COMPL-CR20(6: 4)
                                    SEQ-CR20
           START CRD020 KEY IS NOT < CHAVE-CR20 INVALID KEY
                  MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                  READ CRD020 NEXT RECORD AT END
                       MOVE "10" TO ST-CRD020
                  NOT AT END
                       MOVE COD-COMPL-CR20(2: 4) TO CONTRATO-W
                       IF CONTRATO-W <> NR-CONTRATO-OE10
                          MOVE "10" TO ST-CRD020
                       ELSE
                          IF SITUACAO-CR20 > 2
                             CONTINUE
                          ELSE
                             PERFORM CONT-GRAVA-WORK
                          END-IF
                       END-IF
                  END-READ
           END-PERFORM.
       CONT-GRAVA-WORK SECTION.
           IF TIPO-DOCTO-CR20 <> 2
              CONTINUE
           ELSE
             MOVE CONTRATO-W   TO CONTRATO-WK
                                  GS-EXIBE-VENCTO
             READ WORK INVALID KEY
                  INITIALIZE REG-WORK
                  PERFORM GRAVA-WORK
             NOT INVALID KEY
                  PERFORM REGRAVA-WORK
             END-READ
             MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
             PERFORM CALL-DIALOG-SYSTEM
           END-IF.

       GRAVA-WORK SECTION.
           MOVE CONTRATO-W              TO CONTRATO-WK
                                           NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                INITIALIZE REG-COD040.

           MOVE DATA-ULT-EVENTO-OE10    TO DATA-ULT-EV-WK
           MOVE IDENTIFICACAO-CO40      TO IDENTIF-WK.
           MOVE CIDADE-CO40             TO CIDADE.
           READ CAD010 INVALID KEY
                MOVE SPACES             TO NOME-CID.

           MOVE NOME-CID                TO CIDADE-WK.

           MOVE VALOR-TOT-CR20          TO VLR-TOTAL-WK.
           IF DATA-RCTO-CR20 <> ZEROS
              MOVE VALOR-LIQ-CR20       TO VLR-PAGO-WK
              MOVE ZEROS                TO VLR-ATRASADO-WK
                                           VLR-RESTANTE-WK
           ELSE
              IF DATA-VENCTO-CR20 < DATA-DIA-I
                 MOVE VALOR-TOT-CR20    TO VLR-ATRASADO-WK
                 MOVE ZEROS             TO VLR-PAGO-WK
                                           VLR-RESTANTE-WK
              ELSE
                 MOVE VALOR-TOT-CR20    TO VLR-RESTANTE-WK
                 MOVE ZEROS             TO VLR-PAGO-WK
                                           VLR-ATRASADO-WK
              END-IF
           END-IF
           PERFORM CALCULO-JUROS
           MOVE JUROS-W                 TO JUROS-WK.
           WRITE REG-WORK.
       REGRAVA-WORK SECTION.
           ADD VALOR-TOT-CR20           TO VLR-TOTAL-WK.
           IF DATA-RCTO-CR20 <> ZEROS
              ADD VALOR-LIQ-CR20        TO VLR-PAGO-WK
           ELSE
              IF DATA-VENCTO-CR20 < DATA-DIA-I
                 ADD VALOR-TOT-CR20     TO VLR-ATRASADO-WK
              ELSE
                 ADD VALOR-TOT-CR20     TO VLR-RESTANTE-WK
              END-IF
           END-IF
           ADD JUROS-W              TO JUROS-WK.
           REWRITE REG-WORK.
       CALCULO-JUROS SECTION.
           IF DATA-RCTO-CR20 <> ZEROS
              MOVE DATA-RCTO-CR20 TO GRDIAS-AAMMDD-INICIAL
              MOVE VALOR-LIQ-CR20 TO CUSTO-PREVISTO
           ELSE
              MOVE DATA-VENCTO-CR20 TO GRDIAS-AAMMDD-INICIAL
              MOVE VALOR-TOT-CR20   TO CUSTO-PREVISTO.

           MOVE DATA-ULT-EVENTO-OE10  TO GRDIAS-AAMMDD-FINAL.
           CALL "GRDIAS1" USING PARAMETROS-GRDIAS
           CANCEL "GRDIAS1".
           COMPUTE MESES-W = GRDIAS-NUM-DIAS / 30.
           COMPUTE PRAZO-MEDIO ROUNDED = (CUSTO-PREVISTO * MESES-W) /
                                  CUSTO-PREVISTO.
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-PREVISTO * (TAXA-ACUMULADA - 1).

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO VLR-GERAL-TOT PARC-GERAL-TOT
                         VLR-PAGO-TOT PARC-PAGO-TOT
                         VLR-ATRASADO-TOT PARC-ATRASADO-TOT
                         VLR-RESTANTE-TOT PARC-RESTANTE-TOT.
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    IF GS-ORDEM-RADIO = 2 AND
                       VLR-ATRASADO-WK = ZEROS
                       CONTINUE
                    ELSE
                       PERFORM MOVER-DADOS-LINDET
                    END-IF
               END-READ
           END-PERFORM.
           PERFORM TOTALIZA.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO CONTRATO-WK
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "IDENTIF." TO GS-DESCR-ORDEM
                MOVE SPACES TO IDENTIF-WK
                START WORK KEY IS NOT < IDENTIF-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CIDADE " TO GS-DESCR-ORDEM
                MOVE SPACES    TO CIDADE-WK
                START WORK KEY IS NOT < CIDADE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           MOVE CONTRATO-WK       TO GS-LINDET(1: 5)
           MOVE IDENTIF-WK        TO GS-LINDET(06: 12)
           MOVE CIDADE-WK         TO GS-LINDET(18: 11)
           MOVE DATA-ULT-EV-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(29: 11)
           MOVE VLR-PAGO-WK       TO VALOR-E
           ADD VLR-PAGO-WK        TO VLR-PAGO-TOT
           MOVE VALOR-E           TO GS-LINDET(40: 14)
           MOVE VLR-ATRASADO-WK   TO VALOR-E
           ADD VLR-ATRASADO-WK    TO VLR-ATRASADO-TOT
           MOVE VALOR-E           TO GS-LINDET(54: 14)
           MOVE VLR-RESTANTE-WK   TO VALOR-E
           ADD VLR-RESTANTE-WK    TO VLR-RESTANTE-TOT
           MOVE VALOR-E           TO GS-LINDET(68: 14)
           MOVE VLR-TOTAL-WK      TO VALOR-E
           ADD VLR-TOTAL-WK       TO VLR-GERAL-TOT
           MOVE VALOR-E           TO GS-LINDET(82: 14)
           MOVE JUROS-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(96: 13)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA SECTION.
           MOVE VLR-GERAL-TOT     TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-GERAL(1: 13)
           MOVE VLR-PAGO-TOT      TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-PAGO(1: 13)
           MOVE VLR-ATRASADO-TOT  TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-ATRASADO(1: 13)
           MOVE VLR-RESTANTE-TOT  TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-RESTANTE(1: 13).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "OEP051" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM
           MOVE ZEROS  TO LIN
           PERFORM CABECALHO
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   IF GS-ORDEM-RADIO  = 2 AND
                      VLR-ATRASADO-WK = ZEROS
                      CONTINUE
                   ELSE
                      PERFORM MOVER-DADOS-RELATORIO
                   END-IF
              END-READ
           END-PERFORM.
           MOVE GS-TOTAL-GERAL    TO TOTAL-GERAL-REL
           MOVE GS-TOTAL-PAGO     TO TOTAL-PAGO-REL
           MOVE GS-TOTAL-ATRASADO TO TOTAL-ATRASADO-REL
           MOVE GS-TOTAL-RESTANTE TO TOTAL-RESTANTE-REL
           WRITE REG-RELAT FROM LINTOT AFTER 2
           WRITE REG-RELAT FROM LINTOT1

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL.
           MOVE CONTRATO-WK       TO LINDET-REL(1: 5)
           MOVE IDENTIF-WK        TO LINDET-REL(06: 12)
           MOVE CIDADE-WK         TO LINDET-REL(18: 11)
           MOVE DATA-ULT-EV-WK    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(29: 11)
           MOVE VLR-PAGO-WK       TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(40: 14)
           MOVE VLR-ATRASADO-WK   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(54: 14)
           MOVE VLR-RESTANTE-WK   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(68: 14)
           MOVE VLR-TOTAL-WK      TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(82: 14)
           MOVE JUROS-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(96: 13)

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE ZEROS TO TOTAL-W.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           MOVE GS-TAXA TO TAXA-REL    .
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           IF GS-ORDEM-RADIO = 1 MOVE "GERAL" TO TIPO-REL
           ELSE MOVE "ATRASADO"  TO TIPO-REL.
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
           CLOSE CAD010 COD040 OED010 CRD020 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
