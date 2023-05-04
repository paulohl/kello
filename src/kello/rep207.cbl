       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP207.
      *DATA: 24/07/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELAÇAO DE CONFERÊNCIA DE REPORTAGEM - CONTABILIDADE
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY REPX100.
           COPY REPX101.
           COPY REPX103.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY REPW100.
       COPY REPW101.
       COPY REPW103.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(140).
       WORKING-STORAGE SECTION.
           COPY "REP207.CPB".
           COPY "REP207.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ST-RED103             PIC XX       VALUE SPACES.
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
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  COL1                  PIC 9(2)     VALUE ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  DESPESA-W             PIC 9(8)V99  VALUE ZEROS.
           05  DESCRICAO-W           PIC X(20)    VALUE SPACES.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
      *    GRAVA-W CONTROLA SE GRAVA P/ WORK - ORDEM CIDADE / REGIAO
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.

           05  TOT-RELATORIO         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-RELATORIO     PIC 9(8)V99  VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(40).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(66)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(72)   VALUE
           "RELACAO DE CONFERENCIA DE REPORTAGEM - CONTABILIDADE".
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-REPOR  CODIGO  FOTOGRAFO/CINEGRAFIS COMBUSTIV  HOSPEDAG
      -    "  REFEICAO  PASSAGEM   ALUGUEL  MATERIAL    OUTROS".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED101.
           MOVE "RED103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED103.

           OPEN INPUT CGD001 RED100 RED101 RED103.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
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


       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO TOT-RELATORIO TOT-GER-RELATORIO.

           MOVE GS-VECTO-INI TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV  TO VECTO-INI
           MOVE GS-VECTO-FIM TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV  TO VECTO-FIM
           MOVE VECTO-INI TO DATA-MOV-R100.
           START RED100 KEY IS NOT < DATA-MOV-R100 INVALID KEY
                 MOVE "10" TO ST-RED100.
           PERFORM UNTIL ST-RED100 = "10"
              READ RED100 NEXT RECORD AT END MOVE "10" TO ST-RED100
              NOT AT END
                IF DATA-MOV-R100 > VECTO-FIM MOVE "10" TO ST-RED100
                ELSE
                   PERFORM MOVER-DADOS-LINDET
                   PERFORM MOVER-VALOR-EQUIPE
                   MOVE SPACES TO GS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   MOVE "TOTAL RELATORIO: " TO GS-LINDET
                   MOVE TOT-RELATORIO       TO VALOR-E
                   ADD TOT-RELATORIO        TO TOT-GER-RELATORIO
                   MOVE ZEROS               TO TOT-RELATORIO
                   MOVE VALOR-E             TO GS-LINDET(96: 13)
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM

                   MOVE SPACES TO GS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE "TOTAL RELATORIO: " TO GS-LINDET
           MOVE TOT-GER-RELATORIO   TO VALOR-E
           MOVE ZEROS               TO TOT-GER-RELATORIO
           MOVE VALOR-E             TO GS-LINDET(96: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-LINDET SECTION.
           MOVE SPACES TO GS-LINDET
           MOVE DATA-MOV-R100     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(01: 11)
           MOVE "CONTR: "         TO GS-LINDET(12: 7)
           PERFORM MOVER-CONTRATOS
           MOVE DESCRICAO-W       TO GS-LINDET(19: 21)
           MOVE VLR-COMB-R100     TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(40: 10)
           MOVE VLR-COMB-R100     TO TOT-RELATORIO
           MOVE VLR-HOSP-R100     TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(50: 10)
           ADD VLR-HOSP-R100      TO TOT-RELATORIO
           MOVE VLR-REFEICAO-R100 TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(60: 10)
           ADD VLR-REFEICAO-R100  TO TOT-RELATORIO
           MOVE VLR-PASSAGEM-R100 TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(70: 10)
           ADD VLR-PASSAGEM-R100  TO TOT-RELATORIO
           MOVE VLR-ALUGUEL-R100  TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(80: 10)
           ADD VLR-ALUGUEL-R100   TO TOT-RELATORIO
           MOVE VLR-MAT-R100      TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(90: 10)
           ADD VLR-MAT-R100       TO TOT-RELATORIO
           MOVE VLR-OUTROS-R100   TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(100: 10)
           ADD VLR-OUTROS-R100    TO TOT-RELATORIO.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-CONTRATOS SECTION.
           MOVE DOCTO-R100        TO DOCTO-R101.
           MOVE ZEROS             TO CONTRATO-R101 EVENTO-R101
                                     CONTRATO-ANT.
           START RED101 KEY IS NOT < CHAVE-R101 INVALID KEY
              MOVE "10" TO ST-RED101.
           MOVE ZEROS TO COL1.
           MOVE SPACES TO DESCRICAO-W.
           PERFORM UNTIL ST-RED101 = "10"
             READ RED101 NEXT RECORD AT END MOVE "10" TO ST-RED101
               NOT AT END
                IF DOCTO-R101 <> DOCTO-R100 MOVE "10" TO ST-RED101
                ELSE
                 IF CONTRATO-R101 NOT = CONTRATO-ANT
                    MOVE CONTRATO-R101 TO CONTRATO-ANT
                    IF COL1 NOT < 20 MOVE "10" TO ST-RED101
                    ELSE
                        MOVE CONTRATO-R101 TO DESCRICAO-W(COL1: 4)
                        ADD 5 TO COL1
                 END-IF
                END-IF
             END-READ
           END-PERFORM.
       MOVER-VALOR-EQUIPE SECTION.
           MOVE DOCTO-R100     TO DOCTO-R103
           MOVE ZEROS          TO SEQ-R103
           MOVE SPACES         TO GS-LINDET
           START RED103 KEY IS NOT < ALT1-R103 INVALID KEY
                 MOVE "10" TO ST-RED103.
           PERFORM UNTIL ST-RED103 = "10"
             READ RED103 NEXT RECORD AT END MOVE "10" TO ST-RED103
               NOT AT END
                 IF DOCTO-R103 <> DOCTO-R100 MOVE "10" TO ST-RED103
                 ELSE
                   MOVE CODIGO-R103     TO GS-LINDET(12: 7) CODIGO-CG01
                   READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01       TO GS-LINDET(19: 21)
                   MOVE VLR-REPORT-R103 TO VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET(100: 10)
                   ADD VLR-REPORT-R103  TO TOT-RELATORIO
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                 END-IF
             END-READ
           END-PERFORM.

      *----------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP207" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS TO TOT-RELATORIO TOT-GER-RELATORIO

           MOVE VECTO-INI TO DATA-MOV-R100.
           START RED100 KEY IS NOT < DATA-MOV-R100 INVALID KEY
                 MOVE "10" TO ST-RED100.
           PERFORM UNTIL ST-RED100 = "10"
              READ RED100 NEXT RECORD AT END MOVE "10" TO ST-RED100
              NOT AT END
                IF DATA-MOV-R100 > VECTO-FIM MOVE "10" TO ST-RED100
                ELSE
                   PERFORM MOVER-DADOS-REL
                   PERFORM MOVER-VALOR-EQUIPE-REL
                   MOVE "TOTAL RELATORIO: " TO LINDET-REL
                   MOVE TOT-RELATORIO       TO VALOR-E
                   ADD TOT-RELATORIO        TO TOT-GER-RELATORIO
                   MOVE ZEROS               TO TOT-RELATORIO
                   MOVE VALOR-E             TO LINDET-REL(96: 13)
                   WRITE REG-RELAT FROM LINDET AFTER 2
                   MOVE SPACES TO REG-RELAT
                   WRITE REG-RELAT
                END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO LINDET-REL
           MOVE "TOTAL RELATORIO: " TO LINDET-REL
           MOVE TOT-GER-RELATORIO   TO VALOR-E
           MOVE ZEROS               TO TOT-GER-RELATORIO
           MOVE VALOR-E             TO LINDET-REL(96: 13)
           WRITE REG-RELAT FROM LINDET AFTER 2.

           copy descondensa.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 5 TO LIN.
       MOVER-DADOS-REL SECTION.
           MOVE SPACES TO LINDET-REL
           MOVE DATA-MOV-R100     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01: 11)
           MOVE "CONTR: "         TO LINDET-REL(12: 7)
           PERFORM MOVER-CONTRATOS
           MOVE DESCRICAO-W       TO LINDET-REL(19: 21)
           MOVE VLR-COMB-R100     TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(40: 10)
           MOVE VLR-COMB-R100     TO TOT-RELATORIO
           MOVE VLR-HOSP-R100     TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(50: 10)
           ADD VLR-HOSP-R100      TO TOT-RELATORIO
           MOVE VLR-REFEICAO-R100 TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(60: 10)
           ADD VLR-REFEICAO-R100  TO TOT-RELATORIO
           MOVE VLR-PASSAGEM-R100 TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(70: 10)
           ADD VLR-PASSAGEM-R100  TO TOT-RELATORIO
           MOVE VLR-ALUGUEL-R100  TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(80: 10)
           ADD VLR-ALUGUEL-R100   TO TOT-RELATORIO
           MOVE VLR-MAT-R100      TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(90: 10)
           ADD VLR-MAT-R100       TO TOT-RELATORIO
           MOVE VLR-OUTROS-R100   TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(100: 10)
           ADD VLR-OUTROS-R100    TO TOT-RELATORIO.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       MOVER-VALOR-EQUIPE-REL SECTION.
           MOVE DOCTO-R100     TO DOCTO-R103
           MOVE ZEROS          TO SEQ-R103
           MOVE SPACES         TO LINDET-REL
           START RED103 KEY IS NOT < ALT1-R103 INVALID KEY
                 MOVE "10" TO ST-RED103.
           PERFORM UNTIL ST-RED103 = "10"
             READ RED103 NEXT RECORD AT END MOVE "10" TO ST-RED103
               NOT AT END
                 IF DOCTO-R103 <> DOCTO-R100 MOVE "10" TO ST-RED103
                 ELSE
                   MOVE CODIGO-R103     TO LINDET-REL(12: 7) CODIGO-CG01
                   READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01       TO LINDET-REL(19: 21)
                   MOVE VLR-REPORT-R103 TO VALOR-E1
                   MOVE VALOR-E1        TO LINDET-REL(100: 10)
                   ADD VLR-REPORT-R103  TO TOT-RELATORIO
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.

      *------------------------------------------------------------
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 RED100 RED101 RED103.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
