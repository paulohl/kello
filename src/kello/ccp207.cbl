       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP207.
      *DATA: 17/07/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RELATORIO ANALISTICO DO CONTA CORRENTE
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CCPX100.
           COPY CCPX011.
           COPY CGPX001.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = FORNEC-WK SEQ-WK
                  ALTERNATE RECORD KEY IS ALT1-WK = NOME-WK MOVTO-WK
                            WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CCPW011.
       COPY CGPW001.
       COPY CCPW100.
       FD  WORK.
       01  REG-WORK.
           05  FORNEC-WK           PIC 9(6).
           05  SEQ-WK              PIC 9(5).
           05  NOME-WK             PIC X(30).
           05  RESP-WK             PIC X(5).
           05  HISTORICO-WK        PIC X(30).
           05  MOVTO-WK            PIC 9(8).
           05  VALOR-WK            PIC 9(8)V99.
           05  DEB-CRED-WK         PIC X.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP207.CPB".
           COPY "CCP207.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CCD011             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  LIN                   pic 9(02).
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  ANOMES-ANT            PIC 9(6)     VALUE ZEROS.
           05  MESANOI.
               10  ANO-II            PIC 9(4).
               10  MES-II            PIC 9(2).
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALORS-E              PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FORNEC-ANT            PIC 9(6)     VALUE ZEROS.
           05  NOME-ANT              PIC X(30)    VALUE SPACES.
           05  SALDO-ANTERIOR        PIC S9(8)V99 VALUE ZEROS.
           05  TOTAL-W               PIC S9(8)V99  VALUE ZEROS.
           05  TOTAL-GERAL           PIC S9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
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
           05  FILLER              PIC X(41)   VALUE
           "RELATORIO ANALITICO DO CTA.CORR.-ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "NOME                           HISTORICO
      -    "  DATA-VECTO         VALOR T         TOTAL RESP. ".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CCD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD011.
           MOVE "CCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           OPEN INPUT CCD011 CCD100 CGD001.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD011 <> "00"
              MOVE "ERRO ABERTURA CCD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-VENCTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-ACHAR-VENCTO-TRUE
                   PERFORM ACHAR-VENCTO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       ACHAR-VENCTO SECTION.
           MOVE DATA-DIA-I     TO GRTIME-DATE.
           MOVE 2              TO GRTIME-TYPE.
           MOVE 1              TO GRTIME-FUNCTION.
           MOVE 7              TO GRTIME-DAYS.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           MOVE 01010001       TO GS-VENCTO-INI.
           CALL "GRIDAT1" USING GRTIME-DATE-FINAL.
           MOVE GRTIME-DATE-FINAL TO GS-VENCTO-FIM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-VENCTO-INI TO DATA-INV
                                     VENCTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VENCTO-INI.
           MOVE GS-VENCTO-FIM TO DATA-INV
                                     VENCTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VENCTO-FIM.

      *    ANOMES-ANT P/ CALCULO SALDO ANTERIOR
           MOVE VENCTO-INI(1: 6) TO MESANO-I.
           SUBTRACT 1 FROM MES-II
           IF MES-II = ZEROS
              MOVE 12  TO MES-II
              SUBTRACT 1 FROM ANO-II
           END-IF.
           MOVE MESANO-I         TO ANOMES-ANT.

           MOVE VENCTO-INI TO DATA-MOVTO-CC100
           START CCD100 KEY IS NOT < DATA-MOVTO-CC100 INVALID KEY
                  MOVE "10" TO ST-CCD100.
           PERFORM UNTIL ST-CCD100 = "10"
             READ CCD100 NEXT RECORD AT END MOVE "10" TO ST-CCD100
              NOT AT END
               IF SITUACAO-CC100 > 1 CONTINUE
               ELSE
                IF DATA-MOVTO-CC100 < VENCTO-INI OR
                   DATA-MOVTO-CC100 > VENCTO-FIM CONTINUE
                ELSE
                  PERFORM MOVER-DADOS-WORK
                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  WRITE REG-WORK
                END-IF
               END-IF
             END-READ
           END-PERFORM.
           PERFORM CALCULA-SALDO-ANTERIOR.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CALCULA-SALDO-ANTERIOR SECTION.
           MOVE ZEROS TO FORNEC-WK SEQ-WK FORNEC-ANT.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF FORNEC-WK <> FORNEC-ANT

                   PERFORM CALCULA-SALDO-FORNEC
                   MOVE FORNEC-WK     TO FORNEC-ANT
                   INITIALIZE REG-WORK
                   MOVE FORNEC-ANT    TO FORNEC-WK CODIGO-CG01
                   READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01      TO NOME-WK
                   MOVE "SALDO ANTERIOR" TO HISTORICO-WK
                   MOVE ZEROS          TO SEQ-WK
                   MOVE SALDO-ANTERIOR TO VALOR-WK
                   IF SALDO-ANTERIOR > 0
                      MOVE "C"         TO DEB-CRED-WK
                   ELSE MOVE "D"       TO DEB-CRED-WK
                   END-IF
                   WRITE REG-WORK
                ELSE CONTINUE
                END-IF
             END-READ
           END-PERFORM.
       CALCULA-SALDO-FORNEC SECTION.
           MOVE ZEROS        TO ANOMES-MVTO-CC11.
           MOVE FORNEC-WK    TO FORNEC-CC11
           MOVE ZEROS        TO SALDO-ANTERIOR
           START CCD011 KEY IS NOT < ALT-CC11 INVALID KEY
                 MOVE "10" TO ST-CCD011.
           PERFORM UNTIL ST-CCD011 = "10"
            READ CCD011 NEXT RECORD AT END MOVE "10" TO ST-CCD011
             NOT AT END
              MOVE ANOMES-MVTO-CC11 TO GS-EXIBE-VENCTO
              MOVE "TELA-AGUARDA1"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM

              IF ANOMES-MVTO-CC11 > ANOMES-ANT OR
                 FORNEC-CC11 <> FORNEC-WK
                   MOVE "10" TO ST-CCD011
              ELSE
               ADD SALDOS-CC11      TO SALDO-ANTERIOR
               SUBTRACT SALDOE-CC11 FROM SALDO-ANTERIOR
              END-IF
            END-READ
           END-PERFORM.

           MOVE VENCTO-INI   TO DATA-MOVTO-CC100
           MOVE 01           TO DATA-MOVTO-CC100(7: 2)
           START CCD100 KEY IS NOT < DATA-MOVTO-CC100 INVALID KEY
                 MOVE "10" TO ST-CCD100.
           PERFORM UNTIL ST-CCD100 = "10"
             READ CCD100 NEXT RECORD AT END MOVE "10" TO ST-CCD100
               NOT AT END
                  MOVE DATA-MOVTO-CC100 TO GS-EXIBE-VENCTO
                  MOVE "TELA-AGUARDA1"  TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM

                  IF DATA-MOVTO-CC100 NOT < VENCTO-INI
                     MOVE "10" TO ST-CCD100
                  ELSE
                     IF FORNEC-CC100 <> FORNEC-WK OR SITUACAO-CC100 > 1
                        CONTINUE
                     ELSE
                       EVALUATE CRED-DEB-CC100
                         WHEN 0 ADD VALOR-CC100 TO SALDO-ANTERIOR
                         WHEN 1 SUBTRACT VALOR-CC100 FROM SALDO-ANTERIOR
                       END-EVALUATE
                     END-IF
                  END-IF
             END-READ
           END-PERFORM.

       MOVER-DADOS-WORK SECTION.
           MOVE DATA-MOVTO-CC100     TO MOVTO-WK
                                       GS-EXIBE-VENCTO
           MOVE FORNEC-CC100         TO FORNEC-WK CODIGO-CG01
           MOVE SEQ-CC100            TO SEQ-WK
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01            TO NOME-WK.
           MOVE DESCRICAO-CC100      TO HISTORICO-WK.
      *    CONSIDERA AO CONTRARIO, POIS É EXTRATO DO CLIENTE
           IF CRED-DEB-CC100 = 0
              MOVE "C" TO DEB-CRED-WK
           ELSE MOVE "D" TO DEB-CRED-WK.
           MOVE VALOR-CC100          TO VALOR-WK
           MOVE RESPONSAVEL-CC100    TO RESP-WK.
      *-----------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO TOTAL-GERAL.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
                 NOT AT END
                    PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "TOTAL GERAL" TO GS-LINDET
           MOVE TOTAL-GERAL   TO VALORS-E
           MOVE VALORS-E      TO GS-LINDET(90: 15)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "NOME/DATA-MOVTO " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                 IF NOME-ANT NOT = NOME-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE NOME-WK           TO GS-LINDET(01: 31)
           MOVE HISTORICO-WK      TO GS-LINDET(32: 31)
           MOVE MOVTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(63: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(74: 14)
           MOVE DEB-CRED-WK       TO GS-LINDET(88: 2)
           IF DEB-CRED-WK = "C"
              ADD VALOR-WK        TO TOTAL-W TOTAL-GERAL
           ELSE SUBTRACT VALOR-WK FROM TOTAL-W TOTAL-GERAL
           END-IF
           MOVE TOTAL-W           TO VALORS-E
           MOVE VALORS-E          TO GS-LINDET(90: 15).
           MOVE RESP-WK           TO GS-LINDET(105: 6).

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO TOTAL-W TOTAL-GERAL
           MOVE SPACES TO NOME-ANT.
       MOVER-CHAVE-ANT SECTION.
           MOVE NOME-WK           TO NOME-ANT.
       TOTALIZA SECTION.
           MOVE ZEROS TO TOTAL-W.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP207" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *---------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W TOTAL-GERAL.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
                NOT AT END
                  PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           MOVE SPACES            TO LINDET-REL
           MOVE "TOTAL GERAL..."  TO LINDET-REL
           MOVE TOTAL-GERAL       TO VALORS-E
           MOVE VALORS-E          TO LINDET-REL(90: 15)
           WRITE REG-RELAT FROM LINDET AFTER 2.

           COPY DESCONDENSA.
       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
               IF NOME-ANT NOT = SPACES
                 IF NOME-ANT NOT = NOME-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           MOVE NOME-WK           TO LINDET-REL(01: 31)
           MOVE HISTORICO-WK      TO LINDET-REL(32: 31)
           MOVE MOVTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(63: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(74: 14)
           MOVE DEB-CRED-WK       TO LINDET-REL(88: 2)
           IF DEB-CRED-WK = "C"
              ADD VALOR-WK        TO TOTAL-W TOTAL-GERAL
           ELSE SUBTRACT VALOR-WK FROM TOTAL-W TOTAL-GERAL
           END-IF
           MOVE TOTAL-W           TO VALORS-E
           MOVE VALORS-E          TO LINDET-REL(90: 14).
           MOVE RESP-WK           TO LINDET-REL(32: 6)


           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE ZEROS TO TOTAL-W.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
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
           CLOSE CCD011 CCD100 CGD001 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
