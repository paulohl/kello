       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP202.
      *DATA: 03/02/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Relatório de Crédito Geral do Conta corrente
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CCPX010.
           COPY CCPX100.
           COPY CCPX105.
           COPY CCPX115.
           COPY CCPX110.
           COPY CCPX120.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = CODIGO-WK SEQ-WK
                  ALTERNATE RECORD KEY IS NOME-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CCPW010.
       COPY CCPW100.
       COPY CCPW105.
       COPY CCPW115.
       COPY CCPW110.
       COPY CCPW120.
       FD  WORK.
       01  REG-WORK.
           05  CODIGO-WK           PIC 9(6).
           05  SEQ-WK              PIC 9(5).
           05  NOME-WK             PIC X(30).
           05  SALARIO-FUNC-WK     PIC 9(7)V99.
           05  SALARIO-REPORT-WK   PIC 9(7)V99.
           05  SALARIO-REPRES-WK   PIC 9(7)V99.
           05  SALARIO-VEND-WK     PIC 9(7)V99.
           05  TOT-CREDITO-WK      PIC 9(7)V99.
           05  SALDO-CTACORR-WK    PIC S9(7)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP202.CPB".
           COPY "CCP202.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CCD010             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD105             PIC XX       VALUE SPACES.
           05  ST-CCD115             PIC XX       VALUE SPACES.
           05  ST-CCD110             PIC XX       VALUE SPACES.
           05  ST-CCD120             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
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
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9(4).
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9(4).
               10  MES-II            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  DATAI.
               10  ANO-I             PIC 9(4).
               10  MES-I             PIC 99.
               10  DIA-I             PIC 99.
           05  DATA-I REDEFINES DATAI PIC 9(8).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           05  CODIGO-W              PIC 9(6)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  VALOR-E               PIC ZZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZZZ.ZZZ,ZZ-.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  SALDO-APAGAR          PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-CTACORR-W       PIC S9(8)V99 VALUE ZEROS.
           05  SALARIO-FUNC-TOT      PIC 9(8)V99  VALUE ZEROS.
           05  SALARIO-REPORT-TOT    PIC 9(8)V99  VALUE ZEROS.
           05  SALARIO-VEND-TOT      PIC 9(8)V99  VALUE ZEROS.
           05  SALARIO-REPRES-TOT    PIC 9(8)V99  VALUE ZEROS.
           05  TOT-CREDITO-TOT       PIC 9(8)V99  VALUE ZEROS.
           05  SALDO-CTACORR-TOT     PIC S9(8)V99 VALUE ZEROS.
           05  CTACORR-LIQUIDO       PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-APAGAR-TOT      PIC S9(8)V99 VALUE ZEROS.
           05  VALOR-APAGAR-TOT      PIC S9(8)V99 VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(62)   VALUE
           "RELATORIO DE CREDITO GERAL - CONTA CORRENTE".
           05  FILLER              PIC X(15)   VALUE "       VENCTO: ".
           05  VENCTO-REL          PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "MESANO-BASE: ".
           05  MESANO-BASE-REL     PIC 99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "NOME                        CRED-FUNC. CRED-REPOR CRED-VEND.
      -    " CRED-REPR. TOT-CREDIT CTA.CORRENT LIB VLR-APAGAR".
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
           MOVE DATA-INV TO DATA-DIA-INV.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CCD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD010.
           MOVE "CCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "CCD105"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD105.
           MOVE "CCD115"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD115.
           MOVE "CCD110"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD110.
           MOVE "CCD120"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD120.

           OPEN INPUT CGD001 CCD010 CCD100 CCD105 CCD110 CCD115 CCD120.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.
           IF ST-CCD010 <> "00"
              MOVE "ERRO ABERTURA CCD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD105 <> "00"
              MOVE "ERRO ABERTURA CCD105: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD105 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD115 <> "00"
              MOVE "ERRO ABERTURA CCD115: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD115 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD110 <> "00"
              MOVE "ERRO ABERTURA CCD110: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD110 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD120 <> "00"
              MOVE "ERRO ABERTURA CCD120: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD120 TO GS-MENSAGEM-ERRO(23: 02)
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
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
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

       GRAVA-WORK SECTION.
           CLOSE WORK. OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-MESANO-BASE TO MESANO-W.
           MOVE ANO-WW TO ANO-II.
           MOVE MES-WW TO MES-II.

           MOVE ZEROS TO CODIGO-CG01.
           START CGD001 KEY IS NOT < CODIGO-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
             READ CGD001 NEXT RECORD AT END MOVE "10" TO ST-CGD001
               NOT AT END
                 IF T-FUNC-CG01 = 1 AND GS-T-FUNC = 1 OR
                  T-REPRES-CG01 = 1 AND GS-T-REPRES = 1 OR
                   T-FOTOG-CG01 = 1 AND GS-T-REPORT = 1 OR
                     T-CINEG-CG01 = 1 AND GS-T-REPORT = 1 OR
                       T-VEND-CG01 = 1 AND GS-T-VEND = 1
                         PERFORM CONT-GRAVA-WORK
                 ELSE CONTINUE
                 END-IF
             END-READ
           END-PERFORM.
           PERFORM GRAVA-SALDO-CTACORR.
       CONT-GRAVA-WORK SECTION.
           INITIALIZE REG-WORK.
           MOVE CODIGO-CG01 TO GS-EXIBE-PROCESS
           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE MESANO-I TO MESANO-BASE-CC105 MESANO-BASE-CC110
                            MESANO-BASE-CC120 MESANO-BASE-CC115.
           MOVE CODIGO-CG01 TO CODIGO-CC105 CODIGO-CC110 CODIGO-CC120
                               CODIGO-CC115.
           READ CCD105 INVALID KEY CONTINUE
             NOT INVALID KEY
              COMPUTE SALARIO-FUNC-WK = SALARIO1-CC105 + SALARIO2-CC105
              ADD SALARIO-FUNC-WK TO TOT-CREDITO-WK.
           READ CCD115 INVALID KEY CONTINUE
             NOT INVALID KEY
              IF GS-T-REPRES = 0 CONTINUE
              ELSE
               COMPUTE SALARIO-REPRES-WK = SALARIO1-CC115 +
                                           SALARIO2-CC115
               ADD SALARIO-REPRES-WK TO TOT-CREDITO-WK.
           READ CCD110 INVALID KEY CONTINUE
             NOT INVALID KEY
              IF GS-T-REPORT = 0 CONTINUE
              ELSE
               MOVE VALOR-CREDITO-CC110 TO SALARIO-REPORT-WK
               ADD VALOR-CREDITO-CC110 TO TOT-CREDITO-WK.
           READ CCD120 INVALID KEY CONTINUE
             NOT INVALID KEY
              IF GS-T-VEND = 0 CONTINUE
              ELSE
               MOVE VALOR-CREDITO-CC120 TO SALARIO-VEND-WK
               ADD VALOR-CREDITO-CC120 TO TOT-CREDITO-WK.

      *    IF GS-T-FUNC = 1 AND GS-T-REPORT = 1 AND GS-T-VEND = 1
      *               AND GS-T-REPRES = 1
      *                 PERFORM CONT-GRAVA1-WORK
      *    ELSE IF GS-T-FUNC = 1
      *            IF SALARIO-FUNC-WK = ZEROS CONTINUE
      *            ELSE PERFORM CONT-GRAVA1-WORK
      *         ELSE IF GS-T-REPORT = 1
      *                 IF SALARIO-REPORT-WK = ZEROS CONTINUE
      *                 ELSE PERFORM CONT-GRAVA1-WORK
      *              ELSE IF GS-T-VEND = 1
      *                      IF SALARIO-VEND-WK = ZEROS CONTINUE
      *                      ELSE PERFORM CONT-GRAVA1-WORK
      *                   ELSE
      *                     IF GS-T-REPRES = 1
      *                       IF SALARIO-REPRES-WK = ZEROS CONTINUE
      *                       ELSE PERFORM CONT-GRAVA1-WORK.
           PERFORM CONT-GRAVA1-WORK.

       CONT-GRAVA1-WORK SECTION.
      *    IF TOT-CREDITO-WK = ZEROS CONTINUE
      *    ELSE
           ADD 1 TO SEQ-W
           MOVE SEQ-W TO SEQ-WK
           MOVE CODIGO-CG01   TO CODIGO-WK
           MOVE NOME-CG01     TO NOME-WK
           MOVE ZEROS               TO SALDO-CTACORR-WK
           WRITE REG-WORK
           END-WRITE.
      *    END-IF.

       GRAVA-SALDO-CTACORR SECTION.
           MOVE GS-VENCTO(1: 2) TO VENCTO-INV(7: 2)
           MOVE GS-VENCTO(3: 2) TO VENCTO-INV(5: 2)
           MOVE GS-VENCTO(5: 4) TO VENCTO-INV(1: 4).

      *    GRAVAR SALDO CONTA CORRENTE

           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           MOVE VENCTO-INV(1: 6) TO MESANO-I

           PERFORM UNTIL ST-WORK = "10"
            READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
      *        pegar saldo anterior (acumulado)

               MOVE CODIGO-WK   TO FORNEC-CC10
               MOVE ZEROS       TO ANOMES-VCTO-CC10
               START CCD010 KEY IS NOT < ALT-CC10 INVALID KEY
                     MOVE "10" TO ST-CCD010
               END-START
               PERFORM UNTIL ST-CCD010 = "10"
                 READ CCD010 NEXT RECORD AT END MOVE "10" TO ST-CCD010
                   NOT AT END
                    IF ANOMES-VCTO-CC10 NOT < MESANO-I OR
                       FORNEC-CC10 NOT = CODIGO-WK
                         MOVE "10" TO ST-CCD010
                    ELSE
                     ADD SALDOS-CC10 TO SALDO-CTACORR-WK
                     SUBTRACT SALDOE-CC10 FROM SALDO-CTACORR-WK
                    END-IF
                 END-READ
               END-PERFORM

               MOVE CODIGO-WK TO FORNEC-CC100
               MOVE ZEROS     TO SITUACAO-CC100
               MOVE VENCTO-INV(1: 6) TO DATA-VENCTO-CC100(1: 6)
               MOVE 01               TO DATA-VENCTO-CC100(7: 2)
               START CCD100 KEY IS NOT < ALT1-CC100 INVALID KEY
                         MOVE "10" TO ST-CCD100
               END-START
               PERFORM UNTIL ST-CCD100 = "10"
                READ CCD100 NEXT RECORD AT END
                                MOVE "10" TO ST-CCD100
                  NOT AT END
                  IF FORNEC-CC100 NOT = CODIGO-WK OR SITUACAO-CC100 > 0
                     OR DATA-VENCTO-CC100 > VENCTO-INV
                        MOVE "10" TO ST-CCD100
                  ELSE
                     IF CRED-DEB-CC100 = 0
                         ADD VALOR-CC100 TO SALDO-CTACORR-WK
                     ELSE SUBTRACT VALOR-CC100 FROM SALDO-CTACORR-WK
                     END-IF
                END-READ
               END-PERFORM
               REWRITE REG-WORK
            END-READ
           END-PERFORM.
      *    APAGAR TODOS REGISTROS DO WORK QUE ESTIVEREM
      *    COM TOT-CREDITO-WK = ZEROS E SALDO-CTACORR-WK = ZEROS

           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                 IF TOT-CREDITO-WK = ZEROS AND SALDO-CTACORR-WK = ZEROS
                    DELETE WORK
                    END-DELETE
                 END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.


       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           CLOSE WORK.  OPEN I-O WORK.
           MOVE ZEROS TO SALARIO-FUNC-TOT SALARIO-REPORT-TOT
                         SALARIO-REPRES-TOT SALARIO-VEND-TOT
                         TOT-CREDITO-TOT
                         SALDO-CTACORR-TOT SALDO-APAGAR-TOT
                         VALOR-APAGAR-TOT CTACORR-LIQUIDO.
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                          MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                MOVE SPACES TO GS-LINDET
                MOVE NOME-WK          TO GS-LINDET(1: 27)
                MOVE SALARIO-FUNC-WK  TO VALOR-E
                MOVE VALOR-E          TO GS-LINDET(29: 11)
                ADD SALARIO-FUNC-WK   TO SALARIO-FUNC-TOT
                MOVE SALARIO-REPORT-WK TO VALOR-E
                MOVE VALOR-E          TO GS-LINDET(40: 11)
                ADD SALARIO-REPORT-WK TO SALARIO-REPORT-TOT
                MOVE SALARIO-VEND-WK  TO VALOR-E
                MOVE VALOR-E          TO GS-LINDET(51: 11)
                ADD SALARIO-VEND-WK   TO SALARIO-VEND-TOT
                MOVE SALARIO-REPRES-WK TO VALOR-E
                MOVE VALOR-E          TO GS-LINDET(62: 11)
                ADD SALARIO-REPRES-WK TO SALARIO-REPRES-TOT
                MOVE TOT-CREDITO-WK   TO VALOR-E
                ADD TOT-CREDITO-WK    TO TOT-CREDITO-TOT
                MOVE VALOR-E          TO GS-LINDET(73: 11)
                MOVE SALDO-CTACORR-WK TO VALOR-E1
                MOVE VALOR-E1         TO GS-LINDET(84: 12)
                ADD SALDO-CTACORR-WK  TO SALDO-CTACORR-TOT
                MOVE CODIGO-WK        TO GS-LINDET(114: 06)
                MOVE SEQ-WK           TO GS-LINDET(121: 05)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM TOTALIZA.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE "TOTAL..."        TO GS-LINTOT(1: 10)
           MOVE SALARIO-FUNC-TOT  TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(29: 11)
           MOVE SALARIO-REPORT-TOT TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(40: 11)
           MOVE SALARIO-VEND-TOT  TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(51: 11)
           MOVE SALARIO-REPRES-TOT TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(62: 11)
           MOVE TOT-CREDITO-TOT   TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(73: 11)
           MOVE SALDO-CTACORR-TOT TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINTOT(84: 12)
           MOVE "INSERE-TOTAL"    TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP202" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
                 NOT AT END
                   MOVE SPACES TO LINDET-REL
                   MOVE NOME-WK          TO LINDET-REL(1: 27)
                   MOVE SALARIO-FUNC-WK  TO VALOR-E
                   MOVE VALOR-E          TO LINDET-REL(29: 11)
                   MOVE SALARIO-REPORT-WK TO VALOR-E
                   MOVE VALOR-E          TO LINDET-REL(40: 11)
                   MOVE SALARIO-VEND-WK  TO VALOR-E
                   MOVE VALOR-E          TO LINDET-REL(51: 11)
                   MOVE SALARIO-REPRES-WK TO VALOR-E
                   MOVE VALOR-E          TO LINDET-REL(62: 11)
                   MOVE TOT-CREDITO-WK   TO VALOR-E
                   MOVE VALOR-E          TO LINDET-REL(73: 11)
                   MOVE SALDO-CTACORR-WK TO VALOR-E1
                   MOVE VALOR-E1         TO LINDET-REL(84: 12)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
                   END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL.

           copy descondensa.

       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINDET-REL.
           MOVE "TOTAL..."        TO LINDET-REL(1: 10)
           MOVE SALARIO-FUNC-TOT  TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(29: 11)
           MOVE SALARIO-REPORT-TOT TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(40: 11)
           MOVE SALARIO-VEND-TOT  TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(51: 11)
           MOVE SALARIO-REPRES-TOT TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(62: 11)
           MOVE TOT-CREDITO-TOT   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(73: 11)
           MOVE SALDO-CTACORR-TOT TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(84: 12)
           WRITE REG-RELAT FROM LINDET-REL.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           MOVE GS-VENCTO TO VENCTO-REL.
           MOVE GS-MESANO-BASE TO MESANO-BASE-REL.
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
           CLOSE CCD010 CCD100 CCD105 CCD110 CCD115 CCD120 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
