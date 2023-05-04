       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP206.
      *DATA: 10/09/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: TRIAGEM DO CONTA CORRENTE
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CCPX001.
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
       COPY CCPW001.
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
           05  SALDO-CC-WK         PIC S9(8)V99.
           05  VLR-CREDITO-WK      PIC 9(8)V99.
           05  VLR-DEBITO-WK       PIC 9(8)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP206.CPB".
           COPY "CCP206.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(60).

       01  VARIAVEIS.
           05  ST-CCD001             PIC XX       VALUE SPACES.
           05  ST-CCD010             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD105             PIC XX       VALUE SPACES.
           05  ST-CCD115             PIC XX       VALUE SPACES.
           05  ST-CCD110             PIC XX       VALUE SPACES.
           05  ST-CCD120             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9(4).
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9(4).
               10  MES-II            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  MESANOIW.
               10  ANO-IIW            PIC 9(4).
               10  MES-IIW            PIC 99.
           05  MESANO-IW REDEFINES MESANOIW PIC 9(6).
           05  DATAI.
               10  ANO-I             PIC 9(4).
               10  MES-I             PIC 99.
               10  DIA-I             PIC 99.
           05  DATA-I REDEFINES DATAI PIC 9(8).
           05  MES-EMISSAO           PIC 9(2)     VALUE ZEROS.
           05  QTDE-POR-FOLHA        PIC 9        VALUE ZEROS.
           05  SALDO-POSITIVO        PIC 9(8)V99  VALUE ZEROS.
           05  SALDO-NEGATIVO        PIC S9(8)V99  VALUE ZEROS.
           05  SALDO-W               PIC S9(8)V99 VALUE ZEROS.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  COD-EMPRESA-W         PIC 9        VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  VALORE-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE ENTRADA
           05  VALORS-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE SAIDA
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(45)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(67)   VALUE
           "TRIAGEM DO CONTA CORRENTE                           ".
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "MESANO-BASE: ".
           05  MESANO-BASE-REL     PIC 99/9999.
       01  CAB03.
           05  FILLER              PIC X(90)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(90)  VALUE
           "NOME                           SALDO-CTA.CORR   VLR-CREDITO
      -    "   VLR-DEBITO        SALDO ".
       01  LINDET.
           05  LINDET-REL          PIC X(90)  VALUE SPACES.

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
           OPEN INPUT CGD001 CCD100 CCD105 CCD115 CCD110 CCD120 CCD001
                      CCD010.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD001 <> "00"
              MOVE "ERRO ABERTURA CCD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD010 <> "00"
              MOVE "ERRO ABERTURA CCD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD010 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-VERIFICA-ACESSO-TRUE
                    PERFORM VERIFICA-ACESSO-USUARIO
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
       VERIFICA-ACESSO-USUARIO SECTION.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-ACESSO-CC01.
           MOVE USUARIO-W          TO NOME-REDUZ-CC01.
           READ CCD001 INVALID KEY
                MOVE 0  TO GS-LIBERA-ACESSO
             NOT INVALID KEY
                MOVE 1 TO GS-LIBERA-ACESSO.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-MESANO-BASE TO MESANO-W.
           MOVE ANO-WW TO ANO-II.
           MOVE MES-WW TO MES-II.
           MOVE MESANO-I TO MESANO-IW.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-W.
           EVALUATE TIPO-LCTO-W
             WHEN 1 PERFORM GRAVA-WORK-CCD105
             WHEN 2 PERFORM GRAVA-WORK-CCD120
             WHEN 3 PERFORM GRAVA-WORK-CCD115
             WHEN 4 PERFORM GRAVA-WORK-CCD110
           END-EVALUATE.
           PERFORM GRAVA-SALDO-CTACORR.
       GRAVA-WORK-CCD105 SECTION.
           MOVE MESANO-I TO MESANO-BASE-CC105.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-W.
           MOVE ZEROS TO CODIGO-CC105.
           START CCD105 KEY IS NOT < CHAVE-CC105 INVALID KEY
                  MOVE "10" TO ST-CCD105.
           INITIALIZE REG-WORK.
           PERFORM UNTIL ST-CCD105 = "10"
             READ CCD105 NEXT RECORD AT END MOVE "10" TO ST-CCD105
              NOT AT END
                  IF MESANO-BASE-CC105 NOT = MESANO-I
                       MOVE "10" TO ST-CCD105
                  ELSE
                   ADD 1 TO SEQ-WK
                   MOVE CODIGO-CC105        TO CODIGO-WK CODIGO-CG01
                                               GS-EXIBE-PROCESS
                   READ CGD001 INVALID KEY MOVE "*******" TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01           TO NOME-WK
                   COMPUTE VLR-CREDITO-WK = SALARIO1-CC105 +
                                            SALARIO2-CC105
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-WORK
                   END-IF
               END-READ
           END-PERFORM.
           CLOSE WORK.  OPEN I-O WORK.
       GRAVA-WORK-CCD115 SECTION.
           MOVE MESANO-I TO MESANO-BASE-CC115.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-W.
           MOVE ZEROS TO CODIGO-CC115.
           START CCD115 KEY IS NOT < CHAVE-CC115 INVALID KEY
                  MOVE "10" TO ST-CCD115.
           INITIALIZE REG-WORK.
           PERFORM UNTIL ST-CCD115 = "10"
             READ CCD115 NEXT RECORD AT END MOVE "10" TO ST-CCD115
              NOT AT END
                  IF MESANO-BASE-CC115 NOT = MESANO-I
                       MOVE "10" TO ST-CCD115
                  ELSE
                   ADD 1 TO SEQ-WK
                   MOVE CODIGO-CC115        TO CODIGO-WK CODIGO-CG01
                                               GS-EXIBE-PROCESS
                   READ CGD001 INVALID KEY MOVE "*******" TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01           TO NOME-WK
                   COMPUTE VLR-CREDITO-WK = SALARIO1-CC115 +
                                            SALARIO2-CC115
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-WORK
                   END-IF
               END-READ
           END-PERFORM.
           CLOSE WORK.  OPEN I-O WORK.
       GRAVA-WORK-CCD110 SECTION.
           MOVE MESANO-I TO MESANO-BASE-CC110.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-W.
           MOVE ZEROS TO CODIGO-CC110.
           START CCD110 KEY IS NOT < CHAVE-CC110 INVALID KEY
                  MOVE "10" TO ST-CCD110.
           INITIALIZE REG-WORK.
           PERFORM UNTIL ST-CCD110 = "10"
             READ CCD110 NEXT RECORD AT END MOVE "10" TO ST-CCD110
              NOT AT END
                  IF MESANO-BASE-CC110 NOT = MESANO-I
                       MOVE "10" TO ST-CCD110
                  ELSE
                   ADD 1 TO SEQ-WK
                   MOVE CODIGO-CC110        TO CODIGO-WK CODIGO-CG01
                                               GS-EXIBE-PROCESS
                   READ CGD001 INVALID KEY MOVE "*******" TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01           TO NOME-WK
                   MOVE VALOR-CREDITO-CC110 TO VLR-CREDITO-WK
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-WORK
                   END-IF
               END-READ
           END-PERFORM.
           CLOSE WORK.  OPEN I-O WORK.
       GRAVA-WORK-CCD120 SECTION.
           MOVE MESANO-I TO MESANO-BASE-CC120.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-W.
           MOVE ZEROS TO CODIGO-CC120.
           START CCD120 KEY IS NOT < CHAVE-CC120 INVALID KEY
                  MOVE "10" TO ST-CCD120.
           INITIALIZE REG-WORK.
           PERFORM UNTIL ST-CCD120 = "10"
             READ CCD120 NEXT RECORD AT END MOVE "10" TO ST-CCD120
              NOT AT END
                  IF MESANO-BASE-CC120 NOT = MESANO-I
                       MOVE "10" TO ST-CCD120
                  ELSE
                   ADD 1 TO SEQ-WK
                   MOVE CODIGO-CC120        TO CODIGO-WK CODIGO-CG01
                                               GS-EXIBE-PROCESS
                   READ CGD001 INVALID KEY MOVE "*******" TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01           TO NOME-WK
                   MOVE VALOR-CREDITO-CC120 TO VLR-CREDITO-WK
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-WORK
                   END-IF
               END-READ
           END-PERFORM.
           CLOSE WORK.  OPEN I-O WORK.
       GRAVA-SALDO-CTACORR SECTION.
           ADD 1 TO MES-IIW.
           IF MES-IIW > 12
              ADD 1 TO ANO-IIW
              MOVE 1 TO MES-IIW.

           MOVE MESANO-I        TO VENCTO-INV(1: 6)
           MOVE 01              TO VENCTO-INV(7: 2).

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
                     ADD SALDOS-CC10 TO SALDO-CC-WK
                     SUBTRACT SALDOE-CC10 FROM SALDO-CC-WK
                    END-IF
                 END-READ
               END-PERFORM
               PERFORM TERMINAR-CALCULO-SALDO-CC
               MOVE CODIGO-WK TO FORNEC-CC100
               MOVE ZEROS     TO SITUACAO-CC100
               MOVE MESANO-IW        TO VENCTO-INV(1: 6)
               MOVE 01              TO VENCTO-INV(7: 2)
               MOVE VENCTO-INV       TO DATA-VENCTO-CC100
               START CCD100 KEY IS NOT < ALT1-CC100 INVALID KEY
                         MOVE "10" TO ST-CCD100
               END-START
               PERFORM UNTIL ST-CCD100 = "10"
                READ CCD100 NEXT RECORD AT END
                                MOVE "10" TO ST-CCD100
                  NOT AT END
                  IF FORNEC-CC100 NOT = CODIGO-WK OR SITUACAO-CC100 > 0
                        MOVE "10" TO ST-CCD100
                  ELSE
                     IF CRED-DEB-CC100 = 0
                        CONTINUE
                     ELSE ADD VALOR-CC100 TO VLR-DEBITO-WK
                     END-IF
                  END-IF
                END-READ
               END-PERFORM
               REWRITE REG-WORK
            END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
      *GALHO-ACERTAR MES 09 E 10
       TERMINAR-CALCULO-SALDO-CC SECTION.
           MOVE ZEROS TO SITUACAO-CC100.
           MOVE CODIGO-WK TO FORNEC-CC100.
           MOVE MESANO-I TO DATA-VENCTO-CC100(1: 6) DATA-FIM(1: 6).
           MOVE 01       TO DATA-VENCTO-CC100(7: 2).
           MOVE 29       TO DATA-FIM(7: 2).
           START CCD100 KEY IS NOT < ALT1-CC100 INVALID KEY
                 MOVE "10" TO ST-CCD100.
           PERFORM UNTIL ST-CCD100 = "10"
            READ CCD100 NEXT RECORD AT END MOVE "10" TO ST-CCD100
               NOT AT END
                 IF DATA-VENCTO-CC100 > DATA-FIM OR
                    SITUACAO-CC100 > 0 OR FORNEC-CC100 <> CODIGO-WK
                    MOVE "10" TO ST-CCD100
                 ELSE
                  MOVE DATA-VENCTO-CC100(1: 6) TO ANOMES-VCTO-CC10
                  MOVE FORNEC-CC100            TO FORNEC-CC10
                  MOVE ZEROS TO VALORE-W VALORS-W
                  IF CRED-DEB-CC100 = 0 MOVE VALOR-CC100 TO VALORS-W
                  ELSE MOVE VALOR-CC100 TO VALORE-W
                  END-IF
                  SUBTRACT VALORE-W FROM SALDO-CC-WK
                  ADD VALORS-W TO SALDO-CC-WK
                 END-IF
              END-READ
           END-PERFORM.
      *---------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO SALDO-POSITIVO SALDO-NEGATIVO.

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           CLOSE WORK.  OPEN I-O WORK.
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                          MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
               COMPUTE SALDO-W = VLR-CREDITO-WK - VLR-DEBITO-WK
               ADD SALDO-CC-WK       TO SALDO-W
               IF SALDO-W < 1 CONTINUE
               ELSE
                MOVE NOME-WK          TO GS-LINDET(1: 31)
                MOVE SALDO-CC-WK      TO VALOR-E1
                MOVE VALOR-E1         TO GS-LINDET(32: 15)
                MOVE VLR-CREDITO-WK   TO VALOR-E
                MOVE VALOR-E          TO GS-LINDET(47: 14)
                MOVE VLR-DEBITO-WK    TO VALOR-E
                MOVE VALOR-E          TO GS-LINDET(61: 14)
                COMPUTE SALDO-W = VLR-CREDITO-WK - VLR-DEBITO-WK
                ADD SALDO-CC-WK       TO SALDO-W
                MOVE SALDO-W          TO VALOR-E1
                MOVE VALOR-E1         TO GS-LINDET(75: 14)
                IF SALDO-W < 0 SUBTRACT SALDO-W FROM SALDO-NEGATIVO
                ELSE ADD SALDO-W TO SALDO-POSITIVO
                END-IF
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
               END-IF
              END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM TOTALIZA.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE "TOTAL POSITIVO: "        TO GS-LINTOT(1: 16)
           MOVE SALDO-POSITIVO            TO VALOR-E
           MOVE VALOR-E                   TO GS-LINTOT(17: 13)
           MOVE "TOTAL NEGATIVO: "        TO GS-LINTOT(40: 16)
           MOVE SALDO-NEGATIVO            TO VALOR-E1
           MOVE VALOR-E1                   TO GS-LINTOT(56: 13)
           MOVE "INSERE-TOTAL"    TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP206" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
                 NOT AT END
                 COMPUTE SALDO-W = VLR-CREDITO-WK - VLR-DEBITO-WK
                 ADD SALDO-CC-WK TO SALDO-W
                 IF SALDO-W < 1 CONTINUE
                 ELSE
                   MOVE NOME-WK          TO LINDET-REL(1: 31)
                   MOVE SALDO-CC-WK      TO VALOR-E1
                   MOVE VALOR-E1         TO LINDET-REL(32: 15)
                   MOVE VLR-CREDITO-WK   TO VALOR-E
                   MOVE VALOR-E          TO LINDET-REL(47: 14)
                   MOVE VLR-DEBITO-WK    TO VALOR-E
                   MOVE VALOR-E          TO LINDET-REL(61: 14)
                   COMPUTE SALDO-W = VLR-CREDITO-WK - VLR-DEBITO-WK
                   ADD SALDO-CC-WK TO SALDO-W
                   MOVE SALDO-W          TO VALOR-E1
                   MOVE VALOR-E1         TO LINDET-REL(75: 14)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
                   END-IF
                 END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL.

           COPY DESCONDENSA.

       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINDET-REL.
           MOVE "TOTAL POSITIVO: "        TO LINDET-REL(1: 16)
           MOVE SALDO-POSITIVO            TO VALOR-E
           MOVE VALOR-E                   TO LINDET-REL(17: 13)
           MOVE "TOTAL NEGATIVO: "        TO LINDET-REL(40: 16)
           MOVE SALDO-NEGATIVO            TO VALOR-E1
           MOVE VALOR-E1                   TO LINDET-REL(56: 13).
           WRITE REG-RELAT FROM LINDET AFTER 2.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
       CABECALHO SECTION.
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
           CLOSE CGD001 CCD001 CCD100 CCD105 CCD110 CCD115 CCD120 WORK
                 CCD010.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
