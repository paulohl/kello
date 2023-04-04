       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP200.
      *DATA: 12/01/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RELATÓRIO DE CONTAS CORRENTES
      *FUNÇÃO: Listar todos os nomes (que estiverem na função
      *        selecionada) buscando do cadastro geral, para que seja
      *        relacionados os saldos pendentes (ccd100 e ccd105)
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CCPX100.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CODIGO-WK
                  ALTERNATE RECORD KEY IS NOME-WK
                  ALTERNATE RECORD KEY IS VENCIDOS-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-FORNEC-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TOTAL-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CCPW100.
       FD  WORK.
       01  REG-WORK.
           05  CODIGO-WK           PIC 9(6).
           05  NOME-WK             PIC X(30).
           05  TIPO-FORNEC-WK      PIC 99.
           05  VENCIDOS-WK         PIC S9(8)V99.
           05  AVENCER-WK          PIC S9(8)V99.
           05  TOTAL-WK            PIC S9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP200.CPB".
           COPY "CCP200.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  lin                   pic 9(02)    value zeros.
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
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-WI            PIC 9999.
               10  MES-WI            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  FORNEC-ANT            PIC 9(6)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  GRAVA1-REGRAVA2       PIC 9        VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC 99/9999  BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  TOTAL-W               PIC S9(8)V99 VALUE ZEROS.
           05  VENCIDOS-W            PIC S9(8)V99 VALUE ZEROS.
           05  AVENCER-W             PIC S9(8)V99 VALUE ZEROS.
           05  VENCIDOS-G            PIC S9(8)V99 VALUE ZEROS.
           05  AVENCER-G             PIC S9(8)V99 VALUE ZEROS.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(55)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(57)   VALUE
           "RELACAO DE CONTAS CORRENTES ".
           05  FILLER              PIC X(28)   VALUE SPACES.
           05  FILLER              PIC X(06)   VALUE "TIPO: ".
           05  TIPO-REL            PIC X(19)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB03A.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "-".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "CODIGO   FUNCIONARIO                       TF   VLR-VENCIDO
      -    " VLR-A-VENCER          TOTAL ".
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
           MOVE "CCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           OPEN INPUT CGD001 CCD100.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-GERAR-RELATORIO-TRUE
                    PERFORM GERAR-RELATORIO
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
       GERAR-RELATORIO SECTION.
           CLOSE WORK.  OPEN OUTPUT WORK.  CLOSE WORK.
           OPEN I-O WORK.
           MOVE ZEROS TO VENCIDOS-W AVENCER-W FORNEC-ANT
                         VENCIDOS-G AVENCER-G.
           MOVE ZEROS TO SITUACAO-CC100.
           MOVE ZEROS TO FORNEC-CC100.
           MOVE GS-VENCTO-INI TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO VENCTO-INI.
           MOVE GS-VENCTO-FIM TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO VENCTO-FIM.
           MOVE ZEROS    TO DATA-VENCTO-CC100.
           START CCD100 KEY IS NOT < ALT1-CC100 INVALID KEY
                 MOVE "10" TO ST-CCD100.
           PERFORM UNTIL ST-CCD100 = "10"
              READ CCD100 NEXT RECORD AT END
                   PERFORM GRAVA-DADOS
                   MOVE "10" TO ST-CCD100
              NOT AT END
                 MOVE FORNEC-CC100    TO GS-EXIBE-CODIGO
                 MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 IF SITUACAO-CC100 > 0
                    PERFORM GRAVA-DADOS
                    MOVE "10" TO ST-CCD100
                 ELSE
                   IF FORNEC-ANT NOT = ZEROS
                      IF FORNEC-CC100 NOT = FORNEC-ANT
                         PERFORM GRAVA-DADOS
                      END-IF
                   END-IF
                   IF DATA-VENCTO-CC100 < VENCTO-INI OR
                      DATA-VENCTO-CC100 > VENCTO-FIM
                      CONTINUE
                   ELSE
                        MOVE FORNEC-CC100 TO FORNEC-ANT
                        MOVE TIPO-FORN-CC100 TO TIPO-FORNEC-WK
                        IF DATA-VENCTO-CC100 < DATA-DIA-I
                           IF CRED-DEB-CC100 = 0
                              ADD VALOR-CC100 TO VENCIDOS-W
                           ELSE
                              SUBTRACT VALOR-CC100 FROM VENCIDOS-W
                        ELSE
                           IF CRED-DEB-CC100 = 0
                              ADD VALOR-CC100 TO AVENCER-W
                           ELSE
                              SUBTRACT VALOR-CC100 FROM AVENCER-W
                        END-IF
                     END-IF
              END-READ
           END-PERFORM.
       GRAVA-DADOS SECTION.
           MOVE FORNEC-ANT      TO CODIGO-WK CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01       TO NOME-WK.
           MOVE VENCIDOS-W      TO VENCIDOS-WK.
           MOVE AVENCER-W       TO AVENCER-WK.
           COMPUTE TOTAL-WK = AVENCER-W + VENCIDOS-W.
           ADD VENCIDOS-W       TO VENCIDOS-G.
           ADD AVENCER-W        TO AVENCER-G.
           WRITE REG-WORK.
           MOVE ZEROS TO VENCIDOS-W AVENCER-W.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM ORDEM.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                MOVE SPACES            TO GS-LINDET
                MOVE CODIGO-WK         TO GS-LINDET(1: 9)
                MOVE NOME-WK           TO GS-LINDET(10: 35)
                MOVE VENCIDOS-WK       TO VALOR-E
                MOVE TIPO-FORNEC-WK    TO GS-LINDET(49: 05)
                MOVE VALOR-E           TO GS-LINDET(57: 16)
                MOVE AVENCER-WK        TO VALOR-E
                MOVE VALOR-E           TO GS-LINDET(76: 16)
                COMPUTE TOTAL-W = AVENCER-WK + VENCIDOS-WK
                MOVE TOTAL-W           TO VALOR-E
                MOVE VALOR-E           TO GS-LINDET(94: 14)
                MOVE CODIGO-WK         TO GS-LINDET(115: 6)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           MOVE VENCIDOS-G TO GS-TOTAL-VENCIDO
           MOVE AVENCER-G  TO GS-TOTAL-AVENCER
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1 MOVE SPACES TO NOME-WK
                    MOVE ZEROS  TO VENCIDOS-WK
                    MOVE "NOME           " TO GS-DESC-ORDEM
                    START WORK KEY IS NOT < NOME-WK INVALID KEY
                          MOVE "10" TO ST-WORK
                    END-START
             WHEN 2 MOVE ZEROS  TO TIPO-FORNEC-WK
                    MOVE "TIPO FORNECEDOR" TO GS-DESC-ORDEM
                    START WORK KEY IS NOT < TIPO-FORNEC-WK INVALID KEY
                          MOVE "10" TO ST-WORK
                    END-START
             WHEN 3 MOVE ZEROS  TO VENCIDOS-WK
                    MOVE "VENCIDOS       " TO GS-DESC-ORDEM
                    START WORK KEY IS NOT < VENCIDOS-WK INVALID KEY
                          MOVE "10" TO ST-WORK
                    END-START
             WHEN 4 MOVE ZEROS  TO TOTAL-WK
                    MOVE "TOTAL          " TO GS-DESC-ORDEM
                    START WORK KEY IS NOT < TOTAL-WK INVALID KEY
                          MOVE "10" TO ST-WORK
                    END-START
           END-EVALUATE.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP200" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                MOVE SPACES            TO LINDET-REL
                MOVE CODIGO-WK         TO LINDET-REL(1: 9)
                MOVE NOME-WK           TO LINDET-REL(10: 35)
                MOVE TIPO-FORNEC-WK    TO LINDET-REL(45: 5)
                MOVE VENCIDOS-WK       TO VALOR-E
                MOVE VALOR-E           TO LINDET-REL(48: 15)
                MOVE AVENCER-WK        TO VALOR-E
                MOVE VALOR-E           TO LINDET-REL(63: 15)
                MOVE TOTAL-WK          TO VALOR-E
                MOVE VALOR-E           TO LINDET-REL(78: 14)
                WRITE REG-RELAT FROM LINDET
                END-WRITE
                ADD 1 TO LIN
                IF LIN > 56 PERFORM CABECALHO
                END-IF
              END-READ
           END-PERFORM.
           WRITE REG-RELAT FROM CAB03A.
           MOVE SPACES TO LINDET-REL.
           MOVE "TOTAL... "       TO LINDET-REL(1: 15).
           MOVE VENCIDOS-G        TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(48: 16).
           MOVE AVENCER-G         TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(63: 16).
           COMPUTE TOTAL-W = VENCIDOS-G + AVENCER-G.
           MOVE TOTAL-W           TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(78: 14)

           copy descondensa.

       CABECALHO SECTION.
           MOVE GS-TIPO-LCTO TO TIPO-REL.
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
           CLOSE CCD100 CGD001 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

