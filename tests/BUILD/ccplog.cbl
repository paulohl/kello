       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPLOG.
      *DATA: 06/07/2009
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: EXTRATO DOS LOGS
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
           COPY LOGCCD.SEL.

           SELECT WORK ASSIGN  TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  STATUS       IS ST-WORK
                  RECORD KEY   IS  SEQ-WK.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CCPW100.
       COPY LOGCCD.FD.

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(06).
           05  FORNEC-WK           PIC 9(06).
           05  SEQUEN-WK           PIC 9(05).
           05  USUARIO-WK          PIC X(05).
           05  DATA-WK             PIC 9(08).
           05  HORA-WK             PIC 9(08).
           05  PROGRAMA-WK         PIC X(10).
           05  VALOR-WK            PIC 9(09)V99.
           05  DESCRICAO-WK        PIC X(30).
           05  DATA-MOV-WK         PIC 9(08).
           05  IP-MAQUINA-WK       PIC X(20).
           05  MAC-MAQUINA-WK      PIC X(20).
           05  HOST-MAQUINA-WK     PIC X(10).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(155).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCPLOG.CPB".
           COPY "CCPLOG.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD101             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD021             PIC XX       VALUE SPACES.
           05  ST-LOGCCD             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9999.
               10  MES-II            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  MESANOINI.
               10 ANOINI             PIC 9(04).
               10 MESINI             PIC 9(02).
           05  MESANOFIN.
               10 ANOFIN             PIC 9(04).
               10 MESFIN             PIC 9(02).
           05  DIAINI                PIC 9(02).
           05  DIAFIN                PIC 9(02).
           05  NUM-DIAS              PIC 9(02).
           05  QTDE-TAXA             PIC 9(04).
           05  IND                   PIC 9(04).
           05  TOTAL-TAXA            PIC 9(04)V999.
           05  AUX-VALOR             PIC S9(09)V99.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  GRAVA1-REGRAVA2       PIC 9        VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-BASE             PIC 9(8)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(6)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  VALOR1                PIC 9(08)V99.
           05  VALOR2                PIC s9(08)V99.
           05  VALOR-E1              PIC ZZZZZ.ZZZ,ZZ-.
           05  DIA-MES-E             PIC ZZ/ZZ.
           05  SALDO-WI              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-WT              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-GI              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-GT              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-ANT             PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-ANT-T           PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-E               PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  TIPO-ANT              PIC 9        VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           05  VALOR-AUX             PIC 9(09)V99 VALUE ZEROS.
           05  DS-DIAS               pic 9(05)    value zeros.
           05  CONTADOR              pic 9(05)    value zeros.
           05  VALORE-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE ENTRADA
           05  VALORS-W              PIC 9(8)V99  VALUE ZEROS.
           05  AUX-SITUACAO          PIC 9(01)    VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(55)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02A.
           05  FILLER              PIC X(12)    VALUE "FORNECEDOR: ".
           05  NOME-REL            PIC X(50)   VALUE SPACES.
           05  FILLER              PIC X(01).
           05  FILLER              PIC X(11)   VALUE "INT.VECTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(05) VALUE " ATE ".
           05  VENCTO-FIM-REL      PIC 99/99/9999.

       01  CAB03.
           05  FILLER              PIC X(134)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(120)  VALUE
         "FORNEC SEQUE USUAR   DATA   HORA  PROGRAMA         VALOR DESCR
      -  "ICAO                      DT-MOVTO IP                  MAC".

       01  DET-LOG.
           05 DET-FORNEC           PIC 9(06).
           05 FILLER               PIC X(01).
           05 DET-SEQUEN           PIC 9(05).
           05 FILLER               PIC X(01).
           05 DET-USUARIO          PIC X(05).
           05 FILLER               PIC X(01).
           05 DET-DIA              PIC 99/.
           05 DET-MES              PIC 99/.
           05 DET-ANO              PIC 99.
           05 FILLER               PIC X(01).
           05 DET-HORA             PIC 99.
           05 FILLER               PIC X(01) VALUE ":".
           05 DET-MINU             PIC 99.
           05 FILLER               PIC X(01).
           05 DET-PROGRAMA         PIC X(07).
           05 FILLER               PIC X(01).
           05 DET-VALOR            PIC ZZZ.ZZZ.ZZ9,99.
           05 FILLER               PIC X(01).
           05 DET-DESCRICAO        PIC X(30).
           05 FILLER               PIC X(01).
           05 DET-DIA-MOV          PIC 99/.
           05 DET-MES-MOV          PIC 99/.
           05 DET-ANO-MOV          PIC 99.
           05 FILLER               PIC X(01).
           05 DET-IP               PIC X(19).
           05 FILLER               PIC X(01).
           05 DET-MAC              PIC X(17).

       01  LINDET.
           05  LINDET-REL          PIC X(150)  VALUE SPACES.

           copy   "ldifdias".

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
           MOVE "CGD001"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CCD100"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "LOGCCD"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOGCCD.

           OPEN I-O   LOGCCD
           CLOSE      LOGCCD
           OPEN INPUT LOGCCD

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
           IF ST-LOGCCD <> "00"
              MOVE "ERRO ABERTURA LOGCCD: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOGCCD TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-LER-FORNEC-TRUE
                    PERFORM LER-FORNECEDOR
               WHEN GS-POPUP-FORNEC-TRUE
                    PERFORM POPUP-FORNECEDOR
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

       POPUP-FORNECEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-CODIGO.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME.
       LER-FORNECEDOR SECTION.
           MOVE GS-CODIGO TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "*********" TO NOME-CG01.
           MOVE NOME-CG01 TO GS-NOME.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       GERAR-RELATORIO SECTION.
           MOVE GS-DATAINI  TO  DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV    TO  DATA-INI
           MOVE GS-DATA-FIM TO  DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV    TO  DATA-FIM

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           INITIALIZE REG-LOGCCD
                      SEQ-W

           IF GS-CODIGO > 0
              MOVE GS-CODIGO TO LOGCCD-FORNEC
              START LOGCCD KEY IS NOT LESS CHAVE-LOGCCD INVALID KEY
                   MOVE "10" TO ST-LOGCCD
              END-START
              PERFORM UNTIL ST-LOGCCD = "10"
                   READ LOGCCD NEXT AT END
                        MOVE "10" TO ST-LOGCCD
                   NOT AT END
                        IF GS-CODIGO <> LOGCCD-FORNEC
                           MOVE "10" TO ST-LOGCCD
                        ELSE
                           IF (LOGCCD-DATA NOT < DATA-INI AND
                               LOGCCD-DATA NOT > DATA-FIM) OR
                               DATA-INI = 0
                               IF GS-USUARIO = SPACES OR
                                  LOGCCD-USUARIO
                                  PERFORM MOVER-DADOS
                               END-IF
                           END-IF
                        END-IF
                   END-READ
              END-PERFORM
           ELSE
              IF GS-USUARIO <> SPACES
                 MOVE GS-USUARIO  TO LOGCCD-USUARIO
                 MOVE DATA-INI TO LOGCCD-DATA
                 START LOGCCD KEY IS NOT LESS LOGCCD-CH-USUARIO
                                                             INVALID KEY
                      MOVE "10" TO ST-LOGCCD
                 END-START
                 PERFORM UNTIL ST-LOGCCD = "10"
                      READ LOGCCD NEXT AT END
                           MOVE "10" TO ST-LOGCCD
                      NOT AT END
                           IF LOGCCD-USUARIO <> GS-USUARIO   OR
                             (DATA-FIM > 0 AND LOGCCD-DATA > DATA-FIM)
                              MOVE "10" TO ST-LOGCCD
                           ELSE
                              PERFORM MOVER-DADOS
                           END-IF
                      END-READ
                 END-PERFORM
              ELSE
                 MOVE DATA-INI TO LOGCCD-DATA
                 START LOGCCD KEY IS NOT LESS LOGCCD-CH-DATA INVALID KEY
                      MOVE "10" TO ST-LOGCCD
                 END-START
                 PERFORM UNTIL ST-LOGCCD = "10"
                      READ LOGCCD NEXT AT END
                           MOVE "10" TO ST-LOGCCD
                      NOT AT END
                           IF LOGCCD-DATA > DATA-FIM
                              MOVE "10" TO ST-LOGCCD
                           ELSE
                              PERFORM MOVER-DADOS
                           END-IF
                      END-READ
                 END-PERFORM
              END-IF
           END-IF
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS SECTION.
           MOVE LOGCCD-FORNEC          TO FORNEC-CC100
           MOVE LOGCCD-SEQ             TO SEQ-CC100
           READ CCD100 NOT INVALID KEY
                ADD 1                   TO SEQ-W
                MOVE SEQ-W              TO GS-EXIBE-CODIGO SEQ-WK
                MOVE "TELA-AGUARDA1"    TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM

                MOVE LOGCCD-FORNEC      TO FORNEC-WK
                MOVE LOGCCD-SEQ         TO SEQUEN-WK
                MOVE LOGCCD-USUARIO     TO USUARIO-WK
                MOVE LOGCCD-DATA        TO DATA-WK
                MOVE LOGCCD-HORA        TO HORA-WK
                MOVE LOGCCD-PROGRAMA    TO PROGRAMA-WK
                MOVE LOGCCD-IP-MAQUINA  TO IP-MAQUINA-WK
                MOVE LOGCCD-MAC-MAQUINA TO MAC-MAQUINA-WK
                MOVE LOGCCD-HOST        TO HOST-MAQUINA-WK
                MOVE VALOR-CC100        TO VALOR-WK
                MOVE DESCRICAO-CC100    TO DESCRICAO-WK
                MOVE DATA-MOVTO-CC100   TO DATA-MOV-WK
                WRITE REG-WORK
                END-WRITE.
       MOVER-DADOS-FIM.
           EXIT.

       CARREGA-LISTA SECTION.
           MOVE ZEROS TO SALDO-WT SALDO-GT
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB04 TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB03 TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS SEQ-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   MOVE FORNEC-WK         TO DET-FORNEC
                   MOVE SEQUEN-WK         TO DET-SEQUEN
                   MOVE USUARIO-WK        TO DET-USUARIO
                   MOVE DATA-WK(1:4)      TO DET-ANO
                   MOVE DATA-WK(5:2)      TO DET-MES
                   MOVE DATA-WK(7:2)      TO DET-DIA
                   MOVE HORA-WK(1:2)      TO DET-HORA
                   MOVE HORA-WK(3:2)      TO DET-MINU
                   MOVE PROGRAMA-WK       TO DET-PROGRAMA
                   MOVE VALOR-WK          TO DET-VALOR
                   MOVE DESCRICAO-WK      TO DET-DESCRICAO
                   MOVE DATA-MOV-WK(1:4)  TO DET-ANO-MOV
                   MOVE DATA-MOV-WK(5:2)  TO DET-MES-MOV
                   MOVE DATA-MOV-WK(7:2)  TO DET-DIA-MOV
                   MOVE IP-MAQUINA-WK     TO DET-IP
                   MOVE MAC-MAQUINA-WK    TO DET-MAC

                   MOVE DET-LOG           TO GS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       LINHA-BRANCO SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
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
           MOVE "CCPLOG" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO SALDO-WT SALDO-GT
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.

           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS SEQ-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   MOVE FORNEC-WK         TO DET-FORNEC
                   MOVE SEQUEN-WK         TO DET-SEQUEN
                   MOVE USUARIO-WK        TO DET-USUARIO
                   MOVE DATA-WK(1:4)      TO DET-ANO
                   MOVE DATA-WK(5:2)      TO DET-MES
                   MOVE DATA-WK(7:2)      TO DET-DIA
                   MOVE HORA-WK(1:2)      TO DET-HORA
                   MOVE HORA-WK(3:2)      TO DET-MINU
                   MOVE PROGRAMA-WK       TO DET-PROGRAMA
                   MOVE VALOR-WK          TO DET-VALOR
                   MOVE DESCRICAO-WK      TO DET-DESCRICAO
                   MOVE DATA-MOV-WK(1:4)  TO DET-ANO-MOV
                   MOVE DATA-MOV-WK(5:2)  TO DET-MES-MOV
                   MOVE DATA-MOV-WK(7:2)  TO DET-DIA-MOV
                   MOVE IP-MAQUINA-WK     TO DET-IP
                   MOVE MAC-MAQUINA-WK    TO DET-MAC

                   MOVE DET-LOG           TO LINDET-REL

                   WRITE REG-RELAT FROM LINDET-REL
                   END-WRITE

                   ADD 1 TO LIN
                   IF LIN > 56
                      PERFORM CABECALHO
                   END-IF

              END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           MOVE SPACES TO NOME-REL
           STRING GS-CODIGO "-" GS-NOME INTO NOME-REL
           MOVE GS-DATAINI    TO VENCTO-INI-REL
           MOVE GS-DATA-FIM   TO VENCTO-FIM-REL
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02A AFTER 2.
           WRITE REG-RELAT FROM CAB03
           WRITE REG-RELAT FROM CAB04
           WRITE REG-RELAT FROM CAB03
           MOVE 8 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CCD100 CGD001 WORK LOGCCD
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
