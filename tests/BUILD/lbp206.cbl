       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP206.
      *DATA: 06/06/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: ANÁLISE DE FILMES RECEBIDOS
      *FUNÇÃO: Listar todos as análises que estiverem dentro de interva-
      *        lo de data solicitado
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CGPX001.
           COPY COPX003.
           COPY COPX040.
           COPY LBPX021.
           COPY LBPX100.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS DATA-MOVTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FOTOGRAFO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-FILME-WK DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK DUPLICATES
                  ALTERNATE RECORD KEY IS CONTRATO-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT EXCEL ASSIGN TO ARQUIVO-EXCEL
                        ORGANIZATION IS SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY COPW003.
       COPY COPW040.
       COPY CGPW001.
       COPY LBPW021.
       COPY LBPW100.
       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(4).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  FOTOGRAFO-WK        PIC X(15).
           05  CONTRATO-WK         PIC 9(4).
           05  EVENTO-WK           PIC X(10).
           05  CIDADE-WK           PIC X(10).
           05  IDENTIFICACAO-WK    PIC X(10).
           05  QT-FILME-WK         PIC 9(4).
           05  TIPO-FILME-WK       PIC X(10).
           05  QT-FORM-WK          PIC 9(4).
           05  QT-FOTO-WK          PIC 9(4).
           05  QT-FIL-FORM-WK      PIC 9(3)V9999.
           05  QT-FOT-FORM-WK      PIC 9(4)V99.
           05  IDENTIFICADOR-WK    PIC 9(9).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       FD EXCEL.
       01 REG-EXCEL.
          05 EXCEL-DATA            PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FOTOGRAFO       PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CONTRATO        PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-IDENTIFICACAO   PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CIDADE          PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-EVENTO          PIC X(30).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FILME           PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-TIPO-FILME      PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FORMANDOS       PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FILME-FORM      PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FOT-FORM        PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-IDENTIFICADOR   PIC X(20).
          05 FILLER                PIC X(02) VALUE X"0DA0".

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP206.CPB".
           COPY "LBP206.CPY".
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
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-LBD021             PIC XX       VALUE SPACES.
           05  ST-LBD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
      *    CONTROLE DE QUEBRA
           05  DATA-MOVTO-ANT        PIC 9(8)     VALUE ZEROS.
           05  FOTOGRAFO-ANT         PIC X(15)    VALUE SPACES.
           05  CIDADE-ANT            PIC X(10)    VALUE SPACES.
           05  TIPO-FILME-ANT        PIC X(10)    VALUE SPACES.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
      *    TOTALIZA VARIAVEIS
           05  TOTAL-FILME           PIC 9(8)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZZ.ZZZ.
           05  TOTAL-FILME-G         PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FORM            PIC 9(6)     VALUE ZEROS.
           05  TOTAL-FORM-G          PIC 9(6)     VALUE ZEROS.
           05  TOTAL-FOTO            PIC 9(6)     VALUE ZEROS.
           05  TOTAL-FOTO-G          PIC 9(6)     VALUE ZEROS.
           05  FIL-FORM-E            PIC ZZZ,ZZZZ BLANK WHEN ZEROS.
           05  FOT-FORM-E            PIC Z.ZZZ,ZZ BLANK WHEN ZEROS.
           05  FORM-E                PIC ZZZZ.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  MASC-IDENTIFICADOR    PIC ZZZZZZZZ9 BLANK WHEN ZEROS.
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
           05  FILLER              PIC X(36)   VALUE
           "ANALISE DE MIDIAS RECEBIDAS- ORDEM: ".
           05  ORDEM-REL           PIC X(27)   VALUE SPACES.
           05  FILLER              PIC X(08)    VALUE SPACES.
           05  ORDEM-SOLIC-REL     PIC X(39)    VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(118) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(118)  VALUE
           "DATA-MOVTO FOTOGRAFO       CONT IDENTIFIC. CIDADE     EVENTO
      -    "     QT.FILMES TIPO-MIDIA FORM FIL/FORM FOT/FORM     NR-HD".
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
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "LBD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD021.
           MOVE "LBD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD100.
           OPEN INPUT CGD001 CAD010 COD003 COD040 LBD021 LBD100.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
           IF ST-LBD021 <> "00"
              MOVE "ERRO ABERTURA LBD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD100 <> "00"
              MOVE "ERRO ABERTURA LBD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD100 TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LE-CONTRATO
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM POPUP-CONTRATO
               WHEN GS-EXCEL-TRUE
                   PERFORM GERAR-EXCEL
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       GERAR-EXCEL SECTION.
           MOVE SPACES TO ARQUIVO-EXCEL

           IF GS-TIPO = 1
              STRING "\ARQUIVOS\LABO-" GS-DATA-INI "-" GS-DATA-FIM INTO
              ARQUIVO-EXCEL
           ELSE
              STRING "\ARQUIVOS\LABO-" GS-CONTRATO INTO ARQUIVO-EXCEL.


           OPEN OUTPUT EXCEL

           MOVE "DATA-MOVTO"      TO EXCEL-DATA
           MOVE "FOTÓGRAFO"       TO EXCEL-FOTOGRAFO
           MOVE "CONT"            TO EXCEL-CONTRATO
           MOVE "IDENTIFIC."      TO EXCEL-IDENTIFICACAO
           MOVE "CIDADE"          TO EXCEL-CIDADE
           MOVE "EVENTO"          TO EXCEL-EVENTO
           MOVE "QT.FILMES"       TO EXCEL-FILME
           MOVE "TIPO-FILME"      TO EXCEL-TIPO-FILME
           MOVE "FORM"            TO EXCEL-FORMANDOS
           MOVE "FIL/FORM"        TO EXCEL-FILME-FORM
           MOVE "FOT/FORM"        TO EXCEL-FOT-FORM


           WRITE REG-EXCEL

           PERFORM ORDEM.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   EVALUATE GS-ORDEM
                     WHEN 1 IF DATA-MOVTO-ANT NOT = ZEROS
                               IF DATA-MOVTO-ANT NOT = DATA-MOVTO-WK
                                  PERFORM TOTALIZA-EXCEL
                     WHEN 2
                            IF FOTOGRAFO-ANT  NOT = SPACES
                               IF FOTOGRAFO-ANT NOT = FOTOGRAFO-WK
                                  PERFORM TOTALIZA-EXCEL
                     WHEN 3
                            IF CONTRATO-ANT NOT = ZEROS
                               IF CONTRATO-ANT NOT = CONTRATO-WK
                                  PERFORM TOTALIZA-EXCEL
                     WHEN 4
                            IF CIDADE-ANT NOT = SPACES
                               IF CIDADE-ANT NOT = CIDADE-WK
                                  PERFORM TOTALIZA-EXCEL
                     WHEN 5
                            IF TIPO-FILME-ANT NOT = SPACES
                               IF TIPO-FILME-ANT NOT = TIPO-FILME-WK
                                  PERFORM TOTALIZA-EXCEL
                   END-EVALUATE
                   PERFORM MOVER-CHAVE-ANT
                   MOVE DATA-MOVTO-WK      TO DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV           TO DATA-E
                   MOVE DATA-E             TO EXCEL-DATA
                   MOVE FOTOGRAFO-WK       TO EXCEL-FOTOGRAFO
                   MOVE CONTRATO-WK        TO EXCEL-CONTRATO
                   MOVE IDENTIFICACAO-WK   TO EXCEL-IDENTIFICACAO
                   MOVE CIDADE-WK          TO EXCEL-CIDADE
                   MOVE EVENTO-WK          TO EXCEL-EVENTO
                   MOVE QT-FILME-WK        TO QTDE-E
                   ADD QT-FILME-WK         TO TOTAL-FILME
                   ADD QT-FOTO-WK          TO TOTAL-FOTO
                   ADD QT-FORM-WK          TO TOTAL-FORM
                   MOVE QTDE-E             TO EXCEL-FILME
                   MOVE TIPO-FILME-WK      TO EXCEL-TIPO-FILME
                   MOVE QT-FORM-WK         TO FORM-E
                   MOVE FORM-E             TO EXCEL-FORMANDOS
                   MOVE QT-FIL-FORM-WK     TO FIL-FORM-E
                   MOVE FIL-FORM-E         TO EXCEL-FILME-FORM
                   MOVE QT-FOT-FORM-WK     TO FOT-FORM-E
                   MOVE FOT-FORM-E         TO EXCEL-FOT-FORM
                   MOVE IDENTIFICADOR-WK   TO MASC-IDENTIFICADOR
                   MOVE MASC-IDENTIFICADOR TO EXCEL-IDENTIFICADOR

                   WRITE REG-EXCEL
              END-READ
           END-PERFORM.

           PERFORM TOTALIZA-EXCEL

           MOVE SPACES TO EXCEL-DATA
                          EXCEL-FOTOGRAFO
                          EXCEL-CONTRATO
                          EXCEL-IDENTIFICACAO
                          EXCEL-CIDADE
                          EXCEL-EVENTO
                          EXCEL-FILME
                          EXCEL-TIPO-FILME
                          EXCEL-FORMANDOS
                          EXCEL-FILME-FORM
                          EXCEL-FOT-FORM
                          EXCEL-IDENTIFICADOR

           MOVE "TOT GERAL "    TO EXCEL-DATA
           MOVE TOTAL-FILME-G   TO QTDE-E
           MOVE QTDE-E          TO EXCEL-FILME
           MOVE TOTAL-FORM-G    TO QTDE-E
           MOVE QTDE-E          TO EXCEL-FORMANDOS
           COMPUTE FIL-FORM-E = TOTAL-FILME-G / TOTAL-FORM-G
           MOVE FIL-FORM-E      TO EXCEL-FILME-FORM
           COMPUTE FOT-FORM-E = TOTAL-FOTO-G / TOTAL-FORM-G
           MOVE FOT-FORM-E      TO EXCEL-FOT-FORM

           WRITE REG-EXCEL

           CLOSE EXCEL.

       TOTALIZA-EXCEL SECTION.
           MOVE SPACES TO EXCEL-DATA
                          EXCEL-FOTOGRAFO
                          EXCEL-CONTRATO
                          EXCEL-IDENTIFICACAO
                          EXCEL-CIDADE
                          EXCEL-EVENTO
                          EXCEL-FILME
                          EXCEL-TIPO-FILME
                          EXCEL-FORMANDOS
                          EXCEL-FILME-FORM
                          EXCEL-FOT-FORM
                          EXCEL-IDENTIFICADOR

           MOVE SPACES          TO EXCEL-DATA
           MOVE TOTAL-FILME     TO QTDE-E
           MOVE QTDE-E          TO EXCEL-FILME
           MOVE TOTAL-FORM      TO QTDE-E
           MOVE QTDE-E          TO EXCEL-FORMANDOS
           COMPUTE FIL-FORM-E = TOTAL-FILME / TOTAL-FORM
           MOVE FIL-FORM-E      TO EXCEL-FILME-FORM
           COMPUTE FOT-FORM-E = TOTAL-FOTO / TOTAL-FORM
           MOVE FOT-FORM-E      TO EXCEL-FOT-FORM

           ADD TOTAL-FILME    TO TOTAL-FILME-G
           ADD TOTAL-FOTO     TO TOTAL-FOTO-G
           ADD TOTAL-FORM     TO TOTAL-FORM-G
           MOVE ZEROS TO TOTAL-FILME TOTAL-FOTO TOTAL-FORM

           WRITE REG-EXCEL

           MOVE SPACES TO EXCEL-DATA
                          EXCEL-FOTOGRAFO
                          EXCEL-CONTRATO
                          EXCEL-IDENTIFICACAO
                          EXCEL-CIDADE
                          EXCEL-EVENTO
                          EXCEL-FILME
                          EXCEL-TIPO-FILME
                          EXCEL-FORMANDOS
                          EXCEL-FILME-FORM
                          EXCEL-FOT-FORM
                          EXCEL-IDENTIFICADOR

           WRITE REG-EXCEL.

       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE SPACES TO IDENTIFICACAO-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO.

       POPUP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "COP040T"
           MOVE PASSAR-STRING-1(22: 11)  TO GS-DESC-CONTRATO
           MOVE PASSAR-STRING-1(52: 4)   TO GS-CONTRATO.

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

           EVALUATE GS-TIPO
             WHEN 1
               MOVE GS-DATA-INI    TO DATA-INV DATA-E
               MOVE DATA-E         TO ORDEM-SOLIC-REL(17: 11)
               MOVE "a "           TO ORDEM-SOLIC-REL(28: 2)
               CALL "GRIDAT2" USING DATA-INV
               MOVE DATA-INV       TO DATA-INI
               MOVE GS-DATA-FIM    TO DATA-INV DATA-E
               MOVE DATA-E         TO ORDEM-SOLIC-REL(30: 10)
               CALL "GRIDAT2" USING DATA-INV
               MOVE DATA-INV       TO DATA-FIM
               MOVE ZEROS          TO SEQ-L100
               MOVE DATA-INI       TO DATA-MOVTO-L100
               MOVE ZEROS          TO SEQ-WK
               START LBD100 KEY IS NOT < CHAVE-L100 INVALID KEY
                      MOVE "10" TO ST-LBD100
               END-START
             WHEN 2
               MOVE GS-CONTRATO    TO CONTRATO-L100
               MOVE ZEROS          TO FOTOGRAFO-L100
               START LBD100 KEY IS NOT < ALT1-L100 INVALID KEY
                     MOVE "10" TO ST-LBD100
               END-START
           END-EVALUATE.
           MOVE ZEROS TO SEQ-WK
           PERFORM UNTIL ST-LBD100 = "10"
             READ LBD100 NEXT RECORD AT END MOVE "10" TO ST-LBD100
              NOT AT END
               IF GS-TIPO = 1
                  IF DATA-MOVTO-L100 > DATA-FIM
                     MOVE "10" TO ST-LBD100
                  ELSE PERFORM MOVER-DADOS-WORK
                  END-IF
               ELSE
                 IF CONTRATO-L100 <> GS-CONTRATO
                    MOVE "10" TO ST-LBD100
                 ELSE PERFORM MOVER-DADOS-WORK
                 END-IF
               END-IF

             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           ADD 1                   TO SEQ-WK
           MOVE DATA-MOVTO-L100    TO DATA-MOVTO-WK GS-EXIBE-VENCTO
           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE FOTOGRAFO-L100     TO CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
           END-READ
           MOVE NOME-CG01          TO FOTOGRAFO-WK
           MOVE QTDE-FILMES-L100   TO QT-FILME-WK
           MOVE TIPO-FILME-L100    TO CODIGO-LB21
           READ LBD021 INVALID KEY MOVE SPACES TO DESCRICAO-LB21
           END-READ
           MOVE DESCRICAO-LB21     TO TIPO-FILME-WK
           MOVE CONTRATO-L100      TO CONTRATO-WK NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE SPACES TO IDENTIFICACAO-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40 TO IDENTIFICACAO-WK
           MOVE QTDE-FORM-CO40     TO QT-FORM-WK

           COMPUTE QT-FOTO-WK = QTDE-FILMES-L100 * QT-FOTOS-LB21
           COMPUTE QT-FIL-FORM-WK = QT-FILME-WK / QT-FORM-WK
           COMPUTE QT-FOT-FORM-WK = QT-FOTO-WK / QT-FORM-WK

           MOVE CIDADE-CO40        TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
           END-READ
           MOVE NOME-CID           TO CIDADE-WK
           MOVE EVENTO-L100        TO CODIGO-CO03
           READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03
           END-READ
           MOVE NOME-CO03          TO EVENTO-WK

           MOVE IDENTIFICADOR-L100 TO IDENTIFICADOR-WK

           WRITE REG-WORK.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE ZEROS TO TOTAL-FILME-G TOTAL-FILME TOTAL-FORM
                         TOTAL-FORM-G TOTAL-FOTO TOTAL-FOTO-G.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA

           MOVE "TOTAL GERAL: " TO GS-LINDET(1: 30)
           MOVE TOTAL-FILME-G   TO QTDE-E
           MOVE QTDE-E          TO GS-LINDET(66: 10)
           MOVE TOTAL-FORM-G    TO QTDE-E
           MOVE QTDE-E          TO GS-LINDET(83: 9)
           COMPUTE FIL-FORM-E = TOTAL-FILME-G / TOTAL-FORM-G
           MOVE FIL-FORM-E      TO GS-LINDET(92: 9)
           COMPUTE FOT-FORM-E = TOTAL-FOTO-G / TOTAL-FORM-G
           MOVE FOT-FORM-E      TO GS-LINDET(101: 8).

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE "REFRESH-DATA" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "DATA-MOVTO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < DATA-MOVTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "FOTOGRAFO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < FOTOGRAFO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "CIDADE    " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CIDADE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "TIPO-FILME" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < TIPO-FILME-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF DATA-MOVTO-ANT NOT = ZEROS
                 IF DATA-MOVTO-ANT NOT = DATA-MOVTO-WK PERFORM TOTALIZA
             WHEN 2
              IF FOTOGRAFO-ANT  NOT = SPACES
                 IF FOTOGRAFO-ANT NOT = FOTOGRAFO-WK PERFORM TOTALIZA
             WHEN 3
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK PERFORM TOTALIZA
             WHEN 4
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK PERFORM TOTALIZA
             WHEN 5
              IF TIPO-FILME-ANT NOT = SPACES
                 IF TIPO-FILME-ANT NOT = TIPO-FILME-WK PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE DATA-MOVTO-WK      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(1: 11)
           MOVE FOTOGRAFO-WK       TO GS-LINDET(12: 16)
           MOVE CONTRATO-WK        TO GS-LINDET(28: 5)
           MOVE IDENTIFICACAO-WK   TO GS-LINDET(33: 11)
           MOVE CIDADE-WK          TO GS-LINDET(44: 11)
           MOVE EVENTO-WK          TO GS-LINDET(55: 11)
           MOVE QT-FILME-WK        TO QTDE-E
           ADD QT-FILME-WK         TO TOTAL-FILME.
           ADD QT-FOTO-WK          TO TOTAL-FOTO.
           ADD QT-FORM-WK          TO TOTAL-FORM.
           MOVE QTDE-E             TO GS-LINDET(66: 10)
           MOVE TIPO-FILME-WK      TO GS-LINDET(76: 11)
           MOVE QT-FORM-WK         TO FORM-E
           MOVE FORM-E             TO GS-LINDET(87: 5)
           MOVE QT-FIL-FORM-WK     TO FIL-FORM-E
           MOVE FIL-FORM-E         TO GS-LINDET(92: 9)
           MOVE QT-FOT-FORM-WK     TO FOT-FORM-E
           MOVE FOT-FORM-E         TO GS-LINDET(101: 8)
           MOVE IDENTIFICADOR-WK   TO MASC-IDENTIFICADOR
           MOVE MASC-IDENTIFICADOR TO GS-LINDET(111:9).

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO DATA-MOVTO-ANT CONTRATO-ANT
           MOVE SPACES TO FOTOGRAFO-ANT CIDADE-ANT TIPO-FILME-ANT.
       MOVER-CHAVE-ANT SECTION.
           MOVE DATA-MOVTO-WK     TO DATA-MOVTO-ANT.
           MOVE FOTOGRAFO-WK      TO FOTOGRAFO-ANT.
           MOVE CIDADE-WK         TO CIDADE-ANT
           MOVE TIPO-FILME-WK     TO TIPO-FILME-ANT
           MOVE CONTRATO-WK       TO CONTRATO-ANT.
       TOTALIZA SECTION.
           MOVE SPACES          TO GS-LINDET
           MOVE TOTAL-FILME     TO QTDE-E
           MOVE QTDE-E          TO GS-LINDET(66: 10)
           MOVE TOTAL-FORM      TO QTDE-E
           MOVE QTDE-E          TO GS-LINDET(83: 9)
           COMPUTE FIL-FORM-E = TOTAL-FILME / TOTAL-FORM
           MOVE FIL-FORM-E      TO GS-LINDET(92: 9)
           COMPUTE FOT-FORM-E = TOTAL-FOTO / TOTAL-FORM
           MOVE FOT-FORM-E      TO GS-LINDET(101: 8).

           ADD TOTAL-FILME    TO TOTAL-FILME-G
           ADD TOTAL-FOTO     TO TOTAL-FOTO-G
           ADD TOTAL-FORM     TO TOTAL-FORM-G
           MOVE ZEROS TO TOTAL-FILME TOTAL-FOTO TOTAL-FORM.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP206" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO TOTAL-FILME TOTAL-FILME-G TOTAL-FOTO
                         TOTAL-FOTO-G TOTAL-FORM TOTAL-FORM-G.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                      PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL
           MOVE SPACES          TO LINDET-REL
           MOVE "TOTAL GERAL: " TO LINDET-REL(1: 30)
           MOVE TOTAL-FILME-G   TO QTDE-E
           MOVE QTDE-E          TO LINDET-REL(66: 10)
           MOVE TOTAL-FORM-G    TO QTDE-E
           MOVE QTDE-E          TO LINDET-REL(83: 9)
           COMPUTE FIL-FORM-E = TOTAL-FILME-G / TOTAL-FORM-G
           MOVE FIL-FORM-E      TO LINDET-REL(92: 9)
           COMPUTE FOT-FORM-E = TOTAL-FOTO-G / TOTAL-FORM-G
           MOVE FOT-FORM-E      TO LINDET-REL(101: 8)
           MOVE IDENTIFICADOR-WK   TO MASC-IDENTIFICADOR
           MOVE MASC-IDENTIFICADOR TO GS-LINDET(111:9).

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF DATA-MOVTO-ANT NOT = ZEROS
                 IF DATA-MOVTO-ANT NOT = DATA-MOVTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF FOTOGRAFO-ANT  NOT = SPACES
                 IF FOTOGRAFO-ANT NOT = FOTOGRAFO-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF TIPO-FILME-ANT NOT = SPACES
                 IF TIPO-FILME-ANT NOT = TIPO-FILME-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS
           MOVE GS-LINDET TO LINDET-REL

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES          TO LINDET-REL
           MOVE "TOTAL......: " TO LINDET-REL(1: 30)
           MOVE TOTAL-FILME     TO QTDE-E
           MOVE QTDE-E          TO LINDET-REL(66: 10)
           MOVE TOTAL-FORM      TO QTDE-E
           MOVE QTDE-E          TO LINDET-REL(83: 9)
           COMPUTE FIL-FORM-E = TOTAL-FILME / TOTAL-FORM
           MOVE FIL-FORM-E      TO LINDET-REL(92: 9)
           COMPUTE FOT-FORM-E = TOTAL-FOTO / TOTAL-FORM
           MOVE FOT-FORM-E      TO LINDET-REL(101: 8).

           ADD TOTAL-FILME    TO TOTAL-FILME-G
           ADD TOTAL-FOTO     TO TOTAL-FOTO-G
           ADD TOTAL-FORM     TO TOTAL-FORM-G
           MOVE ZEROS TO TOTAL-FILME TOTAL-FOTO TOTAL-FORM.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.

           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
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
           CLOSE CAD010 COD003 COD040 LBD021 LBD100 CGD001.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
