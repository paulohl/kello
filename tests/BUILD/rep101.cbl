       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP101.
      *DATA: 19-11-2014
      *AUTOR: ALFREDO SAVIOLLI
      *PROGRAMA: Relação de eventos
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX012.
           COPY COPX001.
           COPY COPX003.
           COPY COPX040.
           COPY COPX060.
           COPY COPX061.
           COPY IEPX011.
           COPY REPX101a.
           COPY CAPX004.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK =
                   CONTRATO-WK
                   EVENTO-WK
                  ALTERNATE RECORD KEY IS CURSO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS EVENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS STATUS-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LOCAL-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
           COPY CAPW010.
           COPY CAPW012.
           COPY COPW001.
           COPY COPW003.
           COPY COPW040.
           COPY COPW060.
           COPY COPW061.
           COPY IEPW011.
           COPY REPW101a.
           COPY CAPW004.

       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  CURSO-WK            PIC X(15).
           05  STATUS-WK           PIC X(11).
           05  DATA-WK             PIC 9(08).
           05  EVENTO-WK           PIC X(20).
           05  LOCAL-WK            PIC X(25).
           05  CIDADE-WK           PIC X(12).
           05  QT-FORM-WK          PIC 9(4).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "REP101.CPB".
           COPY "REP101.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-COD061             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-REP101A            PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.

           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  OK                    PIC X(01)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(8)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  MESANO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.

           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  CURSO-ANT             PIC X(15)    VALUE SPACES.
           05  STATUS-ANT            PIC X(11)    VALUE SPACES.
           05  DATA-ANT              PIC 9(08)    VALUE ZEROS.
           05  EVENTO-ANT            PIC X(20)    VALUE SPACES.
           05  LOCAL-ANT             PIC X(25)    VALUE SPACES.
           05  CIDADE-ANT            PIC X(12)    VALUE SPACES.

           05  QT-CURSO              PIC 9        VALUE ZEROS.
           05  CURSO-W               PIC X(15)    VALUE SPACES.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.
           05  DATAINI               PIC 9(8)     VALUE ZEROS.
           05  DATAFIM               PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FORM            PIC 9(8)     VALUE ZEROS.
           05  TOTAL-EVENTOS         PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FORM-G          PIC 9(8)     VALUE ZEROS.
           05  TOTAL-EVENTOS-G       PIC 9(8)     VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  CONTINUAR             PIC X(01)    VALUE SPACES.
           05  ACHEI                 PIC X(01)    VALUE SPACES.
           05  QTDE-NAO              PIC 9(03)    VALUE ZEROS.
           05  QTDE-REC              PIC 9(03)    VALUE ZEROS.

           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 AUX-DATA                     PIC 9(08).
       01 FILLER REDEFINES AUX-DATA.
          05 AUX-ANO                   PIC 9(04).
          05 AUX-MES                   PIC 9(02).
          05 AUX-DIA                   PIC 9(02).

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
           05  FILLER              PIC X(29)   VALUE
           "RELACAO DE EVENTOS - ORDEM: ".
           05  ORDEM-REL           PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(17)
               VALUE "DATA REALIZACAO: ".
           05  DATAINI-REL         PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  DATAFIM-REL         PIC 99/99/9999.
           05  FILLER              PIC X(03).
           05  FILLER              PIC X(13)
               VALUE "QUAL CIDADE: ".
           05  QUAL-CIDADE-REL     PIC X(20).

       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(107)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(108)  VALUE
           "CONT IDENTIF.        CIDADE       EVENTO                 DT
      -    "EVEN  STATUS      LOCAL                     QTDE".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(109)  VALUE SPACES.

       01  CAB-TOT.
           05 CAB-TOT-DESCRICAO    PIC X(16).
           05 CAB-TOT-EVENTOS      PIC ZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 FILLER               PIC X(07)
              VALUE "EVENTOS".
           05 FILLER               PIC X(20).
           05 CAB-TOT-FORMANDOS    PIC ZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 FILLER               PIC X(09)
              VALUE "FORMANDOS".


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

           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060
           MOVE "COD061"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD061
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011
           MOVE "REP101A" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-REP101A
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004

           OPEN INPUT CAD010 CAD012 COD001 COD003 COD040 COD060 COD061
                      IED011 CAD004
           OPEN I-O   REP101A
           CLOSE      REP101A
           OPEN INPUT REP101A.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-COD061 <> "00"
              MOVE "ERRO ABERTURA COD061: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD061 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-REP101A <> "00"
              MOVE "ERRO ABERTURA REP101A: "  TO GS-MENSAGEM-ERRO
              MOVE ST-REP101A TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM VERIFICAR-SENHA-STATUS
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
               WHEN GS-LE-REGIAO-TRUE
                    PERFORM LE-REGIAO
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LE-CIDADE
               WHEN GS-LE-EVENTO-TRUE
                    PERFORM LE-EVENTO
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM CHAMAR-POPUP-REGIAO
               WHEN GS-LE-STATUS-TRUE
                    PERFORM LE-STATUS
               WHEN GS-CHAMAR-POP-STATUS-TRUE
                    PERFORM CHAMAR-POPUP-STATUS
               WHEN GS-CHAMAR-POP-CIDADE-TRUE
                    PERFORM CHAMAR-POPUP-CIDADE
               WHEN GS-CHAMAR-POP-EVENTO-TRUE
                    PERFORM CHAMAR-POPUP-EVENTO
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR
               WHEN GS-CARREGAR-STATUS-TRUE
                    PERFORM CARREGAR-STATUS
               WHEN GS-GRAVA-STATUS-TRUE
                    PERFORM GRAVA-STATUS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB12
           NOT INVALID KEY
               ENABLE-OBJECT PB12.

           CLOSE CAD004.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GRAVA-STATUS SECTION.
           CLOSE    REP101A
           OPEN I-O REP101A

           INITIALIZE REG-REP101A
           START REP101A KEY IS NOT LESS CODIGO-REP101A INVALID KEY
                MOVE "10" TO ST-REP101A.
           PERFORM UNTIL ST-REP101A = "10"
                READ REP101A NEXT AT END
                     MOVE "10" TO ST-REP101A
                NOT AT END
                     DELETE REP101A INVALID KEY
                         MOVE "Erro de Exclusão...REP101A" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                     END-DELETE
                END-READ
           END-PERFORM

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-REP101A
               WRITE REG-REP101A
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      REP101A
           OPEN INPUT REP101A.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-REP101A
           START REP101A KEY IS NOT LESS CODIGO-REP101A INVALID KEY
               MOVE "10" TO ST-REP101A.

           PERFORM UNTIL ST-REP101A = "10"
               READ REP101A NEXT AT END
                    MOVE "10" TO ST-REP101A
               NOT AT END
                    MOVE CODIGO-REP101A TO CODIGO-CO01
                    READ COD001 NOT INVALID KEY
                         MOVE "S"              TO ACHEI
                         MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                         MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                         MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                    END-READ
               END-READ
           END-PERFORM

           IF ACHEI = "N"
              CLOSE      REP101A
              OPEN I-O   REP101A
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO CODIGO-REP101A
                        WRITE REG-REP101A

                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM
              CLOSE      REP101A
              OPEN INPUT REP101A.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.

       INCLUIR SECTION.
           MOVE "Você Deseja Incluir o Status?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE GS-STATUS        TO GS-LINHA-STATUS(1:2)
              MOVE GS-DESC-STATUS   TO GS-LINHA-STATUS(4:30)
              MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       LE-STATUS SECTION.
           MOVE GS-STATUS              TO CODIGO-CO01
           READ COD001 INVALID KEY
                MOVE SPACES            TO STATUS-CO01
           END-READ
           MOVE STATUS-CO01            TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       CHAMAR-POPUP-STATUS SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP001T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       CHAMAR-POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CAP010T"
           EVALUATE GS-QUAL-CIDADE
               WHEN 1 MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-CIDADE
                      MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE
               WHEN 2 MOVE PASSAR-STRING-1(1: 30) TO
                           GS-DESC-CIDADE-EVENTO
                      MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE-EVENTO
           END-EVALUATE.

       CHAMAR-POPUP-EVENTO SECTION.
           CALL   "COP003T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "COP003T".
           MOVE PASSAR-STRING-1(33: 05) TO GS-EVENTO
           MOVE PASSAR-STRING-1(1:30)   TO GS-DESC-EVENTO.

       VERIFICAR-IGUAL SECTION.
           MOVE 0   TO GS-FLAG-CRITICA
           MOVE "N" TO ACHEI
           MOVE 1   TO GS-CONT
           MOVE SPACES TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               IF GS-LINHA-STATUS(1:2) = GS-STATUS
                  MOVE "S" TO ACHEI
                  EXIT PERFORM
               ELSE
                  ADD 1 TO GS-CONT
                  MOVE SPACES TO GS-LINHA-STATUS
                  MOVE "LER-LINHA" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               END-IF
           END-PERFORM

           IF ACHEI = "S"
              MOVE "Status já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.


       CHAMAR-POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2)  TO GS-REGIAO.
           MOVE PASSAR-STRING-1(1: 30)  TO GS-DESC-REGIAO.

       LE-REGIAO SECTION.
           MOVE GS-REGIAO       TO CODIGO-REG.
           READ CAD012 INVALID KEY
                MOVE SPACES     TO NOME-REG.
           MOVE NOME-REG        TO GS-DESC-REGIAO.

       LE-CIDADE SECTION.
           EVALUATE GS-QUAL-CIDADE
               WHEN 1 MOVE GS-CIDADE        TO CIDADE
                      READ CAD010 INVALID KEY
                           MOVE SPACES      TO NOME-CID
                      END-READ
                      MOVE NOME-CID         TO GS-DESC-CIDADE
               WHEN 2 MOVE GS-CIDADE-EVENTO TO CIDADE
                      READ CAD010 INVALID KEY
                           MOVE SPACES      TO NOME-CID
                      END-READ
                      MOVE NOME-CID         TO GS-DESC-CIDADE-EVENTO
           END-EVALUATE.

       LE-EVENTO SECTION.
           MOVE GS-EVENTO       TO CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE SPACES     TO NOME-CO03.
           MOVE NOME-CO03       TO GS-DESC-EVENTO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME.

           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           STRING GS-DATA-INI(5:4)
                  GS-DATA-INI(3:2)
                  GS-DATA-INI(1:2) INTO DATAINI

           STRING GS-DATA-FIM(5:4)
                  GS-DATA-FIM(3:2)
                  GS-DATA-FIM(1:2) INTO DATAFIM


           INITIALIZE REG-COD060
           MOVE DATAINI TO DATAREALIZA-CO60
           START COD060 KEY IS NOT < DATAREALIZA-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.

           PERFORM UNTIL ST-COD060 = "10"
                 READ COD060 NEXT RECORD AT END
                      MOVE "10" TO ST-COD060
                 NOT AT END
                     IF DATAREALIZA-CO60 > DATAFIM
                        MOVE "10"             TO ST-COD060
                     ELSE
                        IF CIDADE-CO60 IS NOT NUMERIC
                           MOVE 0 TO CIDADE-CO60
                        END-IF
                        MOVE DATAREALIZA-CO60 TO AUX-DATA
                        IF AUX-MES < 1 OR AUX-MES > 12
                           MOVE SPACES TO MENSAGEM
                           STRING "CONTRATO = " NR-CONTRATO-CO60 X"0A"
                                  "ITEM = " ITEM-CO60 X"0A"
                                  "DATA REALIZACAO = " AUX-DIA "/"
                                  AUX-MES "/" AUX-ANO INTO MENSAGEM
                             MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                        ELSE
                           IF GS-CONTRATO = 0 OR NR-CONTRATO-CO60
                              IF GS-EVENTO = 0 OR CODEVENTO-CO60
                                IF GS-DT-PREV-REAL = 9 OR
                                   DT-PREV-REAL-CO60
                                   IF GS-CIDADE-EVENTO = 0 OR
                                      CIDADE-CO60
                                      MOVE NR-CONTRATO-CO60 TO
                                           NR-CONTRATO-CO40
                                      READ COD040 INVALID KEY
                                           MOVE ZEROS TO CIDADE-CO40
                                      END-READ
                                      PERFORM PESQUISAR-STATUS
                                      IF ACHEI = "S"
                                         MOVE CIDADE-CO40  TO CIDADE
                                         READ CAD010 INVALID KEY
                                              MOVE SPACES TO NOME-CID
                                         END-READ
                                         IF GS-CIDADE = 0 OR CIDADE-CO40
                                            IF GS-UF = SPACES OR
                                               UF-CID
                                               IF GS-REGIAO = 0 OR
                                                  REGIAO-CID
                                                  PERFORM MOVER-WORK
                                               END-IF
                                            END-IF
                                         END-IF
                                      END-IF
                                   END-IF
                                 END-IF
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                 END-READ
           END-PERFORM

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-WORK SECTION.
           MOVE DATAREALIZA-CO60        TO GS-EXIBE-VENCTO
           MOVE "TELA-AGUARDA1"         TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE NR-CONTRATO-CO60        TO CONTRATO-WK
           MOVE IDENTIFICACAO-CO40      TO CURSO-WK
           MOVE DATAREALIZA-CO60        TO DATA-WK
           MOVE LOCAL-CO60              TO LOCAL-WK
      *ADICIONADO DIA 22/06/2016 CIDADE QUE SERÁ REALIZADO O EVENTO
           IF GS-QUAL-CID = 1
              IF CIDADE-CO60 > 0
                 MOVE CIDADE-CO60          TO CIDADE
                 READ CAD010 INVALID KEY
                      MOVE SPACES          TO NOME-CID
                 END-READ
              END-IF
           END-IF
      *FIM ADICIONADO DIA 22/06/2016 CIDADE QUE SERÁ REALIZADO O EVENTO

           MOVE NOME-CID                TO CIDADE-WK
           MOVE QT-PARTICIPANTE-CO60    TO QT-FORM-WK
           MOVE CODEVENTO-CO60          TO CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE "*****"            TO NOME-CO03
           END-READ
           MOVE NOME-CO03               TO EVENTO-WK
           EVALUATE DT-PREV-REAL-CO60
               WHEN 1 MOVE "PREVISTO"   TO STATUS-WK
               WHEN 2 MOVE "CONFIRMADA" TO STATUS-WK
               WHEN 3 MOVE SPACES       TO STATUS-WK
           END-EVALUATE
           WRITE REG-WORK.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
                   READ WORK NEXT RECORD AT END
                        MOVE "10" TO ST-WORK
                   NOT AT END
                        PERFORM MOVER-DADOS-LINDET
                   END-READ
           END-PERFORM.
           PERFORM TOTALIZA


           MOVE "TOTAL GERAL...: " TO CAB-TOT-DESCRICAO
           MOVE TOTAL-EVENTOS-G    TO CAB-TOT-EVENTOS
           MOVE TOTAL-FORM-G       TO CAB-TOT-FORMANDOS

           MOVE CAB-TOT            TO GS-LINDET
           MOVE "INSERE-LIST"      TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.


      *    MOVE "TOTAL GERAL: " TO GS-LINDET(1: 30)
      *    MOVE TOTAL-FORM-G    TO GS-LINDET(101: 8)
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.

       PESQUISAR-STATUS SECTION.
           MOVE "N" TO ACHEI

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES OR ACHEI = "S"
               IF GS-LINHA-STATUS(1:2) = STATUS-CO40
                  MOVE "S" TO ACHEI
               END-IF
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO CONTRATO-WK
                START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "IDENTIFICACAO" TO GS-DESCR-ORDEM
                MOVE SPACES TO CURSO-WK
                START WORK KEY IS NOT < CURSO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CIDADE"      TO GS-DESCR-ORDEM
                MOVE SPACES TO CIDADE-WK
                START WORK KEY IS NOT < CIDADE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "EVENTO" TO GS-DESCR-ORDEM
                MOVE SPACES TO EVENTO-WK
                START WORK KEY IS NOT < EVENTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "DATA PREVISTA" TO GS-DESCR-ORDEM
                MOVE ZEROS TO DATA-WK
                START WORK KEY IS NOT < DATA-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "STATUS"        TO GS-DESCR-ORDEM
                MOVE SPACES TO STATUS-WK
                START WORK KEY IS NOT < STATUS-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 7
                MOVE "LOCAL"  TO GS-DESCR-ORDEM
                MOVE SPACES TO LOCAL-WK
                START WORK KEY IS NOT < LOCAL-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF CURSO-ANT  NOT = SPACES
                 IF CURSO-ANT NOT = CURSO-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF EVENTO-ANT NOT = SPACES
                 IF EVENTO-ANT NOT = EVENTO-WK
                    PERFORM TOTALIZA
             WHEN 5
              IF DATA-ANT NOT = SPACES
                 IF DATA-ANT NOT = DATA-WK
                    PERFORM TOTALIZA
             WHEN 6
              IF STATUS-ANT NOT = SPACES
                 IF STATUS-ANT NOT = STATUS-WK
                    PERFORM TOTALIZA
             WHEN 7
              IF LOCAL-ANT NOT = SPACES
                 IF LOCAL-ANT NOT = LOCAL-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS SECTION.
           MOVE SPACES            TO GS-LINDET

           MOVE CONTRATO-WK       TO GS-LINDET(1: 5)
           MOVE CURSO-WK          TO GS-LINDET(6: 16)
           MOVE CIDADE-WK         TO GS-LINDET(22:13)
           MOVE EVENTO-WK         TO GS-LINDET(35:21)
           MOVE DATA-WK           TO AUX-DATA
           STRING AUX-DIA "/" AUX-MES "/" AUX-ANO INTO GS-LINDET(56:11)
           MOVE STATUS-WK         TO GS-LINDET(67:12)
           MOVE LOCAL-WK          TO GS-LINDET(79:26)
           MOVE QT-FORM-WK        TO GS-LINDET(105:5)

           ADD QT-FORM-WK         TO TOTAL-FORM

           ADD 1                  TO TOTAL-EVENTOS.

       ZERA-VARIAVEIS SECTION.
           INITIALIZE CONTRATO-ANT
                      CURSO-ANT
                      STATUS-ANT
                      DATA-ANT
                      EVENTO-ANT
                      LOCAL-ANT
                      CIDADE-ANT
                      TOTAL-FORM-G
                      TOTAL-EVENTOS-G
                      TOTAL-FORM
                      TOTAL-EVENTOS.

       MOVER-CHAVE-ANT SECTION.
           MOVE CURSO-WK          TO CURSO-ANT
           MOVE STATUS-WK         TO STATUS-ANT
           MOVE DATA-WK           TO DATA-ANT
           MOVE EVENTO-WK         TO EVENTO-ANT
           MOVE LOCAL-WK          TO LOCAL-ANT
           MOVE CIDADE-WK         TO CIDADE-ANT.

       TOTALIZA SECTION.
           MOVE "TOTAL ORDEM...: " TO CAB-TOT-DESCRICAO
           MOVE TOTAL-EVENTOS      TO CAB-TOT-EVENTOS
           MOVE TOTAL-FORM         TO CAB-TOT-FORMANDOS

           MOVE CAB-TOT            TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

      *    MOVE SPACES        TO GS-LINDET
      *    MOVE TOTAL-FORM    TO GS-LINDET(101: 8)
           ADD TOTAL-FORM     TO TOTAL-FORM-G
           ADD TOTAL-EVENTOS  TO TOTAL-EVENTOS-G
           MOVE ZEROS         TO TOTAL-FORM
           MOVE ZEROS         TO TOTAL-EVENTOS
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP101" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           MOVE GS-DATA-INI   TO DATAINI-REL
           MOVE GS-DATA-FIM   TO DATAFIM-REL

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           MOVE "TOTAL GERAL...: " TO CAB-TOT-DESCRICAO
           MOVE TOTAL-EVENTOS-G    TO CAB-TOT-EVENTOS
           MOVE TOTAL-FORM-G       TO CAB-TOT-FORMANDOS
           MOVE CAB-TOT            TO LINDET-REL
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.


      *    MOVE SPACES        TO LINDET-REL
      *    MOVE TOTAL-FORM-G  TO LINDET-REL(101: 8)
      *    WRITE REG-RELAT FROM LINDET-REL.
      *    ADD 1 TO LIN.

           COPY DESCONDENSA.
       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF CURSO-ANT  NOT = SPACES
                 IF CURSO-ANT NOT = CURSO-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF EVENTO-ANT NOT = SPACES
                 IF EVENTO-ANT NOT = EVENTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF DATA-ANT NOT = SPACES
                 IF DATA-ANT NOT = DATA-WK
                    PERFORM TOTALIZA-REL
             WHEN 6
              IF STATUS-ANT NOT = SPACES
                 IF STATUS-ANT NOT = STATUS-WK
                    PERFORM TOTALIZA-REL
             WHEN 7
              IF LOCAL-ANT NOT = SPACES
                 IF LOCAL-ANT NOT = LOCAL-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.

           MOVE CONTRATO-WK       TO LINDET-REL(1: 5)
           MOVE CURSO-WK          TO LINDET-REL(6: 16)
           MOVE CIDADE-WK         TO LINDET-REL(22:13)
           MOVE EVENTO-WK         TO LINDET-REL(35:21)
           MOVE DATA-WK           TO AUX-DATA
           STRING AUX-DIA "/" AUX-MES "/" AUX-ANO INTO LINDET-REL(56:11)
           MOVE STATUS-WK         TO LINDET-REL(67:12)
           MOVE LOCAL-WK          TO LINDET-REL(79:26)
           MOVE QT-FORM-WK        TO LINDET-REL(105:5)

           ADD QT-FORM-WK         TO TOTAL-FORM

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.

       TOTALIZA-REL SECTION.
           MOVE "TOTAL ORDEM...: " TO CAB-TOT-DESCRICAO
           MOVE TOTAL-EVENTOS      TO CAB-TOT-EVENTOS
           MOVE TOTAL-FORM         TO CAB-TOT-FORMANDOS
           MOVE CAB-TOT            TO LINDET-REL
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.

           ADD TOTAL-EVENTOS       TO TOTAL-EVENTOS-G
           ADD TOTAL-FORM          TO TOTAL-FORM-G

           MOVE ZEROS              TO TOTAL-EVENTOS
                                      TOTAL-FORM

           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.






       CABECALHO SECTION.
           EVALUATE GS-QUAL-CID
               WHEN 1 MOVE "EVENTO"   TO QUAL-CIDADE-REL
               WHEN 2 MOVE "CONTRATO" TO QUAL-CIDADE-REL
           END-EVALUATE
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
           CLOSE CAD010 CAD012 COD001 COD003 COD040 COD060 COD061 IED011
                 REP101A

           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
