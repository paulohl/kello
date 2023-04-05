       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPP061.
      *DATA: 21/07/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: CONTAS A PAGAR EM ABERTO
      *FUNÇÃO: Listar todos os títulos que estiverem dentro do intervalo
      *        de vencimento. As ordens serão: Vencto, Portador, Forne-
      *        cedor e tipo-fornecedor, valor.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CXPX020.
           COPY CPPX020.
           COPY CAPX018.
           COPY CAPX019.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = FORNEC-WK SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK = VENCTO-WK VALOR-WK
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-FORNEC-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PORTADOR-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NOME-FORN-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CTA-APURACAO-WK
                      WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CXPW020.
       COPY CAPW018.
       COPY CAPW019.
       COPY CPPW020.
       FD  WORK.
       01  REG-WORK.
           05  FORNEC-WK           PIC 9(6).
           05  SEQ-WK              PIC 9(5).
           05  NOME-FORN-WK        PIC X(20).
           05  PORTADOR-WK         PIC 9(2).
           05  TIPO-FORNEC-WK      PIC 9(2).
           05  VENCTO-WK           PIC 9(8).
           05  DESCRICAO-WK        PIC X(30).
           05  VALOR-WK            PIC 9(8)V99.
           05  PREV-DEF-WK         PIC 9.
           05  SUSPENSO-WK         PIC 9.
           05  CTA-APURACAO-WK     PIC 9(5).
           05  RESPONSAVEL-WK      PIC X(5).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CPP061.CPB".
           COPY "CPP061.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD019             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
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
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  FORNEC-ANT            PIC 9(6)     VALUE ZEROS.
           05  NOME-FORN-ANT         PIC X(20)    VALUE SPACES.
           05  PORTADOR-ANT          PIC 9(2)     VALUE ZEROS.
           05  TIPO-FORN-ANT         PIC 9(2)     VALUE ZEROS.
           05  VENCTO-ANT            PIC 9(8)     VALUE ZEROS.
           05  CTA-APUR-ANT          PIC 9(5)     VALUE ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  FORNEC-W              PIC 9(6)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
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
           05  FILLER              PIC X(36)   VALUE
           "RELACAO DE CONTAS A PAGAR - ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(105)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(105)  VALUE
           "DATA-MOVTO NOME-FORNECEDOR      DESCRICAO
      -    "   TP PO         VALOR         TOTAL RESP. T".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(105)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "TOTAL PERIODO: ".
           05  TOTAL-PERIODO-REL   PIC ZZ.ZZZ.ZZZ,ZZ.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "TOTAL VENCIDO: ".
           05  TOTAL-VENCIDO-REL   PIC ZZ.ZZZ.ZZZ,ZZ.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "TOTAL A VENCER: ".
           05  TOTAL-AVENCER-REL   PIC ZZ.ZZZ.ZZZ,ZZ.

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
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CAD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD019.
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           OPEN INPUT CAD018 CAD019 CGD001 CXD020.
           OPEN I-O CPD020.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD019 <> "00"
              MOVE "ERRO ABERTURA CAD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD019 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM CHAMA-ALTERACAO
      *        WHEN GS-REGRAVA-DADOS-TRUE
      *             PERFORM REGRAVA-DADOS
               WHEN GS-LE-FORNEC-TRUE
                   PERFORM LE-FORNEC
               WHEN GS-LE-PORTADOR-TRUE
                   PERFORM LE-PORTADOR
               WHEN GS-LE-TIPO-FORN-TRUE
                   PERFORM LE-TIPO-FORNEC
               WHEN GS-LE-COD-APUR-TRUE
                   PERFORM LE-COD-APURACAO
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
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

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CALL "CGP001T" USING PASSAR-PARAMETROS
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(33: 6) TO GS-COD-FORN
                    PERFORM LE-FORNEC
             WHEN 2 CALL "CAP019T" USING PASSAR-PARAMETROS
                    CANCEL "CAP019T"
                    MOVE PASSAR-STRING-1(33: 2) TO GS-TIPO-FORN
                    PERFORM LE-FORNEC
             WHEN 3 CALL "CAP018T" USING PASSAR-PARAMETROS
                    CANCEL "CAP018T"
                    MOVE PASSAR-STRING-1(33: 2) TO GS-PORTADOR
                    PERFORM LE-PORTADOR
             WHEN 4 CALL "CXP020T" USING PASSAR-PARAMETROS
                    CANCEL "CXP020T"
                    MOVE PASSAR-STRING-1(52: 5) TO GS-COD-APURACAO
                    PERFORM LE-COD-APURACAO
           END-EVALUATE.
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
       LE-FORNEC SECTION.
           MOVE GS-COD-FORN    TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "*****" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-DESCR-APURACAO.
       LE-PORTADOR SECTION.
           MOVE GS-PORTADOR    TO PORTADOR.
           READ CAD018 INVALID KEY MOVE "******" TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-PORTADOR.
       LE-TIPO-FORNEC SECTION.
           MOVE GS-TIPO-FORN   TO CODIGO-TIPO.
           READ CAD019 INVALID KEY MOVE "****" TO NOME-TIPO.
           MOVE NOME-TIPO          TO GS-DESCR-TIPO-FORN.
       LE-COD-APURACAO SECTION.
           MOVE GS-COD-APURACAO TO CODIGO-COMPL-CX20.
           READ CXD020 INVALID KEY MOVE "*****" TO DESCRICAO-CX20.
           MOVE DESCRICAO-CX20      TO GS-DESCR-APURACAO.
      *VERIFICA-VENCTO-ANT SECTION.
      *    IF GS-VENCTO-INI NOT = VENCTO-INI-ANT
      *       OR GS-VENCTO-FIM NOT = VENCTO-FIM-ANT
      *          PERFORM GRAVA-WORK.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-VENCTO-INI TO DATA-INV VENCTO-INI-ANT
                                     VENCTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VENCTO-INI.
           MOVE GS-VENCTO-FIM TO DATA-INV VENCTO-FIM-ANT
                                     VENCTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VENCTO-FIM.
           MOVE VENCTO-INI TO DATA-VENCTO-CP20
           MOVE ZEROS TO SITUACAO-CP20 FORNEC-CP20
           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                  MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
             READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
              NOT AT END
               IF SITUACAO-CP20 > 1 MOVE "10" TO ST-CPD020
               ELSE
                IF SITUACAO-CP20 = 1 AND DATA-VENCTO-CP20 > VENCTO-FIM
                      MOVE "10" TO ST-CPD020
                ELSE
                IF DATA-VENCTO-CP20 < VENCTO-INI OR
                   DATA-VENCTO-CP20 > VENCTO-FIM CONTINUE
                ELSE
                 IF TIPO-FORN-CP20 = 3 AND PREV-DEF-CP20 = 1 CONTINUE
                 ELSE
                  PERFORM MOVER-DADOS-WORK
                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  WRITE REG-WORK
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           MOVE DATA-VENCTO-CP20    TO VENCTO-WK
                                       GS-EXIBE-VENCTO
           MOVE FORNEC-CP20         TO FORNEC-WK CODIGO-CG01
           IF FORNEC-CP20 NOT = 000123
              READ CGD001 INVALID KEY MOVE "*****" TO NOME-CG01
              END-READ
              MOVE NOME-CG01           TO NOME-FORN-WK
           ELSE
              MOVE "BRINDES-"       TO NOME-FORN-WK
              MOVE NR-DOCTO-CP20    TO NOME-FORN-WK(9: 10)
           END-IF.
           MOVE SEQ-CP20            TO SEQ-WK
           MOVE TIPO-FORN-CP20      TO TIPO-FORNEC-WK
           MOVE RESPONSAVEL-CP20    TO RESPONSAVEL-WK
           MOVE CODREDUZ-APUR-CP20  TO CTA-APURACAO-WK
           MOVE PORTADOR-CP20       TO PORTADOR-WK
           MOVE DESCRICAO-CP20      TO DESCRICAO-WK
           MOVE VALOR-TOT-CP20      TO VALOR-WK
           MOVE PREV-DEF-CP20       TO PREV-DEF-WK.
           IF SITUACAO-CP20 = 1 MOVE 1 TO SUSPENSO-WK
           ELSE MOVE 0 TO SUSPENSO-WK.
       VERIFICA-PREV-DEF SECTION.
           IF GS-PREVISTO-TRUE
              IF GS-DEFINITIVO-TRUE
                 MOVE 2 TO PREV-DEF-W
              ELSE MOVE 1 TO PREV-DEF-W
           ELSE IF GS-DEFINITIVO-TRUE
                   MOVE 0 TO PREV-DEF-W
                ELSE MOVE 3 TO PREV-DEF-W.
      * PREV-DEF = 2 OS 2 TIPOS ESTAO SELECIONADOS
      * PREV-DEF = 3 NENHUM DOS TIPOS ESTAO SELECIONADOS
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           PERFORM VERIFICA-PREV-DEF.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO GS-TOTAL-PERIODO GS-TOTAL-VENCIDO
                         GS-TOTAL-AVENCER.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-SUSPENSO-TRUE AND SUSPENSO-WK = 1
                   IF PREV-DEF-W = 3 OR 2 PERFORM MOVER-DADOS-LINDET
                   ELSE IF PREV-DEF-W = 0 AND PREV-DEF-WK = 0
                           PERFORM MOVER-DADOS-LINDET
                        ELSE
                          IF PREV-DEF-W = 1 AND PREV-DEF-WK = 1
                             PERFORM MOVER-DADOS-LINDET
                          END-IF
                        END-IF
                   END-IF
                ELSE
                   IF SUSPENSO-WK = 1 OR PREV-DEF-W = 3 CONTINUE
                   ELSE
                    IF PREV-DEF-W = 2 PERFORM MOVER-DADOS-LINDET
                    ELSE
                        IF PREV-DEF-W = 0 AND PREV-DEF-WK = 0
                           PERFORM MOVER-DADOS-LINDET
                        ELSE
                          IF PREV-DEF-W = 1 AND PREV-DEF-WK = 1
                             PERFORM MOVER-DADOS-LINDET
                          END-IF
                        END-IF
                    END-IF
                   END-IF
                END-IF
              END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "VENCTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK
                START WORK KEY IS NOT < VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "FORNECEDOR" TO GS-DESCR-ORDEM
                MOVE SPACES TO NOME-FORN-WK
                START WORK KEY IS NOT < NOME-FORN-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "PORTADOR" TO GS-DESCR-ORDEM
                MOVE ZEROS TO PORTADOR-WK
                START WORK KEY IS NOT < PORTADOR-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "TIPO FORNEC" TO GS-DESCR-ORDEM
                MOVE ZEROS TO TIPO-FORNEC-WK
                START WORK KEY IS NOT < TIPO-FORNEC-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "VCTO/VALOR" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK VALOR-WK
                START WORK KEY IS NOT < ALT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "CTA.APUR." TO GS-DESCR-ORDEM
                MOVE ZEROS TO CTA-APURACAO-WK
                START WORK KEY IS NOT < CTA-APURACAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF FORNEC-ANT  NOT = ZEROS
                 IF FORNEC-ANT NOT = FORNEC-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF PORTADOR-ANT NOT = ZEROS
                 IF PORTADOR-ANT NOT = PORTADOR-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF TIPO-FORN-ANT NOT = ZEROS
                 IF TIPO-FORN-ANT NOT = TIPO-FORNEC-WK
                    PERFORM TOTALIZA
             WHEN 5
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
             WHEN 6
              IF CTA-APUR-ANT NOT = ZEROS
                 IF CTA-APUR-ANT NOT = CTA-APURACAO-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(01: 11)
           MOVE NOME-FORN-WK      TO GS-LINDET(12: 21)
           MOVE DESCRICAO-WK      TO GS-LINDET(33: 31)
           MOVE TIPO-FORNEC-WK    TO GS-LINDET(64: 03)
           MOVE PORTADOR-WK       TO GS-LINDET(67: 03)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(70: 14)
           ADD VALOR-WK           TO TOTAL-W GS-TOTAL-PERIODO.
           MOVE TOTAL-W           TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(84: 14)
           IF VENCTO-WK < DATA-DIA-I
              ADD VALOR-WK        TO GS-TOTAL-VENCIDO
           ELSE ADD VALOR-WK      TO GS-TOTAL-AVENCER.
           MOVE CTA-APURACAO-WK   TO GS-LINDET(98: 06)
           IF PREV-DEF-WK = ZEROS MOVE "D" TO GS-LINDET(104: 1)
           ELSE MOVE "P" TO GS-LINDET(104: 1)
           END-IF
           IF SUSPENSO-WK = 1 MOVE "S" TO GS-LINDET(105: 1)
           ELSE MOVE " " TO GS-LINDET(105: 1)
           END-IF
           MOVE FORNEC-WK         TO GS-LINDET(110: 6)
           MOVE SEQ-WK            TO GS-LINDET(116: 5).
       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO VENCTO-ANT PORTADOR-ANT FORNEC-ANT
                         TIPO-FORN-ANT TOTAL-W CTA-APUR-ANT.
       MOVER-CHAVE-ANT SECTION.
           MOVE FORNEC-WK         TO FORNEC-ANT.
           MOVE TIPO-FORNEC-WK    TO TIPO-FORN-ANT.
           MOVE VENCTO-WK         TO VENCTO-ANT.
           MOVE CTA-APURACAO-WK   TO CTA-APUR-ANT.
           MOVE PORTADOR-WK       TO PORTADOR-ANT.
       TOTALIZA SECTION.
           MOVE ZEROS TO TOTAL-W.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CHAMA-ALTERACAO SECTION.
           IF GS-LINDET = SPACES MOVE ZEROS TO GS-LINDET.
           MOVE GS-LINDET(110: 6) TO FORNEC-CP20 FORNEC-W.
           MOVE GS-LINDET(116: 5) TO SEQ-CP20 SEQ-W.
           START CPD020 KEY IS = CHAVE-CP20 INVALID KEY CONTINUE
             NOT INVALID KEY
               READ CPD020
               END-READ
                 MOVE FORNEC-CP20  TO PASSAR-STRING(1: 6)
                 MOVE SEQ-CP20     TO PASSAR-STRING(7: 5)
                 MOVE USUARIO-W    TO PASSAR-STRING(13: 5)
                 CALL "CPP020A" USING PASSAR-STRING
                 CANCEL "CPP020A"
                 MOVE FORNEC-W TO FORNEC-CP20
                 MOVE SEQ-W    TO SEQ-CP20
                 READ CPD020
                 END-READ
           END-START.

      *    MOVE DATA-MOVTO-W       TO GS-DATA-MOVTO.
      *    MOVE FORNEC-CP20        TO GS-COD-FORN CODIGO-CG01.
      *    READ CGD001 INVALID KEY MOVE "****" TO NOME-CG01.
      *    MOVE NOME-CG01          TO GS-DESCR-FORN.
      *    MOVE TIPO-FORN-CP20     TO GS-TIPO-FORN CODIGO-TIPO.
      *    READ CAD019 INVALID KEY MOVE "****" TO NOME-TIPO.
      *    MOVE NOME-TIPO          TO GS-DESCR-TIPO-FORN.
      *    MOVE PORTADOR-CP20      TO GS-PORTADOR PORTADOR.
      *    READ CAD018 INVALID KEY MOVE "******" TO NOME-PORT.
      *    MOVE NOME-PORT          TO GS-DESCR-PORTADOR.
      *    MOVE NR-DOCTO-CP20      TO GS-NR-DOCTO.
      *    MOVE DATA-EMISSAO-CP20  TO DATA-INV.
      *    CALL "GRIDAT1" USING DATA-INV.
      *    MOVE DATA-INV           TO GS-DATA-EMISSAO.
      *    MOVE DATA-VENCTO-CP20   TO DATA-INV.
      *    CALL "GRIDAT1" USING DATA-INV.
      *    MOVE DATA-INV           TO GS-DATA-VENCTO.
      *    MOVE DESCRICAO-CP20     TO GS-DESCRICAO.
      *    MOVE TIPO-MOEDA-CP20    TO GS-TIPO-MOEDA.
      *    EVALUATE TIPO-MOEDA-CP20
      *      WHEN 0 MOVE "-Real"   TO GS-TIPO-MOEDA(2: 6)
      *      WHEN 1 MOVE "-Dolar"  TO GS-TIPO-MOEDA(2: 5)
      *    END-EVALUATE
      *    MOVE CODREDUZ-APUR-CP20 TO GS-COD-APURACAO
      *                               CODIGO-REDUZ-CX20.
      *    READ CXD020 INVALID KEY MOVE "******" TO DESCRICAO-CX20.
      *    MOVE DESCRICAO-CX20     TO GS-DESCR-APURACAO.
      *    MOVE PREV-DEF-CP20      TO GS-PREV-DEF
      *    EVALUATE PREV-DEF-CP20
      *      WHEN 0 MOVE "-Definitivo" TO GS-PREV-DEF(2: 11)
      *      WHEN 0 MOVE "-Previsto  " TO GS-PREV-DEF(2: 11)
      *    END-EVALUATE
      *    MOVE JUROS-MORA-CP20    TO GS-JUROS-MORA.
      *    MOVE MULTA-ATRASO-CP20  TO GS-MULTA-ATRASO.
      *    MOVE TAXA-APLIC-CP20    TO GS-TAXA
      *    MOVE VALOR-TOT-CP20     TO GS-VALOR-TOTAL.
      *    MOVE RESPONSAVEL-CP20   TO GS-RESPONSAVEL.
      *    MOVE SITUACAO-CP20      TO GS-SITUACAO.
      *    MOVE TIPO-CONTA-CP20    TO GS-TIPO-CONTA.
      *    EVALUATE TIPO-CONTA-CP20
      *      WHEN 0 MOVE "-Temporária" TO GS-TIPO-CONTA(2: 11)
      *      WHEN 0 MOVE "-Permanente" TO GS-TIPO-CONTA(2: 11)
      *    END-EVALUATE.
      *    MOVE "CHAMA-ALTERACAO" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *REGRAVA-DADOS SECTION.
      *    MOVE DATA-DIA-I            TO DATA-MOVTO-CP20.
      *    MOVE GS-TIPO-FORN      TO TIPO-FORN-CP20
      *    MOVE GS-PORTADOR       TO PORTADOR-CP20
      *    MOVE GS-NR-DOCTO       TO NR-DOCTO-CP20.
      *    MOVE GS-DATA-EMISSAO   TO DATA-INV.
      *    CALL "GRIDAT2" USING DATA-INV.
      *    MOVE DATA-INV              TO DATA-EMISSAO-CP20.
      *    MOVE GS-DATA-VENCTO    TO DATA-INV
      *    CALL "GRIDAT2" USING DATA-INV.
      *    MOVE DATA-INV              TO DATA-VENCTO-CP20.
      *    MOVE GS-DESCRICAO      TO DESCRICAO-CP20
      *    IF GS-TIPO-MOEDA = SPACES MOVE "0" TO TIPO-MOEDA-CP20
      *    ELSE MOVE GS-TIPO-MOEDA(1: 1) TO TIPO-MOEDA-CP20.
      *    MOVE GS-COD-APURACAO   TO CODREDUZ-APUR-CP20
      *    MOVE GS-JUROS-MORA     TO JUROS-MORA-CP20
      *    MOVE GS-MULTA-ATRASO   TO MULTA-ATRASO-CP20
      *    IF GS-PREV-DEF = SPACES MOVE "0" TO PREV-DEF-CP20
      *    ELSE MOVE GS-PREV-DEF(1: 1) TO PREV-DEF-CP20.
      *    MOVE GS-TAXA           TO TAXA-APLIC-CP20
      *    MOVE GS-RESPONSAVEL    TO RESPONSAVEL-CP20
      *    MOVE USUARIO-W             TO DIGITADOR-CP20.
      *    IF GS-TIPO-CONTA = SPACES MOVE "0" TO TIPO-CONTA-CP20
      *    ELSE MOVE GS-TIPO-CONTA(1: 1) TO TIPO-CONTA-CP20.
      *    MOVE GS-VALOR-TOTAL    TO VALOR-TOT-CP20
      *    MOVE GS-SITUACAO       TO SITUACAO-CP20.
      *    REWRITE REG-CPD020.
           PERFORM MOVER-DADOS-WORK.
           REWRITE REG-WORK.
           PERFORM MOVER-DADOS.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    Deveria ser regravado o arquivo work, pois pode ter havido
      *    alguma alteração no arquivo em que diferencie a classificação
      *    por exemplo, mudar a data de vencto fora do intervalo soli-
      *    citado pelo usuário. Mas foi solicitado que não regrave.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CPP061" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-SUSPENSO-TRUE AND SUSPENSO-WK = 1
                   IF PREV-DEF-W = 3 OR 2 PERFORM MOVER-DADOS-RELATORIO
                   ELSE IF PREV-DEF-W = 0 AND PREV-DEF-WK = 0
                           PERFORM MOVER-DADOS-RELATORIO
                        ELSE
                          IF PREV-DEF-W = 1 AND PREV-DEF-WK = 1
                             PERFORM MOVER-DADOS-RELATORIO
                          END-IF
                        END-IF
                   END-IF
                ELSE
                   IF SUSPENSO-WK = 1 OR PREV-DEF-W = 3 CONTINUE
                   ELSE
                    IF PREV-DEF-W = 2 PERFORM MOVER-DADOS-RELATORIO
                    ELSE
                        IF PREV-DEF-W = 0 AND PREV-DEF-WK = 0
                           PERFORM MOVER-DADOS-RELATORIO
                        ELSE
                          IF PREV-DEF-W = 1 AND PREV-DEF-WK = 1
                             PERFORM MOVER-DADOS-RELATORIO
                          END-IF
                        END-IF
                    END-IF
                   END-IF
                END-IF
              END-READ
           END-PERFORM.
           MOVE GS-TOTAL-PERIODO  TO TOTAL-PERIODO-REL.
           MOVE GS-TOTAL-VENCIDO  TO TOTAL-VENCIDO-REL.
           MOVE GS-TOTAL-AVENCER  TO TOTAL-AVENCER-REL.
           WRITE REG-RELAT FROM LINTOT AFTER 2.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF FORNEC-ANT NOT = ZEROS
                 IF FORNEC-ANT NOT = FORNEC-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF PORTADOR-ANT NOT = ZEROS
                 IF PORTADOR-ANT NOT = PORTADOR-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF TIPO-FORN-ANT NOT = ZEROS
                 IF TIPO-FORN-ANT NOT = TIPO-FORNEC-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF CTA-APUR-ANT NOT = ZEROS
                 IF CTA-APUR-ANT NOT = CTA-APURACAO-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01: 11)
           MOVE NOME-FORN-WK      TO LINDET-REL(12: 21)
           MOVE DESCRICAO-WK      TO LINDET-REL(33: 31)
           MOVE TIPO-FORNEC-WK    TO LINDET-REL(64: 03)
           MOVE PORTADOR-WK       TO LINDET-REL(67: 03)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(70: 14)
           ADD VALOR-WK           TO TOTAL-W.
           MOVE TOTAL-W           TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(84: 14)
           MOVE CTA-APURACAO-WK   TO LINDET-REL(98: 06).
           IF SUSPENSO-WK = 1
              MOVE "S" TO LINDET-REL(105: 1)
           ELSE MOVE " " TO LINDET-REL(105: 1)
           END-IF.
           IF PREV-DEF-WK = ZEROS MOVE "D" TO LINDET-REL(104: 1)
           ELSE MOVE "P" TO LINDET-REL(104: 1).
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
           CLOSE CPD020 CAD018 CAD019 CGD001 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

