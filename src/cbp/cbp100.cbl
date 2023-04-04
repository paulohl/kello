       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP100.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 03/11/1998
      *DESCRIÇÃO: Manutenção no talão de cheques

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CBPX001.
           COPY CBPX003.
           COPY CBPX100.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CBPW001.
       COPY CBPW003.
       COPY CBPW100.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CBP100.CPB".
           COPY "CBP100.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CPADAY1".
           COPY "CPDIAS1".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(65).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(60).
       01  PASSAR-USUARIO            PIC X(20)    VALUE SPACES.
       01  VARIAVEIS.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-CBD003             PIC XX       VALUE SPACES.
           05  ST-CBD100             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.

           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W = 0 (não ocorreu erro abertura) erro-w=1 (houve erro)
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  DATA-E                PIC 99/99/9999.
           05  BANCO-HEAD            PIC 9(6)     VALUE ZEROS.
           05  CHEQUE-INI-HEAD       PIC 9(6)     VALUE ZEROS.
           05  CHEQUE-FIM-HEAD       PIC 9(6)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(53)   VALUE
           "CONFERENCIA NO TALAO DE CHEQUES                   ".
           05  FILLER              PIC X(10)   VALUE "    HORA: ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "SEQ.  HISTORICO                   NR-DOCTO      VALOR".
       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CBP100-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV    TO DATA-INV DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV
           CANCEL "GRIDAT2"
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CBP100-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CBP100-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CBP100-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CBD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD001.
           MOVE "CBD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD003.
           MOVE "CBD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD100.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.

           OPEN I-O   CBD100
           CLOSE      CBD100
           OPEN INPUT CBD100

           OPEN INPUT CBD001 CBD003 CGD001.
           IF ST-CBD100 = "35"
              CLOSE CBD100      OPEN OUTPUT CBD100
              CLOSE CBD100      OPEN I-O CBD100
           END-IF.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO CBP100-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CBP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD003 <> "00"
              MOVE "ERRO ABERTURA CBD003: "  TO CBP100-MENSAGEM-ERRO
              MOVE ST-CBD003 TO CBP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD100 <> "00"
              MOVE "ERRO ABERTURA CBD100: "  TO CBP100-MENSAGEM-ERRO
              MOVE ST-CBD100 TO CBP100-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CBP100-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CBP100-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CBP100-SAVE-FLG-TRUE
                    PERFORM SALVAR-DADOS
                    PERFORM CARREGA-ULTIMOS
                    PERFORM LIMPAR-DADOS
               WHEN CBP100-LOAD-FLG-TRUE
                    PERFORM CARREGAR-DADOS
               WHEN CBP100-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI-RECORD
                    PERFORM CARREGA-ULTIMOS
               WHEN CBP100-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
               WHEN CBP100-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CBP100-CARREGA-ULT-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN CBP100-CARREGA-LIST-BOX-TRUE
                    MOVE CBP100-LINDET(1: 6) TO CBP100-BANCO
                    MOVE CBP100-LINDET(8: 6) TO CBP100-NR-CHEQUE
                    PERFORM CARREGAR-DADOS
               WHEN CBP100-POPUP-BANCO-TRUE
                    PERFORM CHAMAR-POPUP-BANCO
               WHEN CBP100-POPUP-NOMINAL-A-TRUE
                    PERFORM CHAMAR-POPUP-NOMINAL
               WHEN CBP100-POPUP-SITUACAO-TRUE
                    PERFORM CHAMAR-POPUP-SITUACAO
               WHEN CBP100-LER-BANCO-TRUE
                    PERFORM LER-BANCO
               WHEN CBP100-LER-SITUACAO-TRUE
                    PERFORM LER-SITUACAO
               WHEN CBP100-LER-NOMINAL-TRUE
                    PERFORM LER-NOMINAL
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-BANCO SECTION.
           MOVE CBP100-BANCO TO CODIGO-FORN-CB01.
           READ CBD001 INVALID KEY
                MOVE SPACES TO TITULAR-CB01.
           MOVE TITULAR-CB01 TO CBP100-DESC-BANCO.
       LER-SITUACAO SECTION.
           MOVE CBP100-SITUACAO TO SITUACAO-CB03.
           READ CBD003 INVALID KEY
                MOVE SPACES TO NOME-SIT-CB03.
           MOVE NOME-SIT-CB03  TO CBP100-DESC-SITUACAO.
       LER-NOMINAL SECTION.
           MOVE CBP100-NOMINAL-A TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01        TO CBP100-NOME-NOMINAL-A.
       SALVAR-DADOS SECTION.
           CLOSE      CBD100
           OPEN I-O   CBD100
           MOVE CBP100-BANCO              TO CODIGO-FORN-CB100.
           MOVE CBP100-NR-CHEQUE          TO NR-CHEQUE-CB100.
           READ CBD100 INVALID KEY
                MOVE "CHEQUE-NAO-ENCONTRADO" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
            NOT INVALID KEY
                MOVE CBP100-DATA-EMISSAO       TO DATA-EMISSAO-CB100
                MOVE CBP100-DATA-VENCTO        TO DATA-VENCTO-CB100
                MOVE CBP100-VALOR              TO VALOR-CB100
                MOVE CBP100-NOMINAL-A          TO NOMINAL-A-CB100
                MOVE CBP100-SITUACAO           TO SITUACAO-CB100
                REWRITE REG-CBD100
                END-REWRITE
                PERFORM MOVER-DADOS-LISTA.
           CLOSE      CBD100
           OPEN INPUT CBD100.
       CHAMAR-POPUP-BANCO SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CBP001T".
           MOVE PASSAR-STRING(49: 6) TO CBP100-BANCO.
           PERFORM LER-BANCO.
       CHAMAR-POPUP-SITUACAO SECTION.
           CALL   "CBP003T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CBP003T".
           MOVE PASSAR-STRING(40: 2) TO CBP100-SITUACAO.
           PERFORM LER-SITUACAO.
       CHAMAR-POPUP-NOMINAL SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING(33: 6) TO CBP100-NOMINAL-A.
           PERFORM LER-NOMINAL.
       CARREGAR-DADOS SECTION.
           MOVE CBP100-BANCO        TO CODIGO-FORN-CB100.
           MOVE CBP100-NR-CHEQUE    TO NR-CHEQUE-CB100.
           READ CBD100 INVALID KEY
                INITIALIZE REG-CBD100
                MOVE 0 TO CBP100-ENCONTROU-CHEQUE
           NOT INVALID KEY
                MOVE 1 TO CBP100-ENCONTROU-CHEQUE.
           MOVE CODIGO-FORN-CB100   TO CODIGO-FORN-CB01.
           READ CBD001 INVALID KEY
                MOVE SPACES TO NOME-BANCO-CB01.
           MOVE NOME-BANCO-CB01     TO CBP100-DESC-BANCO.

           MOVE DATA-EMISSAO-CB100  TO CBP100-DATA-EMISSAO.
           MOVE DATA-VENCTO-CB100   TO CBP100-DATA-VENCTO.
           MOVE VALOR-CB100         TO CBP100-VALOR.
           MOVE NOMINAL-A-CB100     TO CBP100-NOMINAL-A CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01           TO CBP100-NOME-NOMINAL-A.
           MOVE SITUACAO-CB100      TO CBP100-SITUACAO SITUACAO-CB03.
           READ CBD003 INVALID KEY
                MOVE SPACES TO NOME-SIT-CB03.
           MOVE NOME-SIT-CB03       TO CBP100-DESC-SITUACAO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CBD100
           MOVE CBP100-BANCO      TO BANCO-HEAD.
           MOVE CBP100-CHEQUE-INI TO CHEQUE-INI-HEAD.
           MOVE CBP100-CHEQUE-FIM TO CHEQUE-FIM-HEAD.
           INITIALIZE CBP100-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           MOVE BANCO-HEAD       TO CBP100-BANCO.
           MOVE CHEQUE-INI-HEAD  TO CBP100-CHEQUE-INI.
           MOVE CHEQUE-FIM-HEAD  TO CBP100-CHEQUE-FIM.
       EXCLUI-RECORD SECTION.
           CLOSE      CBD100
           OPEN I-O   CBD100
           DELETE CBD100
           CLOSE      CBD100
           OPEN INPUT CBD100.
           PERFORM LIMPAR-DADOS.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CBP100-MENSAGEM-ERRO
           MOVE ST-CBD100       TO CBP100-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE CBP100-BANCO         TO CODIGO-FORN-CB100.
           MOVE CBP100-CHEQUE-INI    TO NR-CHEQUE-CB100.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO CBP100-CONT.
           START CBD100 KEY IS NOT < CHAVE-CB100 INVALID KEY
                 MOVE "10" TO ST-CBD100.
           PERFORM UNTIL ST-CBD100 = "10"
              READ CBD100 NEXT RECORD AT END MOVE "10" TO ST-CBD100
              NOT AT END
                IF CODIGO-FORN-CB100 NOT = CBP100-BANCO OR
                   NR-CHEQUE-CB100 > CBP100-CHEQUE-FIM
                   MOVE "10" TO ST-CBD100
                ELSE
                   PERFORM MOVER-DADOS-LISTA
              END-READ
           END-PERFORM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES            TO CBP100-LINDET
           MOVE CODIGO-FORN-CB100 TO CBP100-LINDET(01: 7)
           MOVE NR-CHEQUE-CB100   TO CBP100-LINDET(8: 7)
           MOVE DATA-EMISSAO-CB100 TO DATA-E
           MOVE DATA-E             TO CBP100-LINDET(15: 11)
           MOVE DATA-VENCTO-CB100  TO DATA-E
           MOVE DATA-E             TO CBP100-LINDET(26: 11)
           MOVE NOMINAL-A-CB100    TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO CBP100-LINDET(37: 27)
           MOVE VALOR-CB100        TO VALOR-E
           MOVE VALOR-E            TO CBP100-LINDET(65: 14)
           MOVE SITUACAO-CB100     TO CBP100-LINDET(79: 2)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CBP100-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CBP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE CBP100-BANCO         TO CODIGO-FORN-CB100.
           MOVE CBP100-CHEQUE-INI    TO NR-CHEQUE-CB100.
           START CBD100 KEY IS NOT < CHAVE-CB100 INVALID KEY
                 MOVE "10" TO ST-CBD100.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CBD100 = "10"
              READ CBD100 NEXT RECORD AT END MOVE "10" TO ST-CBD100
              NOT AT END
               IF CODIGO-FORN-CB100 NOT = CBP100-BANCO OR
                  NR-CHEQUE-CB100 > CBP100-CHEQUE-FIM
                  MOVE "10" TO ST-CBD100
               ELSE
                 MOVE SPACES             TO LINDET-REL
                 MOVE CODIGO-FORN-CB100  TO LINDET-REL(01: 7)
                 MOVE NR-CHEQUE-CB100    TO LINDET-REL(8: 7)
                 MOVE DATA-EMISSAO-CB100 TO DATA-E
                 MOVE DATA-E             TO LINDET-REL(15: 11)
                 MOVE DATA-VENCTO-CB100  TO DATA-E
                 MOVE DATA-E             TO LINDET-REL(26: 11)
                 MOVE NOMINAL-A-CB100    TO CODIGO-CG01
                 READ CGD001 INVALID KEY
                      MOVE SPACES TO NOME-CG01
                 END-READ
                 MOVE NOME-CG01          TO LINDET-REL(37: 27)
                 MOVE VALOR-CB100        TO VALOR-E
                 MOVE VALOR-E            TO LINDET-REL(65: 14)
                 MOVE SITUACAO-CB100     TO LINDET-REL(79: 2)
                 WRITE REG-RELAT FROM LINDET
                 END-WRITE
                 ADD 1 TO LIN
                 IF LIN > 56 PERFORM CABECALHO
                 END-IF
               END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CBP100-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 CBD001 CBD003 CBD100.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
