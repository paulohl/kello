       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP101.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 26/10/1998
      *DESCRIÇÃO: Digitação de extratos bancários
      * Sempre será feita a conferencia de saldo anterior, caso haja
      * divergencia entre os saldos, avisar o usuários para tomar as
      * medidas necessaria p/ solução do problema. Nesse movimento
      * será criado o arquivo cbd102(arquivo de saldo).

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CBPX001.
           COPY CBPX004.
           COPY CBPX101.
           COPY CBPX102.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CBPW001.
       COPY CBPW004.
       COPY CBPW101.
       COPY CBPW102.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CBP101.CPB".
           COPY "CBP101.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CPADAY1".
           COPY "CPDIAS1".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(65).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(65).
       01  PASSAR-USUARIO            PIC X(20)    VALUE SPACES.
       01  VARIAVEIS.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-CBD004             PIC XX       VALUE SPACES.
           05  ST-CBD101             PIC XX       VALUE SPACES.
           05  ST-CBD102             PIC XX       VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.

           05  ULTIMA-SEQ            PIC 9(3)     VALUE ZEROS.
      *    Ult-SEQUENCIA-Será utilizado p/ encontrar a última sequencia
      *    utilizada do movto diario
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W = 0 (não ocorreu erro abertura) erro-w=1 (houve erro)
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-EXTRATO-I        PIC 9(8)     VALUE ZEROS.
           05  DATA-EXTRATO-HEAD     PIC 9(8)     VALUE ZEROS.
           05  BANCO-HEAD            PIC 9(6)     VALUE ZEROS.
           05  DESC-BANCO-HEAD       PIC X(30)    VALUE SPACES.
           05  DATA-ULT-EXTRATO-HEAD PIC 9(8)     VALUE ZEROS.
           05  SALDO-INICIAL-HEAD    PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL-HEAD      PIC S9(8)V99 VALUE ZEROS.
           05  DATA-ANT              PIC 9(8)     VALUE ZEROS.
           05  BANCO-ANT             PIC 9(6)     VALUE ZEROS.
           05  ULTIMA-DATA           PIC 9(8)     VALUE ZEROS.
           05  ULTIMO-SALDO          PIC S9(8)V99 VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  DATA-E                PIC 99/99/9999.
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
           05  FILLER              PIC X(46)   VALUE
           "CONFER.MOVIMENTO DE EXTRATO BANCARIO DO DIA: ".
           05  DATA-MOV-REL        PIC 99/99/9999.
           05  FILLER              PIC X(7)    VALUE "   HR: ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "SEQ.  TIPO DE LANCTO                    NR-DOCTO   VALOR-CRE
      -    "DITO  VALOR-DEBITO".
       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CBP101-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV    TO DATA-INV DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV
           CANCEL "GRIDAT2"
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CBP101-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CBP101-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CBP101-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CBD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD001.
           MOVE "CBD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD004.
           MOVE "CBD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD101.
           MOVE "CBD102" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD102.

           OPEN I-O   CBD101 CBD102.
           OPEN INPUT CBD001 CBD004.
           IF ST-CBD101 = "35"
              CLOSE CBD101      OPEN OUTPUT CBD101
              CLOSE CBD101      OPEN I-O CBD101
           END-IF.
           IF ST-CBD102 = "35"
              CLOSE CBD102      OPEN OUTPUT CBD102
              CLOSE CBD102      OPEN I-O CBD102
           END-IF.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO CBP101-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CBP101-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD004 <> "00"
              MOVE "ERRO ABERTURA CBD004: "  TO CBP101-MENSAGEM-ERRO
              MOVE ST-CBD004 TO CBP101-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD101 <> "00"
              MOVE "ERRO ABERTURA CBD101: "  TO CBP101-MENSAGEM-ERRO
              MOVE ST-CBD101 TO CBP101-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD102 <> "00"
              MOVE "ERRO ABERTURA CBD102: "  TO CBP101-MENSAGEM-ERRO
              MOVE ST-CBD102 TO CBP101-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CBP101-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CBP101-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CBP101-SAVE-FLG-TRUE
                    PERFORM SALVAR-DADOS
                    PERFORM LIMPAR-DADOS
               WHEN CBP101-LOAD-FLG-TRUE
                    PERFORM CARREGAR-DADOS
               WHEN CBP101-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI-RECORD
                    PERFORM CARREGA-ULTIMOS
               WHEN CBP101-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
               WHEN CBP101-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CBP101-CARREGA-ULT-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN CBP101-CARREGA-LIST-BOX-TRUE
                    MOVE CBP101-LINDET(1: 3) TO SEQ-CB101
                    PERFORM CARREGAR-DADOS
               WHEN CBP101-POPUP-BANCO-TRUE
                    PERFORM CHAMAR-POPUP-BANCO
               WHEN CBP101-POPUP-HISTORICO-TRUE
                    PERFORM CHAMAR-POPUP-HISTORICO
               WHEN CBP101-LER-BANCO-TRUE
                    PERFORM LER-BANCO
               WHEN CBP101-LER-HISTORICO-TRUE
                    PERFORM LER-HISTORICO
               WHEN CBP101-GRAVA-SALDOS-TRUE
                    PERFORM GRAVA-SALDO
               WHEN CBP101-LER-SALDO-TRUE
                    PERFORM LER-SALDO
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
           MOVE CBP101-BANCO TO CODIGO-FORN-CB01.
           READ CBD001 INVALID KEY MOVE SPACES TO TITULAR-CB01.
           MOVE TITULAR-CB01 TO CBP101-DESC-BANCO.
       LER-HISTORICO SECTION.
           MOVE CBP101-HISTORICO TO CODIGO-CB04.
           READ CBD004 INVALID KEY MOVE SPACES TO HISTORICO-CB04.
           MOVE HISTORICO-CB04 TO CBP101-DESC-HISTORICO.
       SALVAR-DADOS SECTION.
           MOVE CBP101-BANCO              TO CONTA-BANCO-CB101.
           MOVE CBP101-HISTORICO          TO HISTORICO-CB101.
           MOVE CBP101-NR-CHEQUE          TO NR-CHEQUE-CB101.
           MOVE CBP101-VALOR              TO VALOR-CB101.
           MOVE DATA-EXTRATO-I            TO DATA-EXTRATO-CB101.
           MOVE 1                         TO RESOLVER-CB101.
      *    IF ULTIMA-SEQ = ZEROS PERFORM ACHA-SEQUENCIA.
           ADD 1 TO ULTIMA-SEQ.
           MOVE ULTIMA-SEQ                TO SEQ-CB101
           MOVE ZEROS TO ST-CBD101
           PERFORM UNTIL ST-CBD101 = "10"
             WRITE REG-CBD101 INVALID KEY
                   ADD 1 TO ULTIMA-SEQ
                   MOVE ULTIMA-SEQ TO SEQ-CB101
             NOT INVALID KEY
                   MOVE "10" TO ST-CBD101
             END-WRITE
           END-PERFORM.
           PERFORM GRAVA-SALDO.
           PERFORM MOVER-DADOS-LISTA.
       GRAVA-SALDO SECTION.
           MOVE DATA-EXTRATO-I TO DATA-EXTRATO-CB102.
           MOVE CBP101-BANCO   TO CONTA-BANCO-CB102.
           READ CBD102 INVALID KEY
                MOVE ZEROS TO SALDO-CB102
                IF CBP101-HISTORICO < 50
                   SUBTRACT CBP101-VALOR FROM SALDO-CB102
                   SUBTRACT CBP101-VALOR FROM
                            CBP101-SALDO-FINAL
                ELSE ADD CBP101-VALOR TO SALDO-CB102
                     ADD CBP101-VALOR TO CBP101-SALDO-FINAL
                END-IF
                WRITE REG-CBD102
                END-WRITE
           NOT INVALID KEY
                IF CBP101-HISTORICO < 50
                   SUBTRACT CBP101-VALOR FROM SALDO-CB102
                   SUBTRACT CBP101-VALOR FROM
                       CBP101-SALDO-FINAL
                ELSE ADD CBP101-VALOR TO SALDO-CB102
                     ADD CBP101-VALOR TO CBP101-SALDO-FINAL
                END-IF
                REWRITE REG-CBD102
                END-REWRITE
           END-READ.
       LER-SALDO SECTION.
           IF CBP101-DATA-EXTRATO NOT = DATA-ANT OR
              CBP101-BANCO NOT = BANCO-ANT
                  PERFORM ACHA-SEQUENCIA.
           MOVE ZEROS TO DATA-EXTRATO-CB102.
           MOVE CBP101-BANCO         TO CONTA-BANCO-CB102.
           START CBD102 KEY IS NOT < ALT1-CB102 INVALID KEY
                 MOVE "10" TO ST-CBD102.
           MOVE ZEROS TO ULTIMO-SALDO ULTIMA-DATA.
           PERFORM UNTIL ST-CBD102 = "10"
              READ CBD102 NEXT RECORD AT END
                   MOVE "10" TO ST-CBD102
              NOT AT END
                  IF DATA-EXTRATO-CB102 NOT < DATA-EXTRATO-I OR
                     CONTA-BANCO-CB102 NOT = CBP101-BANCO
                     MOVE "10" TO ST-CBD102
                  ELSE
                     MOVE DATA-EXTRATO-CB102    TO ULTIMA-DATA
                     ADD SALDO-CB102           TO ULTIMO-SALDO
              END-READ
           END-PERFORM.
           MOVE ULTIMA-DATA    TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV       TO CBP101-DATA-ULT-EXTRATO.
           MOVE ULTIMO-SALDO   TO CBP101-SALDO-INICIAL
                                  CBP101-SALDO-FINAL.
      *  LER SALDO FINAL
           MOVE DATA-EXTRATO-I TO DATA-EXTRATO-CB101.
           MOVE CBP101-BANCO   TO CONTA-BANCO-CB101.
           MOVE ZEROS          TO SEQ-CB101.
           START CBD101 KEY IS NOT < CHAVE-CB101 INVALID KEY
                 MOVE "10" TO ST-CBD101.
           PERFORM UNTIL ST-CBD101 = "10"
             READ CBD101 NEXT RECORD AT END
                  MOVE "10" TO ST-CBD101
             NOT AT END
                  IF DATA-EXTRATO-CB101 NOT = DATA-EXTRATO-I OR
                     CONTA-BANCO-CB101  NOT = CBP101-BANCO
                     MOVE "10" TO ST-CBD101
                  ELSE
                     IF HISTORICO-CB101 < 50
                        SUBTRACT VALOR-CB101 FROM CBP101-SALDO-FINAL
                     ELSE
                        ADD VALOR-CB101 TO CBP101-SALDO-FINAL
             END-READ
           END-PERFORM.
       ACHA-SEQUENCIA SECTION.
           MOVE CBP101-BANCO        TO CONTA-BANCO-CB101.
           MOVE CBP101-DATA-EXTRATO TO DATA-INV DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           CANCEL "GRIDAT2".
           MOVE DATA-INV TO DATA-EXTRATO-I.
           MOVE DATA-EXTRATO-I  TO DATA-EXTRATO-CB101.
           MOVE ZEROS TO ULTIMA-SEQ SEQ-CB101.
           START CBD101 KEY IS NOT < CHAVE-CB101 INVALID KEY
                 MOVE "10" TO ST-CBD101.
           PERFORM UNTIL ST-CBD101 = "10"
             READ CBD101 NEXT RECORD AT END
                  MOVE "10" TO ST-CBD101
             NOT AT END
                  IF DATA-EXTRATO-CB101 NOT = DATA-EXTRATO-I OR
                     CONTA-BANCO-CB101  NOT = CBP101-BANCO
                     MOVE "10" TO ST-CBD101
                 ELSE
                     MOVE SEQ-CB101 TO ULTIMA-SEQ
             END-READ
           END-PERFORM.
           PERFORM CARREGA-ULTIMOS.
       CHAMAR-POPUP-BANCO SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CBP001T".
           MOVE PASSAR-STRING(49: 6) TO CBP101-BANCO.
           PERFORM LER-BANCO.
       CHAMAR-POPUP-HISTORICO SECTION.
           CALL   "CBP004T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CBP004T".
           MOVE PASSAR-STRING(40: 2) TO CBP101-HISTORICO.
           PERFORM LER-HISTORICO.
       CARREGAR-DADOS SECTION.
           MOVE DATA-EXTRATO-I      TO DATA-EXTRATO-CB101.
           MOVE CBP101-BANCO        TO CONTA-BANCO-CB101.
           START CBD101 KEY IS NOT < CHAVE-CB101 INVALID KEY
                INITIALIZE REG-CBD101.
           READ CBD101 NEXT RECORD AT END
                INITIALIZE REG-CBD101.
           MOVE HISTORICO-CB101     TO CBP101-HISTORICO
                                       CODIGO-CB04.
           READ CBD004 INVALID KEY
                MOVE SPACES TO HISTORICO-CB04.
           MOVE HISTORICO-CB04      TO CBP101-DESC-HISTORICO.
           MOVE VALOR-CB101         TO CBP101-VALOR.
           MOVE NR-CHEQUE-CB101     TO CBP101-NR-CHEQUE.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           MOVE CBP101-DATA-EXTRATO     TO DATA-EXTRATO-HEAD
           MOVE CBP101-BANCO            TO BANCO-HEAD
           MOVE CBP101-DESC-BANCO       TO DESC-BANCO-HEAD
           MOVE CBP101-DATA-ULT-EXTRATO TO DATA-ULT-EXTRATO-HEAD
           MOVE CBP101-SALDO-INICIAL    TO SALDO-INICIAL-HEAD
           MOVE CBP101-SALDO-FINAL      TO SALDO-FINAL-HEAD
           INITIALIZE REG-CBD101
           INITIALIZE CBP101-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           MOVE DATA-EXTRATO-HEAD       TO CBP101-DATA-EXTRATO
           MOVE BANCO-HEAD              TO CBP101-BANCO
           MOVE DESC-BANCO-HEAD         TO CBP101-DESC-BANCO
           MOVE DATA-ULT-EXTRATO-HEAD   TO CBP101-DATA-ULT-EXTRATO
           MOVE SALDO-INICIAL-HEAD      TO CBP101-SALDO-INICIAL
           MOVE SALDO-FINAL-HEAD        TO CBP101-SALDO-FINAL.
       EXCLUI-RECORD SECTION.
           DELETE CBD101.
           MOVE DATA-EXTRATO-I TO DATA-EXTRATO-CB102.
           MOVE CBP101-BANCO   TO CONTA-BANCO-CB102.
      *  NA EXCLUSÃO O CALCULA PARA O SALDO É AO CONTRARIO DA ENTRADA
           READ CBD102 INVALID KEY CONTINUE
             NOT INVALID KEY
              IF CBP101-HISTORICO < 50
                 ADD CBP101-VALOR TO SALDO-CB102
                 ADD CBP101-VALOR TO CBP101-SALDO-FINAL
              ELSE
                 SUBTRACT CBP101-VALOR FROM SALDO-CB102
                 SUBTRACT CBP101-VALOR FROM
                          CBP101-SALDO-FINAL
              END-IF
              REWRITE REG-CBD102
           END-READ
           PERFORM LIMPAR-DADOS.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CBP101-MENSAGEM-ERRO
           MOVE ST-CBD101       TO CBP101-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE DATA-EXTRATO-I   TO DATA-EXTRATO-CB101.
           MOVE CBP101-BANCO     TO CONTA-BANCO-CB101.
           MOVE ZEROS            TO SEQ-CB101.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO CBP101-CONT.
           START CBD101 KEY IS NOT < CHAVE-CB101 INVALID KEY
                 MOVE "10" TO ST-CBD101.
           PERFORM UNTIL ST-CBD101 = "10"
              READ CBD101 NEXT RECORD AT END MOVE "10" TO ST-CBD101
              NOT AT END
                IF DATA-EXTRATO-CB101 NOT = DATA-EXTRATO-I
                  OR CONTA-BANCO-CB101 NOT = CBP101-BANCO
                          MOVE "10" TO ST-CBD101
                ELSE
                   PERFORM MOVER-DADOS-LISTA
              END-READ
           END-PERFORM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES            TO CBP101-LINDET
           MOVE SEQ-CB101         TO CBP101-LINDET(01: 04)
           MOVE HISTORICO-CB101   TO CBP101-LINDET(06: 03) CODIGO-CB04
           READ CBD004 INVALID KEY MOVE SPACES TO HISTORICO-CB04.
           MOVE HISTORICO-CB04    TO CBP101-LINDET(09: 30)
           MOVE NR-CHEQUE-CB101   TO CBP101-LINDET(41: 10)
           MOVE VALOR-CB101       TO VALOR-E
           IF HISTORICO-CB101 < 50
              MOVE VALOR-E           TO CBP101-LINDET(67: 13)
           ELSE MOVE VALOR-E         TO CBP101-LINDET(52: 13)
           END-IF
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CBP101-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CBP101" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE DATA-EXTRATO-I TO DATA-EXTRATO-CB101.
           MOVE CBP101-BANCO   TO CONTA-BANCO-CB101.
           MOVE ZEROS          TO SEQ-CB101.
           START CBD101 KEY IS NOT < CHAVE-CB101 INVALID KEY
                 MOVE "10" TO ST-CBD101.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CBD101 = "10"
              READ CBD101 NEXT RECORD AT END MOVE "10" TO ST-CBD101
              NOT AT END
                IF DATA-EXTRATO-CB101 NOT = DATA-EXTRATO-I OR
                   CONTA-BANCO-CB101  NOT = CBP101-BANCO
                   MOVE "10" TO ST-CBD101
                ELSE
                   MOVE SPACES            TO LINDET-REL
                   MOVE SEQ-CB101         TO LINDET-REL(01: 06)
                   MOVE HISTORICO-CB101   TO LINDET-REL(06: 3)
                                             CODIGO-CB04
                   READ CBD004 INVALID KEY
                        MOVE SPACES TO HISTORICO-CB04
                   END-READ
                   MOVE HISTORICO-CB04    TO LINDET-REL(09: 30)
                   MOVE NR-CHEQUE-CB101   TO LINDET-REL(41: 10)
                   MOVE VALOR-CB101       TO VALOR-E
                   IF HISTORICO-CB101 < 50
                      MOVE VALOR-E        TO LINDET-REL(66: 13)
                   ELSE MOVE VALOR-E      TO LINDET-REL(52: 13)
                END-IF
                WRITE REG-RELAT FROM LINDET
                ADD 1 TO LIN
                IF LIN > 56
                   PERFORM CABECALHO
                END-IF
             END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           MOVE DATA-MOVTO-W   TO DATA-MOV-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CBP101-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CBD001 CBD004 CBD101 CBD102.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
