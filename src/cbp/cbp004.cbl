       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP004.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 19/10/1998
      *DESCRIÇÃO: CADASTRO DE HISTÓRICOS DE LANCAMENTOS EM EXTRATO
      *           BANCÁRIO
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CXPX020.
           COPY CBPX004.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CXPW020.
       COPY CBPW004.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CBP004.CPB".
           COPY "CBP004.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(65).
       01  VARIAVEIS.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CBD004             PIC XX       VALUE SPACES.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  ULT-CODIGO            PIC 99       VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
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
           05  FILLER              PIC X(63)   VALUE
           "CADASTRO - HISTORICO DE LANCAMENTOS EM EXT.BANCARIO".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "CD  DESCRICAO                        APUR-C APUR-D".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CBP004-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CBP004-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CBP004-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE CBP004-VERSION-NO  TO DS-VERSION-NO.
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CBD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD004.
           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           OPEN I-O CBD004.
           OPEN INPUT CXD020.
           MOVE 1 TO GRAVA-W.
           IF ST-CBD004 = "35"
              CLOSE CBD004      OPEN OUTPUT CBD004
              CLOSE CBD004      OPEN I-O CBD004
           END-IF.
           IF ST-CBD004 <> "00"
              MOVE "ERRO ABERTURA CBD004: "  TO CBP004-MENSAGEM-ERRO
              MOVE ST-CBD004 TO CBP004-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CBP004-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CBP004-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CBP004-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                MOVE 1 TO CBP004-ORDER
                PERFORM ACHAR-CODIGO
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CBP004-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CBP004-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP004-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP004-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CBP004-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CBP004-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CBP004-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP004-CARREGA-LIST-BOX-TRUE
                   MOVE CBP004-LINDET(1: 2) TO CBP004-CODIGO
                   PERFORM CARREGAR-DADOS
               WHEN CBP004-LER-APURACAO-TRUE
                   MOVE CBP004-CONTA-APURACAO TO CODIGO-REDUZ-CX20
                   PERFORM LEITURA-APURACAO
               WHEN CBP004-POPUP-APURACAO-TRUE
                   PERFORM CHAMAR-POPUP-APURACAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE CBP004-CODIGO      TO CODIGO-CB04.
           READ CBD004 INVALID KEY
                INITIALIZE REG-CBD004
                MOVE 1 TO GRAVA-W.

           MOVE HISTORICO-CB04     TO CBP004-DESCRICAO.
           MOVE APURACAO-RED-C-CB04  TO CBP004-CONTA-APURACAO-C
                                        CODIGO-REDUZ-CX20.
           PERFORM LEITURA-APURACAO.
           MOVE CBP004-DESC-APURACAO TO CBP004-DESC-APURACAO-C.
           MOVE APURACAO-RED-D-CB04  TO CBP004-CONTA-APURACAO-D
                                        CODIGO-REDUZ-CX20.
           PERFORM LEITURA-APURACAO.
           MOVE CBP004-DESC-APURACAO TO CBP004-DESC-APURACAO-D.

       LEITURA-APURACAO SECTION.
           READ CXD020 INVALID KEY
                MOVE SPACES TO DESCRICAO-CX20.
           MOVE DESCRICAO-CX20     TO CBP004-DESC-APURACAO.
       CHAMAR-POPUP-APURACAO SECTION.
           CALL   "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CXP020T".
           MOVE PASSAR-PARAMETROS(52: 5) TO CBP004-CONTA-APURACAO
                                            CODIGO-REDUZ-CX20.
           PERFORM LEITURA-APURACAO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CBD004
           MOVE CBP004-ORDER TO ORDEM-W
           INITIALIZE CBP004-DATA-BLOCK
           MOVE ORDEM-W TO CBP004-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE CBD004.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE CBP004-CODIGO             TO CODIGO-CB04.
           MOVE CBP004-DESCRICAO          TO HISTORICO-CB04.
           MOVE CBP004-CONTA-APURACAO-C   TO APURACAO-RED-C-CB04.
           MOVE CBP004-CONTA-APURACAO-D   TO APURACAO-RED-D-CB04.
           IF GRAVA-W = 1
              WRITE REG-CBD004 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE SUBTRACT 1 FROM ULT-CODIGO
                REWRITE REG-CBD004 INVALID KEY
                PERFORM ERRO-GRAVACAO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CBP004-MENSAGEM-ERRO
           MOVE ST-CBD004       TO CBP004-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
           PERFORM ACHAR-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF CBP004-ORDER = ZEROS
              MOVE SPACES TO HISTORICO-CB04
              START CBD004 KEY IS NOT < HISTORICO-CB04
                    INVALID KEY MOVE "10" TO ST-CBD004
           ELSE
             MOVE ZEROS TO CODIGO-CB04
               START CBD004 KEY IS NOT < CODIGO-CB04
                 INVALID KEY MOVE "10" TO ST-CBD004.
           MOVE SPACES TO CBP004-LINDET.
           MOVE ZEROS TO CBP004-CONT.
           PERFORM UNTIL ST-CBD004 = "10"
              READ CBD004 NEXT RECORD AT END MOVE "10" TO ST-CBD004
              NOT AT END
                ADD 1 TO CBP004-CONT
                MOVE SPACES                TO CBP004-LINDET
                MOVE CODIGO-CB04           TO CBP004-LINDET(1: 5)
                MOVE HISTORICO-CB04        TO CBP004-LINDET(06: 33)
                MOVE APURACAO-RED-C-CB04   TO CBP004-LINDET(38: 05)
                MOVE APURACAO-RED-D-CB04   TO CBP004-LINDET(46: 05)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CBP004-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "CBP004" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO ULT-CODIGO ST-CBD004.
           PERFORM UNTIL ST-CBD004 = "10"
             ADD 1 TO ULT-CODIGO
             MOVE ULT-CODIGO TO CODIGO-CB04
             READ CBD004 INVALID KEY MOVE "10" TO ST-CBD004
                  NOT INVALID KEY CONTINUE
             END-READ
           END-PERFORM.
           SUBTRACT 1 FROM ULT-CODIGO.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CBP004-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CBP004-CODIGO.
           MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO PAG-W.
           IF CBP004-ORDER = 1
              MOVE ZEROS TO CODIGO-CB04
              START CBD004 KEY IS NOT < CODIGO-CB04 INVALID KEY
                           MOVE "10" TO ST-CBD004
           ELSE MOVE SPACES TO HISTORICO-CB04
                START CBD004 KEY IS NOT < HISTORICO-CB04 INVALID KEY
                           MOVE "10" TO ST-CBD004.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CBD004 = "10"
             READ CBD004 NEXT RECORD AT END MOVE "10" TO ST-CBD004
              NOT AT END
                MOVE SPACES TO LINDET-REL
                MOVE CODIGO-CB04           TO LINDET-REL(1: 5)
                MOVE HISTORICO-CB04        TO LINDET-REL(6: 33)
                MOVE APURACAO-RED-C-CB04   TO LINDET-REL(38: 5)
                MOVE APURACAO-RED-D-CB04   TO LINDET-REL(44: 5)
                WRITE REG-RELAT FROM LINDET
                ADD 1 TO LIN
                IF LIN > 56 PERFORM CABECALHO
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
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CBP004-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CBD004 CXD020.
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
