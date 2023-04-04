       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP003.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 16/10/1998
      *DESCRIÇÃO: CADASTRO DE SITUACAO DE CHEQUES -CONTROLE BANCARIO
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CBPX003.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CBPW003.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CBP003.CPB".
           COPY "CBP003.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CBD003             PIC XX       VALUE SPACES.
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
           "RELACAO DE SITUACAO DE CHEQUES - CONT.BANCARIO".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "CD  DESCRICAO  ".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CBP003-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CBP003-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CBP003-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE CBP003-VERSION-NO  TO DS-VERSION-NO.
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CBD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD003.
           OPEN I-O CBD003.
           MOVE 1 TO GRAVA-W.
           IF ST-CBD003 = "35"
              CLOSE CBD003      OPEN OUTPUT CBD003
              CLOSE CBD003      OPEN I-O CBD003
           END-IF.
           IF ST-CBD003 <> "00"
              MOVE "ERRO ABERTURA CBD003: "  TO CBP003-MENSAGEM-ERRO
              MOVE ST-CBD003 TO CBP003-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CBP003-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                MOVE 1 TO CBP003-ORDER
                PERFORM ACHAR-CODIGO
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CBP003-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CBP003-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP003-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP003-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CBP003-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CBP003-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CBP003-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP003-CARREGA-LIST-BOX-TRUE
                   MOVE CBP003-LINDET(1: 2) TO CBP003-SITUACAO
                   PERFORM CARREGAR-DADOS
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
           MOVE CBP003-SITUACAO    TO SITUACAO-CB03.
           READ CBD003 INVALID KEY INITIALIZE REG-CBD003
                                   MOVE 1 TO GRAVA-W.
           MOVE NOME-SIT-CB03      TO CBP003-DESCR-SITUACAO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CBD003
           MOVE CBP003-ORDER TO ORDEM-W
           INITIALIZE CBP003-DATA-BLOCK
           MOVE ORDEM-W TO CBP003-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE CBD003.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE CBP003-SITUACAO           TO SITUACAO-CB03.
           MOVE CBP003-DESCR-SITUACAO     TO NOME-SIT-CB03.
           IF GRAVA-W = 1
              WRITE REG-CBD003 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE SUBTRACT 1 FROM ULT-CODIGO
                REWRITE REG-CBD003 INVALID KEY
                PERFORM ERRO-GRAVACAO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CBP003-MENSAGEM-ERRO
           MOVE ST-CBD003       TO CBP003-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
           PERFORM ACHAR-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF CBP003-ORDER = ZEROS
              MOVE SPACES TO NOME-SIT-CB03
              START CBD003 KEY IS NOT < NOME-SIT-CB03
                    INVALID KEY MOVE "10" TO ST-CBD003
           ELSE
             MOVE ZEROS TO SITUACAO-CB03
               START CBD003 KEY IS NOT < SITUACAO-CB03
                 INVALID KEY MOVE "10" TO ST-CBD003.
           MOVE SPACES TO CBP003-LINDET.
           MOVE ZEROS TO CBP003-CONT.
           PERFORM UNTIL ST-CBD003 = "10"
              READ CBD003 NEXT RECORD AT END MOVE "10" TO ST-CBD003
              NOT AT END
                ADD 1 TO CBP003-CONT
                MOVE SPACES                TO CBP003-LINDET
                MOVE SITUACAO-CB03         TO CBP003-LINDET(1: 7)
                MOVE NOME-SIT-CB03         TO CBP003-LINDET(13: 30)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CBP003-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "CBP003" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO ULT-CODIGO ST-CBD003.
           PERFORM UNTIL ST-CBD003 = "10"
             ADD 1 TO ULT-CODIGO
             MOVE ULT-CODIGO TO SITUACAO-CB03
             READ CBD003 INVALID KEY MOVE "10" TO ST-CBD003
                  NOT INVALID KEY CONTINUE
             END-READ
           END-PERFORM.
           SUBTRACT 1 FROM ULT-CODIGO.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CBP003-SITUACAO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CBP003-SITUACAO.
           MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO PAG-W.
           IF CBP003-ORDER = 1
              MOVE ZEROS TO SITUACAO-CB03
              START CBD003 KEY IS NOT < SITUACAO-CB03 INVALID KEY
                           MOVE "10" TO ST-CBD003
           ELSE MOVE SPACES TO NOME-SIT-CB03
                START CBD003 KEY IS NOT < NOME-SIT-CB03 INVALID KEY
                           MOVE "10" TO ST-CBD003.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CBD003 = "10"
             READ CBD003 NEXT RECORD AT END MOVE "10" TO ST-CBD003
              NOT AT END
                MOVE SPACES TO LINDET-REL
                MOVE SITUACAO-CB03         TO LINDET-REL(1: 4)
                MOVE NOME-SIT-CB03         TO LINDET-REL(5: 30)
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
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CBP003-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CBD003.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.

