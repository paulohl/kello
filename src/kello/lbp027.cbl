       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP027.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 24/05/2000
      *DESCRIÇÃO: CADASTRO DE TIPO DE FOTOS - LABORATORIO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY LBPX027.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY LBPW027.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP027.CPB".
           COPY "LBP027.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-LBD027             PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(2)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de CODIGO-LB27 utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
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
           "RELACAO DE TIPO DE FOTOS - LABORATORIO ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "CD    DESCRICAO                      ".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO.
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "LBD027" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD027.
           OPEN I-O LBD027
           MOVE 1 TO GRAVA-W.
           IF ST-LBD027 = "35"
              CLOSE LBD027      OPEN OUTPUT LBD027
              CLOSE LBD027      OPEN I-O LBD027
           END-IF.
           IF ST-LBD027 <> "00"
              MOVE "ERRO ABERTURA LBD027: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD027 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              MOVE 1 TO GS-ORDER
              PERFORM ACHAR-CODIGO
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 2) TO GS-CODIGO
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
           MOVE GS-CODIGO       TO CODIGO-LB27.
           READ LBD027 INVALID KEY INITIALIZE REG-LBD027
                                   MOVE 1 TO GRAVA-W.
           MOVE DESCRICAO-LB27     TO GS-DESCRICAO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-LBD027
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE LBD027.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-CODIGO       TO CODIGO-LB27.
           MOVE GS-DESCRICAO    TO DESCRICAO-LB27.
           IF GRAVA-W = 1
              WRITE REG-LBD027 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-LBD027 INVALID KEY
                PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-LBD027       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF GS-ORDER = ZEROS
              MOVE SPACES TO DESCRICAO-LB27
              START LBD027 KEY IS NOT < DESCRICAO-LB27
                    INVALID KEY MOVE "10" TO ST-LBD027
           ELSE
             MOVE ZEROS TO CODIGO-LB27
               START LBD027 KEY IS NOT < CODIGO-LB27
                 INVALID KEY MOVE "10" TO ST-LBD027.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-LBD027 = "10"
              READ LBD027 NEXT RECORD AT END MOVE "10" TO ST-LBD027
              NOT AT END
                ADD 1 TO GS-CONT
                MOVE SPACES TO GS-LINDET
                MOVE CODIGO-LB27          TO GS-LINDET(01: 06)
                MOVE DESCRICAO-LB27       TO GS-LINDET(07: 31)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "LBP027" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO PAG-W.
           IF GS-ORDER = 1
              MOVE ZEROS TO CODIGO-LB27
              START LBD027 KEY IS NOT < CODIGO-LB27 INVALID KEY
                           MOVE "10" TO ST-LBD027
           ELSE MOVE SPACES TO DESCRICAO-LB27
                START LBD027 KEY IS NOT < DESCRICAO-LB27 INVALID KEY
                           MOVE "10" TO ST-LBD027.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-LBD027 = "10"
             READ LBD027 NEXT RECORD AT END MOVE "10" TO ST-LBD027
              NOT AT END
               MOVE SPACES TO LINDET-REL
                MOVE CODIGO-LB27              TO LINDET-REL(01: 06)
                MOVE DESCRICAO-LB27           TO LINDET-REL(07: 31)
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
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO ULT-CODIGO ST-LBD027.
           PERFORM UNTIL ST-LBD027 = "10"
             ADD 1 TO ULT-CODIGO
             MOVE ULT-CODIGO TO CODIGO-LB27
             READ LBD027 INVALID KEY MOVE "10" TO ST-LBD027
                  NOT INVALID KEY CONTINUE
             END-READ
           END-PERFORM.
           SUBTRACT 1 FROM ULT-CODIGO.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO GS-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO GS-CODIGO
           MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE LBD027.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
