       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP030.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 03/07/1998
      *DESCRIÇÃO: Cadastro de Portador
      *           Os portadores, serão os locais p/ pagamento ou
      *           recebimento dos documentos.
      *           Ex. carteira, banco, ch pre
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CXPX030.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CXPW030.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP030.CPB".
           COPY "CXP030.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD030             PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(2)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de portador utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           COPY "PARAMETR".

       01  EMP-REFERENCIA.
           10  FILLER            PIC X(15)
               VALUE "\PROGRAMA\KELLO".
           10  VAR1              PIC X VALUE "\".
           10  EMP-REC           PIC XXX.
           10  VAR2              PIC X VALUE "\".
           10  ARQ-REC           PIC X(10).
       01  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

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
           "RELACAO DE HISTORICO         ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "COD.      DESCRICAO DO HISTORICO".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP030-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CXP030-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP030-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE CXP030-VERSION-NO  TO DS-VERSION-NO.
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CXD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD030.
           OPEN I-O CXD030
           MOVE 1 TO GRAVA-W.
           IF ST-CXD030 = "35"
              CLOSE CXD030      OPEN OUTPUT CXD030
              CLOSE CXD030      OPEN I-O CXD030
           END-IF.
           IF ST-CXD030 <> "00"
              MOVE "ERRO ABERTURA CXD030: "  TO CXP030-MENSAGEM-ERRO
              MOVE ST-CXD030 TO CXP030-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP030-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                MOVE 1 TO CXP030-ORDER
                PERFORM ACHAR-CODIGO
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP030-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CXP030-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CXP030-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CXP030-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CXP030-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CXP030-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN CXP030-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CXP030-CARREGA-LIST-BOX-TRUE
                   MOVE CXP030-LINDET(1: 2) TO CXP030-CODIGO
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
           MOVE CXP030-CODIGO       TO COD-HISTORICO-CX30.
           READ CXD030 INVALID KEY INITIALIZE REG-CXD030
                                   MOVE 1 TO GRAVA-W.
           MOVE HISTORICO-CX30    TO CXP030-NOME.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CXD030
           MOVE CXP030-ORDER TO ORDEM-W
           INITIALIZE CXP030-DATA-BLOCK
           MOVE ORDEM-W TO CXP030-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE CXD030.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE CXP030-CODIGO       TO COD-HISTORICO-CX30.
           MOVE CXP030-NOME         TO HISTORICO-CX30.
           IF GRAVA-W = 1
              WRITE REG-CXD030 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-CXD030 INVALID KEY
                PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CXP030-MENSAGEM-ERRO
           MOVE ST-CXD030       TO CXP030-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF CXP030-ORDER = ZEROS
              MOVE SPACES TO HISTORICO-CX30
              START CXD030 KEY IS NOT < HISTORICO-CX30
                    INVALID KEY MOVE "10" TO ST-CXD030
           ELSE
             MOVE ZEROS TO COD-HISTORICO-CX30
               START CXD030 KEY IS NOT < COD-HISTORICO-CX30
                 INVALID KEY MOVE "10" TO ST-CXD030.
           MOVE SPACES TO CXP030-LINDET.
           MOVE ZEROS TO CXP030-CONT.
           PERFORM UNTIL ST-CXD030 = "10"
              READ CXD030 NEXT RECORD AT END MOVE "10" TO ST-CXD030
              NOT AT END
                ADD 1 TO CXP030-CONT
                MOVE SPACES TO CXP030-LINDET
                MOVE COD-HISTORICO-CX30 TO CXP030-LINDET(01: 06)
                MOVE HISTORICO-CX30     TO CXP030-LINDET(07: 30)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CXP030-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "CXP030" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO PAG-W.
           IF CXP030-ORDER = 1
              MOVE ZEROS TO COD-HISTORICO-CX30
              START CXD030 KEY IS NOT < COD-HISTORICO-CX30 INVALID KEY
                           MOVE "10" TO ST-CXD030
           ELSE MOVE SPACES TO HISTORICO-CX30
                START CXD030 KEY IS NOT < HISTORICO-CX30 INVALID KEY
                           MOVE "10" TO ST-CXD030.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CXD030 = "10"
             READ CXD030 NEXT RECORD AT END MOVE "10" TO ST-CXD030
              NOT AT END
               MOVE SPACES TO LINDET-REL
                MOVE COD-HISTORICO-CX30    TO LINDET-REL(01: 08)
                MOVE HISTORICO-CX30        TO LINDET-REL(09: 30)
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
           MOVE ZEROS TO ULT-CODIGO ST-CXD030.
           PERFORM UNTIL ST-CXD030 = "10"
             ADD 1 TO ULT-CODIGO
             MOVE ULT-CODIGO TO COD-HISTORICO-CX30
             READ CXD030 INVALID KEY MOVE "10" TO ST-CXD030
                  NOT INVALID KEY CONTINUE
             END-READ
           END-PERFORM.
           SUBTRACT 1 FROM ULT-CODIGO.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CXP030-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CXP030-CODIGO
           MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP030-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD030.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.

