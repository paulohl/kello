       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIEP002.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 02/09/1998
      *DESCRIÇÃO: Cadastro de recebedores de mensagens interas CIE

       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.
           COPY CIEPX002.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.
       COPY CIEPW002.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CIEP002.CPB".
           COPY "CIEP002.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD002             PIC XX      VALUE SPACES.
           05  ST-CIED002            PIC XX      VALUE SPACES.
           05  ULT-CODIGO            PIC 9(2)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de tipo-lancamento utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
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
           "RELACAO DE MENSAGEM PADRÃO - CIE ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "CD DESCRICAO                                 RESPONSAVEL
      -    "  SUPERIOR     ".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CIEP002-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W.
           INITIALIZE CIEP002-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CIEP002-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CIEP002-VERSION-NO  TO DS-VERSION-NO

           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL

           OPEN INPUT CAD002.
           OPEN I-O CIED002.
           MOVE 1 TO GRAVA-W.
           IF ST-CIED002 = "35"
              CLOSE CIED002      OPEN OUTPUT CIED002
              CLOSE CIED002      OPEN I-O CIED002
           END-IF.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO CIEP002-MENSAGEM-ERRO
              MOVE ST-CAD002 TO CIEP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CIED002 <> "00"
              MOVE "ERRO ABERTURA CIED002: "  TO CIEP002-MENSAGEM-ERRO
              MOVE ST-CIED002 TO CIEP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CIEP002-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS  MOVE 1 TO CIEP002-ORDER
                PERFORM ACHAR-CODIGO
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CIEP002-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CIEP002-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CIEP002-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CIEP002-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CIEP002-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CIEP002-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN CIEP002-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CIEP002-LER-USUARIO-TRUE
                   PERFORM LER-USUARIO
               WHEN CIEP002-LER-SUPERIOR-TRUE
                   PERFORM LER-SUPERIOR
               WHEN CIEP002-CARREGA-LIST-BOX-TRUE
                   MOVE CIEP002-LINDET(1: 2) TO CIEP002-CODIGO
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

       LER-USUARIO SECTION.
           MOVE CIEP002-RESPONSAVEL TO CODIGO-CA002.
           READ CAD002 INVALID KEY MOVE SPACES TO NOME-CA002.
           MOVE NOME-CA002          TO CIEP002-NOME-RESPONSAVEL.
       LER-SUPERIOR SECTION.
           MOVE CIEP002-SUPERIOR TO CODIGO-CA002.
           READ CAD002 INVALID KEY MOVE SPACES TO NOME-CA002.
           MOVE NOME-CA002          TO CIEP002-NOME-SUPERIOR.
       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE CIEP002-CODIGO       TO CODIGO-CI02
           READ CIED002 INVALID KEY INITIALIZE REG-CIED002
                                   MOVE 1 TO GRAVA-W.
           MOVE DESCRICAO-CI02         TO CIEP002-DESCRICAO.
           MOVE RESPONSAVEL-CI02       TO CIEP002-RESPONSAVEL.
           MOVE SUPERIOR-CI02          TO CIEP002-SUPERIOR.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CIED002
           MOVE CIEP002-ORDER TO ORDEM-W
           INITIALIZE CIEP002-DATA-BLOCK
           MOVE ORDEM-W TO CIEP002-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE CIED002.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE CIEP002-CODIGO       TO CODIGO-CI02
           MOVE CIEP002-DESCRICAO    TO DESCRICAO-CI02.
           MOVE CIEP002-RESPONSAVEL  TO RESPONSAVEL-CI02.
           MOVE CIEP002-SUPERIOR     TO SUPERIOR-CI02.
           IF GRAVA-W = 1
              WRITE REG-CIED002 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-CIED002 INVALID KEY
                PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CIEP002-MENSAGEM-ERRO
           MOVE ST-CIED002       TO CIEP002-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO CODIGO-CI02.
           START CIED002 KEY IS NOT < CODIGO-CI02
                 INVALID KEY MOVE "10" TO ST-CIED002.
           MOVE SPACES TO CIEP002-LINDET.
           MOVE ZEROS TO CIEP002-CONT.
           PERFORM UNTIL ST-CIED002 = "10"
              READ CIED002 NEXT RECORD AT END MOVE "10" TO ST-CIED002
              NOT AT END
                ADD 1 TO CIEP002-CONT
                MOVE SPACES TO CIEP002-LINDET
                MOVE CODIGO-CI02       TO CIEP002-LINDET(01: 03)
                MOVE DESCRICAO-CI02    TO CIEP002-LINDET(04: 40)
                MOVE RESPONSAVEL-CI02  TO CIEP002-LINDET(45: 10)
                MOVE SUPERIOR-CI02     TO CIEP002-LINDET(65: 10)
                MOVE "INSERE-LIST"     TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CIEP002-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CIEP002" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO CODIGO-CI02.
           START CIED002 KEY IS NOT < CODIGO-CI02 INVALID KEY
                        MOVE "10" TO ST-CIED002
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CIED002 = "10"
             READ CIED002 NEXT RECORD AT END MOVE "10" TO ST-CIED002
              NOT AT END
               MOVE SPACES TO LINDET-REL
                MOVE CODIGO-CI02           TO LINDET-REL(01: 03)
                MOVE DESCRICAO-CI02        TO LINDET-REL(04: 40)
                MOVE RESPONSAVEL-CI02      TO LINDET-REL(45: 10)
                MOVE SUPERIOR-CI02         TO LINDET-REL(65: 10)
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
           MOVE ZEROS TO CODIGO-CI02 ULT-CODIGO
           START CIED002 KEY IS NOT < CODIGO-CI02 INVALID KEY
                 MOVE "10" TO ST-CIED002
           END-START
           PERFORM UNTIL ST-CIED002 = "10"
              READ CIED002 NEXT RECORD AT END MOVE "10" TO ST-CIED002
                NOT AT END
                 MOVE CODIGO-CI02 TO ULT-CODIGO
              END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CIEP002-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CIEP002-CODIGO
           MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CIEP002-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CIED002 CAD002.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
