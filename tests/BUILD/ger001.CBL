       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GER001.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 27-11-2005.
      *DESCRIÇÃO: Cadastro de Usuários CHEFES
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.
           COPY GERX001.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.
       COPY GERW001.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "GER001.CPB".
           COPY "GER001.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       77  LIN                       PIC 9(02).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-GED001             PIC XX       VALUE SPACES.
           05  CODIGO-E              PIC Z(3).
           05  ULT-CODIGO            PIC 9(3)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  USUARIO-WK            PIC 9(02)    VALUE ZEROS.
           05  TIPO1-W               PIC 9(01)    VALUE ZEROS.
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
           "RELACAO DE CADASTRO ATENDIMENTO A CLIENTE".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
         "USUARIO    PROGRAMA                               OBSERVACAO".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).


       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "GED001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-GED001.
           OPEN I-O GED001
           CLOSE    GED001
           OPEN I-O GED001
           OPEN INPUT CAD002
           MOVE 1 TO GRAVA-W.
           IF ST-GED001 = "35"
              CLOSE GED001      OPEN OUTPUT GED001
              CLOSE GED001      OPEN I-O    GED001
           END-IF.
           IF ST-GED001 <> "00"
              MOVE "ERRO ABERTURA GED001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-GED001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
      *          MOVE 1 TO GS-ORDER
      *          PERFORM ACHAR-CODIGO
               PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                    PERFORM CARREGA-ULTIMOS
                    MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(01:03)         TO GS-USUARIO
                   MOVE GS-LINDET(12:38)         TO GS-PROGRAMA
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-USUARIO-FLG-TRUE
                   PERFORM LE-USUARIO
               WHEN GS-VERIFICA-PROG-FLG-TRUE
                   PERFORM VERIFICAR-PROGRAMAS
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VERIFICAR-PROGRAMAS SECTION.
           EVALUATE GS-PROGRAMA
               WHEN "CXP100 - Movimento do Caixa           "
                    MOVE 0 TO GS-FLAG-CRITICA
               WHEN "CPP020 - Movimento do Contas a Pagar  "
                    MOVE 0 TO GS-FLAG-CRITICA
               WHEN "CRP020 - Movimento do Contas a Receber"
                    MOVE 0 TO GS-FLAG-CRITICA
               WHEN "CHP010 - Movimento de Cheques         "
                    MOVE 0 TO GS-FLAG-CRITICA
               WHEN "CCP100 - Movimento do Conta Corrente  "
                    MOVE 0 TO GS-FLAG-CRITICA
               WHEN "COP040 - Movimento do Contrato        "
                    MOVE 0 TO GS-FLAG-CRITICA
               WHEN OTHER MOVE "Programa Informado Inválido" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.

       LE-USUARIO SECTION.
           MOVE GS-USUARIO        TO CODIGO-CA002
           READ CAD002 INVALID KEY
               MOVE "Usuário Não Cadastrado" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
               MOVE SPACES TO NOME-REDUZ-CA002.

           MOVE NOME-REDUZ-CA002 TO GS-DESC-USUARIO.

       CARREGAR-DADOS SECTION.
           MOVE ZEROS             TO GRAVA-W.
           MOVE GS-USUARIO(1:3)   TO USUARIO-GE01
           MOVE GS-PROGRAMA       TO PROGRAMA-GE01
           READ GED001 INVALID KEY
                INITIALIZE REG-GED001
                MOVE 1 TO GRAVA-W.
           MOVE DESCRICAO-GE01    TO GS-DESCRICAO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-GED001
           MOVE GS-ORDER    TO ORDEM-W
           MOVE GS-USUARIO  TO USUARIO-WK
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W     TO GS-ORDER
           MOVE USUARIO-WK  TO GS-USUARIO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE GED001.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-USUARIO        TO USUARIO-GE01
           MOVE GS-PROGRAMA(1:38) TO PROGRAMA-GE01
           MOVE GS-DESCRICAO      TO DESCRICAO-GE01
           IF GRAVA-W = 1
              WRITE REG-GED001 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE
              REWRITE REG-GED001 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-GED001       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX"   TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-GED001
           IF GS-ORDER = ZEROS
              MOVE SPACES          TO PROGRAMA-GE01
              START GED001 KEY IS NOT < ALT-GE01 INVALID KEY
                    MOVE "10" TO ST-GED001
           ELSE
              MOVE ZEROS           TO USUARIO-GE01
              START GED001 KEY IS NOT < CHAVE-GE01 INVALID KEY
                    MOVE "10" TO ST-GED001.

           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-GED001 = "10"
              READ GED001 NEXT RECORD AT END
                   MOVE "10" TO ST-GED001
              NOT AT END
                   MOVE SPACES                   TO GS-LINDET
                   MOVE USUARIO-GE01             TO GS-LINDET(1:3)
                                                    CODIGO-CA002
                   READ CAD002 INVALID KEY
                        MOVE "-----"             TO NOME-REDUZ-CA002
                   END-READ
                   MOVE NOME-REDUZ-CA002         TO GS-LINDET(5:5)
                   MOVE PROGRAMA-GE01            TO GS-LINDET(12:38)
                   MOVE DESCRICAO-GE01           TO GS-LINDET(51:60)
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "GER001" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE 0                              TO PAG-W
           IF GS-ORDER = 1
              MOVE ZEROS                        TO USUARIO-GE01
              START GED001 KEY IS NOT < CHAVE-GE01 INVALID KEY
                    MOVE "10" TO ST-GED001
           ELSE
              MOVE SPACES                     TO PROGRAMA-GE01
              START GED001 KEY IS NOT < ALT-GE01 INVALID KEY
                    MOVE "10" TO ST-GED001.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-GED001 = "10"
             READ GED001 NEXT RECORD AT END
                  MOVE "10" TO ST-GED001
             NOT AT END
                  MOVE SPACES                   TO LINDET-REL
                  MOVE USUARIO-GE01             TO LINDET-REL(1:3)
                                                   CODIGO-CA002
                  READ CAD002 INVALID KEY
                       MOVE "-----"             TO NOME-REDUZ-CA002
                  END-READ
                  MOVE NOME-REDUZ-CA002         TO LINDET-REL(5:5)
                  MOVE PROGRAMA-GE01            TO LINDET-REL(12:38)
                  MOVE DESCRICAO-GE01           TO LINDET-REL(51:60)

                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
             END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to gs-flag-critica
           move spaces to mensagem.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE GED001 CAD002
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

