       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP002.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14/01/2006
      *DESCRIÇÃO: Cadastro de VARIAÇÃO DE COMISSÃO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       PRINTER IS LPRINTER
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY RCPX001.
           COPY RCPX002.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY RCPW001.
       COPY RCPW002.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP002.CPB".
           COPY "RCP002.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RCD001             PIC XX       VALUE SPACES.
           05  ST-RCD002             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ,ZZZ.
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
           "RELACAO DE CADASTRO DE COMISSAO".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
       "COD   TIPO        0 à 30 dias  31 à 120 dias  121 à 240 dias  24
      -"1 dias  +".

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
           MOVE "RCD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD001.
           MOVE "RCD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD002.
           OPEN I-O RCD001 RCD002
           IF ST-RCD001 = "35"
              CLOSE RCD001      OPEN OUTPUT RCD001
              CLOSE RCD001      OPEN I-O RCD001
           END-IF.
           IF ST-RCD002 = "35"
              CLOSE RCD002      OPEN OUTPUT RCD002
              CLOSE RCD002      OPEN I-O RCD002
           END-IF.
           IF ST-RCD001 <> "00"
              MOVE "ERRO ABERTURA RCD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD002 <> "00"
              MOVE "ERRO ABERTURA RCD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD002 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
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
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 3) TO GS-COMISSAO
                   MOVE GS-LINDET(7:15) TO GS-TIPO
                   PERFORM CARREGAR-DADOS
               WHEN GS-VERIFICA-CODIGO-TRUE
                   PERFORM VERIFICAR-RCD001
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-RCD002
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       CARREGAR-RCD002 SECTION.
           MOVE GS-TIPO           TO TIPO-RC02
           MOVE GS-COMISSAO       TO CODIGO-COMIS-RC02.
           READ RCD002 INVALID KEY
                INITIALIZE REG-RCD002
                MOVE 0 TO GS-GRAVA
           NOT INVALID KEY
                MOVE 1                            TO GS-GRAVA
                MOVE COMIS-0-30-DIAS              TO GS-DIAS-0-30
                MOVE COMIS-31-120-DIAS            TO GS-DIAS-31-120
                MOVE COMIS-121-240-DIAS           TO GS-DIAS-121-240
                MOVE COMIS-241-000-DIAS           TO GS-DIAS-241-000.

       VERIFICAR-RCD001 SECTION.
           MOVE GS-COMISSAO TO CODIGO-COMIS-RC01
           READ RCD001 INVALID KEY
               MOVE "Código Informado Não Cadastrado Na Tabela de Comiss
      -             "ão" TO MENSAGEM
               MOVE "C"  TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to gs-flag-critica
           move spaces to mensagem.

       CARREGAR-DADOS SECTION.
           MOVE GS-TIPO           TO TIPO-RC02
           MOVE GS-COMISSAO       TO CODIGO-COMIS-RC02.
           READ RCD002 INVALID KEY
                INITIALIZE REG-RCD002
                MOVE 0 TO GS-GRAVA
           NOT INVALID KEY
                MOVE COMIS-0-30-DIAS              TO GS-DIAS-0-30
                MOVE COMIS-31-120-DIAS            TO GS-DIAS-31-120
                MOVE COMIS-121-240-DIAS           TO GS-DIAS-121-240
                MOVE COMIS-241-000-DIAS           TO GS-DIAS-241-000.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-RCD002
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE RCD002
           PERFORM LIMPAR-DADOS.
       SALVAR-DADOS SECTION.
           MOVE GS-TIPO           TO TIPO-RC02
           MOVE GS-COMISSAO       TO CODIGO-COMIS-RC02
           MOVE GS-DIAS-0-30      TO COMIS-0-30-DIAS
           MOVE GS-DIAS-31-120    TO COMIS-31-120-DIAS
           MOVE GS-DIAS-121-240   TO COMIS-121-240-DIAS
           MOVE GS-DIAS-241-000   TO COMIS-241-000-DIAS
           IF GS-GRAVA = 0
              WRITE REG-RCD002 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE
              REWRITE REG-RCD002 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-RCD002       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO TIPO-RC02
           MOVE ZEROS  TO CODIGO-COMIS-RC02
           START RCD002 KEY IS NOT < CHAVE-RC02
               INVALID KEY MOVE "10" TO ST-RCD002.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-RCD002 = "10"
              READ RCD002 NEXT RECORD AT END
                   MOVE "10" TO ST-RCD002
              NOT AT END
                ADD 1 TO GS-CONT
      *         MOVE SPACES TO GS-LINDET
                MOVE CODIGO-COMIS-RC02            TO GS-LINDET(01: 06)
                MOVE TIPO-RC02                    TO GS-LINDET(07: 15)
                MOVE COMIS-0-30-DIAS              TO VALOR-E
                MOVE VALOR-E                      TO GS-LINDET(24: 06)
                MOVE COMIS-31-120-DIAS            TO VALOR-E
                MOVE VALOR-E                      TO GS-LINDET(39: 06)
                MOVE COMIS-121-240-DIAS           TO VALOR-E
                MOVE VALOR-E                      TO GS-LINDET(55: 06)
                MOVE COMIS-241-000-DIAS           TO VALOR-E
                MOVE VALOR-E                      TO GS-LINDET(68: 06)

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
           MOVE "RCP002" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE SPACES TO TIPO-RC02
           MOVE ZEROS  TO CODIGO-COMIS-RC02
           START RCD002 KEY IS NOT < CHAVE-RC02 INVALID KEY
                        MOVE "10" TO ST-RCD002
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-RCD002 = "10"
             READ RCD002 NEXT RECORD AT END
                MOVE "10" TO ST-RCD002
             NOT AT END
                MOVE SPACES                       TO LINDET-REL
                MOVE CODIGO-COMIS-RC02            TO LINDET-REL(01: 06)
                MOVE TIPO-RC02                    TO LINDET-REL(07: 15)
                MOVE COMIS-0-30-DIAS              TO VALOR-E
                MOVE VALOR-E                      TO LINDET-REL(24: 06)
                MOVE COMIS-31-120-DIAS            TO VALOR-E
                MOVE VALOR-E                      TO LINDET-REL(39: 06)
                MOVE COMIS-121-240-DIAS           TO VALOR-E
                MOVE VALOR-E                      TO LINDET-REL(55: 06)
                MOVE COMIS-241-000-DIAS           TO VALOR-E
                MOVE VALOR-E                      TO LINDET-REL(68: 06)

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
           MOVE 4 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE RCD001 RCD002.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
