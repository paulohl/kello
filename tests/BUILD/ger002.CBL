       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GER002.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 27-11-2005.
      *DESCRIÇÃO: Liberação dos Programas CHEFES
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.
           COPY GERX001.
           COPY GERX002.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.
       COPY GERW001.
       COPY GERW002.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "GER002.CPB".
           COPY "GER002.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       77  LIN                       PIC 9(02).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-GED001             PIC XX       VALUE SPACES.
           05  ST-GED002             PIC XX       VALUE SPACES.
           05  CODIGO-E              PIC Z(3).
           05  ULT-CODIGO            PIC 9(3)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  USUARIO-WK            PIC 9(02)    VALUE ZEROS.
           05  PROGRAMA-WK           PIC X(38)    VALUE SPACES.
           05  TIPO1-W               PIC 9(01)    VALUE ZEROS.
           05  MASC-DATA             PIC 99/99/9999 VALUE ZEROS.
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
           "RELACAO DAS PERMISSOES DOS USUARIOS".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(110)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)   VALUE
          "USUARIO    PROGRAMA                               USUARIO
      -   "OPERACAO  DATA LIMITE IDENTIFICACAO".

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
           MOVE "GED002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-GED002.
           OPEN I-O GED001 GED002
           CLOSE    GED001 GED002
           OPEN I-O GED001 GED002
           OPEN INPUT CAD002
           MOVE 1 TO GRAVA-W.
           IF ST-GED001 = "35"
              CLOSE GED001      OPEN OUTPUT GED001
              CLOSE GED001      OPEN I-O    GED001
           END-IF.
           IF ST-GED002 = "35"
              CLOSE GED002      OPEN OUTPUT GED002
              CLOSE GED002      OPEN I-O    GED002
           END-IF.
           IF ST-GED001 <> "00"
              MOVE "ERRO ABERTURA GED001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-GED001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-GED002 <> "00"
              MOVE "ERRO ABERTURA GED002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-GED002 TO GS-MENSAGEM-ERRO(23: 02)
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
                   MOVE GS-LINDET(01:03)         TO GS-USUARIO-CHEFE
                   MOVE GS-LINDET(12:38)         TO GS-PROGRAMA
                   MOVE GS-LINDET(51:3)          TO GS-USUARIO-PERMITIDO
                   MOVE SPACES TO GS-DESCRICAO
                   STRING GS-LINDET(84:60) DELIMITED BY " "
                   INTO GS-DESCRICAO
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-USUARIO-C-FLG-TRUE
                   PERFORM LE-USUARIO-C
               WHEN GS-LE-USUARIO-P-FLG-TRUE
                   PERFORM LE-USUARIO-P
               WHEN GS-VERIFICA-PROG-FLG-TRUE
                   PERFORM VERIFICAR-PROGRAMAS
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-PROGRAMA
               WHEN "CXP100 - Movimento do Caixa           "
                    MOVE "Módulo de Consulta Movimento do Caixa Não Impl
      -             "ementado" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               WHEN "CPP020 - Movimento do Contas a Pagar  "
                    CALL "CPP051T" USING PARAMETROS-W PASSAR-PARAMETROS
                    CANCEL "CPP051T"
                    MOVE PASSAR-STRING-1 TO GS-DESCRICAO
               WHEN "CRP020 - Movimento do Contas a Receber"
                    MOVE "Módulo de Consulta Movimento do Contas a Receb
      -             "er Não Implementado" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               WHEN "CHP010 - Movimento de Cheques         "
                    MOVE "Módulo de Consulta Movimento de Cheques Não Im
      -             "plementado" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               WHEN "CCP100 - Movimento do Conta Corrente  "
                    MOVE "Módulo de Consulta Movimento do Conta Corrente
      -             "Não Implementado" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               WHEN "COP040 - Movimento do Contrato        "
                    MOVE "Módulo de Consulta Movimento do Contrato Não I
      -             "mplementado" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               WHEN OTHER MOVE "Programa Informado Inválido" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.

       VERIFICAR-PROGRAMAS SECTION.
           EVALUATE GS-PROGRAMA
               WHEN "CXP100 - Movimento do Caixa           "
                    PERFORM VERIFICAR-USUARIO-PROGRAMA
               WHEN "CPP020 - Movimento do Contas a Pagar  "
                    PERFORM VERIFICAR-USUARIO-PROGRAMA
               WHEN "CRP020 - Movimento do Contas a Receber"
                    PERFORM VERIFICAR-USUARIO-PROGRAMA
               WHEN "CHP010 - Movimento de Cheques         "
                    PERFORM VERIFICAR-USUARIO-PROGRAMA
               WHEN "CCP100 - Movimento do Conta Corrente  "
                    PERFORM VERIFICAR-USUARIO-PROGRAMA
               WHEN "COP040 - Movimento do Contrato        "
                    PERFORM VERIFICAR-USUARIO-PROGRAMA
               WHEN OTHER MOVE "Programa Informado Inválido" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.

       VERIFICAR-USUARIO-PROGRAMA SECTION.
           MOVE GS-USUARIO-CHEFE       TO USUARIO-GE01
           MOVE GS-PROGRAMA            TO PROGRAMA-GE01
           READ GED001 INVALID KEY
               MOVE "Usuário CHEFE e Programa Não Cadastrado" TO
               MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM.


       LE-USUARIO-C SECTION.
           MOVE GS-USUARIO-CHEFE      TO CODIGO-CA002
           READ CAD002 INVALID KEY
               MOVE "Usuário Não Cadastrado" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
               MOVE SPACES TO NOME-REDUZ-CA002.

           MOVE NOME-REDUZ-CA002 TO GS-DESC-USUARIO-C.

       LE-USUARIO-P SECTION.
           MOVE GS-USUARIO-PERMITIDO  TO CODIGO-CA002
           READ CAD002 INVALID KEY
               MOVE "Usuário Não Cadastrado" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
               MOVE SPACES TO NOME-REDUZ-CA002.

           MOVE NOME-REDUZ-CA002 TO GS-DESC-USUARIO-P.

       CARREGAR-DADOS SECTION.
           MOVE ZEROS                TO GRAVA-W.
           MOVE GS-USUARIO-CHEFE     TO USUARIO-C-GE02
           MOVE GS-PROGRAMA          TO PROGRAMA-GE02
           MOVE GS-USUARIO-PERMITIDO TO USUARIO-P-GE02
           MOVE GS-DESCRICAO         TO IDENTIFICACAO-GE02
           READ GED002 INVALID KEY
                INITIALIZE REG-GED002
                MOVE 1 TO GRAVA-W.

      *    MOVE IDENTIFICACAO-GE02   TO GS-DESCRICAO
           MOVE OPERACAO-GE02        TO GS-OPERACAO
           MOVE DATA-LIMITE-GE02     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO GS-DATA-LIMITE.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-GED002
           MOVE GS-ORDER          TO ORDEM-W
           MOVE GS-USUARIO-CHEFE  TO USUARIO-WK
           MOVE GS-PROGRAMA       TO PROGRAMA-WK
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W     TO GS-ORDER
           MOVE USUARIO-WK  TO GS-USUARIO-CHEFE
           MOVE PROGRAMA-WK TO GS-PROGRAMA
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE GED002.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-USUARIO-CHEFE     TO USUARIO-C-GE02
           MOVE GS-PROGRAMA          TO PROGRAMA-GE02
           MOVE GS-USUARIO-PERMITIDO TO USUARIO-P-GE02
           MOVE SPACES               TO IDENTIFICACAO-GE02
           STRING GS-DESCRICAO DELIMITED BY " " INTO IDENTIFICACAO-GE02
           MOVE GS-OPERACAO          TO OPERACAO-GE02
           MOVE GS-DATA-LIMITE       TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO DATA-LIMITE-GE02
           IF GRAVA-W = 1
              WRITE REG-GED002 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE
              REWRITE REG-GED002 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-GED002       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX"   TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-GED002
           IF GS-ORDER = ZEROS
              MOVE SPACES          TO PROGRAMA-GE02
              START GED002 KEY IS NOT < ALT-GE02 INVALID KEY
                    MOVE "10" TO ST-GED002
           ELSE
              MOVE ZEROS           TO USUARIO-C-GE02
              START GED002 KEY IS NOT < CHAVE-GE02 INVALID KEY
                    MOVE "10" TO ST-GED002.

           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-GED002 = "10"
              READ GED002 NEXT RECORD AT END
                   MOVE "10" TO ST-GED002
              NOT AT END
                   MOVE SPACES                   TO GS-LINDET
                   MOVE USUARIO-C-GE02           TO GS-LINDET(1:3)
                                                    CODIGO-CA002
                   READ CAD002 INVALID KEY
                        MOVE "-----"             TO NOME-REDUZ-CA002
                   END-READ
                   MOVE NOME-REDUZ-CA002         TO GS-LINDET(5:5)
                   MOVE PROGRAMA-GE02            TO GS-LINDET(12:38)
                   MOVE USUARIO-P-GE02           TO GS-LINDET(51:3)
                                                    CODIGO-CA002
                   READ CAD002 INVALID KEY
                        MOVE "-----"             TO NOME-REDUZ-CA002
                   END-READ
                   MOVE NOME-REDUZ-CA002         TO GS-LINDET(55:5)
                   MOVE OPERACAO-GE02            TO GS-LINDET(62:10)
                   MOVE DATA-LIMITE-GE02         TO DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV                 TO MASC-DATA
                   MOVE MASC-DATA                TO GS-LINDET(72:10)
                   MOVE IDENTIFICACAO-GE02       TO GS-LINDET(84:30)

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
           MOVE "GER002" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE 0                              TO PAG-W
           IF GS-ORDER = 1
              MOVE ZEROS                        TO USUARIO-C-GE02
              START GED002 KEY IS NOT < CHAVE-GE02 INVALID KEY
                    MOVE "10" TO ST-GED002
           ELSE
              MOVE SPACES                     TO PROGRAMA-GE02
              START GED002 KEY IS NOT < ALT-GE02 INVALID KEY
                    MOVE "10" TO ST-GED002.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-GED002 = "10"
             READ GED002 NEXT RECORD AT END
                  MOVE "10" TO ST-GED002
             NOT AT END
                  MOVE SPACES                    TO LINDET-REL
                  MOVE USUARIO-C-GE02           TO LINDET-REL(1:3)
                                                   CODIGO-CA002
                  READ CAD002 INVALID KEY
                       MOVE "-----"             TO NOME-REDUZ-CA002
                  END-READ
                  MOVE NOME-REDUZ-CA002         TO LINDET-REL(5:5)
                  MOVE PROGRAMA-GE02            TO LINDET-REL(12:38)
                  MOVE USUARIO-P-GE02           TO LINDET-REL(51:3)
                                                   CODIGO-CA002
                  READ CAD002 INVALID KEY
                       MOVE "-----"             TO NOME-REDUZ-CA002
                  END-READ
                  MOVE NOME-REDUZ-CA002         TO LINDET-REL(55:5)
                  MOVE OPERACAO-GE02            TO LINDET-REL(62:10)
                  MOVE DATA-LIMITE-GE02         TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV                 TO MASC-DATA
                  MOVE MASC-DATA                TO LINDET-REL(72:10)
                  MOVE IDENTIFICACAO-GE02       TO LINDET-REL(84:60)

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
           CLOSE GED001 GED002 CAD002
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

