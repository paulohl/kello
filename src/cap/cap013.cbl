       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAP013.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 17/08/1998
      *DESCRIÇÃO: Cadastro de bancos p/ Posição financeira
      *           Os códigos serão os mesmos do cadastro geral.
      *           Esse cadastro é só p/ identificar, os bancos
      *           do cadastro geral.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CAPX013.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CAPW013.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY "CAP013.CPB".
           COPY "CAP013.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD013             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  lin                   pic 9(02)    value zeros.
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
           "RELACAO DE BANCOS P/ POSICAO FINANCEIRA".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "COD.      NOME-BANCO   ".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

           COPY IMPRESSORA.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CAP013-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CAP013-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CAP013-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CAP013-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD013" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD013.
           OPEN I-O CAD013.
           OPEN INPUT CGD001.
           IF ST-CAD013 = "35"
              CLOSE CAD013      OPEN OUTPUT CAD013
              CLOSE CAD013      OPEN I-O CAD013
           END-IF.
           IF ST-CAD013 <> "00"
              MOVE "ERRO ABERTURA CAD013: "  TO CAP013-MENSAGEM-ERRO
              MOVE ST-CAD013 TO CAP013-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CAP013-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CAP013-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CAP013-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CAP013-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CAP013-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM LIMPAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CAP013-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM LIMPAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CAP013-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN CAP013-EXIBE-BANCO-TRUE
                    PERFORM EXIBE-BANCO
               WHEN CAP013-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CAP013-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CAD013
           INITIALIZE CAP013-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           MOVE CAP013-CODIGO TO CODIGO-CA13.
           READ CAD013 INVALID KEY CONTINUE
             NOT INVALID KEY
                 DELETE CAD013
                 PERFORM LIMPAR-DADOS.
       SALVAR-DADOS SECTION.
           MOVE CAP013-CODIGO       TO CODIGO-CA13.
           WRITE REG-CAD013 INVALID KEY CONTINUE.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CAP013-MENSAGEM-ERRO
           MOVE ST-CAD013       TO CAP013-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       EXIBE-BANCO SECTION.
           MOVE CAP013-CODIGO TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE 1 TO CAP013-ERRO
             NOT INVALID KEY
               MOVE NOME-CG01 TO CAP013-NOME
               MOVE ZEROS     TO CAP013-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO CODIGO-CA13
           START CAD013 KEY IS NOT < CODIGO-CA13
                 INVALID KEY MOVE "10" TO ST-CAD013.
           MOVE SPACES TO CAP013-LINDET.
           MOVE ZEROS TO CAP013-CONT.
           PERFORM UNTIL ST-CAD013 = "10"
              READ CAD013 NEXT RECORD AT END MOVE "10" TO ST-CAD013
              NOT AT END
                ADD 1 TO CAP013-CONT
                MOVE SPACES TO CAP013-LINDET
                MOVE CODIGO-CA13       TO CAP013-LINDET(01: 06)
                                          CODIGO-CG01
                READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                END-READ
                MOVE NOME-CG01         TO CAP013-LINDET(10: 30)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CAP013-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CAP013" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO CODIGO-CA13
           START CAD013 KEY IS NOT < CODIGO-CA13 INVALID KEY
                           MOVE "10" TO ST-CAD013.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CAD013 = "10"
             READ CAD013 NEXT RECORD AT END MOVE "10" TO ST-CAD013
              NOT AT END
               MOVE SPACES TO LINDET-REL
                MOVE CODIGO-CA13           TO LINDET-REL(01: 08)
                                              CODIGO-CG01
                READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                END-READ
                MOVE NOME-CG01             TO LINDET-REL(09: 20)
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
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CAP013-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD013 CGD001.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

