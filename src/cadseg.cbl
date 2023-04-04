       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADSEG.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 25-03-2011
      *DESCRIÇÃO: Cadastro de Segmentos

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       PRINTER IS LPRINTER
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CADSEG.SEL.
           COPY LOGACESS.SEL.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
           COPY CADSEG.FD.
           COPY LOGACESS.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY "CADSEG.CPB".
           COPY "CADSEG.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CADSEG             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(2)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de região utilizado
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
           "RELACAO DE SEGMENTOS      ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "CODIGO NOME SEGMENTO".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU                 PIC 9(04).
             10 WS-MES-CPU                 PIC 9(02).
             10 WS-DIA-CPU                 PIC 9(02).
          05 FILLER                        PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

           COPY IMPRESSORA.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
      *PROCEDURE DIVISION USING POP-UP.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CAP012-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CAP012-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CAP012-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CAP012-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CADSEG"           TO ARQ-REC
           MOVE EMPRESA-REF        TO PATH-CADSEG
           MOVE "LOGACESS"         TO ARQ-REC
           MOVE EMPRESA-REF        TO ARQUIVO-LOGACESS

           OPEN I-O CADSEG
           CLOSE    CADSEG
           OPEN I-O CADSEG
           MOVE 1 TO GRAVA-W.
           IF ST-CADSEG = "35"
              CLOSE CADSEG      OPEN OUTPUT CADSEG
              CLOSE CADSEG      OPEN I-O    CADSEG
           END-IF.
           IF ST-CADSEG <> "00"
              MOVE "ERRO ABERTURA CADSEG: "  TO CAP012-MENSAGEM-ERRO
              MOVE ST-CADSEG TO CAP012-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CAP012-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CADSEG"            to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           close      cadseg
           open input cadseg

           IF ERRO-W = ZEROS
                MOVE 1 TO CAP012-ORDER
                PERFORM ACHAR-CODIGO
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CAP012-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CAP012-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CAP012-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CAP012-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CAP012-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CAP012-PRINTER-FLG-TRUE
                   copy impressora.chama.
                   if lnk-mapeamento <> spaces
                      PERFORM IMPRIME-RELATORIO
                   end-if
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN CAP012-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CAP012-CARREGA-LIST-BOX-TRUE
                   MOVE CAP012-LINDET(1: 4) TO CAP012-CODIGO
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
           MOVE CAP012-CODIGO       TO CADSEG-CODIGO
           READ CADSEG INVALID KEY
                INITIALIZE REG-CADSEG
                MOVE 1              TO GRAVA-W.

           MOVE CADSEG-NOME         TO CAP012-NOME.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CADSEG
           MOVE CAP012-ORDER TO ORDEM-W
           INITIALIZE CAP012-DATA-BLOCK
           MOVE ORDEM-W TO CAP012-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           close      cadseg
           open i-o   cadseg
           DELETE CADSEG.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
           close      cadseg
           open input cadseg.
       SALVAR-DADOS SECTION.
           close cadseg
           open i-o cadseg
           MOVE CAP012-CODIGO       TO CADSEG-CODIGO
           MOVE CAP012-NOME         TO CADSEG-NOME
           IF GRAVA-W = 1
              WRITE REG-CADSEG INVALID KEY
                   PERFORM ERRO-GRAVACAO
              END-WRITE
           ELSE
              REWRITE REG-CADSEG INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
           close      cadseg
           open input cadseg.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CAP012-MENSAGEM-ERRO
           MOVE ST-CADSEG       TO CAP012-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF CAP012-ORDER = ZEROS
              MOVE SPACES TO CADSEG-NOME
              START CADSEG KEY IS NOT < CADSEG-NOME
                    INVALID KEY MOVE "10" TO ST-CADSEG
           ELSE
             MOVE ZEROS TO CADSEG-CODIGO
               START CADSEG KEY IS NOT < CADSEG-CODIGO
                 INVALID KEY MOVE "10" TO ST-CADSEG.
           MOVE SPACES TO CAP012-LINDET.
           MOVE ZEROS TO CAP012-CONT.
           PERFORM UNTIL ST-CADSEG = "10"
              READ CADSEG NEXT RECORD AT END MOVE "10" TO ST-CADSEG
              NOT AT END
                ADD 1 TO CAP012-CONT
      *         MOVE SPACES TO CAP012-LINDET
                MOVE CADSEG-CODIGO     TO CAP012-LINDET(01: 06)
                MOVE CADSEG-NOME       TO CAP012-LINDET(08: 40)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CAP012-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CADSEG" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           IF CAP012-ORDER = 1
              MOVE ZEROS TO CADSEG-CODIGO
              START CADSEG KEY IS NOT < CADSEG-CODIGO INVALID KEY
                           MOVE "10" TO ST-CADSEG
           ELSE MOVE SPACES TO CADSEG-NOME
                START CADSEG KEY IS NOT < CADSEG-NOME INVALID KEY
                           MOVE "10" TO ST-CADSEG.
           MOVE ZEROS TO LIN
           PERFORM CABECALHO
           PERFORM UNTIL ST-CADSEG = "10"
             READ CADSEG NEXT RECORD AT END
                  MOVE "10" TO ST-CADSEG
             NOT AT END
                  MOVE SPACES TO LINDET-REL
                  MOVE CADSEG-CODIGO TO LINDET-REL(01: 06)
                  MOVE CADSEG-NOME   TO LINDET-REL(08: 40)
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
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO CADSEG-CODIGO ULT-CODIGO
           START CADSEG KEY IS NOT < CADSEG-CODIGO INVALID KEY
                 MOVE "10" TO ST-CADSEG
           END-START
           PERFORM UNTIL ST-CADSEG = "10"
                 READ CADSEG NEXT RECORD AT END
                      MOVE "10" TO ST-CADSEG
                 NOT AT END
                      MOVE CADSEG-CODIGO TO ULT-CODIGO
                 END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CAP012-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CAP012-CODIGO
           MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CAP012-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CADSEG"            to logacess-programa
           move "FECHADO"           to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess
           CLOSE CADSEG.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

