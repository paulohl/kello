       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAP014.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 08/02/1999
      *DESCRIÇÃO: Cadastro de Departamento
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       PRINTER IS LPRINTER
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX014.
           COPY LOGX001.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW014.
       COPY LOGW001.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CAP014.CPB".
           COPY "CAP014.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD014             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(2)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    do depto utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
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
           05  lin                   pic 9(02) value zeros.
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
           "RELACAO DE DEPARTAMENTO   ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "COD.      DEPARTAMENTO".

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


       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
      *PROCEDURE DIVISION USING POP-UP.
       PROCEDURE DIVISION.

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
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD014" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD014.
           MOVE "LOG001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
           ARQUIVO-LOGACESS.
           OPEN I-O CAD014 LOG001
           MOVE 1 TO GRAVA-W.
           IF ST-CAD014 = "35"
              CLOSE CAD014      OPEN OUTPUT CAD014
              CLOSE CAD014      OPEN I-O CAD014
           END-IF.
           IF ST-LOG001 = "35"
              CLOSE LOG001      OPEN OUTPUT LOG001
              CLOSE LOG001      OPEN I-O LOG001
           END-IF.

           IF ST-CAD014 <> "00"
              MOVE "ERRO ABERTURA CAD014: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD014 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE CAD014 LOG001
           OPEN INPUT CAD014

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CAP014"            to logacess-programa
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
           MOVE GS-CODIGO       TO CODIGO-CA14.
           READ CAD014 INVALID KEY INITIALIZE REG-CAD014
                                   MOVE 1 TO GRAVA-W.
           MOVE DEPARTAMENTO-CA14          TO GS-NOME.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CAD014
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           CLOSE CAD014
           OPEN I-O CAD014 LOG001
           DELETE CAD014 NOT INVALID KEY
               MOVE USUARIO-W   TO LOG1-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG1-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG1-HORAS
               MOVE "E"         TO LOG1-OPERACAO
               MOVE "CAD014"    TO LOG1-ARQUIVO
               MOVE "CAP014"    TO LOG1-PROGRAMA
               MOVE REG-CAD014  TO LOG1-REGISTRO
               WRITE REG-LOG001
               END-WRITE.

           CLOSE CAD014 LOG001
           OPEN INPUT CAD014

           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           CLOSE CAD014
           OPEN I-O CAD014 LOG001
           MOVE GS-CODIGO       TO CODIGO-CA14.
           MOVE GS-NOME TO DEPARTAMENTO-CA14.
           IF GRAVA-W = 1
              WRITE REG-CAD014 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG1-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG1-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG1-HORAS
                   MOVE "I"         TO LOG1-OPERACAO
                   MOVE "CAD014"    TO LOG1-ARQUIVO
                   MOVE "CAP014"    TO LOG1-PROGRAMA
                   MOVE REG-CAD014  TO LOG1-REGISTRO
                   WRITE REG-LOG001
                   END-WRITE
                END-WRITE
           ELSE
              REWRITE REG-CAD014 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG1-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG1-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG1-HORAS
                   MOVE "A"         TO LOG1-OPERACAO
                   MOVE "CAD014"    TO LOG1-ARQUIVO
                   MOVE "CAP014"    TO LOG1-PROGRAMA
                   MOVE REG-CAD014  TO LOG1-REGISTRO
                   WRITE REG-LOG001
                   END-WRITE
                   SUBTRACT 1 FROM ULT-CODIGO
           END-IF
           CLOSE CAD014 LOG001
           OPEN INPUT CAD014.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-CAD014       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF GS-ORDER = ZEROS
              MOVE SPACES TO DEPARTAMENTO-CA14
              START CAD014 KEY IS NOT < DEPARTAMENTO-CA14
                    INVALID KEY MOVE "10" TO ST-CAD014
           ELSE
             MOVE ZEROS TO CODIGO-CA14
               START CAD014 KEY IS NOT < CODIGO-CA14
                 INVALID KEY MOVE "10" TO ST-CAD014.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-CAD014 = "10"
              READ CAD014 NEXT RECORD AT END MOVE "10" TO ST-CAD014
              NOT AT END
                ADD 1 TO GS-CONT
      *         MOVE SPACES TO GS-LINDET
                MOVE CODIGO-CA14        TO GS-LINDET(01: 06)
                MOVE DEPARTAMENTO-CA14          TO GS-LINDET(07: 15)
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
           MOVE "CAP014" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           IF GS-ORDER = 1
              MOVE ZEROS TO CODIGO-CA14
              START CAD014 KEY IS NOT < CODIGO-CA14 INVALID KEY
                           MOVE "10" TO ST-CAD014
           ELSE MOVE SPACES TO DEPARTAMENTO-CA14
                START CAD014 KEY IS NOT < DEPARTAMENTO-CA14 INVALID KEY
                           MOVE "10" TO ST-CAD014.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CAD014 = "10"
             READ CAD014 NEXT RECORD AT END MOVE "10" TO ST-CAD014
              NOT AT END
               MOVE SPACES TO LINDET-REL
                MOVE CODIGO-CA14            TO LINDET-REL(01: 08)
                MOVE DEPARTAMENTO-CA14      TO LINDET-REL(09: 15)
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
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO CODIGO-CA14 ULT-CODIGO
           START CAD014 KEY IS NOT < CODIGO-CA14 INVALID KEY
                 MOVE "10" TO ST-CAD014
           END-START
           PERFORM UNTIL ST-CAD014 = "10"
              READ CAD014 NEXT RECORD AT END MOVE "10" TO ST-CAD014
                NOT AT END
                 MOVE CODIGO-CA14 TO ULT-CODIGO
              END-READ
           END-PERFORM.
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
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CAP014"            to logacess-programa
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
           CLOSE CAD014
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

