       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAP010.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 29/06/1998
      *DESCRIÇÃO: Cadastro de cidade
      *campos: nome-cid = nome reduzido p/ relatórios
      *        Regiao-cid = Cidades são divididas por região, que por
      *                     sua vez existe um cadastro de regiao(cad012)
      *        Distancia-cid = kilometragem entre Sta Fé e a cidade
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX012.
           COPY LOGX002.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW012.
       COPY LOGW002.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY "CAP010.CPB".
           COPY "CAP010.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  CODIGO-E              PIC Z(8).
           05  ULT-CODIGO            PIC 9(4)     VALUE ZEROS.
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
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

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



       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "RELACAO DE CIDADE         ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "COD. NOME-COMPLETO            NOME-REDUZIDO UF  DDD      CEP
      -    " DIST.  POPULAC RE".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

           COPY IMPRESSORA.

      *LINKAGE SECTION.
      *77  POP-UP                  PIC X(30).
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
           MOVE GS-VERSION-NO  TO DS-VERSION-NO


           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD010"   TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"   TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "LOG002"   TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN I-O   CAD010 LOG002 CAD012
           CLOSE      CAD010 LOG002 CAD012
           OPEN I-O   CAD010 LOG002
           OPEN INPUT CAD012

           MOVE 1 TO GRAVA-W.
           IF ST-LOG002 = "35"
              CLOSE LOG002      OPEN OUTPUT LOG002
              CLOSE LOG002      OPEN I-O LOG002
           END-IF.
           IF ST-CAD010 = "35"
              CLOSE CAD010      OPEN OUTPUT CAD010
              CLOSE CAD010      OPEN I-O CAD010
           END-IF.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           close      cad010 log002
           open input cad010

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CAP010"            to logacess-programa
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
                   MOVE GS-LINDET(1: 4) TO GS-CIDADE
                   PERFORM CARREGAR-DADOS
               WHEN GS-EXIBE-REGIAO-TRUE
                   MOVE GS-REGIAO TO CODIGO-REG
                   PERFORM EXIBE-REGIAO
               WHEN GS-POP-UP-TRUE
                   PERFORM POPUP-REGIAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       POPUP-REGIAO SECTION.
           CALL "CAP012T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-REGIAO.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-REGIAO.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE GS-CIDADE       TO CIDADE.
           READ CAD010 INVALID KEY INITIALIZE REG-CAD010
                                   MOVE 1 TO GRAVA-W.
           MOVE NOME-COMPL-CID    TO GS-NOME-COMPL.
           MOVE NOME-CID          TO GS-NOME-REDUZ.
           MOVE UF-CID            TO GS-UF.
           MOVE DDD-CID           TO GS-DDD.
           MOVE CEP-CID           TO GS-CEP.
           MOVE DISTANCIA-CID     TO GS-DISTANCIA.
           MOVE POPULACAO-CID     TO GS-POPULACAO.
           MOVE REGIAO-CID        TO GS-REGIAO CODIGO-REG.
           PERFORM EXIBE-REGIAO.

       EXIBE-REGIAO SECTION.
           READ CAD012 INVALID KEY
                MOVE SPACES TO NOME-REG.
           MOVE NOME-REG          TO GS-NOME-REGIAO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CAD010
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       EXCLUI-RECORD SECTION.
           close cad010
           open i-o cad010 log002
           DELETE CAD010 NOT INVALID KEY
               MOVE USUARIO-W   TO LOG2-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG2-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG2-HORAS
               MOVE "E"         TO LOG2-OPERACAO
               MOVE "CAD010"    TO LOG2-ARQUIVO
               MOVE "CAP010"    TO LOG2-PROGRAMA
               MOVE REG-CAD010  TO LOG2-REGISTRO
               WRITE REG-LOG002
               END-WRITE.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W

           close cad010 log002
           open input cad010.

       SALVAR-DADOS SECTION.
           close cad010
           open i-o cad010 log002
           MOVE GS-CIDADE       TO CIDADE.
           MOVE GS-NOME-REDUZ   TO NOME-CID.
           MOVE GS-NOME-COMPL   TO NOME-COMPL-CID.
           MOVE GS-UF           TO UF-CID.
           MOVE GS-DDD          TO DDD-CID.
           MOVE GS-CEP          TO CEP-CID.
           MOVE GS-DISTANCIA    TO DISTANCIA-CID.
           MOVE GS-POPULACAO    TO POPULACAO-CID.
           MOVE GS-REGIAO       TO REGIAO-CID.
           IF GRAVA-W = 1
              WRITE REG-CAD010 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG2-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "I"         TO LOG2-OPERACAO
                   MOVE "CAD010"    TO LOG2-ARQUIVO
                   MOVE "CAP010"    TO LOG2-PROGRAMA
                   MOVE REG-CAD010  TO LOG2-REGISTRO
                   WRITE REG-LOG002
                   END-WRITE
              END-WRITE
           ELSE
              REWRITE REG-CAD010 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG2-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "A"         TO LOG2-OPERACAO
                   MOVE "CAD010"    TO LOG2-ARQUIVO
                   MOVE "CAP010"    TO LOG2-PROGRAMA
                   MOVE REG-CAD010  TO LOG2-REGISTRO
                   WRITE REG-LOG002
                   END-WRITE
                   SUBTRACT 1 FROM ULT-CODIGO.

           close cad010 log002
           open input cad010.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-CAD010       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.

       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF GS-ORDER = ZEROS
              MOVE SPACES TO NOME-COMPL-CID
              START CAD010 KEY IS NOT < NOME-COMPL-CID
                    INVALID KEY MOVE "10" TO ST-CAD010
           ELSE
             MOVE ZEROS TO CIDADE
               START CAD010 KEY IS NOT < CIDADE
                 INVALID KEY MOVE "10" TO ST-CAD010.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-CAD010 = "10"
              READ CAD010 NEXT RECORD AT END MOVE "10" TO ST-CAD010
              NOT AT END
      *         ADD 1 TO GS-CONT
                MOVE SPACES TO GS-LINDET
                MOVE CIDADE            TO GS-LINDET(01: 05)
                MOVE NOME-COMPL-CID    TO GS-LINDET(06: 31)
                MOVE NOME-CID          TO GS-LINDET(37: 14)
                MOVE UF-CID            TO GS-LINDET(51: 03)
                MOVE DDD-CID           TO GS-LINDET(54: 05)
                MOVE CEP-CID           TO GS-LINDET(59: 09)
                MOVE DISTANCIA-CID     TO GS-LINDET(68: 06)
                MOVE POPULACAO-CID     TO GS-LINDET(74: 09)
                MOVE REGIAO-CID        TO GS-LINDET(83: 02)
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
           MOVE "CAP010" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           IF GS-ORDER = 1
              MOVE ZEROS TO CIDADE
              START CAD010 KEY IS NOT < CIDADE INVALID KEY
                           MOVE "10" TO ST-CAD010
           ELSE MOVE SPACES TO NOME-COMPL-CID
                START CAD010 KEY IS NOT < NOME-COMPL-CID INVALID KEY
                           MOVE "10" TO ST-CAD010.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CAD010 = "10"
             READ CAD010 NEXT RECORD AT END MOVE "10" TO ST-CAD010
              NOT AT END
               MOVE SPACES TO LINDET-REL
                MOVE CIDADE                TO LINDET-REL(01: 05)
                MOVE NOME-COMPL-CID(1: 25) TO LINDET-REL(06: 26)
                MOVE NOME-CID              TO LINDET-REL(32: 14)
                MOVE UF-CID                TO LINDET-REL(46: 03)
                MOVE DDD-CID               TO LINDET-REL(49: 05)
                MOVE CEP-CID               TO LINDET-REL(54: 09)
                MOVE DISTANCIA-CID         TO LINDET-REL(63: 06)
                MOVE POPULACAO-CID         TO CODIGO-E
                MOVE CODIGO-E              TO LINDET-REL(69: 09)
                MOVE REGIAO-CID            TO LINDET-REL(78: 02)
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
           MOVE 4 TO LIN.
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO CIDADE ULT-CODIGO
           START CAD010 KEY IS NOT < CIDADE INVALID KEY
                 MOVE "10" TO ST-CAD010
           END-START
           PERFORM UNTIL ST-CAD010 = "10"
              READ CAD010 NEXT RECORD AT END MOVE "10" TO ST-CAD010
                NOT AT END
                 MOVE CIDADE TO ULT-CODIGO
              END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO GS-CIDADE.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO GS-CIDADE
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
           move "CAP010"            to logacess-programa
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
           CLOSE CAD010 CAD012.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

