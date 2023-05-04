       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRP012.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 25/04/2000
      *DESCRIÇÃO: Cadastro de OBSERVACOES DE CONVITES
      *           P/ PLANEJAMENTO DE REPORTAGEM
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       PRINTER IS LPRINTER
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PRPX012.
           COPY LOGX002.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY PRPW012.
       COPY LOGW002.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "PRP012.CPB".
           COPY "PRP012.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-PRD012             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
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
           "OBSERVACAO SOBRE CONVITE - P/ PLANEJAMENTO".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

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
           MOVE "PRD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PRD012.
           MOVE "LOG002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
           ARQUIVO-LOGACESS
           OPEN I-O PRD012 LOG002

           MOVE 1 TO GRAVA-W.
           IF ST-LOG002 = "35"
              CLOSE LOG002      OPEN OUTPUT LOG002
              CLOSE LOG002      OPEN I-O LOG002
           END-IF.
           IF ST-PRD012 = "35"
              CLOSE PRD012      OPEN OUTPUT PRD012
              CLOSE PRD012      OPEN I-O PRD012
           END-IF.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD012 <> "00"
              MOVE "ERRO ABERTURA PRD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           MOVE 1 TO COD-USUARIO-W
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           close prd012 log002
           open input prd012

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "PRP012"            to logacess-programa
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
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
               WHEN GS-LER-PRD012-TRUE
                   PERFORM LER-PRD012
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-VERIFICA-CONTRATO-TRUE
                    PERFORM VERIFICA-CONTRATO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-PRD012 SECTION.
           MOVE 0              TO GRAVA-W
           MOVE GS-CONTRATO    TO CONTRATO-PR12
           MOVE GS-SEQUENCIA   TO SEQUENCIA-PR12
           READ PRD012 INVALID KEY
               MOVE SPACES TO OBSERVACAO-PR12
               MOVE 1 TO GRAVA-W.
           MOVE OBSERVACAO-PR12    TO GS-OBS.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       VERIFICA-CONTRATO SECTION.
           MOVE 0 TO GRAVA-W.
           MOVE GS-CONTRATO TO CONTRATO-PR12.
           MOVE ALL "9" TO SEQUENCIA-PR12
           START PRD012 KEY IS LESS THAN CHAVE-PR12
           READ PRD012 NEXT RECORD
           IF ST-PRD012 <> "00" AND "02"
              MOVE 1 TO GS-SEQUENCIA
           ELSE
              IF GS-CONTRATO = CONTRATO-PR12
                 MOVE SEQUENCIA-PR12 TO GS-SEQUENCIA
                 ADD 1 TO GS-SEQUENCIA
              ELSE
                 MOVE 1 TO GS-SEQUENCIA.
           MOVE SPACES TO GS-OBS
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-PRD012
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           CLOSE PRD012
           OPEN I-O PRD012 LOG002
           DELETE PRD012 NOT INVALID KEY
               MOVE USUARIO-W   TO LOG2-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG2-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG2-HORAS
               MOVE "E"         TO LOG2-OPERACAO
               MOVE "PRD012"    TO LOG2-ARQUIVO
               MOVE "PRP012"    TO LOG2-PROGRAMA
               MOVE REG-PRD012  TO LOG2-REGISTRO
               WRITE REG-LOG002
               END-WRITE.
           CLOSE PRD012 LOG002
           OPEN INPUT LOG002
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           CLOSE PRD012
           OPEN I-O PRD012 LOG002
           MOVE GS-CONTRATO     TO CONTRATO-PR12
           MOVE GS-OBS          TO OBSERVACAO-PR12.
           IF GRAVA-W = 1
              WRITE REG-PRD012 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG2-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "I"         TO LOG2-OPERACAO
                   MOVE "PRD012"    TO LOG2-ARQUIVO
                   MOVE "PRP012"    TO LOG2-PROGRAMA
                   MOVE REG-PRD012  TO LOG2-REGISTRO
                   WRITE REG-LOG002
                   END-WRITE
              END-WRITE
           ELSE
              REWRITE REG-PRD012 INVALID KEY
                  PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                  MOVE USUARIO-W   TO LOG2-USUARIO
                  MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                  MOVE WS-DATA-CPU TO LOG2-DATA
                  ACCEPT WS-HORA-SYS FROM TIME
                  MOVE WS-HORA-SYS TO LOG2-HORAS
                  MOVE "A"         TO LOG2-OPERACAO
                  MOVE "PRD012"    TO LOG2-ARQUIVO
                  MOVE "PRP012"    TO LOG2-PROGRAMA
                  MOVE REG-PRD012  TO LOG2-REGISTRO
                  WRITE REG-LOG002
                  END-WRITE.
           CLOSE PRD012 LOG002
           OPEN INPUT PRD012.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-PRD012       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "PRP012" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           PERFORM CABECALHO.
           MOVE SPACES          TO LINDET-REL
           MOVE "CONTRATO: "    TO LINDET-REL
           MOVE GS-CONTRATO     TO LINDET-REL(11: 4)
           WRITE REG-RELAT FROM LINDET AFTER 3
           MOVE SPACES          TO LINDET-REL
           MOVE GS-OBS          TO LINDET-REL
           WRITE REG-RELAT FROM LINDET AFTER 2.
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
           move "PRP012"            to logacess-programa
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

           CLOSE PRD012
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
