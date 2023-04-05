       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALSEQBRR.
       AUTHOR. ALFREDO SAVIOLLI NETO.
      *PROGRAMA: ALTERA A SEQUENCIA DO BANCO DO BRASIL - RDA
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SEQBRAS ASSIGN TO PATH-SEQBRA
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-SEQ
                  RECORD KEY IS CONT-SEQUENCIA.


       DATA DIVISION.
       FILE SECTION.

       FD  SEQBRAS.
       01  REG-SEQBRAS.
           05  CONT-SEQUENCIA  PIC 9.
           05  SEQUENCIA       PIC 9(10).


       WORKING-STORAGE SECTION.
           COPY "ALSEQBRR.CPB".
           COPY "ALSEQBRR.CPY".
           COPY "DS-CNTRL.MF".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-SEQ                PIC XX       VALUE SPACES.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
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
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(70)   VALUE
           "RELACAO DE CADASTRO DE USUARIOS".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(10)   VALUE "COD".
           05  FILLER              PIC X(35)   VALUE "NOME".
           05  FILLER              PIC X(15)   VALUE "REDUZIDO".
           05  FILLER              PIC X(10)   VALUE "IMPRESSORA".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CAP002-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           INITIALIZE CAP002-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE ZEROS TO ERRO-W.
           MOVE CAP002-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CAP002-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "SEQBRA2"          TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-SEQBRA.
           OPEN I-O SEQBRAS.
           IF ST-SEQ = "35"
              CLOSE SEQBRAS
              OPEN OUTPUT SEQBRAS
              CLOSE SEQBRAS
              OPEN I-O SEQBRAS
              MOVE 1 TO CONT-SEQUENCIA
              MOVE ZEROS TO SEQUENCIA
              WRITE REG-SEQBRAS
              CLOSE SEQBRAS   OPEN I-O SEQBRAS.

           IF ST-SEQ <> "00"
              MOVE "ERRO ABERTURA SEQBRAS "  TO CAP002-MENSAGEM-ERRO
              MOVE ST-SEQ  TO CAP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CAP002-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CAP002-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
               WHEN CAP002-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   PERFORM SET-UP-FOR-REFRESH-SCREEN
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

       CARREGAR-DADOS SECTION.
           MOVE 1                TO CONT-SEQUENCIA
           READ SEQBRAS INVALID KEY
               MOVE ZEROS        TO SEQUENCIA.

           MOVE SEQUENCIA        TO CAP002-SEQUENCIA.



       SALVAR-DADOS SECTION.
           MOVE 1                TO CONT-SEQUENCIA
           READ SEQBRAS INVALID KEY
                MOVE CAP002-SEQUENCIA TO SEQUENCIA
                WRITE REG-SEQBRAS
                END-WRITE
           NOT INVALID KEY
                MOVE CAP002-SEQUENCIA TO SEQUENCIA
                REWRITE REG-SEQBRAS
                END-REWRITE
           END-READ.

       CLEAR-FLAGS SECTION.
           INITIALIZE CAP002-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "ALSEQBRR" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CAP002-DATA-BLOCK.
               IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE SEQBRAS
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.


