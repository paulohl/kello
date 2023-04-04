       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONPRO.
      * AUTOR: ALFREDO SAVIOLLI NETO
      * DATA: 02-06-2011
      * FUNCAO: CONSULTA POP-UP PRODUTOS
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CADPRO.SEL.

           COPY CADMOD.SEL.

       DATA DIVISION.
       FILE SECTION.
           COPY CADPRO.FD.

           COPY CADMOD.FD.

       WORKING-STORAGE SECTION.
           COPY "CONPRO.CPB".
           COPY "CONPRO.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CADPRO             PIC XX       VALUE SPACES.
           05  ST-CADMOD             PIC XX       VALUE SPACES.
           05  W-ERRO                PIC 9(01)    VALUE ZEROS.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(2)   VALUE SPACES.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
       01  LNK-PRODUTO            PIC 9(04).

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W LNK-PRODUTO.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CAP018T-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE 0      TO W-ERRO
           INITIALIZE CAP018T-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CAP018T-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CAP018T-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CADPRO"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CADPRO.
           MOVE "CADMOD"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CADMOD.
           OPEN I-O   CADPRO CADMOD
           CLOSE      CADPRO CADMOD
           OPEN INPUT CADPRO CADMOD
           IF ST-CADPRO <> "00"
              MOVE "ERRO ABERTURA CADPRO: "  TO CAP018T-MENSAGEM-ERRO
              MOVE ST-CADPRO TO CAP018T-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
              MOVE 1 TO W-ERRO.

           IF ST-CADMOD <> "00"
              MOVE "ERRO ABERTURA CADMOD: "  TO CAP018T-MENSAGEM-ERRO
              MOVE ST-CADMOD TO CAP018T-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
              MOVE 1 TO W-ERRO.

           IF W-ERRO = 0
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CAP018T-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CAP018T-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN CAP018T-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-CADPRO
           START CADPRO KEY IS NOT < CADPRO-CH-NOME INVALID KEY
                 MOVE "10"                   TO ST-CADPRO.

           MOVE SPACES TO CAP018T-LINDET.
           MOVE ZEROS TO CAP018T-CONT.
           PERFORM UNTIL ST-CADPRO = "10"
                 READ CADPRO NEXT RECORD AT END
                      MOVE "10" TO ST-CADPRO
                 NOT AT END
                      ADD 1                  TO CAP018T-CONT
                      MOVE CADPRO-NOME       TO CAP018T-LINDET(01:40)
                      MOVE CADPRO-MODELO     TO CADMOD-CODIGO
                      READ CADMOD INVALID KEY
                           MOVE "*********"  TO CADMOD-NOME
                      END-READ
                      MOVE CADMOD-NOME       TO CAP018T-LINDET(42:30)

                      MOVE CADPRO-CODIGO     TO CAP018T-LINDET(73:4)
                      MOVE CADPRO-MODELO     TO CAP018T-LINDET(77:4)

                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           MOVE FUNCTION NUMVAL(CAP018T-LINDET(73:4)) TO LNK-PRODUTO.
       CLEAR-FLAGS SECTION.
           INITIALIZE CAP018T-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CONPRO" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CAP018T-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CADPRO CADMOD.
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
