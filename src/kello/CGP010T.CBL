       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGP010T.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX010.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       WORKING-STORAGE SECTION.
           COPY "CGP010T.CPB".
           COPY "CGP010T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(8)   VALUE SPACES.
           05  CLASSIF-W             PIC X      VALUE SPACES.

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
       01  STRING-1               PIC X(65) VALUE SPACES.

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE "00000000" TO STRING-1(33: 8).
           MOVE "0"      TO STRING-1(42: 1)
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           OPEN INPUT CGD010.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN GS-ITEM-SELECIONADO-TRUE
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
           MOVE ZEROS TO GS-CONT.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO COMPRADOR-CG10.
           START CGD010 KEY IS NOT < COMPRADOR-CG10
                    INVALID KEY MOVE "10" TO ST-CGD010.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-CGD010 = "10"
              READ CGD010 NEXT RECORD AT END MOVE "10" TO ST-CGD010
              NOT AT END
               MOVE COMPRADOR-CG10(1: I) TO INICIAL-A-COMPARAR
               IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-CGD010
               ELSE
                ADD 1 TO GS-CONT
                MOVE COMPRADOR-CG10    TO GS-LINDET(01: 30)
                MOVE CODIGO-CG10       TO GS-LINDET(33: 08)
                MOVE CLASSIF-CG10      TO GS-LINDET(42: 1)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-LINDET(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE GS-LINDET(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.

       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(1: 45) TO STRING-1.
           MOVE STRING-1(33: 8) TO CODIGO-W.
           MOVE STRING-1(42: 1) TO CLASSIF-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(33: 8).
           IF CLASSIF-W = SPACES MOVE 0 TO STRING-1(42: 1).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CGP010T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD010.
           move ds-quit-set to ds-control.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
