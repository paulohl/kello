       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP003T.
      * FUNCTION - POP UP - CADASTRO DE BANCOS
      * AUTHOR: MARELI AMANCIO VOLPATO
      * DATE: 03/11/1998
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CBPX003.

       DATA DIVISION.
       FILE SECTION.
           COPY CBPW003.

       WORKING-STORAGE SECTION.
           COPY "CBP003T.CPB".
           COPY "CBP003T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CBD003             PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(6)   VALUE SPACES.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
           COPY "PARAMETR".

       01  STRING-1               PIC X(65) VALUE SPACES.
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE "00" TO STRING-1(40: 2).
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CBD003"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-CBD003.
           OPEN INPUT CBD003.
           IF ST-CBD003 <> "00"
              MOVE "ERRO ABERTURA CBD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD003 TO GS-MENSAGEM-ERRO(23: 02)
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
           MOVE SPACES TO NOME-SIT-CB03.
           START CBD003 KEY IS NOT < NOME-SIT-CB03
                    INVALID KEY MOVE "10" TO ST-CBD003.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-CBD003 = "10"
              READ CBD003 NEXT RECORD AT END MOVE "10" TO ST-CBD003
              NOT AT END
                ADD 1 TO GS-CONT
                MOVE NOME-SIT-CB03        TO GS-LINDET(01: 31)
                MOVE SITUACAO-CB03        TO GS-LINDET(40: 02)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(1: 60)      TO STRING-1.
           MOVE STRING-1(40: 2)       TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(40: 2).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CBP003T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CBD003.
           move ds-quit-set to ds-control.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
