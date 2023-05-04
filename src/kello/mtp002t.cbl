       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP002T.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
       DATE-WRITTEN. 01/03/2001.
      * FUNCAO: CONSULTA POP-UP DE LOCAL ARMAZENAMENTO DE ALBUM
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY MTPX002.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-WK
                  ALTERNATE RECORD KEY IS NOME-WK WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
       COPY MTPW002.
       FD  WORK.
       01  REG-WORK.
           05  CODIGO-WK              PIC 9(2).
           05  NOME-WK                PIC X(10).
       WORKING-STORAGE SECTION.
           COPY "MTP002T.CPB".
           COPY "MTP002T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD002             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)   VALUE ZEROS.
           05  CODIGO-TRANSF         PIC 9(2)   VALUE ZEROS.
           05  CODIGO-W              PIC X(4)   VALUE SPACES.

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
           MOVE "00" TO STRING-1(16: 2).
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.     CLOSE WORK.  OPEN I-O WORK.
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "MTD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD002.
           OPEN INPUT MTD002
           IF ST-MTD002 <> "00"
              MOVE "ERRO ABERTURA MTD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD002 TO GS-MENSAGEM-ERRO(23: 02)
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
           PERFORM GRAVA-WORK.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK
                    INVALID KEY MOVE "10" TO ST-WORK.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                ADD 1                  TO GS-CONT
                MOVE NOME-WK           TO GS-LINDET(01: 15)
                MOVE CODIGO-WK         TO GS-LINDET(16: 02)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       GRAVA-WORK SECTION.
           MOVE SPACES                 TO NOME-MT02
           START MTD002 KEY IS NOT < NOME-MT02 INVALID KEY
                 MOVE "10" TO ST-MTD002.
           PERFORM UNTIL ST-MTD002 = "10"
             READ MTD002 NEXT RECORD AT END MOVE "10" TO ST-MTD002
               NOT AT END
                    MOVE CODIGO-MT02    TO CODIGO-WK
                    MOVE NOME-MT02      TO NOME-WK
                    WRITE REG-WORK
             END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(1: 25) TO STRING-1.
           MOVE STRING-1(16: 2) TO CODIGO-W
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(16: 2).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP002T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE MTD002 WORK.
           DELETE FILE WORK.
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
