       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAP002T.
      * AUTORA: MARELI AMÂNCIO VOLPATO
      * DATA: 13/11/1998
      * FUNCAO: CONSULTA POP-UP USUÁRIOS
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.


           SELECT ACAD002 ASSIGN TO "\PROGRAMA\KELLO\PADRAO\ACAD002"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-ACAD002
                  RECORD KEY IS CODIGO-ACA002
                  ALTERNATE RECORD KEY IS NOME-ACA002.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.


      * Cadastro de USUARIO
       FD  ACAD002.
       01  REG-ACAD002.
           05  CODIGO-ACA002        PIC 9(03).
           05  SENHA-ACA002         PIC 9(04) COMP-3.
           05  NOME-ACA002          PIC X(30).
           05  NOME-REDUZ-ACA002    PIC X(05).
           05  IMPRESSORA-ACA002    PIC 9(02).

       WORKING-STORAGE SECTION.
           COPY "CAP002T.CPB".
           COPY "CAP002T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-ACAD002            PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(3)   VALUE SPACES.

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
           MOVE "000" TO STRING-1(38: 3).
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           OPEN INPUT CAD002
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
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

           OPEN OUTPUT ACAD002
           CLOSE       ACAD002
           OPEN I-O    ACAD002

           INITIALIZE REG-CAD002
           START CAD002 KEY IS NOT LESS CODIGO-CA002 INVALID KEY
                MOVE "10" TO ST-CAD002.

           PERFORM UNTIL ST-CAD002 = "10"
                READ CAD002 NEXT AT END
                     MOVE "10" TO ST-CAD002
                NOT AT END
                     MOVE REG-CAD002 TO REG-ACAD002
                     WRITE REG-ACAD002
                END-READ
           END-PERFORM

           CLOSE       ACAD002
           OPEN INPUT  ACAD002

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-ACAD002
           MOVE GS-LINDET TO NOME-ACA002
           START ACAD002 KEY IS NOT < NOME-ACA002 INVALID KEY
                 MOVE "10" TO ST-ACAD002.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-ACAD002 = "10"
              READ ACAD002 NEXT RECORD AT END
                   MOVE "10" TO ST-ACAD002
              NOT AT END
                   ADD 1                      TO GS-CONT
                   MOVE NOME-ACA002           TO GS-LINDET(1: 30)
                   MOVE NOME-REDUZ-ACA002     TO GS-LINDET(32: 5)
                   MOVE CODIGO-ACA002         TO GS-LINDET(38: 03)
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM

           CLOSE ACAD002.
       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(1: 40)           TO STRING-1.
           MOVE STRING-1(38: 3)            TO CODIGO-W.
           IF CODIGO-W = SPACES MOVE ZEROS TO STRING-1(38: 3).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CAP002T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD002
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

