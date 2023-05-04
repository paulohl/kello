       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP019T2.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 07/10/2004.
      * FUNCAO: CONSULTA POP-UP DE FICHA DE IDENTIFICACAO FORMANDO
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY MTPX019.

       DATA DIVISION.
       FILE SECTION.
       COPY MTPW019.

       WORKING-STORAGE SECTION.
           COPY "MTP019T2.CPB".
           COPY "MTP019T2.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD019             PIC XX       VALUE SPACES.
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
           05  CONTRATO-TRANSF       PIC 9(4)   VALUE ZEROS.
           05  CONTRATO-W            PIC X(4)   VALUE SPACES.
           05  ALBUM-W               PIC X(4)   VALUE SPACES.
           05  LETRA                 PIC X      VALUE SPACES.
           05  LETRA1                PIC X      VALUE SPACES.

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
           MOVE "0000" TO STRING-1(40: 4).
           MOVE "0000" TO STRING-1(45: 4).
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           ACCEPT VARIA-W FROM TIME.
           MOVE EMPRESA-W TO EMP-REC
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           OPEN INPUT MTD019
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE
              PERFORM LOAD-SCREENSET.

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
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM GRAVA-WORK.

       GRAVA-WORK SECTION.
           INITIALIZE REG-MTD019

           MOVE GS-LINHA-AUX      TO NOME-FORM-MT19
           MOVE GS-LINHA-AUX(1:1) TO LETRA.


           START MTD019 KEY IS NOT LESS ALT2-MT19 INVALID KEY
               MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
             READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
               NOT AT END

                   MOVE NOME-FORM-MT19(1: 1)   TO LETRA1

                   IF LETRA1 <> LETRA CONTINUE
                   ELSE
                        ADD 1                  TO GS-CONT
                        MOVE NOME-FORM-MT19    TO GS-LINDET(01: 30)
                        MOVE CONTRATO-MT19     TO GS-LINDET(40: 04)
                        MOVE SEQ-MT19          TO GS-LINDET(45: 04)

                        MOVE "INSERE-LIST" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.

       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(1: 50) TO STRING-1.
           MOVE STRING-1(40: 4) TO CONTRATO-W
           MOVE STRING-1(45: 4) TO ALBUM-W.
           IF CONTRATO-W = SPACES MOVE ZEROS TO STRING-1(40: 4).
           IF ALBUM-W = SPACES MOVE ZEROS TO STRING-1(45: 4).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP019T2" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE MTD019
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
