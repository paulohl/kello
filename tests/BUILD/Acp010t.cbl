       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACP010T.
      * AUTORA: MARELI AMÂNCIO VOLPATO
      * DATA: 04/04/2004
      * FUNCAO: CONSULTA POP-UP ATENDIMENTO A CLIENTE
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY ACPX010.
           COPY ACPX020.

       DATA DIVISION.
       FILE SECTION.
           COPY ACPW010.
           COPY ACPW020.

       WORKING-STORAGE SECTION.
           COPY "ACP010T.CPB".
           COPY "ACP010T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-ACD010             PIC XX       VALUE SPACES.
           05  ST-ACD020             PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(65)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(3)   VALUE SPACES.
           05  WS-TIPO               PIC 9(01)  VALUE ZEROS.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 MENSAGEM                   PIC X(200).
       01 RESP-MSG                   PIC X(01).
       01 TIPO-MSG                   PIC X(01).

       01 DET-DETALHE.
          05 DET-CODIGO              PIC 9(03).
          05 FILLER                  PIC X(02).
          05 DET-NOME                PIC X(60).

       LINKAGE SECTION.
       01  STRING-1               PIC X(65) VALUE SPACES.

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "ACD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-ACD010.
           MOVE "ACD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-ACD020.
           OPEN INPUT ACD010 ACD020
           IF ST-ACD020 <> "00"
              MOVE "ERRO ABERTURA ACD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-ACD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE
           IF ST-ACD010 <> "00"
              MOVE "ERRO ABERTURA ACD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-ACD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO
           ELSE PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-CARREGA-SB-TIPOS-TRUE
                    PERFORM CARREGA-SB-TIPOS
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

       CARREGA-SB-TIPOS SECTION.
           INITIALIZE REG-ACD020 GS-TAB-TIPO GS-CONT
           START ACD020 KEY IS NOT LESS CHAVE-AC20 INVALID KEY
               MOVE "10" TO ST-ACD020.

           PERFORM UNTIL ST-ACD020 = "10"
               READ ACD020 NEXT RECORD AT END
                   MOVE "10"   TO ST-ACD020
               NOT AT END
                   ADD  1      TO GS-CONT
                   MOVE SPACES TO GS-TAB-TIPOS(GS-CONT)
                   STRING CODIGO-AC20(3:1) "-" DESCRICAO-AC20 INTO
                          GS-TAB-TIPOS(GS-CONT)
               END-READ
            END-PERFORM

            MOVE "INSERE-TIPO" TO DS-PROCEDURE
            PERFORM CALL-DIALOG-SYSTEM.


       CARREGA-MENSAGEM-ERRO SECTION.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-TAB-TIPOS(1)(1:1)  TO TIPO-AC10
           MOVE WS-TIPO               TO TIPO-AC10
           MOVE SPACES                TO DESCRICAO-AC10
           MOVE ZEROS                 TO GS-CONT

           START ACD010 KEY IS NOT < ALT-AC10 INVALID KEY
                 MOVE "10" TO ST-ACD010.

           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-ACD010 = "10"

              READ ACD010 NEXT RECORD AT END
                   MOVE "10" TO ST-ACD010
              NOT AT END
                   IF GS-TAB-TIPOS(1)(1:1) <> TIPO-AC10(3:1)
                      MOVE "10" TO ST-ACD010
                   ELSE
                      ADD 1               TO GS-CONT

                      MOVE CODIGO-AC10    TO GS-TAB-LINDET(GS-CONT)
                      (62:3)
                      MOVE DESCRICAO-AC10 TO GS-TAB-LINDET(GS-CONT)
                      (1:60)
                   END-IF
              END-READ
           END-PERFORM
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       ITEM-SELECIONADO SECTION.
           MOVE GS-LINHA-AUXILIAR      TO STRING-1.
           MOVE GS-TAB-TIPOS(1)(1:1)   TO STRING-1(61:1)
           MOVE STRING-1(62: 3)        TO CODIGO-W.
           IF CODIGO-W = SPACES
              MOVE ZEROS TO STRING-1(62: 3).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "ACP010T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE ACD010.
           move ds-quit-set to ds-control
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
