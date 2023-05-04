       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP043.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 20/07/2000
      *DESCRIÇÃO: Emissão de Etiquetas p/ Fita de vídeo P/ ALBUM
      *    1 ETIQUETA POR COLUNA (FORMULARIO 66 LINHAS) *
      *    1 FITA  (2 ETIQUETAS)   3 FITAS (5 ETIQUETAS)*
      *    2 FITAS (3 ETIQUETAS)   4 FITAS (6 ETIQUETAS)*

       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY MTPX019.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY MTPW019.
       FD  RELAT.
       01  REG-RELAT.
           05  FILLER        PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP043.CPB".
           COPY "MTP043.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W(FLAG)- PARA SABER SE OCORREU ERRO-ABERTURA ARQUIVO
      *  ERRO-W = 0 (NÃO)  ERRO-W = 1 (SIM)
           05  I                     PIC 9        VALUE ZEROS.
           05  J                     PIC 9        VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       01  LINDET.
           05  FILLER          PIC X(8)    VALUE SPACES.
           05  LINHA-REL OCCURS 4 TIMES.
               10 ALBUM-REL    PIC 9999.9999 BLANK WHEN ZEROS.
               10 FILLER       PIC X(09)    VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           OPEN INPUT MTD019.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-IMPRIME-ETIQUETA-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-ETIQUETA
                    END-IF
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       IMPRIME-ETIQUETA SECTION.
           OPEN OUTPUT RELAT.
           MOVE GS-CONTRATO TO ALBUM-MT19(1: 4)
           MOVE ZEROS       TO ALBUM-MT19(5: 4)
           START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                 MOVE "10" TO ST-MTD019
           END-START.

           PERFORM ZERA-ALBUM.
           MOVE ZEROS TO J.

           PERFORM UNTIL ST-MTD019 = "10"
             READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
               NOT AT END
                 MOVE ALBUM-MT19   TO GS-MENSAGEM
                 MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 IF CONTRATO-MT19 <> GS-CONTRATO MOVE "10" TO ST-MTD019
                 ELSE
                    ADD 1 TO J
                    IF J > 4 PERFORM IMPRESSAO-DETALHE
                             PERFORM ZERA-ALBUM
                             MOVE 1 TO J
                    END-IF
                    MOVE ALBUM-MT19    TO ALBUM-REL(J)
                 END-IF
             END-READ
           END-PERFORM.
           PERFORM IMPRESSAO-DETALHE.
           CLOSE RELAT.
       ZERA-ALBUM SECTION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
             MOVE ZEROS TO ALBUM-REL(I)
           END-PERFORM.
       IMPRESSAO-DETALHE SECTION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GS-QT-ETIQUETA
             WRITE REG-RELAT FROM LINDET AFTER 2
             MOVE SPACES TO REG-RELAT
             WRITE REG-RELAT AFTER 4
           END-PERFORM.

       CARREGA-MENSAGEM-ERRO SECTION.
           MOVE 1 TO ERRO-W.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP043" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE MTD019.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
