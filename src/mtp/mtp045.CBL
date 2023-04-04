       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP045.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 19/07/2000
      *DESCRIÇÃO: Emissão de Etiquetas - FICHA IDENTIFICACAO

       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY MTPX020.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY MTPW020.
       FD  RELAT.
       01  REG-RELAT.
           05  FILLER        PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP045.CPB".
           COPY "MTP045.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD020             PIC XX       VALUE SPACES.
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


       01  CAB01.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(05)   VALUE 'CAMP '.
           05  FILLER          PIC X(06)   VALUE 'N.ALB '.
           05  FILLER          PIC X(04)   VALUE 'EST '.
           05  FILLER          PIC X(04)   VALUE 'ENC '.
           05  FILLER          PIC X(06)   VALUE 'Q.FOL '.
           05  FILLER          PIC X(06)   VALUE 'Q.FOT '.
           05  FILLER          PIC X(04)   VALUE 'POS '.
           05  FILLER          PIC X(04)   VALUE 'FIT '.
           05  FILLER          PIC X(03)   VALUE 'P.F'.

       01  CAB02.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(05)   VALUE '==== '.
           05  FILLER          PIC X(06)   VALUE '===== '.
           05  FILLER          PIC X(04)   VALUE '=== '.
           05  FILLER          PIC X(04)   VALUE '=== '.
           05  FILLER          PIC X(06)   VALUE '===== '.
           05  FILLER          PIC X(06)   VALUE '===== '.
           05  FILLER          PIC X(04)   VALUE '=== '.
           05  FILLER          PIC X(04)   VALUE '=== '.
           05  FILLER          PIC X(03)   VALUE '==='.

       01  CAB03.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  CAMP-REL        PIC 9(04)   VALUE ZEROS.
           05  FILLER          PIC X(01)   VALUE SPACES.
           05  ALBUM-REL       PIC 9(04)   VALUE ZEROS.
           05  FILLER          PIC X(02)   VALUE SPACES.
           05  ESTOJO-REL      PIC 9(01)   VALUE ZEROS.
           05  FILLER          PIC X(03)   VALUE SPACES.
           05  ENCAD-REL       PIC 9(01)   VALUE ZEROS.
           05  FILLER          PIC X(03)   VALUE SPACES.
           05  QT-FOL-REL      PIC 9(04)   VALUE ZEROS.
           05  FILLER          PIC X(02)   VALUE SPACES.
           05  QT-FOT-REL      PIC 9(04)   VALUE ZEROS.
           05  FILLER          PIC X(02)   VALUE SPACES.
           05  POSTER-REL      PIC 9(01)   VALUE ZEROS.
           05  FILLER          PIC X(03)   VALUE SPACES.
           05  FITA-REL        PIC 9(01)   VALUE ZEROS.
           05  FILLER          PIC X(03)   VALUE SPACES.
           05  PORT-FIT-REL    PIC 9(03)   VALUE ZEROS.

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
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.

           OPEN INPUT MTD020.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
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
           MOVE GS-CONTRATO TO CONTRATO-MTG
           MOVE GS-SEQ-INI  TO NRALBUM-MTG

           START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                 MOVE "10" TO ST-MTD020
           END-START

           PERFORM UNTIL ST-MTD020 = "10"
             READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
               NOT AT END
                IF CONTRATO-MTG <> GS-CONTRATO
                   MOVE "10" TO ST-MTD020
                ELSE
                 MOVE ALBUM-MTG        TO GS-MENSAGEM
                 MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 IF GS-TIPO-SEQ = 1
                    IF NRALBUM-MTG < GS-SEQ-INI OR
                       NRALBUM-MTG > GS-SEQ-FIM
                       CONTINUE
                    ELSE
                       IF NAO-GEROU-ALBUM-MTG <> 1
                          PERFORM MOVER-DADOS-LINDET
                       END-IF
                    END-IF
                 ELSE
                    IF NAO-GEROU-ALBUM-MTG <> 1
                       PERFORM MOVER-DADOS-LINDET
                    END-IF
                 END-IF
                END-IF
             END-READ
           END-PERFORM.
           CLOSE RELAT.
       MOVER-DADOS-LINDET SECTION.
           IF NRALBUM-MTG = 0000 CONTINUE
           ELSE
            MOVE CONTRATO-MTG     TO CAMP-REL
            MOVE NRALBUM-MTG      TO ALBUM-REL
            MOVE QT-ESTOJO-MTG    TO ESTOJO-REL
            MOVE QT-ENCADER-MTG   TO ENCAD-REL
            MOVE QT-FOLHAS-MTG    TO QT-FOL-REL
            MOVE QT-FOTOS-MTG     TO QT-FOT-REL
            MOVE QT-FITAS-MTG     TO FITA-REL
            MOVE QT-POSTER-MTG    TO POSTER-REL
            MOVE QT-PORTA-FITA-MTG TO PORT-FIT-REL
            PERFORM IMPRIME-LINHA
           END-IF.

       IMPRIME-LINHA SECTION.
           WRITE REG-RELAT FROM CAB01.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB03.
           MOVE SPACES TO REG-RELAT. WRITE REG-RELAT AFTER 3.

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
           MOVE "MTP045" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE MTD020.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
