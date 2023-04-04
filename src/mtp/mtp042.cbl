       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP042.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 20/07/2000
      *DESCRIÇÃO: Emissão de Etiquetas p/ Fita de vídeo
      *           COM DADOS FIXOS
      *    1 ETIQUETA POR COLUNA (FORMULARIO 66 LINHAS) *
      *    1 FITA  (2 ETIQUETAS)   3 FITAS (5 ETIQUETAS)*
      *    2 FITAS (3 ETIQUETAS)   4 FITAS (6 ETIQUETAS)*
      *    5 FITAS  (1 ETIQUETA)
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY COPX040.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY COPW040.
       FD  RELAT.
       01  REG-RELAT.
           05  FILLER        PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP042.CPB".
           COPY "MTP042.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010           PIC XX       VALUE SPACES.
           05  ST-COD040           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W(FLAG)- PARA SABER SE OCORREU ERRO-ABERTURA ARQUIVO
      *  ERRO-W = 0 (NÃO)  ERRO-W = 1 (SIM)
           05  K                     PIC 9        VALUE ZEROS.
           05  I                     PIC 9        VALUE ZEROS.
           05  J                     PIC 9        VALUE ZEROS.
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
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  ALBUM-REL       PIC ZZZZ.ZZZZ BLANK WHEN ZEROS.
           05  FILLER          PIC X(6)    VALUE SPACES.
           05  NUMERO-REL      PIC 9       BLANK WHEN ZEROS.
           05  FILLER          PIC X(6)    VALUE SPACES.
           05  CURSO-REL       PIC X(27)   VALUE SPACES.

       01  CAB02.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  NOME-REL        PIC X(42)   VALUE SPACES.

       01  CAB03.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  CIDADE-REL      PIC X(31)   VALUE SPACES.
           05  UF-REL          PIC X(11)   VALUE SPACES.

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
      *    MOVE 1 TO COD-USUARIO-W.

           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.

           OPEN INPUT CAD010 COD040.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.


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
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LER-CONTRATO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-CONTRATO SECTION.
           MOVE GS-CONTRATO    TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY INITIALIZE REG-COD040.
           MOVE IDENTIFICACAO-CO40   TO GS-CURSO
           MOVE CIDADE-CO40          TO CIDADE
           READ CAD010 INVALID KEY INITIALIZE REG-CAD010.
           MOVE NOME-COMPL-CID       TO GS-CIDADE
           MOVE UF-CID               TO GS-UF.
       IMPRIME-ETIQUETA SECTION.
           OPEN OUTPUT RELAT.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > GS-QT-PESSOAS
              MOVE K                TO GS-MENSAGEM
              MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              PERFORM MOVER-DADOS-LINDET
           END-PERFORM.
           CLOSE RELAT.
       MOVER-DADOS-LINDET SECTION.
           MOVE GS-CONTRATO      TO ALBUM-REL
           MOVE GS-NOME          TO NOME-REL
           MOVE GS-CURSO         TO CURSO-REL
           MOVE GS-CIDADE        TO CIDADE-REL
           MOVE GS-UF            TO UF-REL.
           PERFORM IMPRESSAO-DETALHE.

       IMPRESSAO-DETALHE SECTION.
      *    PERFORM VARYING I FROM 1 BY 1 UNTIL I > GS-QT-FITA
             EVALUATE GS-QT-FITA
               WHEN 1 MOVE ZEROS TO NUMERO-REL
                      PERFORM IMPRIME-LINHA
                      PERFORM IMPRIME-LINHA
               WHEN 2 PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                        MOVE J TO NUMERO-REL
                        PERFORM IMPRIME-LINHA
                      END-PERFORM
               WHEN 3 PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
                        MOVE J TO NUMERO-REL
                        PERFORM IMPRIME-LINHA
                      END-PERFORM
               WHEN 4 PERFORM VARYING J FROM 1 BY 1 UNTIL J > 6
                        MOVE J TO NUMERO-REL
                        PERFORM IMPRIME-LINHA
                      END-PERFORM
               WHEN 5 MOVE ZEROS TO NUMERO-REL
                      PERFORM IMPRIME-LINHA
             END-EVALUATE.
      *    END-PERFORM.
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
           MOVE "MTP042" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
