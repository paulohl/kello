       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP041.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 19/07/2000
      *DESCRIÇÃO: Emissão de Etiquetas p/ Fita de vídeo - alfab/numérica
      *    1 ETIQUETA POR COLUNA (FORMULARIO 66 LINHAS) *
      *    1 FITA  (2 ETIQUETAS)   3 FITAS (5 ETIQUETAS)*
      *    2 FITAS (3 ETIQUETAS)   4 FITAS (6 ETIQUETAS)*
      *                            5 FITAS (1 ETIQUETA)
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY MTPX019.
           COPY MTPX020.
           COPY IEPX011.
           COPY CAPX010.
           COPY COPX040.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY IEPW011.
       COPY MTPW019.
       COPY MTPW020.
       COPY COPW040.
       FD  RELAT.
       01  REG-RELAT.
           05  FILLER        PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP041.CPB".
           COPY "MTP041.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  SEQUENCIA             PIC 9(04).
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
           05  ALBUM-REL       PIC 9999.9999 BLANK WHEN ZEROS.
           05  FILLER          PIC X(3)    VALUE SPACES.
           05  NUMERO-REL      PIC 9       BLANK WHEN ZEROS.
           05  FILLER          PIC X(3)    VALUE SPACES.
           05  CURSO-REL       PIC X(27)   VALUE SPACES.
           05  SEQUENCIA-REL   PIC ZZZ9.

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
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.

           OPEN INPUT MTD019 IED011 CAD010 COD040 MTD020.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LE-CONTRATO
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

       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO    TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY INITIALIZE REG-COD040.
           MOVE CIDADE-CO40    TO CIDADE.
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-COMPL-CID.
           MOVE NOME-COMPL-CID TO GS-CIDADE
           MOVE UF-CID         TO GS-UF.
       IMPRIME-ETIQUETA SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO SEQUENCIA
           EVALUATE GS-ORDEM
             WHEN 0 MOVE GS-CONTRATO TO ALBUM-MT19(1: 4)
                    MOVE ZEROS       TO ALBUM-MT19(5: 4)
                    START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                          MOVE "10" TO ST-MTD019
                    END-START
             WHEN 1 MOVE GS-CONTRATO TO CONTRATO-MT19
                    MOVE ZEROS       TO SEQ-MT19
                    MOVE ZEROS       TO CURSO-MT19
                    MOVE SPACES      TO NOME-FORM-MT19
                    START MTD019 KEY IS NOT < ALT1-MT19 INVALID KEY
                          MOVE "10" TO ST-MTD019
                    END-START
           END-EVALUATE.
           PERFORM UNTIL ST-MTD019 = "10"
             READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
               NOT AT END
                IF CONTRATO-MT19 <> GS-CONTRATO
                   MOVE "10" TO ST-MTD019
                ELSE
                 MOVE ALBUM-MT19       TO GS-MENSAGEM
                 MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM

                 MOVE ALBUMMT19        TO ALBUM-MTG
                 READ MTD020 NOT INVALID KEY
                      IF QT-FOTOS-MTG > 0
                         IF SEQ-MT19 > 0
                            ADD 1 TO SEQUENCIA
                         END-IF
                         IF GS-TIPO-SEQ = 1
                            IF SEQ-MT19 < GS-SEQ-INI OR
                                                   SEQ-MT19 > GS-SEQ-FIM
                               CONTINUE
                            ELSE
                               PERFORM MOVER-DADOS-LINDET
                            END-IF
                         ELSE
                            PERFORM MOVER-DADOS-LINDET
                         END-IF
                      END-IF
                 END-READ
                END-IF
             END-READ
           END-PERFORM.
           CLOSE RELAT.
       MOVER-DADOS-LINDET SECTION.
           IF SEQ-MT19 = 0000 CONTINUE
           ELSE
            MOVE ALBUM-MT19       TO ALBUM-REL
            MOVE NOME-FORM-MT19   TO NOME-REL
            MOVE CURSO-MT19 TO CODIGO-IE11
            READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11
            END-READ
            MOVE NOME-IE11        TO CURSO-REL
            MOVE GS-CIDADE        TO CIDADE-REL
            MOVE GS-UF            TO UF-REL
            MOVE SEQUENCIA        TO SEQUENCIA-REL
            PERFORM IMPRESSAO-DETALHE
           END-IF.

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
           MOVE "MTP041" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE MTD019 MTD020 IED011 CAD010 COD040.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
