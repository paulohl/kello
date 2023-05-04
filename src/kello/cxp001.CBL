       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP001.
      *DATA: 14-06-2005
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: LIBERA ACESSO AS CONTAS DE APURAÇÃO
      *FUNÇÃO:
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.
           COPY CXPX020.
           COPY CXPX004.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.
       COPY CXPW020.
       COPY CXPW004.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP001.CPB".
           COPY "CXP001.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS     PIC X(65)    VALUE SPACES.
       01  VARIAVEIS.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CXD004             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
           05  CODIGO-E              PIC 9.99.99.99.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(35)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(80)   VALUE
           "RELACAO DE PROGRAMAS LIBERADOS ".
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "PROGRAMA     DESCRICAO                            LIBERADO".
       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-DIA-INV.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           OPEN INPUT CAD002 CXD020.
           OPEN I-O CXD004.
           CLOSE    CXD004
           OPEN I-O CXD004
           IF ST-CXD004 = "35"
              CLOSE CXD004   OPEN OUTPUT CXD004   CLOSE CXD004
              OPEN I-O CXD004.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD004 <> "00"
              MOVE "ERRO ABERTURA CXD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-LE-USUARIO-TRUE
                    PERFORM LE-USUARIO
               WHEN GS-POPUP-USUARIO-TRUE
                    PERFORM CHAMA-POPUP-USUARIO
               WHEN GS-LE-USUARIO2-TRUE
                    PERFORM LE-USUARIO2
               WHEN GS-POPUP-USUARIO2-TRUE
                    PERFORM CHAMA-POPUP-USUARIO2
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-COPIAR-USUARIO-TRUE
                    PERFORM COPIAR-USUARIO
               WHEN GS-LIBERAR-TRUE
                    PERFORM LIBERACAO
                    PERFORM CARREGA-LISTA
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LE-USUARIO SECTION.
           MOVE GS-CODIGO TO CODIGO-CA002.
           READ CAD002 INVALID KEY MOVE SPACES TO NOME-CA002.
           MOVE NOME-CA002 TO GS-USUARIO.

       LE-USUARIO2 SECTION.
           MOVE GS-CODIGO-NOVO TO CODIGO-CA002.
           READ CAD002 INVALID KEY
                MOVE SPACES TO NOME-CA002.
           MOVE NOME-CA002 TO GS-USUARIO-NOVO.

       CHAMA-POPUP-USUARIO SECTION.
           CALL "CAP002T" USING PARAMETROS-W PASSAR-PARAMETROS.
           MOVE PASSAR-PARAMETROS(38: 3) TO GS-CODIGO.
           PERFORM LE-USUARIO.

       CHAMA-POPUP-USUARIO2 SECTION.
           CALL "CAP002T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP002T"
           MOVE PASSAR-PARAMETROS(38: 3) TO GS-CODIGO-NOVO.
           PERFORM LE-USUARIO2.

       LIBERACAO SECTION.
           IF GS-LIBERAR-TODOS = 1
              INITIALIZE REG-CXD020
              START CXD020 KEY IS NOT LESS CODIGO-COMPL-CX20 INVALID KEY
                    MOVE "10" TO ST-CXD020
              END-START
              PERFORM UNTIL ST-CXD020 = "10"
                    READ CXD020 NEXT AT END
                         MOVE "10" TO ST-CXD020
                    NOT AT END
                         MOVE GS-CODIGO TO COD-USUARIO-CX004
                         MOVE CODIGO-REDUZ-CX20 TO PROGRAMA-CX004
                         WRITE REG-CXD004
                    END-READ
              END-PERFORM
           ELSE
              INITIALIZE REG-CXD004
              MOVE GS-CODIGO           TO COD-USUARIO-CX004
              START CXD004 KEY IS NOT LESS CHAVE-CX004 INVALID KEY
                   MOVE "10" TO ST-CXD004
              END-START
              PERFORM UNTIL ST-CXD004 = "10"
                   READ CXD004 NEXT AT END
                        MOVE "10" TO ST-CXD004
                   NOT AT END
                        IF GS-CODIGO <> COD-USUARIO-CX004
                           MOVE "10" TO ST-CXD004
                        ELSE
                           DELETE CXD004
                           END-DELETE
                        END-IF
                   END-READ
              END-PERFORM
           END-IF.

       COPIAR-USUARIO SECTION.
           INITIALIZE REG-CXD004
           MOVE GS-CODIGO-NOVO     TO COD-USUARIO-CX004
           START CXD004 KEY IS NOT LESS CHAVE-CX004 INVALID KEY
                MOVE "10" TO ST-CXD004.

           PERFORM UNTIL ST-CXD004 = "10"
                READ CXD004 NEXT AT END
                     MOVE "10" TO ST-CXD004
                NOT AT END
                     IF GS-CODIGO-NOVO <> COD-USUARIO-CX004
                        MOVE "10" TO ST-CXD004
                     ELSE
                        DELETE CXD004
                        END-DELETE
                     END-IF
                END-READ
           END-PERFORM

           MOVE SPACES     TO GS-LINDET
           MOVE 1          TO GS-LINHA
           MOVE "LER-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINDET = SPACES
               IF GS-LINDET(58: 3) = "SIM"
                  MOVE GS-CODIGO-NOVO      TO COD-USUARIO-CX004
                  MOVE GS-LINDET(52: 05)   TO PROGRAMA-CX004
                  WRITE REG-CXD004
               END-IF
               MOVE SPACES TO GS-LINDET
               ADD 1       TO GS-LINHA
               MOVE "LER-LIST" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       ITEM-SELECIONADO SECTION.
           MOVE GS-CODIGO         TO COD-USUARIO-CX004.
           MOVE GS-LINDET(52: 05) TO PROGRAMA-CX004.
           READ CXD004 INVALID KEY
                WRITE REG-CXD004
                MOVE "SIM" TO GS-LINDET(58: 3)
            NOT INVALID KEY
                MOVE "NÃO" TO GS-LINDET(58: 3)
                DELETE CXD004
           END-READ.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS TO CODIGO-COMPL-CX20.
              START CXD020 KEY IS NOT < CODIGO-COMPL-CX20
                    INVALID KEY MOVE "10" TO ST-CXD020.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-CXD020 = "10"
              READ CXD020 NEXT RECORD AT END MOVE "10" TO ST-CXD020
              NOT AT END
                MOVE SPACES TO GS-LINDET
                MOVE CODIGO-COMPL-CX20 TO CODIGO-E
                EVALUATE GRAU-CX20
                  WHEN 1 PERFORM GRAU-1
                  WHEN 2 PERFORM GRAU-2
                  WHEN 3 PERFORM GRAU-3
                  WHEN 4 PERFORM GRAU-4
                END-EVALUATE
                MOVE GS-CODIGO          TO COD-USUARIO-CX004
                MOVE CODIGO-REDUZ-CX20  TO PROGRAMA-CX004
                READ CXD004 INVALID KEY
                     MOVE "NÃO" TO GS-LINDET(58: 3)
                NOT INVALID KEY
                     MOVE "SIM" TO GS-LINDET(58: 3)
                END-READ
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       GRAU-1 SECTION.
           MOVE CODIGO-E          TO GS-LINDET(1: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET(12: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET(52: 05).
       GRAU-2 SECTION.
           MOVE CODIGO-E          TO GS-LINDET(4: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET(15: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET(52: 05).
       GRAU-3 SECTION.
           MOVE CODIGO-E          TO GS-LINDET(7: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET(18: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET(52: 05).
       GRAU-4 SECTION.
           MOVE CODIGO-E          TO GS-LINDET(10: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET(21: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET(52: 05).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP001" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
      *    MOVE ZEROS TO PAG-W.
      *    COPY "COND-IMP".
      *    OPEN OUTPUT RELAT.
      *    MOVE ZEROS TO LIN. PERFORM CABECALHO.
      *    MOVE SPACES TO LINDET-REL
      *    MOVE ZEROS TO PROGRAMA-CA003.
      *    START CAD003 KEY IS NOT < PROGRAMA-CA003 INVALID KEY
      *          MOVE "10" TO ST-CAD004.
      *    PERFORM UNTIL ST-CAD003 = "10"
      *       READ CAD003 NEXT RECORD AT END MOVE "10" TO ST-CAD003
      *       NOT AT END
      *         MOVE PROGRAMA-CA003  TO LINDET-REL(1: 8)
      *         MOVE DESCRICAO-CA003 TO LINDET-REL(14: 31)
      *         MOVE GS-CODIGO       TO COD-USUARIO-CA004
      *         MOVE PROGRAMA-CA003  TO PROGRAMA-CA004
      *         READ CAD004 INVALID KEY MOVE "NAO" TO LINDET-REL(57: 3)
      *              NOT INVALID KEY MOVE "SIM" TO LINDET-REL(57: 3)
      *         END-READ
      *         WRITE REG-RELAT FROM LINDET
      *         END-WRITE
      *         ADD 1 TO LIN
      *         IF LIN > 56 PERFORM CABECALHO
      *         END-IF
      *       END-READ
      *    END-PERFORM.
      *
      *    MOVE SPACES TO REG-RELAT.
      *    WRITE REG-RELAT AFTER PAGE.
      *    CLOSE RELAT.
      *    COPY "DESC-IMP".
       TOTALIZA-REL SECTION.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD002 CXD020 CXD004.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
