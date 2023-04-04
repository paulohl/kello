       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRP011.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 25/04/2000
      *DESCRIÇÃO: Observações feita através de avaliação p/ um determi-
      *           nado funcionário - Planejamento de Reportagem
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       PRINTER IS LPRINTER
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CGPX001.
           COPY PRPX011.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CGPW001.
       COPY PRPW011.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "PRP011.CPB".
           COPY "PRP011.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-PRD011             PIC XX       VALUE SPACES.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  SAIR                  PIC X(01)    VALUE SPACES.
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
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "OBSERVACAO FEITA ATRAVES DE AVALIACAO     ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "DOCTO  CODIGO NOME                           CID. NOME-CIDAD
      -    "E   DATA-REPOR VEZ".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "PRD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PRD011.
           OPEN I-O PRD011.
           OPEN INPUT CAD010 CGD001.
           IF ST-PRD011 = "35"
              CLOSE PRD011      OPEN OUTPUT PRD011
              CLOSE PRD011      OPEN I-O PRD011
           END-IF.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD011 <> "00"
              MOVE "ERRO ABERTURA PRD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                    PERFORM SALVAR-DADOS
                    PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI-RECORD
               WHEN GS-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
               WHEN GS-CARREGA-UL-TRUE
                    PERFORM CARREGAR-ULTIMO
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-VERIFICA-CADASTRO-TRUE
                    PERFORM VERIFICA-CADASTRO
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LER-CIDADE
               WHEN GS-LE-CODIGO-TRUE
                    PERFORM LER-FUNCIONARIO
               WHEN GS-POPUP-CIDADE-TRUE
                    PERFORM POPUP-CIDADE
               WHEN GS-POPUP-CODIGO-TRUE
                    PERFORM POPUP-FUNCIONARIO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-CIDADE SECTION.
           MOVE GS-CIDADE TO CIDADE.
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID  TO GS-NOME-CID.
       LER-FUNCIONARIO SECTION.
           MOVE GS-CODIGO   TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01   TO GS-NOME-FUNC.

       CARREGAR-ULTIMO SECTION.
           INITIALIZE REG-PRD011

           MOVE "N" TO SAIR
           MOVE  0  TO GS-DOCTO

           PERFORM UNTIL SAIR = "S"
               ADD  1         TO GS-DOCTO
               MOVE GS-DOCTO  TO DOCTO-PR11
               MOVE GS-CODIGO TO CODIGO-PR11
               READ PRD011 INVALID KEY
                   MOVE "S" TO SAIR
               END-READ
           END-PERFORM.

      *    MOVE ZEROS       TO DOCTO-PR11
      *    MOVE GS-CODIGO   TO CODIGO-PR11
      *    START PRD011 KEY IS LESS THEN CHAVE-PR11 INVALID KEY
      *        MOVE "10" TO ST-PRD011.
      *    PERFORM UNTIL ST-PRD011 = "10"
      *        READ PRD011 NEXT AT END
      *            MOVE "10" TO ST-PRD011
      *        NOT AT END
      *            IF GS-CODIGO <> CODIGO-PR11
      *               MOVE "10" TO ST-PRD011
      *            ELSE
      *               MOVE DOCTO-PR11 TO GS-DOCTO
      *            END-IF
      *        END-READ
      *    END-PERFORM

      *    ADD 1 TO GS-DOCTO.

       POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE.
           PERFORM LER-CIDADE.
       POPUP-FUNCIONARIO SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-CODIGO.
           PERFORM LER-FUNCIONARIO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-PRD011
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE PRD011.
           PERFORM LIMPAR-DADOS.
       VERIFICA-CADASTRO SECTION.
           MOVE GS-DOCTO        TO DOCTO-PR11.
           MOVE GS-CODIGO       TO CODIGO-PR11.
           READ PRD011 INVALID KEY
                MOVE 1 TO GRAVA-W
           NOT INVALID KEY
                MOVE 0 TO GRAVA-W
                MOVE DOCTO-PR11         TO GS-DOCTO
                MOVE CODIGO-PR11        TO GS-CODIGO CODIGO-CG01
                READ CGD001 INVALID KEY
                     MOVE SPACES TO NOME-CG01
                END-READ
                MOVE NOME-CG01          TO GS-NOME-FUNC
                MOVE CIDADE-PR11        TO GS-CIDADE CIDADE
                READ CAD010 INVALID KEY
                     MOVE SPACES TO NOME-CID
                END-READ
                MOVE NOME-CID           TO GS-NOME-CID
                MOVE DATA-REPORT-PR11   TO GS-DATA-REPORT
                MOVE VEZ-APRESENTA-PR11 TO GS-VEZ-APRESENTA
                MOVE OBSERVACAO-PR11(1) TO GS-OBS1
                MOVE OBSERVACAO-PR11(2) TO GS-OBS2
                MOVE OBSERVACAO-PR11(3) TO GS-OBS3
           END-READ.
       SALVAR-DADOS SECTION.
           MOVE GS-DOCTO         TO DOCTO-PR11.
           MOVE GS-CODIGO        TO CODIGO-PR11.
           MOVE GS-CIDADE        TO CIDADE-PR11.
           MOVE GS-DATA-REPORT   TO DATA-REPORT-PR11.
           MOVE GS-VEZ-APRESENTA TO VEZ-APRESENTA-PR11.
           MOVE GS-OBS1          TO OBSERVACAO-PR11(1)
           MOVE GS-OBS2          TO OBSERVACAO-PR11(2)
           MOVE GS-OBS3          TO OBSERVACAO-PR11(3).
           IF GRAVA-W = 1
              WRITE REG-PRD011 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE
              REWRITE REG-PRD011 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-PRD011       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "PRP011" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           PERFORM CABECALHO.
           MOVE GS-DOCTO        TO DOCTO-PR11.
           MOVE ZEROS           TO CODIGO-PR11.
           START PRD011 KEY IS NOT < CHAVE-PR11 INVALID KEY
                 MOVE "10" TO ST-PRD011.
           PERFORM UNTIL ST-PRD011 = "10"
             READ PRD011 NEXT RECORD AT END MOVE "10" TO ST-PRD011
               NOT AT END
                 IF DOCTO-PR11 <> GS-DOCTO MOVE "10" TO ST-PRD011
                 ELSE
                   MOVE DOCTO-PR11    TO LINDET-REL(1: 7)
                   MOVE CODIGO-PR11   TO LINDET-REL(8: 7) CODIGO-CG01
                   READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01          TO LINDET-REL(15: 31)
                   MOVE CIDADE-PR11        TO LINDET-REL(46: 5) CIDADE
                   READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
                   END-READ
                   MOVE NOME-CID           TO LINDET-REL(51: 14)
                   MOVE DATA-REPORT-PR11   TO DATA-E
                   MOVE DATA-E             TO LINDET-REL(65: 11)
                   MOVE VEZ-APRESENTA-PR11 TO LINDET-REL(76: 2)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
                   END-IF
                   IF OBSERVACAO-PR11(1) <> SPACES
                      MOVE OBSERVACAO-PR11(1) TO LINDET-REL
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56 PERFORM CABECALHO
                      END-IF
                   END-IF
                   IF OBSERVACAO-PR11(2) <> SPACES
                      MOVE OBSERVACAO-PR11(2) TO LINDET-REL
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56 PERFORM CABECALHO
                      END-IF
                   END-IF
                   IF OBSERVACAO-PR11(3) <> SPACES
                      MOVE OBSERVACAO-PR11(3) TO LINDET-REL
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56 PERFORM CABECALHO
                      END-IF
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
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
           CLOSE PRD011 CAD010 CGD001.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
