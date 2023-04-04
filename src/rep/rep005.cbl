       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP005.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 25/02/2000
      *DESCRIÇÃO: CADASTRO DE HOTEL - REPORTAGEM
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA
       PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY REPX005.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY REPW005.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY "REP005.CPB".
           COPY "REP005.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(65).

       01  VARIAVEIS.
           05  ST-RED005             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(4)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(100)  VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(103)  VALUE
           "RELACAO DE HOTEL         ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(120)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(120)  VALUE
           "COD. HOTEL                          ENDERECO
      -    "       CIDADE           VLR-DIARIA   FONE-1   FONE-2".

       01  LINDET.
           05  LINDET-REL          PIC X(120)  VALUE SPACES.

           copy impressora.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO.
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "RED005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED005.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           OPEN I-O RED005.
           OPEN INPUT CAD010.
           MOVE 1 TO GRAVA-W.
           IF ST-RED005 = "35"
              CLOSE RED005      OPEN OUTPUT RED005
              CLOSE RED005      OPEN I-O RED005
           END-IF.
           IF ST-RED005 <> "00"
              MOVE "ERRO ABERTURA RED005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                MOVE 1 TO GS-ORDER
                PERFORM ACHAR-CODIGO
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4) TO GS-CODIGO
                   PERFORM CARREGAR-DADOS
               WHEN GS-LER-CIDADE-TRUE
                   PERFORM LER-CIDADE
               WHEN GS-POPUP-CIDADE-TRUE
                   PERFORM CHAMAR-POPUP-CIDADE
               WHEN GS-CHAMA-POPUP-TRUE
                   PERFORM CHAMAR-ORDEM-CIDADE
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
           MOVE GS-CIDADE      TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID           TO GS-NOME-CIDADE
           MOVE DDD-CID            TO GS-DDD.
       CHAMAR-POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING(35: 4) TO GS-CIDADE.
           PERFORM LER-CIDADE.
       CHAMAR-ORDEM-CIDADE SECTION.
           CALL   "REP005T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "REP005T".
           MOVE PASSAR-STRING(37: 4) TO GS-CODIGO
           PERFORM CARREGAR-DADOS.

       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE GS-CODIGO          TO CODIGO-RE05.
           READ RED005 INVALID KEY INITIALIZE REG-RED005
                                   MOVE 1 TO GRAVA-W.
           MOVE CODIGO-RE05        TO GS-CODIGO
           MOVE HOTEL-RE05         TO GS-HOTEL
           MOVE ENDERECO-RE05      TO GS-ENDERECO
           MOVE CIDADE-RE05        TO GS-CIDADE
           PERFORM LER-CIDADE
           MOVE VLR-DIARIA-RE05    TO GS-VLR-DIARIA
           MOVE FONE1-RE05         TO GS-FONE-1
           MOVE FONE2-RE05         TO GS-FONE-2.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-RED005
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE RED005.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-CODIGO              TO CODIGO-RE05
           MOVE GS-HOTEL               TO HOTEL-RE05
           MOVE GS-ENDERECO            TO ENDERECO-RE05
           MOVE GS-CIDADE              TO CIDADE-RE05
           MOVE GS-NOME-CIDADE         TO NOME-CID-RE05
           MOVE GS-VLR-DIARIA          TO VLR-DIARIA-RE05
           MOVE GS-FONE-1              TO FONE1-RE05
           MOVE GS-FONE-2              TO FONE2-RE05.
           IF GRAVA-W = 1
              WRITE REG-RED005 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-RED005 INVALID KEY
                PERFORM ERRO-GRAVACAO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-RED005       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.

       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF GS-ORDER = ZEROS
              MOVE SPACES TO HOTEL-RE05
              START RED005 KEY IS NOT < HOTEL-RE05
                    INVALID KEY MOVE "10" TO ST-RED005
           ELSE
             MOVE ZEROS TO CODIGO-RE05
               START RED005 KEY IS NOT < CODIGO-RE05
                 INVALID KEY MOVE "10" TO ST-RED005.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-RED005 = "10"
              READ RED005 NEXT RECORD AT END MOVE "10" TO ST-RED005
              NOT AT END
                ADD 1 TO GS-CONT
                MOVE SPACES                TO GS-LINDET
                MOVE CODIGO-RE05           TO GS-LINDET(1: 5)
                MOVE HOTEL-RE05            TO GS-LINDET(6: 31)
                MOVE CIDADE-RE05           TO GS-LINDET(37: 5)
                MOVE NOME-CID-RE05         TO GS-LINDET(42: 13)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "REP005" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           copy condensa.

           MOVE ZEROS TO PAG-W.
           IF GS-ORDER = 1
              MOVE ZEROS TO CODIGO-RE05
              START RED005 KEY IS NOT < CODIGO-RE05 INVALID KEY
                    MOVE "10" TO ST-RED005
           ELSE
              MOVE SPACES TO HOTEL-RE05
              START RED005 KEY IS NOT < HOTEL-RE05 INVALID KEY
                    MOVE "10" TO ST-RED005.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-RED005 = "10"
               READ RED005 NEXT RECORD AT END
                    MOVE "10" TO ST-RED005
               NOT AT END
                    MOVE SPACES TO LINDET-REL
                    MOVE CODIGO-RE05           TO LINDET-REL(1: 5)
                    MOVE HOTEL-RE05            TO LINDET-REL(6: 31)
                    MOVE ENDERECO-RE05         TO LINDET-REL(37: 31)
                    MOVE NOME-CID-RE05         TO LINDET-REL(68: 14)
                    MOVE VLR-DIARIA-RE05       TO VALOR-E
                    MOVE VALOR-E               TO LINDET-REL(82: 14)
                    MOVE FONE1-RE05            TO LINDET-REL(96: 9)
                    MOVE FONE2-RE05            TO LINDET-REL(105: 8)
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 57
                       PERFORM CABECALHO
                    END-IF
               END-READ
           END-PERFORM.

           copy descondensa.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
      ********************************************************
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO CODIGO-RE05 ULT-CODIGO
           START RED005 KEY IS NOT < CODIGO-RE05 INVALID KEY
                 MOVE "10" TO ST-RED005
           END-START
           PERFORM UNTIL ST-RED005 = "10"
              READ RED005 NEXT RECORD AT END MOVE "10" TO ST-RED005
                NOT AT END
                 MOVE CODIGO-RE05 TO ULT-CODIGO
              END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO GS-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO GS-CODIGO
           MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE.
      ************************************************************
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE RED005 CAD010.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
