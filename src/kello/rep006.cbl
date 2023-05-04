       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP006.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 29/02/2000
      *DESCRIÇÃO: CADASTRO DE VEICULOS - REPORTAGEM

      *ACERTAR DATA ULTIMA ATUALIZAÇÃO

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA
       PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY REPX004.
           COPY REPX006.
           COPY REPX007.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY REPW004.
       COPY REPW006.
       COPY REPW007.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY "REP006.CPB".
           COPY "REP006.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(60).

       01  VARIAVEIS.
           05  ST-RED004             PIC XX       VALUE SPACES.
           05  ST-RED006             PIC XX       VALUE SPACES.
           05  ST-RED007             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(3)     VALUE ZEROS.
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
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
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
           "RELACAO DE VEICULOS      ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(120)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(120)  VALUE
           "COD VEICULO              PLACA     ANO CT VENC-SEGUR SEGURAD
      -    "ORA           SI QP CARGA-EQUIPAM PRO RENAVAM    COMBUS".

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
           MOVE "RED004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED004.
           MOVE "RED006" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED006.
           MOVE "RED007" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED007.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           OPEN I-O RED006.
           OPEN INPUT RED004 RED007 CGD001.
           MOVE 1 TO GRAVA-W.
           IF ST-RED006 = "35"
              CLOSE RED006      OPEN OUTPUT RED006
              CLOSE RED006      OPEN I-O RED006
           END-IF.
           IF ST-RED004 <> "00"
              MOVE "ERRO ABERTURA RED004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED006 <> "00"
              MOVE "ERRO ABERTURA RED006: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED006 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED007 <> "00"
              MOVE "ERRO ABERTURA RED007: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED007 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
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
                   copy impressora.chama.
                   if lnk-impressora <> spaces
                      PERFORM IMPRIME-RELATORIO
                   end-if
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 3) TO GS-CODIGO
                   PERFORM CARREGAR-DADOS
               WHEN GS-LER-SITUACAO-TRUE
                   PERFORM LER-SITUACAO
               WHEN GS-LER-CATEGORIA-TRUE
                   PERFORM LER-CATEGORIA
               WHEN GS-LER-PROPRIETARIO-TRUE
                   PERFORM LER-PROPRIETARIO
               WHEN GS-POPUP-GERAL-TRUE
                   PERFORM CHAMAR-POPUP-GERAL
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-CATEGORIA SECTION.
           MOVE GS-CATEGORIA   TO CODIGO-RE07.
           READ RED007 INVALID KEY MOVE SPACES TO CATEGORIA-RE07.
           MOVE CATEGORIA-RE07 TO GS-DESC-CATEGORIA.
       LER-SITUACAO SECTION.
           MOVE GS-SITUACAO    TO CODIGO-RE04.
           READ RED004 INVALID KEY MOVE SPACES TO DESCRICAO-RE04.
           MOVE DESCRICAO-RE04 TO GS-DESC-SITUACAO.
       LER-PROPRIETARIO SECTION.
           MOVE GS-PROPRIETARIO TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01       TO GS-DESC-PROPRIETARIO.
       CHAMAR-POPUP-GERAL SECTION.
           EVALUATE GS-POPUP
             WHEN 1 CALL   "REP004T" USING PARAMETROS-W PASSAR-STRING
                    CANCEL "REP004T"
                    MOVE PASSAR-STRING(22: 2) TO GS-SITUACAO
                    MOVE PASSAR-STRING(1: 20) TO GS-DESC-SITUACAO
             WHEN 2 CALL   "REP007T" USING PARAMETROS-W PASSAR-STRING
                    CANCEL "REP007T"
                    MOVE PASSAR-STRING(37: 2) TO GS-CATEGORIA
                    MOVE PASSAR-STRING(1: 15) TO GS-DESC-CATEGORIA
             WHEN 3 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING(33: 6) TO GS-PROPRIETARIO
                    MOVE PASSAR-STRING(1: 30) TO GS-DESC-PROPRIETARIO
           END-EVALUATE.
       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE GS-CODIGO          TO CODIGO-RE06.
           READ RED006 INVALID KEY INITIALIZE REG-RED006
                                   MOVE 1 TO GRAVA-W.
           MOVE CODIGO-RE06        TO GS-CODIGO
           MOVE VEICULO-RE06       TO GS-VEICULO
           MOVE PLACA-RE06         TO GS-PLACA
           MOVE ANO-FAB-RE06       TO GS-ANO-FAB
           MOVE CATEGORIA-KILOM-RE06     TO GS-CATEGORIA CODIGO-RE07
           READ RED007 INVALID KEY MOVE SPACES TO CATEGORIA-RE07
           END-READ
           MOVE CATEGORIA-RE07     TO GS-DESC-CATEGORIA
           MOVE SEGURADORA-RE06    TO GS-SEGURADORA
           MOVE VENCTO-SEGURO-RE06 TO GS-VENCTO-SEGURO
           MOVE SITUACAO-RE06      TO GS-SITUACAO CODIGO-RE04
           READ RED004 INVALID KEY MOVE SPACES TO DESCRICAO-RE04
           END-READ
           MOVE DESCRICAO-RE04     TO GS-DESC-SITUACAO
           MOVE QTDE-PASSAG-RE06   TO GS-QTDE-PASSAG
           MOVE CARGA-EQUIP-RE06   TO GS-CARGA-EQUIP
           MOVE PROPRIETARIO-RE06  TO GS-PROPRIETARIO CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
           END-READ
           MOVE NOME-CG01          TO GS-DESC-PROPRIETARIO
           MOVE RENAVAM-RE06       TO GS-RENAVAM
           MOVE ESPECIE-TIPO-RE06  TO GS-TIPO
           MOVE COMBUSTIVEL-RE06   TO GS-COMBUSTIVEL
           MOVE MODELO-RE06        TO GS-MODELO
           MOVE DATA-ULT-ATUAL-RE06  TO GS-DATA-ULT-ATUAL
           MOVE COR-RE06           TO GS-COR.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-RED006
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE RED006.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-CODIGO                  TO CODIGO-RE06
           MOVE GS-VEICULO                 TO VEICULO-RE06
           MOVE GS-PLACA                   TO PLACA-RE06
           MOVE GS-ANO-FAB                 TO ANO-FAB-RE06
           MOVE GS-CATEGORIA               TO CATEGORIA-KILOM-RE06
           MOVE GS-SEGURADORA              TO SEGURADORA-RE06
           MOVE GS-VENCTO-SEGURO           TO VENCTO-SEGURO-RE06
           MOVE GS-SITUACAO                TO SITUACAO-RE06
           MOVE GS-QTDE-PASSAG             TO QTDE-PASSAG-RE06
           MOVE GS-CARGA-EQUIP             TO CARGA-EQUIP-RE06
           MOVE GS-PROPRIETARIO            TO PROPRIETARIO-RE06
           MOVE GS-RENAVAM                 TO RENAVAM-RE06
           MOVE GS-TIPO                    TO ESPECIE-TIPO-RE06
           MOVE GS-COMBUSTIVEL             TO COMBUSTIVEL-RE06
           MOVE GS-MODELO                  TO MODELO-RE06
           MOVE GS-DATA-ULT-ATUAL          TO DATA-ULT-ATUAL-RE06
           MOVE GS-COR                     TO COR-RE06
           IF GRAVA-W = 1
              WRITE REG-RED006 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-RED006 INVALID KEY
                PERFORM ERRO-GRAVACAO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-RED006       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.

       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF GS-ORDER = ZEROS
              MOVE SPACES TO VEICULO-RE06
              START RED006 KEY IS NOT < VEICULO-RE06
                    INVALID KEY MOVE "10" TO ST-RED006
           ELSE
             MOVE ZEROS TO CODIGO-RE06
               START RED006 KEY IS NOT < CODIGO-RE06
                 INVALID KEY MOVE "10" TO ST-RED006.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-RED006 = "10"
              READ RED006 NEXT RECORD AT END MOVE "10" TO ST-RED006
              NOT AT END
                MOVE SPACES                TO GS-LINDET
                MOVE CODIGO-RE06           TO GS-LINDET(1: 4)
                MOVE VEICULO-RE06          TO GS-LINDET(5: 21)
                MOVE PLACA-RE06            TO GS-LINDET(26: 9)
                MOVE ANO-FAB-RE06          TO GS-LINDET(35: 5)
                MOVE CATEGORIA-KILOM-RE06  TO GS-LINDET(40: 3)
                MOVE VENCTO-SEGURO-RE06    TO DATA-E
                MOVE DATA-E                TO GS-LINDET(43: 11)
                MOVE SEGURADORA-RE06       TO GS-LINDET(54: 21)
                MOVE SITUACAO-RE06         TO GS-LINDET(75: 3)
                MOVE QTDE-PASSAG-RE06      TO GS-LINDET(78: 3)
                MOVE CARGA-EQUIP-RE06      TO GS-LINDET(81: 14)
                MOVE PROPRIETARIO-RE06     TO GS-LINDET(95: 3)
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
           MOVE "REP006" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           copy condensa.

           MOVE ZEROS TO PAG-W.
           IF GS-ORDER = 1
              MOVE ZEROS TO CODIGO-RE06
              START RED006 KEY IS NOT < CODIGO-RE06 INVALID KEY
                           MOVE "10" TO ST-RED006
           ELSE MOVE SPACES TO VEICULO-RE06
                START RED006 KEY IS NOT < VEICULO-RE06 INVALID KEY
                           MOVE "10" TO ST-RED006.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-RED006 = "10"
             READ RED006 NEXT RECORD AT END MOVE "10" TO ST-RED006
              NOT AT END
                MOVE SPACES TO LINDET-REL
                MOVE CODIGO-RE06           TO LINDET-REL(1: 4)
                MOVE VEICULO-RE06          TO LINDET-REL(5: 21)
                MOVE PLACA-RE06            TO LINDET-REL(26: 9)
                MOVE ANO-FAB-RE06          TO LINDET-REL(35: 5)
                MOVE CATEGORIA-KILOM-RE06  TO LINDET-REL(40: 3)
                MOVE VENCTO-SEGURO-RE06    TO DATA-E
                MOVE DATA-E                TO LINDET-REL(43: 11)
                MOVE SEGURADORA-RE06       TO LINDET-REL(54: 21)
                MOVE SITUACAO-RE06         TO LINDET-REL(75: 3)
                MOVE QTDE-PASSAG-RE06      TO LINDET-REL(78: 3)
                MOVE CARGA-EQUIP-RE06      TO LINDET-REL(81: 14)
                MOVE PROPRIETARIO-RE06     TO LINDET-REL(95: 4)
                MOVE RENAVAM-RE06          TO LINDET-REL(99: 11)
                MOVE COMBUSTIVEL-RE06      TO LINDET-REL(110: 6)
                WRITE REG-RELAT FROM LINDET
                ADD 1 TO LIN
                IF LIN > 56 PERFORM CABECALHO
                END-IF
             END-READ
           END-PERFORM.

           copy descondensa.

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
      ********************************************************
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO CODIGO-RE06 ULT-CODIGO
           START RED006 KEY IS NOT < CODIGO-RE06 INVALID KEY
                 MOVE "10" TO ST-RED006
           END-START
           PERFORM UNTIL ST-RED006 = "10"
              READ RED006 NEXT RECORD AT END MOVE "10" TO ST-RED006
                NOT AT END
                 MOVE CODIGO-RE06 TO ULT-CODIGO
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
           CLOSE RED004 RED006 RED007 CGD001.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
