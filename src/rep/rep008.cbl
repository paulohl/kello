       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP008.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 02/03/2000
      *DESCRIÇÃO: Cadastro de EQUIPAMENTOS POR PESSOA -  REPORTAGEM
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       PRINTER IS LPRINTER
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY REPX001.
           COPY REPX008.
           COPY CGPX001.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY REPW001.
       COPY REPW008.
       COPY CGPW001.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY "REP008.CPB".
           COPY "REP008.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(65).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-RED001             PIC XX       VALUE SPACES.
           05  ST-RED008             PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de região utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
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
           "RELACAO DE EQUIPAMENTO POR PESSOA".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "PROPR. DESCRICAO                     EQU DESCRICAO      ".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

           copy impressora.

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
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "RED001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED001.
           MOVE "RED008" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED008.
           OPEN I-O RED008.
           OPEN INPUT CGD001 RED001.
           MOVE 1 TO GRAVA-W.
           IF ST-RED008 = "35"
              CLOSE RED008      OPEN OUTPUT RED008
              CLOSE RED008      OPEN I-O RED008
           END-IF.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED001 <> "00"
              MOVE "ERRO ABERTURA RED001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED008 <> "00"
              MOVE "ERRO ABERTURA RED008: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED008 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                MOVE 1 TO GS-ORDER
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 6)   TO GS-PROPRIETARIO
                   MOVE GS-LINDET(8: 30)  TO GS-DESC-PROPRIETARIO
                   MOVE GS-LINDET(40: 3)  TO GS-EQUIPAMENTO
                   MOVE GS-LINDET(45: 20) TO GS-DESC-EQUIPAMENTO
                   PERFORM CARREGAR-DADOS
               WHEN GS-LER-PROPRIETARIO-TRUE
                   PERFORM LER-PROPRIETARIO
               WHEN GS-LER-EQUIPAMENTO-TRUE
                   PERFORM LER-EQUIPAMENTO
               WHEN GS-POPUP-TRUE
                   PERFORM CONSULTA-POPUP
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE GS-PROPRIETARIO    TO PROPRIETARIO-RE08.
           MOVE GS-EQUIPAMENTO     TO EQUIPAMENTO-RE08.
           READ RED008 INVALID KEY INITIALIZE REG-RED008
                                   MOVE 1 TO GRAVA-W.
           MOVE PROPRIETARIO-RE08  TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO GS-DESC-PROPRIETARIO
           MOVE EQUIPAMENTO-RE08   TO CODIGO-RE01.
           READ RED001 INVALID KEY MOVE SPACES TO DESCRICAO-RE01.
           MOVE DESCRICAO-RE01     TO GS-DESC-EQUIPAMENTO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-RED008
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LER-PROPRIETARIO SECTION.
           MOVE GS-PROPRIETARIO TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01       TO GS-DESC-PROPRIETARIO.
       LER-EQUIPAMENTO SECTION.
           MOVE GS-EQUIPAMENTO  TO CODIGO-RE01.
           READ RED001 INVALID KEY MOVE SPACES TO DESCRICAO-RE01.
           MOVE DESCRICAO-RE01  TO GS-DESC-EQUIPAMENTO.
       CONSULTA-POPUP SECTION.
           EVALUATE GS-OPCAO-POPUP
             WHEN 2 CALL   "REP001T" USING PARAMETROS-W PASSAR-STRING
                    CANCEL "REP001T"
                    MOVE PASSAR-STRING(22: 3) TO GS-EQUIPAMENTO
                    MOVE PASSAR-STRING(1: 20) TO GS-DESC-EQUIPAMENTO
             WHEN 1 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING(33: 6) TO GS-PROPRIETARIO
                    MOVE PASSAR-STRING(1: 30) TO GS-DESC-PROPRIETARIO
           END-EVALUATE.
       EXCLUI-RECORD SECTION.
           DELETE RED008.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-PROPRIETARIO      TO PROPRIETARIO-RE08.
           MOVE GS-EQUIPAMENTO       TO EQUIPAMENTO-RE08.
           IF GRAVA-W = 1
              WRITE REG-RED008 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-RED008 INVALID KEY
                PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-RED008       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-PROPRIETARIO TO PROPRIETARIO-RE08.
           MOVE ZEROS TO EQUIPAMENTO-RE08.
           START RED008 KEY IS NOT < CHAVE-RE08 INVALID KEY
                             MOVE "10" TO ST-RED008.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-RED008 = "10"
              READ RED008 NEXT RECORD AT END MOVE "10" TO ST-RED008
              NOT AT END
               IF PROPRIETARIO-RE08 <> GS-PROPRIETARIO
                  MOVE "10" TO ST-RED008
               ELSE
                ADD 1 TO GS-CONT
      *         MOVE SPACES TO GS-LINDET
                MOVE PROPRIETARIO-RE08  TO GS-LINDET(01: 06) CODIGO-CG01
                READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                END-READ
                MOVE NOME-CG01          TO GS-LINDET(08: 30)
                MOVE EQUIPAMENTO-RE08   TO GS-LINDET(40: 4) CODIGO-RE01
                READ RED001 INVALID KEY MOVE SPACES TO DESCRICAO-RE01
                END-READ
                MOVE DESCRICAO-RE01     TO GS-LINDET(45: 20)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
               END-IF
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP008" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO PROPRIETARIO-RE08 EQUIPAMENTO-RE08.
           START RED008 KEY IS NOT < CHAVE-RE08 INVALID KEY
                           MOVE "10" TO ST-RED008.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-RED008 = "10"
             READ RED008 NEXT RECORD AT END MOVE "10" TO ST-RED008
              NOT AT END
               MOVE SPACES TO LINDET-REL
                MOVE PROPRIETARIO-RE08  TO LINDET-REL(01: 06)
                                           CODIGO-CG01
                READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                END-READ
                MOVE NOME-CG01          TO LINDET-REL(08: 30)
                MOVE EQUIPAMENTO-RE08   TO LINDET-REL(40: 4) CODIGO-RE01
                READ RED001 INVALID KEY MOVE SPACES TO DESCRICAO-RE01
                END-READ
                MOVE DESCRICAO-RE01     TO LINDET-REL(45: 20)
                WRITE REG-RELAT FROM LINDET
                ADD 1 TO LIN
                IF LIN > 56 PERFORM CABECALHO
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
           MOVE 4 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 RED001 RED008.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
