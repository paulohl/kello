       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP001.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 16/10/1998
      *DESCRIÇÃO: CADASTRO DE CONTAS BANCARIAS - CONCILIAÇÃO
      *           O campo CODIGO-ACESSO - senha p/ acesso conta
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CAPX010.
           COPY CBPX001.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CAPW010.
       COPY CBPW001.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CBP001.CPB".
           COPY "CBP001.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(60).

       01  VARIAVEIS.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
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
           "RELACAO DE CONTAS BANCARIAS         ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "CODIGO TITULAR              BANCO NOME-BANCO           AGENC
      -    " NR-CONTA       CIDA".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CBP001-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CBP001-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CBP001-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE CBP001-VERSION-NO  TO DS-VERSION-NO.
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CBD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD001.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           OPEN I-O CBD001.
           OPEN INPUT CGD001 CAD010.
           MOVE 1 TO GRAVA-W.
           IF ST-CBD001 = "35"
              CLOSE CBD001      OPEN OUTPUT CBD001
              CLOSE CBD001      OPEN I-O CBD001
           END-IF.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO CBP001-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CBP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CBP001-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CBP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO CBP001-MENSAGEM-ERRO
              MOVE ST-CAD010 TO CBP001-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CBP001-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                MOVE 1 TO CBP001-ORDER
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CBP001-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CBP001-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP001-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP001-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
               WHEN CBP001-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN CBP001-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CBP001-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP001-CARREGA-LIST-BOX-TRUE
                   MOVE CBP001-LINDET(1: 6) TO CBP001-CODIGO-FORN
                   PERFORM CARREGAR-DADOS
               WHEN CBP001-LER-FORNEC-TRUE
                   PERFORM LER-FORNECEDOR
               WHEN CBP001-LER-CIDADE-TRUE
                   PERFORM LER-CIDADE
               WHEN CBP001-POPUP-FORNEC-TRUE
                   PERFORM CHAMAR-POPUP-FORNEC
               WHEN CBP001-POPUP-CIDADE-TRUE
                   PERFORM CHAMAR-POPUP-CIDADE
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-FORNECEDOR SECTION.
           MOVE CBP001-CODIGO-FORN TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO CBP001-NOME-FORN.
       CHAMAR-POPUP-FORNEC SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING(33: 6) TO CBP001-CODIGO-FORN.
           MOVE PASSAR-STRING(1: 30) TO CBP001-NOME-FORN.
       LER-CIDADE SECTION.
           MOVE CBP001-CIDADE      TO CIDADE.
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID           TO CBP001-NOME-CIDADE.
       CHAMAR-POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING(35: 4) TO CBP001-CIDADE.
           MOVE PASSAR-STRING(1: 30) TO CBP001-NOME-CIDADE.
       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE CBP001-CODIGO-FORN TO CODIGO-FORN-CB01.
           READ CBD001 INVALID KEY INITIALIZE REG-CBD001
                                   MOVE 1 TO GRAVA-W.
           PERFORM LER-FORNECEDOR.
           MOVE TITULAR-CB01       TO CBP001-TITULAR
           MOVE NOME-BANCO-CB01    TO CBP001-NOME-BANCO
           MOVE CIDADE-CB01        TO CBP001-CIDADE
           PERFORM LER-CIDADE.
           MOVE NR-BANCO-CB01      TO CBP001-NR-BANCO
           MOVE AGENCIA-CB01       TO CBP001-AGENCIA
           MOVE NR-CONTA-CB01      TO CBP001-NR-CONTA
           MOVE COMPENSACAO-CB01   TO CBP001-COMPENSACAO
           MOVE NOME-GERENTE-CB01  TO CBP001-NOME-GERENTE
           MOVE ENDERECO-CB01      TO CBP001-ENDERECO
           MOVE TELEFONE-CB01      TO CBP001-TELEFONE
           MOVE CELULAR-CB01       TO CBP001-CELULAR
           MOVE E-MAIL-CB01        TO CBP001-E-MAIL
           MOVE CODIGO-ACESSO-CB01 TO CBP001-CODIGO-ACESSO
           MOVE PROCURADOR-CB01    TO CBP001-PROCURADOR
           MOVE VENCTO-PROCUR-CB01 TO CBP001-VENCTO-PROCURACAO
           MOVE LIMITE-CB01        TO CBP001-LIMITE.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CBD001
           MOVE CBP001-ORDER TO ORDEM-W
           INITIALIZE CBP001-DATA-BLOCK
           MOVE ORDEM-W TO CBP001-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE CBD001.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE CBP001-CODIGO-FORN        TO CODIGO-FORN-CB01
           MOVE CBP001-TITULAR            TO TITULAR-CB01
           MOVE CBP001-NOME-BANCO         TO NOME-BANCO-CB01
           MOVE CBP001-CIDADE             TO CIDADE-CB01
           MOVE CBP001-NR-BANCO           TO NR-BANCO-CB01
           MOVE CBP001-AGENCIA            TO AGENCIA-CB01
           MOVE CBP001-NR-CONTA           TO NR-CONTA-CB01
           MOVE CBP001-COMPENSACAO        TO COMPENSACAO-CB01
           MOVE CBP001-NOME-GERENTE       TO NOME-GERENTE-CB01
           MOVE CBP001-ENDERECO           TO ENDERECO-CB01
           MOVE CBP001-TELEFONE           TO TELEFONE-CB01
           MOVE CBP001-CELULAR            TO CELULAR-CB01
           MOVE CBP001-E-MAIL             TO E-MAIL-CB01
           MOVE CBP001-CODIGO-ACESSO      TO CODIGO-ACESSO-CB01
           MOVE CBP001-PROCURADOR         TO PROCURADOR-CB01
           MOVE CBP001-VENCTO-PROCURACAO  TO VENCTO-PROCUR-CB01
           MOVE CBP001-LIMITE             TO LIMITE-CB01
           IF GRAVA-W = 1
              WRITE REG-CBD001 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-CBD001 INVALID KEY
                PERFORM ERRO-GRAVACAO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CBP001-MENSAGEM-ERRO
           MOVE ST-CBD001       TO CBP001-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF CBP001-ORDER = ZEROS
              MOVE SPACES TO TITULAR-CB01
              START CBD001 KEY IS NOT < TITULAR-CB01
                    INVALID KEY MOVE "10" TO ST-CBD001
           ELSE
             MOVE ZEROS TO CODIGO-FORN-CB01
               START CBD001 KEY IS NOT < CODIGO-FORN-CB01
                 INVALID KEY MOVE "10" TO ST-CBD001.
           MOVE SPACES TO CBP001-LINDET.
           MOVE ZEROS TO CBP001-CONT.
           PERFORM UNTIL ST-CBD001 = "10"
              READ CBD001 NEXT RECORD AT END MOVE "10" TO ST-CBD001
              NOT AT END
                ADD 1 TO CBP001-CONT
                MOVE SPACES                TO CBP001-LINDET
                MOVE CODIGO-FORN-CB01      TO CBP001-LINDET(1: 7)
                MOVE TITULAR-CB01          TO CBP001-LINDET(8: 20)
                MOVE NR-BANCO-CB01         TO CBP001-LINDET(29: 5)
                MOVE NOME-BANCO-CB01       TO CBP001-LINDET(34: 21)
                MOVE AGENCIA-CB01          TO CBP001-LINDET(55: 5)
                MOVE NR-CONTA-CB01         TO CBP001-LINDET(60: 16)
                MOVE CIDADE-CB01           TO CBP001-LINDET(76: 4)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CBP001-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "CBP001" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO PAG-W.
           IF CBP001-ORDER = 1
              MOVE ZEROS TO CODIGO-FORN-CB01
              START CBD001 KEY IS NOT < CODIGO-FORN-CB01 INVALID KEY
                           MOVE "10" TO ST-CBD001
           ELSE MOVE SPACES TO TITULAR-CB01
                START CBD001 KEY IS NOT < TITULAR-CB01 INVALID KEY
                           MOVE "10" TO ST-CBD001.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CBD001 = "10"
             READ CBD001 NEXT RECORD AT END MOVE "10" TO ST-CBD001
              NOT AT END
                MOVE SPACES TO LINDET-REL
                MOVE CODIGO-FORN-CB01      TO LINDET-REL(1: 7)
                MOVE TITULAR-CB01          TO LINDET-REL(8: 20)
                MOVE NR-BANCO-CB01         TO LINDET-REL(29: 5)
                MOVE NOME-BANCO-CB01       TO LINDET-REL(34: 21)
                MOVE AGENCIA-CB01          TO LINDET-REL(55: 5)
                MOVE NR-CONTA-CB01         TO LINDET-REL(60: 16)
                MOVE CIDADE-CB01           TO LINDET-REL(76: 4)
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
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CBP001-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CBD001 CGD001 CAD010.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.

