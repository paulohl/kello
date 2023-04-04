       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP002.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 23/07/1999
      *DESCRIÇÃO: Cadastro de Brindes/mercadorias/serviços
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       PRINTER IS LPRINTER
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY COPX002.
           COPY CGPX001.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CGPW001.
       COPY COPW002.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP002.CPB".
           COPY "COP002.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(3)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    do brinde utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  PASSAR-STRING-1       PIC X(65).
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
           "RELACAO DE CADASTRO DE BRINDES".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "COD DESCRICAO            VALOR/UNITARIO M G.PG FORNE1 FORNE2
      -    " FORNE3 SITUACAO".

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
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "COD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD002.
           OPEN I-O   COD002
           OPEN INPUT CGD001 CAD004.
           MOVE 1 TO GRAVA-W.
           IF ST-COD002 = "35"
              CLOSE COD002      OPEN OUTPUT COD002
              CLOSE COD002      OPEN I-O COD002
           END-IF.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
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
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-ATIVO-INAT-FLG-TRUE
                   PERFORM ATIVO-INAT-RECORD
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
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 3) TO GS-CODIGO
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-FORNECEDOR-TRUE
                    PERFORM LE-FORNECEDOR
               WHEN GS-POPUP-FORNECEDOR-TRUE
                    PERFORM POPUP-FORNECEDOR
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop"

          OPEN INPUT CAD004
          MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
          MOVE "SENHA56"          TO PROGRAMA-CA004
          READ CAD004 INVALID KEY
              DISABLE-OBJECT PB1
          NOT INVALID KEY
              ENABLE-OBJECT PB1.

          CLOSE CAD004.

       LE-FORNECEDOR SECTION.
           MOVE GS-FORNEC-G TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01 TO GS-DESC-G.
       POPUP-FORNECEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-FORNEC-G.
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-G.
       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE GS-CODIGO       TO CODIGO-CO02.
           READ COD002 INVALID KEY INITIALIZE REG-COD002
                                   MOVE 1 TO GRAVA-W
           NOT INVALID KEY
             MOVE NOME-CO02            TO GS-NOME
             MOVE VALOR-CO02           TO GS-VALOR
             EVALUATE MULT-FORM-CO02
               WHEN 1 MOVE "1-Sim" TO GS-MULT-FORM
               WHEN 2 MOVE "2-Não" TO GS-MULT-FORM
             END-EVALUATE
             EVALUATE GERAR-PAGAR-CO02
               WHEN 1 MOVE "1-Sim" TO GS-GERAR-PAGAR
               WHEN 2 MOVE "2-Não" TO GS-GERAR-PAGAR
             END-EVALUATE
             MOVE FORNEC1-CO02         TO GS-FORNEC1 GS-FORNEC-G
             PERFORM LE-FORNECEDOR
             MOVE GS-DESC-G            TO GS-DESC1.
             MOVE FORNEC2-CO02         TO GS-FORNEC2 GS-FORNEC-G
             PERFORM LE-FORNECEDOR
             MOVE GS-DESC-G            TO GS-DESC2.
             MOVE FORNEC3-CO02         TO GS-FORNEC3 GS-FORNEC-G
             PERFORM LE-FORNECEDOR
             MOVE GS-DESC-G            TO GS-DESC3
             MOVE SITUACAO-CO02        TO GS-SITUACAO
             EVALUATE SITUACAO-CO02
                  WHEN 1 MOVE "Ativo"    TO GS-DESC-SITUACAO
                  WHEN 2 MOVE "Inativo"  TO GS-DESC-SITUACAO
             END-EVALUATE.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-COD002
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           MOVE 1       TO GS-SITUACAO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE COD002.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.

       ATIVO-INAT-RECORD SECTION.
           IF SITUACAO-CO02 = 1
              MOVE 2 TO SITUACAO-CO02
           ELSE
              MOVE 1 TO SITUACAO-CO02
           END-IF
           REWRITE REG-COD002
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.

       SALVAR-DADOS SECTION.
           MOVE GS-CODIGO       TO CODIGO-CO02.
           MOVE GS-NOME         TO NOME-CO02
           MOVE GS-MULT-FORM(1: 1)    TO MULT-FORM-CO02
           MOVE GS-GERAR-PAGAR(1: 1)  TO GERAR-PAGAR-CO02
           MOVE GS-FORNEC1      TO FORNEC1-CO02
           MOVE GS-FORNEC2      TO FORNEC2-CO02
           MOVE GS-FORNEC3      TO FORNEC3-CO02
           MOVE GS-VALOR        TO VALOR-CO02
           MOVE GS-SITUACAO     TO SITUACAO-CO02
           IF GRAVA-W = 1
              WRITE REG-COD002 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-COD002 INVALID KEY
                PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-COD002       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF GS-ORDER = ZEROS
              MOVE SPACES TO NOME-CO02
              START COD002 KEY IS NOT < NOME-CO02
                    INVALID KEY MOVE "10" TO ST-COD002
           ELSE
             MOVE ZEROS TO CODIGO-CO02
               START COD002 KEY IS NOT < CODIGO-CO02
                 INVALID KEY MOVE "10" TO ST-COD002.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-COD002 = "10"
              READ COD002 NEXT RECORD AT END MOVE "10" TO ST-COD002
              NOT AT END
                ADD 1 TO GS-CONT
      *         MOVE SPACES TO GS-LINDET
                MOVE CODIGO-CO02          TO GS-LINDET(01: 04)
                MOVE NOME-CO02            TO GS-LINDET(05: 22)
                MOVE VALOR-CO02           TO VALOR-E
                MOVE VALOR-E              TO GS-LINDET(27: 14)
                EVALUATE MULT-FORM-CO02
                  WHEN 1 MOVE "SIM"       TO GS-LINDET(41: 05)
                  WHEN 2 MOVE "NAO"       TO GS-LINDET(41: 05)
                END-EVALUATE
                EVALUATE GERAR-PAGAR-CO02
                  WHEN 1 MOVE "SIM"       TO GS-LINDET(46: 05)
                  WHEN 2 MOVE "NAO"       TO GS-LINDET(46: 05)
                END-EVALUATE
                MOVE FORNEC1-CO02         TO GS-LINDET(51: 07)
                MOVE FORNEC2-CO02         TO GS-LINDET(58: 07)
                MOVE FORNEC3-CO02         TO GS-LINDET(65: 06)
                EVALUATE SITUACAO-CO02
                     WHEN 1 MOVE "Ativo  "  TO GS-LINDET(72:10)
                     WHEN 2 MOVE "Inativo"  TO GS-LINDET(72:10)
                END-EVALUATE
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP002" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           IF GS-ORDER = 1
              MOVE ZEROS TO CODIGO-CO02
              START COD002 KEY IS NOT < CODIGO-CO02 INVALID KEY
                           MOVE "10" TO ST-COD002
           ELSE MOVE SPACES TO NOME-CO02
                START COD002 KEY IS NOT < NOME-CO02 INVALID KEY
                           MOVE "10" TO ST-COD002.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-COD002 = "10"
             READ COD002 NEXT RECORD AT END MOVE "10" TO ST-COD002
              NOT AT END
               MOVE SPACES TO LINDET-REL
                MOVE CODIGO-CO02          TO LINDET-REL(01: 04)
                MOVE NOME-CO02            TO LINDET-REL(05: 22)
                MOVE VALOR-CO02           TO VALOR-E
                MOVE VALOR-E              TO LINDET-REL(27: 14)
                MOVE MULT-FORM-CO02       TO LINDET-REL(41: 02)
                EVALUATE GERAR-PAGAR-CO02
                  WHEN 1 MOVE "SIM"       TO LINDET-REL(43: 05)
                  WHEN 2 MOVE "NAO"       TO LINDET-REL(48: 05)
                END-EVALUATE
                MOVE FORNEC1-CO02         TO LINDET-REL(53: 07)
                MOVE FORNEC2-CO02         TO LINDET-REL(60: 07)
                MOVE FORNEC3-CO02         TO LINDET-REL(67: 06)
                EVALUATE SITUACAO-CO02
                     WHEN 1 MOVE "Ativo  "  TO LINDET-REL(69:10)
                     WHEN 2 MOVE "Inativo"  TO LINDET-REL(69:10)
                END-EVALUATE
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
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO CODIGO-CO02 ULT-CODIGO
           START COD002 KEY IS NOT < CODIGO-CO02 INVALID KEY
                 MOVE "10" TO ST-COD002
           END-START
           PERFORM UNTIL ST-COD002 = "10"
              READ COD002 NEXT RECORD AT END MOVE "10" TO ST-COD002
                NOT AT END
                 MOVE CODIGO-CO02 TO ULT-CODIGO
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
           MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD002 CGD001.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
