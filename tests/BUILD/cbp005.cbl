       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP005.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 09/11/1998
      *DESCRIÇÃO: CADASTRO DE CONTAS BANCARIAS CONFORME TIPO LANCTO.
      *           P/ CONTROLE DE LANÇAMENTOS DOBRADOS NO CAIXA, ESSE
      *           CADASTRO SERÁ UTILIZADO NO PROGRAMA DE CONCILIAÇÃO
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CBPX001.
           COPY CBPX004.
           COPY CBPX005.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CBPW001.
       COPY CBPW004.
       COPY CBPW005.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CBP005.CPB".
           COPY "CBP005.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-CBD004             PIC XX       VALUE SPACES.
           05  ST-CBD005             PIC XX       VALUE SPACES.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  TIPO-LCTO-W           PIC 9(2)     VALUE ZEROS.
           05  BANCO1-W              PIC 9(6)     VALUE ZEROS.
           05  DESC-TIPO-LCTO-W      PIC X(30)    VALUE SPACES.
           05  DESC-BANCO1-W         PIC X(30)    VALUE SPACES.
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
           "RELACAO DE CADASTRO DE CONTROLE DE BANCOS     ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "TP     BANCO                          CONTRAPARTIDA-BANCO".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

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
           MOVE "CBD005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD005.
           MOVE "CBD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD004.
           MOVE "CBD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD001.
           OPEN I-O CBD005.
           OPEN INPUT CBD001 CBD004.
           MOVE 1 TO GRAVA-W.
           IF ST-CBD005 = "35"
              CLOSE CBD005      OPEN OUTPUT CBD005
              CLOSE CBD005      OPEN I-O CBD005
           END-IF.
           IF ST-CBD005 <> "00"
              MOVE "ERRO ABERTURA CBD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD004 <> "00"
              MOVE "ERRO ABERTURA CBD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD004 TO GS-MENSAGEM-ERRO(23: 02)
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
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 2) TO GS-TIPO-LCTO
                   MOVE GS-LINDET(5: 6) TO GS-BANCO1
                   PERFORM CARREGAR-DADOS
               WHEN GS-LER-BANCO-TRUE
                   PERFORM LER-BANCO
               WHEN GS-CHAMAR-POPUPBANCO-TRUE
                   PERFORM CHAMAR-POPUPBANCO
               WHEN GS-LER-TIPO-TRUE
                   PERFORM LER-TIPO
               WHEN GS-CHAMAR-POPUPTIPO-TRUE
                   PERFORM CHAMAR-POPUPTIPO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POPUPBANCO SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CBP001T".
           MOVE PASSAR-STRING-1(49: 6) TO GS-BANCOW.
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-BANCOW.
       LER-BANCO SECTION.
           MOVE GS-BANCOW         TO CODIGO-FORN-CB01.
           READ CBD001 INVALID KEY MOVE SPACES TO NOME-BANCO-CB01.
           MOVE NOME-BANCO-CB01   TO GS-DESC-BANCOW.
       CHAMAR-POPUPTIPO  SECTION.
           CALL   "CBP004T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CBP004T".
           MOVE PASSAR-STRING-1(40: 2) TO GS-TIPO-LCTO.
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-TIPO-LCTO.
       LER-TIPO SECTION.
           MOVE GS-TIPO-LCTO   TO CODIGO-CB04.
           READ CBD004 INVALID KEY MOVE SPACES TO HISTORICO-CB04.
           MOVE HISTORICO-CB04     TO GS-DESC-TIPO-LCTO.
       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE GS-TIPO-LCTO TO TIPO-HIST-CB05.
           PERFORM LER-TIPO.
           MOVE GS-BANCO1    TO BANCO-CB05.
           READ CBD005 INVALID KEY INITIALIZE REG-CBD005
                                   MOVE 1 TO GRAVA-W.
           MOVE BANCO-CB05   TO GS-BANCOW.
           PERFORM LER-BANCO.
           MOVE GS-DESC-BANCOW TO GS-DESC-BANCO1.
           MOVE CONTRAPART-BANCO-CB05 TO GS-BANCO2 GS-BANCOW.
           PERFORM LER-BANCO.
           MOVE GS-DESC-BANCOW TO GS-DESC-BANCO2.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CBD005
           MOVE GS-ORDER TO ORDEM-W
           IF GS-SAVE-FLG-TRUE
              MOVE GS-TIPO-LCTO TO TIPO-LCTO-W
              MOVE GS-DESC-TIPO-LCTO TO DESC-TIPO-LCTO-W
              MOVE GS-BANCO1    TO BANCO1-W
              MOVE GS-DESC-BANCO1 TO DESC-BANCO1-W
           ELSE MOVE ZEROS TO TIPO-LCTO-W BANCO1-W
                MOVE SPACES TO DESC-TIPO-LCTO-W DESC-BANCO1-W.
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           MOVE TIPO-LCTO-W TO GS-TIPO-LCTO
           MOVE BANCO1-W    TO GS-BANCO1.
           MOVE DESC-TIPO-LCTO-W TO GS-DESC-TIPO-LCTO
           MOVE DESC-BANCO1-W    TO GS-DESC-BANCO1.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE CBD005.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-TIPO-LCTO          TO TIPO-HIST-CB05.
           MOVE GS-BANCO1             TO BANCO-CB05.
           MOVE GS-BANCO2             TO CONTRAPART-BANCO-CB05.
           WRITE REG-CBD005.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-CBD005       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS     TO TIPO-HIST-CB05 BANCO-CB05.
           START CBD005 KEY IS NOT < CHAVE-CB05 INVALID KEY
                 MOVE "10" TO ST-CBD005.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-CBD005 = "10"
              READ CBD005 NEXT RECORD AT END MOVE "10" TO ST-CBD005
              NOT AT END
                ADD 1 TO GS-CONT
                MOVE SPACES                TO GS-LINDET
                MOVE TIPO-HIST-CB05        TO GS-LINDET(1: 4)
                MOVE BANCO-CB05            TO GS-LINDET(05: 6)
                                              GS-BANCOW
                PERFORM LER-BANCO
                MOVE GS-DESC-BANCOW        TO GS-LINDET(13: 21)
                MOVE CONTRAPART-BANCO-CB05 TO GS-LINDET(33: 6)
                                              GS-BANCOW
                PERFORM LER-BANCO
                MOVE GS-DESC-BANCOW        TO GS-LINDET(42: 20)
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
           MOVE "CBP005" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO PAG-W.
           MOVE ZEROS        TO TIPO-HIST-CB05 BANCO-CB05.
           START CBD005 KEY IS NOT < CHAVE-CB05 INVALID KEY
                 MOVE "10" TO ST-CBD005.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CBD005 = "10"
             READ CBD005 NEXT RECORD AT END MOVE "10" TO ST-CBD005
              NOT AT END
                MOVE SPACES TO LINDET-REL
                MOVE TIPO-HIST-CB05        TO LINDET-REL(1: 7)
                MOVE BANCO-CB05            TO LINDET-REL(08: 6)
                                              GS-BANCOW
                PERFORM LER-BANCO
                MOVE GS-DESC-BANCOW        TO LINDET-REL(16: 21)
                MOVE CONTRAPART-BANCO-CB05 TO LINDET-REL(39: 6)
                                              GS-BANCOW
                PERFORM LER-BANCO
                MOVE GS-DESC-BANCOW        TO LINDET-REL(48: 20)

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
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CBD005 CBD004 CBD001.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
