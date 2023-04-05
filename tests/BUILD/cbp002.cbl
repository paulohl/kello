       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP002.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 19/10/1998
      *DESCRIÇÃO: CADASTRO DE CONTAS DE TALÃO DE CHEQUES
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CBPX002.
           COPY CBPX100.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CBPW002.
       COPY CBPW100.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CBP002.CPB".
           COPY "CBP002.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(65).
       01  VARIAVEIS.
           05  ST-CBD002             PIC XX       VALUE SPACES.
           05  ST-CBD100             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
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
           05  DATA-E                PIC 99/99/9999.
           05  DATA-TALAO1            PIC 9(8)     VALUE ZEROS.
           05  QT-FOLHA              PIC 9999     VALUE ZEROS.
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
           "RELACAO DE REGISTRO DE TALOES DE CHEQUES      ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "FORNEC  NR.FOLHAS  FOLHA-INIC  FOLHA-FINAL        DATA".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CBP002-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CBP002-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CBP002-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE CBP002-VERSION-NO  TO DS-VERSION-NO.
           MOVE DATA-INV           TO DATA-TALAO1.
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CBD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD002.
           MOVE "CBD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD100.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           OPEN I-O CBD002 CBD100.
           OPEN INPUT CGD001.
           IF ST-CBD002 = "35"
              CLOSE CBD002      OPEN OUTPUT CBD002
              CLOSE CBD002      OPEN I-O CBD002
           END-IF.
           IF ST-CBD100 = "35"
              CLOSE CBD100      OPEN OUTPUT CBD100
              CLOSE CBD100      OPEN I-O CBD100
           END-IF.
           IF ST-CBD002 <> "00"
              MOVE "ERRO ABERTURA CBD002: "  TO CBP002-MENSAGEM-ERRO
              MOVE ST-CBD002 TO CBP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD100 <> "00"
              MOVE "ERRO ABERTURA CBD100: "  TO CBP002-MENSAGEM-ERRO
              MOVE ST-CBD100 TO CBP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CBP002-MENSAGEM-ERRO
              MOVE ST-CBD002 TO CBP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CBP002-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                MOVE 1 TO CBP002-ORDER
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CBP002-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CBP002-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP002-LOAD-FLG-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
      *        WHEN CBP002-EXCLUI-FLG-TRUE
      *            PERFORM EXCLUI-RECORD
      *            PERFORM CARREGA-ULTIMOS
               WHEN CBP002-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN CBP002-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CBP002-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CBP002-CARREGA-LIST-BOX-TRUE
                   MOVE CBP002-LINDET(1: 6) TO CBP002-CODIGO-FORN
                   MOVE CBP002-LINDET(19: 6) TO CBP002-FL-INICIAL
      *            PERFORM CARREGAR-DADOS
               WHEN CBP002-LER-FORNEC-TRUE
                   PERFORM LER-FORNECEDOR
               WHEN CBP002-CHAMAR-PROGRAMA-TRUE
                   PERFORM CHAMAR-PROGRAMA
               WHEN CBP002-CONF-NR-FOLHA-TRUE
                   PERFORM CONFERE-FOLHA

           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CONFERE-FOLHA SECTION.
           COMPUTE QT-FOLHA = (CBP002-FL-FINAL - CBP002-FL-INICIAL) + 1.
           IF QT-FOLHA NOT = CBP002-NR-FOLHAS
                   MOVE 1 TO CBP002-ERRO-QT-FOLHA
           ELSE MOVE 0 TO CBP002-ERRO-QT-FOLHA.
       CHAMAR-PROGRAMA SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING1.
           CANCEL "CBP001T".
           MOVE PASSAR-STRING1(49: 6) TO CBP002-CODIGO-FORN.
           MOVE PASSAR-STRING1(1: 30) TO CBP002-NOME-FORN.
       LER-FORNECEDOR SECTION.
           MOVE CBP002-CODIGO-FORN TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO CBP002-NOME-FORN.
      *CARREGAR-DADOS SECTION.
      *    MOVE CBP002-CODIGO-FORN TO CODIGO-FORN-CB02.
      *    MOVE CBP002-FL-INICIAL  TO FL-INICIAL-CB02.
      *    READ CBD002 INVALID KEY
      *         INITIALIZE REG-CBD002.
      *    PERFORM LER-FORNECEDOR.
      *    MOVE FL-FINAL-CB02      TO CBP002-FL-FINAL
      *    MOVE NR-FOLHAS-CB02     TO CBP002-NR-FOLHAS.
      *    MOVE DATA-CB02          TO CBP002-DATA.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CBD002
           MOVE CBP002-ORDER TO ORDEM-W
           INITIALIZE CBP002-DATA-BLOCK
           MOVE ORDEM-W TO CBP002-ORDER
           MOVE DATA-TALAO1 TO CBP002-DATA.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
      *EXCLUI-RECORD SECTION.
      *    DELETE CBD002.
      *    PERFORM LIMPAR-DADOS.
       SALVAR-DADOS SECTION.
           MOVE CBP002-CODIGO-FORN        TO CODIGO-FORN-CB100.
           MOVE CBP002-FL-INICIAL         TO NR-CHEQUE-CB100.
           START CBD100 KEY IS NOT < CHAVE-CB100 INVALID KEY
                   PERFORM SALVAR-DADOS1
              NOT INVALID KEY
              READ CBD100 NEXT RECORD AT END PERFORM SALVAR-DADOS1
                NOT AT END
                IF CODIGO-FORN-CB100 = CBP002-CODIGO-FORN AND
                  NR-CHEQUE-CB100 NOT > CBP002-FL-FINAL
                   MOVE "APRESENTA-ERRO-INCL" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                ELSE PERFORM SALVAR-DADOS1
              END-READ
           END-START.
       SALVAR-DADOS1 SECTION.
           MOVE CBP002-CODIGO-FORN        TO CODIGO-FORN-CB02.
           MOVE CBP002-FL-INICIAL         TO FL-INICIAL-CB02.
           MOVE CBP002-FL-FINAL           TO FL-FINAL-CB02.
           MOVE CBP002-DATA               TO DATA-CB02.
           MOVE CBP002-NR-FOLHAS          TO NR-FOLHAS-CB02.
           MOVE DATA-TALAO1               TO DATA-CB02.
           WRITE REG-CBD002 INVALID KEY
                   PERFORM ERRO-GRAVACAO.

           PERFORM VARYING CBP002-FL-INICIAL FROM CBP002-FL-INICIAL
                  BY 1 UNTIL CBP002-FL-INICIAL > CBP002-FL-FINAL
                     INITIALIZE REG-CBD100
                     MOVE CBP002-CODIGO-FORN  TO CODIGO-FORN-CB100
                     MOVE CBP002-FL-INICIAL   TO NR-CHEQUE-CB100
                     MOVE 1                   TO SITUACAO-CB100
                     WRITE REG-CBD100
                     END-WRITE
           END-PERFORM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CBP002-MENSAGEM-ERRO
           MOVE ST-CBD002       TO CBP002-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE CBP002-CODIGO-FORN     TO CODIGO-FORN-CB02.
           MOVE ZEROS                  TO FL-INICIAL-CB02.
           START CBD002 KEY IS NOT < CHAVE-CB02 INVALID KEY
                 MOVE "10" TO ST-CBD002.
           MOVE ZEROS TO CBP002-CONT.
           PERFORM UNTIL ST-CBD002 = "10"
              READ CBD002 NEXT RECORD AT END MOVE "10" TO ST-CBD002
              NOT AT END
               IF CODIGO-FORN-CB02 <> CBP002-CODIGO-FORN
                      MOVE "10" TO ST-CBD002
               ELSE
                ADD 1 TO CBP002-CONT
                MOVE SPACES                TO CBP002-LINDET
                MOVE CODIGO-FORN-CB02      TO CBP002-LINDET(1: 7)
                MOVE NR-FOLHAS-CB02        TO CBP002-LINDET(08: 5)
                MOVE FL-INICIAL-CB02       TO CBP002-LINDET(19: 6)
                MOVE FL-FINAL-CB02         TO CBP002-LINDET(31: 6)
                MOVE DATA-CB02             TO DATA-E
                MOVE DATA-E                TO CBP002-LINDET(45: 10)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
               END-IF
              END-READ
           END-PERFORM.
           MOVE DATA-TALAO1           TO CBP002-DATA.
       CLEAR-FLAGS SECTION.
           INITIALIZE CBP002-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "CBP002" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO PAG-W.
           MOVE CBP002-BANCO TO CODIGO-FORN-CB02.
           MOVE ZEROS        TO FL-INICIAL-CB02.
           START CBD002 KEY IS NOT < CHAVE-CB02 INVALID KEY
                 MOVE "10" TO ST-CBD002.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CBD002 = "10"
             READ CBD002 NEXT RECORD AT END MOVE "10" TO ST-CBD002
              NOT AT END
               IF CODIGO-FORN-CB02 <> CBP002-BANCO
                  MOVE "10" TO ST-CBD002
               ELSE
                MOVE SPACES TO LINDET-REL
                MOVE CODIGO-FORN-CB02      TO LINDET-REL(1: 7)
                MOVE NR-FOLHAS-CB02        TO LINDET-REL(8: 11)
                MOVE FL-INICIAL-CB02       TO LINDET-REL(19: 10)
                MOVE FL-FINAL-CB02         TO LINDET-REL(31: 11)
                MOVE DATA-CB02             TO DATA-E
                MOVE DATA-E                TO LINDET-REL(45: 10)
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
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CBP002-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CBD002 CBD100 CGD001.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
