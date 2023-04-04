       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP070.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 01/12/1999.
      *FUNÇÃO: Movimento de CRÉDITOS de contrato

      * Normalmente será lançamentos com pagto de fotos antecipados ou
      * crédito de organização de eventos

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY IEPX011.
           COPY COPX040.
           COPY COPX070.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY IEPW011.
       COPY COPW040.
       COPY COPW070.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP070.CPB".
           COPY "COP070.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD070             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
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
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.

           05  CUSTO-PREVISTO-W      PIC 9(8)V99  VALUE ZEROS.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(76)   VALUE
           "CONFERENCIA DO MOVIMENTO DE CREDITOS".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(130)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONT IT CUR TU DESCRICAO                             DATA-VE
      -    "CTO VALOR-PREVIST DATA-SOLIC S R DATA-PAGTO    VALOR-PAGO  D
      -    "IAS ".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD070" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD070.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           OPEN I-O COD070.
           OPEN INPUT IED011 COD040.
           IF ST-COD070 = "35"
              CLOSE COD070      OPEN OUTPUT COD070
              CLOSE COD070      OPEN I-O COD070
           END-IF.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD070 <> "00"
              MOVE "ERRO ABERTURA COD070: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD070 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1 PERFORM REGRAVA-DADOS
                   ELSE PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4)  TO NR-CONTRATO-CO70
                   MOVE GS-LINDET(6: 2)  TO ITEM-CO70
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CURSO-TRUE
                   PERFORM LE-CURSO
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-CALCULA-DIAS-PRAZ-TRUE
                    PERFORM CALCULA-PRAZO-MEDIO
               WHEN GS-VERIFICAR-CONTRATO-TRUE
                    PERFORM VERIFICA-CONTRATO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICA-CONTRATO SECTION.
           MOVE GS-CONTRATO            TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE "Número do Contrato Não Cadastrado no (COP040)"
                  TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-CURSO
           END-EVALUATE.
       CARREGA-POP-UP-CURSO SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-IE11
           START IED011 KEY IS NOT < NOME-IE11 INVALID KEY
                 MOVE "10" TO ST-IED011.
           PERFORM UNTIL ST-IED011 = "10"
              READ IED011 NEXT RECORD AT END MOVE "10" TO ST-IED011
               NOT AT END
                MOVE NOME-IE11(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-IED011
                ELSE
                  MOVE NOME-IE11       TO GS-LINDET1(1: 42)
                  MOVE CODIGO-IE11     TO GS-LINDET1(43: 03)
                  MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-LINDET1(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE GS-LINDET1(I: 1)
                          TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.
       ITEM-SELECIONADO SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1
               MOVE GS-LINDET1(43: 3) TO GS-CURSO
               MOVE GS-LINDET1(1: 40) TO GS-NOME-CURSO
           END-EVALUATE.
      *----------------------------------------------------------------
       EXCLUI SECTION.
           DELETE COD070.
           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
       LE-CURSO SECTION.
           MOVE GS-CURSO           TO CODIGO-IE11.
           READ IED011 INVALID KEY MOVE "******" TO NOME-IE11.
           MOVE NOME-IE11          TO GS-NOME-CURSO.
       CARREGAR-DADOS SECTION.
           START COD070 KEY IS = CHAVE-CO70 INVALID KEY CONTINUE.
           READ COD070 INVALID KEY INITIALIZE REG-COD070.
           MOVE NR-CONTRATO-CO70     TO  GS-CONTRATO
           MOVE ITEM-CO70            TO  GS-NR-ITEM
           MOVE DESCRICAO-CO70       TO  GS-DESCRICAO
           MOVE CURSO-CO70           TO  GS-CURSO CODIGO-IE11
           READ IED011 INVALID KEY MOVE "*****" TO NOME-IE11.
           MOVE NOME-IE11            TO  GS-NOME-CURSO.
           MOVE TURMA-CO70           TO  GS-TURMA
           MOVE VALOR-PREVISTO-CO70  TO  GS-VALOR-PREVISTO
           MOVE DATA-VENCTO-CO70     TO  DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO  GS-DATA-VENCTO
           MOVE DATA-MOVTO-CO70    TO  GS-DATA-MOVTO
           EVALUATE SUSP-PREV-DEF-CO70
             WHEN 0 MOVE "0-Previsto  "  TO GS-SUSP-PREV-DEF
             WHEN 1 MOVE "1-Definitivo"  TO GS-SUSP-PREV-DEF
             WHEN 2 MOVE "2-Suspenso  "  TO GS-SUSP-PREV-DEF
           END-EVALUATE.
           MOVE VALOR-RECTO-CO70     TO  GS-VALOR-RECTO
           MOVE DATA-RECTO-CO70      TO  GS-DATA-RECTO
           EVALUATE REALIZADO-CO70
             WHEN 0 MOVE "0-Não"     TO  GS-REALIZADO
             WHEN 1 MOVE "1-Sim"     TO  GS-REALIZADO
           END-EVALUATE.
           MOVE DIAS-PRAZO-CO70      TO  GS-PRAZO-MEDIO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-COD070
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO            TO NR-CONTRATO-CO70
           MOVE GS-NR-ITEM             TO ITEM-CO70
           MOVE GS-DESCRICAO           TO DESCRICAO-CO70
           MOVE GS-CURSO               TO CURSO-CO70
           MOVE GS-TURMA               TO TURMA-CO70
           MOVE GS-VALOR-PREVISTO      TO VALOR-PREVISTO-CO70
           MOVE GS-DATA-VENCTO         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV               TO DATA-VENCTO-CO70
           MOVE GS-DATA-MOVTO          TO DATA-MOVTO-CO70
           MOVE GS-SUSP-PREV-DEF(1: 1) TO SUSP-PREV-DEF-CO70
           MOVE GS-VALOR-RECTO         TO VALOR-RECTO-CO70
           MOVE GS-DATA-RECTO          TO DATA-RECTO-CO70
           MOVE GS-REALIZADO(1: 1)     TO REALIZADO-CO70
           PERFORM CALCULA-PRAZO-MEDIO.
           MOVE GS-PRAZO-MEDIO         TO DIAS-PRAZO-CO70.
       CALCULA-PRAZO-MEDIO SECTION.
           IF GS-DATA-RECTO = ZEROS
              MOVE GS-DATA-VENCTO     TO GRTIME-DATE
           ELSE MOVE GS-DATA-RECTO    TO GRTIME-DATE.
           MOVE GRTIME-DATE TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO GRTIME-DATE
           MOVE GS-CONTRATO           TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE ZEROS TO DATA-PREV-VENDA-CO40.
           MOVE DATA-PREV-VENDA-CO40  TO GRTIME-DATE-FINAL.
           MOVE GRTIME-DATE-FINAL     TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO GRTIME-DATE-FINAL
           IF GRTIME-DATE > GRTIME-DATE-FINAL
              MOVE "EXIBE-ERRO-DATA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
              MOVE 2                     TO GRTIME-TYPE
              MOVE 3                     TO GRTIME-FUNCTION
              CALL "GRTIME" USING PARAMETROS-GRTIME
              MOVE GRTIME-DAYS-FINAL     TO GS-PRAZO-MEDIO.

       GRAVA-DADOS SECTION.
           MOVE ZEROS TO ST-COD070.
           PERFORM UNTIL ST-COD070 = "10"
             WRITE REG-COD070 INVALID KEY
                 ADD 1 TO ITEM-CO70
               NOT INVALID KEY
                 MOVE "10" TO ST-COD070.
           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           REWRITE REG-COD070 INVALID KEY
                 MOVE "Erro Regravacao COD070" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD070 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-CONTRATO     TO NR-CONTRATO-CO70.
           MOVE ZEROS           TO ITEM-CO70 GS-NR-ITEM.
           START COD070 KEY IS NOT < CHAVE-CO70
                    INVALID KEY MOVE "10" TO ST-COD070.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-COD070 = "10"
              READ COD070 NEXT RECORD AT END MOVE "10" TO ST-COD070
              NOT AT END
                IF NR-CONTRATO-CO70 <> GS-CONTRATO
                   MOVE "10" TO ST-COD070
                ELSE
                   PERFORM MOVER-DADOS-LISTA
                   MOVE ITEM-CO70     TO GS-NR-ITEM
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
           ADD 1 TO GS-NR-ITEM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES              TO GS-LINDET
           MOVE NR-CONTRATO-CO70    TO GS-LINDET(1: 5)
           MOVE ITEM-CO70           TO GS-LINDET(6: 3)
           MOVE CURSO-CO70          TO GS-LINDET(9: 4)
           MOVE TURMA-CO70          TO GS-LINDET(13: 3)
           MOVE DESCRICAO-CO70      TO GS-LINDET(16: 28)
           MOVE VALOR-PREVISTO-CO70 TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET(45: 14)
           MOVE DATA-VENCTO-CO70    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV            TO DATA-E
           MOVE DATA-E              TO GS-LINDET(59: 11)
           MOVE DATA-MOVTO-CO70     TO DATA-E
           MOVE DATA-E              TO GS-LINDET(70: 11)
           MOVE SUSP-PREV-DEF-CO70  TO GS-LINDET(81: 2)
           MOVE REALIZADO-CO70      TO GS-LINDET(83: 2)
           MOVE DATA-RECTO-CO70     TO DATA-E
           MOVE DATA-E              TO GS-LINDET(85: 11)
           MOVE VALOR-RECTO-CO70    TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET(96: 13).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP070" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *----------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-CONTRATO    TO NR-CONTRATO-CO70.
           MOVE ZEROS          TO ITEM-CO70.
           START COD070 KEY IS = CHAVE-CO70 INVALID KEY
                 MOVE "10" TO ST-COD070.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-COD070 = "10"
             READ COD070 NEXT RECORD AT END MOVE "10" TO ST-COD070
              NOT AT END
                IF NR-CONTRATO-CO70 <> GS-CONTRATO
                         MOVE "10" TO ST-COD070
                ELSE
                  MOVE NR-CONTRATO-CO70    TO LINDET-REL(1: 5)
                  MOVE ITEM-CO70           TO LINDET-REL(6: 3)
                  MOVE CURSO-CO70          TO LINDET-REL(9: 4)
                  MOVE TURMA-CO70          TO LINDET-REL(13: 3)
                  MOVE DESCRICAO-CO70      TO LINDET-REL(16: 37)
                  MOVE DATA-VENCTO-CO70    TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV            TO DATA-E
                  MOVE DATA-E              TO LINDET-REL(54: 11)
                  MOVE VALOR-PREVISTO-CO70 TO VALOR-E
                  MOVE VALOR-E             TO LINDET-REL(65: 14)
                  MOVE DATA-MOVTO-CO70   TO DATA-E
                  MOVE DATA-E              TO LINDET-REL(79: 11)
                  MOVE SUSP-PREV-DEF-CO70  TO LINDET-REL(90: 2)
                  MOVE REALIZADO-CO70      TO LINDET-REL(92: 2)
                  MOVE DATA-RECTO-CO70     TO DATA-E
                  MOVE DATA-E              TO LINDET-REL(94: 11)
                  MOVE VALOR-RECTO-CO70     TO VALOR-E
                  MOVE VALOR-E             TO LINDET-REL(105: 14)
                  MOVE DIAS-PRAZO-CO70     TO LINDET-REL(119: 6)
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
                END-IF
             END-READ
           END-PERFORM.

           COPY DESCONDENSA.

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

       exibir-mensagem section.
           move 1 to gs-flag-critica
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 COD070 IED011.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
