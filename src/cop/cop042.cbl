       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP042.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 13/09/1999.
      *FUNÇÃO: SIMULAÇÃO DE CONTRATO

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX002.
           COPY COPX005.
           COPY COPX042.
           COPY COPX043.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW002.
       COPY COPW005.
       COPY COPW042.
       COPY COPW043.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "COP042.CPB".
           COPY "COP042.CPY".
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
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD042             PIC XX       VALUE SPACES.
           05  ST-COD043             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
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
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  ITEM-W                PIC 9(2)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  CUSTO-TOTAL           PIC 9(8)V99  VALUE ZEROS.
           05  TIPO-GRAVACAO         PIC 9        VALUE ZEROS.
           05  CUSTO-PREVISTO-W      PIC 9(8)V99  VALUE ZEROS.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
      *    COPY "CBDATA1.CPY".
      *    MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "COD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD042" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD042.
           MOVE "COD043" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD043.
           OPEN INPUT COD042 COD043 COD002 COD005.
           IF ST-COD042 = "35"
              CLOSE COD042      OPEN OUTPUT COD042
              CLOSE COD042      OPEN I-O COD042
           END-IF.
           IF ST-COD043 = "35"
              CLOSE COD043      OPEN OUTPUT COD043
              CLOSE COD043      OPEN I-O COD043
           END-IF.

           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD005 <> "00"
              MOVE "ERRO ABERTURA COD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD042 <> "00"
              MOVE "ERRO ABERTURA COD042: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD042 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD043 <> "00"
              MOVE "ERRO ABERTURA COD043: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD043 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM PREENCHER-SELECTION-BOX
               WHEN GS-SAVE-CONTRATO-TRUE
                    PERFORM SALVAR-CONTRATO
               WHEN GS-SAVE-BRINDE-TRUE
                    PERFORM SALVAR-BRINDE
                    PERFORM LIMPAR-DADOS-BRINDE
                    MOVE ITEM-W TO GS-ITEM
               WHEN GS-EXCLUI-CONTRATO-TRUE
                    PERFORM EXCLUI-CONTRATO
               WHEN GS-EXCLUI-BRINDE-TRUE
                    PERFORM EXCLUI-BRINDE
               WHEN GS-CLEAR-CONTRATO-TRUE
                    PERFORM LIMPAR-DADOS-CONTRATO
               WHEN GS-CLEAR-BRINDE-TRUE
                    PERFORM LIMPAR-DADOS-BRINDE
      *        WHEN GS-PRINTER-FLG-TRUE
      *             PERFORM IMPRIME-RELATORIO
               WHEN GS-ACHA-SEQ-TRUE
                    PERFORM ACHAR-SEQUENCIA
               WHEN GS-VERIF-SEQ-TRUE
                    PERFORM VERIFICA-SEQ-CONTRATO
               WHEN GS-VERIFICA-CUSTO-TRUE
                    PERFORM VERIFICA-CUSTO
               WHEN GS-CALCULA-VLR-TOTAL-TRUE
                    PERFORM CALCULA-VLR-TOTAL
               WHEN GS-CARREGA-LIST-BOX-TRUE
                    MOVE GS-SEQ           TO SEQ-CO43
                    MOVE GS-LINDET(1: 2)  TO ITEM-CO43
                    PERFORM CARREGAR-DADOS-BRINDE
               WHEN GS-LE-BRINDE-TRUE
                    PERFORM LE-BRINDE
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM CHAMAR-POP-UP
               WHEN GS-POPUP-SEQ-TRUE
                    PERFORM POPUP-SEQ
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-CARREGA-RELATORIO-TRUE
                    PERFORM CARREGA-RELATORIO
               WHEN GS-LE-PADRAO-TRUE
                    PERFORM LER-PADRAO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       PREENCHER-SELECTION-BOX SECTION.
           MOVE "LIMPAR-SB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-COD005
                      GS-POSICAO
           START COD005 KEY IS NOT LESS PADRAO-CO05 INVALID KEY
                 MOVE "10" TO ST-COD005.

           PERFORM UNTIL ST-COD005 = "10"
                 READ COD005 NEXT AT END
                      MOVE "10" TO ST-COD005
                 NOT AT END
                      MOVE PADRAO-CO05  TO GS-PADRAO
                      MOVE "INSERIR-SB" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.

       POPUP-SEQ SECTION.
           CALL   "COP042T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "COP042T"
           MOVE PASSAR-STRING-1(1: 6) TO GS-SEQ.
       LER-PADRAO SECTION.
           MOVE GS-PADRAO TO PADRAO-CO05.
           READ COD005 INVALID KEY MOVE ZEROS TO PREV-FOTOS-CO05.
           MOVE PREV-FOTOS-CO05  TO GS-PADRAO-QT-FOTO
           COMPUTE GS-QT-FOTOS = GS-PADRAO-QT-FOTO * GS-QT-FORM.
       CARREGA-RELATORIO SECTION.
           MOVE GS-SEQ TO PASSAR-STRING-1(1: 6).
           MOVE IMPRESSORA-W TO PASSAR-STRING-1(8: 2)
           MOVE GS-PERC-VENDA TO PASSAR-STRING-1(12:2)
           CALL   "COP043" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "COP043".
       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-BRINDE
           END-EVALUATE.
       CARREGA-POP-UP-BRINDE SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-CO02.
           START COD002 KEY IS NOT < NOME-CO02 INVALID KEY
                 MOVE "10" TO ST-COD002.
           PERFORM UNTIL ST-COD002 = "10"
              READ COD002 NEXT RECORD AT END MOVE "10" TO ST-COD002
               NOT AT END
                MOVE NOME-CO02(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-COD002
                ELSE
                  MOVE NOME-CO02       TO GS-LINDET1(1: 32)
                  MOVE CODIGO-CO02     TO GS-LINDET1(33: 03)
                  MOVE "INSERE-POP-UP-BRINDE" TO DS-PROCEDURE
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
               MOVE GS-LINDET1(33: 3) TO GS-COD-BRINDE
               MOVE GS-LINDET1(1: 30) TO GS-NOME-BRINDE
           END-EVALUATE.
       ACHAR-SEQUENCIA SECTION.
           MOVE ZEROS TO SEQ-CO42
                         GS-SEQ
           MOVE ALL "9" TO SEQ-CO42
