       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP045.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 02/06/2011.
      *FUNÇÃO: Cadastro de Produtos x Contrato


       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX004.

           COPY COPX040.

           COPY COPX045.

           COPY CADPRO.SEL.

           COPY CADMOD.SEL.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW004.

           COPY COPW040.

           COPY COPW045.

           COPY CADPRO.FD.

           COPY CADMOD.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP045.CPB".
           COPY "COP045.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD045             PIC XX       VALUE SPACES.
           05  ST-CADPRO             PIC XX       VALUE SPACES.
           05  ST-CADMOD             PIC XX       VALUE SPACES.
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
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  AUX-CONTRATO          PIC 9(04).
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
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
           05  QTDE-FORM-W           PIC 9(4)     VALUE ZEROS.
           05  QTDE-TURMAS-W         PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.

       01 mensagem                 pic x(200).
       01 tipo-msg                 pic x(01).
       01 resp-msg                 pic x(01).

       77 janelaPrincipal          object reference.
       77 handle8                  pic 9(08) comp-x value zeros.
       77 wHandle                  pic 9(09) comp-5 value zeros.

       01 MASC-QTDE                PIC Z.ZZ9.
       01 MASC-CUSTO               PIC Z.ZZ9,999.
       01 MASC-VENDA               PIC ZZZ.ZZ9,999.

       01  CAB01.
           05  EMPRESA-REL         PIC X(91)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(79)   VALUE
           "CONFERENCIA CONTRATO X PRODUTO".
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(111)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(111)  VALUE
           "CONT PRODUTO                                        MODELO
      -    "                         QTDE VLR.CUSTO   VLR.VENDA".

       01  LINDET.
           05  LINDET-REL          PIC X(111)  VALUE SPACES.
       LINKAGE SECTION.

           COPY "PARAMETR".

       01  STRING-1          PIC X(65) VALUE SPACES.
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD045" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD045.
           MOVE "CADPRO" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADPRO.
           MOVE "CADMOD" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADMOD.
           OPEN I-O   COD045 COD040 CAD004 CADPRO CADMOD
           CLOSE      CAD004 CADPRO COD045 COD040 CADMOD
           OPEN INPUT CAD004 COD040 COD045 CADPRO CADMOD.

           IF ST-COD045 = "35"
              CLOSE COD045      OPEN OUTPUT COD045
              CLOSE COD045      OPEN I-O    COD045
           END-IF.
           IF ST-COD040 = "35"
              CLOSE COD040      OPEN OUTPUT COD040
              CLOSE COD040      OPEN I-O    COD040
           END-IF.
           IF ST-CADPRO = "35"
              CLOSE CADPRO      OPEN OUTPUT CADPRO
              CLOSE CADPRO      OPEN I-O    CADPRO
           END-IF.
           IF ST-CADMOD = "35"
              CLOSE CADMOD      OPEN OUTPUT CADMOD
              CLOSE CADMOD      OPEN I-O    CADMOD
           END-IF.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD045 <> "00"
              MOVE "ERRO ABERTURA COD045: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD045 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADPRO <> "00"
              MOVE "ERRO ABERTURA CADPRO: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADPRO TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADMOD <> "00"
              MOVE "ERRO ABERTURA CADMOD: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADMOD TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1
                      PERFORM REGRAVA-DADOS
                   ELSE
                      PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
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
               WHEN GS-FINALIZA-INICIAL-TRUE
                    PERFORM FINALIZA-INICIAL
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4)  TO NR-CONTRATO-CO45
                   MOVE GS-LINDET(6: 4)  TO PRODUTO-CO45
                   MOVE GS-LINDET(53:4)  TO MODELO-CO45
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-PRODUTO-TRUE
                   PERFORM LE-PRODUTO
               WHEN GS-CHAMAR-PRODUTO-TRUE
                   PERFORM CHAMAR-PRODUTO
               WHEN GS-LE-MODELO-TRUE
                   PERFORM LE-MODELO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop"

          move cab04     to gs-linha-cab
          move function numval(string-1) to gs-acp-contrato
          refresh-object principal.

       FINALIZA-INICIAL SECTION.
           MOVE AUX-CONTRATO TO NR-CONTRATO-CO40
           READ COD040 NOT INVALID KEY
               MOVE "N" TO FLAG-GRAVA-CO40
               REWRITE REG-COD040.

       EXCLUI SECTION.
           CLOSE      COD045
           OPEN I-O   COD045

           DELETE COD045

           CLOSE      COD045
           OPEN INPUT COD045

           PERFORM LIMPAR-DADOS
           PERFORM CARREGA-ULTIMOS.

       LE-PRODUTO SECTION.
           MOVE GS-ACP-PRODUTO     TO CADPRO-CODIGO
           MOVE GS-ACP-MODELO      TO CADPRO-MODELO
           READ CADPRO INVALID KEY
                MOVE "Produto Informado Inválido" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE CADPRO-NOME   TO GS-DESC-PRODUTO
                REFRESH-OBJECT PRINCIPAL.

       CHAMAR-PRODUTO SECTION.
           CALL   "CONPRO" USING PARAMETROS-W
                                 GS-ACP-PRODUTO
           CANCEL "CONPRO"
           MOVE GS-ACP-PRODUTO     TO CADPRO-CODIGO
           MOVE GS-ACP-MODELO      TO CADPRO-MODELO
           READ CADPRO INVALID KEY
                MOVE "Produto Informado Inválido" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE CADPRO-NOME   TO GS-DESC-PRODUTO
                MOVE CADPRO-MODELO TO GS-ACP-MODELO
                                      CADMOD-CODIGO
                READ CADMOD INVALID KEY
                     MOVE SPACES   TO CADMOD-NOME
                END-READ
                MOVE CADMOD-NOME   TO GS-DESC-MODELO
           END-READ
           REFRESH-OBJECT PRINCIPAL.

       LE-MODELO SECTION.
           MOVE GS-ACP-PRODUTO     TO CADPRO-CODIGO
           MOVE GS-ACP-MODELO      TO CADPRO-MODELO
           READ CADPRO INVALID KEY
                MOVE "Produto Informado Inválido" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE CADPRO-NOME   TO GS-DESC-PRODUTO
                MOVE CADPRO-MODELO TO GS-ACP-MODELO
                                      CADMOD-CODIGO
                READ CADMOD INVALID KEY
                     MOVE SPACES   TO CADMOD-NOME
                END-READ
                MOVE CADMOD-NOME   TO GS-DESC-MODELO
           END-READ

           REFRESH-OBJECT PRINCIPAL.

       CARREGAR-DADOS SECTION.
           START COD045 KEY IS = CHAVE-CO45 INVALID KEY
                 CONTINUE.

           READ COD045 INVALID KEY
                INITIALIZE REG-COD045.

           MOVE NR-CONTRATO-CO45     TO GS-ACP-CONTRATO
           MOVE PRODUTO-CO45         TO GS-ACP-PRODUTO
                                        CADPRO-CODIGO
           READ CADPRO INVALID KEY
                MOVE "**********"    TO CADPRO-NOME
           END-READ
           MOVE CADPRO-NOME          TO GS-DESC-PRODUTO

           MOVE MODELO-CO45          TO GS-ACP-MODELO
                                        CADMOD-CODIGO
           READ CADMOD INVALID KEY
                MOVE "*********"     TO CADMOD-NOME
           END-READ
           MOVE CADMOD-NOME          TO GS-DESC-MODELO

           MOVE QTDE-PRODUTO-CO45    TO GS-ACP-QTDE
           MOVE CUSTO-CO45           TO GS-ACP-CUSTO-UNIT
           MOVE VENDA-CO45           TO GS-ACP-VENDA-UNIT

           REFRESH-OBJECT PRINCIPAL.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           MOVE GS-AUX-CONTRATO      TO AUX-CONTRATO
           INITIALIZE REG-COD045
           INITIALIZE GS-DATA-BLOCK
           MOVE AUX-CONTRATO         TO GS-AUX-CONTRATO
                                        GS-ACP-CONTRATO
           MOVE CAB04                TO GS-LINHA-CAB
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

       SALVAR-DADOS SECTION.
           MOVE GS-ACP-CONTRATO         TO NR-CONTRATO-CO45
           MOVE GS-ACP-PRODUTO          TO PRODUTO-CO45
           MOVE GS-ACP-MODELO           TO MODELO-CO45
           MOVE GS-ACP-QTDE             TO QTDE-PRODUTO-CO45
           MOVE GS-ACP-CUSTO-UNIT       TO CUSTO-CO45
           MOVE GS-ACP-VENDA-UNIT       TO VENDA-CO45.

       GRAVA-DADOS SECTION.
           CLOSE      COD045
           OPEN I-O   COD045

           WRITE REG-COD045 INVALID KEY
                 MOVE "Erro gravacao COD045" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD045 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           END-WRITE

           CLOSE      COD045
           OPEN INPUT COD045

           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           CLOSE      COD045
           OPEN I-O   COD045

           REWRITE REG-COD045 INVALID KEY
                 MOVE "Erro Regravacao COD045" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD045 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           END-REWRITE

           CLOSE      COD045
           OPEN INPUT COD045

           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO"  TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-COD045
           MOVE GS-ACP-CONTRATO  TO NR-CONTRATO-CO45.
           START COD045 KEY IS NOT < CHAVE-CO45 INVALID KEY
                 MOVE "10" TO ST-COD045.

           MOVE SPACES           TO GS-LINDET.
           PERFORM UNTIL ST-COD045 = "10"
              READ COD045 NEXT RECORD AT END
                   MOVE "10"     TO ST-COD045
              NOT AT END
                   IF NR-CONTRATO-CO45 <> GS-ACP-CONTRATO
                      MOVE "10" TO ST-COD045
                   ELSE
                      PERFORM MOVER-DADOS-LISTA
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
              END-READ
           END-PERFORM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE NR-CONTRATO-CO45   TO GS-LINDET(1: 5)
           MOVE PRODUTO-CO45       TO GS-LINDET(6: 4)
                                      CADPRO-CODIGO
           MOVE MODELO-CO45        TO CADPRO-MODELO
           READ CADPRO INVALID KEY
                MOVE "*********"   TO CADPRO-NOME
           END-READ
           MOVE CADPRO-NOME        TO GS-LINDET(11:40)
           MOVE MODELO-CO45        TO GS-LINDET(53:4)
                                      CADMOD-CODIGO
           READ CADMOD INVALID KEY
                MOVE "**********"  TO CADMOD-NOME
           END-READ
           MOVE CADMOD-NOME        TO GS-LINDET(58:26)

           MOVE QTDE-PRODUTO-CO45  TO MASC-QTDE
           MOVE MASC-QTDE          TO GS-LINDET(85:5)
           MOVE CUSTO-CO45         TO MASC-CUSTO
           MOVE MASC-CUSTO         TO GS-LINDET(91:9)
           MOVE VENDA-CO45         TO MASC-VENDA
           MOVE MASC-VENDA         TO GS-LINDET(101:11).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP045" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           INITIALIZE REG-COD045
           MOVE GS-ACP-CONTRATO    TO NR-CONTRATO-CO45
           START COD045 KEY IS = CHAVE-CO45 INVALID KEY
                 MOVE "10"         TO ST-COD045.

           MOVE ZEROS              TO LIN
           PERFORM CABECALHO
           PERFORM UNTIL ST-COD045 = "10"
                 READ COD045 NEXT RECORD AT END
                      MOVE "10" TO ST-COD045
                 NOT AT END
                      IF NR-CONTRATO-CO45 <> GS-ACP-CONTRATO
                         MOVE "10" TO ST-COD045
                      ELSE
                         MOVE SPACES             TO LINDET-REL
                         MOVE NR-CONTRATO-CO45   TO LINDET-REL(1: 5)
                         MOVE PRODUTO-CO45       TO LINDET-REL(6: 4)
                                                    CADPRO-CODIGO
                         MOVE MODELO-CO45        TO CADPRO-MODELO
                         READ CADPRO INVALID KEY
                              MOVE "*********"   TO CADPRO-NOME
                         END-READ
                         MOVE CADPRO-NOME        TO LINDET-REL(11:40)
                         MOVE MODELO-CO45        TO LINDET-REL(53:4)
                                                    CADMOD-CODIGO
                         READ CADMOD INVALID KEY
                              MOVE "**********"  TO CADMOD-NOME
                         END-READ
                         MOVE CADMOD-NOME        TO LINDET-REL(58:26)

                         MOVE QTDE-PRODUTO-CO45  TO MASC-QTDE
                         MOVE MASC-QTDE          TO LINDET-REL(85:5)
                         MOVE CUSTO-CO45         TO MASC-CUSTO
                         MOVE MASC-CUSTO         TO LINDET-REL(91:9)
                         MOVE VENDA-CO45         TO MASC-VENDA
                         MOVE MASC-VENDA         TO LINDET-REL(101:11)
                         WRITE REG-RELAT FROM LINDET
                         ADD 1 TO LIN
                         IF LIN > 60
                            PERFORM CABECALHO
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
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to gs-flag-critica
           move spaces to mensagem.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 COD045 CADPRO CAD004 CADMOD
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
