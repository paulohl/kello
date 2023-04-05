       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP001.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 21/06/2000
      *FUNÇÃO: Cadastro de Contratos P/ MONTAGEM


       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass"
           AListview          is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX040.
           COPY COPX045.
           COPY CADPRO.SEL.
           COPY CADMOD.SEL.
           COPY MTPX001.
           COPY MTPX001P.
           COPY CEAPX010.
           COPY CAPX010.
           COPY PARX001.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
           COPY COPW040.
           COPY COPW045.
           COPY CADPRO.FD.
           COPY CADMOD.FD.
           COPY CAPW010.
           COPY MTPW001.
           COPY MTPW001P.
           COPY CEAPW010.
           COPY PARW001.
           COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP001.CPB".
           COPY "MTP001.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD045             PIC XX       VALUE SPACES.
           05  ST-CADPRO             PIC XX       VALUE SPACES.
           05  ST-CADMOD             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-MTD001             PIC XX       VALUE SPACES.
           05  ST-MTD001P            PIC XX       VALUE SPACES.
           05  ST-CEAD010            PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  NR-CONTRATO-W         PIC 9(4)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZ.ZZZ.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  LIN                   PIC 9(02).

           05  WS-STATUS-REVENDIDO   PIC 9(02)    VALUE ZEROS.
           05  WS-STATUS-ANALISE     PIC 9(02)    VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  masc-valor            pic zzz.zz9,999 blank when zeros.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.
       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 UMITEM                       object reference.
       77 UMOBJETO                     object reference.

       01 lnktabelaPro.
          02 lnkobjetoscolPro          object reference occurs 99 times.
       01 lnktabelaColPro.
          02 lnkcolunasPro pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnkusu.
          copy usuario.cpy.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU         PIC 9(04).
             10 WS-MES-CPU         PIC 9(02).
             10 WS-DIA-CPU         PIC 9(02).
          05 FILLER                PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).


       01 indice                   pic 9(02).
       01 wssize                   pic 9(09) comp-5 value zeros.
       01 wsIndice                 pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "CONFERENCIA DO CADASTRO DE CONTRATO P/ MONTAGEM".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  LINDET.
           05  LINDET-REL          PIC X(132)  VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario
           MOVE "COD040"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD045"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD045.
           MOVE "CADPRO"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADPRO.
           MOVE "CADMOD"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADMOD.
           MOVE "MTD001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD001.
           MOVE "MTD001P" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD001P.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "PAR001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS
           OPEN I-O MTD001 CEAD010 MTD001P COD045 CADPRO CADMOD
           CLOSE           CEAD010 MTD001P COD045 CADPRO CADMOD
           OPEN INPUT CEAD010 CAD010 COD040 PAR001 MTD001P COD045
                      CADPRO CADMOD
           IF ST-MTD001 = "35"
              CLOSE MTD001      OPEN OUTPUT MTD001
              CLOSE MTD001      OPEN I-O MTD001
           END-IF.
           IF ST-MTD001P = "35"
              CLOSE MTD001P     OPEN OUTPUT MTD001P
              CLOSE MTD001P     OPEN I-O    MTD001P
           END-IF.
           IF ST-CADPRO = "35"
              CLOSE CADPRO      OPEN OUTPUT CADPRO
              CLOSE CADPRO      OPEN I-O    CADPRO
           END-IF.
           IF ST-CADMOD = "35"
              CLOSE CADMOD      OPEN OUTPUT CADMOD
              CLOSE CADMOD      OPEN I-O    CADMOD
           END-IF.
           IF ST-MTD001 <> "00"
              MOVE "ERRO ABERTURA MTD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD001P <> "00"
              MOVE "ERRO ABERTURA MTD001P: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD001P TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CEAD010 <> "00"
              MOVE "ERRO ABERTURA CEAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CEAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADPRO <> "00"
              MOVE "ERRO ABERTURA CADPRO: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADPRO TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADMOD <> "00"
              MOVE "ERRO ABERTURA CADMOD: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADMOD TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD045 <> "00"
              MOVE "ERRO ABERTURA COD045: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD045 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE 1 TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                MOVE "Parametrização do Brinde Não Cadastrada"
                TO GS-MENSAGEM-ERRO
                PERFORM CARREGA-MENSAGEM-ERRO
           NOT INVALID KEY
                IF STATUS-REVENDIDO-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-REVENDIDO-PAR001
                END-IF
                MOVE STATUS-REVENDIDO-PAR001 TO WS-STATUS-REVENDIDO
                IF STATUS-ANALISE-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-ANALISE-PAR001
                END-IF
                MOVE STATUS-ANALISE-PAR001   TO WS-STATUS-ANALISE
           END-READ

           CLOSE PAR001

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP001"            to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess


           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW
               WHEN GS-SAVE-FLG-TRUE
                    PERFORM VALIDAR-PRODUZIDAS
                    IF GS-FLAG-CRITICA = 0
                       PERFORM SALVAR-DADOS
                       IF GS-TIPO-GRAVACAO = 1
                          PERFORM REGRAVA-DADOS
                       ELSE
                          PERFORM GRAVA-DADOS
                       END-IF
                    END-IF
                    IF GS-INDIVIDUAL = "S"
                       CALL   "MTP020" USING PARAMETROS-W GS-CONTRATO
                       CANCEL "MTP020"
                    END-IF
                    MOVE 0 TO GS-FLAG-CRITICA
                    PERFORM CARREGAR-DADOS
                    PERFORM VALIDAR-PRODUZIDAS
                    IF GS-FLAG-CRITICA = 0
                       MOVE GS-CONTRATO TO NR-CONTRATO-W
                       PERFORM LIMPAR-DADOS
                    END-IF
               WHEN GS-LOAD-FLG-TRUE
                    PERFORM CARREGAR-DADOS
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
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM CHAMAR-POP-UP
               WHEN GS-LE-PRODUTO-TRUE
                    PERFORM LER-OPCAO-PRODUTO
               WHEN GS-VALIDA-PRODUZIDA-TRUE
                    PERFORM VALIDAR-PRODUZIDAS
               WHEN GS-SOMAR-TRUE
                    PERFORM SOMAR
               WHEN GS-EVENTO-FLG-TRUE
                    PERFORM TRATAR-EVENTO
               WHEN GS-ALTERAR-CUSTO-TRUE
                    PERFORM ALTERAR-CUSTO
               WHEN GS-CUSTO-PRODUTOS-TRUE
                    PERFORM CUSTO-PRODUTOS
               WHEN GS-ESCALA-VENDA-TRUE
                    PERFORM ESCALA-VENDA
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CUSTO-PRODUTOS SECTION.
           CALL   "MTP020C" USING PARAMETROS-W GS-CONTRATO
           CANCEL "MTP020C".
       CUSTO-PRODUTOS-FIM.
           EXIT.

       ESCALA-VENDA SECTION.
           CALL   "MTP020E" USING PARAMETROS-W GS-CONTRATO
           CANCEL "MTP020E".
       ESCALA-VENDA-FIM.
           EXIT.

       ALTERAR-CUSTO SECTION.
           move 5                to indice
           initialize wsTexto
           move gs-vlr-custo     to masc-valor
           string masc-valor x"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
            using wsItem lnkcolunasPro(indice) wsTexto

           close      mtd001p
           open i-o   mtd001p

           move gs-contrato         to contrato-mt01p
           move gs-acp-produto      to produto-mt01p
           move gs-acp-modelo       to modelo-mt01p
           move gs-vlr-custo        to custo-mt01p

           write reg-mtd001p invalid key
                 rewrite reg-mtd001p invalid key
                     move "Erro de Gravação...MTD001P" to mensagem
                     move "C" to tipo-msg
                     perform exibir-mensagem
                 end-rewrite
           end-write

           close      mtd001p
           open input mtd001p

           unshow-window win3 principal
           invoke gs-listview-produtos "SetFocus".

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Identificação" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Referência" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
                 using z"Produto" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---
          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
                using z"Modelo" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Valor Custo" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

          perform mostrar-fonte-favoPro
          perform mostrar-colunas-favoPro

          invoke gs-listview-produtos "gridLines"
          invoke gs-listview-produtos "noBorder".

       mostrar-colunas-favoPro section.
          initialize wsTexto
          move "listview-mtp001-produtos" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview-produtos
                                  wsTexto
                                  lnktabelaPro.
       mostrar-colunas-favoPro-fim.
           exit.

       mostrar-fonte-favoPro section.
           move "listview-mtp001-produtos" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview-produtos wsTexto.
       mostrar-fonte-favoPro-fim.
           exit.

       EXPORTAR-PARA-EXCEL-PRO section.
           invoke aListview "ExportarParaOExcel"
                    using gs-listview-produtos lnkTabelaPro.
       EXPORTAR-PARA-EXCEL-PRO-fim.
           EXIT.


       zebrar-itensPro section.
           move "listview-mtp001-produtos" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview-produtos wsTexto
           invoke gs-listview-produtos "redrawallitems".
       zebrar-itensPro-fim.
           exit.

       chamar-colunas-favoPro section.
           move "listview-mtp001-produtos" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview-produtos
                               wsTexto
                               lnktabelaPro

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro.
       chamar-colunas-favoPro-fim.
           exit.

       TRATAR-EVENTO SECTION.
           evaluate gs-evento
               when 34123  perform chamar-colunas-favopro
               when 34013  perform produto-aceito
               when 34592  perform produto-aceito
               when 34027  set-focus ef9
           end-evaluate.

       produto-aceito section.
           INITIALIZE WSITEM
           INVOKE GS-LISTVIEW-PRODUTOS "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INVOKE GS-LISTVIEW-PRODUTOS "indexOf" USING UMITEM
                                                    RETURNING WSITEM

              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(1)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO


              MOVE FUNCTION NUMVAL(WSTEXTO(1:4)) TO NR-CONTRATO-CO45
              MOVE FUNCTION NUMVAL(WSTEXTO(5:4)) TO PRODUTO-CO45
              MOVE FUNCTION NUMVAL(WSTEXTO(9:4)) TO MODELO-CO45
              READ COD045 INVALID KEY
                  MOVE "Produto do Contrato Não Encontrado" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                  MOVE PRODUTO-CO45             TO GS-ACP-PRODUTO
                                                   CADPRO-CODIGO
                  MOVE MODELO-CO45              TO GS-ACP-MODELO
                                                   CADPRO-MODELO
                                                   CADMOD-CODIGO
                  READ CADPRO INVALID KEY
                       MOVE "********"          TO CADPRO-NOME
                  END-READ
                  MOVE CADPRO-NOME              TO GS-DESC-PRODUTO

                  READ CADMOD INVALID KEY
                       MOVE "********"          TO CADMOD-NOME
                  END-READ
                  MOVE CADMOD-NOME              TO GS-DESC-MODELO

                  INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(5)
                                         RETURNING UMOBJETO
                  INITIALIZE WSTEXTO
                  INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                  MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-VLR-CUSTO

                  REFRESH-OBJECT WIN3
                  SHOW-WINDOW WIN3
                  SET-FOCUS EF-VLR-CUSTO.

       SOMAR SECTION.
           COMPUTE GS-FOTO-PRODUZIDA = GS-FOTO-MONTADA +
                                       GS-FOTO-PERDIDA +
                                       GS-FOTO-AVULSA

           REFRESH-OBJECT PRINCIPAL.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win3 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VALIDAR-PRODUZIDAS SECTION.
           IF GS-FOTO-PRODUZIDA <> GS-FOTO-MONTADA + GS-FOTO-PERDIDA +
                                   GS-FOTO-AVULSA
              MOVE "Total de Fotos Produzidas Incorreta" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move    1 to gs-flag-critica.


       CHAMAR-POP-UP SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CEAP010T".
           EVALUATE GS-OPCAO
             WHEN 1 MOVE PASSAR-STRING-1(7: 40) TO GS-DESC-ALBUM
                    MOVE PASSAR-STRING-1(1: 5)  TO GS-COD-ALBUM
             WHEN 2 MOVE PASSAR-STRING-1(7: 40) TO GS-DESC-FOLHA
                    MOVE PASSAR-STRING-1(1: 5)  TO GS-COD-FOLHA
             WHEN 3 MOVE PASSAR-STRING-1(7: 40) TO GS-DESC-SEDA
                    MOVE PASSAR-STRING-1(1: 5)  TO GS-COD-SEDA
             WHEN 4 MOVE PASSAR-STRING-1(7: 40) TO GS-DESC-FOTO
                    MOVE PASSAR-STRING-1(1: 5)  TO GS-COD-FOTO
           END-EVALUATE.
       LER-OPCAO-PRODUTO SECTION.
           EVALUATE GS-OPCAO
             WHEN 1 MOVE GS-COD-ALBUM   TO PRODUTO
                    PERFORM LER-PRODUTO
                    MOVE DESC-PROD      TO GS-DESC-ALBUM
             WHEN 2 MOVE GS-COD-FOLHA   TO PRODUTO
                    PERFORM LER-PRODUTO
                    MOVE DESC-PROD      TO GS-DESC-FOLHA
             WHEN 3 MOVE GS-COD-SEDA    TO PRODUTO
                    PERFORM LER-PRODUTO
                    MOVE DESC-PROD      TO GS-DESC-SEDA
             WHEN 4 MOVE GS-COD-FOTO    TO PRODUTO
                    PERFORM LER-PRODUTO
                    MOVE DESC-PROD      TO GS-DESC-FOTO
           END-EVALUATE.
       LER-PRODUTO SECTION.
           READ CEAD010 INVALID KEY
                MOVE SPACES TO DESC-PROD.

      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GS-TIPO-GRAVACAO.
           MOVE GS-CONTRATO              TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                INITIALIZE REG-COD040
           END-READ

           MOVE IDENTIFICACAO-CO40       TO GS-IDENTIFICACAO
           MOVE QTDE-FORM-CO40           TO GS-QT-FORM
           MOVE PADRAO-CO40              TO GS-PADRAO
           MOVE CIDADE-CO40              TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES              TO NOME-CID
           END-READ
           MOVE NOME-CID                 TO GS-NOME-CIDADE

           MOVE GS-CONTRATO TO CONTRATO-MT01.
           START MTD001 KEY IS = CONTRATO-MT01 INVALID KEY
                 CONTINUE.
           READ MTD001 INVALID KEY
                INITIALIZE REG-MTD001
           NOT INVALID KEY
                MOVE 1 TO GS-TIPO-GRAVACAO
                MOVE GUIA-MT01                TO GS-NR-GUIA
                MOVE PRODUZIDA-MT01           TO GS-FOTO-PRODUZIDA
                MOVE MONTADA-MT01             TO GS-FOTO-MONTADA
                MOVE PERDIDA-MT01             TO GS-FOTO-PERDIDA
                MOVE AVULSA-MT01              TO GS-FOTO-AVULSA
                MOVE CLIEN-ALBUM-MT01         TO GS-CLIENTE-ALBUM
                MOVE CODALBUM-MT01            TO GS-COD-ALBUM PRODUTO
                PERFORM LER-PRODUTO
                MOVE DESC-PROD                TO GS-DESC-ALBUM
                MOVE CODFOLHA-MT01            TO GS-COD-FOLHA PRODUTO
                PERFORM LER-PRODUTO
                MOVE DESC-PROD                TO GS-DESC-FOLHA
                MOVE CODSEDA-MT01             TO GS-COD-SEDA PRODUTO
                PERFORM LER-PRODUTO
                MOVE DESC-PROD                TO GS-DESC-SEDA
                MOVE CODFOTO-MT01             TO GS-COD-FOTO PRODUTO
                PERFORM LER-PRODUTO
                MOVE DESC-PROD                TO GS-DESC-FOTO
           END-READ

           EVALUATE STATUS-CO40
               WHEN WS-STATUS-REVENDIDO
                    MOVE 1                      TO GS-FLAG-CRITICA
                    MOVE "Contrato com STATUS Revendido" TO
                    MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               WHEN WS-STATUS-ANALISE
                    MOVE 1 TO GS-FLAG-CRITICA
                    MOVE "Contrato com STATUS Analise" TO
                    MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               WHEN OTHER
                    MOVE 0 TO GS-FLAG-CRITICA
           END-EVALUATE

           INVOKE GS-LISTVIEW-PRODUTOS "DeleteAll"
           INITIALIZE REG-COD045
           MOVE GS-CONTRATO                TO NR-CONTRATO-CO45
           START COD045 KEY IS NOT LESS CHAVE-CO45 INVALID KEY
                 MOVE "10" TO ST-COD045.

           PERFORM UNTIL ST-COD045 = "10"
                 READ COD045 NEXT AT END
                      MOVE "10" TO ST-COD045
                 NOT AT END
                      IF GS-CONTRATO <> NR-CONTRATO-CO45
                         MOVE "10" TO ST-COD045
                      ELSE
                         initialize indice
                         invoke gs-listview-produtos
                                "adicionarItem" returning wsItem

                         add 1 to indice
                         initialize wsTexto
                         string nr-contrato-co45 produto-co45
                                modelo-co45 X"00"  into wsTexto
                         invoke gs-listview-produtos
                                "preencherColunaZ"
                           using wsItem lnkcolunasPro(indice) wsTexto

                         move produto-co45 to cadpro-codigo
                         move modelo-co45  to cadpro-modelo
                         read cadpro invalid key
                              move "**********" to cadpro-nome
                         end-read

                         add 1 to indice
                         initialize wsTexto
                         string cadpro-referencia
                                             x"00" delimited by "  "
                           into wsTexto
                         invoke gs-listview-produtos
                                "preencherColunaZ"
                           using wsItem lnkcolunasPro(indice) wsTexto

                         move modelo-co45  to cadmod-codigo
                         read cadmod invalid key
                              move "**********" to cadmod-nome
                         end-read

                         add 1 to indice
                         initialize wsTexto
                         string cadpro-nome x"00" delimited by "  "
                           into wsTexto
                         invoke gs-listview-produtos
                                "preencherColunaZ"
                           using wsItem lnkcolunasPro(indice) wsTexto


                         add 1 to indice
                         initialize wsTexto
                         string cadmod-nome x"00" delimited by "  "
                           into wsTexto
                         invoke gs-listview-produtos
                                "preencherColunaZ"
                           using wsItem lnkcolunasPro(indice) wsTexto


                         move nr-contrato-co45 to contrato-mt01p
                         move produto-co45     to produto-mt01p
                         move modelo-co45      to modelo-mt01p
                         read mtd001p invalid key
                              initialize reg-mtd001p
                         end-read

                         add 1 to indice
                         initialize wsTexto
                         move custo-mt01p to masc-valor
                         string masc-valor x"00" into wsTexto
                         invoke gs-listview-produtos "preencherColunaZ"
                           using wsItem lnkcolunasPro(indice) wsTexto

                      END-IF
                 END-READ
           END-PERFORM

           perform mostrar-colunas-favoPro
           perform zebrar-itensPro


           INITIALIZE REG-MTD001P
           refresh-object principal.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-MTD001
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           INVOKE GS-LISTVIEW-PRODUTOS "DELETEALL".
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO                 TO CONTRATO-MT01
           MOVE GS-NR-GUIA                  TO GUIA-MT01
           MOVE GS-FOTO-PRODUZIDA           TO PRODUZIDA-MT01
           MOVE GS-FOTO-MONTADA             TO MONTADA-MT01
           MOVE GS-FOTO-PERDIDA             TO PERDIDA-MT01
           MOVE GS-FOTO-AVULSA              TO AVULSA-MT01
           MOVE GS-CLIENTE-ALBUM            TO CLIEN-ALBUM-MT01
           MOVE GS-COD-ALBUM                TO CODALBUM-MT01
           MOVE GS-COD-FOLHA                TO CODFOLHA-MT01
           MOVE GS-COD-SEDA                 TO CODSEDA-MT01
           MOVE GS-COD-FOTO                 TO CODFOTO-MT01.

       GRAVA-DADOS SECTION.
           WRITE REG-MTD001 INVALID KEY
               MOVE "Erro Gravação MTD001" TO GS-MENSAGEM-ERRO
               MOVE ST-MTD001 TO GS-MENSAGEM-ERRO(24: 5)
               MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           REWRITE REG-MTD001 INVALID KEY
                 MOVE "Erro Regravação MTD001" TO GS-MENSAGEM-ERRO
                 MOVE ST-MTD001 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
       EXCLUI SECTION.
           DELETE MTD001 NOT INVALID KEY
                  CLOSE      MTD001P
                  OPEN I-O   MTD001P
                  INITIALIZE REG-MTD001P
                  START MTD001P KEY IS NOT LESS CHAVE-MT01P INVALID KEY
                        MOVE "10" TO ST-MTD001P
                  END-START
                  PERFORM UNTIL ST-MTD001P = "10"
                        READ MTD001P NEXT AT END
                             MOVE "10" TO ST-MTD001P
                        NOT AT END
                             IF GS-CONTRATO <> CONTRATO-MT01P
                                MOVE "10" TO ST-MTD001P
                             ELSE
                                DELETE MTD001P INVALID KEY
                                    MOVE "Erro de Exclusão...MTD001P"
                                      TO MENSAGEM
                                    MOVE "C" TO TIPO-MSG
                                    PERFORM EXIBIR-MENSAGEM
                                END-DELETE
                             END-IF
                        END-READ
                  END-PERFORM
                  CLOSE        MTD001P
                  OPEN INPUT   MTD001P.
           PERFORM LIMPAR-DADOS.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP001" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT.

           MOVE SPACES                     TO LINDET-REL
           MOVE "Contrato: "               TO LINDET-REL(1: 10)
           MOVE GS-CONTRATO                TO LINDET-REL(11: 10)
           MOVE "IDENTIFICACAO: "          TO LINDET-REL(21: 15)
           MOVE GS-IDENTIFICACAO           TO LINDET-REL(36: 30)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "CIDADE: "                 TO LINDET-REL(1: 8)
           MOVE GS-NOME-CIDADE             TO LINDET-REL(9: 40)
           MOVE "QT-FORM: "                TO LINDET-REL(49: 9)
           MOVE GS-QT-FORM                 TO LINDET-REL(58: 10)
           MOVE "PADRAO: "                 TO LINDET-REL(68: 8)
           MOVE GS-PADRAO                  TO LINDET-REL(76: 1)
           WRITE REG-RELAT FROM LINDET
           MOVE "Nr.Guia Produção......: " TO LINDET-REL(1: 24)
           MOVE GS-NR-GUIA                 TO QTDE-E
           MOVE QTDE-E                     TO LINDET-REL(25: 7)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "Total Fotos Produzidas: " TO LINDET-REL(1: 24)
           MOVE GS-FOTO-PRODUZIDA          TO QTDE-E
           MOVE QTDE-E                     TO LINDET-REL(25: 7)
           WRITE REG-RELAT FROM LINDET

           MOVE "Total Fotos Montadas..: " TO LINDET-REL(1: 24)
           MOVE GS-FOTO-MONTADA            TO QTDE-E
           MOVE QTDE-E                     TO LINDET-REL(25: 7)
           WRITE REG-RELAT FROM LINDET
           MOVE "Total Fotos Perdidas..: " TO LINDET-REL(1: 24)
           MOVE GS-FOTO-PERDIDA            TO QTDE-E
           MOVE QTDE-E                     TO LINDET-REL(25: 7)
           WRITE REG-RELAT FROM LINDET
           MOVE "Total Fotos Avulsas...: " TO LINDET-REL(1: 24)
           MOVE GS-FOTO-AVULSA             TO QTDE-E
           MOVE QTDE-E                     TO LINDET-REL(25: 7)
           WRITE REG-RELAT FROM LINDET
           MOVE "Total Clientes/Albuns.: " TO LINDET-REL(1: 24)
           MOVE GS-CLIENTE-ALBUM           TO QTDE-E
           MOVE QTDE-E                     TO LINDET-REL(25: 7)
           WRITE REG-RELAT FROM LINDET
           MOVE "Código Produto Álbum..: " TO LINDET-REL(1: 24)
           MOVE GS-COD-ALBUM               TO QTDE-E PRODUTO
           MOVE QTDE-E                     TO LINDET-REL(25: 7)
           PERFORM LER-PRODUTO
           MOVE DESC-PROD                  TO LINDET-REL(32: 45)
           WRITE REG-RELAT FROM LINDET AFTER 2

           MOVE "Código Produto Folha..: " TO LINDET-REL(1: 24)
           MOVE GS-COD-FOLHA               TO QTDE-E PRODUTO
           MOVE QTDE-E                     TO LINDET-REL(25: 7)
           PERFORM LER-PRODUTO
           MOVE DESC-PROD                  TO LINDET-REL(32: 45)
           WRITE REG-RELAT FROM LINDET
           MOVE "Código Produto Sedas..: " TO LINDET-REL(1: 24)
           MOVE GS-COD-SEDA                TO QTDE-E PRODUTO
           MOVE QTDE-E                     TO LINDET-REL(25: 7)
           PERFORM LER-PRODUTO
           MOVE DESC-PROD                  TO LINDET-REL(32: 45)
           WRITE REG-RELAT FROM LINDET
           MOVE "Código Produto Fotos..: " TO LINDET-REL(1: 24)
           MOVE GS-COD-FOTO                TO QTDE-E PRODUTO
           MOVE QTDE-E                     TO LINDET-REL(25: 7)
           PERFORM LER-PRODUTO
           MOVE DESC-PROD                  TO LINDET-REL(32: 45)
           WRITE REG-RELAT FROM LINDET

           MOVE SPACES TO LINDET-REL
           WRITE REG-RELAT FROM LINDET.

           CLOSE RELAT.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
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
           CLOSE COD040 COD045 CAD010 MTD001 CEAD010 MTD001P CADPRO
                 CADMOD.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP001"            to logacess-programa
           move "FECHADO"           to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
