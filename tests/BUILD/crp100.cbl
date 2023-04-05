       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP100.
      *DATA: 20/04/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RECEBIMENTOS DE TÍTULOS
      *FUNÇÃO: Listar todos os títulos que estiverem dentro do intervalo
      *        de vencimento. As ordens serão: Vencto, Portador, cliente
      *        e VCTO/vcto.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           AListview          is class "alistview"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX010.
           COPY CGPX011.
           COPY CRPX020.
           COPY CRPX020B.
           COPY CAPX018.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = CLASSIF-WK
                                           CLIENTE-WK
                                           SEQ-WK
                  ALTERNATE RECORD KEY IS
                     CHAVE-VENCTO-WK = VENCTO-WK
                                       CLASSIF-WK
                                       CLIENTE-WK
                                       SEQ-WK.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       COPY CGPW011.
       COPY CAPW018.
       COPY CRPW020.
       COPY CRPW020B.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  CLASSIF-WK          PIC 9.
           05  CLIENTE-WK          PIC 9(8).
           05  SEQ-WK              PIC 9(5).
           05  NOME-CLIEN-WK       PIC X(20).
           05  NOSSO-NR-WK         PIC X(15).
           05  PORTADOR-WK         PIC X(60).
           05  VENCTO-WK           PIC 9(8).
           05  VALOR-WK            PIC 9(8)V99.
           05  TIPO-WK             PIC X(20).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP100.CPB".
           COPY "CRP100.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  CLIENTE-W             PIC 9(8)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(15)    VALUE SPACES.
           05  NOME-CLIEN-ANT        PIC X(20)    VALUE SPACES.
           05  PORTADOR-ANT          PIC X(10)    VALUE ZEROS.
           05  ALBUM-ANT             PIC 9(8)     VALUE ZEROS.
           05  VENCTO-ANT            PIC 9(8)     VALUE ZEROS.
           05  AUX-TIPO              PIC 9(01)    VALUE ZEROS.
           05  TOT-VALOR-PARCIAL     PIC 9(09)V99 VALUE ZEROS.
           05  TOT-JUROS-PARCIAL     PIC 9(09)V99 VALUE ZEROS.
           05  TOT-MULTA-PARCIAL     PIC 9(09)V99 VALUE ZEROS.
           05  TOT-DESC-PARCIAL      PIC 9(09)V99 VALUE ZEROS.
           05  TOT-LIQ-PARCIAL       PIC 9(09)V99 VALUE ZEROS.
           05  TOT-VALOR-TOTAL       PIC 9(09)V99 VALUE ZEROS.
           05  TOT-JUROS-TOTAL       PIC 9(09)V99 VALUE ZEROS.
           05  TOT-MULTA-TOTAL       PIC 9(09)V99 VALUE ZEROS.
           05  TOT-DESC-TOTAL        PIC 9(09)V99 VALUE ZEROS.
           05  TOT-LIQ-TOTAL         PIC 9(09)V99 VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           COPY "PARAMETR".

       77 janelaPrincipal          object reference.
       77 handle8                  pic 9(08) comp-x value zeros.
       77 wHandle                  pic 9(09) comp-5 value zeros.

       01 aux-valor                pic 9(09)v99 value zeros.
       01 wsSize                   pic 9(09) comp-5 value zeros.
       01 wsIndice                 pic 9(09) comp-5 value zeros.
       01 umItem                   object reference value null.
       01 umTexto                  object reference value null.
       01 wsColValor               pic 9(09) comp-5 value 6.
       01 wsColChave               pic 9(09) comp-5 value 7.
       77 wsCheckEnable            pic 99       comp-5 value 0.
       01 indice                   pic 9(02)  value zeros.
       01 wsitem                   pic 9(09)  comp-5 value zeros.
       01 wsTexto                  pic x(255) value spaces.
       01 lnktabela.
          02 lnkobjetoscol         object reference occurs 99 times.

       01 lnktabelaCol.
          02 lnkcolunas            pic 9(09) comp-5 value zeros
                                          occurs 99 times.

       01  CAB01.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RECEBIMENTOS DE TITULOS     ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "NOME-CLIENTE         ALBUM      CART    DATA            VALO
      -    "R      JUROS      MULTA   DESCONTO   VLR-LIQUIDO".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

       01  DET-TOTALIZA.
           05 DET-DESCRICAO        PIC X(47) VALUE SPACES.
           05 DET-TOT-VLR          PIC ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 DET-TOT-JRS          PIC ZZZ.ZZ9,99     BLANK WHEN ZEROS.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 DET-TOT-MUL          PIC ZZZ.ZZ9,99     BLANK WHEN ZEROS.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 DET-TOT-DES          PIC ZZZ.ZZ9,99     BLANK WHEN ZEROS.
           05 DET-TOT-LIQ          PIC ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.

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

       01 lnkusu.
          copy usuario.cpy.


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD020B" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020B.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CAD018 CGD010 CGD011.
           OPEN I-O CRD020 CRD020B.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020B <> "00"
              MOVE "ERRO ABERTURA CRD020B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario.

           CLOSE      CRD020 CRD020B
           OPEN INPUT CRD020 CRD020B
      *    MOVE 1 TO COD-USUARIO-W

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP100"            to logacess-programa
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


           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
                    PERFORM CRIAR-LISTVIEW
               WHEN GS-REGRAVA-DADOS-TRUE
                    PERFORM BAIXAR-CONTAS
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-ACHAR-VENCTO-TRUE
                    PERFORM ACHAR-VENCTO
               WHEN GS-EVENTO-FLG-TRUE
                    PERFORM TRATAR-EVENTO
               WHEN GS-SELECIONA-TRUE
                    PERFORM VERIFICAR-SELECAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       BAIXAR-CONTAS SECTION.
           CLOSE    CRD020 CRD020B
           OPEN I-O CRD020 CRD020B

           MOVE SPACES TO MENSAGEM

           MOVE 0   TO WSINDICE
                       GS-TOTAL-SELECIONADO

           INVOKE GS-LISTVIEW-BAIXA "Size" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
               ADD 1 TO WSINDICE
               INVOKE GS-LISTVIEW-BAIXA "ITEMATINDEX" USING WSINDICE
                                                  RETURNING UMITEM
               IF UMITEM <> NULL
                  INITIALIZE WSCHECKENABLE
                  INVOKE UMITEM "GETCHECKBOXVALUE"
                         RETURNING WSCHECKENABLE
                  IF WSCHECKENABLE = 1
                     INVOKE UMITEM "GETCOLUMNVALUE"
                                       USING WSCOLCHAVE
                                   RETURNING UMTEXTO
                     INITIALIZE WSTEXTO
                     INVOKE UMTEXTO "GETVALUE" RETURNING WSTEXTO
                     MOVE WSTEXTO(1:9)             TO COD-COMPL-CR20
                     MOVE WSTEXTO(10:5)            TO SEQ-CR20

                     MOVE WSTEXTO       TO GS-LINHA-AVISO
                     REFRESH-OBJECT WIN3

                     READ CRD020 INVALID KEY
                          MOVE SPACES TO MENSAGEM
                          STRING "Contas a Receber Não Encontrado"
                                 x"0da0"
                                 "COD-COMPL-CR20 = " COD-COMPL-CR20
                                 x"0da0"
                                 "SEQ-CR20 = " SEQ-CR20
                            INTO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                     NOT INVALID KEY
                          INITIALIZE REG-CRD020B
                          MOVE CLASS-CLIENTE-CR20 TO CLASS-CLIENTE-CR20B
                          MOVE CLIENTE-CR20       TO CLIENTE-CR20B
                          MOVE SEQ-CR20           TO SEQ-CR20B
                          MOVE VALOR-TOT-CR20     TO VALOR-TOT-CR20B
                          MOVE JURO-RCTO-CR20     TO JURO-RCTO-CR20B
                          MOVE DESCONTO-CR20      TO DESCONTO-CR20B
                          MOVE VALOR-TOT-CR20     TO VALOR-BAIXA-CR20B
                          MOVE MULTA-RCTO-CR20    TO MULTA-RCTO-CR20B
                          STRING GS-DATA-RCTO(5:4)
                                 GS-DATA-RCTO(3:2)
                                 GS-DATA-RCTO(1:2)
                                                 INTO DATA-RCTO-CR20B
                          MOVE VALOR-SALDO-CR20   TO VALOR-LIQ-CR20B
                          MOVE SEQ-CAIXA-CR20     TO SEQ-CAIXA-CR20B
                          MOVE "7-Bx Sistema"     TO FORMA-PAGTO-CR20B
                          MOVE USUARIO-W          TO USUARIO-CR20B
                          MOVE DCR-MEM-CR20       TO DCR-MEM-CR20B
                          WRITE REG-CRD020B INVALID KEY
                                MOVE "Erro de Gravação..CRD020B" TO
                                MENSAGEM
                                MOVE "C" TO TIPO-MSG
                                PERFORM EXIBIR-MENSAGEM
                          NOT INVALID KEY
                                MOVE 2 TO SITUACAO-CR20
                                STRING GS-DATA-RCTO(5:4)
                                       GS-DATA-RCTO(3:2)
                                       GS-DATA-RCTO(1:2)
                                  INTO DATA-RCTO-CR20
                                MOVE 0 TO VALOR-SALDO-CR20
                                MOVE VALOR-TOT-CR20 TO VALOR-LIQ-CR20
                                MOVE "7-Bx Sistema" TO FORMA-PAGTO-CR20
                                REWRITE REG-CRD020 INVALID KEY
                                    MOVE "Erro de Regravação...CRD020"
                                    TO MENSAGEM
                                    MOVE "C" TO TIPO-MSG
                                    PERFORM EXIBIR-MENSAGEM
                                END-REWRITE
                          END-WRITE
                     END-READ
                  END-IF
               END-IF
           END-PERFORM

           CLOSE      CRD020 CRD020B
           OPEN INPUT CRD020 CRD020B

           IF MENSAGEM EQUAL SPACES
              MOVE "Títulos Baixados com Sucesso" TO MENSAGEM
              MOVE "I" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           MOVE SPACES TO GS-LINHA-AVISO.


       VERIFICAR-SELECAO SECTION.
          MOVE 0   TO WSINDICE
                      GS-TOTAL-SELECIONADO

           INVOKE GS-LISTVIEW-BAIXA "Size" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
               ADD 1 TO WSINDICE
               INVOKE GS-LISTVIEW-BAIXA "ITEMATINDEX" USING WSINDICE
                                                  RETURNING UMITEM
               IF UMITEM <> NULL
                  INITIALIZE WSCHECKENABLE
                  INVOKE UMITEM "GETCHECKBOXVALUE"
                         RETURNING WSCHECKENABLE
                  IF WSCHECKENABLE = 1
                     IF GS-SELECAO = 0
                        PERFORM DESMARCAR
                     END-IF
                  ELSE
                     IF GS-SELECAO = 1
                        PERFORM MARCAR
                        INVOKE UMITEM "GETCOLUMNVALUE"
                                          USING WSCOLVALOR
                                      RETURNING UMTEXTO
                        INITIALIZE WSTEXTO
                        INVOKE UMTEXTO "GETVALUE" RETURNING WSTEXTO
                        MOVE FUNCTION NUMVAL(WSTEXTO) TO AUX-VALOR

                        ADD AUX-VALOR TO GS-TOTAL-SELECIONADO
                     END-IF
                  END-IF
               END-IF
           END-PERFORM.

       DESMARCAR SECTION.
           INITIALIZE WSCHECKENABLE
           INVOKE UMITEM "SETCHECKBOXVALUE"
                  USING WSCHECKENABLE
           INVOKE UMITEM "UNSELECTCHECKBOX".

       MARCAR SECTION.
           MOVE 1 TO WSCHECKENABLE
           INVOKE UMITEM "SETCHECKBOXVALUE"
                  USING WSCHECKENABLE
           INVOKE UMITEM "SELECTCHECKBOX".


       TRATAR-EVENTO SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34027  SET-FOCUS EF1
               WHEN 34594  PERFORM CALCULAR-SELECIONADO
               WHEN 34596  PERFORM CALCULAR-SELECIONADO
               WHEN 34013  PERFORM ITEM-ACEITO
               WHEN 34592  PERFORM ITEM-ACEITO
               WHEN 34123  PERFORM CHAMAR-COLUNAS-FAVO.

       ITEM-ACEITO SECTION.
           IF GS-TOTAL-SELECIONADO = 0
              MOVE "Nenhum Título Selecionado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE GS-TOTAL-SELECIONADO TO GS-VALOR-TITULO
              SHOW-WINDOW WIN3
              REFRESH-OBJECT WIN3
              SET-FOCUS EF4.


       CALCULAR-SELECIONADO SECTION.
          invoke gs-listview-baixa "size" returning wsSize

          move 0   to wsIndice
                      gs-total-selecionado

          perform wsSize times
             add 1 to wsIndice
             invoke gs-listview-baixa "itemAtIndex" using wsIndice
                                                returning umItem
             if umItem <> null
                initialize wsCheckEnable
                invoke umItem "getCheckBoxValue"
                       returning wsCheckEnable

                if wsCheckEnable = 1
                   invoke umItem "getColumnValue"
                                     using wsColValor
                                 returning umTexto
                   initialize wsTexto
                   invoke umTexto "getValue" returning wsTexto
                   move function numval(wsTexto) to aux-valor

                   add aux-valor to gs-total-selecionado
                end-if
             end-if
          end-perform
          refresh-object principal.

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---                                7
          add 1 to indice
          invoke GS-LISTVIEW-BAIXA "adicionarColunaZ"
            using z"Cliente" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke GS-LISTVIEW-BAIXA "adicionarColunaZ"
            using z"Álbum" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke GS-LISTVIEW-BAIXA "adicionarColunaZ"
            using z"Tipo" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke GS-LISTVIEW-BAIXA "adicionarColunaZ"
            using z"Portador" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke GS-LISTVIEW-BAIXA "adicionarColunaZ"
            using z"Vencimento" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke GS-LISTVIEW-BAIXA "adicionarColunaZ"
            using z"Valor Saldo" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke GS-LISTVIEW-BAIXA "adicionarColunaZ"
            using z"Chave" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke GS-LISTVIEW-BAIXA "checkboxes".
          invoke GS-LISTVIEW-BAIXA "gridLines"
          invoke GS-LISTVIEW-BAIXA "noBorder".

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-crp100" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview-baixa
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.

       mostrar-fonte-favo section.
           move "listview-crp100" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview-baixa wstexto.
       mostrar-fonte-favo-fim.
           exit.

       exportar-para-excel section.
           invoke aListview "ExportarParaOExcel"
                    using gs-listview-baixa lnktabela.
       exportar-para-excel-fim.
           EXIT.

       zebrar-itens section.
           move "listview-crp100" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview-baixa wstexto
           invoke gs-listview-baixa "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-crp100" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview-baixa
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.


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

       ACHAR-VENCTO SECTION.
           MOVE DATA-DIA-I     TO GRTIME-DATE.
           MOVE 2              TO GRTIME-TYPE.
           MOVE 1              TO GRTIME-FUNCTION.
           MOVE 7              TO GRTIME-DAYS.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           MOVE 01010001       TO GS-VENCTO-INI.
           CALL "GRIDAT1" USING GRTIME-DATE-FINAL.
           MOVE GRTIME-DATE-FINAL TO GS-VENCTO-FIM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-TIPO-DOCUMENTO(1:1) TO AUX-TIPO

           MOVE GS-VENCTO-INI          TO DATA-INV
                                          VENCTO-INI-ANT
                                          VENCTO-INI-REL

           CALL "GRIDAT2"           USING DATA-INV
           MOVE DATA-INV               TO VENCTO-INI

           MOVE GS-VENCTO-FIM          TO DATA-INV
                                          VENCTO-FIM-ANT
                                          VENCTO-FIM-REL

           CALL "GRIDAT2"           USING DATA-INV
           MOVE DATA-INV               TO VENCTO-FIM.

           INITIALIZE REG-CRD020

           INITIALIZE REG-CRD020
           MOVE VENCTO-INI  TO DATA-VENCTO-CR20
           MOVE 0           TO SITUACAO-CR20
           MOVE ZEROS       TO CLIENTE-CR20 CLASS-CLIENTE-CR20
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020
           END-START
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      IF SITUACAO-CR20 <> 0
                         MOVE "10" TO ST-CRD020
                      ELSE
                         IF DATA-VENCTO-CR20 > VENCTO-FIM
                            MOVE "10" TO ST-CRD020
                         ELSE
                            IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                               IF GS-PORTADOR = 0 OR PORTADOR-CR20
                                  PERFORM MOVER-DADOS-WORK
                                  MOVE DATA-VENCTO-CR20       TO
                                       GS-EXIBE-VENCTO
                                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                                  WRITE REG-WORK
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           INITIALIZE REG-CRD020
           MOVE VENCTO-INI  TO DATA-VENCTO-CR20
           MOVE 1           TO SITUACAO-CR20
           MOVE ZEROS       TO CLIENTE-CR20 CLASS-CLIENTE-CR20
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020
           END-START
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      IF SITUACAO-CR20 <> 1
                         MOVE "10" TO ST-CRD020
                      ELSE
                         IF DATA-VENCTO-CR20 > VENCTO-FIM
                            MOVE "10" TO ST-CRD020
                         ELSE
                            IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                               IF GS-PORTADOR = 0 OR PORTADOR-CR20
                                  PERFORM MOVER-DADOS-WORK
                                  MOVE DATA-VENCTO-CR20       TO
                                       GS-EXIBE-VENCTO
                                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                                  WRITE REG-WORK
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM


           CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.


       MOVER-DADOS-WORK SECTION.
           MOVE DATA-VENCTO-CR20       TO VENCTO-WK
                                          GS-EXIBE-VENCTO

           MOVE CLASS-CLIENTE-CR20      TO CLASSIF-WK
                                           CLASSIF-CG10
                                           CLASSIF-CG11

           MOVE CLIENTE-CR20            TO CLIENTE-WK
                                           CODIGO-CG10
                                           CODIGO-CG11
           READ CGD010 INVALID KEY
                MOVE "*******"          TO COMPRADOR-CG10
           END-READ

           MOVE COMPRADOR-CG10          TO NOME-CLIEN-WK.
           READ CGD011 INVALID KEY
                MOVE ZEROS              TO CIDADE1-CG11
           END-READ

           EVALUATE TIPO-DOCTO-CR20
               WHEN 0     MOVE "0-Boleto"             TO TIPO-WK
               WHEN 1     MOVE "1-Dupl/Promis"        TO TIPO-WK
               WHEN 2     MOVE "2-Org.Evento"         TO TIPO-WK
               WHEN 3     MOVE "3-Debito Automatico"  TO TIPO-WK
               WHEN 4     MOVE "4-Cartao de Credito"  TO TIPO-WK
               WHEN OTHER MOVE "*********"            TO TIPO-WK
           END-EVALUATE

           MOVE SEQ-CR20                TO SEQ-WK
           MOVE PORTADOR-CR20           TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "******"           TO NOME-PORT
           END-READ

           MOVE SPACES                  TO PORTADOR-WK
           STRING PORTADOR-CR20 "-" NOME-PORT INTO PORTADOR-WK

           MOVE VALOR-SALDO-CR20        TO VALOR-WK.

       CARREGA-LISTA SECTION.
           INVOKE GS-LISTVIEW-BAIXA "DeleteAll"

           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS CHAVE-VENCTO-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      INITIALIZE INDICE
                      INVOKE GS-LISTVIEW-BAIXA "ADICIONARITEM"
                                                     RETURNING WSITEM


                      MOVE CLASSIF-WK  TO CLASSIF-CG10
                      MOVE CLIENTE-WK  TO CODIGO-CG10
                      READ CGD010 INVALID KEY
                           MOVE "*********" TO COMPRADOR-CG10
                      END-READ

                      ADD 1 TO INDICE
                      INITIALIZE WSTEXTO
                      STRING COMPRADOR-CG10 X"00" DELIMITED BY "   "
                        INTO WSTEXTO
                      INVOKE GS-LISTVIEW-BAIXA "PREENCHERCOLUNAZ"
                       USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

                      ADD 1 TO INDICE
                      INITIALIZE WSTEXTO
                      STRING CLIENTE-WK X"00" DELIMITED BY "   "
                        INTO WSTEXTO
                      INVOKE GS-LISTVIEW-BAIXA "PREENCHERCOLUNAZ"
                       USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

                      ADD 1 TO INDICE
                      INITIALIZE WSTEXTO
                      STRING TIPO-WK X"00" DELIMITED BY "   "
                        INTO WSTEXTO
                      INVOKE GS-LISTVIEW-BAIXA "PREENCHERCOLUNAZ"
                       USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

                      ADD 1 TO INDICE
                      INITIALIZE WSTEXTO
                      STRING PORTADOR-WK
                             X"00" DELIMITED BY "   " INTO WSTEXTO
                      INVOKE GS-LISTVIEW-BAIXA "PREENCHERCOLUNAZ"
                       USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

                      ADD 1 TO INDICE
                      INITIALIZE WSTEXTO
                      STRING VENCTO-WK(7:2) "/"
                             VENCTO-WK(5:2) "/"
                             VENCTO-WK(1:4) X"00"
                             DELIMITED BY "   " INTO WSTEXTO
                      INVOKE GS-LISTVIEW-BAIXA "PREENCHERCOLUNAZ"
                       USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

                      ADD 1 TO INDICE
                      INITIALIZE WSTEXTO
                      MOVE VALOR-WK          TO VALOR-E
                      STRING VALOR-E X"00" INTO WSTEXTO
                      INVOKE GS-LISTVIEW-BAIXA "PREENCHERCOLUNAZ"
                       USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

                      ADD 1 TO INDICE
                      INITIALIZE WSTEXTO
                      STRING CLASSIF-WK CLIENTE-WK SEQ-WK
                                  X"00" INTO WSTEXTO
                      INVOKE GS-LISTVIEW-BAIXA "PREENCHERCOLUNAZ"
                       USING WSITEM LNKCOLUNAS(INDICE) WSTEXTO

                 END-READ
           END-PERFORM

           PERFORM MOSTRAR-FONTE-FAVO
           PERFORM MOSTRAR-COLUNAS-FAVO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       exibir-mensagem section.
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
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP100"            to logacess-programa
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

           CLOSE CRD020 CRD020B CAD018 CGD010 CGD011.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
