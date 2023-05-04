       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP023.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 31/10/2000.
      *FUNÇÃO: MANUTENACO NA PLANILHA - MONTAGEM DE ALBUM

       ENVIRONMENT DIVISION.
       class-control.
           AListview          is class "alistview"
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX004.
           COPY COPX040.
           COPY CGPX001.
           COPY MTPX002.
           COPY MTPX019.
           COPY MTPX020.
           COPY MTPX023.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW004.
       COPY COPW040.
       COPY CGPW001.
       COPY MTPW002.
       COPY MTPW019.
       COPY MTPW020.
       COPY MTPW023.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP023.CPB".
           COPY "MTP023.CPY".
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
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-MTD002             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD023             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
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
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  POSSE-W               PIC 9        VALUE ZEROS.
           05  IDENTIFICACAO-W       PIC X(30)    VALUE SPACES.
           05  DATA-ROMANEIO-W       PIC 9(08)    VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  TIPO-FOGO             PIC 9        VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-W            PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(4)     VALUE ZEROS.
           05  MENSAGEM              PIC X(200)   VALUE SPACES.
           05  TIPO-MSG              PIC X(01)    VALUE SPACES.
           05  RESP-MSG              PIC X(01)    VALUE SPACES.
           COPY "PARAMETR".


       77 wsTexto                      pic x(255) value spaces.
       77 wsCheckEnable                pic 99     comp-5 value 0.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 UMITEM                       object reference.
       77 UMOBJETO                     object reference.

       01 WSCOLUNACONTRATO             pic 9(009) comp-5 value zeros.
       01 WSCOLUNAALBUM                pic 9(009) comp-5 value zeros.
       01 WSCOLUNAPOSSE                pic 9(009) comp-5 value zeros.
       01 WSCOLUNACODPOSSE             pic 9(009) comp-5 value zeros.

       01 lnktabelaPro.
          02 lnkobjetoscolPro          object reference occurs 99 times.
       01 lnktabelaColPro.
          02 lnkcolunasPro pic 9(09) comp-5 value zeros occurs 99 times.
       01 indice                   pic 9(02).
       01 wssize                   pic 9(09) comp-5 value zeros.
       01 wsIndice                 pic 9(09) comp-5 value zeros.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 lnkusu.
          copy usuario.cpy.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA-DIA-I FROM DATE.
           MOVE 20             TO DATA-DIA-I(1: 2)
           MOVE DATA-DIA-I     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV       TO DATA-DIA-W.

           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario
           MOVE "CAD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "MTD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD002.
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD023" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD023.
           OPEN I-O MTD023 MTD020 MTD019.
           CLOSE    MTD020
           OPEN INPUT COD040 CAD010 CGD001 MTD002 CAD004 MTD020.
           IF ST-MTD019 = "35"
              CLOSE MTD019      OPEN OUTPUT MTD019
              CLOSE MTD019      OPEN I-O MTD019
           END-IF.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.
           IF ST-MTD023 = "35"
              CLOSE MTD023      OPEN OUTPUT MTD023
              CLOSE MTD023      OPEN I-O MTD023
           END-IF.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD002 <> "00"
              MOVE "ERRO ABERTURA MTD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD023 <> "00"
              MOVE "ERRO ABERTURA MTD023: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD023 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      MTD023 MTD020 MTD019
           OPEN INPUT MTD023 MTD020 MTD019

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW
                   PERFORM VERIFICAR-SENHA62
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM LIMPAR-DADOS
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
               WHEN GS-LE-VENDEDOR-TRUE
                   PERFORM LE-VENDEDOR
               WHEN GS-POP-UP-VENDEDOR-TRUE
                   PERFORM POP-UP-VENDEDOR
               WHEN GS-LE-ALBUM-TRUE
                   PERFORM LE-ALBUM
               WHEN GS-LE-CIDADE-TRUE
                   PERFORM LE-CIDADE
               WHEN GS-LE-POSSE-TRUE
                   PERFORM LE-POSSE
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-VERIF-TIPO-FOGO-TRUE
                    PERFORM VERIFICA-TIPO-FOGO
               WHEN GS-CHAMAR-MTP029-TRUE
                    PERFORM CHAMAR-MTP029
               WHEN GS-VERIFICAR-CONTRATO-TRUE
                    PERFORM VERIFICA-CONTRATO
               WHEN GS-REVERTER-TRUE
                    PERFORM REVERTER-STATUS
               WHEN GS-TRATAR-EVENTO-TRUE
                    PERFORM TRATAR-EVENTOS
               WHEN GS-LER-ALBUNS-POSSE-TRUE
                    PERFORM LER-ALBUNS-POSSE
               WHEN GS-TROCAR-POSSE-TRUE
                    PERFORM TROCAR-POSSES
               WHEN GS-MARCAR-TODOS-TRUE
                    PERFORM MARCAR-TODOS
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       MARCAR-TODOS SECTION.
           INITIALIZE WSITEM MENSAGEM

           INVOKE GS-LISTVIEW "Size" RETURNING WSSIZE
           PERFORM WSSIZE TIMES
              ADD 1                         TO WSITEM

              INVOKE GS-LISTVIEW "ITEMATINDEX"
                        USING WSITEM RETURNING UMITEM

              IF GS-MARCAR = 1
                 invoke umitem "selectcheckbox"
              ELSE
                 invoke umitem "unselectCheckBox"
              END-IF
           END-PERFORM.


       TROCAR-POSSES SECTION.
           CLOSE    MTD020 MTD019 MTD023
           OPEN I-O MTD020 MTD019 MTD023

           INITIALIZE WSITEM MENSAGEM

           INVOKE GS-LISTVIEW "Size" RETURNING WSSIZE
           PERFORM WSSIZE TIMES
              ADD 1                         TO WSITEM

              INVOKE GS-LISTVIEW "ITEMATINDEX"
                        USING WSITEM RETURNING UMITEM

              INITIALIZE WSCHECKENABLE
              INVOKE UMITEM "GETCHECKBOXVALUE"
                     RETURNING WSCHECKENABLE

              IF WSCHECKENABLE = 1
      *>Contrato
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING WSCOLUNACONTRATO RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO) TO CONTRATO-MTG
      *>Album
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING WSCOLUNAALBUM RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO) TO NRALBUM-MTG

                 READ MTD020 INVALID KEY
                      MOVE "Planilha de montagem não encontrada" TO
                                  MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                 NOT INVALID KEY
                      if GS-CODIGO-POSSE > 0
                         MOVE GS-POSSE(1:1)   TO POSSE-MTG
                         MOVE GS-CODIGO-POSSE TO CODIGO-POSSE-MTG
                      end-if
                      if gs-data-romaneio > 0
                         MOVE GS-DATA-ROMANEIO       TO DATA-INV
                         CALL "GRIDAT2" USING DATA-INV
                         MOVE DATA-INV               TO DATAROMANEIO-MTG
                      end-if

                      if gs-anomes-visita > 0
           MOVE GS-ANOMES-VISITA(1: 2) TO ANOMES-VISITA-MTG(5: 2)
           MOVE GS-ANOMES-VISITA(3: 4) TO ANOMES-VISITA-MTG(1: 4)
                      end-if

                      if gs-visita > 0
                         move gs-visita to visita-mtg
                      end-if

                      REWRITE REG-MTD020 INVALID KEY
                          MOVE "Erro de Regravação...MTD020" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                      END-REWRITE
                 END-READ
              END-IF
           END-PERFORM

           CLOSE      MTD020 MTD019 MTD023
           OPEN INPUT MTD020 MTD019 MTD023

           IF MENSAGEM EQUAL SPACES
              MOVE "Álbuns atualizados com sucesso" TO MENSAGEM
              MOVE "I" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              PERFORM LER-ALBUNS-POSSE.

       TRATAR-EVENTOS SECTION.
           evaluate gs-acp-evento
               when 34123  perform chamar-colunas-favopro
           end-evaluate.

       LER-ALBUNS-POSSE SECTION.
           invoke gs-listview "DeleteAll"

           initialize reg-mtd019
           move gs-contrato        to contrato-mt19
           start mtd019 key is not less album-mt19 invalid key
                move "10" to st-mtd019.

           perform until st-mtd019 = "10"
                read mtd019 next at end
                     move "10" to st-mtd019
                not at end
                     if gs-contrato <> contrato-mt19
                        move "10" to st-mtd019
                     else
                        move albummt19 to album-mtg
                        read mtd020 not invalid key
                             if fogo-mtg <> 1 and 7 and 8
                                if nao-gerou-album-mtg <> 1
                                   perform inserir-listview
                                end-if
                             end-if
                        end-read
                     end-if
                end-read
           end-perform

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro

           invoke gs-listview "Size" returning wsSize

           if wsSize = 0
              move "Nenhum Álbum encontrado" to mensagem
              move "C" to tipo-msg
              perform exibir-mensagem
           else
              show-window win1
              invoke gs-listview "SetFocus".

       inserir-listview section.
           initialize indice
           invoke gs-listview "adicionarItem" returning wsItem

           add 1 to indice
           initialize wsTexto
           string contrato-mt19 X"00"  into wsTexto
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string seq-mt19 X"00"  into wsTexto
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string nome-form-mt19 x"00"  into wstexto
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           evaluate posse-mtg
               when 1     string "1 - Em Estoque"   x"00"  into wstexto
               when 2     string "2 - Com Vendedor" x"00"  into wstexto
               when 3     string "3 - Montagem"     x"00"  into wstexto
               when 4     string "4 - Revendido"    x"00"  into wstexto
               when other move x"00"                         to wsTexto
           end-evaluate
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string codigo-posse-mtg X"00"  into wsTexto
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           evaluate posse-mtg
                 WHEN 1 MOVE "1-Estoque"      TO GS-POSSE
                        MOVE CODIGO-POSSE-MTG TO GS-CODIGO-POSSE
                                                 CODIGO-MT02
                        READ MTD002 INVALID KEY
                             MOVE SPACES      TO NOME-MT02
                        END-READ
                        MOVE NOME-MT02        TO GS-DESC-POSSE
                 WHEN 2 MOVE "2-Vendedor"     TO GS-POSSE
                        MOVE CODIGO-POSSE-MTG TO GS-CODIGO-POSSE
                                                 CODIGO-CG01
                        READ CGD001 INVALID KEY
                             MOVE SPACES      TO NOME-CG01
                        END-READ
                        MOVE NOME-CG01        TO GS-DESC-POSSE
                 WHEN 3 MOVE "3-Montagem"     TO GS-POSSE
                        MOVE ZEROS            TO GS-CODIGO-POSSE
                        MOVE SPACES           TO GS-DESC-POSSE
                 WHEN 4 MOVE "4-Revendido"    TO GS-POSSE
                        MOVE CODIGO-POSSE-MTG TO GS-CODIGO-POSSE
                                                 CODIGO-CG01
                        READ CGD001 INVALID KEY
                             MOVE SPACES      TO NOME-CG01
                        END-READ
                        MOVE NOME-CG01        TO GS-DESC-POSSE
                 WHEN OTHER MOVE SPACES TO GS-POSSE
                            MOVE ZEROS  TO GS-CODIGO-POSSE
                            MOVE SPACES TO GS-DESC-POSSE
           end-evaluate
           string gs-desc-posse x"00" delimited by "  " into wsTexto
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           if fogo-mtg = 9
              string "Fogo" x"00"  into wsTexto
           else
              move X"00" to wstexto
           end-if
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto.

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Contrato" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)
                         WSCOLUNACONTRATO
      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Álbum" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)
                         WSCOLUNAALBUM
      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Nome" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Posse" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)
                         WSCOLUNAPOSSE

      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Código" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)
                         WSCOLUNACODPOSSE

      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Nome Posse" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Status" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

          perform mostrar-fonte-favoPro
          perform mostrar-colunas-favoPro

          invoke gs-listview "gridLines"
          invoke gs-listview "checkboxes"
          invoke gs-listview "noBorder".

       mostrar-colunas-favoPro section.
          initialize wsTexto
          move "listview-mtp023" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview
                                  wsTexto
                                  lnktabelaPro.
       mostrar-colunas-favoPro-fim.
           exit.

       mostrar-fonte-favoPro section.
           move "listview-mtp023" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview wsTexto.
       mostrar-fonte-favoPro-fim.
           exit.

       EXPORTAR-PARA-EXCEL-PRO section.
           invoke aListview "ExportarParaOExcel"
                    using gs-listview lnkTabelaPro.
       EXPORTAR-PARA-EXCEL-PRO-fim.
           EXIT.


       zebrar-itensPro section.
           move "listview-mtp023" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview wsTexto
           invoke gs-listview "redrawallitems".
       zebrar-itensPro-fim.
           exit.

       chamar-colunas-favoPro section.
           move "listview-mtp023" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview
                               wsTexto
                               lnktabelaPro

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro.
       chamar-colunas-favoPro-fim.
           exit.


       VERIFICAR-SENHA62 SECTION.
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA62"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
                DISABLE-OBJECT PB3
           NOT INVALID KEY
                ENABLE-OBJECT PB3
           END-READ.
       VERIFICAR-SENHA62-FIM.
           EXIT.

       REVERTER-STATUS SECTION.
           MOVE GS-CONTRATO     TO CONTRATO-MTG.
           MOVE GS-NR-ALBUM     TO NRALBUM-MTG
           READ MTD020 INVALID KEY
                MOVE "Álbum não Encontrado" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                OPEN I-O   MTD020
                CLOSE      MTD020

                MOVE 0 TO FOGO-MTG

                REWRITE REG-MTD020 INVALID KEY
                    MOVE "Erro de Regração...MTD020" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-REWRITE

                CLOSE      MTD020
                OPEN INPUT MTD020.

       REVERTER-STATUS-FIM.
           EXIT.

       VERIFICA-CONTRATO SECTION.
           MOVE GS-CONTRATO            TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE "Número do Contrato Não Cadastrado no (COP040)"
                  TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.

       CENTRALIZAR SECTION.
          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-MTP029 SECTION.
           CALL   "MTP029a" USING PARAMETROS-W GS-CONTRATO GS-NR-ALBUM
           CANCEL "MTP029a"

           MOVE GS-CONTRATO         TO CONTRATO-MT19
           MOVE GS-NR-ALBUM         TO SEQ-MT19
           READ MTD019 INVALID KEY
                MOVE SPACES         TO NOME-FORM-MT19
           END-READ
           MOVE NOME-FORM-MT19      TO GS-NOME-FORMANDO
                                       GS-NOME-FORM
           MOVE CIDADE-MT19         TO GS-CIDADE
           PERFORM LE-CIDADE
           MOVE FONE-MT19           TO GS-FONE
           MOVE UF-MT19             TO GS-ESTADO.

           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       LE-VENDEDOR SECTION.
           IF GS-VENDEDOR-1 > 0
              MOVE GS-VENDEDOR-1 TO CODIGO-CG01
              READ CGD001 INVALID KEY
                   MOVE "Vendedor Inválido" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
                   MOVE ZEROS TO GS-VENDEDOR-1
                   MOVE SPACES TO GS-DESC-VENDEDOR1
               NOT INVALID KEY
                IF T-VEND-CG01 = 1
                   MOVE NOME-CG01 TO GS-DESC-VENDEDOR1
                   PERFORM SET-UP-FOR-REFRESH-SCREEN
                ELSE
                   MOVE "Código Informado Incompatível com Vendedor" TO
                   MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       POP-UP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR-1
           MOVE GS-VENDEDOR-1 TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE "Vendedor Inválido" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
                MOVE ZEROS TO GS-VENDEDOR-1
                MOVE SPACES TO GS-DESC-VENDEDOR1
            NOT INVALID KEY
             IF T-VEND-CG01 = 1
                MOVE NOME-CG01 TO GS-DESC-VENDEDOR1
                PERFORM SET-UP-FOR-REFRESH-SCREEN
             ELSE
                MOVE "Código Informado Incompatível com Vendedor" TO
                MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.


       CHAMAR-POP-UP SECTION.
           MOVE GS-POSSE(1: 1)  TO POSSE-W(1: 1)
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO
                    MOVE PASSAR-STRING-1(22: 11) TO GS-IDENTIFICACAO
             WHEN 2 CALL   "MTP019T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "MTP019T"
                    MOVE PASSAR-STRING-1(45: 4) TO GS-NR-ALBUM
                    MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-FORMANDO
             WHEN 3 CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CAP010T"
                    MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE CIDADE
                    PERFORM LE-CIDADE
             WHEN 4 IF POSSE-W = 1
                       CALL   "MTP002T" USING PARAMETROS-W
                                              PASSAR-STRING-1
                       CANCEL "MTP002T"
                       MOVE PASSAR-STRING-1(16: 2) TO GS-CODIGO-POSSE
                       MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-POSSE
                    ELSE
                       CALL   "CGP001T" USING PARAMETROS-W
                                              PASSAR-STRING-1
                       CANCEL "CGP001T"
                       MOVE PASSAR-STRING-1(33: 6) TO GS-CODIGO-POSSE
                       MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-POSSE
                    END-IF
           END-EVALUATE.
       LE-CIDADE SECTION.
           MOVE GS-CIDADE   TO CIDADE.
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID    TO GS-NOME-CID.
           MOVE UF-CID      TO GS-ESTADO.
       LE-POSSE SECTION.
           MOVE GS-POSSE(1: 1) TO POSSE-W
           IF POSSE-W = 1
              MOVE GS-CODIGO-POSSE TO CODIGO-MT02
              READ MTD002 INVALID KEY MOVE SPACES TO NOME-MT02
              END-READ
              MOVE NOME-MT02   TO GS-DESC-POSSE
           ELSE
              MOVE GS-CODIGO-POSSE TO CODIGO-CG01
              READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
              END-READ
              MOVE NOME-CG01   TO GS-DESC-POSSE
              IF T-VEND-CG01 = 0
                 MOVE "Não é Vendedor" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
           END-IF.

       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE "******" TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO.
       LE-ALBUM SECTION.
           MOVE GS-CONTRATO        TO CONTRATO-MT19 ALBUM-MTG(1: 4)
           MOVE GS-NR-ALBUM        TO SEQ-MT19 ALBUM-MTG(5: 4)
           READ MTD019 INVALID KEY
              MOVE "NÃO CADASTRADO" TO NOME-FORM-MT19
              NOT INVALID KEY
               READ MTD020 INVALID KEY MOVE "NÃO CADASTRADO NA PLANILHA"
                    TO NOME-FORM-MT19.
           MOVE NOME-FORM-MT19    TO GS-NOME-FORMANDO.

       VERIFICA-TIPO-FOGO SECTION.
           MOVE GS-FOGO(1: 1)   TO TIPO-FOGO
           IF TIPO-FOGO = 8 OR 9 AND GS-DATA-FOGO = ZEROS
              MOVE DATA-DIA-W    TO GS-DATA-FOGO
           ELSE
              MOVE ZEROS         TO GS-DATA-FOGO.
      *----------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           MOVE GS-CONTRATO     TO CONTRATO-MTG.
           MOVE GS-NR-ALBUM     TO NRALBUM-MTG
           READ MTD020 INVALID KEY CONTINUE
                NOT INVALID KEY
                  IF FOGO-MTG = 1 OR FOGO-MTG = 8
                     MOVE 1 TO GS-FLAG-CRITICA
                     MOVE SPACES TO GS-TEXTO
                     STRING "ÁLBUM VENDIDO" X"0DA0"
                            "STATUS => " FOGO-MTG X"0DA0"
                            "1 - Vendido" X"0DA0"
                            "8 - Vendido/Fogo" INTO GS-TEXTO
                     MOVE "ALBUM-VENDIDO" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  ELSE
                     IF NAO-GEROU-ALBUM-MTG = 1
                        MOVE 1 TO GS-FLAG-CRITICA
                        MOVE "SEM-ALBUM" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     ELSE
                        MOVE GS-CONTRATO TO CONTRATO-MTG3
                        MOVE GS-NR-ALBUM TO SEQ-MTG3
                        READ MTD023 INVALID KEY
                             MOVE ZEROS              TO GS-VENDEDOR-1
                                                        GS-DT-1-VISITA
                             MOVE SPACES           TO GS-DESC-VENDEDOR1
                             MOVE "N" TO GS-ACHEI
                        NOT INVALID KEY
                             MOVE VENDEDOR-MTG3       TO GS-VENDEDOR-1
                                                     CODIGO-CG01
                             READ CGD001 INVALID KEY

                                  MOVE SPACES      TO GS-DESC-VENDEDOR1
                             NOT INVALID KEY
                                  MOVE NOME-CG01   TO GS-DESC-VENDEDOR1
                             END-READ
                             MOVE DATA-MTG3           TO DATA-INV
                             CALL "GRIDAT1" USING DATA-INV
                             MOVE DATA-INV            TO GS-DT-1-VISITA
                             MOVE "S" TO GS-ACHEI
                        END-READ
                        PERFORM CONT-CARREGAR-DADOS.

       CONT-CARREGAR-DADOS SECTION.
           MOVE GS-CONTRATO         TO NR-CONTRATO-CO40
                                       CONTRATO-MT19
           READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40  TO GS-IDENTIFICACAO.
           MOVE GS-NR-ALBUM         TO SEQ-MT19.
           READ MTD019 INVALID KEY MOVE SPACES TO NOME-FORM-MT19.
           MOVE NOME-FORM-MT19      TO GS-NOME-FORMANDO GS-NOME-FORM.
           MOVE CIDADE-MT19         TO GS-CIDADE
           PERFORM LE-CIDADE
           MOVE FONE-MT19           TO GS-FONE
           MOVE UF-MT19             TO GS-ESTADO
           MOVE QT-ESTOJO-MTG       TO GS-QT-ESTOJO
           MOVE QT-ENCADER-MTG      TO GS-QT-ENCADER
           MOVE QT-FOLHAS-MTG       TO GS-QT-FOLHA
           MOVE QT-FOTOS-MTG        TO GS-QT-FOTO
           MOVE QT-FITAS-MTG        TO GS-QT-FITA
           MOVE QT-DVD-MTG          TO GS-QT-DVD
           MOVE QT-POSTER-MTG       TO GS-QT-POSTER
           MOVE QT-PORTA-FITA-MTG   TO GS-QT-PORTA-FITA
           MOVE QT-FOTO-CD-MTG      TO GS-QT-FOTO-CD
           MOVE QT-PORTA-DVD-MTG    TO GS-QT-PORTA-DVD
           MOVE QT-MOLDURA-MTG      TO GS-QT-MOLDURA

           MOVE DATAROMANEIO-MTG    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV            TO GS-DATA-ROMANEIO

           MOVE SPACES              TO GS-OBS-VENDA

           MOVE 0 TO GS-TP-FOGO
           EVALUATE FOGO-MTG
             WHEN 0 MOVE "0-Montagem    "    TO GS-FOGO
      *      WHEN 1 MOVE "1-Vendido     "    TO GS-FOGO
      *      WHEN 8 MOVE "8-Vendido-Fogo"    TO GS-FOGO
             WHEN 7 MOVE "Vendido Vanderlei" TO GS-OBS-VENDA
             WHEN 9 MOVE "9-Fogo        "    TO GS-FOGO
                    MOVE 1                   TO GS-TP-FOGO
           END-EVALUATE
           MOVE DATA-FOGO-MTG             TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                  TO GS-DATA-FOGO

           MOVE ANOMES-VISITA-MTG(1: 4)   TO MESANO-W(3: 4)
           MOVE ANOMES-VISITA-MTG(5: 2)   TO MESANO-W(1: 2)
           MOVE MESANO-W            TO GS-ANOMES-VISITA
           MOVE VISITA-MTG          TO GS-VISITA.
           EVALUATE POSSE-MTG
             WHEN 1 MOVE "1-Estoque"      TO GS-POSSE
                    MOVE CODIGO-POSSE-MTG TO GS-CODIGO-POSSE CODIGO-MT02
                    READ MTD002 INVALID KEY MOVE SPACES TO NOME-MT02
                    END-READ
                    MOVE NOME-MT02        TO GS-DESC-POSSE
             WHEN 2 MOVE "2-Vendedor"     TO GS-POSSE
                    MOVE CODIGO-POSSE-MTG TO GS-CODIGO-POSSE CODIGO-CG01
                    READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                    END-READ
                    MOVE NOME-CG01        TO GS-DESC-POSSE
             WHEN 3 MOVE "3-Montagem"     TO GS-POSSE
                    MOVE ZEROS            TO GS-CODIGO-POSSE
                    MOVE SPACES           TO GS-DESC-POSSE
             WHEN 4 MOVE "4-Revendido"    TO GS-POSSE
                    MOVE CODIGO-POSSE-MTG TO GS-CODIGO-POSSE CODIGO-CG01
                    READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                    END-READ
                    MOVE NOME-CG01        TO GS-DESC-POSSE
             WHEN OTHER MOVE SPACES       TO GS-DESC-POSSE
                        MOVE ZEROS        TO GS-CODIGO-POSSE
                        MOVE SPACES       TO GS-DESC-POSSE
           END-EVALUATE.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-MTD019.
           INITIALIZE REG-MTD020.
           INITIALIZE REG-MTD023.
           MOVE GS-CONTRATO TO CONTRATO-W
           MOVE GS-IDENTIFICACAO TO IDENTIFICACAO-W
           MOVE GS-DATA-ROMANEIO  TO DATA-ROMANEIO-W
           INITIALIZE GS-DATA-BLOCK
           MOVE CONTRATO-W TO GS-CONTRATO.
           MOVE IDENTIFICACAO-W TO GS-IDENTIFICACAO.
           MOVE DATA-ROMANEIO-W TO GS-DATA-ROMANEIO
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       SALVAR-DADOS SECTION.
           CLOSE    MTD020 MTD019 MTD023
           OPEN I-O MTD020 MTD019 MTD023
           MOVE GS-CONTRATO           TO CONTRATO-MTG
           MOVE GS-NR-ALBUM           TO NRALBUM-MTG
           READ MTD020 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                PERFORM MOVER-DADOS-MTD020
                REWRITE REG-MTD020
                END-REWRITE

                MOVE ALBUM-MTG TO ALBUM-MT19
                READ MTD019 INVALID KEY
                     CONTINUE
                NOT INVALID KEY
                     PERFORM MOVER-DADOS-MTD019
                     REWRITE REG-MTD019
                     END-REWRITE
                END-READ
           END-READ

           MOVE "N" TO GS-ACHEI

           IF GS-VENDEDOR-1 > 0 AND GS-DT-1-VISITA > 0
              MOVE GS-VENDEDOR-1 TO CODIGO-CG01
              READ CGD001 NOT INVALID KEY
                 IF T-VEND-CG01 = 1
                    MOVE GS-CONTRATO            TO CONTRATO-MTG3
                    MOVE GS-NR-ALBUM            TO SEQ-MTG3
                    READ MTD023 INVALID KEY
                         MOVE "S" TO GS-ACHEI
                         MOVE GS-VENDEDOR-1      TO VENDEDOR-MTG3
                         MOVE GS-DT-1-VISITA     TO DATA-INV
                         CALL "GRIDAT2" USING DATA-INV
                         MOVE DATA-INV           TO DATA-MTG3
                         WRITE REG-MTD023
                         END-WRITE.

           CLOSE      MTD020 MTD019 MTD023
           OPEN INPUT MTD020 MTD019 MTD023.

       MOVER-DADOS-MTD020 SECTION.
           MOVE GS-DATA-FOGO           TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV               TO DATA-FOGO-MTG

           MOVE GS-DATA-ROMANEIO       TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV               TO DATAROMANEIO-MTG

           MOVE GS-FOGO(1: 1)          TO FOGO-MTG
           MOVE GS-ANOMES-VISITA(1: 2) TO ANOMES-VISITA-MTG(5: 2)
           MOVE GS-ANOMES-VISITA(3: 4) TO ANOMES-VISITA-MTG(1: 4)
           MOVE GS-VISITA              TO VISITA-MTG.
           MOVE FUNCTION NUMVAL(GS-POSSE(1: 1))  TO POSSE-MTG.
           MOVE GS-CODIGO-POSSE        TO CODIGO-POSSE-MTG.
       MOVER-DADOS-MTD019 SECTION.
           MOVE GS-CIDADE       TO CIDADE-MT19
           MOVE GS-NOME-FORM    TO NOME-FORM-MT19
           MOVE GS-ESTADO       TO UF-MT19
           MOVE GS-FONE         TO FONE-MT19.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to gs-flag-critica
           move spaces to mensagem.


       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP023" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040 CGD001 MTD002 MTD019 MTD020 MTD023
                 CAD004.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
