       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP040.
      *DATA: 20/07/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: CONFERENCIA DE ETIQUETAS DE FITA DE VIDEO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass"
           AListview          is class "alistview".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY IEPX011.
           COPY MTPX019.
           COPY MTPX025.
           COPY MTPX030.
           COPY COPX003.
           COPY COPX008.
           COPY COPX040.
           COPY COPX060.
           COPY CGPX011.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS ALBUM-WK
                  ALTERNATE RECORD KEY IS NOME-WK
                        WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK =
                       CIDADE-WK NOME-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK =
                       CURSO-WK NOME-WK WITH DUPLICATES
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-WORK.

           SELECT ARQUI ASSIGN TO ARQUIVO-IMPRESSAO
                        ORGANIZATION IS LINE SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL
                        FILE STATUS IS FS-ARQUI.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT WORK2 ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS CHAVE-WK2 = NOME-CURSO-WK2.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CGPW011.
       COPY IEPW011.
       COPY MTPW019.
       COPY MTPW025.
       COPY MTPW030.
       COPY COPW003.
       COPY COPW008.
       COPY COPW040.
       COPY COPW060.
       FD  WORK.
       01  REG-WORK.
           05  ALBUM-WK            PIC 9(8).
           05  NOME-WK             PIC X(30).
           05  CIDADE-WK           PIC X(13).
           05  CURSO-WK            PIC X(30).
           05  DATA-WK             PIC 9(8).
           05  IDENTIFICADO-WK     PIC X(3).
           05  MOTIVO-WK           PIC X(20).
           05  EVENTO-WK           PIC X(30).
           05  QTDE-IMAGENS-WK     PIC 9(6).
       FD  ARQUI
           LABEL RECORD IS OMITTED.
       01  REG-ARQUI.
           05  FILLER              PIC X(130).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       FD  WORK2.
       01  REG-WORK2.
           05 COD-CURSO-WK2             PIC 999.
           05 NOME-CURSO-WK2            PIC X(12).

       WORKING-STORAGE SECTION.
           COPY "MTP040.CPB".
           COPY "MTP040.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       01  NOME-IMPRESSORA           PIC X(100).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD025             PIC XX       VALUE SPACES.
           05  ST-MTD030             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD008             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  FS-ARQUI              PIC XX       VALUE SPACES.
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
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  QTDE-E                PIC ZZ.ZZZ.ZZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  TOT-FORMANDO          PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
           05  COMPACTA              PIC X(01)    VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  CONTRATO-REL          PIC 9(4)    VALUE ZEROS.
           05  AUX-DATA              PIC 9(08).
           05  MASC-QTDE             PIC ZZZ.ZZ9 BLANK WHEN ZEROS.
           05  AUX-EVENTO            PIC 9(5)    VALUE ZEROS.

           COPY "PARAMETR".
           COPY IMPRESSORA.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

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
           05  FILLER              PIC X(50)   VALUE
           "CONFERENCIA ETIQUETA DE FITA DE VIDEO - ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.

       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA           ALBUM CURSO                          NOME
      -    "                      CIDADE               IDENT".
       01  LINDET.
           05  LINDET-REL          PIC X(128)  VALUE SPACES.
       01  LINTOT.
           05  LINTOT-REL          PIC X(128)  VALUE SPACES.

       01 status-code           PIC X(2) COMP-5.

       01 indice                   pic 9(02).
       01 wssize                   pic 9(09) comp-5 value zeros.
       01 wsIndice                 pic 9(09) comp-5 value zeros.
       77 wsTexto                  pic x(255) value spaces.
       77 wsItem                   pic 9(009) comp-5 value zeros.
       01 UMITEM                   OBJECT REFERENCE VALUE NULL.
       01 UMOBJETO                 OBJECT REFERENCE VALUE NULL.

       01 wsColAlbum               pic 9(009) comp-5 value zeros.

       01 lnktabela.
          02 lnkobjetoscol             object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabela2.
          02 lnkobjetoscol2            object reference occurs 99 times.
       01 lnktabelaCol2.
          02 lnkcolunas2   pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabela3.
          02 lnkobjetoscol3            object reference occurs 99 times.
       01 lnktabelaCol3.
          02 lnkcolunas3   pic 9(09) comp-5 value zeros occurs 99 times.


       01 wsColDataAlb                 pic 9(09) comp-5 value zeros.
       01 wsColNrAlb                   pic 9(09) comp-5 value zeros.
       01 wsColCursoAlb                pic 9(09) comp-5 value zeros.
       01 wsColFormAlb                 pic 9(09) comp-5 value zeros.
       01 wsColCidAlb                  pic 9(09) comp-5 value zeros.
       01 wsColIdentAlb                pic 9(09) comp-5 value zeros.
       01 wsColEventoAlb               pic 9(09) comp-5 value zeros.
       01 wsColQtdAlb                  pic 9(09) comp-5 value zeros.
       01 wsColMotAlb                  pic 9(09) comp-5 value zeros.

       01 gs-nr-album                  pic 9(08) value zeros.


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
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                    TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO       TO DS-VERSION-NO
           MOVE EMPRESA-W           TO EMP-REC
           MOVE NOME-EMPRESA-W      TO EMPRESA-REL
           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD025"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD025.
           MOVE "MTD030"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD030.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003
           MOVE "COD008"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD008
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           COMPUTE VARIA-W2 = VARIA-W + 10

           OPEN INPUT CAD010 IED011 MTD019 COD040 COD008 MTD025 CGD011
                      COD003 COD060 MTD030.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD025 <> "00"
              MOVE "ERRO ABERTURA MTD025: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD025 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD030 <> "00"
              MOVE "ERRO ABERTURA MTD030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD008 <> "00"
              MOVE "ERRO ABERTURA COD008: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD008 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW-ALB
                   PERFORM CRIAR-LISTVIEW
                   PERFORM CRIAR-LISTVIEW2
               WHEN GS-PRINTER-FLG-TRUE
                    IF GS-IMPRIMIR = "S"
                       COPY IMPRESSORA.CHAMA.
                       IF LNK-MAPEAMENTO <> SPACES
                          PERFORM IMPRIME-RELATORIO
                       END-IF
                    ELSE
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
               WHEN GS-CARREGA-CURSOS-TRUE
                    PERFORM CARREGAR-CURSOS
                    PERFORM CARREGAR-EVENTOS
               WHEN GS-INSERIR-QTDE-TRUE
                    PERFORM INSERIR-EVENTO
               WHEN GS-TRATAR-EVENTO-TRUE
                    EVALUATE GS-QUAL-LIST
                       WHEN 1 PERFORM TRATAR-EVENTO
                       WHEN 2 PERFORM TRATAR-EVENTO2
                       WHEN 3 PERFORM TRATAR-EVENTO3
                    END-EVALUATE
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       INSERIR-EVENTO SECTION.
           IF GS-ACP-QTDE-IMAGEM = 0
              MOVE "Quantidade de imagens não informada" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              SET-FOCUS EF-QTDE-IMAGEM
           ELSE
              CLOSE    MTD030
              OPEN I-O MTD030
              MOVE GS-ACP-QTDE-IMAGEM TO QTDE-IMAGENS-MT30
              WRITE REG-MTD030 INVALID KEY
                    REWRITE REG-MTD030 INVALID KEY
                            MOVE "Erro de Gravação...MTD030" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                    NOT INVALID KEY
                            PERFORM OK-EVENTO
                    END-REWRITE
              NOT INVALID KEY
                    PERFORM OK-EVENTO
              END-WRITE
              CLOSE      MTD030
              OPEN INPUT MTD030
              PERFORM ATUALIZAR-WORK.

       ATUALIZAR-WORK SECTION.
           CLOSE      WORK
           OPEN I-O   WORK

           MOVE GS-NR-ALBUM TO ALBUM-WK
           READ WORK INVALID KEY
                MOVE "Album não encontrado" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                INITIALIZE REG-MTD030
                           QTDE-IMAGENS-WK
                MOVE GS-NR-ALBUM            TO ALBUMMT30
                START MTD030 KEY IS NOT LESS CHAVE-MT30 INVALID KEY
                      MOVE "10" TO ST-MTD030
                END-START

                PERFORM UNTIL ST-MTD030 = "10"
                      READ MTD030 NEXT AT END
                           MOVE "10" TO ST-MTD030
                      NOT AT END
                           IF GS-NR-ALBUM <> ALBUMMT30
                              MOVE "10" TO ST-MTD030
                           ELSE
                              IF AUX-EVENTO = 0 OR COD-EVENTO-MT30
                                 ADD QTDE-IMAGENS-MT30 TO
                                     QTDE-IMAGENS-WK
                              END-IF
                           END-IF
                      END-READ
                END-PERFORM

                REWRITE REG-WORK INVALID KEY
                        MOVE "Erro de Regravação...WORK" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ

           CLOSE      WORK
           OPEN INPUT WORK

           PERFORM CARREGA-LISTA.

       OK-EVENTO SECTION.
           INITIALIZE INDICE
           INVOKE GS-ACP-LISTVIEW-EVE "ADICIONARITEM"
                  RETURNING WSITEM

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE GS-ACP-QTDE-IMAGEM  TO MASC-QTDE
           STRING MASC-QTDE X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

           MOVE COD-EVENTO-MT30     TO CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE "*******"      TO NOME-CO03
           END-READ

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING NOME-CO03 X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING COD-EVENTO-MT30 X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING ITEM-EVENTO-MT30 X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

           PERFORM MOSTRAR-COLUNAS-FAVO2
           PERFORM MOSTRAR-FONTE-FAVO2
           PERFORM ZEBRAR-ITENS2.


       CRIAR-LISTVIEW-ALB SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-alb "adicionarColunaZ"
            using z"Data" returning lnkobjetoscol3(indice)
          invoke lnkobjetoscol3(indice) "centered"
          move indice to lnkcolunas3(indice)
                         wsColDataAlb
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-alb "adicionarColunaZ"
            using z"Álbum" returning lnkobjetoscol3(indice)
          invoke lnkobjetoscol3(indice) "centered"
          move indice to lnkcolunas3(indice)
                         wsColNrAlb
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-alb "adicionarColunaZ"
            using z"Curso" returning lnkobjetoscol3(indice)
          move indice to lnkcolunas3(indice)
                         wsColCursoAlb
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-alb "adicionarColunaZ"
            using z"Formando" returning lnkobjetoscol3(indice)
          move indice to lnkcolunas3(indice)
                         wsColFormAlb
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-alb "adicionarColunaZ"
            using z"Cidade" returning lnkobjetoscol3(indice)
          move indice to lnkcolunas3(indice)
                         wsColCidAlb
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-alb "adicionarColunaZ"
            using z"Ident" returning lnkobjetoscol3(indice)
          invoke lnkobjetoscol3(indice) "centered"
          move indice to lnkcolunas3(indice)
                         wsColIdentAlb
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-alb "adicionarColunaZ"
            using z"Evento" returning lnkobjetoscol3(indice)
          invoke lnkobjetoscol3(indice) "centered"
          move indice to lnkcolunas3(indice)
                         wsColEventoAlb

      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-alb "adicionarColunaZ"
            using z"Qtde Imag" returning lnkobjetoscol3(indice)
          invoke lnkobjetoscol3(indice) "RightJustified"
          move indice to lnkcolunas3(indice)
                         wsColQtdAlb

      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-alb "adicionarColunaZ"
            using z"Motivo" returning lnkobjetoscol3(indice)
          move indice to lnkcolunas3(indice)
                         wsColMotAlb

          perform mostrar-fonte-favo3
          perform mostrar-colunas-favo3

          invoke gs-acp-listview-alb "gridLines"
          invoke gs-acp-listview-alb "noBorder".

       mostrar-colunas-favo3 section.
          initialize wsTexto
          move "listview-mtp019-3" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview-alb
                                  wsTexto
                                  lnktabela3.
       mostrar-colunas-favo3-fim.
           exit.

       mostrar-fonte-favo3 section.
           move "listview-mtp019-3" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview-alb wsTexto.
       mostrar-fonte-favo3-fim.
           exit.

       zebrar-itens3 section.
           move "listview-mtp019-3" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-acp-listview-alb wsTexto
           invoke gs-acp-listview-alb "redrawallitems".
       zebrar-itens3-fim.
           exit.

       chamar-colunas-favo3 section.
           move "listview-mtp019-3" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview-alb
                               wsTexto
                               lnktabela3

           perform mostrar-colunas-favo3
           perform mostrar-fonte-favo3
           perform zebrar-itens3.
       chamar-colunas-favo3-fim.
           exit.

       TRATAR-EVENTO SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34592  PERFORM ALBUM-ACEITO
               WHEN 34013  PERFORM ALBUM-ACEITO
               WHEN 34123  PERFORM CHAMAR-COLUNAS-FAVO3
               WHEN 34027  SET-FOCUS EF16
           END-EVALUATE.

       ALBUM-ACEITO SECTION.
           INITIALIZE WSITEM
           INVOKE GS-ACP-LISTVIEW-ALB "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
      *>Nº Album
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas3(wsColNrAlb) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-NR-ALBUM

              move gs-nr-album    to albummt19
              read mtd019 invalid key
                   move "Ficha de identificação não encontrada"
                            to mensagem
                   move "C" to tipo-msg
                   perform exibir-mensagem
              not invalid key
                   invoke gs-acp-listview "DeleteAll"
                   invoke gs-acp-listview-eve "DeleteAll"

                   PERFORM CARREGAR-EVENTOS-ALBUM

                   INITIALIZE REG-COD060
                   MOVE CONTRATO-MT19 TO NR-CONTRATO-CO60
                   START COD060 KEY IS NOT LESS CHAVE-CO60 INVALID KEY
                         MOVE "10" TO ST-COD060
                   END-START
                   PERFORM UNTIL ST-COD060 = "10"
                         READ COD060 NEXT AT END
                              MOVE "10" TO ST-COD060
                         NOT AT END
                              IF CONTRATO-MT19 <> NR-CONTRATO-CO60
                                 MOVE "10" TO ST-COD060
                              ELSE
                                 initialize indice
                                 invoke gs-acp-listview "adicionarItem"
                                              returning wsItem

                                 move codevento-co60 to codigo-co03
                                 read cod003 invalid key
                                      move "*****"   to nome-co03
                                 end-read

                                 add 1 to indice
                                 initialize wsTexto
                                 string nome-co03 X"00"
                                        delimited by "  " into wsTexto
                                 invoke gs-acp-listview
                                                      "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                                 add 1 to indice
                                 initialize wsTexto
                                 string codevento-co60 X"00"
                                        delimited by "  " into wsTexto
                                 invoke gs-acp-listview
                                                      "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                                 add 1 to indice
                                 initialize wsTexto
                                 string item-co60 X"00"
                                        delimited by "  " into wsTexto
                                 invoke gs-acp-listview
                                                      "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto
                              END-IF
                         END-READ
                   END-PERFORM

                   PERFORM MOSTRAR-COLUNAS-FAVO
                   PERFORM MOSTRAR-FONTE-FAVO
                   PERFORM ZEBRAR-ITENS

                   show-window win4
                   show-window win5
                   invoke gs-acp-listview "Setfocus".


       TRATAR-EVENTO2 SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34592  PERFORM EVENTO-ACEITO
               WHEN 34013  PERFORM EVENTO-ACEITO
               WHEN 34123  PERFORM CHAMAR-COLUNAS-FAVO
               WHEN 34027  UNSHOW-WINDOW WIN4 PRINCIPAL
                           SET-FOCUS EF16
           END-EVALUATE.

       EVENTO-ACEITO SECTION.
           INITIALIZE WSITEM
           INVOKE GS-ACP-LISTVIEW "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INITIALIZE REG-MTD030
              MOVE GS-NR-ALBUM         TO ALBUMMT30

      *>Código Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas(2) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO COD-EVENTO-MT30
      *>Item Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas(3) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO ITEM-EVENTO-MT30

              READ MTD030 INVALID KEY
                   MOVE ZEROS               TO QTDE-IMAGENS-MT30
              END-READ

              MOVE QTDE-IMAGENS-MT30        TO GS-ACP-QTDE-IMAGEM
              REFRESH-OBJECT WIN5
              SET-FOCUS EF-QTDE-IMAGEM.


       TRATAR-EVENTO3 SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34592  PERFORM ALTERAR-QTDE
               WHEN 34013  PERFORM ALTERAR-QTDE
               WHEN 34046  PERFORM EXCLUIR-EVENTO
               WHEN 34123  PERFORM CHAMAR-COLUNAS-FAVO2
               WHEN 34027  INVOKE GS-ACP-LISTVIEW "SetFocus"
           END-EVALUATE.

       ALTERAR-QTDE SECTION.
           INITIALIZE WSITEM
           INVOKE GS-ACP-LISTVIEW-EVE "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
      *>Quantidade Imagens
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas2(1) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-ACP-QTDE-IMAGEM
      *>Código Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas2(3) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO COD-EVENTO-MT30
      *>Item Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas2(4) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO ITEM-EVENTO-MT30

              MOVE GS-NR-ALBUM         TO ALBUMMT30
              READ MTD030 INVALID KEY
                   MOVE "Não encontrei o MTD030" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   CLOSE      MTD030
                   OPEN I-O   MTD030

                   DELETE MTD030 INVALID KEY
                          MOVE "Erro de Exclusão...MTD030" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                   NOT INVALID KEY
                          INVOKE UMITEM "Finalize" RETURNING UMITEM
                          SET-FOCUS EF-QTDE-IMAGEM
                   END-DELETE

                   CLOSE      MTD030
                   OPEN INPUT MTD030
                   PERFORM ATUALIZAR-WORK.

       EXCLUIR-EVENTO SECTION.
           INITIALIZE WSITEM
           INVOKE GS-ACP-LISTVIEW-EVE "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
      *>Código Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas2(3) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO COD-EVENTO-MT30
      *>Item Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas2(4) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO ITEM-EVENTO-MT30

              MOVE GS-NR-ALBUM         TO ALBUMMT30
              READ MTD030 INVALID KEY
                   MOVE "Não encontrei o MTD030" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   CLOSE      MTD030
                   OPEN I-O   MTD030

                   DELETE MTD030 INVALID KEY
                          MOVE "Erro de Exclusão...MTD030" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                   NOT INVALID KEY
                          INVOKE UMITEM "Finalize" RETURNING UMITEM
                          INVOKE GS-ACP-LISTVIEW "Setfocus"
                   END-DELETE

                   CLOSE      MTD030
                   OPEN INPUT MTD030
                   PERFORM ATUALIZAR-WORK.

       CARREGAR-EVENTOS-ALBUM SECTION.
           INVOKE GS-ACP-LISTVIEW-EVE "DeleteAll"

           INITIALIZE REG-MTD030
           MOVE GS-NR-ALBUM        TO ALBUMMT30
           START MTD030 KEY IS NOT LESS CHAVE-MT30 INVALID KEY
                 MOVE "10" TO ST-MTD030.

           PERFORM UNTIL ST-MTD030 = "10"
                 READ MTD030 NEXT AT END
                      MOVE "10" TO ST-MTD030
                 NOT AT END
                      IF GS-NR-ALBUM  <> ALBUMMT30
                         MOVE "10" TO ST-MTD030
                      ELSE
                         INITIALIZE INDICE
                         INVOKE GS-ACP-LISTVIEW-EVE "ADICIONARITEM"
                                RETURNING WSITEM

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE QTDE-IMAGENS-MT30 TO MASC-QTDE
                         STRING MASC-QTDE X"00" INTO WSTEXTO
                         INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

                         MOVE COD-EVENTO-MT30     TO CODIGO-CO03
                         READ COD003 INVALID KEY
                              MOVE "*******"      TO NOME-CO03
                         END-READ

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING NOME-CO03 X"00" INTO WSTEXTO
                         INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING COD-EVENTO-MT30 X"00" INTO WSTEXTO
                         INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING ITEM-EVENTO-MT30 X"00" INTO WSTEXTO
                         INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

                      END-IF
                 END-READ
           END-PERFORM

           PERFORM MOSTRAR-COLUNAS-FAVO2
           PERFORM MOSTRAR-FONTE-FAVO2
           PERFORM ZEBRAR-ITENS2.

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Evento" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Código" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Item" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke gs-acp-listview "gridLines"
          invoke gs-acp-listview "noBorder".

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-mtp019-1" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.

       mostrar-fonte-favo section.
           move "listview-mtp019-1" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       zebrar-itens section.
           move "listview-mtp019-1" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-acp-listview wsTexto
           invoke gs-acp-listview "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-mtp019-1" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.

       CRIAR-LISTVIEW2 SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-eve "adicionarColunaZ"
            using z"Qtde Imagens" returning lnkobjetoscol2(indice)
          invoke lnkobjetoscol2(indice) "RightJustified"
          move indice to lnkcolunas2(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-eve "adicionarColunaZ"
            using z"Evento" returning lnkobjetoscol2(indice)
          move indice to lnkcolunas2(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-eve "adicionarColunaZ"
            using z"Código" returning lnkobjetoscol2(indice)
          invoke lnkobjetoscol2(indice) "centered"
          move indice to lnkcolunas2(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-eve "adicionarColunaZ"
            using z"Item" returning lnkobjetoscol2(indice)
          invoke lnkobjetoscol2(indice) "centered"
          move indice to lnkcolunas2(indice)

          perform mostrar-fonte-favo2
          perform mostrar-colunas-favo2

          invoke gs-acp-listview-eve "gridLines"
          invoke gs-acp-listview-eve "noBorder".

       mostrar-colunas-favo2 section.
          initialize wsTexto
          move "listview-mtp019-2" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview-eve
                                  wsTexto
                                  lnktabela2.
       mostrar-colunas-favo2-fim.
           exit.

       mostrar-fonte-favo2 section.
           move "listview-mtp019-2" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview-eve wsTexto.
       mostrar-fonte-favo2-fim.
           exit.

       zebrar-itens2 section.
           move "listview-mtp019-2" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-acp-listview-eve wsTexto
           invoke gs-acp-listview-eve "redrawallitems".
       zebrar-itens2-fim.
           exit.

       chamar-colunas-favo2 section.
           move "listview-mtp019-2" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview-eve
                               wsTexto
                               lnktabela2

           perform mostrar-colunas-favo2
           perform mostrar-fonte-favo2
           perform zebrar-itens2.
       chamar-colunas-favo2-fim.
           exit.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win4 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       CARREGAR-CURSOS SECTION.
           OPEN OUTPUT WORK2
           CLOSE       WORK2
           OPEN I-O    WORK2

           INITIALIZE REG-MTD019
           MOVE GS-CONTRATO   TO CONTRATO-MT19.
           MOVE ZEROS         TO SEQ-MT19.
           START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                  MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
             READ MTD019 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD019
             NOT AT END
                 IF CONTRATO-MT19 <> GS-CONTRATO
                    MOVE "10" TO ST-MTD019
                 ELSE
                    MOVE CURSO-MT19        TO CODIGO-IE11
                    READ IED011 INVALID KEY
                         MOVE SPACES TO NOME-REDUZ-IE11
                    END-READ
                    MOVE NOME-REDUZ-IE11   TO NOME-CURSO-WK2
                    MOVE CURSO-MT19        TO COD-CURSO-WK2
                    WRITE REG-WORK2
                    END-WRITE
                 END-IF
             END-READ

           END-PERFORM

           MOVE "TODOS"           TO NOME-CURSO-WK2
           MOVE 0                 TO COD-CURSO-WK2
           WRITE REG-WORK2



           MOVE 20 TO GS-CONT

           MOVE "LIMPAR-SB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK GS-CONT
           MOVE SPACES TO NOME-CURSO-WK2

           START WORK2 KEY IS NOT LESS CHAVE-WK2 INVALID KEY
               MOVE "10" TO ST-WORK2.

           PERFORM UNTIL ST-WORK2 = "10"
               READ WORK2 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK2
               NOT AT END
                   ADD  1              TO GS-CONT
                   MOVE SPACES         TO GS-CURSO
                   MOVE COD-CURSO-WK2  TO GS-CURSO(16:3)
                   MOVE NOME-CURSO-WK2 TO GS-CURSO(1:15)
                   MOVE "INSERIR-SB"  TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM

           CLOSE WORK2.

       CARREGAR-EVENTOS SECTION.
           MOVE "APAGAR-EVENTOS"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-COD060
                      GS-CONT
           MOVE GS-CONTRATO          TO NR-CONTRATO-CO60
           START COD060 KEY IS NOT LESS CHAVE-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.

           PERFORM UNTIL ST-COD060 = "10"
                 READ COD060 NEXT AT END
                      MOVE "10"      TO ST-COD060
                 NOT AT END
                      IF GS-CONTRATO <> NR-CONTRATO-CO60
                         MOVE "10"   TO ST-COD060
                      ELSE
                         MOVE CODEVENTO-CO60 TO CODIGO-CO03
                         READ COD003 INVALID KEY
                              MOVE "******"  TO NOME-CO03
                         END-READ
                         ADD 1                  TO GS-CONT

                         MOVE SPACES            TO GS-DESC-EVENTO
                         STRING NOME-CO03 "-" CODIGO-CO03
                                              INTO GS-DESC-EVENTO
                         MOVE "INSERIR-EVENTOS" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM

                      END-IF
                 END-READ
           END-PERFORM

           ADD 1                              TO GS-CONT
           MOVE SPACES                        TO GS-DESC-EVENTO
           MOVE ZEROS                         TO CODIGO-CO03
           MOVE "TODOS"                       TO NOME-CO03
           STRING NOME-CO03 "-" CODIGO-CO03 INTO GS-DESC-EVENTO
           MOVE "INSERIR-EVENTOS"             TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CHAMAR-POPUP SECTION.
           CALL "COP040T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP040T".
           MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO.
           PERFORM LE-CONTRATO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO.
      *------------------------------------------------------------
       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE FUNCTION NUMVAL(GS-DESC-EVENTO(22:5)) TO AUX-EVENTO

           MOVE GS-CONTRATO   TO CONTRATO-MT19.
           MOVE ZEROS         TO SEQ-MT19.
           START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                  MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
             READ MTD019 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD019
             NOT AT END
                  IF CONTRATO-MT19 <> GS-CONTRATO
                     MOVE "10" TO ST-MTD019
                  ELSE
                     STRING "0" ALBUMMT19 INTO COD-COMPL-CG11
                     READ CGD011 INVALID KEY
                          INITIALIZE REG-CGD011
                     END-READ
                     IF GS-SEXO(1:1) = SPACES OR SEXO2-CG11
                        IF GS-CURSO(16:3) = "000" OR SPACES OR
                           CURSO-MT19
                           IF GS-IDENTIFICADO = 3 OR IDENTIFICADO-MT19

                              MOVE ALBUM-MT19          TO GS-EXIBE-MOVTO
                              MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
                              PERFORM CALL-DIALOG-SYSTEM
                              MOVE ALBUM-MT19          TO ALBUM-WK
                              MOVE NOME-FORM-MT19      TO NOME-WK
                              MOVE CIDADE-MT19         TO CIDADE
                              READ CAD010 INVALID KEY
                                   MOVE SPACES TO NOME-CID
                              END-READ
                              MOVE NOME-CID            TO CIDADE-WK
                              MOVE CURSO-MT19          TO CODIGO-IE11
                              READ IED011 INVALID KEY
                                   MOVE SPACES TO NOME-IE11
                              END-READ
                              MOVE NOME-IE11          TO CURSO-WK
                              MOVE DATAMOV-MT19       TO DATA-INV
                              CALL "GRIDAT1" USING DATA-INV
                              MOVE DATA-INV           TO DATA-WK
                              EVALUATE IDENTIFICADO-MT19
                                 WHEN 1 MOVE "SIM"    TO IDENTIFICADO-WK
                                        MOVE SPACES TO MOTIVO-WK
                                 WHEN 0 MOVE "NAO"    TO IDENTIFICADO-WK
                                        MOVE SPACES TO MOTIVO-WK
                                        PERFORM VER-MOTIVO
                                 WHEN OTHER
                                        MOVE SPACES TO IDENTIFICADO-WK
                                        MOVE SPACES TO MOTIVO-WK
                              END-EVALUATE
                              MOVE GS-DESC-EVENTO(1:20) TO EVENTO-WK
                              MOVE ZEROS            TO QTDE-IMAGENS-WK
                              INITIALIZE REG-MTD030
                              MOVE ALBUM-WK         TO ALBUMMT30
                              START MTD030 KEY IS NOT LESS CHAVE-MT30
                                                             INVALID KEY
                                    MOVE "10" TO ST-MTD030
                              END-START
                              PERFORM UNTIL ST-MTD030 = "10"
                                   READ MTD030 NEXT AT END
                                        MOVE "10" TO ST-MTD030
                                   NOT AT END
                                        IF ALBUM-WK <> ALBUM-MT30
                                           MOVE "10" TO ST-MTD030
                                        ELSE
                                           IF AUX-EVENTO = 0 OR
                                              COD-EVENTO-MT30
                                              ADD QTDE-IMAGENS-MT30
                                               TO QTDE-IMAGENS-WK
                                           END-IF
                                        END-IF
                                   END-READ
                              END-PERFORM
                              IF AUX-EVENTO = 0 OR
                                 QTDE-IMAGENS-WK > 0
                                 WRITE REG-WORK
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                 END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
      *---------------------------------------------------
       VER-MOTIVO SECTION.
           INITIALIZE REG-MTD025 AUX-DATA
           MOVE CONTRATO-MT19      TO CONTRATO-MT25
           MOVE SEQ-MT19           TO ALBUM-MT25
           START MTD025 KEY IS NOT LESS CHAVE1-MT25 INVALID KEY
               MOVE "10" TO ST-MTD025.
           PERFORM UNTIL ST-MTD025 = "10"
               READ MTD025 NEXT RECORD AT END
                   MOVE "10" TO ST-MTD025
               NOT AT END
               IF CONTRATO-MT19 <> CONTRATO-MT25 OR
                  SEQ-MT19      <> ALBUM-MT25
                  MOVE "10" TO ST-MTD025
               ELSE
                  IF DATA-MT25 > AUX-DATA OR DATA-MT25 = AUX-DATA
                     MOVE DATA-MT25   TO AUX-DATA
                     MOVE MOTIVO-MT25 TO CODIGO-CO08
                     READ COD008 INVALID KEY
                           MOVE "*****" TO MOTIVO-WK
                     NOT INVALID KEY
                           MOVE DESCRICAO-CO08 TO MOTIVO-WK
                     END-READ.

       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-FORMANDO
                         GS-TOT-IMAGENS

           INVOKE GS-ACP-LISTVIEW-ALB "DeleteAll"

           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA
           REFRESH-OBJECT PRINCIPAL.

       MOVER-DADOS-LINDET SECTION.
           ADD 1                  TO TOT-FORMANDO
           ADD QTDE-IMAGENS-WK    TO GS-TOT-IMAGENS
           MOVE DATA-WK           TO DATA-E
           MOVE QTDE-IMAGENS-WK   TO MASC-QTDE

           INITIALIZE INDICE
           INVOKE GS-ACP-LISTVIEW-ALB "ADICIONARITEM"
                  RETURNING WSITEM

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING DATA-E X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-ALB "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING ALBUM-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-ALB "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING CURSO-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-ALB "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING NOME-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-ALB "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING CIDADE-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-ALB "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING IDENTIFICADO-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-ALB "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING EVENTO-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-ALB "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO


           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING MASC-QTDE X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-ALB "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING MOTIVO-WK X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-ALB "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS3(INDICE) WSTEXTO

          perform mostrar-fonte-favo3
          perform mostrar-colunas-favo3
          perform zebrar-itens3.


       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 0
                MOVE "ALBUM        " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALBUM-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 1
                MOVE "CURSO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT2-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "NOME-FORMANDO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < NOME-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CIDADE" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE TOT-FORMANDO      TO QTDE-E
           MOVE QTDE-E            TO GS-LINTOT(1: 11).
           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP040" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           close work open input work

           MOVE GS-CONTRATO TO CONTRATO-REL.

           STRING "\ARQUIVOS\" CONTRATO-REL INTO ARQUIVO-IMPRESSAO

           OPEN OUTPUT ARQUI.

           IF GS-IMPRIMIR = "S"
              COPY CONDENSA.
           END-IF

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           PERFORM IMPRIMIR-FILTRO


           INVOKE gs-acp-listview-alb "Size" RETURNING WSSIZE
           INITIALIZE WSINDICE

           PERFORM WSSIZE TIMES
               ADD 1 TO WSINDICE

               invoke gs-acp-listview-alb "itematindex" using wsindice
                                                returning umitem

               initialize wstexto
               invoke umitem "getcolumnvalue" using wsColNrAlb
                                          returning umobjeto
               invoke umObjeto "GetValue" returning wstexto

               move function numval(wstexto) to album-wk
               read work key is album-wk invalid key
                    move "Nao achei" to mensagem
                    move "C" to tipo-msg
                    perform exibir-mensagem
               not invalid key
                    PERFORM MOVER-DADOS-RELATORIO
               end-read
           END-PERFORM


      *    PERFORM UNTIL ST-WORK = "10"
      *        READ WORK NEXT RECORD AT END
      *             MOVE "10" TO ST-WORK
      *        NOT AT END
      *             PERFORM MOVER-DADOS-RELATORIO
      *        END-READ
      *    END-PERFORM.
           PERFORM TOTALIZA-REL

           MOVE "SALTAR PAGINA" TO REG-ARQUI
           WRITE REG-ARQUI

           CLOSE ARQUI.

           IF GS-IMPRIMIR = "S"
              COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE DATA-WK           TO DATA-E
           MOVE DATA-E            TO LINDET-REL(1: 11)
           MOVE ALBUM-WK          TO LINDET-REL(12: 10)
           MOVE CURSO-WK          TO LINDET-REL(22: 31)
           MOVE NOME-WK           TO LINDET-REL(53: 31)
           MOVE CIDADE-WK         TO LINDET-REL(84: 13)
           MOVE IDENTIFICADO-WK   TO LINDET-REL(104:3)
           MOVE QTDE-IMAGENS-WK   TO MASC-QTDE
           MOVE MASC-QTDE         TO LINDET-REL(108:07)
           MOVE MOTIVO-WK         TO LINDET-REL(116:20).
           WRITE REG-ARQUI FROM LINDET
           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT FROM LINDET
           END-IF

           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE "TOTAL-FORMANDO: " TO LINTOT-REL(1: 16).
           MOVE TOT-FORMANDO       TO QTDE-E
           MOVE QTDE-E             TO LINTOT-REL(17: 10).

           MOVE "TOTAL-IMAGENS: "  TO LINTOT-REL(30:15)
           MOVE GS-TOT-IMAGENS     TO QTDE-E
           MOVE QTDE-E             TO LINTOT-REL(47:10)

           WRITE REG-ARQUI FROM LINTOT.

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT FROM LINTOT.

       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              MOVE "INICIO" TO REG-ARQUI
              WRITE REG-ARQUI
              WRITE REG-ARQUI FROM CAB01
              IF GS-IMPRIMIR = "S"
                 WRITE REG-RELAT FROM CAB01
              END-IF
           ELSE
              MOVE "SALTAR PAGINA" TO REG-ARQUI
              WRITE REG-ARQUI
              MOVE "INICIO" TO REG-ARQUI
              WRITE REG-ARQUI
              WRITE REG-ARQUI FROM CAB01
              IF GS-IMPRIMIR = "S"
                 WRITE REG-RELAT FROM CAB01 AFTER PAGE
              END-IF.

           WRITE REG-ARQUI FROM CAB02.
           WRITE REG-ARQUI FROM CAB03.
           WRITE REG-ARQUI FROM CAB04.
           WRITE REG-ARQUI FROM CAB03.


           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT FROM CAB02
              WRITE REG-RELAT FROM CAB03
              WRITE REG-RELAT FROM CAB04
              WRITE REG-RELAT FROM CAB03.

           MOVE 7 TO LIN.

       IMPRIMIR-FILTRO SECTION.
           MOVE SPACES TO REG-ARQUI
           STRING "CONTRATO..: " GS-CONTRATO   INTO REG-ARQUI
           WRITE REG-ARQUI
           MOVE SPACES TO REG-ARQUI
           STRING "CURSO.....: " GS-CURSO      INTO REG-ARQUI
           WRITE REG-ARQUI
           MOVE SPACES TO REG-ARQUI
           STRING "SEXO......: " GS-SEXO       INTO REG-ARQUI
           WRITE REG-ARQUI
           MOVE SPACES TO REG-ARQUI
           STRING "EVENTOS...: " GS-ACP-EVENTO INTO REG-ARQUI
           WRITE REG-ARQUI
           WRITE REG-ARQUI FROM CAB03

           IF GS-IMPRIMIR = "S"
              MOVE SPACES TO REG-RELAT
              STRING "CONTRATO..: " GS-CONTRATO   INTO REG-RELAT
              WRITE REG-RELAT
              MOVE SPACES TO REG-RELAT
              STRING "CURSO.....: " GS-CURSO      INTO REG-RELAT
              WRITE REG-RELAT
              MOVE SPACES TO REG-RELAT
              STRING "SEXO......: " GS-SEXO       INTO REG-RELAT
              WRITE REG-RELAT
              MOVE SPACES TO REG-RELAT
              STRING "EVENTOS...: " GS-ACP-EVENTO INTO REG-RELAT
              WRITE REG-RELAT
              WRITE REG-RELAT FROM CAB03.


       EXIBIR-MENSAGEM SECTION.
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
           CLOSE CAD010 COD040 IED011 MTD019 WORK MTD025 COD008 CGD011
                 COD003 COD060 MTD030
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
